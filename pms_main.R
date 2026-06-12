###############################################
# PMS(Portfolio Monitoring System) 1.01 / 2025-12-28 메인 스크립트 (루프 버전)
#
# 1줄 사용법 : 그냥 Ctrl + Alt + R 키를 누른다(전체 실행)
#
# - stock_eval.R / stock_eval_us.R 필요(각각 국내 및 미국 주식 데이터 수집 모듈)
# - risk_module.R 필요(리스크관리 함수 모음)
#   . risk_module.R의 몬테카를로, MDD, 인출, 팩터, PCA를 모두 호출
# - pms_benchmark.R : S&P500, NASDAQ 지수와 벤치마크 비교용 소스
#
# [입력 파일]
#         input_stock.csv    : 한국주식
#         input_stock_us.csv : 미국주식
#         output_sum.csv     : 전일 평가액총액, 수익금(입력이자 출력파일)
#         dividend.csv       : 수기로 입력한 연초부터 현재까지 배당금 내역(귀찮으면 안만들어도 됨)
#
# [출력 파일]
#         output_stock_{YYYY-MM-DD}.xlsx      : 한국주식 평가액
#         output_stock_us_{YYYY-MM-DD}.xlsx   : 미국주식 평가액
#         output_sum.csv                      : 평가액총액, 수익금
#                                               (최소 100일이상 데이터 필요)
#         reports/Daily_Risk_{YYYYMMDD}.pdf   : 1페이지 그래프 보고서
#         reports/gemini_prompt.txt           : 제미나이 질의어(프롬프트)
#
# - 누적 데이터(output_sum.csv)가 100일이 안되면 리스크 관리 분석은 생략
# 주) 리스크 및 운용 성과 평가는 TWR 기준,계좌 증감 및 체감 성과 표시는 NAV 기준으로 해석(형식적으로는 NAV 기반, 개념적으로는 TWR(Time-Weighted Return)에 해당)
###############################################
# 이 코드는 "돈을 얼마나 벌었는지를 관리하지 않는다. 망하지 않을 구조만 관리한다."
# 위험관리 핵심 3대 지표 : MDD · CVaR · Risk-Off 3개

# 주의 : 표본수가 100일 미만이면 그래프는 만들지 않음 -> 100일 이상 표본수(누적일) 필요, 단, 엑셀 출력은 계속 만듦

# 마이너 업데이트 기록
# 2026-06-12 금융소득종합과세자(연간 금융소득 2천만원 초과)가 되는지 여부를 미리 알수 있는 텍스트 그래프 추가
#            같은 폴더에 dividend.csv를 만들고 '날짜,종목명,통화,배당금,환율' 순서로 입력하여 수기로 관리
#            그렇게 하면 연말까지 예상 배당금을 미리 알 수 있음(분리과세 배당도 있으므로 오차 감안하여 단순참고)

# =========================================================
# 패키지 설치/로드
# =========================================================
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr",
         "PerformanceAnalytics", "showtext", "zoo", "tidyr", "quantmod",
         "xts", "rugarch", "htmltools", "tidyverse", "DT", "ggplot2",
         "dplyr", "writexl", "purrr", "broom")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(readr); library(readxl); library(showtext)
library(openxlsx); library(rvest); library(httr)
library(dplyr); library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)
library(zoo); library(tidyr); library(quantmod); library(xts)
library(rugarch); library(htmltools); library(tidyverse)
library(writexl); library(purrr); library(broom)

# -------------------------------------------------
# Sunday check : 일요일이면 바로 종료
# (참고) 토요일은 전날 밤에 미국시장이 운영되므로 그냥 실행하게 함
# -------------------------------------------------
today <- Sys.Date()

# as.POSIXlt 기준: 0 = Sunday, 1 = Monday, ..., 6 = Saturday
if (as.POSIXlt(today)$wday == 0) {  # 일요일
  message("[PMS] 오늘은 일요일이므로 실행하지 않고 종료합니다.")
  if (interactive()) {  # Rstudio에서 하면 R세션유지하고 종료
    stop("PMS stopped (The stock market is closed today because it is Sunday)", call. = FALSE)
  } else {              # 스크립트를 실행하면 세션까지 종료
    quit(save = "no", status = 0)
  }
}


# =========================================================
# 개인별 세팅 변수
# =========================================================
wd        <- "c:\\PMS_Core"  # 작업 디렉토리
fund_name <- "JS Fund"       # 펀드 이름(멋지게 지어보자)

weights <- c(
  0.36,  # SPY등
  0.20,  # SCHD
  0.15,  # QQQ
  0.10,  # TQQQ
  0.10,  # GOLD
  0.05,  # IEF
  0.04   # CASH : 현금이 아니라 종목으로서의 현금을 뜻함
)

REPEAT_FLAG = TRUE  # 주기적인 반복이면 TRUE, 1회 실행이면 FALSE

# =========================================================
# 개인별 세팅 변수 끝
# =========================================================

weights <- setNames(weights, c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF","CASH"))
setwd(wd)
options(scipen = 999)

# 리스크 + 팩터 + PCA 모듈 로드
source("risk_module.R")

suppressWarnings(
  try(update_factor_data(), silent = TRUE)
)

# =========================================================
# 실행 제어 변수
# =========================================================
count <- 1
last_mc_date <- as.Date(NA)
week_kor <- c("일", "월", "화", "수", "목", "금", "토")
min_days_for_risk <- 100

font_add(family = "malgun", regular = "C:\\Windows\\Fonts\\malgun.ttf")
showtext_auto()

# 환율 정보 변후 초기화
#spx <- NA
spx_val <- if(is.list(spx)) spx$spx_value else "(미수집)" 
spx_diff <- if(is.list(spx)) spx$spx_diff_label else "-" 
spx_pct <- if(is.list(spx)) spx$spx_pct else NA_real_

# =========================================================
# 유틸: TWR 계산 컬럼 추가
# =========================================================
add_twr_return_to_dd <- function(dd, ret_clip = 0.5, flow_deadband = 1000) {
  dd <- dd %>% arrange(Date)
  
  dd <- dd %>%
    mutate(
      Invested     = Sum - Profit,
      Invested_lag = lag(Invested),
      Sum_lag      = lag(Sum),
      Flow_raw     = Invested - Invested_lag,
      Flow         = if_else(!is.na(Flow_raw) & abs(Flow_raw) <= flow_deadband, 0, Flow_raw),
      Gross_base   = Sum_lag + Flow,
      Return       = if_else(!is.na(Gross_base) & Gross_base > 0, Sum / Gross_base - 1, NA_real_)
    ) %>%
    mutate(Return = if_else(!is.na(Return) & abs(Return) < ret_clip, Return, NA_real_))
  
  return(dd)
}

# =========================================================
# Gemini Prompt 생성
# =========================================================
make_gemini_prompt_pms <- function(
    dd,
    sum_xts,
    badge_text = NULL,
    fund_name = "JS Fund",
    report_time_kst = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    flow_text = "금일 Flow(입출금): 0원 / 매수·매도: 없음(거래 0건)",
    warnings_vec = character(0),
    errors_vec   = character(0),
    take_last_n_days = 2,
    benchmark_name = "SPY",
    benchmark_ret  = NA_real_,
    # 딱 두 개만 받음
    cvar_amt = NA_real_,   # CVaR (원)
    pa_mdd   = NA_real_    # MDD (비율, 음수)
) {
  
  if (is.null(dd) || NROW(dd) == 0) stop("dd가 비어 있습니다.")
  if (is.null(sum_xts) || NROW(sum_xts) == 0) stop("sum_xts가 비어 있습니다.")
  
  # ---- 유틸 포맷 ----
  fmt_comma <- function(x) {
    if (!is.finite(x)) return("(미제공)")
    if (requireNamespace("scales", quietly = TRUE)) return(scales::comma(x))
    format(x, big.mark = ",", scientific = FALSE)
  }
  fmt_pct <- function(x, acc = 0.01) {
    if (!is.finite(x)) return("(미제공)")
    if (requireNamespace("scales", quietly = TRUE)) return(scales::percent(x, accuracy = acc))
    paste0(round(x * 100, 2), "%")
  }
  fmt_cvar_amt <- function(x) {
    if (!is.finite(x)) return("(미제공)")
    paste0(fmt_comma(x), "원")
  }
  
  # ---- DD_now ----
  dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
  
  # ---- 최근 N일 표 ----
  dd_tail <- utils::tail(dd, take_last_n_days)
  tab_txt <- utils::capture.output(print(dd_tail))
  
  # ---- 배지/경고/에러 ----
  badge_txt <- if (!is.null(badge_text) && nzchar(badge_text)) badge_text else "(미제공)"
  warn_txt  <- if (length(warnings_vec) > 0) paste0("- ", warnings_vec, collapse = "\n") else "(없음)"
  err_txt   <- if (length(errors_vec)  > 0) paste0("- ", errors_vec,  collapse = "\n") else "(없음)"
  
  # ---- KPI: dd 마지막 행 기준 ----
  last_row <- dd[NROW(dd), , drop = FALSE]
  
  nav_today <- if ("Sum" %in% names(last_row)) as.numeric(last_row$Sum) else NA_real_
  nav_prev  <- if ("Sum_lag" %in% names(last_row)) as.numeric(last_row$Sum_lag) else NA_real_
  nav_diff  <- nav_today - nav_prev
  nav_diff_pct <- if (is.finite(nav_prev) && nav_prev != 0) nav_diff / nav_prev else NA_real_
  
  ret_nav <- if ("Return_NAV" %in% names(last_row)) as.numeric(last_row$Return_NAV) else NA_real_
  ret_twr <- if ("Return_TWR" %in% names(last_row)) as.numeric(last_row$Return_TWR) else NA_real_
  
  bm_line <- if (is.finite(benchmark_ret)) {
    paste0("- 벤치마크(", benchmark_name, ") 금일 수익률: ", fmt_pct(benchmark_ret, acc = 0.01))
  } else {
    paste0("- 벤치마크(", benchmark_name, ") 금일 수익률: (미제공 → 평가 유보)")
  }
  
  rel_line <- if (is.finite(benchmark_ret) && is.finite(ret_twr)) {
    paste0("- ", benchmark_name, " 대비 상대성과(금일, TWR): ", fmt_pct(ret_twr - benchmark_ret, acc = 0.01))
  } else {
    paste0("- ", benchmark_name, " 대비 상대성과: (평가 유보)")
  }
  
  kpi_txt <- paste0(
    "- 금일 평가금(Sum): ", fmt_comma(nav_today), "원\n",
    "- 전일 대비: ", fmt_comma(nav_diff), "원 (", fmt_pct(nav_diff_pct, acc = 0.01), ")\n",
    "- 금일 Return_NAV: ", fmt_pct(ret_nav, acc = 0.01), "\n",
    "- 금일 Return_TWR: ", fmt_pct(ret_twr, acc = 0.01), "\n",
    bm_line, "\n",
    rel_line
  )
  
  # ---- 핵심 위험지표 ----
  risk_txt <- paste0(
    "- 현재 드로다운(DD_now): ", fmt_pct(dd_now, acc = 0.01), "\n",
    "- 최대낙폭(MDD(%)): ", round(pa_mdd*100, 2), "\n",
    "- CVaR(95%, 원): ", fmt_cvar_amt(cvar_amt)
  )
  
  # 프로그래밍도 중요하지만 Prompt engineering도 중요
  # 아래 스크립트를 잘 써야지 인공지능이 제대로 답변해 줌
  
  paste0(
    "당신은 연기금·헤지펀드 등 기관 자산운용사에서 다년간 근무한
      수석 포트폴리오 매니저(CIO급)입니다.
      
      아래 입력 데이터는 한 개인 포트폴리오의
      ‘금일 운용 결과 및 리스크 상태’를 요약한 내부 보고용 자료입니다.
      
      다음의 **엄격한 규칙**을 반드시 준수하여,
      기관 운용사의 일일 운용 코멘트 형식으로 작성하십시오.
      
      [필수 규칙]
      
      1. 예측 금지
      - 향후 시장 전망, 상승·하락 예측, 목표 수익률 제시 금지
      
      2. 투자 권유·행동 지시 금지
      - 매수·매도·비중 조정·리밸런싱 등 행동을 유도하는 표현 금지
      - “~하는 것이 바람직하다 / 필요하다”와 같은 문장 금지
      
      3. 해석 중심 서술
      - 이미 발생한 결과와 현재 상태에 대한 해석만 허용
      
      4. 성과 지표 해석 필수
      - Return_NAV(계좌 기준 수익률)과 Return_TWR(운용 성과)의 차이를
      반드시 설명할 것
      - 환율 변동, 현금 흐름, 평가 시점 차이 등은
      ‘가능성’ 수준으로만 언급할 것(단정 금지)
      
      5. 문체
      - 존댓말
      - 기관 자산운용사 내부 일일 리포트 톤
      - 감정적 표현, 과장된 표현 금지
      
      ---
        
        [출력 형식 — 반드시 이 순서를 따를 것]
      
      1) 오늘의 한 줄 요약  
      - 운용 상태를 가장 압축적으로 요약한 문장 1개
      - 최신 평가액과 전일대비 금액은 메일 맨위의 첫줄에 요약해서 먼저 보고
      
      2) 운용 상태 설명  
      - 금일 거래 여부, Flow 유무, Risk-Off 상태를 중심으로
      현재 포트폴리오의 ‘운용 안정성’을 설명
      
      3) 핵심 요약(KPI)  
      - 평가금 변화, 일간 수익률, 상대적 성과 상태를
      수치 중심으로 간결히 요약
      
      4) 성과 요약 (Return_NAV vs Return_TWR)  
      - 두 지표가 왜 차이가 발생했는지
      구조적·기술적 요인 관점에서 설명
      
      5) 드로다운 및 위험지표 해석  
      - DD_now, MDD, CVaR 수치를
      ‘위험의 크기’가 아닌 ‘위험의 위치와 상태’ 관점에서 해석
      
      6) 특이사항  
      - 금일 데이터에서 눈에 띄는 점이 있을 경우만 서술
      - 없으면 “특이사항 없음”으로 명시
      - 아래의 Drift에도 간략히 요약
      
      7) 금일 시장 환경 한 줄 요약
      - 입력 데이터에 시장 지표가 없는 경우,
        정량 수치 없이 거시적 분위기를 정성적으로 요약할 수 있습니다.
      - 특정 지수 수치, 방향성 예측, 투자 판단은 금지합니다.
      
      8) 오늘의 원달러환율과 전일대비를 숫자로 표시하고, 오늘 환화평가금과의 관계와 아래의 S&P500지수 일간등락의 관계를 함께 분석해줘
      
      9) 전영업일대비 보유종목 변동을 알기 쉽게 표시해줘

      10)  마지막 끝낼 때 [오늘의 유머]라고 말머리를 달고 아주 랜덤하게 주식 유머 하나만 짧게 해줘
      
      주의)
      정량 수치가 없는 경우, 시장 환경은
      일반적인 금융시장 분위기를 정성적으로 요약할 수 있습니다.
      단, 특정 수치·지수·방향성 예측은 금지합니다.
      ---\n",
    "[Fund Name] : ", fund_name, "\n",
    "[Report Time] : ", report_time_kst, " (KST)\n\n",
    "=== [0] Flow/거래 ===\n", flow_text, "\n\n",
    "=== [1] 배지 ===\n", badge_txt, "\n\n",
    "=== [2] KPI ===\n", kpi_txt, "\n\n",
    "=== [3] 드로다운/위험지표 ===\n", risk_txt, "\n\n",
    "=== [4] 최근 ", take_last_n_days, "일 원자료(dd tail) ===\n",
    paste(tab_txt, collapse = "\n"), "\n\n",
    "=== [5] Warnings ===\n", warn_txt, "\n\n",
    "=== [6] Errors ===\n", err_txt, "\n\n",
    "=== [7] 원달러환율(전일대비) ===\n", exchange_rate, "(", exchange_diff, ")", "\n\n",
    "=== [9] S&P500 지수 :", spx$spx_value, "(전일대비:", spx$spx_diff_label, ", 일간변동률:", spx$spx_pct, "%)\n\n",
    "===[10] Drift 의견 :", get0("PMS_OPINION_DRIFT", ifnotfound = ""),"\n\n",
    "===[11] ", stock_change_msg,"\n\n")
}


save_if_changed <- function(text, file_path) {
  old <- if (file.exists(file_path)) paste(readLines(file_path, warn = FALSE), collapse = "\n") else ""
  if (!identical(old, text)) {
    writeLines(text, file_path, useBytes = TRUE)
    return(TRUE)
  }
  FALSE
}

make_badge_text <- function(sum_xts, GLD_MODE) {
  dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
  if (isTRUE(GLD_MODE)) {
    "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
  } else if (!is.na(dd_now) && dd_now <= -0.12 && dd_now > -0.20) {
    "현재 운용 상태 :  CAUTION  (DD 12~20% · 주의 관찰)"
  } else {
    "현재 운용 상태 :  NORMAL  (Risk-Off : OFF)"
  }
}

# 전일 파일 찾기
get_prev_file <- function(prefix = "output_stock_", ext = "xlsx") {
  pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
  files <- dir(pattern = pattern)
  if (length(files) == 0) return(NA)
  dates <- as.Date(sub(paste0(prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext), "\\1", files))
  valid_idx <- which(dates < Sys.Date())
  if (length(valid_idx) == 0) return(NA)
  files[which.max(dates[valid_idx])]
}

join_stock_data <- function(today_df, prev_df) {
  today_df %>%
    distinct(종목번호, 보유증권사, .keep_all = TRUE) %>%
    left_join(prev_df, by = c("종목번호", "보유증권사")) %>%
    mutate(
      한화평가금 = trunc(한화평가금),
      전일한화평가금 = trunc(전일한화평가금),
      전일대비 = trunc(한화평가금 - 전일한화평가금),
      전일대비율 = if_else(
        is.na(전일한화평가금) | 전일한화평가금 == 0,
        NA_character_,
        sprintf("%.2f", round((한화평가금 - 전일한화평가금) / 전일한화평가금 * 100, 2))
      ),
      비중 = sprintf("%.2f", round(한화평가금 / sum(한화평가금, na.rm = TRUE) * 100, 2))
    ) %>%
    arrange(desc(한화평가금))
}


get_naver_usdkrw <- function(start_date, end_date) {  # 네이버 금융에서 특정일부터 특정일까지 환율조회
  start_date <- ymd(start_date)
  end_date   <- ymd(end_date)
  
  result <- data.frame()
  page <- 1
  
  repeat {
    url <- paste0(
      "https://finance.naver.com/marketindex/exchangeDailyQuote.naver?",
      "marketindexCd=FX_USDKRW&page=", page
    )
    
    tbl <- read_html(url, encoding = "EUC-KR") %>%
      html_table(fill = TRUE) %>%
      .[[1]]
    
    # names(tbl) <- c("date", "rate", "change", "buy_cash", "sell_cash", 
    #                 "send", "receive", "tc_buy", "foreign_check")
    names(tbl) <- c("date", "rate", "change", "buy_cash", "sell_cash", 
                    "send", "receive")
    
    tbl <- tbl %>%
      filter(!is.na(date), str_detect(date, "\\d{4}\\.\\d{2}\\.\\d{2}")) %>%
      mutate(
        date = ymd(str_replace_all(date, "\\.", "-")),
        rate = as.numeric(str_replace_all(rate, ",", ""))
      ) %>%
      select(date, rate)
    
    result <- bind_rows(result, tbl)
    
    if (min(tbl$date) < start_date) break
    
    page <- page + 1
    Sys.sleep(0.2)
  }
  
  result %>%
    filter(date >= start_date, date <= end_date) %>%
    arrange(date)
}


PROMPT_FILE <- file.path("reports", "gemini_prompt.txt")
UPDATE_EVERY_SEC <- 10
last_update_time <- Sys.time() - 9999


# =========================================================
# 루프 시작 전에 전일자 파일과 비교하여 변동사항이 있는지 표시
# =========================================================


get_prev_output_file <- function(prefix) {
  files <- list.files(
    pattern = paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.xlsx$")
  )
  
  files <- sort(files)
  
  if (length(files) < 2) return(NA_character_)
  
  files[length(files) - 1]
}

compare_stock_change <- function(today_file, prev_file, market = "KR") {
  
  if (is.na(prev_file) || !file.exists(prev_file) || !file.exists(today_file)) {
    return(tibble())
  }
  
  prev <- read_excel(prev_file)
  today <- read_excel(today_file)
  
  prev2 <- prev %>%
    filter(!is.na(종목명), !is.na(종목번호)) %>%
    select(종목명, 종목번호, 보유증권사, 수량) %>%
    mutate(
      시장 = market,
      old_qty = as.numeric(수량)
    ) %>%
    select(시장, 종목명, 종목번호, 보유증권사, old_qty)
  
  today2 <- today %>%
    filter(!is.na(종목명), !is.na(종목번호)) %>%
    select(종목명, 종목번호, 보유증권사, 수량) %>%
    mutate(
      시장 = market,
      new_qty = as.numeric(수량)
    ) %>%
    select(시장, 종목명, 종목번호, 보유증권사, new_qty)
  
  full_join(
    prev2,
    today2,
    by = c("시장", "종목명", "종목번호", "보유증권사")
  ) %>%
    mutate(
      old_qty = ifelse(is.na(old_qty), 0, old_qty),
      new_qty = ifelse(is.na(new_qty), 0, new_qty),
      diff_qty = new_qty - old_qty,
      변동유형 = case_when(
        old_qty == 0 & new_qty > 0 ~ "신규매수",
        old_qty > 0 & new_qty == 0 ~ "전량매도",
        old_qty > 0 & new_qty > old_qty ~ "추가매수",
        old_qty > 0 & new_qty < old_qty ~ "일부매도",
        TRUE ~ "변동없음"
      )
    ) %>%
    filter(변동유형 != "변동없음")
}

today <- Sys.Date()

today_ko_file <- paste0("output_stock_", today, ".xlsx")
today_us_file <- paste0("output_stock_us_", today, ".xlsx")

prev_ko_file <- get_prev_output_file("output_stock_")
prev_us_file <- get_prev_output_file("output_stock_us_")

change_ko <- compare_stock_change(
  today_file = today_ko_file,
  prev_file  = prev_ko_file,
  market = "KR"
)

change_us <- compare_stock_change(
  today_file = today_us_file,
  prev_file  = prev_us_file,
  market = "US"
)

change_all <- bind_rows(change_ko, change_us)

print(change_all, n = Inf)

make_stock_change_message <- function(change_all) {
  
  if (nrow(change_all) == 0) {
    return("[전영업일 대비 보유종목 변동]\n변동사항 없음\n")
  }
  
  msg <- "[전영업일 대비 보유종목 변동]\n"
  
  for (tp in c("신규매수", "추가매수", "일부매도", "전량매도")) {
    
    tmp <- change_all %>% filter(변동유형 == tp)
    
    if (nrow(tmp) > 0) {
      msg <- paste0(msg, "\n■ ", tp, "\n")
      
      lines <- paste0(
        "- [", tmp$시장, "] ",
        tmp$종목명, " / ",
        tmp$보유증권사,
        " : ",
        tmp$old_qty,
        "주 → ",
        tmp$new_qty,
        "주 (",
        ifelse(tmp$diff_qty > 0, "+", ""),
        tmp$diff_qty,
        "주)"
      )
      
      msg <- paste0(msg, paste(lines, collapse = "\n"), "\n")
    }
  }
  
  msg
}

stock_change_msg <- make_stock_change_message(change_all)

cat(stock_change_msg)


# =========================================================
# 금융소득종합과세 체크 함수
# - dividend.csv의 배당금은 세후 입금액 기준이라고 가정
# - 세전 환산 후 2천만원 기준과 비교
# - 목적 : 금융소득종합과세자가 되지 않도록 예상 세전 배당금이 2천만원 초과하는지 예측
# =========================================================
print_financial_income_gauge <- function(
    file_path = "dividend.csv",
    limit_dividend = 20000000,
    tax_rate = 0.154,
    width = 50,
    encoding = "UTF-8"
) {
  
  library(readr)
  library(dplyr)
  library(lubridate)
  
  if (!file.exists(file_path)) {
    cat("\n[금융소득종합과세 CHECK] dividend.csv 파일이 없어 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  df <- read_csv(
    file_path,
    comment = "#",
    locale = locale(encoding = encoding),
    show_col_types = FALSE
  )
  
  if (nrow(df) == 0) {
    cat("\n[금융소득종합과세 CHECK] 배당 데이터가 없습니다.\n")
    return(invisible(NULL))
  }
  
  df <- df %>%
    mutate(
      날짜 = as.Date(날짜),
      배당금 = as.numeric(배당금),
      환율 = as.numeric(환율),
      세후배당금_KRW환산 = 배당금 * 환율,
      세전배당금_KRW환산 = 세후배당금_KRW환산 / (1 - tax_rate)
    )
  
  current_gross <- sum(df$세전배당금_KRW환산, na.rm = TRUE)
  current_net   <- sum(df$세후배당금_KRW환산, na.rm = TRUE)
  
  days_passed <- yday(Sys.Date())
  expected_gross <- current_gross * 365 / days_passed
  
  current_rate  <- current_gross / limit_dividend * 100
  expected_rate <- expected_gross / limit_dividend * 100
  
  remain <- limit_dividend - expected_gross
  
  status <- case_when(
    expected_rate < 70  ~ "안전",
    expected_rate < 90  ~ "적정",
    expected_rate < 100 ~ "경고(고배당주 매수 주의)",
    TRUE                ~ "위험(더이상 고배당주 매수 금지)"
  )
  
  current_m  <- round(current_gross / 1e6, 1)
  expected_m <- round(expected_gross / 1e6, 1)
  net_m      <- round(current_net / 1e6, 1)
  limit_m    <- round(limit_dividend / 1e6, 1)
  
  current_pos <- round(min(current_gross / limit_dividend, 1.2) * width)
  expected_pos <- round(min(expected_gross / limit_dividend, 1.2) * width)
  
  current_pos <- max(current_pos, 1)
  expected_pos <- max(expected_pos, 1)
  
  line <- paste(rep("-", width), collapse = "")
  
  cat("\n====================================================\n")
  cat("금융소득종합과세 체크(세전 배당 기준 : 분리과세배당도 있으므로 오차는 있음을 감안)\n\n")
  
  cat("0 ", line, " ", limit_m, "백만원\n", sep = "")
  
  cat(
    paste0(
      strrep(" ", current_pos),
      "▲ 현재 [", current_m, "백만원]\n"
    )
  )
  
  cat(
    paste0(
      strrep(" ", expected_pos),
      "▲ 연말예상 [", expected_m, "백만원]\n\n"
    )
  )
  
  cat(sprintf(
    "현재 누적 세전 : %.1f백만원 / %.1f백만원 (%.1f%%)\n",
    current_m, limit_m, current_rate
  ))
  
  cat(sprintf(
    "현재 누적 세후 : %.1f백만원\n",
    net_m
  ))
  
  cat(sprintf(
    "연말 예상 세전 : %.1f백만원 / %.1f백만원 (%.1f%%)\n",
    expected_m, limit_m, expected_rate
  ))
  
  if (remain >= 0) {
    cat(sprintf("예상 여유      : %.1f백만원\n", remain / 1e6))
  } else {
    cat(sprintf("예상 초과      : %.1f백만원\n", abs(remain) / 1e6))
  }
  
  cat("상태           :", status, "\n")
  cat("====================================================\n\n")
  
  return(invisible(list(
    current_gross = current_gross,
    current_net = current_net,
    expected_gross = expected_gross,
    current_rate = current_rate,
    expected_rate = expected_rate,
    remain = remain,
    status = status
  )))
}

print_financial_income_gauge("c:\\PMS_Core\\dividend.csv")  # 배당파일이 없으면 무시하고 건너뜀


# =========================================================
# 반복 루프 시작
# =========================================================
repeat {
  
  #graphics.off()  # 열린 windows 닫기
  
  now  <- as.POSIXct(Sys.time())
  hhmm <- format(now, "%H:%M")
  wday <- as.numeric(format(now, "%u"))  # 1=월 ~ 7=일
  in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"
  
  cat("[", count, "회차] ", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      " : 실행 시작***********************************************\n", sep="")
  
  tryCatch({
    
    # =========================================================
    # 현재 보유자산 평가 업데이트
    # =========================================================
    suppressWarnings(source("stock_eval.R"))
    suppressWarnings(source("stock_eval_us.R"))
    
    today <- Sys.Date()
    file1 <- paste0("output_stock_",    today, ".xlsx")
    file2 <- paste0("output_stock_us_", today, ".xlsx")
    output_file <- "output_sum.csv"
    
    if (!file.exists(file1) || !file.exists(file2)) {
      cat("[경고] 오늘 평가 파일이 아직 없습니다. (", file1, ", ", file2, ")\n", sep="")
      goto_sleep <- TRUE
    } else {
      goto_sleep <- FALSE
    }
    
    if (!goto_sleep) {
      
      data1 <- read_excel(file1)
      data2 <- read_excel(file2)
      
      last_value1   <- tail(data1[["평가금"]],  1)
      last_value1_2 <- tail(data1[["수익금"]], 1)
      
      last_value2   <- tail(data2[["평가금"]],  1)
      last_value2_2 <- tail(data2[["수익금"]], 1)
      
      sum_value    <- round(last_value1 + last_value2, 0)
      profit_value <- round(last_value1_2 + last_value2_2, 0)
      
      # ---------------------------------------------------------
      #  현금성(CASH_LIKE) 별도 관리 (KOFR/BIL//GOV/MMF/CMA 등)
      # ---------------------------------------------------------
      cash_like <- 0  # ★ 현금성 금액(원). 필요 시 수동 입력/연동
      
      sum_value <- round(sum_value + cash_like, 0)
      
      result <- data.frame(Date = today, Sum = sum_value, Profit = profit_value)
      
      # output_sum.csv 갱신
      if (file.exists(output_file)) {
        existing_data <- read_csv(output_file,
                                  col_types = cols(Date = col_date(format = ""),
                                                   Sum = col_double(),
                                                   Profit = col_double()),
                                  show_col_types = FALSE)
        
        # # existing_data의 마지막행이 공백행이면 제거
        # existing_data <- existing_data %>%
        #   dplyr::filter(!is.na(Date))
        
        
        # 마지막행이 오늘이라면 그 행 삭제
        if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
          existing_data <- existing_data[-nrow(existing_data), ]
        }
        
        updated_data <- bind_rows(existing_data, result)
      } else {
        updated_data <- result
      }
      
      write_csv(updated_data, output_file)
      
      # is_initial_mode <- (nrow(updated_data) < min_days_for_risk)
      dd_daily_n <- updated_data %>% dplyr::distinct(Date) %>% dplyr::filter(!is.na(Date)) %>% nrow()
      risk_ready <- dd_daily_n >= min_days_for_risk
      is_initial_mode <- !risk_ready
      
      
      # 분석용 데이터 재읽기 + Return 계산
      dd <- readr::read_csv(
        output_file,
        col_types = readr::cols(Date = readr::col_date(format = ""),
                                Sum = readr::col_double(),
                                Profit = readr::col_double()),
        show_col_types = FALSE
      ) %>% arrange(Date)
      
      dd <- add_twr_return_to_dd(dd)
      dd <- dd %>% rename(Return_TWR = Return)
      dd_ret <- dd %>% dplyr::filter(!is.na(Return_TWR))
      
      dd_daily_n <- dd %>% distinct(Date) %>% filter(!is.na(Date)) %>% nrow()
      risk_ready <- dd_daily_n >= min_days_for_risk
      
      dd_daily <- dd %>%
        group_by(Date) %>%
        summarise(Sum = last(Sum), .groups="drop") %>%
        arrange(Date)
      
      if (nrow(dd_daily) >= 2 && all(!is.na(dd_daily$Date)) && all(!is.na(dd_daily$Sum))) {
        sum_xts <- xts(dd_daily$Sum, order.by = dd_daily$Date)
      } else {
        sum_xts <- xts()
      }
      
      if (NROW(sum_xts) >= 2) {
        ret_xts <- Return.calculate(sum_xts, method="discrete")
        ret_xts <- na.omit(ret_xts)
        if (NROW(ret_xts) < 1) ret_xts <- xts()
      } else {
        ret_xts <- xts()
      }
      if (NROW(ret_xts) > 0) colnames(ret_xts) <- fund_name
      
      if (NROW(ret_xts) > 0) {
        dd_nav <- data.frame(Date = as.Date(index(ret_xts)), Return_NAV = as.numeric(ret_xts))
        dd <- dd %>% left_join(dd_nav, by = "Date")
      } else {
        dd <- dd %>% mutate(Return_NAV = NA_real_)
      }
      
      cat(sprintf("오늘 평가액: %s원 | 총수익: %s원\n",
                  comma(round(sum_value,0)), comma(round(profit_value,0))))
      
      if (NROW(ret_xts) >= 5) {
        cat("\n=========== PerformanceAnalytics 성과 요약 ===========\n")
        print(table.AnnualizedReturns(ret_xts))
        cat("\nMax Drawdown:\n"); print(maxDrawdown(ret_xts))
        cat("Sharpe(연환산, Rf=0):\n"); print(SharpeRatio.annualized(ret_xts, Rf = 0))
        cat("Calmar(연환산, 일간 NAV 기반):\n"); print(CalmarRatio(ret_xts))
        cat("======================================================\n\n")
      } else {
        cat("[경고] 일간 수익률 표본이 너무 적어 PerformanceAnalytics 요약 생략\n")
      }
      
      if (!risk_ready) {
        cat("[초기모드] 그래프/리스크 리포트(PDF) 생성 생략\n")
        print(tail(dd %>% select(Date, Sum, Profit, Return_TWR, Return_NAV), 2))
      } else {
        
        today_date <- max(dd$Date, na.rm = TRUE)
        
        if (is.na(last_mc_date) || last_mc_date < today_date) {
          
          cat("\n[리스크] 오늘 기준 몬테카/미래MDD/인출 시뮬레이션...\n")
          suppressWarnings(try(run_mc_from_dd(dd, years=10, monthly_contrib=5000000, n_sims=5000), silent=TRUE))
          suppressWarnings(try(run_future_mdd_from_dd(dd, years=10, monthly_contrib=5000000, n_sims=2000), silent=TRUE))
          #suppressWarnings(try(run_mc_withdraw_from_dd(dd, years=30, annual_withdraw=200000000,
          suppressWarnings(try(run_mc_withdraw_from_dd(dd, years=30, annual_withdraw=78000000,  # 연 78백만원(월 650만원) 인출 가정
                                                       n_sims=5000, withdraw_freq="monthly"), silent=TRUE))
          # 연 인출액 7,800만원은
          # 국민연금을 제외한 월 450만 원의 실질 소비를
          # 물가상승률 2.5%를 반영해 30년 평균 명목 기준으로 환산한 값
          
          if (file.exists("factors_monthly.csv") && file.exists("asset_returns_monthly.csv")) {
            cat("[리스크] 팩터 회귀 실행...\n")
            suppressWarnings(try(run_factor_model_from_files("asset_returns_monthly.csv","factors_monthly.csv",weights), silent=TRUE))
          } else {
            cat("[리스크] 팩터 분석 스킵: 파일 없음\n")
          }
          
          if (file.exists("asset_returns_monthly.csv")) {
            cat("[리스크] PCA 실행...\n")
            suppressWarnings(try(run_pca_dashboard_from_file("asset_returns_monthly.csv", weights), silent=TRUE))
          } else {
            cat("[리스크] PCA 스킵: 파일 없음\n")
          }
          
          last_mc_date <- today_date
        } else {
          cat("\n[리스크] 오늘(", format(today_date), ") 몬테카/팩터/PCA 이미 실행됨\n\n", sep="")
        }
        
        # =========================================================
        # 그래프/트리맵/상세표
        # =========================================================
        dd_plot_base <- dd %>% filter(!is.na(Date), !is.na(Sum))
        
        sum_left  <- dd_plot_base$Sum / 1e7
        ret_right <- dd_plot_base$Return_NAV * 100
        
        sum_range     <- range(sum_left,  na.rm = TRUE)
        return_range  <- range(ret_right, na.rm = TRUE)
        
        a <- diff(sum_range) / diff(return_range)
        b <- sum_range[1] - a * return_range[1]
        
        start_date <- format(min(dd_plot_base$Date, na.rm = TRUE), "%Y-%m-%d")
        end_date   <- format(max(dd_plot_base$Date, na.rm = TRUE), "%Y-%m-%d")
        
        plot_title <- paste0(
          fund_name, " Portfolio Monitoring System (", start_date, " ~ ", end_date, ")  ",
          format(Sys.time(), "%Y년 %m월 %d일"),
          "(", week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], ") ",
          format(Sys.time(), "%H시 %M분")
        )
        
        dd_series <- if (NROW(sum_xts) >= 1) (sum_xts / cummax(sum_xts)) - 1 else xts()
        
        if (NROW(ret_xts) < 63 || NROW(ret_xts) == 0) {
          vol63_xts <- xts(rep(NA_real_, NROW(ret_xts)), order.by = index(ret_xts))
          today_dd <- NA_real_
          today_vol63 <- NA_real_
          consecutive_days <- 0
          GLD_MODE <- FALSE
        } else {
          vol63_xts <- zoo::rollapply(
            ret_xts, width = 63,
            FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
            align = "right", fill = NA
          )
          today_dd   <- as.numeric(last(dd_series))
          today_vol63 <- as.numeric(last(vol63_xts))
          
          cond_xts <- (vol63_xts >= 0.25) & (dd_series <= -0.15)
          cond_vec <- as.logical(coredata(cond_xts))
          valid_idx <- which(!is.na(cond_vec))
          
          consecutive_days <- 0
          if (length(valid_idx) > 0) {
            i <- tail(valid_idx, 1)
            if (isTRUE(cond_vec[i])) {
              while (i >= 1 && isTRUE(cond_vec[i])) {
                consecutive_days <- consecutive_days + 1
                i <- i - 1
              }
            }
          }
          GLD_MODE <- (consecutive_days >= 63)
        }
        
        dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
        if (GLD_MODE) {
          badge_text  <- "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
          badge_color <- "firebrick"
        } else if (!is.na(dd_now) && dd_now <= -0.12 && dd_now > -0.20) {
          badge_text  <- "현재 운용 상태 :  CAUTION  (DD 12~20% · 주의 관찰)"
          badge_color <- "goldenrod"
        } else {
          badge_text  <- "현재 운용 상태 :  NORMAL  (Risk-Off : OFF)"
          badge_color <- "darkgreen"
        }
        
        ret_xts_clean <- na.omit(ret_xts)
        if (NROW(ret_xts_clean) >= 5) {
          pa_tab    <- table.AnnualizedReturns(ret_xts_clean)
          pa_annret <- as.numeric(pa_tab["Annualized Return", 1])
          pa_annvol <- as.numeric(pa_tab["Annualized Std Dev", 1])
          pa_mdd    <- as.numeric(maxDrawdown(ret_xts_clean))
          pa_sharpe <- as.numeric(SharpeRatio.annualized(ret_xts_clean, Rf = 0))
          pa_calmar <- as.numeric(CalmarRatio(ret_xts_clean))
        } else {
          pa_annret <- NA_real_
          pa_annvol <- NA_real_
          pa_mdd    <- NA_real_
          pa_sharpe <- NA_real_
          pa_calmar <- NA_real_
        }
        
        fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.2f%%", x * 100))
        fmt_num <- function(x) ifelse(is.na(x), "-", sprintf("%.2f", x))
        
        # ---------- 트리맵 데이터 ----------
        dt_ko <- data_ko %>% head(-1) %>%
          dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
        dt_en <- data_en %>% head(-2) %>%
          dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
        
        dt_ko <- dt_ko %>% mutate(한화평가금 = 평가금, 한화매수가격 = 매수가격)
        dt_en <- dt_en %>% mutate(한화평가금 = 평가금 * exchange_rate,
                                  한화매수가격 = 매수가격 * exchange_rate)
        
        dt_fn <- bind_rows(dt_ko, dt_en) %>%
          dplyr::select(-평가금) %>%
          arrange(desc(한화평가금))
        
        showtext_auto(FALSE)
        dt_fn$종목명_tm <- ifelse(
          nchar(dt_fn$종목명) > 10,
          paste0(substr(dt_fn$종목명, 1, 10), "\n", substr(dt_fn$종목명, 11, 999)),
          dt_fn$종목명
        )
        treemap(dt_fn, 
                index="종목명_tm", 
                vSize="한화평가금", 
                title="종목구성비율 트리맵",
                palette = "Set3",
                border.col = "white",
                inflate.labels = TRUE,
                lowerbound.cex.labels = 0.5)
        showtext_auto()
        
        
        # 1일 평균 증가액
        fit <- lm(sum_left ~ as.numeric(dd_plot_base$Date), data = dd_plot_base)
        slope_per_day <- coef(fit)[2]
        
        
        prev_ko_file <- get_prev_file("output_stock_")
        prev_en_file <- get_prev_file("output_stock_us_")
        
        data_prev_ko <- if (!is.na(prev_ko_file) && file.exists(prev_ko_file)) read_excel(prev_ko_file) else NULL
        data_prev_en <- if (!is.na(prev_en_file) && file.exists(prev_en_file)) read_excel(prev_en_file) else NULL
        
        if (!is.null(data_prev_ko) && !is.null(data_prev_en)) {
          data_prev_ko <- data_prev_ko %>% head(-1) %>%
            dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 평가금)
          data_prev_en <- data_prev_en %>% head(-2) %>%
            mutate(한화평가금 = 평가금 * exchange_rate) %>%
            dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 한화평가금)
          data_prev_fn <- bind_rows(data_prev_ko, data_prev_en) %>%
            arrange(desc(전일한화평가금))
        } else {
          data_prev_fn <- data.frame(종목번호=character(), 보유증권사=character(), 전일한화평가금=numeric())
        }
        
        
        rt <- join_stock_data(dt_fn, data_prev_fn) %>%
          mutate(
            총매수금 = 한화매수가격 * 수량,
            총수익금 = 한화평가금 - 총매수금,
            총수익률 = round((총수익금 / pmax(총매수금, 1)) * 100, 2)
          ) %>%
          dplyr::select(종목명, 보유증권사, 한화매수가격, 수량, 한화평가금, 전일한화평가금,
                        전일대비, 전일대비율, 비중, 총매수금, 총수익금, 총수익률)
        
        # =========================================================
        # 자산군 합계/비중 계산
        # =========================================================
        
        # 종목 합(=rt에 들어온 자산)과 총합(종목+현금성) 분리
        today_tsum_stock <- sum(rt$한화평가금, na.rm = TRUE)  # 종목합
        today_tsum       <- today_tsum_stock + cash_like       # 총합(종목 + 현금성)
        
        asset_SCHD <- rt %>%
          filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        asset_QQQ <- rt %>%
          filter(
            str_detect(종목명, "나스닥100|QQQ") &
              !str_detect(종목명, "TQQQ")
          ) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        asset_TQQQ <- rt %>%
          filter(str_detect(종목명, "TQQQ")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        asset_GLD <- rt %>%
          filter(str_detect(종목명, "금현물")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        asset_IEF <- rt %>%
          filter(str_detect(종목명, "채권|국채")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        asset_CASH <- rt %>%
          filter(str_detect(종목명, "KOFR|BIL|SGOV|머니마켓")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계) %>% tidyr::replace_na(0)
        
        # SPY_ETC는 "종목 중 나머지"로 정의
        asset_SPY_ETC <- today_tsum_stock - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_IEF - asset_CASH
        
        asset_SPY_ETC[is.na(asset_SPY_ETC)] <- 0
        
        # 비중(%)은 총합(today_tsum = 종목+현금성) 기준
        asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
        asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
        asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
        asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
        asset_IEF_ratio     <- asset_IEF     / today_tsum * 100
        asset_CASH_ratio    <- asset_CASH    / today_tsum * 100
        asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
        
        # drift용 current_weights (합=1)
        current_weights <- c(
          SPY_ETC = asset_SPY_ETC_ratio / 100,
          SCHD    = asset_SCHD_ratio    / 100,
          QQQ     = asset_QQQ_ratio     / 100,
          TQQQ    = asset_TQQQ_ratio    / 100,
          GOLD    = asset_GLD_ratio     / 100,
          IEF     = asset_IEF_ratio     / 100,
          CASH    = asset_CASH_ratio    / 100
        )
        
        # (선택) 비중 합계 체크
        cat("[CHECK] ratios sum = ",
            asset_SPY_ETC_ratio + asset_SCHD_ratio + asset_QQQ_ratio + asset_TQQQ_ratio +
              asset_GLD_ratio + asset_IEF_ratio + asset_CASH_ratio, "\n")
        
        # =========================================================
        # 리스크 모듈 추가 실행
        # =========================================================
        need_nm <- c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF","CASH")
        if (is.null(names(weights)) || any(names(weights) == "")) {
          if (length(weights) == length(need_nm)) {
            names(weights) <- need_nm
            cat("weights 이름 자동 부여 완료\n")
          } else {
            cat("weights 오류 → 리스크 분석 스킵\n")
            goto_sleep <- TRUE
          }
        }
        
        if (!goto_sleep) {
          
          if (abs(sum(weights, na.rm = TRUE) - 1) > 1e-6) {
            weights <- weights / sum(weights, na.rm = TRUE)
            cat("weights 정규화 완료 (합=1)\n")
          }
          
          current_nav <- tail(dd$Sum, 1)
          
          suppressWarnings(try(run_stress_replay_from_file("asset_returns_monthly.csv", weights, current_nav, monthly_contrib=0), silent=TRUE))
          suppressWarnings(try(run_garch_vol_alert(dd_ret), silent=TRUE))
          
          cvar_obj <- NULL
          suppressWarnings(try({
            cvar_obj <- run_var_cvar_from_file("asset_returns_monthly.csv", weights, current_nav, alpha=0.95)
          }, silent=TRUE))
          cvar_amt <- if (!is.null(cvar_obj) && !is.null(cvar_obj$cvar_amt)) cvar_obj$cvar_amt else NA_real_
          
          suppressWarnings(try(run_drift_rebal_signal(target_weights=weights, current_weights=current_weights, threshold=0.05), silent=TRUE))
          
          
          # 투자원금 대비 현재평가액을 환율과 함께 그래프로 표현
          dd_simple <- dd %>%
            select(Date, Sum, Invested) %>%
            mutate(Date = as.Date(Date))
          
          # 네이버 원/달러 환율 조회
          usdkrw <- get_naver_usdkrw(
            start_date = min(dd_simple$Date),
            end_date   = max(dd_simple$Date)
          )
          
          # 환율 데이터 정리
          fx_df <- usdkrw %>%
            rename(
              Date = date,
              FX = rate
            ) %>%
            mutate(Date = as.Date(Date))
          
          # PMS 데이터와 환율 결합
          dd_simple <- dd_simple %>%
            left_join(fx_df, by = "Date") %>%
            arrange(Date) %>%
            mutate(FX = zoo::na.locf(FX, na.rm = FALSE))
          
          # 막대용 long 변환
          dd_long <- dd_simple %>%
            select(Date, Invested, Sum) %>%
            pivot_longer(
              cols = c(Invested, Sum),
              names_to = "Type",
              values_to = "Amount"
            )
          
          # 스케일 배율
          scale_factor <- max(dd_long$Amount / 1e6, na.rm = TRUE) /
            max(dd_simple$FX, na.rm = TRUE)
          
          fx_month_start <- dd_simple %>%
            filter(!is.na(FX)) %>%
            mutate(month = format(Date, "%Y-%m")) %>%
            group_by(month) %>%
            slice_min(Date, n = 1, with_ties = FALSE) %>%
            ungroup()
          
          s <- ggplot() +
            geom_col(
              data = dd_long,
              aes(x = Date, y = Amount / 1e6, fill = Type),
              position = "dodge",
              alpha = 0.85
            ) +
            geom_line(
              data = dd_simple %>% 
                filter(!is.na(FX)),
              aes(x = Date, y = FX * scale_factor, color = "환율"),
              linewidth = 1.2
            ) +
            annotate(
              "text",
              x = max(dd_simple$Date, na.rm = TRUE),
              y = tail(na.omit(dd_simple$FX), 1) * scale_factor,
              label = paste0(
                "오늘 환율 : ",
                round(tail(na.omit(dd_simple$FX), 1), 1)
              ),
              hjust = 1.1,
              vjust = -0.5,
              color = "darkgreen",
              size = 4,
              fontface = "bold"
            ) +
            geom_hline(
              yintercept = mean(fx_df$FX, na.rm = TRUE) * scale_factor,
              linetype = "dashed",
              color = "gray50",
              alpha = 0.4,
              linewidth = 0.8
            ) +
            geom_text(
              data = fx_month_start %>% 
                filter(!is.na(FX)),
              aes(
                x = Date,
                y = FX * scale_factor,
                label = round(FX, 1)
              ),
              vjust = -0.3,
              size = 3,
              color = "darkgreen"
            ) +
            scale_y_continuous(
              name = "금액(백만원)",
              labels = label_comma(),
              sec.axis = sec_axis(
                ~ . / scale_factor,
                name = "환율(USD/KRW)"
              )
            ) +
            scale_fill_manual(
              values = c(
                "Invested" = "blue",
                "Sum" = "red"
              ),
              labels = c(
                "Invested" = "투자원금",
                "Sum" = "현재평가액"
              )
            ) +
            scale_color_manual(
              values = c("환율" = "darkgreen")
            ) +
            labs(
              title = paste0(
                "PMS : 투자원금 vs 현재평가액(막대) + 원/달러 환율 : ",
                round(tail(fx_df$FX, 1), 1),
                "  |  평균환율(수평점선) : ",
                round(mean(fx_df$FX, na.rm = TRUE), 1)
              ),
              x = "날짜",
              fill = NULL,
              color = NULL
            ) +
            theme_minimal(base_size = 13) +
            theme(
              legend.position = "top",
              axis.title.y.right = element_text(color = "darkgreen"),
              axis.text.y.right = element_text(color = "darkgreen")
            )
          
          print(s)
          
          
          # ---------- DT 출력 (캡션 HTML) ----------
          today_sum <- tail(dd$Sum, 1)
          yesterday_sum <- tail(dd$Sum, 2)[1]
          diff_value <- today_sum - yesterday_sum
          diff_pct   <- (diff_value / yesterday_sum) * 100
          diff_color <- if(diff_value > 0) "blue" else if(diff_value < 0) "red" else "black"
          diff_sign  <- if(diff_value > 0) "+" else ""
          
          caption_string <- paste0(
            "<div style='display:flex;justify-content:center;align-items:center;margin-bottom:15px;'>",
            "<span style='font-size:130%;font-weight:bold;color:black;margin-right:15px;'>",
            format(Sys.time(), "%Y년 %m월 %d일"),
            "(", week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], ") ",
            format(Sys.time(), "%H시 %M분   "),
            "<span style='font-size:100%;font-weight:bold;color:black;margin-right:15px;'>",
            "한화평가금합계 ", scales::comma(sum_value), "원</span>",
            "<span style='font-size:90%;font-weight:bold;color:", diff_color, ";'>",
            "(전일대비 ", diff_sign, scales::comma(diff_value), "원, ",
            diff_sign, sprintf("%.2f%%, ", diff_pct), exchange_rate, "원/달러,", exchange_diff, ")</span></div>"
          )
          
          print(
            datatable(
              rt,
              caption = htmltools::HTML(caption_string),
              options = list(pageLength=100,
                             columnDefs=list(list(targets=c("전일대비율","비중","총수익률"), className="dt-right")),
                             dom='t')
            ) %>%
              formatCurrency(columns=c("한화평가금","한화매수가격","전일한화평가금","전일대비","총매수금","총수익금"),
                             currency="", mark=",", digits=0) %>%
              formatRound(columns=c("전일대비율","비중","총수익률"), digits=2) %>%
              formatString(columns=c("전일대비율","비중","총수익률"), suffix="%") %>%
              formatStyle(columns=c("전일대비","총수익금"),
                          color=styleInterval(c(-0.000001,0.000001), c("red","black","blue")),
                          fontWeight=styleInterval(0, c("bold","normal"))) %>%
              formatStyle(columns=c("전일대비율","총수익률"),
                          color=styleInterval(c(-0.000001,0.000001), c("red","gray","blue")),
                          fontWeight=styleInterval(0, c("bold","normal")))
          )
          
          
          # ---------- label_text 화면에 표시할 글자 ----------
          label_text <- paste0(
            "오늘평가액 : ", comma(round(sum_value, 0)), "원   ",
            "총수익 : ", comma(round(tail(dd$Profit, 1), 0)),"원",
            " (", round(tail(dd$Profit / sum_value, 1)*100, 2), "%)\n",
            "리스크(63D) Vol:", ifelse(is.na(today_vol63), "-", sprintf("%.2f%%", today_vol63*100)),
            "  DD:", ifelse(is.na(today_dd), "-", sprintf("%.2f%%", today_dd*100)),
            "  지속:", consecutive_days, "D",
            "  신규적립:", ifelse(GLD_MODE, "GLD", "정상"), "\n",
            "PA(연환산)  Return:", fmt_pct(pa_annret),
            "  Vol:", fmt_pct(pa_annvol),
            "  MDD:", fmt_pct(pa_mdd),
            "  Sharpe:", fmt_num(pa_sharpe),
            "  Calmar:", fmt_num(pa_calmar), "\n",
            "CVaR(95%) : ", ifelse(is.na(cvar_amt), "-", comma(round(cvar_amt, 0))), "원\n",
            "전일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
            "원 (",
            ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
            round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
            "%)  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)\n",
            "SPY_ETC:SCHD:QQQ:TQQQ:GOLD:IEF:CASH(목표(억)) = ",
            sprintf("%.1f", today_tsum * as.numeric(weights['SPY_ETC']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['SCHD']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['QQQ']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['TQQQ']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['GOLD']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['IEF']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(weights['CASH']/1e8)), "\n",
            
            "SPY_ETC:SCHD:QQQ:TQQQ:GOLD:IEF:CASH(현재(억)) = ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['SPY_ETC']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['SCHD']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['QQQ']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['TQQQ']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['GOLD']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['IEF']/1e8)), " : ",
            sprintf("%.1f", today_tsum * as.numeric(current_weights['CASH']/1e8)), "\n"
            
          )
          
          common_date_range <- range(dd_plot_base$Date, na.rm = TRUE)
          common_date_range[2] <- common_date_range[2] + 2
          
          
          
          # 창1 갱신
          #windows()
          suppressWarnings(source("pms_benchmark.R"))  # S&P500, NASDAQ100과 벤치마크 비교 : 한화면에 벤치마크 지수와 비교해줌
          
          # ---------- 상단 플롯(p) ----------
          month_start_label <- dd_plot_base %>%
            filter(!is.na(Sum)) %>%
            mutate(
              month = format(Date, "%Y-%m"),
              sum_left = Sum / 1e7
            ) %>%
            group_by(month) %>%
            slice_min(Date, n = 1, with_ties = FALSE) %>%
            ungroup()
          
          
          # ---------- 실제 Drawdown 기준 -5%, -10% 구간 음영 ----------
          dd_shade <- dd_plot_base %>%
            arrange(Date) %>%
            mutate(
              Peak = cummax(Sum),
              DD = Sum / Peak - 1,
              DD_zone = case_when(
                DD <= -0.10 ~ "DD_10",
                DD <= -0.05 ~ "DD_5",
                TRUE ~ NA_character_
              )
            ) %>%
            mutate(
              is_shade = !is.na(DD_zone),
              group = cumsum(is_shade != lag(is_shade, default = first(is_shade)))
            ) %>%
            filter(is_shade) %>%
            group_by(group, DD_zone) %>%
            summarise(
              xmin = min(Date),
              xmax = max(Date),
              .groups = "drop"
            )
          
          # ---------- 선형회귀선 대비 +5% 이상 과열 구간 음영 ----------
          lm_fit_over <- lm(I(Sum / 1e7) ~ as.numeric(Date), data = dd_plot_base)
          
          over_shade <- dd_plot_base %>%
            arrange(Date) %>%
            mutate(
              sum_left = Sum / 1e7,
              trend = predict(lm_fit_over, newdata = dd_plot_base),
              gap = sum_left / trend - 1,
              over_zone = if_else(gap >= 0.05, "OVER_5", NA_character_)
            ) %>%
            mutate(
              is_over = !is.na(over_zone),
              group = cumsum(is_over != lag(is_over, default = first(is_over)))
            ) %>%
            filter(is_over) %>%
            group_by(group, over_zone) %>%
            summarise(
              xmin = min(Date),
              xmax = max(Date),
              .groups = "drop"
            )
          
          p <- ggplot(dd_plot_base, aes(x = Date)) +
            
            # 선형회귀선 대비 +5% 이상: 희미한 붉은색
            geom_rect(
              data = over_shade,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "mistyrose",
              alpha = 0.35
            ) +
            
            # DD -5% 이하 구간
            geom_rect(
              data = dd_shade %>% filter(DD_zone == "DD_5"),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "gray80",
              alpha = 0.18
            ) +
            
            # DD -10% 이하 구간
            geom_rect(
              data = dd_shade %>% filter(DD_zone == "DD_10"),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "gray55",
              alpha = 0.25
            ) +
            
            geom_point(aes(y = sum_left, color = Profit / 1e7), size = 5, na.rm = TRUE) +
            geom_line(aes(y = sum_left, group = 1), color = "gray", na.rm = TRUE) +
            
            geom_smooth(
              aes(y = sum_left),
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "orange",
              linetype = "dashed",
              linewidth = 1
            ) +
            
            geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1, na.rm = TRUE) +
            geom_point(aes(y = a * ret_right + b), color = "green", size = 2, na.rm = TRUE) +
            geom_hline(yintercept = b, color = "yellow2", linewidth = 1.2, alpha = 0.6) +
            
            scale_color_gradient(
              low  = "#D55E00",
              high = "#0072B2",
              name = "손익\n(단위:\n천만원)"
            ) +
            
            scale_x_date(
              limits = common_date_range,
              date_breaks = "2 months",
              labels = scales::label_date_short(),
              expand = c(0, 0)
            ) +
            
            scale_y_continuous(
              name = "보유합계(천만원)",
              sec.axis = sec_axis(~ (. - b) / a, name = "일간수익률(%)")
            ) +
            
            labs(
              title = plot_title,
              subtitle = paste0("USD/KRW ", exchange_rate, " (", exchange_diff, ")"),
              x = NULL,
              y = NULL
            ) +
            
            theme_minimal(base_size = 13) +
            theme(
              plot.title.position = "plot",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
              axis.title.y.right = element_text(color = "green", size = 9, face = "bold"),
              legend.title = element_text(size = 9),
              legend.text  = element_text(size = 8)
            ) +
            
            coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
            
            annotate(
              "text",
              x = min(dd_plot_base$Date, na.rm = TRUE),
              y = max(sum_left, na.rm = TRUE),
              label = label_text,
              hjust = 0,
              vjust = 1,
              size = 3.5,
              color = "black"
            ) +
            
            annotate(
              "label",
              x = max(dd_plot_base$Date, na.rm = TRUE),
              y = min(sum_left, na.rm = TRUE) * 1.02,
              label = badge_text,
              hjust = 1,
              vjust = 0,
              size = 5.5,
              fontface = "bold",
              fill = badge_color,
              color = "white"
            ) +
            
            geom_text(
              data = month_start_label,
              aes(
                x = Date,
                y = sum_left,
                label = paste0(round(Sum / 1e8, 1), "억")
              ),
              vjust = 4,
              size = 3,
              color = "black",
              fontface = "bold",
              inherit.aes = FALSE
            )
          
          
          # ---------- 중단 누적수익(막대)+수익률(선) ----------
          dd_mid <- dd_plot_base %>%
            mutate(Profit_man = Profit / 1e7,
                   Return_pct = (Profit / Sum) * 100,
                   Status = ifelse(Profit_man >= 0, "Plus", "Minus"))
          
          range_profit <- range(dd_mid$Profit_man, na.rm = TRUE)
          range_return <- range(dd_mid$Return_pct, na.rm = TRUE)
          rescale_a <- diff(range_profit) / diff(range_return)
          rescale_b <- range_profit[1] - rescale_a * range_return[1]
          
          p_mid <- ggplot(dd_mid, aes(x = Date)) +
            geom_bar(aes(y = Profit_man, fill = Status), stat="identity",
                     width=1, alpha=0.5, na.rm=TRUE) +
            geom_hline(yintercept = rescale_b, color="gold", linewidth=0.8, alpha=0.6) +
            geom_line(aes(y = Return_pct * rescale_a + rescale_b),
                      color="#F4A261", linewidth=1, na.rm = TRUE) +
            scale_fill_manual(values=c("Plus"="dodgerblue4","Minus"="firebrick3")) +
            scale_x_date(limits = common_date_range,
                         date_breaks = "2 months",
                         labels = scales::label_date_short(),
                         expand = c(0, 0)) +
            scale_y_continuous(name="누적수익(천만원)", labels=scales::comma,
                               sec.axis = sec_axis(~ (. - rescale_b)/rescale_a, name="투자수익률(%)")) +
            labs(title = paste0("누적 성과 추이 (수익금: ", scales::comma(tail(dd_mid$Profit, 1)),
                                "원 / 수익률: ", sprintf("%.2f%%", tail(dd_mid$Return_pct, 1)), ")")) +
            theme_minimal(base_size=13) +
            theme(legend.position="none",
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.y.right=element_text(color="#F4A261", size=10, face="bold"),
                  panel.grid.minor=element_blank(),
                  plot.title=element_text(size=11, face="bold", hjust=0.5))
          
          # ---------- Drawdown 플롯 ----------
          dd2 <- dd_plot_base %>%
            mutate(Peak=cummax(Sum),
                   DD=ifelse(Peak>0, Sum/Peak - 1, 0))
          
          mdd_value <- min(dd2$DD, na.rm=TRUE)
          mdd_end_idx <- which.min(dd2$DD)
          mdd_end_date <- dd2$Date[mdd_end_idx]
          mdd_end_sum <- dd2$Sum[mdd_end_idx]
          mdd_start_idx <- which.max(dd2$Sum[1:mdd_end_idx])
          mdd_start_date <- dd2$Date[mdd_start_idx]
          mdd_start_sum <- dd2$Sum[mdd_start_idx]
          
          peak_label <- paste0("피크\n", scales::comma(mdd_start_sum), "원\n(", format(mdd_start_date), ")")
          trough_label <- paste0("바닥\n", scales::comma(mdd_end_sum), "원\n(", format(mdd_end_date), ")")
          
          vol_df <- data.frame(Date=as.Date(index(vol63_xts)),
                               Vol63=as.numeric(coredata(vol63_xts)))
          dd_plot <- dplyr::left_join(dd2, vol_df, by="Date") %>% drop_na()
          
          dd_range <- range(dd_plot$DD * 100, na.rm=TRUE)
          vol_range <- range(dd_plot$Vol63 * 100, na.rm=TRUE)
          scale_a <- diff(dd_range) / diff(vol_range)
          scale_b <- dd_range[1] - scale_a * vol_range[1]
          
          cur_dd_pct <- as.numeric(tail(dd_plot$DD, 1)) * 100
          cur_dd_amt <- as.numeric(tail(dd_plot$Sum, 1) - tail(dd_plot$Peak, 1))
          
          # DD는 (Sum/Peak - 1) 형태의 음수 값
          # 선을 그라데이션 세그먼트로 그리기 위한 데이터(전일→금일 구간)
          dd_seg <- dd_plot %>%
            arrange(Date) %>%
            mutate(
              Date_prev = lag(Date),
              DD_prev   = lag(DD),
              Vol_prev  = lag(Vol63)
            ) %>%
            filter(!is.na(Date_prev), !is.na(DD_prev), !is.na(DD))
          
          # 그라데이션에 사용할 "구간 대표값" (전일/금일 평균)
          dd_seg <- dd_seg %>%
            mutate(DD_mid = (DD_prev + DD) / 2)
          
          dd_min <- min(dd_seg$DD_mid, na.rm = TRUE)
          dd_max <- max(dd_seg$DD_mid, na.rm = TRUE)   # 보통 0 근처
          midpt  <- -0.06  #  0이 아니라 '약간 음수' (원하시면 -0.03 ~ -0.10 사이로 조절)
          
          p_dd <- ggplot() +
            #  DD 그라데이션 라인: 세그먼트로 연결
            geom_segment(
              data = dd_seg,
              aes(
                x = Date_prev, xend = Date,
                y = DD_prev * 100, yend = DD * 100,
                color = DD_mid
              ),
              linewidth = 2,
              lineend = "round"
            ) +
            
            # Vol(기존 유지: 보라색 점선)
            geom_line(
              data = dd_plot,
              aes(x = Date, y = scale_a * (Vol63 * 100) + scale_b),
              color = "purple", linewidth = 1, linetype = "dashed", na.rm = TRUE
            ) +
            
            geom_hline(yintercept = 0, color = "gray50") +
            geom_hline(yintercept = c(-5, -10, -15), linetype="dotted", color="gray70") +
            geom_vline(xintercept = c(mdd_start_date, mdd_end_date), linetype="dashed") +
            
            annotate("label", x = mdd_start_date, y = -2, label = peak_label,
                     size = 3.2, fill = "white") +
            annotate("label", x = mdd_end_date, y = (mdd_value*100)+5, label = trough_label,
                     size = 3.2, fill = "white") +
            annotate("label", x = mdd_end_date, y = (mdd_value*100)+10,
                     label = paste0("MDD: ", scales::percent(-mdd_value, accuracy=0.01)),
                     size = 3.2, fill = "white") +
            
            scale_x_date(
              limits = common_date_range,
              date_breaks = "2 months",
              labels = scales::label_date_short(),
              expand = c(0, 0)
            ) +
            scale_y_continuous(
              name = "Drawdown (%)",
              sec.axis = sec_axis(~ (. - scale_b)/scale_a, name = "63D Volatility (Annualized %)")
            ) +
            
            scale_color_gradient(
              low  = "red3",        # 아래쪽: 확실한 빨강
              high = "dodgerblue4", # 위쪽: 확실한 파랑
              limits = c(dd_min, dd_max)
            ) +
            
            labs(
              title = paste0(
                "Drawdown (현재: ", sprintf("%.2f%%", cur_dd_pct),
                ", 피크대비: ", scales::comma(cur_dd_amt), "원)"
              ),
              x = "날짜(연/월)"
            ) +
            theme_minimal(base_size = 13) +
            theme(
              axis.title.y.right = element_text(color = "purple", size = 9),
              axis.title.x       = element_text(size = 9),
              legend.position = "none"
            )
          
          
          # =========================================================
          #  비중 막대 그래프 (위: 목표 / 아래: 현재) 
          # =========================================================
          weight_bar_df <- data.frame(
            Asset = factor(
              c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권", "현금"),
              levels = c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권", "현금")
            ),
            Target = c(
              as.numeric(weights["SPY_ETC"]),
              as.numeric(weights["SCHD"]),
              as.numeric(weights["QQQ"]),
              as.numeric(weights["TQQQ"]),
              as.numeric(weights["GOLD"]),
              as.numeric(weights["IEF"]),
              as.numeric(weights["CASH"])
            ) * 100,
            Current = c(
              as.numeric(asset_SPY_ETC_ratio),
              as.numeric(asset_SCHD_ratio),
              as.numeric(asset_QQQ_ratio),
              as.numeric(asset_TQQQ_ratio),
              as.numeric(asset_GLD_ratio),
              as.numeric(asset_IEF_ratio),
              as.numeric(asset_CASH_ratio)
            )
          )
          
          weight_bar_long <- tidyr::pivot_longer(
            weight_bar_df,
            cols = c(Target, Current),
            names_to = "Type",
            values_to = "Weight"
          )
          
          weight_bar_long$Type <- factor(weight_bar_long$Type, levels = c("Target", "Current"))
          weight_bar_long$Asset <- factor(
            weight_bar_long$Asset,
            levels = c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권", "현금")
          )
          
          # 라벨(너무 작은 구간은 생략)
          weight_bar_long$label <- paste0(
            as.character(weight_bar_long$Asset),
            "(", sprintf("%.1f", weight_bar_long$Weight), "%)"
          )
          weight_bar_long$label[weight_bar_long$Weight < 3] <- ""
          
          p_weight_bar <- ggplot(weight_bar_long, aes(x = Type, y = Weight, fill = Asset)) +
            geom_col(
              width = 1.0,
              color = "white",
              linewidth = 0.6,
              position = position_stack(reverse = TRUE)
            ) +
            geom_text(
              aes(label = label),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              color = "black",
              size = 3.0,
              fontface = "bold"
            ) +
            coord_flip() +
            scale_y_continuous(
              limits = c(0, 100),
              labels = function(x) paste0(x, "%"),
              expand = c(0, 0)
            ) +
            scale_x_discrete(expand = c(0, 0), limits = rev(levels(weight_bar_long$Type))) +
            labs(
              title = "자산배분 현황 (위: 목표비중 / 아래: 현재비중)",
              x = NULL, y = NULL
            ) +
            theme_minimal(base_size = 12) +
            theme(
              legend.position = "none",
              axis.text.y = element_text(face = "bold", size = 11),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title  = element_text(face = "bold", hjust = 0.5, size = 11),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(2, 10, 2, 10)
            )
          
          
          # =========================================================
          # 게이지 함수 
          # =========================================================
          
          gauge_share_plot <- function(cur_val, max_val,
                                       title = "Gauge",
                                       cur_text = NULL,
                                       max_text = NULL,
                                       side_txt = NULL,
                                       ring_width = 6) {
            
            if (is.na(cur_val) || is.na(max_val) || max_val == 0) {
              return(
                ggplot2::ggplot() +
                  ggplot2::theme_void() +
                  ggplot2::annotate("text", x = 0, y = 0,
                                    label = paste0(title, "\n데이터 없음"),
                                    size = 5)
              )
            }
            
            ratio <- abs(cur_val) / abs(max_val)
            ratio <- max(0, min(1, ratio))
            
            needle_x <- -pi/2 - ratio * (pi/2)
            
            seg <- data.frame(
              x0  = c(-pi/2, -2*pi/3, -5*pi/6),
              x1  = c(-2*pi/3, -5*pi/6, -pi),
              y   = 1,
              col = c("#9BE37D", "#FFD84D", "#FFA24D")
            )
            
            needle_base <- data.frame(x = needle_x, y0 = 0.0, y1 = 0.3)
            needle_tip  <- data.frame(x = needle_x, y0 = 0.12, y1 = 0.95)
            
            if (is.null(cur_text)) cur_text <- as.character(cur_val)
            if (is.null(max_text)) max_text <- as.character(max_val)
            
            if (is.null(side_txt)) {
              side_txt <- sprintf("Current : %s\nMax(100%%): %s", cur_text, max_text)
            }
            
            share_label <- scales::percent(ratio, accuracy = 0.01)
            
            p_gauge <- ggplot2::ggplot() +
              ggplot2::geom_segment(
                data = seg,
                ggplot2::aes(x = x0, y = y, xend = x1, yend = y, colour = col),
                linewidth = ring_width,
                lineend = "butt"
              ) +
              ggplot2::scale_colour_identity() +
              ggplot2::geom_segment(
                data = needle_base,
                ggplot2::aes(x = x, y = y0, xend = x, yend = y1),
                linewidth = 3.0,
                lineend = "butt"
              ) +
              ggplot2::geom_segment(
                data = needle_tip,
                ggplot2::aes(x = x, y = y0, xend = x, yend = y1),
                linewidth = 1.3,
                arrow = grid::arrow(length = grid::unit(0.04, "npc"), type = "closed")
              ) +
              ggplot2::geom_point(ggplot2::aes(x = -pi/2, y = 0), size = 3.4) +
              ggplot2::geom_point(ggplot2::aes(x = -pi/2, y = 0), size = 1.2) +
              ggplot2::annotate("text", x = -pi/2,   y = 1.14, label = "0%",   size = 3.0, color = "gray") +
              ggplot2::annotate("text", x = -3*pi/4, y = 1.10, label = "50%",  size = 3.0, color = "gray") +
              ggplot2::annotate("text", x = -pi,     y = 1.10, label = "100%", size = 3.0, color = "gray") +
              ggplot2::annotate(
                "text",
                x = needle_x, y = 1.18,
                label = share_label,
                size = 3.3,
                fontface = "bold.italic",
                colour = "blue"
              ) +
              ggplot2::annotate(
                "text",
                x = -pi, y = 1.22,
                label = title,
                hjust = 0,
                vjust = 1,
                size = 4.2,
                fontface = "bold"
              ) +
              ggplot2::coord_polar(theta = "x", start = pi, direction = 1, clip = "off") +
              ggplot2::xlim(-pi, 0) +
              ggplot2::ylim(0, 1.25) +
              ggplot2::theme_void() +
              ggplot2::theme(
                plot.margin = ggplot2::margin(2, 0, 2, 0)
              )
            
            p_text <- ggplot2::ggplot() +
              ggplot2::theme_void() +
              ggplot2::annotate(
                "text",
                x = -0.9,
                y = 0,
                label = side_txt,
                hjust = 0,
                vjust = 0.5,
                size = 3.0,
                colour = "blue",
                fontface = "italic",
                lineheight = 0.95
              ) +
              ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), clip = "off") +
              ggplot2::theme(plot.margin = ggplot2::margin(2, 0, 2, 0))
            
            p_gauge + p_text + patchwork::plot_layout(widths = c(1.55, 0.70))
          }
          
          
          gauge_with_left_title <- function(gauge_plot, title_left,
                                            left_width = 0.55,
                                            right_width = 1.45) {
            
            p_left <- ggplot2::ggplot() +
              ggplot2::theme_void() +
              ggplot2::annotate(
                "text",
                x = 0,
                y = 0,
                label = title_left,
                hjust = 0,
                vjust = 0.5,
                size = 5,
                fontface = "bold"
              ) +
              ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), clip = "off") +
              ggplot2::theme(plot.margin = ggplot2::margin(6, 2, 6, 8))
            
            p_left + gauge_plot +
              patchwork::plot_layout(widths = c(left_width, right_width))
          }
          
          
          build_fear_greed_gauge <- function(dd2) {
            
            if (!"Sum" %in% names(dd2)) {
              return(
                ggplot2::ggplot() +
                  ggplot2::theme_void() +
                  ggplot2::annotate("text", x = 0, y = 0,
                                    label = "Fear & Greed\nSum 컬럼 없음",
                                    size = 5)
              )
            }
            
            y <- as.numeric(dd2$Sum)
            ok <- is.finite(y)
            y <- y[ok]
            
            if (length(y) < 30) {
              return(
                ggplot2::ggplot() +
                  ggplot2::theme_void() +
                  ggplot2::annotate("text", x = 0, y = 0,
                                    label = "Fear & Greed\n데이터 부족",
                                    size = 5)
              )
            }
            
            x <- seq_along(y)
            fit <- stats::lm(y ~ x)
            fitted_y <- as.numeric(stats::fitted(fit))
            resid_vec <- y - fitted_y
            
            cur_resid <- tail(resid_vec, 1)
            resid_sd  <- stats::sd(resid_vec, na.rm = TRUE)
            
            if (!is.finite(resid_sd) || resid_sd == 0) {
              resid_sd <- 1e-9
            }
            
            z <- cur_resid / resid_sd
            
            fg_score <- (z + 3) / 6 * 100
            fg_score <- max(0, min(100, fg_score))
            
            gap_pct <- (tail(y, 1) / tail(fitted_y, 1) - 1) * 100
            
            state_txt <- dplyr::case_when(
              z <= -3 ~ "극도의 공포",
              z <= -2 ~ "공포",
              z <= -1 ~ "약한 공포",
              z <   1 ~ "중립",
              z <   2 ~ "약한 탐욕",
              z <   3 ~ "탐욕",
              TRUE    ~ "극도의 탐욕"
            )
            
            action_txt <- dplyr::case_when(
              z <= -3 ~ "공격매수 검토",
              z <= -2 ~ "분할매수 우호",
              z <= -1 ~ "매수 가능",
              z <   1 ~ "중립 유지",
              z <   2 ~ "신규매수 축소",
              z <   3 ~ "매수 보류",
              TRUE    ~ "과열, 현금 대기"
            )
            
            side_txt <- sprintf(
              "Score : %.0f / 100\nz-score : %.2f\n괴리율 : %.1f%%\n상태 : %s\n판단 : %s",
              fg_score,
              z,
              gap_pct,
              state_txt,
              action_txt
            )
            
            gauge_share_plot(
              cur_val  = fg_score,
              max_val  = 100,
              title    = "Fear & Greed",
              cur_text = sprintf("%.0f", fg_score),
              max_text = "100",
              side_txt = side_txt
            )
          }
          
          
          build_risk_gauge_row <- function(today_dd,
                                           consecutive_days,
                                           cvar_amt,
                                           dd2,
                                           today_sum,
                                           ui_win = 63) {
            
            cur_sum_amt <- as.numeric(today_sum)
            
            dd_vec <- dd2$DD
            dd_vec <- dd_vec[is.finite(dd_vec)]
            
            if (length(dd_vec) < 5) {
              return(
                ggplot2::ggplot() +
                  ggplot2::theme_void() +
                  ggplot2::annotate("text", x = 0, y = 0,
                                    label = "DD 데이터 부족",
                                    size = 5)
              )
            }
            
            cur_dd <- as.numeric(today_dd)
            mdd    <- as.numeric(min(dd_vec, na.rm = TRUE))
            
            cvar_ratio <- NA_real_
            
            if (!is.na(cvar_amt) &&
                is.finite(cvar_amt) &&
                is.finite(cur_sum_amt) &&
                cur_sum_amt > 0) {
              cvar_ratio <- -abs(as.numeric(cvar_amt) / cur_sum_amt)
            }
            
            if (length(dd_vec) == 0 || is.na(tail(dd_vec, 1))) {
              cur_dur <- NA_integer_
            } else if (tail(dd_vec, 1) >= 0) {
              cur_dur <- 0L
            } else {
              i <- length(dd_vec)
              cur_dur <- 0L
              
              while (i >= 1 && !is.na(dd_vec[i]) && dd_vec[i] < 0) {
                cur_dur <- cur_dur + 1L
                i <- i - 1
              }
            }
            
            r <- rle(dd_vec < 0)
            max_dur <- if (any(r$values)) max(r$lengths[r$values]) else 0L
            if (max_dur == 0) max_dur <- 1L
            
            ui_roll <- zoo::rollapply(
              dd_vec,
              width = ui_win,
              align = "right",
              fill = NA_real_,
              FUN = function(x) sqrt(mean(x^2, na.rm = TRUE))
            )
            
            cur_ui <- as.numeric(tail(ui_roll, 1))
            max_ui <- suppressWarnings(max(as.numeric(ui_roll), na.rm = TRUE))
            
            if (!is.finite(max_ui) || max_ui == 0) {
              max_ui <- 1e-9
            }
            
            g1 <- gauge_share_plot(
              cur_dd,
              mdd,
              title    = "DD vs MDD",
              cur_text = scales::percent(cur_dd, accuracy = 0.01),
              max_text = scales::percent(mdd, accuracy = 0.01)
            )
            
            if (is.na(cvar_ratio)) {
              g2 <- ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate("text", x = 0, y = 0,
                                  label = "CVaR 데이터 없음",
                                  size = 5)
            } else {
              g2 <- gauge_share_plot(
                cur_dd,
                cvar_ratio,
                title    = "DD vs CVaR",
                cur_text = scales::percent(cur_dd, accuracy = 0.01),
                max_text = scales::percent(cvar_ratio, accuracy = 0.01)
              )
            }
            
            g3 <- gauge_share_plot(
              cur_dur,
              max_dur,
              title    = "DD Duration",
              cur_text = paste0(cur_dur, "D"),
              max_text = paste0(max_dur, "D")
            )
            
            g4 <- gauge_share_plot(
              cur_ui,
              max_ui,
              title    = paste0("Ulcer(", ui_win, "D)"),
              cur_text = ifelse(
                is.na(cur_ui),
                "NA",
                scales::percent(cur_ui, accuracy = 0.01)
              ),
              max_text = scales::percent(max_ui, accuracy = 0.01)
            )
            
            g5 <- build_fear_greed_gauge(dd2)
            
            patchwork::wrap_plots(g1, g2, g3, g4, g5, nrow = 1)
          }
          
          
          g_row <- build_risk_gauge_row(
            today_dd,
            consecutive_days,
            cvar_amt,
            dd2,
            today_sum,
            ui_win = 63
          )
          
          
          
          
          if (risk_ready) {
            combined_plot <- (p / p_mid / p_dd / p_weight_bar / g_row) +
              patchwork::plot_layout(heights = c(2.2, 1, 1, 0.40, 0.65))
          } else {
            combined_plot <- (p / p_mid / p_weight_bar / g_row) +
              patchwork::plot_layout(heights = c(2.2, 1, 0.40, 0.65))
          }
          
          # 창2 갱신
          #windows() 
          suppressMessages(print(combined_plot))
          
          # PDF 저장
          date_str <- format(Sys.Date(), "%Y%m%d")
          out_dir  <- "reports"
          dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
          pdf_file <- file.path(out_dir, sprintf("Daily_Risk_%s.pdf", date_str))
          if (file.exists(pdf_file)) file.remove(pdf_file)
          
          ggsave(filename = pdf_file, plot = combined_plot, width = 11.69, height = 8.27, device = cairo_pdf)
          cat("Saved:", pdf_file, "\n")
          
          cat(sprintf(
            "\n[RISK-OFF CHECK] 63D Vol=%s, DD=%s, 지속=%d거래일 → GLD_MODE=%s\n\n",
            ifelse(is.na(today_vol63), "-", sprintf("%.2f%%", today_vol63 * 100)),
            ifelse(is.na(today_dd),   "-", sprintf("%.2f%%", today_dd * 100)),
            consecutive_days,
            ifelse(GLD_MODE, "ON", "OFF")
          ))
        }
        
        print(tail(dd %>% select(Date, Sum, Profit, Return_TWR, Return_NAV), 2))
      }
    }
    
  }, error = function(e) {
    cat("[ERROR] 루프 1회차 실행 중 에러 발생: ", conditionMessage(e), "\n", sep="")
  })
  
  cat("장중 10분 그이외는 1시간 후에 다시 실행됨(중단: Interrupt-R 빨간버튼) ",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"), "\n\n", sep="")
  
  count <- count + 1
  
  now <- Sys.time()
  if (exists("is_initial_mode") && as.numeric(difftime(now, last_update_time, units="secs")) >= UPDATE_EVERY_SEC && !is_initial_mode) {
    if (exists("dd") && exists("sum_xts")) {
      gld_mode_now <- if (exists("GLD_MODE")) isTRUE(GLD_MODE) else FALSE
      badge_text <- make_badge_text(sum_xts, gld_mode_now)
      
      warnings_vec <- if (exists("warnings_vec")) warnings_vec else character(0)
      errors_vec   <- if (exists("errors_vec"))   errors_vec   else character(0)
      
      prompt_text <- make_gemini_prompt_pms(
        dd = dd,
        sum_xts = sum_xts,
        badge_text = badge_text,
        fund_name = fund_name,
        take_last_n_days = 2,
        cvar_amt = cvar_amt,
        pa_mdd   = pa_mdd
      )
      
      changed <- save_if_changed(prompt_text, PROMPT_FILE)
      if (changed) message("[Prompt Updated] ", PROMPT_FILE, " @ ", format(now, "%H:%M:%S"))
      last_update_time <- now
    }
  }
  
  if (!REPEAT_FLAG) break
  
  
  # 종목을 좀 묶어서 보기 위해 자산군별 정의하여 통계를 내보자.
  
  print(
    rt %>%
      mutate(
        종목그룹 = case_when(
          grepl("나스닥100", 종목명) | grepl("QQQM", 종목명) ~ "나스닥100ETF",
          
          grepl("S&P500", 종목명) | grepl("SPYM", 종목명) | grepl("IVV", 종목명) ~ "미국S&P500ETF",
          
          grepl("KODEX종합채권액티브ETF", 종목명) |
            grepl("KODEX미국30년국채액티브", 종목명) |
            grepl("ACE미국30년국채액티브\\(H\\)", 종목명) |
            grepl("TIGER미국테크TOP10채권혼합", 종목명) |
            grepl("삼성전자SK하이닉스채권혼합50", 종목명) ~ "채권형ETF",
          
          grepl("KODEX 머니마켓액티브", 종목명) |
            grepl("TIGER KOFR금리액티브", 종목명) |
            grepl("RISE KOFR금리액티브", 종목명) |
            grepl("^BIL$", 종목명) |
            grepl("^SGOV$", 종목명) ~ "현금성ETF",
          
          TRUE ~ 종목명
        )
      ) %>%
      group_by(종목그룹) %>%
      summarise(
        총평가금 = sum(한화평가금 / 1e6),
        총매수금 = sum(총매수금 / 1e6),
        전일대비 = sum(전일대비 / 1e6),
        총수익금 = sum(총수익금 / 1e6),
        .groups = "drop"
      ) %>%
      mutate(
        수익률 = round(총수익금 / 총매수금 * 100, 2),
        비중 = round(총평가금 / sum(총평가금, na.rm = TRUE) * 100, 2),
        수익기여도 = round(총수익금 / sum(총수익금, na.rm = TRUE) * 100, 2),
        효율 = round(수익기여도 / 비중, 2)
        # 효율 해석
        # 1보다 크면: 비중 대비 수익기여도가 높음
        # 1이면: 평균 수준
        # 1보다 작으면: 비중 대비 수익기여도가 낮음
      ) %>%
      filter(비중 >= 1) %>%
      arrange(desc(총평가금)) %>%
      rename(종목명 = 종목그룹),
    n = Inf
  )
  
  
  wait_min <- if (in_fast_range & (wday >= 1 & wday <= 5)) 10 else 60
  Sys.sleep(wait_min * 60)
}

# 배치파일로 실행될때는 강제 종료
# 인터렉티브 모드(RStudio 등)가 아닐 때만 R 세션을 종료
if (!interactive()) {
  quit(save = "no")
}

# 프로그램 후기 :
# 이 프로그램은 “수익을 만들어주는 엔진”이 아니라
# “수익을 망가뜨리는 행동을 제거해서
# 결과적으로 장기 수익을 극대화하는 장치”입니다.

# [부록]
# 소형 자산운용사에서 쓰는 흔한 시스템 구성과 비교
# 이 프로그램은 아마추어 개인 투자자가 쓰는 소형 프로그램이지만 자산운용사에서 쓰는 솔루션과 비교해봄
#
# 1) Front: 주문·체결(OMS/EMS)
# 포트폴리오에서 나온 “매수/매도 의사결정”을 주문으로 바꾸고, 체결을 관리하는 시스템(Order Management가 전략과 실행을 잇는 다리라는 설명이 보편적) 
# 보통은 별도 솔루션(예: Eze/EQS류 등)이나 브로커 시스템을 씀
# 
# 2) Middle: 포트폴리오·리스크·성과(PMS + Risk + Performance/Attribution)
# 포지션/평가/리스크(예: VaR·Stress) + 성과분석(연환산, MDD, 샤프, 칼마) + 성과기여/요인분해가 들어감
# 상용 솔루션 예시로는 SimCorp(리스크·성과/어트리뷰션), FactSet(포트폴리오 분석·리스크·성과) 같은 것들이 대표적
# 초대형 쪽은 BlackRock Aladdin 같은 통합 리스크/포트폴리오 플랫폼을 쓰기도 함(멀티자산 리스크·분석을 강조). 
# 
# 3) Back: 회계·기준가(NAV)·정산·리컨실리에이션
# “운용”보다 더 귀찮고 더 중요한 영역: 기준가 산출, 거래 정산, 수수료, 세무, 기업행사(CA), 데이터 정합성.
# 여기서 많이 쓰이는 이름 중 하나가 Advent Geneva 같은 “포트폴리오 회계/리포팅” 계열입니다. 
#
# 자산운용사에서 쓰는 솔루션과 이 프로그램과 공통 내용 :
# Middle 오피스 영역은 매우 비슷. 특히 아래는 “운용사 데일리 리스크 팩”(전날 대비 포트폴리오의 위험 상태가 변했는지 여부를 한 장으로 판단하기 위한 운용사 내부 보고서)과 비슷
# 일일 누적(펀드/계좌 NAV) + 리포트 자동 생성
# MDD/샤프/칼마 등 성과 요약
# Stress replay(과거 위기 리플레이), 몬테카를로, 미래 MDD 분포
# CVaR 같은 꼬리위험 지표
# 팩터 회귀 / PCA로 “무슨 위험으로 벌었나” 분해
# 이 조합 자체가 상용 솔루션들이 강조하는 “리스크+성과+어트리뷰션(원인분해)” 방향과 일치
# 
# 자산운용사 대비 이 프로그램에 없는 것(각종 규제로 인해 자산운용사가 필수적으로 갖추어야 하는 것들) :
# 주문/체결/사후감시(컴플라이언스)
# 투자한도, 금지종목, 이해상충, pre-trade / post-trade 룰
# 회계/NAV 공정성 + 리컨실리에이션(같은 자산을 서로 다른 장부(내부 vs 외부)가 같은 숫자로 보고 있는지 맞춰보는 작업)
# 브로커/수탁/내부 장부 대사, 기업행사 반영, 가격 소스 관리
# 권한/감사추적(Audit trail) : “누가 언제 무엇을 바꿨나” 기록이 필수
# 펀드별/고객별 템플릿, 공시 수준의 일관성
