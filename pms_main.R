###############################################
# PMS(Portfolio Monitoring System) 1.0 / 2025-12-25 메인 스크립트 (루프 버전)
# - stock_eval.R / stock_eval_us.R 필요(국내, 미국 주식 데이터 수집 모듈)
# - risk_module.R 필요(리스크관리 함수 모음)
#   . risk_module.R의 몬테카, MDD, 인출, 팩터, PCA를 모두 호출
# 입력 파일
#         input_stock.csv    : 한국주식
#         input_stock_us.csv : 미국주식
# 출력 파일
#         output_stock_{YYYY-MM-DD}.xlsx      : 한국주식 평가액
#         output_stock_us_{YYYY-MM-DD}.xlsx   : 미국주식 평가액
#         output_sum.csv                      : 평가액총액, 수익금
#                                               (최소 100일이상 데이터 필요)
#         reports/Daily_Risk_{YYYYMMDD}.pdf   : 1페이지 그래프 보고서
#         reports/gemini_prompt.txt           : 제미나이 질의어(프롬프트)
# - 누적 데이터(output_sum.csv)가 100일이 안되면 리스크 관리 분석은 생략
# 주) 리스크 및 운용 성과 평가는 TWR 기준,계좌 증감 및 체감 성과 표시는 NAV 기준으로 해석(형식적으로는 NAV 기반, 개념적으로는 TWR(Time-Weighted Return)에 해당)
###############################################
# 이 코드는 "돈을 얼마나 벌었는지를 관리하지 않는다. 망하지 않을 구조만 관리한다."
# 위험관리 핵심 3대 지표 : MDD · CVaR · Risk-Off 3개

# =========================================================
# 패키지 설치/로드
# =========================================================
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr",
         "PerformanceAnalytics", "showtext", "zoo", "tidyr", "quantmod", 
         "xts", "rugarch", "htmltools")  

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(readr);   library(readxl); library(showtext)
library(openxlsx); library(rvest); library(httr)
library(dplyr);   library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)
library(zoo); library(tidyr); library(quantmod); library(xts)
library(rugarch); library(htmltools)


# =========================================================
# 개인별 세팅 변수 : 최초에 세팅하면 됨
# =========================================================
wd        <- "c:\\PMS_Core"     # 작업디렉토리
fund_name <- "JS Fund"     # 펀드/계좌 이름(멋진 이름으로 지어보자)

# 각 종목군별 비율 : 합해서 1로 만듦
# 한 번 정하면 장기간 변경하지 않을 각오를 해야 함
weights <- c(
  0.40,  # SPY등(SPY를 비롯한 아래 종목군에 속하지 않는 여러 종목)
  0.20,  # SCHD(배당주를 꾸준히 증가시키는 우량주)
  0.15,  # QQQ(나스닥 기술주)
  0.10,  # TQQQ(QQQ의 3배 레버리지)
  0.10,  # GOLD(금)
  0.05   # IEF(채권)
)
# =========================================================
# 개인별 세팅 끝
# =========================================================

# 이름으로 참조가 가능하도록 setNames
# 조회 예시) as.numeric(weights["SPY_ETC"])
weights <- setNames(weights, c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF"))

setwd(wd)
options(scipen = 999)  # 숫자의 과학적표기법 방지

# 리스크 + 팩터 + PCA 모듈 로드
source("risk_module.R")

# 월간 자산수익률 + 팩터 CSV 업데이트(모듈 제공 함수)
suppressWarnings(
  try(update_factor_data(), silent = TRUE)
)

# =========================================================
# 실행 제어 변수
# =========================================================
count <- 1
last_mc_date <- as.Date(NA)

week_kor <- c("일", "월", "화", "수", "목", "금", "토")

# 초기 구간 최소 표본수 (달력일 기준이 아니라 "실제 기록된 날짜 수" 기준)
min_days_for_risk <- 100

# 폰트
font_add(family = "malgun", regular = "C:/Windows/Fonts/malgun.ttf")
showtext_auto()

# 특정의미를 갖는 열을 추가하는 함수
add_twr_return_to_dd <- function(dd, ret_clip = 0.5, flow_deadband = 1000) {
  suppressMessages(library(dplyr))
  dd <- dd %>% arrange(Date)
  
  dd <- dd %>%
    mutate(
      Invested     = Sum - Profit,  # 누적 투자 원금 (Net Investment)
      Invested_lag = lag(Invested), # 전일 누적 투자 원금
      Sum_lag      = lag(Sum),      # 전일 자산 평가 가액, 전일(Previous Day)의 원금 및 총 자산 가치
      # 어느 투자자의 상황을 가정해서 설명 :
      # 그저께 밤: 통장에 1,000만 원(원금)이 있었고 주식 가치도 1,000만 원
      # 어제: 주가가 올라서 자산이 1,100만 원이 되었습니다. (수익 100만 원 발생)
      # 오늘 아침: 투자자가 200만 원을 추가로 입금했습니다.
      # 이 상황에서 오늘 자 수익률 계산을 위해 불러온 lag 값들은 다음과 같습니다.
      # 항목	값	설명
      # Invested_lag	1,000만 원	어제까지 내가 실제로 넣은 돈 (수익금 100만 원은 제외됨)
      # Sum_lag	1,100만 원	어제 밤 내 계좌에 찍힌 총 금액 (원금 + 수익금)
      
      Flow_raw     = Invested - Invested_lag,  # 오늘 원금이 어제보다 얼마나 늘었거나 줄었는지
      Flow         = if_else(!is.na(Flow_raw) & abs(Flow_raw) <= flow_deadband, 0, Flow_raw),
      #  필터링된 자본 흐름 (Noise-Filtered Flow), 배당금 입금, 아주 작은 수수료 차감 등 운용 성과와 직접 관련 없는 미세한 잡음을 제거하여 수익률 계산의 정확도를 높이기 위함
      Gross_base   = Sum_lag + Flow, # 수정 기초 자산 (Adjusted Beginning Value)
      
      Return       = if_else(!is.na(Gross_base) & Gross_base > 0,
                             Sum / Gross_base - 1,
                             NA_real_)  # 일간 시간가중수익률 (Daily TWR)
    ) %>%
    mutate(Return = if_else(!is.na(Return) & abs(Return) < ret_clip, Return, NA_real_))
  
  return(dd) 
}


# 프롬프트 엔지니어링을 이용하여 자연어로 운용성과를 보고 받음
# ============================================================
# PMS -> Gemini prompt 주기적 저장 
# ============================================================
dir.create("reports", showWarnings = FALSE)

# ---- 1) 프롬프트 생성 함수 (리스크 지표 포함 버전) ----
make_gemini_prompt_pms <- function(dd, sum_xts, badge_text = NULL,
                                   fund_name = "JS Fund",
                                   report_time_kst = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   flow_text = "금일 Flow(입출금): 0원 / 매수·매도: 없음(거래 0건)",
                                   risk_metrics = NULL,
                                   warnings_vec = character(0),
                                   errors_vec   = character(0),
                                   take_last_n_days = 2) {
  
  dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
  dd_tail <- tail(dd, take_last_n_days)
  tab_txt <- utils::capture.output(print(dd_tail))
  
  badge_txt <- if (!is.null(badge_text) && nzchar(badge_text)) badge_text else "(미제공)"
  warn_txt  <- if (length(warnings_vec) > 0) paste0("- ", warnings_vec, collapse = "\n") else "(없음)"
  err_txt   <- if (length(errors_vec)  > 0) paste0("- ", errors_vec,  collapse = "\n") else "(없음)"
  
  # 리스크 지표 텍스트
  risk_txt <- "(미제공)"
  if (!is.null(risk_metrics)) {
    if (is.data.frame(risk_metrics)) {
      risk_txt <- paste(utils::capture.output(print(risk_metrics)), collapse = "\n")
    } else if (is.list(risk_metrics)) {
      nm <- names(risk_metrics)
      lines <- paste0("- ", nm, " = ", sapply(risk_metrics, function(x) {
        if (length(x) == 0 || is.na(x)) return("NA")
        if (is.numeric(x)) return(formatC(x, digits = 6, format = "fg", flag = "#"))
        as.character(x)
      }))
      risk_txt <- paste(lines, collapse = "\n")
    } else {
      risk_txt <- as.character(risk_metrics)
    }
  }
  
  # 실제 운용을 해보니 아래의 자연어로 된 Prompt Engineering도 무척 중요
  # 제미나이가 사용자가 듣기 좋은 말만 하거나 환각에 빠지지 않도록 규칙을 잘 정해야 함
  # 사용법 : reports/gemini_prompt.txt 를 오픈하고 전체 텍스트를 복사해서 제미나이에게 물어 본다.
  # chatgpt, gemini의 API 를 이용하는 방법도 있으나 너무 복잡하고 유료서비스라 간단하게 Prompt만 생성함
  paste0(
    
"[Fund Name] : ", fund_name, "\n",
"[Report Time] : ", report_time_kst, "\n",
"
(KST) 당신은 **“기관 자산운용사(연기금/헤지펀드) 출신의 수석 펀드매니저”**입니다.

아래 Portfolio Management System(PMS) 출력 데이터만을 근거로,

‘오늘의 투자운용 현황’에 대한 일일 운용 코멘트를 전문적으로 작성하세요.

[필수 규칙]

맨위 제목에 [Fund Name]을 책임지고 있는 수석 펀드매니저입니다. 아래와 같이 현재 자금운용상황 보고드립니다. 라고 넣어줘.

예측·단정 금지
입력 데이터에 없는 지표(예: Ulcer Index 등)는 절대 언급하지 말 것.

행동 지시 금지
매수·매도·비중 조정·투자 권유 등 행동을 유도하거나 지시하는 표현 금지
(해석·상태 설명만 허용).

**Return_NAV(계좌 변화)**와 **Return_TWR(운용 성과)**의 차이를 반드시 설명할 것.

많이 차이나면 원달러 환율의 급등과 급락 등의 요인이 주 원인일 수 있다고 밝히되 확정적으로 표현하지 말것.

핵심 판단 축은 **운용 상태(신호등 배지)**와 Drawdown(DD) 및 리스크 지표임.

숫자는 아래 입력 데이터에서만 인용, 단위(원/%) 정확히 표기할 것.

입력 데이터에서 Flow가 명시되지 않는 경우 Flow 관련 결론은 유보.

존댓말, 간결하고 기관 운용 리포트 톤 유지.

**Flow(입출금/매매)**는 입력 데이터에 명시되지 않으면 언급하지 말 것.

입력 데이터와 모순되는 원인(예: Flow=0인데 자금 유출 확정 서술)은
절대 ‘확정’으로 표현하지 말 것.

입력으로 확인되지 않은 원인은 반드시
**“가능성” 또는 “확인 불가”**로만 표현할 것.

성과 요약 시
Return_NAV과 Return_TWR의 차이가 존재하면,
아래 순서로 **‘원인 후보’**를 나열할 것:

(1) Flow(현금흐름)
(2) 배당/세금/수수료 반영 시점
(3) 환율/헤지
(4) 데이터 소스/반올림

이 펀드는 실제 현금 유입·유출은 없으며,성과 요약표에 산출된 Flow 수치는
계산상 분리된 외부 요인으로 해석해야 함

단, **입력 데이터로 명시적으로 확인된 항목만 ‘확정’**으로 서술하고,
그 외는 반드시 **“가능성” 또는 “확인 불가”**로 표현할 것.

입력 데이터에 명시되지 않은 사건
(예: 전일 자금 이동, 회계적 이월 처리, 시스템 내부 보정 등)은
절대 가정하거나 서술하지 말 것.

입력 데이터와 모순되는 원인에
‘추정’, ‘확정’, ‘결정적’ 등의 표현 사용 금지.

원인 분석이 불가능한 경우,
반드시 **“원인 불명(데이터 부족)”**으로 결론을 유보할 것.

Flow 관련 보조 규칙:
입력 데이터 상 **‘금일 Flow = 0원’**으로 확인되며,
이에 반하는 추가 자금 이동의 근거는 입력에 존재하지 않음.
→ 따라서 NAV–TWR 괴리의 원인으로 확정할 수 없음.

데이터 미제공 또는 확인 불가 상태에서는
긍정·부정 평가를 내리지 말고,
반드시 **‘평가 유보’**로 표현할 것.

[출력 형식]

A) 현재 보고 시각과 오늘 한 줄 결론

B) 운용 상태(GREEN / YELLOW / RED)와 근거

상태 분류와 그 근거만 설명할 것

GREEN일 경우에도 행동 권유 문구는 포함하지 말 것

“추가적인 개입 신호는 관찰되지 않음” 수준으로 표현

C) 성과 요약(Return_NAV vs Return_TWR)

차이가 있을 경우, 원인을
(1) Flow → (2) 배당/세금/수수료 → (3) 환율/헤지 → (4) 데이터 소스/반올림
순서로 제시

입력으로 확인된 것만 ‘확정’, 나머지는 ‘가능성’ 또는 ‘확인 불가’

D) 리스크 / 드로다운 요약 (DD + MDD)

DD 현재 수준

관측 기간 내 MDD 위치 설명

평가가 아닌 상태 진술 중심

E) 위험관리 지표 설명

입력으로 제공된 모든 리스크 지표를 빠짐없이 1줄씩 설명

“높을수록/낮을수록 바람직한지”는 일반적 정의로만 설명

수치 미제공 시 평가는 유보

F) 데이터 / 운영상 이슈 (경고 / 오류)

G) 원칙 리마인드 (규칙 유지 / 행동 없음)

사전 정의된 운용 규칙에 따른 상태 유지 문구만 사용

H) 시장 지표 요약

오늘의 미국 주식시장 상황 1줄

현재 원·달러 환율 숫자 + 환율 동향 1줄, 환율은 전일대비 상승 하락분을 숫자로 표현

환율은 공신력있는 기관을 조회해서 참조할 것

미국 IEF ETF가 참고하는 미국채 수익률을 숫자로 1줄

I) 오늘의 주식투자 격언을 랜덤하게 골라서 1줄

상기 내용을 요약 정리하여 3줄로 표현


<입력 데이터>

=== [0] 금일 자금흐름/거래 여부(중요) ===
", flow_text, "

=== [1] 배지/상태 메시지 ===
", badge_txt, "

=== [2] 성과 요약표 (최근 ", take_last_n_days, "일) ===
", paste(tab_txt, collapse = "\n"), "

=== [3] Drawdown(현재) ===
DD_now = ", sprintf("%.6f", dd_now), "
(예: -0.120000 은 고점 대비 -12%)

=== [3-1] 리스크 지표(PMS 계산값: 아래 항목 전부 설명) ===
", risk_txt, "

=== [4] Warnings ===
", warn_txt, "

=== [5] Errors ===
", err_txt, "

위 입력만으로 작성하세요. 맨위의 [Fund Name], [Report Time]문구 자체는 표시하지 말기 바람.
최고의 개인투자 포트폴리오 모니터링 시스템을 통해 관찰하고 있다는 안내문으로 끝내줘.
"
  )
}


# 변경 시에만 저장 ----
save_if_changed <- function(text, file_path) {
  old <- if (file.exists(file_path)) paste(readLines(file_path, warn = FALSE), collapse = "\n") else ""
  if (!identical(old, text)) {
    writeLines(text, file_path, useBytes = TRUE)
    return(TRUE)
  }
  FALSE
}

# 배지 계산 함수(신호등) ----
make_badge_text <- function(sum_xts, GLD_MODE) {
  # MDD 15%이하이고 변동성이 25%이상인 날이 연속적으로 63일 이상이면 GLD_MODE가 TRUE가 되고, 이 경우포트폴리오 신규 매수는 중단하고 GOLD만 매수함
  # 평생 몇 번 되지않을 낮은 확률로 예상하며 시장이 무너지는 경우에 해당할 것으로 예상
  dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
  
  if (isTRUE(GLD_MODE)) {
    "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
  } else if (!is.na(dd_now) && dd_now <= -0.12 && dd_now > -0.20) {
    "현재 운용 상태 :  CAUTION  (DD 12~20% · 주의 관찰)"
  } else {
    "현재 운용 상태 :  NORMAL  (Risk-Off : OFF)"
  }
}

# 주기적 저장 설정 ----
PROMPT_FILE <- file.path("reports", "gemini_prompt.txt")
UPDATE_EVERY_SEC <- 10  # ★ 10초마다(원하면 30, 60으로 바꾸세요)

last_update_time <- Sys.time() - 9999  # 첫 루프에서 바로 저장되게


# =========================================================
# 반복 루프 시작(중단을 원하면 Interrupt-R 빨간버튼 클릭)
# =========================================================
repeat {
  
  now  <- as.POSIXct(Sys.time())
  hhmm <- format(now, "%H:%M")
  wday <- as.numeric(format(now, "%u"))  # 1=월 ~ 7=일
  in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"
  
  cat("[", count, "회차] ", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      " : 실행 시작***********************************************\n", sep="")
  
  # 한 바퀴 에러가 전체 루프를 죽이지 않도록 
  tryCatch({
    
    # =========================================================
    # 현재 보유자산 평가 업데이트
    # =========================================================
    # stock_eval.R, stock_eval_us.R에서 아래 객체들이 생성된다고 가정:
    # - data_ko, data_en, exchange_rate, exchange_diff
    suppressWarnings(source("stock_eval.R"))    # 한국주식 현재가 가져와서 엑셀로 저장
    suppressWarnings(source("stock_eval_us.R")) # 미국주식 현재가 가져와서 엑셀로 저장
    
    today <- Sys.Date()
    
    file1 <- paste0("output_stock_",    today, ".xlsx")
    file2 <- paste0("output_stock_us_", today, ".xlsx")
    output_file <- "output_sum.csv"
    
    column_name  <- "평가금"
    column_name2 <- "수익금"
    
    # 혹시 파일이 아직 생성되지 않았으면 다음 루프로
    if (!file.exists(file1) || !file.exists(file2)) {
      cat("[경고] 오늘 평가 파일이 아직 없습니다. (", file1, ", ", file2, ")\n", sep="")
      goto_sleep <- TRUE
    } else {
      goto_sleep <- FALSE
    }
    
    if (!goto_sleep) {
      
      data1 <- read_excel(file1)
      data2 <- read_excel(file2)
      
      last_value1   <- tail(data1[[column_name]],  1)
      last_value1_2 <- tail(data1[[column_name2]], 1)
      
      last_value2   <- tail(data2[[column_name]],  1)
      last_value2_2 <- tail(data2[[column_name2]], 1)
      
      sum_value    <- round(last_value1 + last_value2, 0)
      profit_value <- round(last_value1_2 + last_value2_2, 0)
      
      yegum   <- 0  # 수동으로 추가해야 할 예금이 있다면 기재
      sum_value <- round(sum_value + yegum, 0)
      
      result <- data.frame(Date = today, Sum = sum_value, Profit = profit_value)
      
      # =========================================================
      # output_sum.csv 갱신 (날짜별 평가액합산, 누적수익 데이터)
      # =========================================================
      if (file.exists(output_file)) {
        existing_data <- read_csv(output_file,
                                  col_types = cols(
                                    Date   = col_date(format = ""),
                                    Sum    = col_double(),
                                    Profit = col_double()
                                  ),
                                  show_col_types = FALSE)
        
        if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
          existing_data <- existing_data[-nrow(existing_data), ]
        }
        updated_data <- bind_rows(existing_data, result)
      } else {
        updated_data <- result
      }
      
      write_csv(updated_data, output_file)
      
      # 100일 이하면 "리스크 분석만" 생략
      is_initial_mode <- (nrow(updated_data) < min_days_for_risk)
      
      # =========================================================
      # 분석용 데이터 재읽기 + Return 계산
      # =========================================================
      dd <- readr::read_csv(
        output_file,
        col_types = readr::cols(
          Date   = readr::col_date(format = ""),
          Sum    = readr::col_double(),
          Profit = readr::col_double()
        ),
        show_col_types = FALSE
      ) %>% arrange(Date)
      
      # 시간가중수익률(시간가중수익률, Time-Weighted Rate of Return) 변수 추가
      # 펀드 매니저나 투자 상품의 순수한 운용 실적을 평가할 때 전 세계적으로 가장 널리 통용되는 표준 지표
      # 특정 투자 기간을 현금 유입이나 유출이 발생한 시점을 기준으로 여러 개의 '하위 기간(Sub-periods)'으로 나누어 각 기간의 수익률을 독립적으로 계산한 뒤, 이를 복리로 연결하여 전체 기간의 수익률을 산출하는 방식
      # TWR은 자금의 유출입 규모와 상관없이, 매니저가 운용하는 1원당 가치가 어떻게 변했는지를 추적
      dd <- add_twr_return_to_dd(dd)
      dd <- dd %>% rename(Return_TWR = Return)
      
      # 분석 시에는 Return NA 제거한 버전 사용 권장
      dd_ret <- dd %>% dplyr::filter(!is.na(Return_TWR))
      
      # =========================================================
      #  초기 구간(100일 이하) 리스크 분석 게이트
      # =========================================================
      # "결측치를 제외하고 실제 데이터가 존재하는 날짜(Date)가 총 며칠인지" 그 고유한 개수를 파악
      dd_daily_n <- dd %>% distinct(Date) %>% filter(!is.na(Date)) %>% nrow()
      
      # 일정기간 데이터가 쌓이지 않은 경우 실행시키지 않기 위함, FALSE이면 데이터 표본수 부족
      risk_ready <- dd_daily_n >= min_days_for_risk
      
      # =========================================================
      # PerformanceAnalytics 준비 (sum_xts, ret_xts)
      # =========================================================
      # 시간가중수익률(TWR)이나 성과 분석 패키지(예: PerformanceAnalytics)를 활용하기 위한 필수적인 전처리 단계
      dd_daily <- dd %>%
        group_by(Date) %>%
        summarise(Sum = last(Sum), .groups="drop") %>%
        arrange(Date)
      
      # timeBased/empty 방지
      if (nrow(dd_daily) >= 2 && all(!is.na(dd_daily$Date)) && all(!is.na(dd_daily$Sum))) {
        sum_xts <- xts(dd_daily$Sum, order.by = dd_daily$Date)
      } else {
        sum_xts <- xts()
      }
      
      # ret_xts 계산 (NAV 기반)
      if (NROW(sum_xts) >= 2) {
        ret_xts <- Return.calculate(sum_xts, method="discrete")
        ret_xts <- na.omit(ret_xts)   
        if (NROW(ret_xts) < 1) ret_xts <- xts()
      } else {
        ret_xts <- xts()
      }
      
      if (NROW(ret_xts) > 0) colnames(ret_xts) <- fund_name
      
      # dd에 NAV 기반 수익률 붙이기(안전한 스코프)
      if (NROW(ret_xts) > 0) {
        dd_nav <- data.frame(Date = as.Date(index(ret_xts)), Return_NAV = as.numeric(ret_xts))
        dd <- dd %>% left_join(dd_nav, by = "Date")
      } else {
        dd <- dd %>% mutate(Return_NAV = NA_real_)
      }
      
      # =========================================================
      # (옵션) 초기 모드에서도 최소 출력(오늘 평가/수익)
      # =========================================================
      cat(sprintf("오늘 평가액: %s원 | 총수익: %s원\n",
                  comma(round(sum_value,0)), comma(round(profit_value,0))))
      
      # =========================================================
      # 리스크/고급 분석 블록 (risk_ready일 때만)
      # =========================================================
      # -----------------------------
      # 성과 요약 출력
      # -----------------------------
      if (NROW(ret_xts) >= 5) {
        cat("\n=========== PerformanceAnalytics 성과 요약 ===========\n")
        print(table.AnnualizedReturns(ret_xts))
        
        cat("\nMax Drawdown:\n")
        print(maxDrawdown(ret_xts))
        
        cat("Sharpe(연환산, Rf=0):\n")
        print(SharpeRatio.annualized(ret_xts, Rf = 0))
        
        cat("Calmar(연환산, 일간 NAV 기반):\n")
        print(CalmarRatio(ret_xts))
        
        cat("======================================================\n\n")
      } else {
        cat("[경고] 일간 수익률 표본이 너무 적어 PerformanceAnalytics 요약 생략\n")
      }
      
      # 데이터 없는 초기라면, 여기서부터(몬테카/리포트/PDF 등)는 스킵하고 누적만 계속
      if (!risk_ready) {
        cat("[초기모드] 그래프/리스크 리포트(PDF) 생성도 생략합니다.\n")
        cat("          (기록만 누적하세요. ", min_days_for_risk, "일 이후 자동으로 분석이 활성화됩니다.)\n", sep="")
        
        # 마지막 2행 출력(전일/오늘 비교를 위함)
        print(tail(dd %>% select(Date, Sum, Profit, Return_TWR, Return_NAV), 2))
        
      } else {
        
        today_date <- max(dd$Date, na.rm = TRUE)
        
        # -----------------------------
        # 1일 1회만 몬테카/팩터/PCA 돌리기
        # -----------------------------
        if (is.na(last_mc_date) || last_mc_date < today_date) {
          
          cat("\n[리스크] 오늘 기준 몬테카를로 10년 스트레스 테스트 실행...\n")
          suppressWarnings(try(
            run_mc_from_dd(dd, years = 10, monthly_contrib = 5000000, n_sims = 5000),
            silent = TRUE
          ))
          
          cat("[리스크] 미래 10년 최대낙폭(MDD) 분포 시뮬레이션 실행...\n")
          suppressWarnings(try(
            run_future_mdd_from_dd(dd, years = 10, monthly_contrib = 5000000, n_sims = 2000),
            silent = TRUE
          ))
          
          cat("[리스크] 은퇴 후 30년, 연 2억 인출 시나리오(현재자산 기준) 시뮬레이션 실행...\n")
          suppressWarnings(try(
            run_mc_withdraw_from_dd(dd, years = 30, annual_withdraw = 200000000,
                                    n_sims = 5000, withdraw_freq = "monthly"),
            silent = TRUE
          ))
          
          # -----------------------------
          # 팩터 회귀 (factors_monthly.csv 있을 때만)
          # -----------------------------
          #  "그 수익이 어디서(어떤 위험을 감내해서) 왔는가"를 규명하는 작업을 수행
          if (file.exists("factors_monthly.csv") && file.exists("asset_returns_monthly.csv")) {
            cat("[리스크] 팩터 회귀모형(Factor Regression) 실행...\n")
            suppressWarnings(try(
              run_factor_model_from_files(
                asset_returns_file = "asset_returns_monthly.csv",
                factors_file       = "factors_monthly.csv",
                weights            = weights
              ),
              silent = TRUE
            ))
          } else {
            cat("[리스크] 팩터 분석 스킵: factors_monthly.csv 또는 asset_returns_monthly.csv 없음\n")
          }
          
          # -----------------------------
          #  PCA (자산수익률 기반)
          # -----------------------------
          # 금융 분석의 워크플로우에서 주성분 분석인 PCA(Principal Component Analysis)를 실행하는 이 코드는, 포트폴리오의 리스크 구조를 '데이터 스스로가 말하게 하는' 비지도 학습(Unsupervised Learning) 단계
          # 앞서 논의한 팩터 회귀모형이 '시장이 정해준 기준(시장, 규모, 가치 등)'으로 내 포트폴리오를 설명하려 했다면, PCA는 외부의 기준 없이 오로지 내 자산들 간의 상관관계만을 분석하여 "이 포트폴리오를 움직이는 진짜 숨은 핵심 동력(Main Drivers)이 무엇인가" 규명
          if (file.exists("asset_returns_monthly.csv")) {
            cat("[리스크] PCA(자산수익률 기반) 실행...\n")
            
            suppressWarnings(try({
              tmp_ar <- read_csv("asset_returns_monthly.csv", show_col_types = FALSE)
              # Date 컬럼 제외하고 수익률 열 후보만 뽑기
              cols <- setdiff(colnames(tmp_ar), c("Date","YM"))
              cat("[DEBUG] returns cols head:", paste(head(cols, 10), collapse = ", "), "\n")
              cat("[DEBUG] weights names :", paste(names(weights), collapse = ", "), "\n")
            }, silent = TRUE))
            
            suppressWarnings(try(
              run_pca_dashboard_from_file("asset_returns_monthly.csv", weights),
              silent = TRUE
            ))
            
          } else {
            cat("[리스크] PCA 스킵: asset_returns_monthly.csv 없음\n")
          }
          
          last_mc_date <- today_date
        } else {
          cat("\n[리스크] 오늘(", format(today_date),
              ") 몬테카를로/팩터/PCA는 이미 실행됨 (다음날 재실행)\n\n", sep = "")
        }
        # 여기까지 요약
        # TWR 계산: "우리가 현재 어디에 와 있는가?" (성과 확정)
        # Factor Model: "우리가 알고 있는 위험들로 이 성과가 설명되는가?" (리스크 분해)
        # PCA: "우리가 미처 파악하지 못한 숨은 위험은 없는가?" (구조적 리스크 진단)
        
        
        # =========================================================
        # 아래부터는 그래프/트리맵/상세표 등
        # =========================================================
        
        # drop_na()는 필요 이상으로 데이터 날릴 수 있으니 최소 컬럼만 기준
        dd_plot_base <- dd %>% filter(!is.na(Date), !is.na(Sum))
        
        sum_left  <- dd_plot_base$Sum / 10000000
        
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
        
        # ---------- 63D Vol / DD (RISK-OFF) ----------
        # Drawdown 계산
        dd_series <- if (NROW(sum_xts) >= 1) (sum_xts / cummax(sum_xts)) - 1 else xts()
        
        # 리스크 상태를 감지하여 방어적 자산 배분(Risk-Off)으로 전환할지 결정하는 알고리즘의 핵심 로직
        # 장기적인 베어마켓(약세장)에서 포트폴리오를 보호하기 위한 논리적 장치
        if (NROW(ret_xts) < 63 || NROW(ret_xts) == 0) {
          cat(sprintf("[RISK-OFF CHECK] 데이터 부족(N=%d) → 63D Vol/DD 판정 생략(NA)\n", NROW(ret_xts)))
          vol63_xts <- xts(rep(NA_real_, NROW(ret_xts)), order.by = index(ret_xts))
          today_dd <- NA_real_
          today_vol63 <- NA_real_
          consecutive_days <- 0
          GLD_MODE <- FALSE
        } else {
          vol63_xts <- zoo::rollapply(
            ret_xts, width = 63,
            FUN   = function(x) sd(x, na.rm = TRUE) * sqrt(252),
            align = "right", fill = NA
          )
          
          today_dd   <- as.numeric(last(dd_series))
          today_vol63 <- as.numeric(last(vol63_xts))
          
          cond_xts <- (vol63_xts >= 0.25) & (dd_series <= -0.15)
          # Drawdown 15%이하이고 63일 변동성이 25%이상
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
          # MDD 15%이하이고 변동성이 25%이상인 날이 연속적으로 63일 이상이면 GLD_MODE가 TRUE
        }
        
        
        # 신호등 처럼 보이게 만드는 로직
        # RISK-OFF가 OFF일 때 MDD가 12~20% 구간이면 주의, 관찰이 필요하다고 알림
        dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
        
        if (GLD_MODE) {
          # 🔴 Risk-Off
          badge_text  <- "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
          badge_color <- "firebrick"
        } else if (!is.na(dd_now) && dd_now <= -0.12 && dd_now > -0.20) {
          # 🟡 주의 관찰
          badge_text  <- "현재 운용 상태 :  CAUTION  (DD 12~20% · 주의 관찰)"
          badge_color <- "goldenrod"
        } else {
          # 🟢 정상
          badge_text  <- "현재 운용 상태 :  NORMAL  (Risk-Off : OFF)"
          badge_color <- "darkgreen"
        }
        
        
        # ---------- PerformanceAnalytics 라벨용 ----------
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
        # 각 변수 설명
        # 연환산 수익률 (pa_annret):
        #   TWR을 1년 단위로 환산한 값, 서로 다른 기간 투자한 상품들을 동일한 선상에서 비교할 수 있게 해줌. 금융감독원 공시 기준에서도 가장 핵심이 되는 수치
        # 연환산 변동성 (pa_annvol):
        #   수익률의 표준편차를 연율화한 것으로, 투자의 '위험(Risk)'. 이 값이 클수록 가격 출렁임이 심하다는 뜻
        # 최대 낙폭 (pa_mdd):
        #   전고점 대비 가장 많이 하락했던 비율입니다. 투자자가 심리적으로 견뎌야 했던 '최악의 순간'을 수치화한 것
        # 샤프 지수 (pa_sharpe):
        #   위험 한 단위당 얼마나 많은 초과 수익을 얻었는지를 나타내는 효율성 지표입니다. 2025년 현재도 펀드 매니저의 실력을 가늠하는 가장 보편적인 잣대
        # 칼마 지수 (pa_calmar):
        #   수익률을 MDD로 나눈 값입니다. "고통(MDD) 대비 얻은 수익이 얼마인가?"를 보여주며, 최근 헤지펀드나 퀀트 전략가들 사이에서 샤프 지수보다 더 중요하게 다뤄지기도 함
        
        
        fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.2f%%", x * 100))
        fmt_num <- function(x) ifelse(is.na(x), "-", sprintf("%.2f", x))
        
        # ---------- 트리맵 데이터 ----------
        dt_ko <- data_ko %>%
          head(-1) %>%
          dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
        
        dt_en <- data_en %>%
          head(-2) %>%
          dplyr::select(종목명, 종목번호, 보유증권사, 평가금, 매수가격, 수량)
        
        dt_ko <- dt_ko %>%
          mutate(한화평가금 = 평가금) %>%
          mutate(한화매수가격 = 매수가격)
        
        dt_en <- dt_en %>%
          mutate(한화평가금 = 평가금 * exchange_rate) %>%
          mutate(한화매수가격 = 매수가격 * exchange_rate)
        
        dt_fn <- bind_rows(dt_ko, dt_en) %>%
          dplyr::select(-평가금) %>%
          arrange(desc(한화평가금))
        
        treemap(
          dt_fn,
          index = "종목명",
          vSize = "한화평가금",
          title = "구성비율 트리맵",
          palette = "Set3",
          fontsize.labels = 18,
          fontcolor.labels = "black",
          fontface.labels = 2,
          bg.labels = 0,
          overlap.labels = 0.5,
          inflate.labels = TRUE,
          align.labels = list(c("center","center"))
        )
        
        # ---------- 1일 평균 증가액 ----------
        fit <- lm(sum_left ~ as.numeric(dd_plot_base$Date), data = dd_plot_base)
        slope_per_day <- coef(fit)[2]
        
        # ---------- 전일 파일 찾기 ----------
        get_prev_file <- function(prefix = "output_stock_", ext = "xlsx") {
          pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
          files <- dir(pattern = pattern)
          if (length(files) == 0) return(NA)
          dates <- as.Date(sub(paste0(prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext), "\\1", files))
          valid_idx <- which(dates < Sys.Date())
          if (length(valid_idx) == 0) return(NA)
          files[which.max(dates[valid_idx])]
        }
        
        prev_ko_file <- get_prev_file("output_stock_")
        prev_en_file <- get_prev_file("output_stock_us_")
        
        data_prev_ko <- if (!is.na(prev_ko_file) && file.exists(prev_ko_file)) read_excel(prev_ko_file) else NULL
        data_prev_en <- if (!is.na(prev_en_file) && file.exists(prev_en_file)) read_excel(prev_en_file) else NULL
        
        if (!is.null(data_prev_ko) && !is.null(data_prev_en)) {
          data_prev_ko <- data_prev_ko %>%
            head(-1) %>%
            dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 평가금)
          
          data_prev_en <- data_prev_en %>%
            head(-2) %>%
            mutate(한화평가금 = 평가금 * exchange_rate) %>%
            dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 한화평가금)
          
          data_prev_fn <- bind_rows(data_prev_ko, data_prev_en) %>%
            arrange(desc(전일한화평가금))
        } else {
          data_prev_fn <- data.frame(종목번호=character(), 보유증권사=character(), 전일한화평가금=numeric())
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
        
        rt <- join_stock_data(dt_fn, data_prev_fn) %>%
          mutate(
            총매수금 = 한화매수가격 * 수량,
            총수익금 = 한화평가금 - 총매수금,
            총수익률 = round((총수익금 / pmax(총매수금, 1)) * 100, 2)
          ) %>%
          dplyr::select(종목명, 보유증권사, 한화매수가격, 수량, 한화평가금, 전일한화평가금,
                        전일대비, 전일대비율, 비중, 총매수금, 총수익금, 총수익률)
        
        today_tsum <- tail(dd$Sum, 1)
        
        # 보유한 주식의 이름으로 종목군(버킷)을 판단
        # 나중에 새로운 이름으로 나오는 종목이 있으면 아래 로직을 변경
        asset_SCHD <- rt %>% filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
          summarise(합계 = sum(한화평가금)) %>% pull(합계)
        
        # QQQ로 검색되었지만 TQQQ는 제외
        asset_QQQ  <- rt %>% filter(str_detect(종목명, "나스닥100|QQQ"), !str_detect(종목명, "TQQQ")) %>%
          summarise(합계 = sum(한화평가금)) %>% pull(합계)
        
        asset_TQQQ <- rt %>% filter(str_detect(종목명, "TQQQ")) %>%
          summarise(합계 = sum(한화평가금)) %>% pull(합계)
        
        asset_GLD  <- rt %>% filter(str_detect(종목명, "금현물")) %>%
          summarise(합계 = sum(한화평가금)) %>% pull(합계)
        
        asset_IEF  <- rt %>% filter(str_detect(종목명, "채권|국채")) %>%
          summarise(합계 = sum(한화평가금)) %>% pull(합계)
        
        asset_SCHD[is.na(asset_SCHD)] <- 0
        asset_QQQ[is.na(asset_QQQ)]   <- 0
        asset_TQQQ[is.na(asset_TQQQ)] <- 0
        asset_GLD[is.na(asset_GLD)]   <- 0
        asset_IEF[is.na(asset_IEF)]   <- 0
        
        # SPY_ETC는 다른 종목군을 제외한 나머지 종목을 의미
        asset_SPY_ETC <- today_tsum - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_IEF
        
        asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
        asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
        asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
        asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
        asset_IEF_ratio     <- asset_IEF     / today_tsum * 100
        asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
        
        current_weights <- c(
          SPY_ETC = asset_SPY_ETC_ratio / 100,
          SCHD    = asset_SCHD_ratio    / 100,
          QQQ     = asset_QQQ_ratio     / 100,
          TQQQ    = asset_TQQQ_ratio    / 100,
          GOLD    = asset_GLD_ratio     / 100,
          IEF     = asset_IEF_ratio     / 100
        )
        
        
        # =========================================================
        # 리스크 모듈 추가 실행 (Stress/GARCH/VaR/DRIFT)
        # =========================================================
        #  코드는 비중의 개수가 맞다면 강제로 이름을 부여하여 분석의 연속성을 확보
        need_nm <- c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF")
        
        if (is.null(names(weights)) || any(names(weights) == "")) {
          if (length(weights) == length(need_nm)) {
            names(weights) <- need_nm
            cat("[방탄] weights 이름 자동 부여 완료\n")
          } else {
            cat("[방탄] weights 오류 → 리스크 분석 스킵\n")
            goto_sleep <- TRUE
          }
        }
        
        if (!goto_sleep) {
          if (abs(sum(weights, na.rm = TRUE) - 1) > 1e-6) {
            weights <- weights / sum(weights, na.rm = TRUE)
            cat("[방탄] weights 정규화 완료 (합=1)\n")
          }
          
          current_nav <- tail(dd$Sum, 1)
          
          suppressWarnings(try(
            #  "만약 2008년 금융위기나 2020년 코로나 팬데믹이 내일 다시 온다면, 현재 내 current_nav(잔고)는 얼마나 깨질까?"를 시뮬레이션
            run_stress_replay_from_file(
              asset_file      = "asset_returns_monthly.csv",
              weights         = weights,
              current_nav     = current_nav,
              monthly_contrib = 0
            ),
            silent = TRUE
          ))
          
          # GARCH 변동성 경고: 단순히 과거 표준편차를 보는 것이 아니라, '변동성의 군집 현상(Volatility Clustering)'을 반영하는 GARCH 모델을 통해 **"최근 시장이 심상치 않게 요동치고 있음"**을 감지
          suppressWarnings(try(run_garch_vol_alert(dd_ret), silent = TRUE))
          # 예측 / 역사적 변동성 비율
          # GARCH 비율	해석
          # < 1.0	평시보다 조용
          # ≈ 1.0	평시 수준
          # 1.2~1.5	주의
          # 1.8 이상	위기 국면
          
          
          # cvar_obj는 try 안/밖 스코프 + 실패 대비
          cvar_obj <- NULL
          suppressWarnings(try({
            # CVaR (Conditional VaR): 하위 5%의 최악의 상황이 발생했을 때 예상되는 손실의 평균값을 계산합니다. 이는 일반적인 변동성 지표보다 훨씬 더 보수적이고 강력한 위험 지표로, '꼬리 위험(Tail Risk)'을 방어하는 데 사용
            cvar_obj <- run_var_cvar_from_file(
              asset_file  = "asset_returns_monthly.csv",
              weights     = weights,
              current_nav = current_nav,
              alpha       = 0.95
            )
          }, silent = TRUE))
          cvar_amt <- if (!is.null(cvar_obj) && !is.null(cvar_obj$cvar_amt)) cvar_obj$cvar_amt else NA_real_
          
          suppressWarnings(try(
            # 시간이 지나 주가가 변하면 내 초기 비중(weights)과 현재 비중(current_weights)이 달라지는데 이를 'Drift'라고 함
            run_drift_rebal_signal(
              target_weights  = weights,
              current_weights = current_weights,
              threshold       = 0.05
            ),
            silent = TRUE
          ))
          
          # ---------- 종목 테이블 ----------
          # 전일 대비 증감액 및 수익률 계산 로직 (에러 방지를 위해 인덱싱 명확화)
          # dd$Sum의 마지막 값이 오늘, 그 전의 값이 어제
          today_sum <- tail(dd$Sum, 1)
          yesterday_sum <- tail(dd$Sum, 2)[1]
          
          diff_value <- today_sum - yesterday_sum
          diff_pct   <- (diff_value / yesterday_sum) * 100
          
          # 증감액에 따른 색상 및 기호 결정(글로벌 표준을 따랐음 : 수익 파랑 / 손실 빨강)
          diff_color <- if(diff_value > 0) "blue" else if(diff_value < 0) "red" else "black"
          diff_sign  <- if(diff_value > 0) "+" else ""
          
          # [에러 해결 핵심] 타이틀을 하나의 HTML 문자열로 통합 생성
          caption_string <- paste0(
            "<div style='display: flex; justify-content: center; align-items: center; margin-bottom: 15px;'>",
            "<span style='font-size: 130%; font-weight: bold; color: black; margin-right: 15px;'>",
            format(Sys.time(), "%Y년 %m월 %d일"),
            "(", week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], ") ",
            format(Sys.time(), "%H시 %M분   "),
            "<span style='font-size: 100%; font-weight: bold; color: black; margin-right: 15px;'>",
            "한화평가금합계 ", scales::comma(sum_value), "원",
            "</span>",
            "<span style='font-size: 100%; font-weight: bold; color: ", diff_color, ";'>",
            "(전일대비 ", diff_sign, scales::comma(diff_value), "원, ",
            diff_sign, sprintf("%.2f%%", diff_pct), ")",
            "</span>",
            "</div>"
          )
          
          # 데이터 테이블 출력
          print(
            datatable(
              rt,
              # htmltools::HTML()을 사용하여 문자열을 안전하게 전달
              caption = htmltools::HTML(caption_string),
              options = list(
                pageLength = 100,
                columnDefs = list(
                  list(targets = c("전일대비율", "비중", "총수익률"), className = "dt-right")
                ),
                dom = 't' # 상단 검색창 등을 숨기고 깔끔하게 테이블만 표시 (필요시 제거)
              )
            ) %>%
              formatCurrency(
                columns = c("한화평가금", "한화매수가격", "전일한화평가금", "전일대비", "총매수금", "총수익금"),
                currency = "",
                mark = ",",
                digits = 0
              ) %>%
              # 소수점 반올림 후 % 기호 부착
              formatRound(columns = c("전일대비율", "비중", "총수익률"), digits = 2) %>%
              formatString(columns = c("전일대비율", "비중", "총수익률"), suffix = "%") %>%
              # 상세 데이터 행 색상 설정 (수익 파랑 / 손실 빨강)
              formatStyle(
                columns = c("전일대비", "총수익금"),
                color = styleInterval(c(-0.000001, 0.000001), c("red", "black", "blue")),
                fontWeight = styleInterval(0, c("bold", "normal"))
              ) %>%
              formatStyle(
                columns = c("전일대비율", "총수익률"),
                color = styleInterval(c(-0.000001, 0.000001), c("red", "gray", "blue")),
                fontWeight = styleInterval(0, c("bold", "normal"))
              )
          )
          
          # ---------- label_text ----------
          label_text <- paste0(
            "오늘평가액 : ", comma(round(sum_value, 0)), "원   ",
            "총수익 : ", comma(round(tail(dd$Profit, 1), 0)),"원",
            " (", round(tail(dd$Profit / sum_value, 1)*100, 2), "%)\n",
            
            "리스크상태(63D) Vol:", ifelse(is.na(today_vol63), "-", sprintf("%.2f%%", today_vol63*100)),
            "  DD:", ifelse(is.na(today_dd), "-", sprintf("%.2f%%", today_dd*100)),
            "  지속:", consecutive_days, "D",
            "  신규적립:", ifelse(GLD_MODE, "GLD(리스크-오프)", "정상(목표비중)"), "\n",
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
            "종목별 목표 평가액 = ",
            "SPY등 ", sprintf("%.1f", (today_tsum * as.numeric(weights["SPY_ETC"])) / 1e8), "억 | ",
            "SCHD ", sprintf("%.1f", (today_tsum * as.numeric(weights["SCHD"])) / 1e8), "억 | ",
            "QQQ ",  sprintf("%.1f", (today_tsum * as.numeric(weights["QQQ"]))     / 1e8), "억 | ",
            "TQQQ ", sprintf("%.1f", (today_tsum * as.numeric(weights["TQQQ"]))    / 1e8), "억 | ",
            "금 ",   sprintf("%.1f", (today_tsum * as.numeric(weights["GOLD"]))    / 1e8), "억 | ",
            "채권 ", sprintf("%.1f", (today_tsum * as.numeric(weights["IEF"]))     / 1e8), "억\n",
            
            "종목별 현재 평가액 = ",
            "SPY등 ", sprintf("%.1f", asset_SPY_ETC / 1e8), "억 | ",
            "SCHD ", sprintf("%.1f", asset_SCHD    / 1e8), "억 | ",
            "QQQ ",  sprintf("%.1f", asset_QQQ     / 1e8), "억 | ",
            "TQQQ ", sprintf("%.1f", asset_TQQQ    / 1e8), "억 | ",
            "금 ",   sprintf("%.1f", asset_GLD     / 1e8), "억 | ",
            "채권 ", sprintf("%.1f", asset_IEF     / 1e8), "억"
          )
          
          # 두 그래프에 공통으로 적용할 X축 범위를 계산
          common_date_range <- range(dd_plot_base$Date, na.rm = TRUE)
          common_date_range[2] <- common_date_range[2] + 2  # 그래프 오른쪽에 여유가 생기도록 2일 여유를 둠
          # 위의 이유로 실행종료시 2개씩 데이터가 어긋난다고 워닝이 뜨는데 무시 가능
          
          # ---------- 상단 플롯(p) ----------
          p <- ggplot(dd_plot_base, aes(x = Date)) +
            geom_point(aes(y = sum_left, color = Profit / 10000000), size = 5) +
            geom_line(aes(y = sum_left, group = 1), color = "gray") +
            geom_smooth(aes(y = sum_left), method = "lm", formula = y ~ x, se = FALSE,
                        color = "orange", linetype = "dashed", linewidth = 1) +
            geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1) +
            geom_point(aes(y = a * ret_right + b), color = "green", size = 2) +
            
            geom_hline(yintercept = b, color = "yellow2", linewidth = 1.2, alpha = 0.6) +
            
            scale_color_gradient(low = "red", high = "blue") +
            
            scale_x_date(
              limits = common_date_range,           # 범위 고정
              date_breaks = "2 months",             # 간격
              labels = scales::label_date_short(),  # 라벨 형식
              expand = c(0, 0)                      # 여백 제거
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
              plot.title = element_text(
                hjust = 0.5,      # 중앙 정렬
                face = "bold",
                size = 14
              ),
              plot.subtitle = element_text(
                hjust = 0.5,
                size = 11,
                color = "gray30"
              ),
              axis.title.y.right = element_text(
                color = "green",  # 연두색 (그래프 선과 매칭)
                size  = 9,
                face  = "bold"
              )
            ) +
            coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
            annotate("text",
                     x = min(dd_plot_base$Date, na.rm = TRUE),
                     y = max(sum_left, na.rm = TRUE),
                     label = label_text,
                     hjust = 0, 
                     vjust = 1, 
                     size  = 3.5,   # 오늘평가액 등 글자 사이즈즈 
                     color = "black") +
            annotate("label",
                     x     = max(dd_plot_base$Date, na.rm = TRUE),
                     y     = min(sum_left, na.rm = TRUE) * 1.02,
                     label = badge_text,
                     hjust = 1, vjust = 0,
                     size  = 5.5,
                     fontface = "bold",
                     fill  = badge_color,
                     color = "white")
          
          
          # ---------- Drawdown 플롯(p_dd) ----------
          dd2 <- dd_plot_base %>%
            mutate(Peak = cummax(Sum),
                   DD   = ifelse(Peak > 0, Sum / Peak - 1, 0))
          
          mdd_value      <- min(dd2$DD, na.rm = TRUE)
          mdd_end_idx    <- which.min(dd2$DD)
          mdd_end_date   <- dd2$Date[mdd_end_idx]
          mdd_end_sum    <- dd2$Sum[mdd_end_idx]
          
          mdd_start_idx  <- which.max(dd2$Sum[1:mdd_end_idx])
          mdd_start_date <- dd2$Date[mdd_start_idx]
          mdd_start_sum  <- dd2$Sum[mdd_start_idx]
          
          peak_label   <- paste0("피크\n", scales::comma(mdd_start_sum), "원\n(", format(mdd_start_date), ")")
          trough_label <- paste0("바닥\n", scales::comma(mdd_end_sum),   "원\n(", format(mdd_end_date), ")")
          
          dd_points <- data.frame(
            Date  = c(mdd_start_date, mdd_end_date),
            DDpct = c(0, mdd_value * 100)
          )
          
          y_peak_label   <- -2
          y_trough_label <- (mdd_value * 100) + 5
          
          vol_df <- data.frame(
            Date = as.Date(index(vol63_xts)),
            Vol63 = as.numeric(coredata(vol63_xts))
          )
          
          dd_plot <- dplyr::left_join(dd2, vol_df, by = "Date") %>% drop_na()
          
          dd_range  <- range(dd_plot$DD * 100, na.rm = TRUE)
          vol_range <- range(dd_plot$Vol63 * 100, na.rm = TRUE)
          
          scale_a <- diff(dd_range) / diff(vol_range)
          scale_b <- dd_range[1] - scale_a * vol_range[1]
          
          cur_dd_pct <- as.numeric(tail(dd_plot$DD, 1)) * 100
          cur_dd_amt <- as.numeric(tail(dd_plot$Sum, 1) - tail(dd_plot$Peak, 1))
          
          
          # 중간에 보여줄 누적 수익금 막대그래프
          # ---------- 중단 누적 수익금(막대) + 수익률(선) 결합 플롯 ----------
          
          # 1. 데이터 준비 및 수익률 계산
          dd_mid <- dd_plot_base %>%
            mutate(
              Profit_man = Profit / 10000000,           # 주축: 누적수익(천만원 단위)
              Return_pct = (Profit / Sum) * 100,        # 보조축: 단순 투자수익률(%)
              Status = ifelse(Profit_man >= 0, "Plus", "Minus")
            )
          
          # 2. 이중 축 변환 계수 계산 (수익금 막대와 수익률 선을 정렬하기 위함)
          # 수익금 범위와 수익률 범위의 비례식을 산출합니다.
          range_profit <- range(dd_mid$Profit_man, na.rm = TRUE)
          range_return <- range(dd_mid$Return_pct, na.rm = TRUE)
          
          # 수익률(%)을 수익금(천만원) 스케일로 변환하는 선형 함수 계수
          # y = a * x + b 형태
          rescale_a <- diff(range_profit) / diff(range_return)
          rescale_b <- range_profit[1] - rescale_a * range_return[1]
          
          p_mid <- ggplot(dd_mid, aes(x = Date)) +
            # 누적 수익금 막대 (배경 역할)
            geom_bar(aes(y = Profit_man, fill = Status),
                     stat = "identity", width = 1, alpha = 0.5, na.rm = TRUE) + # 선을 돋보이게 하기 위해 alpha 조정
            
            # 수익률 0% 기준선 (희미한 노란색/금색)
            geom_hline(yintercept = rescale_b, color = "gold", linewidth = 0.8, alpha = 0.6) +
            
            # 단순 투자수익률 선 그래프
            geom_line(aes(y = Return_pct * rescale_a + rescale_b),
                      color = "#F4A261", linewidth = 1) +  # 살구색
            
            # 색상 설정
            scale_fill_manual(values = c("Plus" = "dodgerblue4", "Minus" = "firebrick3")) +
            
            # 축 설정
            scale_x_date(
              limits = common_date_range,
              date_breaks = "2 months",
              labels = scales::label_date_short(),
              expand = c(0, 0)) +
            scale_y_continuous(
              name = "누적수익(천만원)",
              labels = scales::comma,
              # 보조축: 수익률(%) 표시
              sec.axis = sec_axis(~ (. - rescale_b) / rescale_a, name = "투자수익률(%)")) +
            
            # 제목 및 라벨
            labs(
              title = paste0("누적 성과 추이 (수익금: ", scales::comma(tail(dd_mid$Profit, 1)),
                             "원 / 수익률: ", sprintf("%.2f%%", tail(dd_mid$Return_pct, 1)), ")")) +
            
            # 테마 설정
            theme_minimal(base_size = 13) +
            theme(
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.y.right = element_text(
                color = "#F4A261",   # ← 투자수익률 선과 동일한 살구색
                size  = 10,
                face  = "bold"
              ),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
            )
          
          # Drawdown graph
          p_dd <- ggplot(dd_plot, aes(x = Date)) +
            geom_line(aes(y = DD * 100, color = DD), linewidth = 2) +
            geom_line(aes(y = scale_a * (Vol63 * 100) + scale_b),
                      color = "purple", linewidth = 1, linetype = "dashed") +
            geom_hline(yintercept = 0, color = "gray50") +
            geom_hline(yintercept = c(-5, -10, -15), linetype = "dotted", color = "gray70") +
            geom_vline(xintercept = c(mdd_start_date, mdd_end_date), linetype = "dashed") +
            geom_point(data = dd_points, aes(x = Date, y = DDpct),
                       inherit.aes = FALSE, size = 3, color = "firebrick") +
            scale_color_gradientn(
              colours = c("blue", "lightblue", "orange", "red"),
              values  = scales::rescale(c(-0.05, -0.10, -0.15, -0.30)),
              limits  = c(min(dd2$DD, na.rm = TRUE), 0),
              labels  = scales::percent) +
            scale_y_continuous(
              name = "Drawdown (%)",
              sec.axis = sec_axis(~ (. - scale_b) / scale_a, name = "63D Volatility (Annualized %)")) +
            annotate("label", x = mdd_start_date, y = y_peak_label,
                     label = peak_label, size = 3.2, vjust = 1, hjust = 0.5, fill = "white") +
            annotate("label", x = mdd_end_date, y = y_trough_label,
                     label = trough_label, size = 3.2, vjust = 0, hjust = 0.5, fill = "white") +
            annotate("label", x = mdd_end_date, y = (mdd_value * 100) + 5,
                     label = paste0("MDD: ", scales::percent(-mdd_value, accuracy = 0.01)),
                     size = 3.2, vjust = 1, hjust = 0.5, fill = "white") +
            labs(
              title = paste0("Drawdown (현재: ", sprintf("%.2f%%", cur_dd_pct),
                             ", 피크대비: ", scales::comma(cur_dd_amt), "원)"),
              x = "날짜(연/월)",
              color = "Drawdown") +
            theme_minimal(base_size = 13) +
            theme(axis.title.y.right = element_text(color = "purple",
                                                    size = 9),
                  legend.position = "right")
          
          p_dd <- p_dd + scale_x_date(
            limits = common_date_range, # 동일하게 고정
            date_breaks = "2 months",
            labels = scales::label_date_short(),
            expand = c(0, 0) # 불필요한 양끝 여백 제거
          )
          
          
          # =========================================================
          #  비중 막대 그래프 (위: 목표 / 아래: 현재)
          # =========================================================
          
          today_tsum <- as.numeric(tail(dd$Sum, 1))
          
          weight_bar_df <- data.frame(
            Asset = factor(
              c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권"),
              levels = c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권")
            ),
            Target = c(
              as.numeric(weights["SPY_ETC"]),
              as.numeric(weights["SCHD"]),
              as.numeric(weights["QQQ"]),
              as.numeric(weights["TQQQ"]),
              as.numeric(weights["GOLD"]),
              as.numeric(weights["IEF"])
            ) * 100,
            Current = c(
              as.numeric(asset_SPY_ETC_ratio),
              as.numeric(asset_SCHD_ratio),
              as.numeric(asset_QQQ_ratio),
              as.numeric(asset_TQQQ_ratio),
              as.numeric(asset_GLD_ratio),
              as.numeric(asset_IEF_ratio)
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
            levels = c("SPY등", "SCHD", "QQQ", "TQQQ", "금", "채권")
          )
          
          asset_colors_light <- c(
            "SPY등" = "#4FA3E3",
            "SCHD" = "#5CCB8A",
            "QQQ"  = "#B39DDB",
            "TQQQ" = "#FF8A65",
            "금"   = "#FFD54F",
            "채권" = "#90A4AE"
          )
          
          weight_bar_long$label <- paste0(
            as.character(weight_bar_long$Asset),
            " (", sprintf("%.1f", weight_bar_long$Weight), "%)"
          )
          weight_bar_long$label[weight_bar_long$Weight < 3] <- ""
          
          p_weight_bar <- ggplot(weight_bar_long, aes(x = Type, y = Weight, fill = Asset)) +
            geom_col(
              width = 1.0,      # 두 줄 막대 사이 공백 최소화(두껍게 채움)
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
            scale_fill_manual(values = asset_colors_light, drop = FALSE) +
            scale_y_continuous(
              limits = c(0, 100),
              labels = function(x) paste0(x, "%"),
              expand = c(0, 0)
            ) +
            scale_x_discrete(expand = c(0, 0), limits = rev(levels(weight_bar_long$Type))) +  # Target/Current 사이 여백 제거(범주축)
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
          # combined_plot 결합(위에서 만든 그래프 객체를 합침)
          # =========================================================
          combined_plot <- (p / p_mid / p_dd / p_weight_bar) +
            patchwork::plot_layout(heights =c(2.2, 1, 1, 0.40)) &
            theme(
              legend.position = "none",
              plot.margin = margin(10, 20, 10, 20)
            )
          
          suppressMessages(print(combined_plot))
          
          # ---------- 콘솔 출력 ----------
          print(paste(
            "국내주식수 :", dim(data1)[1] - 1,
            " 해외주식수 :", dim(data2)[1] - 2,
            " 환율 :", exchange_rate, "원/달러",
            "(", exchange_diff, ")"
          ))
          
          # ---------- PDF 저장 ----------
          date_str <- format(Sys.Date(), "%Y%m%d")
          out_dir  <- "reports"
          dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
          pdf_file <- file.path(out_dir, sprintf("Daily_Risk_%s.pdf", date_str))
          if (file.exists(pdf_file)) file.remove(pdf_file)
          
          # ggsave로 1페이지 저장(가로 A4 사이즈 동일)
          # pdf를 다른 프로그램이 열고 있을 때 아래와 같이 생성하면 에러가 날 수 있으므로 유의 
          ggsave(filename = pdf_file, plot = combined_plot, width = 11.69, height = 8.27, device = cairo_pdf)
          cat("Saved:", pdf_file, "\n")
          
          cat(sprintf(
            "\n[RISK-OFF CHECK] 63D Vol=%s, DD=%s, 지속=%d거래일 → GLD_MODE=%s\n\n",
            ifelse(is.na(today_vol63), "-", sprintf("%.2f%%", today_vol63 * 100)),
            ifelse(is.na(today_dd),   "-", sprintf("%.2f%%", today_dd * 100)),
            consecutive_days,
            ifelse(GLD_MODE, "ON", "OFF")
          ))
          
        } else {
          # 초기 모드: 안내만 하고 넘어감
          cat("[초기모드] 그래프/리스크 리포트(PDF) 생성도 생략합니다.\n")
          cat("          (기록만 누적하세요. ", min_days_for_risk, "일 이후 자동으로 분석이 활성화됩니다.)\n", sep="")
        }
        
        # 데이터 마지막 2행 출력(전일/오늘 비교를 위함)
        print(tail(dd %>% select(Date, Sum, Profit, Return_TWR, Return_NAV), 2))
        # 현금 유입이나 매매가 없어도
        # Return_TWR과 Return_NAV는 일치하지 않을 수 있음
        # 이는 오류가 아니라
        # 평가 시점, 환율, 현금 비중 처리 차이에서 발생하는
        # 정상적인 펀드식 성과 측정의 결과임
        # 개인투자자로서 Return_NAV·Profit이 아니라,
        # Return_TWR와 Drawdown(dd)만 보면 됨
        # 요약 : 화면에 표시되어 체감되는 “수익률”은
        # NAV(계좌 기준) 수익률이며, 운용 판단에 써야 할 수익률은 TWR
        # 즉, NAV은	내 자산이 원화로 얼마나 변했나 판단 TWR(PWR)은 투자 자체가 잘 됐나 판단(따라서 환율급등락시 차이가 있을 수 있음)
        # 자산운용사 내부에서 펀드매니저의 연봉이나 성과급(인센티브)을 계산할 때는 Return_TWR(시간가중수익률)만을 사용
        # 고객에게 발송되는 월간/분기별 운용 보고서에는 Return_NAV(순자산가치 수익률)가 메인으로 표시
      }
    }
    
  }, error = function(e) {
    cat("[ERROR] 루프 1회차 실행 중 에러 발생: ", conditionMessage(e), "\n", sep = "")
  })
  
  cat("장중 10분 그이외는 1시간 후에 다시 실행됨(중단을 원하면 Interrupt-R 빨간버튼 클릭) ",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"), "\n\n", sep="")
  
  count <- count + 1
  
  now <- Sys.time()
  if (as.numeric(difftime(now, last_update_time, units = "secs")) >= UPDATE_EVERY_SEC) {
    
    # dd/sum_xts가 준비된 경우에만 갱신
    if (exists("dd") && exists("sum_xts")) {
      
      # GLD_MODE가 아직 없으면 FALSE로 취급(안전)
      gld_mode_now <- if (exists("GLD_MODE")) isTRUE(GLD_MODE) else FALSE
      
      badge_text <- make_badge_text(sum_xts, gld_mode_now)
      
      # warnings_vec/errors_vec는 없으면 빈 벡터
      warnings_vec <- if (exists("warnings_vec")) warnings_vec else character(0)
      errors_vec   <- if (exists("errors_vec"))   errors_vec   else character(0)
      
      prompt_text <- make_gemini_prompt_pms(
        dd = dd,
        sum_xts = sum_xts,
        badge_text = badge_text,
        warnings_vec = warnings_vec,
        errors_vec = errors_vec,
        take_last_n_days = 2
      )
      
      changed <- save_if_changed(prompt_text, PROMPT_FILE)
      if (changed) {
        message("[Prompt Updated] ", PROMPT_FILE, " @ ", format(now, "%H:%M:%S"))
      }
      
      last_update_time <- now
    }
  }
  
  
  wait_min <- if (in_fast_range & (wday >= 1 & wday <= 5)) 10 else 60
  Sys.sleep(wait_min * 60)
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
# 자산운용사 대비 이 프로그램에 없는 것(각종 규제로 인해 필수적으로 갖추어야 하는 것들) :
# 주문/체결/사후감시(컴플라이언스)
# 투자한도, 금지종목, 이해상충, pre-trade / post-trade 룰
# 회계/NAV 공정성 + 리컨실리에이션(같은 자산을 서로 다른 장부(내부 vs 외부)가 같은 숫자로 보고 있는지 맞춰보는 작업)
# 브로커/수탁/내부 장부 대사, 기업행사 반영, 가격 소스 관리
# 권한/감사추적(Audit trail) : “누가 언제 무엇을 바꿨나” 기록이 필수
# 펀드별/고객별 템플릿, 공시 수준의 일관성
