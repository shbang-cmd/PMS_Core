###############################################
# PMS(Portfolio Monitoring System) 1.01 / 2025-12-28 메인 스크립트 (루프 버전)
#
# 1줄 사용법 : 그냥 Ctrl + Alt + R 키를 누른다(전체 실행)
#
# - stock_eval.R / stock_eval_us.R 필요(국내, 미국 주식 데이터 수집 모듈)
# - risk_module.R 필요(리스크관리 함수 모음)
#   . risk_module.R의 몬테카, MDD, 인출, 팩터, PCA를 모두 호출
# 입력 파일
#         input_stock.csv    : 한국주식
#         input_stock_us.csv : 미국주식
#         output_sum.csv                      : 전일 평가액총액, 수익금(입력이자 출력파일)
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

###############################################
# PMS(Portfolio Monitoring System) 1.0 / 2025-12-25 메인 스크립트 (루프 버전)
# 1줄 사용법 : 그냥 Ctrl + Alt + R 키를 누른다(전체 실행)
###############################################

# =========================================================
# 패키지 설치/로드
# =========================================================
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr",
         "PerformanceAnalytics", "showtext", "zoo", "tidyr", "quantmod",
         "xts", "rugarch", "htmltools")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(readr);   library(readxl);  library(showtext)
library(openxlsx); library(rvest);  library(httr)
library(dplyr);   library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)
library(zoo); library(tidyr); library(quantmod); library(xts)
library(rugarch); library(htmltools)

# =========================================================
# 개인별 세팅 변수
# =========================================================
wd        <- "c:\\PMS_Core"
fund_name <- "JS Fund"

weights <- c(
  0.36,  # SPY등
  0.20,  # SCHD
  0.15,  # QQQ
  0.10,  # TQQQ
  0.10,  # GOLD
  0.05,  # IEF
  0.04   # CASH : 현금이 아니라 종목으로서의 현금을 뜻함
)

weights <- setNames(weights, c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF","CASH"))

setwd(wd)
options(scipen = 999)

REPEAT_FLAG = TRUE

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
make_gemini_prompt_pms <- function(dd, sum_xts, badge_text = NULL,
                                   fund_name = "JS Fund",
                                   report_time_kst = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   flow_text = "금일 Flow(입출금): 0원 / 매수·매도: 없음(거래 0건)",
                                   risk_metrics = NULL,
                                   warnings_vec = character(0),
                                   errors_vec   = character(0),
                                   take_last_n_days = 2,
                                   benchmark_name = "SPY",
                                   benchmark_ret  = NULL) {
  
  if (is.null(dd) || NROW(dd) == 0) stop("dd가 비어 있습니다.")
  if (is.null(sum_xts) || NROW(sum_xts) == 0) stop("sum_xts가 비어 있습니다.")
  
  dd_now <- as.numeric(tail((sum_xts / cummax(sum_xts)) - 1, 1))
  dd_tail <- utils::tail(dd, take_last_n_days)
  tab_txt <- utils::capture.output(print(dd_tail))
  
  badge_txt <- if (!is.null(badge_text) && nzchar(badge_text)) badge_text else "(미제공)"
  warn_txt  <- if (length(warnings_vec) > 0) paste0("- ", warnings_vec, collapse = "\n") else "(없음)"
  err_txt   <- if (length(errors_vec)  > 0) paste0("- ", errors_vec,  collapse = "\n") else "(없음)"
  
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
  
  last_row <- dd[NROW(dd), , drop = FALSE]
  nav_today <- if ("Sum" %in% names(last_row)) as.numeric(last_row$Sum) else NA_real_
  nav_prev  <- if ("Sum_lag" %in% names(last_row)) as.numeric(last_row$Sum_lag) else NA_real_
  nav_diff  <- nav_today - nav_prev
  nav_diff_pct <- if (is.finite(nav_prev) && nav_prev != 0) nav_diff / nav_prev else NA_real_
  
  ret_nav <- if ("Return_NAV" %in% names(last_row)) as.numeric(last_row$Return_NAV) else NA_real_
  ret_twr <- if ("Return_TWR" %in% names(last_row)) as.numeric(last_row$Return_TWR) else NA_real_
  
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
  
  bm_line <- if (!is.null(benchmark_ret) && is.finite(benchmark_ret)) {
    paste0("- 벤치마크(", benchmark_name, ") 금일 수익률: ", fmt_pct(benchmark_ret, acc = 0.01))
  } else {
    paste0("- 벤치마크(", benchmark_name, ") 금일 수익률: (미제공 → 평가 유보)")
  }
  
  rel_line <- if (!is.null(benchmark_ret) && is.finite(benchmark_ret) && is.finite(ret_twr)) {
    paste0("- ", benchmark_name, " 대비 상대성과(금일, TWR 기준): ", fmt_pct(ret_twr - benchmark_ret, acc = 0.01))
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
  
  paste0(
    "당신은 기관 자산운용사(연기금/헤지펀드) 출신의 수석 펀드매니저입니다.\n\n",
    "조건:\n",
    "- 예측/투자권유/매수·매도 지시 금지(해석만)\n",
    "- Return_NAV vs Return_TWR 차이 반드시 설명(환율 등은 가능성으로)\n",
    "- 존댓말, 기관 리포트 톤\n\n",
    "출력 형식:\n",
    "1) 오늘 한 줄 요약\n",
    "2) 운용 상태 설명\n",
    "3) 핵심 요약(KPI)\n",
    "4) 성과 요약(Return_NAV vs Return_TWR)\n",
    "5) 드로다운 상태\n",
    "6) 특이사항\n",
    "7) 시장 환경 한 줄\n\n",
    "<입력 데이터>\n\n",
    "=== [0] Flow/거래 ===\n", flow_text, "\n\n",
    "=== [1] 배지 ===\n", badge_txt, "\n\n",
    "=== [1-1] KPI ===\n", kpi_txt, "\n\n",
    "=== [2] 최근 ", take_last_n_days, "일 ===\n",
    paste(tab_txt, collapse = "\n"), "\n\n",
    "=== [3] DD_now ===\n",
    "DD_now = ", sprintf("%.6f", dd_now), "\n\n",
    "=== [3-1] 리스크지표 ===\n", risk_txt, "\n\n",
    "=== [4] Warnings ===\n", warn_txt, "\n\n",
    "=== [5] Errors ===\n", err_txt, "\n\n",
    "위 입력만으로 작성하세요.\n"
  )
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

PROMPT_FILE <- file.path("reports", "gemini_prompt.txt")
UPDATE_EVERY_SEC <- 10
last_update_time <- Sys.time() - 9999

# =========================================================
# 반복 루프 시작
# =========================================================
repeat {
  
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
      # ★ 현금성(CASH_LIKE) 별도 관리 (KOFR/BIL/MMF/CMA 등)
      # - KOFR/BIL이 종목 테이블(rt)에 '종목행'으로 안 들어오는 구조라면
      #   여기서 cash_like로 따로 넣어야 비중/드리프트가 정상 작동합니다.
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
        if (nrow(existing_data) > 0 && tail(existing_data$Date, 1) == Sys.Date()) {
          existing_data <- existing_data[-nrow(existing_data), ]
        }
        updated_data <- bind_rows(existing_data, result)
      } else {
        updated_data <- result
      }
      
      write_csv(updated_data, output_file)
      
      is_initial_mode <- (nrow(updated_data) < min_days_for_risk)
      
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
          suppressWarnings(try(run_mc_withdraw_from_dd(dd, years=30, annual_withdraw=200000000,
                                                       n_sims=5000, withdraw_freq="monthly"), silent=TRUE))
          
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
        treemap(dt_fn, index="종목명_tm", vSize="한화평가금", title="구성비율 트리맵")
        showtext_auto()
        
        # 1일 평균 증가액
        fit <- lm(sum_left ~ as.numeric(dd_plot_base$Date), data = dd_plot_base)
        slope_per_day <- coef(fit)[2]
        
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
        
        # =========================================================
        # 자산군 합계/비중 계산 (CASH는 cash_like로 별도 관리)
        # =========================================================
        
        # 1) 종목 합(=rt에 들어온 자산)과 총합(종목+현금성) 분리
        today_tsum_stock <- sum(rt$한화평가금, na.rm = TRUE)  # 종목합
        today_tsum       <- today_tsum_stock + cash_like       # 총합(종목 + 현금성)
        
        # 2) 종목명 기반 버킷 집계 (필요시 키워드 보완)
        asset_SCHD <- rt %>% filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>% pull(합계)
        
        asset_QQQ  <- rt %>% filter(str_detect(종목명, "나스닥100|QQQ"), !str_detect(종목명, "TQQQ")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>% pull(합계)
        
        asset_TQQQ <- rt %>% filter(str_detect(종목명, "TQQQ")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>% pull(합계)
        
        asset_GLD  <- rt %>% filter(str_detect(종목명, "금현물")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>% pull(합계)
        
        asset_IEF  <- rt %>% filter(str_detect(종목명, "채권|국채")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>% pull(합계)
        
        # 3) NA 방탄
        asset_SCHD[is.na(asset_SCHD)] <- 0
        asset_QQQ[is.na(asset_QQQ)]   <- 0
        asset_TQQQ[is.na(asset_TQQQ)] <- 0
        asset_GLD[is.na(asset_GLD)]   <- 0
        asset_IEF[is.na(asset_IEF)]   <- 0
        
        # 4) CASH는 KOFR/BIL 종목(현금성 ETF)로 정의
        asset_CASH <- rt %>%
          filter(str_detect(종목명, "KOFR|BIL")) %>%
          summarise(합계 = sum(한화평가금, na.rm = TRUE)) %>%
          pull(합계)
        asset_CASH[is.na(asset_CASH)] <- 0 # **"asset_CASH 데이터에서 비어있는 값(NA)들을 찾아내어 모두 숫자 0으로 채워 넣어라"**라는 명확한 전처리 명령
        
        # 5) SPY_ETC는 "종목 중 나머지"로 정의 (현금성 제외)
        asset_SPY_ETC <- today_tsum_stock - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_IEF - asset_CASH
        
        asset_SPY_ETC[is.na(asset_SPY_ETC)] <- 0
        
        # 6) 비중(%)은 총합(today_tsum = 종목+현금성) 기준
        asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
        asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
        asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
        asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
        asset_IEF_ratio     <- asset_IEF     / today_tsum * 100
        asset_CASH_ratio    <- asset_CASH    / today_tsum * 100
        asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
        
        # 7) drift용 current_weights (합=1)
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
          
          suppressWarnings(try(run_stress_replay_from_file("asset_returns_monthly.csv", weights, current_nav, monthly_contrib=0), silent=TRUE))
          suppressWarnings(try(run_garch_vol_alert(dd_ret), silent=TRUE))
          
          cvar_obj <- NULL
          suppressWarnings(try({
            cvar_obj <- run_var_cvar_from_file("asset_returns_monthly.csv", weights, current_nav, alpha=0.95)
          }, silent=TRUE))
          cvar_amt <- if (!is.null(cvar_obj) && !is.null(cvar_obj$cvar_amt)) cvar_obj$cvar_amt else NA_real_
          
          suppressWarnings(try(run_drift_rebal_signal(target_weights=weights, current_weights=current_weights, threshold=0.05), silent=TRUE))
          
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
            "<span style='font-size:100%;font-weight:bold;color:", diff_color, ";'>",
            "(전일대비 ", diff_sign, scales::comma(diff_value), "원, ",
            diff_sign, sprintf("%.2f%%", diff_pct), ")</span></div>"
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
          
          # ---------- label_text ----------
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
            "%)  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)\n"
          )
          
          common_date_range <- range(dd_plot_base$Date, na.rm = TRUE)
          common_date_range[2] <- common_date_range[2] + 2
          
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
            scale_x_date(limits = common_date_range,
                         date_breaks = "2 months",
                         labels = scales::label_date_short(),
                         expand = c(0, 0)) +
            scale_y_continuous(name = "보유합계(천만원)",
                               sec.axis = sec_axis(~ (. - b) / a, name = "일간수익률(%)")) +
            labs(title = plot_title,
                 subtitle = paste0("USD/KRW ", exchange_rate, " (", exchange_diff, ")"),
                 x = NULL, y = NULL) +
            theme_minimal(base_size = 13) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                  plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
                  axis.title.y.right = element_text(color = "green", size = 9, face = "bold")) +
            coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
            annotate("text",
                     x = min(dd_plot_base$Date, na.rm = TRUE),
                     y = max(sum_left, na.rm = TRUE),
                     label = label_text,
                     hjust = 0, vjust = 1,
                     size = 3.5, color = "black") +
            annotate("label",
                     x = max(dd_plot_base$Date, na.rm = TRUE),
                     y = min(sum_left, na.rm = TRUE) * 1.02,
                     label = badge_text,
                     hjust = 1, vjust = 0,
                     size = 5.5, fontface = "bold",
                     fill = badge_color, color = "white")
          
          # ---------- 중단 누적수익(막대)+수익률(선) ----------
          dd_mid <- dd_plot_base %>%
            mutate(Profit_man = Profit / 10000000,
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
                      color="#F4A261", linewidth=1) +
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
          
          p_dd <- ggplot(dd_plot, aes(x = Date)) +
            geom_line(aes(y = DD * 100), linewidth = 2) +
            geom_line(aes(y = scale_a * (Vol63 * 100) + scale_b),
                      color = "purple", linewidth = 1, linetype = "dashed") +
            geom_hline(yintercept = 0, color = "gray50") +
            geom_hline(yintercept = c(-5, -10, -15), linetype="dotted", color="gray70") +
            geom_vline(xintercept = c(mdd_start_date, mdd_end_date), linetype="dashed") +
            annotate("label", x=mdd_start_date, y=-2, label=peak_label, size=3.2, fill="white") +
            annotate("label", x=mdd_end_date, y=(mdd_value*100)+5, label=trough_label, size=3.2, fill="white") +
            annotate("label", x=mdd_end_date, y=(mdd_value*100)+10,
                     label=paste0("MDD: ", scales::percent(-mdd_value, accuracy=0.01)),
                     size=3.2, fill="white") +
            scale_x_date(limits = common_date_range,
                         date_breaks = "2 months",
                         labels = scales::label_date_short(),
                         expand = c(0, 0)) +
            scale_y_continuous(name="Drawdown (%)",
                               sec.axis = sec_axis(~ (. - scale_b)/scale_a, name="63D Volatility (Annualized %)")) +
            labs(title=paste0("Drawdown (현재: ", sprintf("%.2f%%", cur_dd_pct),
                              ", 피크대비: ", scales::comma(cur_dd_amt), "원)"),
                 x="날짜(연/월)") +
            theme_minimal(base_size=13) +
            theme(axis.title.y.right=element_text(color="purple", size=9),
                  legend.position="none")
          
          # =========================================================
          # ✅✅✅ [핵심 수정] 비중 막대 그래프 (Target/Current)
          # =========================================================
          # =========================================================
          #  비중 막대 그래프 (위: 목표 / 아래: 현재)  [정상 버전]
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
            " (", sprintf("%.1f", weight_bar_long$Weight), "%)"
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
          # combined_plot 결합
          # =========================================================
          combined_plot <- (p / p_mid / p_dd / p_weight_bar) +
            patchwork::plot_layout(heights = c(2.2, 1, 1, 0.40)) &
            theme(legend.position = "none",
                  plot.margin = margin(10, 20, 10, 20))
          
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
        dd = dd, sum_xts = sum_xts,
        badge_text = badge_text,
        warnings_vec = warnings_vec,
        errors_vec = errors_vec,
        take_last_n_days = 2
      )
      
      changed <- save_if_changed(prompt_text, PROMPT_FILE)
      if (changed) message("[Prompt Updated] ", PROMPT_FILE, " @ ", format(now, "%H:%M:%S"))
      last_update_time <- now
    }
  }
  
  if (!REPEAT_FLAG) break
  
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
