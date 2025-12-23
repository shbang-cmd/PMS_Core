###############################################
# PMS(Portfolio Monitoring System) 메인 스크립트 (루프 버전)
# - stock_eval.R / stock_eval_us.R 필요(국내, 미국 주식 데이터 수집 모듈)
# - risk_module.R 필요(리스크관리 함수 모음)
#   . risk_module.R의 몬테카, MDD, 인출, 팩터, PCA를 모두 호출
# 입력 파일 
#         input_stock.csv    : 한국주식
#         input_stock_us.csv : 미국주식
# 출력 파일
#         output_stock_{YYYY-MM-DD}.xlsx    : 한국주식 평가액
#         output_stock_us_{YYYY-MM-DD}.xlsx : 미국주식 평가액
#         output_sum.csv                    : 평가액총액, 수익금
# - 누적 데이터(output_sum.csv)가 100일이 안되면 리스크 관리 분석은 생략
###############################################

# 이 코드는 "돈을 얼마나 벌었는지를 관리하지 않는다. 망하지 않을 구조만 관리한다."
# 위험관리 핵심 3대 지표 : MDD · CVaR · Risk-Off 3개

# =========================================================
# 0) 패키지 설치/로드
# =========================================================
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr",
         "PerformanceAnalytics", "showtext", "zoo", "tidyr", "quantmod", "xts",
         "rugarch")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(readr);   library(readxl); library(showtext)
library(openxlsx); library(rvest); library(httr)
library(dplyr);   library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)
library(zoo); library(tidyr); library(quantmod); library(xts)
library(rugarch)

options(scipen = 999)  # 숫자의 과학적표기법 방지

# =========================================================
# 1) 개인별 세팅 변수
# =========================================================
wd        <- "c:\\PMS"     # 작업디렉토리
fund_name <- "JS Fund"     # 펀드/계좌 이름(임의로 바꾸면 됨)

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

# 이름으로 참조가 가능하도록 setNames 
# 조회 예시) as.numeric(weights["SPY_ETC"])
weights <- setNames(weights, c("SPY_ETC","SCHD","QQQ","TQQQ","GOLD","IEF"))

setwd(wd)

# 리스크 + 팩터 + PCA 모듈 로드
source("risk_module.R")

# 월간 자산수익률 + 팩터 CSV 업데이트(모듈 제공 함수)
suppressWarnings(
  try(update_factor_data(), silent = TRUE)
)

# =========================================================
# 2) 실행 제어 변수
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
  
}


# =========================================================
# 3) 반복 루프 시작
# =========================================================
repeat {
  
  now  <- as.POSIXct(Sys.time())
  hhmm <- format(now, "%H:%M")
  wday <- as.numeric(format(now, "%u"))  # 1=월 ~ 7=일
  in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"
  
  cat("[", count, "회차] ", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      " : 실행 시작***********************************************\n", sep="")
  
  # =========================================================
  # A) 현재 보유자산 평가 업데이트
  # =========================================================
  # stock_eval.R, stock_eval_us.R에서 아래 객체들이 생성된다고 가정:
  # - data_ko, data_en, exchange_rate, exchange_diff
  suppressWarnings(source("stock_eval.R"))
  suppressWarnings(source("stock_eval_us.R"))
  
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
    
    yesugum <- 0
    yegum   <- 0
    sum_value <- round(sum_value + yegum, 0)
    
    result <- data.frame(Date = today, Sum = sum_value, Profit = profit_value)
    
    # =========================================================
    # B) output_sum.csv 갱신
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
    
    
    if (nrow(updated_data) <= 100) break  # 100일 이하는 리스크 분석이 의미가 없어 더이상 진행안하고 종료
    
    # =========================================================
    # C) 분석용 데이터 재읽기 + Return 계산
    # =========================================================
    dd <- readr::read_csv(
      output_file,
      col_types = readr::cols(
        Date   = readr::col_date(format = ""),
        Sum    = readr::col_double(),
        Profit = readr::col_double()
      ),
      show_col_types = FALSE
    )
    
    dd <- add_twr_return_to_dd(dd)
    
    # 분석 시에는 Return NA 제거한 버전 사용 권장
    dd_ret <- dd %>% dplyr::filter(!is.na(Return))
    
    
    # =========================================================
    #  초기 구간(100일 이하) 리스크 분석 게이트 (핵심)
    # =========================================================
    dd_daily_n <- dd %>% distinct(Date) %>% filter(!is.na(Date)) %>% nrow()
    risk_ready <- dd_daily_n >= min_days_for_risk  # 일정기간 데이터가 쌓이지 않은 경우 실행시키지 않기 위함
    
    if (risk_ready) {
      # 일간 운용 수익률 계산
      daily_ret <- compute_daily_returns_from_dd(dd)
    }
    
    # dd에 Return 컬럼으로 정렬해서 붙이기
    dd <- dd %>%
      mutate(Return = c(NA, daily_ret)) %>%   # 첫날은 NA
      filter(!is.na(Return))

    # =========================================================
    # D) PerformanceAnalytics 준비 (sum_xts, ret_xts)
    # =========================================================
    dd_daily <- dd %>%
      group_by(Date) %>%
      summarise(Sum = last(Sum), .groups="drop") %>%
      arrange(Date)
    
    # timeBased/empty 방탄
    if (nrow(dd_daily) >= 2 && all(!is.na(dd_daily$Date)) && all(!is.na(dd_daily$Sum))) {
      sum_xts <- xts(dd_daily$Sum, order.by = dd_daily$Date)
    } else {
      sum_xts <- xts()
    }
    
    # ret_xts 계산 
    if (NROW(sum_xts) >= 2) {
      ret_xts <- Return.calculate(sum_xts, method="discrete")
      ret_xts <- ret_xts[!is.na(ret_xts)]
      if (NROW(ret_xts) >= 1) ret_xts <- ret_xts[-1]
      if (NROW(ret_xts) < 1) ret_xts <- xts()
    } else {
      ret_xts <- xts()
    }
    
    if (NROW(ret_xts) > 0) colnames(ret_xts) <- fund_name
    
    # =========================================================
    # E) (옵션) 초기 모드에서도 최소 출력(오늘 평가/수익)
    # =========================================================
    cat(sprintf("오늘 평가액: %s원 | 총수익: %s원\n",
                comma(round(sum_value,0)), comma(round(profit_value,0))))
    
    # =========================================================
    # F) 리스크/고급 분석 블록 (risk_ready일 때만)
    # =========================================================
    # -----------------------------
    # 1) 성과 요약 출력
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
    
    today_date <- max(dd$Date, na.rm = TRUE)
    
    # -----------------------------
    # 2) 1일 1회만 몬테카/팩터/PCA 돌리기
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
      # 3) 팩터 회귀 (factors_monthly.csv 있을 때만)
      # -----------------------------
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
          ") 몬테카/팩터/PCA는 이미 실행됨 (다음날 재실행)\n\n", sep = "")
    }
    
    # =========================================================
    # G) 아래부터는 그래프/트리맵/상세표 등 (risk_ready일 때만 풀버전)
    # =========================================================
    
    # ---------- 축 계산 ----------
    dd <- dd %>% drop_na()
    sum_left  <- dd$Sum / 10000000
    ret_right <- dd$Return * 100
    
    sum_range     <- range(sum_left,  na.rm = TRUE)
    return_range  <- range(ret_right, na.rm = TRUE)
    
    a <- diff(sum_range) / diff(return_range)
    b <- sum_range[1] - a * return_range[1]
    
    start_date <- format(min(dd$Date, na.rm = TRUE), "%Y-%m-%d")
    end_date   <- format(max(dd$Date, na.rm = TRUE), "%Y-%m-%d")
    
    plot_title <- paste0(
      fund_name, " 주식평가액 분석 (", start_date, " ~ ", end_date, ")  ",
      format(Sys.time(), "%Y년 %m월 %d일"),
      "(", week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], ") ",
      format(Sys.time(), "%H시 %M분")
    )
    
    # ---------- 63D Vol / DD (RISK-OFF) ----------
    dd_series <- if (NROW(sum_xts) >= 1) (sum_xts / cummax(sum_xts)) - 1 else xts()
    
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
    
    if (GLD_MODE) {
      badge_text  <- "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
      badge_color <- "firebrick"
    } else {
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
    fit <- lm(sum_left ~ as.numeric(Date), data = dd)
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
    
    asset_SCHD <- rt %>% filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
      summarise(합계 = sum(한화평가금)) %>% pull(합계)
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
        run_stress_replay_from_file(
          asset_file      = "asset_returns_monthly.csv",
          weights         = weights,
          current_nav     = current_nav,
          monthly_contrib = 0
        ),
        silent = TRUE
      ))
      
      suppressWarnings(try(run_garch_vol_alert(dd_ret), silent = TRUE))
      
      suppressWarnings(try(
        cvar_obj <- run_var_cvar_from_file(
          asset_file  = "asset_returns_monthly.csv",
          weights     = weights,
          current_nav = current_nav,
          alpha       = 0.95
        ),
        silent = TRUE
      ))
      
      suppressWarnings(try(
        run_drift_rebal_signal(
          target_weights  = weights,
          current_weights = current_weights,
          threshold       = 0.05
        ),
        silent = TRUE
      ))
    
      # ---------- 종목 테이블 ----------
      # 1. 전일 대비 증감액 및 수익률 계산 로직 (에러 방지를 위해 인덱싱 명확화)
      # dd$Sum의 마지막 값이 오늘, 그 전의 값이 어제
      today_sum <- tail(dd$Sum, 1)
      yesterday_sum <- tail(dd$Sum, 2)[1]
      
      diff_value <- today_sum - yesterday_sum
      diff_pct   <- (diff_value / yesterday_sum) * 100
      
      # 2. 증감액에 따른 색상 및 기호 결정 (글로벌 표준: 수익 파랑 / 손실 빨강)
      diff_color <- if(diff_value > 0) "blue" else if(diff_value < 0) "red" else "black"
      diff_sign  <- if(diff_value > 0) "+" else ""
      
      # 3. [에러 해결 핵심] 타이틀을 하나의 HTML 문자열로 통합 생성
      # <div> 태그와 Flexbox를 사용하여 중앙 정렬 및 한 줄 배치를 구현합니다.
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
      
      # 4. 데이터 테이블 출력
      print(
        datatable(
          rt,
          # htmltools::HTML()을 사용하여 문자열을 안전하게 전달합니다.
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
              "CVaR(95%) : ", comma(round(cvar_obj$cvar_amt, 0)), "원\n",
              "전일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
              "원 (",
              ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
              round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
              "%)  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)\n",
              
              "SPY등:SCHD:QQQ:TQQQ:금:채권(최종목표%) = ", 
              sprintf("%4.1f", as.numeric(weights["SPY_ETC"]) * 100), " : ",
              sprintf("%4.1f", as.numeric(weights["SCHD"]) * 100),  " : ",
              sprintf("%4.1f", as.numeric(weights["QQQ"]) * 100),  " : ",
              sprintf("%4.1f", as.numeric(weights["TQQQ"]) * 100),  " : ",
              sprintf("%4.1f", as.numeric(weights["GOLD"]) * 100),  " : ",
              sprintf("%4.1f", as.numeric(weights["IEF"]) * 100), "\n",
              "SPY등:SCHD:QQQ:TQQQ:금:채권(현재비율%) = ",
              sprintf("%4.1f", asset_SPY_ETC_ratio), " : ",
              sprintf("%4.1f", asset_SCHD_ratio), " : ",
              sprintf("%4.1f", asset_QQQ_ratio), " : ",
              sprintf("%4.1f", asset_TQQQ_ratio), " : ",
              sprintf("%4.1f", asset_GLD_ratio), " : ",
              sprintf("%4.1f", asset_IEF_ratio)
      )
      
      # 두 그래프에 공통으로 적용할 X축 범위를 계산
      common_date_range <- range(dd$Date, na.rm = TRUE)
      # ---------- 상단 플롯(p) ----------
      p <- ggplot(dd, aes(x = Date)) +
              geom_point(aes(y = sum_left, color = Profit / 10000000), size = 5) +
              geom_line(aes(y = sum_left, group = 1), color = "gray") +
              geom_smooth(aes(y = sum_left), method = "lm", se = FALSE,
                          color = "orange", linetype = "dashed", linewidth = 1) +
              geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1) +
              geom_point(aes(y = a * ret_right + b), color = "green", size = 2) +
              
              geom_hline(yintercept = b, color = "yellow2", linewidth = 1.2, alpha = 0.6) +  # 노란색으로 0% 기준선 그리기
              
              scale_color_gradient(low = "red", high = "blue") +
              scale_x_date(date_breaks = "2 months", labels = scales::label_date_short()) +
              scale_y_continuous(
                      name = "보유합계(천만원)",
                      sec.axis = sec_axis(~ (. - b) / a, name = "일간수익률(%)")
              ) +
              labs(
                      title = plot_title,
                      x = paste0(exchange_rate, "원/달러", "(", exchange_diff, ")"),
                      color = "수익"
              ) +
              theme_minimal(base_size = 13) +
              theme(
                      axis.text.x = element_text(angle = 45, hjust = 1),
                      axis.title.y.right = element_text(color = "green"),
                      legend.position = "right",
                      plot.title = element_text(hjust = 0.5, face = "bold")
              ) +
              coord_cartesian(ylim = c(sum_range[1], sum_range[2])) +
              annotate("text",
                       x = min(dd$Date, na.rm = TRUE),
                       y = max(sum_left, na.rm = TRUE),
                       label = label_text,
                       hjust = 0, vjust = 1, size = 5, color = "black") +
              annotate("label",
                       x     = max(dd$Date, na.rm = TRUE),
                       y     = min(sum_left, na.rm = TRUE) * 1.02,
                       label = badge_text,
                       hjust = 1, vjust = 0,
                       size  = 5.5,
                       fontface = "bold",
                       fill  = badge_color,
                       color = "white")
      
      p <- p + scale_x_date(
              limits = common_date_range, # 범위 고정
              date_breaks = "2 months", 
              labels = scales::label_date_short(),
              expand = c(0, 0) # 불필요한 양끝 여백 제거
      )
      
      # ---------- Drawdown 플롯(p_dd) ----------
      dd2 <- dd %>%
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
      dd_mid <- dd %>%
              mutate(
                      Profit_man = Profit / 10000000,           # 주축: 누적수익 (천만원 단위)
                      Return_pct = (Profit / Sum) * 100,        # 보조축: 단순 투자수익률 (%)
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
              # [하단] 누적 수익금 막대 (배경 역할)
              geom_bar(aes(y = Profit_man, fill = Status), 
                       stat = "identity", width = 1, alpha = 0.5, na.rm = TRUE) + # 선을 돋보이게 하기 위해 alpha 조정
              
              # [중앙] 수익률 0% 기준선 (희미한 노란색/금색)
              geom_hline(yintercept = rescale_b, color = "gold", linewidth = 0.8, alpha = 0.6) +
              
              # [상단] 단순 투자수익률 선 그래프
              geom_line(aes(y = Return_pct * rescale_a + rescale_b), 
                        color = "darkslategray4", linewidth = 1) +
              
              # 색상 설정
              scale_fill_manual(values = c("Plus" = "dodgerblue4", "Minus" = "firebrick3")) +
              
              # 축 설정
              scale_x_date(
                      limits = common_date_range,
                      date_breaks = "2 months", 
                      labels = scales::label_date_short(),
                      expand = c(0, 0)
              ) +
              scale_y_continuous(
                      name = "누적수익(천만원)",
                      labels = scales::comma,
                      # 보조축: 수익률(%) 표시
                      sec.axis = sec_axis(~ (. - rescale_b) / rescale_a, name = "투자수익률(%)")
              ) +
              
              # 제목 및 라벨
              labs(
                      title = paste0("누적 성과 추이 (수익금: ", scales::comma(tail(dd$Profit, 1)), 
                                     "원 / 수익률: ", sprintf("%.2f%%", tail(dd_mid$Return_pct, 1)), ")")
              ) +
              
              # 테마 설정
              theme_minimal(base_size = 13) +
              theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.y.right = element_text(color = "darkslategray4", size = 10),
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
                      labels  = scales::percent
              ) +
              scale_y_continuous(
                      name = "Drawdown (%)",
                      sec.axis = sec_axis(~ (. - scale_b) / scale_a, name = "63D Volatility (Annualized %)")
              ) +
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
                      x = "날짜",
                      color = "Drawdown"
              ) +
              theme_minimal(base_size = 13) +
              theme(axis.title.y.right = element_text(color = "purple"),
                    legend.position = "right")
      
      p_dd <- p_dd + scale_x_date(
              limits = common_date_range, # 동일하게 고정
              date_breaks = "2 months", 
              labels = scales::label_date_short(),
              expand = c(0, 0) # 불필요한 양끝 여백 제거
      )
      
      
      # 그래프들을 결합하여 하나의 화면에 보여줌
      # 상(p) / 중(p_mid) / 하(p_dd) 결합
      # 결합 및 레이아웃 수정
      # 상(p) / 중(p_mid) / 하(p_dd) 결합 및 범례 완전 제거
      combined_plot <- (p / p_mid / p_dd) + 
              patchwork::plot_layout(
                      heights = c(2.2, 1, 1) # 높이 비율 유지
              ) & 
              theme(
                      legend.position = "none",        # 모든 범례 제거
                      plot.margin = margin(10, 20, 10, 20), # 좌우 여백을 충분히 주어 깔끔하게 배치
                      axis.title.y.right = element_text(vjust = 1) # 보조축 제목 위치 미세 조정
              )
      
      # 최종 출력 : 여기서 3개 그래프를 합쳐서 출력해줌
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
      
      grDevices::cairo_pdf(filename = pdf_file, width = 11.69, height = 8.27)
      par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
      print(combined_plot)
      dev.off()
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
    
    # 마지막 2행 출력(전일/오늘)
    print(tail(dd, 2))
  }
  
  cat("장중 10분 그이외는 1시간 후에 다시 실행됨(중단을 원하면 Interrupt-R 빨간버튼 클릭) ",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"), "\n\n", sep="")
  
  count <- count + 1
  
  wait_min <- if (in_fast_range & (wday >= 1 & wday <= 5)) 10 else 60
  Sys.sleep(wait_min * 60)
}

# 이 프로그램은 “수익을 만들어주는 엔진”이 아니라
# “수익을 망가뜨리는 행동을 제거해서
# 결과적으로 장기 수익을 극대화하는 장치”입니다.