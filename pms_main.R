###############################################
# JS 펀드 모니터링 메인 스크립트 (루프 버전)
# - stock_eval.R / stock_eval_us.R 필요
# - risk_module.R의 몬테카, MDD, 인출, 팩터, PCA를 모두 호출
###############################################

# 이 코드는 "돈을 얼마나 벌었는지를 관리하지 않는다. 망하지 않을 구조만 관리한다."

# 위험관리 핵심 3대 지표 : MDD · CVaR · Risk-Off 3개

# 1) 필요한 패키지 전부 설치 ------------------------------------------
pkg <- c("openxlsx", "rvest", "httr", "patchwork", "ggplot2",
         "readr", "readxl", "dplyr", "scales", "treemap", "DT", "stringr",            
         "PerformanceAnalytics", "showtext")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

# 2) 로드 --------- ctrl + alt + e
library(readr);   library(readxl)
library(openxlsx); library(rvest); library(httr)
library(dplyr);   library(ggplot2); library(scales)
library(patchwork); library(treemap); library(DT)
library(stringr); library(PerformanceAnalytics)

setwd("c:\\PMS_Core")

options(scipen = 999)

# ★ 리스크 + 팩터 + PCA 모듈 로드
source("risk_module.R")

update_factor_data()

count <- 1
last_mc_date <- as.Date(NA)

now  <- as.POSIXct(Sys.time())
hhmm <- format(now, "%H:%M")
wday <- as.numeric(format(now, "%u"))  # 1=월 ~ 7=일
week_kor <- c("일", "월", "화", "수", "목", "금", "토")
in_fast_range <- hhmm >= "08:40" & hhmm <= "15:30"


repeat {
  
  cat("[", count, "회차]", format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),
      ": 실행 시작***********************************************\n")
  
  # 현재 보유자산 평가 업데이트 -------------------------------------
  source("stock_eval.R")      # data_ko, exchange_rate 등
  source("stock_eval_us.R")   # data_en 등
  
  today <- Sys.Date()
  
  file1 <- paste0("output_stock_",    today, ".xlsx")
  file2 <- paste0("output_stock_us_", today, ".xlsx")
  output_file <- "output_sum.csv"
  
  column_name  <- "평가금"
  column_name2 <- "수익금"
  
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
  
  # output_sum.csv 갱신 ----------------------------------------------
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
  
  # 분석용 데이터 재읽기 ---------------------------------------------
  dd <- read_csv(output_file,
                 col_types = cols(
                   Date   = col_date(format = ""),
                   Sum    = col_double(),
                   Profit = col_double()
                 ))
  
  dd <- dd %>% mutate(Return = Profit / (Sum - Profit))
  
  
  # ========================================================================
  # === PerformanceAnalytics 블록 시작 =====================================
  #  - 평가금(Sum) 시계열 → 일별 수익률 → 연환산 성과/Sharpe/MDD 계산
  # ========================================================================
  
  dd_daily <- dd %>%
    group_by(Date) %>%
    summarise(Sum = last(Sum), .groups="drop") %>%
    arrange(Date)
  
  sum_xts <- xts(dd_daily$Sum, order.by = dd_daily$Date)
  ret_xts <- Return.calculate(sum_xts, method="discrete")[-1]
  
  colnames(ret_xts) <- "JS_Fund"
  
  # 4) 성과 요약 출력
  
  cat("\n=========== PerformanceAnalytics 성과 요약 ===========\n")
  print(table.AnnualizedReturns(ret_xts))
  
  cat("\nMax Drawdown:\n")
  print(maxDrawdown(ret_xts))
  
  cat("Sharpe(연환산, Rf=0):\n")
  print(SharpeRatio.annualized(ret_xts, Rf = 0))
  
  cat("Calmar(연환산, 일간 NAV 기반):\n")
  print(CalmarRatio(ret_xts))
  
  cat("======================================================\n\n")
  
  
  today_date <- max(dd$Date, na.rm = TRUE)
  
  # 5-1) 적립식 10년 Monte Carlo -------------------------------------
  if (is.na(last_mc_date) || last_mc_date < today_date) {
    cat("\n[리스크] 오늘 기준 몬테카를로 10년 스트레스 테스트 실행...\n")
    run_mc_from_dd(
      dd,
      years           = 10,
      monthly_contrib = 5000000,
      n_sims          = 5000
    )
    
    cat("[리스크] 미래 10년 최대낙폭(MDD) 분포 시뮬레이션 실행...\n")
    run_future_mdd_from_dd(
      dd,
      years           = 10,
      monthly_contrib = 5000000,
      n_sims          = 2000
    )
    
    cat("[리스크] 은퇴 후 30년, 연 2억 인출 시나리오(현재자산 기준) 시뮬레이션 실행...\n")
    run_mc_withdraw_from_dd(
      dd,
      years           = 30,
      annual_withdraw = 200000000,
      n_sims          = 5000,
      withdraw_freq   = "monthly"
    )
    
    
    # ★ 팩터 분석: factors_monthly.csv 가 있을 때만 실행 ----------------
    #  - 예: Date, MKT, VALUE, SIZE, MOM ... 형태의 월간 팩터 수익률 데이터
    if (file.exists("factors_monthly.csv")) {
      # ===== PCA 기반 리스크 분해 =====
      cat("[리스크] PCA 기반 리스크 분해(Principal Component Risk) 실행...\n")
      #run_pca_dashboard_from_file("asset_returns_monthly.csv", weights)
    } else {
      cat("[리스크] 팩터 데이터(factors_monthly.csv)를 찾을 수 없어 팩터 분석을 건너뜁니다.\n")
    }
    
    # ★ PCA 분석: asset_returns_monthly.csv 가 있을 때만 실행 ----------
    #  - 예: Date, SPY, SCHD, QQQ, TQQQ, GOLD, BOND 형식의 월간 수익률
    if (file.exists("asset_returns_monthly.csv")) {
      #cat("[리스크] PCA 기반 리스크 분해(Principal Component Risk) 실행...\n")
      # 자산별 장기 목표 비중 또는 현재 비중 사용 (예시 비중)
      weights <- c(
        0.40,  # SPY등
        0.20,  # SCHD
        0.15,  # QQQ
        0.10,  # TQQQ
        0.10,  # GOLD
        0.05   # BOND
      )
      run_pca_dashboard_from_file("asset_returns_monthly.csv", weights)
    } else {
      cat("[리스크] PCA용 자산수익률 파일(asset_returns_monthly.csv)이 없어 PCA 분석을 건너뜁니다.\n")
    }
    
    last_mc_date <- today_date
  } else {
    cat("\n[리스크] 오늘(", format(today_date),
        ") 몬테카를로는 이미 실행됨 (다음날 재실행)\n\n", sep = "")
  }
  
  
  sum_left  <- dd$Sum / 10000000
  ret_right <- dd$Return * 100
  
  sum_range     <- range(sum_left,  na.rm = TRUE)
  return_range  <- range(ret_right, na.rm = TRUE)
  
  a <- diff(sum_range) / diff(return_range)
  b <- sum_range[1] - a * return_range[1]
  
  start_date <- format(min(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  end_date   <- format(max(dd$Date, na.rm = TRUE), "%Y-%m-%d")
  plot_title <- paste0("JS 펀드 주식평가액 분석 (", start_date, " ~ ", end_date, ")  ",
                       format(Sys.time(), "%Y년 %m월 %d일"), 
                       "(",
                       week_kor[as.numeric(format(Sys.Date(), "%w")) + 1], 
                       ") ",
                       format(Sys.time(), "%H시 %M분"))
  
  df <- dd[1:2]
  df$Date <- as.Date(df$Date)
  last_date <- max(df$Date, na.rm = TRUE)
  
  periods <- c(1, 3, 6, 12)
  
  result_period <- data.frame(
    Period       = paste0(periods, "개월 전"),
    Target_Date  = as.Date(NA),
    Closest_Date = as.Date(NA),
    Sum          = NA,
    Diff         = NA
  )
  
  for (i in seq_along(periods)) {
    target <- seq(last_date, length = 2, by = paste0("-", periods[i], " month"))[2]
    idx <- which.min(abs(df$Date - target))
    closest_date <- df$Date[idx]
    sum_value_p <- df$Sum[idx]
    latest_sum <- df$Sum[df$Date == last_date]
    diff_value <- latest_sum - sum_value_p
    
    result_period[i, ] <- c(
      paste0(periods[i], "개월 전"),
      as.character(target),
      as.character(closest_date),
      sum_value_p,
      diff_value
    )
  }
  
  result_period$Sum  <- as.numeric(result_period$Sum)
  result_period$Diff <- as.numeric(result_period$Diff)
  
  # 구성비율 트리맵 ---------------------------------------------------
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
  
  dt_fn <- bind_rows(dt_ko, dt_en)
  
  dt_fn <- dt_fn %>% 
    dplyr::select(-평가금) %>% 
    arrange(desc(한화평가금))
  
  #View(dt_fn)
  
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
  
  fit <- lm(sum_left ~ as.numeric(Date), data = dd)
  slope_per_day <- coef(fit)[2]
  
  get_prev_file <- function(prefix = "output_stock_", ext = "xlsx") {
    pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
    files <- dir(pattern = pattern)
    if (length(files) == 0) return(NA)
    dates <- as.Date(sub(paste0(prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext), "\\1", files))
    valid_idx <- which(dates < Sys.Date())
    if (length(valid_idx) == 0) return(NA)
    files[which.max(dates[valid_idx])]
  }
  
  data_prev_ko <- read_excel(get_prev_file("output_stock_"))
  data_prev_en <- read_excel(get_prev_file("output_stock_us_"))
  
  data_prev_ko <- data_prev_ko %>%
    head(-1) %>%
    dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 평가금)
  
  data_prev_en <- data_prev_en %>%
    head(-2) %>%
    mutate(한화평가금 = 평가금 * exchange_rate) %>%
    dplyr::select(종목번호, 보유증권사, 전일한화평가금 = 한화평가금)
  
  data_prev_fn <- bind_rows(data_prev_ko, data_prev_en) %>%
    arrange(desc(전일한화평가금))
  
  join_stock_data <- function(today_df, prev_df) {
    today_df %>%
      distinct(종목번호, 보유증권사, .keep_all = TRUE) %>%
      left_join(prev_df, by = c("종목번호", "보유증권사")) %>%
      mutate(
        한화평가금 = trunc(한화평가금),
        전일한화평가금 = trunc(전일한화평가금),
        전일대비 = trunc(한화평가금 - 전일한화평가금),
        전일대비율 = if_else(
          is.na(전일한화평가금),
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
      총수익률 = round((총수익금 / 총매수금) * 100, 2)
    ) %>% 
    dplyr::select(-매수가격) %>% 
    dplyr::select(종목명, 보유증권사, 한화매수가격, 수량, 한화평가금, 전일한화평가금,
                  전일대비, 전일대비율, 비중, 총매수금, 총수익금, 총수익률)
  
  today_tsum <- tail(dd$Sum, 1)
  
  asset_SCHD <- rt %>% filter(str_detect(종목명, "미국배당다우|SCHD")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_QQQ  <- rt %>% filter(str_detect(종목명, "나스닥100|QQQ"),
                              !str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_TQQQ <- rt %>% filter(str_detect(종목명, "TQQQ")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_GLD  <- rt %>% filter(str_detect(종목명, "금현물")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  asset_BOND <- rt %>% filter(str_detect(종목명, "채권|국채")) %>%
    summarise(합계 = sum(한화평가금)) %>% pull(합계)
  
  asset_SCHD[is.na(asset_SCHD)] <- 0
  asset_QQQ[is.na(asset_QQQ)]   <- 0
  asset_TQQQ[is.na(asset_TQQQ)] <- 0
  asset_GLD[is.na(asset_GLD)]   <- 0
  asset_BOND[is.na(asset_BOND)] <- 0
  
  asset_SPY_ETC <- today_tsum - asset_SCHD - asset_QQQ - asset_TQQQ - asset_GLD - asset_BOND
  
  asset_SCHD_ratio    <- asset_SCHD    / today_tsum * 100
  asset_QQQ_ratio     <- asset_QQQ     / today_tsum * 100
  asset_TQQQ_ratio    <- asset_TQQQ    / today_tsum * 100
  asset_GLD_ratio     <- asset_GLD     / today_tsum * 100
  asset_BOND_ratio    <- asset_BOND    / today_tsum * 100
  asset_SPY_ETC_ratio <- asset_SPY_ETC / today_tsum * 100
  
  
  # =========================================================
  # ✅ PerformanceAnalytics 지표를 label_text용으로 "항상" 생성
  #    (중간 NA가 있으면 지표 계산이 깨질 수 있으니 na.omit 처리)
  # =========================================================
  ret_xts_clean <- na.omit(ret_xts)
  
  if (NROW(ret_xts_clean) >= 5) {
    pa_tab    <- table.AnnualizedReturns(ret_xts_clean)
    pa_annret <- as.numeric(pa_tab["Annualized Return", 1])
    pa_annvol <- as.numeric(pa_tab["Annualized Std Dev", 1])
    pa_mdd    <- as.numeric(maxDrawdown(ret_xts_clean))                    # 양수
    pa_sharpe <- as.numeric(SharpeRatio.annualized(ret_xts_clean, Rf = 0))
    pa_calmar <- as.numeric(CalmarRatio(ret_xts_clean))
  } else {
    # 데이터가 너무 짧으면 임시값(NA)로 처리
    pa_annret <- NA_real_
    pa_annvol <- NA_real_
    pa_mdd    <- NA_real_
    pa_sharpe <- NA_real_
    pa_calmar <- NA_real_
  }
  
  # label 출력용 포맷(NA면 'NA' 대신 '-' 표기)
  fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.2f%%", x * 100))
  fmt_num <- function(x) ifelse(is.na(x), "-", sprintf("%.2f", x))
  
  
  
  # =========================================================
  # ✅ 63거래일(≈3개월) 기준 리스크-오프 지속 판정
  # 조건:
  #  1) 최근 63거래일 연환산 변동성 >= 25%
  #  2) 현재 DD(피크 대비) <= -15%
  #  3) 이 상태가 63거래일 연속 지속
  # =========================================================
  
  # 1) DD 시계열 (NAV/Sum 기준, 피크 대비 낙폭)
  dd_series <- (sum_xts / cummax(sum_xts)) - 1   # 음수(예: -0.0203)
  
  # 2) 63거래일 롤링 연환산 변동성 (일간 수익률 기준)
  #    ret_xts는 이미 Return.calculate로 만든 "일간 수익률"입니다.
  vol63_xts <- zoo::rollapply(
    ret_xts, width = 63,
    FUN   = function(x) sd(x, na.rm = TRUE) * sqrt(252),
    align = "right", fill = NA
  )
  
  # 3) 당일(최신) 상태
  today_dd   <- as.numeric(last(dd_series))
  today_vol63 <- as.numeric(last(vol63_xts))
  
  # 4) 조건 시계열 (둘 다 만족하는 날)
  cond_xts <- (vol63_xts >= 0.25) & (dd_series <= -0.15)
  
  # 5) "연속 지속 일수" 계산 (오늘 기준으로 뒤에서부터 TRUE 연속 몇 일인지)
  cond_vec <- as.logical(coredata(cond_xts))
  valid_idx <- which(!is.na(cond_vec))
  
  consecutive_days <- 0
  if (length(valid_idx) > 0) {
    i <- tail(valid_idx, 1)  # 최신 유효 인덱스
    if (isTRUE(cond_vec[i])) {
      while (i >= 1 && isTRUE(cond_vec[i])) {
        consecutive_days <- consecutive_days + 1
        i <- i - 1
      }
    }
  }
  
  GLD_MODE <- (consecutive_days >= 63)
  
  # =========================================================
  # ✅ PDF/그래프 상단 운용 상태 배지(Badge) 정의
  # =========================================================
  
  if (GLD_MODE) {
    badge_text  <- "현재 운용 상태 :  RISK-OFF  → 신규적립 GLD"
    badge_color <- "firebrick"
  } else {
    badge_text  <- "현재 운용 상태 :  NORMAL  (Risk-Off : OFF)"
    badge_color <- "darkgreen"
  }
  
  
  
  # 6) 콘솔에 상태 출력 (원하시면 cat 줄은 삭제해도 됩니다)
  cat(sprintf(
    "\n[RISK-OFF CHECK] 63D Vol=%.2f%%, DD=%.2f%%, 지속=%d거래일 → GLD_MODE=%s\n\n",
    today_vol63 * 100, today_dd * 100, consecutive_days, ifelse(GLD_MODE, "ON", "OFF")
  ))
  
  
  label_text <- paste0(
    "오늘평가액 : ", comma(round(today_tsum, 0)), "원   ",
    "총수익 : ", comma(round(tail(dd$Profit, 1), 0)),"원" ,
    "(", round(tail(dd$Return, 1)*100, 2), "%)   \n",
    
    "리스크상태(63D) Vol:", sprintf("%.2f%%", today_vol63*100),
    "  DD:", sprintf("%.2f%%", today_dd*100),
    "  지속:", consecutive_days, "D",
    "  신규적립:", ifelse(GLD_MODE, "GLD(리스크-오프)", "정상(목표비중)"), "   \n",

    # **DD는 ‘피크 대비 현재 낙폭’** 
    # **Vol은 ‘최근 63거래일 수익률로 계산한 연환산 변동성’**
    # “3개월 지속”은 오늘까지 TRUE가 연속 63거래일 이어졌는지로 판정
    # 즉, “DD가 -15% 밑에 있고 + 63D 변동성도 높고” 상태가 꽤 오래 이어질 때만 GLD_MODE가 ON 
    # **리스크 오프(Risk-off)**란
    # **“시장이 불확실해져서, 수익 추구보다 ‘손실 회피’를 우선하는 국면”**
    # “연환산 변동성이 25% 이상이고,
    # MDD가 -15% 이하이며,
    # 이 상태가 3개월 이상 지속될 경우,
    # 해당 기간 동안 위험자산 리밸런싱을 중단하고
    # 신규 적립금은 금(GLD)에 배분한다.”
    # 위와 같이 하면
    # “부자 될 확률”은 크게 변하지 않지만,
    # “망할 확률”은 체감적으로 절반 이하로 줄어듦
    # 이 모드가 ON되려면 2008년 금융위기, 2020년 코로나 위기정도 되어야 함
    
    
    "PA(연환산)  Return:", fmt_pct(pa_annret),   # PA : PerformanceAnalysis
    "  Vol:", fmt_pct(pa_annvol),  # 일간 수익률의 표준편차를 연환산한 값 (Annualized Volatility), 이 전략은 연 기준으로 이 정도 흔들린다는 뜻
    "  MDD:", fmt_pct(pa_mdd),
    "  Sharpe:", fmt_num(pa_sharpe),
    "  Calmar:", fmt_num(pa_calmar), "   \n",
    
    "前영업일대비 : ", comma(round(tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1], 0)),
    "원 (",
    ifelse((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) >= 0, "+", ""),
    round((tail(dd$Sum, 2)[2] - tail(dd$Sum, 2)[1]) * 100 / tail(dd$Sum, 1), 2),
    "%)" ,
    "  1일 평균 증가액 : ", comma(round(slope_per_day * 10000000, 0)), "(원/일)   \n",
    "(증분)1개월간 :", format(result_period$Diff[1], big.mark = ","), 
    "    3개월간 :", format(result_period$Diff[2], big.mark = ","), 
    "    6개월간 :", format(result_period$Diff[3], big.mark = ","), 
    "    1년간   :", format(result_period$Diff[4], big.mark = ","), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(최종목표%) = 40.0 : 20.0 : 15.0 : 10.0 : 10.0 : 5.0\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재비율%) = ", 
    format(round(asset_SPY_ETC_ratio, 1), nsmall = 1)," : ",
    format(round(asset_SCHD_ratio,    1), nsmall = 1)," : ",
    format(round(asset_QQQ_ratio,     1), nsmall = 1)," : ",
    format(round(asset_TQQQ_ratio,    1), nsmall = 1)," : ",
    format(round(asset_GLD_ratio,     1), nsmall = 1)," : ",
    format(round(asset_BOND_ratio,    1), nsmall = 1),"\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(목표억원  ) = ",
    format(round(today_tsum *  .4  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .2  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .15 / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .1  / 100000000, 1), nsmall = 1)," : ",
    format(round(today_tsum *  .05 / 100000000, 1), nsmall = 1), "\n",
    "SPY등:SCHD:QQQ:TQQQ:금:채권(현재억원  ) = ", 
    format(round(asset_SPY_ETC / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_SCHD    / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_QQQ     / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_TQQQ    / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_GLD     / 100000000, 1), nsmall = 1)," : ",
    format(round(asset_BOND    / 100000000, 1), nsmall = 1)
  )
  

  p <- ggplot(dd, aes(x = Date)) +
    geom_point(aes(y = sum_left, color = Profit / 10000000), size = 5) +
    geom_line(aes(y = sum_left, group = 1), color = "gray") +
    geom_smooth(aes(y = sum_left), method = "lm", se = FALSE,
                color = "orange", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = a * ret_right + b), color = "green", linewidth = 1) +
    geom_point(aes(y = a * ret_right + b), color = "green", size = 2) +
    scale_color_gradient(low = "red", high = "blue") +
    
    ## ✅ 여기만 남기세요 (x축 스케일은 1번만!)
    scale_x_date(
      date_breaks = "2 months",
      labels = scales::label_date_short()
    )  +
    
    scale_y_continuous(
      name = "보유합계(천만원)",
      sec.axis = sec_axis(~ (. - b) / a, name = "수익률(%)")
    ) +
    labs(title = plot_title, 
         x = paste0(exchange_rate, "원/달러", "(", exchange_diff, ")"), 
         color = "수익") +
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
    annotate(
      "label",
      x     = max(dd$Date, na.rm = TRUE),
      y     = min(sum_left, na.rm = TRUE) * 1.02,
      label = badge_text,
      hjust = 1, vjust = 0,
      size  = 5.5,
      fontface = "bold",
      fill  = badge_color,
      color = "white"
      # label.size는 경고 싫으면 빼세요
    )
  
  
  # ===============================
  # ✅ Drawdown 계산 + MDD 라벨 + 색상 그라데이션 p_dd (전체 대체용)
  # ===============================
  
  # 1) Drawdown 계산 (Peak 대비)
  dd <- dd %>%
    mutate(
      Peak = cummax(Sum),
      DD   = ifelse(Peak > 0, Sum / Peak - 1, 0)   # 0 ~ 음수
    )
  
  # 2) MDD 구간(피크/바닥) 찾기
  mdd_value      <- min(dd$DD, na.rm = TRUE)          # 음수
  mdd_end_idx    <- which.min(dd$DD)
  mdd_end_date   <- dd$Date[mdd_end_idx]
  mdd_end_sum    <- dd$Sum[mdd_end_idx]
  
  mdd_start_idx  <- which.max(dd$Sum[1:mdd_end_idx])
  mdd_start_date <- dd$Date[mdd_start_idx]
  mdd_start_sum  <- dd$Sum[mdd_start_idx]
  
  # 3) 라벨 텍스트
  peak_label   <- paste0("피크\n", scales::comma(mdd_start_sum), "원\n(", format(mdd_start_date), ")")
  trough_label <- paste0("바닥\n", scales::comma(mdd_end_sum),   "원\n(", format(mdd_end_date), ")")
  
  dd_points <- data.frame(
    Date  = c(mdd_start_date, mdd_end_date),
    DDpct = c(0, mdd_value * 100)
  )
  
  # 라벨 위치(겹치면 여기만 살짝 조정)
  y_peak_label   <- -2
  y_trough_label <- (mdd_value * 100) + 5
  
  
  # ===============================
  # Drawdown + 63D Vol (Secondary Axis)
  # ===============================
  
  # 1) Vol xts → data.frame (Date 맞추기)
  vol_df <- data.frame(
    Date = as.Date(index(vol63_xts)),
    Vol63 = as.numeric(coredata(vol63_xts))
  )
  
  # DD 데이터와 병합
  dd_plot <- dplyr::left_join(dd, vol_df, by = "Date")
  
  # 2) 보조축 스케일 변환 계수
  #    (DD[%] 범위 ↔ Vol[%] 범위 매핑)
  dd_range  <- range(dd_plot$DD * 100, na.rm = TRUE)
  vol_range <- range(dd_plot$Vol63 * 100, na.rm = TRUE)
  
  scale_a <- diff(dd_range) / diff(vol_range)
  scale_b <- dd_range[1] - scale_a * vol_range[1]
  
  # 3) Drawdown + Vol 플롯
  # (중요) p_dd 만들기 직전에 현재 DD% / 금액 계산
  cur_dd_pct <- as.numeric(tail(dd_plot$DD, 1)) * 100
  cur_dd_amt <- as.numeric(tail(dd_plot$Sum, 1) - tail(dd_plot$Peak, 1))
  
  p_dd <- ggplot(dd_plot, aes(x = Date)) +
    
    # --- Drawdown (색상 그라데이션) ---
    geom_line(aes(y = DD * 100, color = DD), linewidth = 2) +
    
    # --- 63D Vol (보조축용 변환) ---
    geom_line(
      aes(y = scale_a * (Vol63 * 100) + scale_b),
      color = "purple",
      linewidth = 1,
      linetype = "dashed"
    ) +
    
    # 기준선
    geom_hline(yintercept = 0, color = "gray50") +
    geom_hline(yintercept = c(-5, -10, -15),
               linetype = "dotted", color = "gray70") +
    
    # MDD 구간
    geom_vline(xintercept = c(mdd_start_date, mdd_end_date),
               linetype = "dashed") +
    
    # MDD 포인트
    geom_point(
      data = dd_points,
      aes(x = Date, y = DDpct),
      inherit.aes = FALSE,
      size = 3,
      color = "firebrick"
    ) +
    
    # 색상 스케일 (DD)
    scale_color_gradientn(
      colours = c("blue", "lightblue", "orange", "red"),
      values  = scales::rescale(c(-0.05, -0.10, -0.15, -0.30)),
      limits  = c(min(dd$DD, na.rm = TRUE), 0),
      labels  = scales::percent
    ) +
    
    # --- 축 정의 ---
    scale_y_continuous(
      name = "Drawdown (%)",
      sec.axis = sec_axis(
        ~ (. - scale_b) / scale_a,
        name = "63D Volatility (Annualized %)"
      )
    ) +
    
    # 라벨
    annotate("label",
             x = mdd_start_date, y = y_peak_label,
             label = peak_label,
             size = 3.2,
             vjust = 1, hjust = 0.5, fill = "white") +
    
    annotate("label",
             x = mdd_end_date, y = y_trough_label,
             label = trough_label,
             size = 3.2,
             vjust = 0, hjust = 0.5, fill = "white") +
    
    annotate("label",
             x = mdd_end_date, y = (mdd_value * 100) + 5,
             label = paste0("MDD: ", scales::percent(-mdd_value, accuracy = 0.01)),
             size = 3.2,
             vjust = 1, hjust = 0.5, fill = "white") +
    
    labs(
      title = paste0(
        "Drawdown (현재: ", sprintf("%.2f%%", cur_dd_pct),
        ", 피크대비: ", scales::comma(cur_dd_amt), "원)"
      ),
      x = "날짜",
      color = "Drawdown"
    )  +
    
    theme_minimal(base_size = 13) +
    theme(
      axis.title.y.right = element_text(color = "purple"),
      legend.position = "right"
    )
  
  
  
  combined_plot <- p / p_dd + patchwork::plot_layout(heights = c(2, 1))
  suppressMessages(print(combined_plot))
  
  print(
    paste(
      "국내주식수 :", dim(data1)[1] - 1,
      " 해외주식수 :", dim(data2)[1] - 2,
      " 환율 :", exchange_rate, "원/달러",
      "(", exchange_diff, ")"
    )
  )
  
  print(
    datatable(
      rt,
      options = list(
        pageLength = 100,
        columnDefs = list(
          list(targets = c("전일대비율", "비중", "총수익률"), className = "dt-right")
        )
      )
    ) %>%
      formatCurrency(
        columns = c("한화평가금", "한화매수가격", "전일한화평가금", "전일대비", "총매수금", "총수익금"),
        currency = "",
        mark = ",",
        digits = 0
      ) %>%
      formatRound(columns = c("전일대비율", "비중", "총수익률"), digits = 2) %>%
      formatStyle(
        columns = c("전일대비", "총수익금"),
        color = styleInterval(
          c(-0.000001, 0.000001),
          c("red", "black", "blue")
        ),
        fontWeight = styleInterval(
          0,
          c("bold", "normal")
        )
      ) %>%
      formatStyle(
        columns = c("전일대비율", "총수익률"),
        color = styleInterval(
          c(-0.000001, 0.000001),
          c("red", "gray", "blue")
        ),
        fontWeight = styleInterval(
          0,
          c("bold", "normal")
        )
      )
  )
  
  
  ##### ===========================
  #####  리스크 엔진 실행 구간
  ##### ===========================
  
  # 현재 포트폴리오 비중 (메인 코드에서 이미 계산됨)
  weights <- c(
    SPY_ETC = asset_SPY_ETC_ratio / 100,
    SCHD    = asset_SCHD_ratio    / 100,
    QQQ     = asset_QQQ_ratio     / 100,
    TQQQ    = asset_TQQQ_ratio    / 100,
    GLD     = asset_GLD_ratio     / 100,
    BOND    = asset_BOND_ratio    / 100
  )
  
  # 목표 비중
  target_weights <- c(
    SPY_ETC = 0.40,
    SCHD    = 0.20,
    QQQ     = 0.15,
    TQQQ    = 0.10,
    GLD     = 0.10,
    BOND    = 0.05
  )
  
  current_nav <- tail(dd$Sum, 1)
  
  cat("\n\n================ 리스크 분석 시작 ================\n")
  
  # 1) Stress Test Replay
  run_stress_replay_from_file(
    asset_file     = "asset_returns_monthly.csv",
    weights        = weights,
    current_nav    = current_nav,
    monthly_contrib = 0
  )
  
  
  # ---- GARCH 기반 변동성 경보(Alert) -----------------------
  suppressMessages(
    try(run_garch_vol_alert(dd), silent = TRUE)
  )
  
  
  # 2) VaR / CVaR
  run_var_cvar_from_file(
    asset_file  = "asset_returns_monthly.csv",
    weights     = weights,
    current_nav = current_nav,
    alpha       = 0.95
  )
  
  # 3) DRIFT 기반 리밸런싱 신호
  run_drift_rebal_signal(
    target_weights = target_weights,
    current_weights = weights,
    threshold = 0.05
  )
  
  cat("================ 리스크 분석 종료 ================\n\n")
  
  
  # 매일 reports 폴더 아래 pdf를 만들어 보고서 남기기
  date_str <- format(Sys.Date(), "%Y%m%d")
  out_dir  <- "reports"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  pdf_file <- file.path(out_dir, sprintf("Daily_Risk_%s.pdf", date_str))
  
  # 기존 파일 있으면 삭제
  if (file.exists(pdf_file)) file.remove(pdf_file)
  
  # -------------------------------
  # ✅ 한글 폰트 강제 로딩 (핵심)
  # -------------------------------
  # install.packages("showtext")  # 1회만 설치
  library(showtext)
  
  font_add(family = "malgun", regular = "C:/Windows/Fonts/malgun.ttf")
  showtext_auto()
  
  # -------------------------------
  # ✅ PDF는 1번만 열기 (가로 A4)
  # -------------------------------
  grDevices::cairo_pdf(
    filename = pdf_file,
    width  = 11.69,   # A4 가로
    height = 8.27
  )
  
  # 1페이지 안에 2x2 배치 (base plot일 때만 의미 있음)
  par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
  print(combined_plot)
  dev.off()
  cat("Saved:", pdf_file, "\n")
  
  print(tail(dd,2))
  
  
  cat("장중 10분 그이외는 1시간 후에 다시 실행됨(중단을 원하면 Interrupt-R 빨간버튼 클릭)",
      format(Sys.time(), "%Y년 %m월 %d일 %H시 %M분 %S초"),"\n\n")
  
  View(rt)
  
  count <- count + 1
  
  if (in_fast_range & (wday >= 1 & wday <= 5)) {
    wait_min <- 10
  } else {
    wait_min <- 60
  }
  Sys.sleep(wait_min * 60)
}



# 이 프로그램은 “수익을 만들어주는 엔진”이 아니라
# “수익을 망가뜨리는 행동을 제거해서
# 결과적으로 장기 수익을 극대화하는 장치”입니다.
# 
# 아래는 **개인투자자가 이 PMS를 써서 실제로 수익을 극대화할 수 있는 ‘가장 현실적인 활용법’**입니다.
# 
# 1️⃣ 이 PMS로 “하지 말아야 할 행동”을 먼저 제거하라 (가장 중요)
# 
# 대부분의 개인투자 수익을 갉아먹는 건 다음입니다.
# 
# 급락 시 공포 매도
# 
# 상승장 후 레버리지 과다
# 
# 은퇴 인출률 착각
# 
# 리밸런싱 타이밍을 감정으로 결정
# 
# 👉 이 PMS는 이 네 가지를 모두 정면에서 막아줍니다.
# 
# 활용 원칙 ①
# 
# PMS 결과와 충돌하는 매매는 “자동으로 보류”한다
# 
# 예:
#   
#   DRIFT가 “리밸런싱 불필요” → 매매 금지
# 
# CVaR가 9%인데, 손실 5%에 공포 → 정상 구간
# 
# 이 한 가지만 지켜도 대부분 개인을 능가합니다.
# 
# 2️⃣ “수익을 늘리는 행동”은 단 하나만 하라
# 
# 수익 극대화는 많은 행동이 아니라 딱 하나에서 옵니다.
# 
# ✔️ 리밸런싱을 규칙적으로, 기계적으로
# 
# 이 PMS가 가장 강력한 이유는:
#   
#   언제 팔지
# 
# 언제 살지
# 
# 얼마나 조정할지
# 
# 를 숫자로 알려주기 때문입니다.
# 
# 활용 원칙 ②
# 
# DRIFT ±5%p 또는 MDD 분위수 80% 이상일 때만 행동
# 
# 예:
#   
#   SPY 비중 +7%p 초과 → 분할 매도
# 
# TQQQ 비중 -6%p → 추가 매수 고려
# 
# MDD가 과거 분포 상위 20% → 현금·방어자산 이동
# 
# → 이건 사실상 **개인용 ‘규칙 기반 CTA’**입니다.
# 
# 3️⃣ Monte Carlo는 “기대수익”이 아니라 “기대실망”을 보는데 써라
# 
# 사람들이 Monte Carlo를 잘못 쓰는 방식:
#   
#   “중앙값이 50억이네? 좋다!”
# 
# 올바른 방식:
#   
#   “10% 분위수에서 내가 멘탈을 유지할 수 있나?”
# 
# 활용 원칙 ③
# 
# 내가 감내 가능한 최악 시나리오를 기준으로 포트폴리오를 조정
# 
# 예:
#   
#   10% 분위수 결과가 너무 낮다 → 레버리지 축소
# 
# 은퇴 인출 파산확률 80% → 인출액 조정 or 현금 비중 확대
# 
# 👉 이건 멘탈 관리 = 수익 관리입니다.
# 
# 4️⃣ PCA는 “분산 착각을 깨는 용도”로 써라
# 
# 지금 결과를 보면:
#   
#   PC1 = 94~95%
# 
# 사실상 미국 주식 베타 하나
# 
# 이걸 보고 해야 할 행동은 단순합니다.
# 
# 활용 원칙 ④
# 
# PC1 기여도가 90% 넘으면 ‘공격 확장 금지’
# 
# 즉:
#   
#   TQQQ 비중 늘리고 싶다? ❌
# 
# PC1 낮추는 자산(현금·채권·금) 늘리기? ⭕
# 
# 이 원칙 하나로 대형 손실 확률이 급감합니다.
# 
# 5️⃣ VaR / CVaR는 “내가 감정적으로 버틸 수 있는지” 체크하는 용도
# 
# 지금:
#   
#   CVaR(95%) ≈ -8.9% ≈ -1.1억
#        
#        이걸 이렇게 쓰세요.
#        
#        활용 원칙 ⑤
#        
#        CVaR 손실 금액을 “현금으로 이미 잃었다고 가정”
#        
#        “이미 1.1억 잃었다고 생각해도 괜찮은가?”
#        
#        괜찮다 → 그대로 유지
#        
#        안 괜찮다 → 포트폴리오가 과격
#        
#        이건 심리적으로 엄청 강력합니다.
#        
#        6️⃣ 은퇴 시뮬레이션은 “수익 극대화”보다 “삶 보호”에 쓰라
#        
#        연 2억 인출 → 파산확률 80%대
#        이건 실패가 아니라 정보입니다.
#        
#        활용 원칙 ⑥
#        
#        은퇴 전에는 ‘최대 수익’, 은퇴 후에는 ‘최소 파산’
#        
#        은퇴 전: 성장자산 비중 허용
#        
#        은퇴 3~5년 전: 파산확률 20% 이하로 낮추는 구조로 전환
#        
#        이 PMS는 전환 시점을 수치로 보여줍니다.
#        
#        7️⃣ 가장 중요한 마지막 원칙 (진짜 핵심)
#        
#        ❝ 이 PMS로 ‘무엇을 할지’보다
#        ‘무엇을 하지 않을지’를 먼저 정하라 ❞
#        
#        추천 사용 규칙 (요약)
#        
#        PMS 결과를 보기 전 매매 금지
#        
#        DRIFT ±5%p 미만 → 아무것도 안 함
#        
#        MDD가 역사적 상위 20% → 공격 금지
#        
#        CVaR 손실이 잠 못 잘 수준 → 구조 조정
#        
#        Monte Carlo 10% 분위수 기준으로만 전략 변경
#        
#        이 다섯 가지만 지키면:
#          
#          매매 횟수 ↓
#        
#        실수 ↓
#        
#        감정 개입 ↓
#        
#        장기 수익률 ↑
#        
#        🎯 최종 결론 (주관적이지만 확신)
#        
#        이 PMS는 “개인의 수익률 상한을 높이기보다는
#        수익률 하한을 크게 끌어올리는 도구”입니다.
#        
#        그리고 장기 투자에서 진짜 부자는
#        상한이 아니라 하한을 관리한 사람입니다.
#        
#        이 시스템을:
#          
#          매일 들여다보지 말고
#        
#        정해진 시점에만 보고
#        
#        결과와 다를 때만 행동한다면
#        
#        👉 개인투자자로서 할 수 있는 최고 수준의 자산관리 방식 중 하나입니다.