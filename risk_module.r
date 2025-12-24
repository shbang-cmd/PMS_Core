###############################################################################
# risk_module.R  (리스크 분석 모듈 모음)
###############################################################################

suppressMessages(library(dplyr))

# ---- GARCH용 패키지 체크 ------------------------------------
.has_rugarch <- requireNamespace("rugarch", quietly = TRUE)

###############################################################################
# 0) 파일 기반 팩터 회귀 (Date 키 고정 버전)
###############################################################################
run_factor_model_from_files <- function(asset_returns_file, factors_file, weights) {
  
  asset_df  <- readr::read_csv(asset_returns_file,  show_col_types = FALSE)
  factor_df <- readr::read_csv(factors_file,        show_col_types = FALSE)
  
  # --- 키는 Date로 고정 (YM은 절대 쓰지 않음) ---
  if (!("Date" %in% names(asset_df)))  stop("asset_returns_file에 Date 컬럼이 없습니다.")
  if (!("Date" %in% names(factor_df))) stop("factors_file에 Date 컬럼이 없습니다.")
  
  # --- 포트수익률(Y) 생성: asset returns × weights ---
  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop("weights는 names가 있어야 합니다. 예: c(SPY=0.4, SCHD=0.2, ...)")
  }
  
  asset_cols <- setdiff(names(asset_df), "Date")
  common_assets <- intersect(asset_cols, names(weights))
  if (length(common_assets) < 2) stop("asset_returns 자산명과 weights names 매칭이 부족합니다.")
  
  w <- weights[common_assets]
  w <- w / sum(w)
  
  port_ret <- as.numeric(as.matrix(asset_df[, common_assets]) %*% as.numeric(w))
  port_df  <- data.frame(Date = asset_df$Date, port = port_ret)
  
  need <- c("MKT","VALUE","GROWTH","MOM")
  miss <- setdiff(need, names(factor_df))
  if (length(miss) > 0) stop(paste("factors_file에 컬럼이 없습니다:", paste(miss, collapse=", ")))
  
  fact_df2 <- factor_df %>% dplyr::select(Date, dplyr::all_of(need))
  
  # --- Date로 병합 ---
  dat <- dplyr::inner_join(port_df, fact_df2, by = "Date") %>% stats::na.omit()
  if (nrow(dat) < 24) stop("팩터회귀 표본이 너무 적습니다(24개월 미만).")
  
  fit <- lm(port ~ MKT + VALUE + GROWTH + MOM, data = dat)
  
  cat("========================================\n")
  cat(" 팩터 회귀모형 결과\n")
  cat(" 포트 수익률 ~ MKT + VALUE + GROWTH + MOM\n")
  cat(" (표본:", nrow(dat), "개월)\n")
  cat("========================================\n\n")
  print(summary(fit))
  
  invisible(fit)
}

###############################################################################
# 0. dd로부터 "현금흐름 보정 일별 운용수익률" 계산  (방탄/Date 정렬 유지)
###############################################################################
# dd: 최소한 아래 컬럼을 가져야 함
#   - Date  : 날짜 (Date 형)
#   - Sum   : 전체 평가금액
#   - Profit: 총수익금 (Sum - 투자원금)
#
# 개념:
#   Invested_t  = Sum_t - Profit_t            (누적 투자원금)
#   Flow_t      = Invested_t - Invested_{t-1} (t일에 외부에서 새로 들어온/나간 돈)
#   Sum_t       = (Sum_{t-1} + Flow_t) * (1 + r_t)
#   ⇒ r_t       = Sum_t / (Sum_{t-1} + Flow_t) - 1
#
# [중요 수정]
# - 기존 버전처럼 NA 제거/클리핑으로 "벡터 길이를 줄여 반환"하면
#   메인에서 dd와 Return 매칭이 깨질 수 있습니다.
# - 따라서 Date별 Return을 NA 포함으로 유지하여 data.frame으로 반환합니다.
###############################################################################
compute_daily_returns_from_dd <- function(dd, ret_clip = 0.5) {
  dd <- dd %>% arrange(Date)
  
  if (!all(c("Date","Sum","Profit") %in% colnames(dd))) {
    stop("compute_daily_returns_from_dd: dd에 'Date','Sum','Profit' 컬럼이 필요합니다.")
  }
  
  dd2 <- dd %>%
    mutate(
      Invested      = Sum - Profit,             # 누적 투자원금
      Invested_lag  = dplyr::lag(Invested),
      Sum_lag       = dplyr::lag(Sum),
      Flow          = Invested - Invested_lag,  # t일에 새로 들어온 순 현금
      Gross_base    = Sum_lag + Flow,           # 운용 대상 자산
      Return        = dplyr::if_else(
        !is.na(Gross_base) & Gross_base > 0,
        Sum / Gross_base - 1,
        NA_real_
      )
    )
  
  # ±50% 이상은 데이터 오류/극단값 가능성이 커서 제거(원하면 주석 처리 가능)
  dd2 <- dd2 %>%
    mutate(Return = dplyr::if_else(!is.na(Return) & abs(Return) < ret_clip, Return, NA_real_))
  
  # ✅ Date별 Return 유지 (NA 포함)
  dd2 %>% dplyr::select(Date, Return)
}

###############################################################################
# 1. 포트폴리오 μ, σ 추정 (현금흐름 보정 버전)
###############################################################################
# dd: Date, Sum, Profit 포함한 데이터프레임
#
# 출력:
#   - mu_daily, sigma_daily   : 일간 기대수익률, 변동성
#   - mu_annual, sigma_annual : 연환산 기대수익률, 변동성 (252 영업일 가정)
###############################################################################
estimate_mu_sigma_from_dd <- function(dd) {
  dd <- dd %>% arrange(Date)
  
  if (nrow(dd) < 2) {
    stop("estimate_mu_sigma_from_dd: dd에 최소 2개 이상의 행이 필요합니다.")
  }
  
  ret_df <- compute_daily_returns_from_dd(dd)
  r_daily <- ret_df$Return
  r_daily <- r_daily[is.finite(r_daily)]
  
  if (length(r_daily) == 0) {
    stop("estimate_mu_sigma_from_dd: 유효한 일별 수익률이 없습니다.")
  }
  if (length(r_daily) < 10) {
    warning("estimate_mu_sigma_from_dd: 유효한 일별 수익률이 10개 미만입니다. 추정치 신뢰도가 낮을 수 있습니다.")
  }
  
  mu_daily    <- mean(r_daily, na.rm = TRUE)
  sigma_daily <- sd(r_daily,   na.rm = TRUE)
  
  mu_annual    <- mu_daily * 252
  sigma_annual <- sigma_daily * sqrt(252)
  
  list(
    mu_daily     = mu_daily,
    sigma_daily  = sigma_daily,
    mu_annual    = mu_annual,
    sigma_annual = sigma_annual
  )
}

###############################################################################
# 2. 적립식 몬테카를로 (현역기 - accumulation phase)
###############################################################################
# 입력:
#   dd              : Date, Sum, Profit 포함
#   years           : 시뮬레이션 기간(년)
#   monthly_contrib : 매월 적립금(원)
#   n_sims          : 시뮬레이션 횟수
###############################################################################
run_mc_from_dd <- function(dd,
                           years = 10,
                           monthly_contrib = 5000000,
                           n_sims = 5000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)        # 현재 평가액
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12   # 월 단위
  sim_terminal <- numeric(n_sims)
  set.seed(123)
  
  for (i in seq_len(n_sims)) {
    V <- V0
    for (m in 1:(years * 12)) {
      V <- V + monthly_contrib   # 매월 적립
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V <- V * exp(r)
    }
    sim_terminal[i] <- V
  }
  
  qs       <- quantile(sim_terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(sim_terminal)
  
  cat("========================================\n")
  cat(" 몬테카를로 기반", years, " 년 후 평가액 분포\n")
  cat("  (", n_sims, "회 시뮬레이션, 월 적립",
      format(monthly_contrib, big.mark = ","), "원)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수 (나쁜 장세) :",
      format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 50% 분위수 (중앙값)   :",
      format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 90% 분위수 (좋은 장세) :",
      format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균값(Mean)          :",
      format(round(mean_val, 0), big.mark = ","), "원\n")
  cat(" 연 기대수익률(추정)   :",
      round(mu * 100, 2), "%,  연 변동성(추정) :", round(sigma * 100, 2), "%\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = sim_terminal,
    summary = list(
      q10          = as.numeric(qs[1]),
      q50          = as.numeric(qs[2]),
      q90          = as.numeric(qs[3]),
      mean         = mean_val,
      mu_annual    = mu,
      sigma_annual = sigma
    )
  ))
}

###############################################################################
# 3. 미래 최대낙폭(MDD) 분포 시뮬레이션
###############################################################################
run_future_mdd_from_dd <- function(dd,
                                   years = 10,
                                   monthly_contrib = 5000000,
                                   n_sims = 2000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12
  m_per_year <- 12
  n_steps <- years * m_per_year
  
  mdd_vec <- numeric(n_sims)
  set.seed(456)
  
  for (i in seq_len(n_sims)) {
    V <- numeric(n_steps + 1)
    V[1] <- V0
    peak <- V0
    dd_path <- numeric(n_steps + 1)
    dd_path[1] <- 0
    
    for (t in 1:n_steps) {
      # 적립
      V[t] <- V[t] + monthly_contrib
      # 수익률
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V[t + 1] <- V[t] * exp(r)
      
      # drawdown 갱신
      if (V[t + 1] > peak) {
        peak <- V[t + 1]
      }
      dd_path[t + 1] <- (V[t + 1] / peak) - 1
    }
    
    mdd_vec[i] <- min(dd_path, na.rm = TRUE)  # 가장 낮은 DD
  }
  
  mdd_percent <- mdd_vec * 100
  qs <- quantile(mdd_percent, c(0.1, 0.5, 0.9))
  
  cat("========================================\n")
  cat(" 미래", years, " 년 동안 최대낙폭(MDD) 분포 (적립 포함)\n")
  cat("  (", n_sims, "경로 시뮬레이션)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수(하위 10%: 비교적 크게 빠지는 경우)   :",
      round(qs[1], 2), "%\n")
  cat(" 50% 분위수(중앙값: 보통 한 번쯤 겪을 만한 MDD)  :",
      round(qs[2], 2), "%\n")
  cat(" 90% 분위수(상위 10%: 상대적으로 덜 빠지는 경우) :",
      round(qs[3], 2), "%\n")
  cat("----------------------------------------\n")
  cat(" * 음수 값이므로 -30% 라는 뜻은 피크 대비 30% 하락을 의미\n")
  cat("========================================\n\n")
  
  invisible(list(
    mdd_raw      = mdd_vec,
    mdd_percent  = mdd_percent,
    summary      = list(q10 = qs[1], q50 = qs[2], q90 = qs[3]),
    mu_annual    = mu,
    sigma_annual = sigma
  ))
}

###############################################################################
# 4. 은퇴 후 인출 시나리오 Monte Carlo (decumulation phase)
###############################################################################
run_mc_withdraw_from_dd <- function(dd,
                                    years = 30,
                                    annual_withdraw = 200000000,
                                    n_sims = 5000,
                                    withdraw_freq = c("annual", "monthly"),
                                    initial_value = NULL) {
  withdraw_freq <- match.arg(withdraw_freq)
  
  est <- estimate_mu_sigma_from_dd(dd)
  
  # 시작자산: initial_value가 지정되면 그 값 사용, 아니면 dd의 마지막 Sum
  V0 <- if (is.null(initial_value)) {
    tail(dd$Sum, 1)
  } else {
    initial_value
  }
  
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  set.seed(789)
  
  ruin_flag <- logical(n_sims)
  terminal  <- numeric(n_sims)
  
  if (withdraw_freq == "annual") {
    dt_year <- 1
    n_steps <- years
    withdraw_step <- annual_withdraw
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1년 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  } else if (withdraw_freq == "monthly") {
    dt_year <- 1 / 12
    n_steps <- years * 12
    withdraw_step <- annual_withdraw / 12
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1개월 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  }
  
  ruin_prob <- mean(ruin_flag)
  qs <- quantile(terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(terminal)
  
  cat("========================================\n")
  cat(" 은퇴 후 인출 시나리오 Monte Carlo\n")
  cat("  기간 :", years, "년,  연 인출액:",
      format(annual_withdraw, big.mark = ","), "원\n", sep = "")
  cat("  시작자산 :", format(round(V0, 0), big.mark = ","), "원\n")
  cat("  가정: 연 기대수익률", round(mu * 100, 2),
      "%, 연 변동성", round(sigma * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 파산 확률 (자산이 0 이하로 떨어질 확률) :", round(ruin_prob * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 기간 종료 시 잔고 10% 분위수 :", format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 50% 분위수 :", format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 90% 분위수 :", format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균 잔고(Mean)              :", format(round(mean_val, 0), big.mark = ","), "원\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = terminal,
    ruin_flag       = ruin_flag,
    ruin_prob       = ruin_prob,
    summary         = list(
      q10  = as.numeric(qs[1]),
      q50  = as.numeric(qs[2]),
      q90  = as.numeric(qs[3]),
      mean = mean_val
    ),
    mu_annual       = mu,
    sigma_annual    = sigma
  ))
}

###############################################################################
# 5. 팩터별 분석 (Factor Regression) - 방탄 버전
###############################################################################
run_factor_model <- function(port_ret, factors_df) {
  
  # ---------------------------
  # ✅ 방탄 1) factors_df 비정상 입력 방어
  # ---------------------------
  if (is.null(port_ret) || length(port_ret) == 0) {
    stop("run_factor_model: port_ret 길이가 0입니다.")
  }
  if (is.null(factors_df) || nrow(factors_df) == 0) {
    stop("run_factor_model: factors_df가 비어있습니다.")
  }
  
  # ---------------------------
  # ✅ 방탄 2) 날짜/키 컬럼 제거 (회귀 X에 들어가면 망함)
  # ---------------------------
  drop_cols <- intersect(names(factors_df), c("YM", "Ym", "ym", "DATE", "Date", "date"))
  if (length(drop_cols) > 0) {
    factors_df <- factors_df[, setdiff(names(factors_df), drop_cols), drop = FALSE]
  }
  
  # ---------------------------
  # ✅ 방탄 3) 숫자형 컬럼만 남기기
  # ---------------------------
  is_num <- vapply(factors_df, function(x) is.numeric(x) || is.integer(x), logical(1))
  factors_df <- factors_df[, is_num, drop = FALSE]
  
  if (ncol(factors_df) == 0) {
    stop("run_factor_model: 회귀에 사용할 numeric 팩터 컬럼이 0개입니다. (YM/Date만 있었던 케이스)")
  }
  
  # ---------------------------
  # ✅ 방탄 4) 길이 일치 / NA 정리
  # ---------------------------
  if (length(port_ret) != nrow(factors_df)) {
    stop(sprintf("run_factor_model: 길이 불일치 port_ret=%d, factors_df=%d",
                 length(port_ret), nrow(factors_df)))
  }
  
  dat <- data.frame(port = port_ret, factors_df)
  dat <- na.omit(dat)
  
  if (nrow(dat) < 12) {
    stop("run_factor_model: 유효표본이 너무 적습니다(NA 제거 후 12개 미만).")
  }
  
  form <- as.formula(paste("port ~", paste(colnames(dat)[-1], collapse = " + ")))
  fit <- lm(form, data = dat)
  
  cat("========================================\n")
  cat(" 팩터 회귀모형 결과\n")
  cat(" 포트 수익률 ~", paste(colnames(dat)[-1], collapse = " + "), "\n")
  cat("========================================\n\n")
  print(summary(fit))
  
  invisible(fit)
}

###############################################################################
# 6. quantmod 기반 월간 자산수익률 + 팩터 CSV 업데이트
###############################################################################
update_factor_data <- function(symbols = c("SPY","SCHD","QQQ","TQQQ","GLD","IEF"),
                               start_date = "2010-01-01",
                               save_path = getwd()) {
  
  suppressMessages(library(quantmod))
  suppressMessages(library(dplyr))
  suppressMessages(library(readr))
  suppressMessages(library(tidyr))
  
  setwd(save_path)
  
  # 1) 최신 데이터 다운로드
  getSymbols(symbols, from = start_date, auto.assign = TRUE)
  
  # 2) 월간 자산 수익률 계산
  monthly_list <- lapply(symbols, function(sym) {
    px <- Ad(get(sym))
    ret <- monthlyReturn(px, type = "arithmetic")
    colnames(ret) <- sym
    ret
  })
  
  all_monthly_xts <- do.call(merge, monthly_list)
  asset_returns <- data.frame(Date = as.Date(index(all_monthly_xts)),
                              coredata(all_monthly_xts)) %>%
    drop_na()
  
  write_csv(asset_returns, "asset_returns_monthly.csv")
  
  # 3) 팩터 계산
  factors <- asset_returns %>%
    mutate(
      MKT    = SPY,
      VALUE  = SCHD - SPY,
      GROWTH = QQQ  - SPY,
      MOM    = TQQQ - QQQ,
      YM     = format(Date, "%Y-%m")
    ) %>%
    select(Date, YM, MKT, VALUE, GROWTH, MOM)
  
  write_csv(factors, "factors_monthly.csv")
  
  cat("[팩터/자산수익률 데이터 자동 업데이트 완료]\n")
}

###############################################################
# (1) CSV 불러와서 팩터 회귀 & 요약 출력하는 함수
###############################################################
run_factor_dashboard_from_file <- function(dd, factor_file = "factors_monthly.csv") {
  if (!file.exists(factor_file)) {
    cat("[리스크] 팩터 파일(", factor_file, ")을 찾을 수 없어 분석을 건너뜁니다.\n")
    return(NULL)
  }
  
  suppressMessages(library(dplyr))
  suppressMessages(library(readr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(broom))
  
  # 1) 팩터 데이터 읽기
  factors <- read_csv(factor_file, show_col_types = FALSE)
  
  # 2) dd(포트폴리오 평가금)에서 월간 수익률 추출
  dd_month <- dd %>%
    mutate(YM = format(Date, "%Y-%m")) %>%
    group_by(YM) %>%
    summarise(Sum = last(Sum, order_by = Date), .groups="drop") %>%
    mutate(Return = Sum / lag(Sum) - 1) %>%
    drop_na()
  
  # 3) 팩터 데이터와 merge
  merged <- inner_join(dd_month, factors, by = "YM")
  
  # 4) 회귀 준비
  fit <- lm(Return ~ MKT + VALUE + GROWTH + MOM, data = merged)
  reg_summary <- summary(fit)   # ✅ [FIX] reg_summary 미정의 버그 수정
  
  cat("\n========================================\n")
  cat(" [리스크] 팩터별 요인 민감도(Factor Exposure) 분석\n")
  cat("========================================\n\n")
  
  s <- reg_summary
  print(coef(s))
  cat("\nR2:", s$r.squared, "AdjR2:", s$adj.r.squared, "\n")
  
  # 계수만 뽑기
  coef_df <- tidy(fit)
  
  # 5) 시각화
  p <- ggplot(coef_df %>% filter(term != "(Intercept)"), aes(x = term, y = estimate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "포트폴리오 요인 민감도(Factor Exposure)",
         x = "Factor", y = "민감도 (회귀계수)") +
    theme_minimal(base_size = 13)
  
  print(p)
  
  return(list(model = fit, summary = reg_summary, coef = coef_df))
}

###############################################################
# PCA 기반 리스크 분해 + 자동 해석 메시지 포함
###############################################################
run_pca_risk <- function(asset_returns, weights, scale. = TRUE) {
  suppressMessages(library(dplyr))
  
  asset_returns <- as.data.frame(asset_returns)
  
  if (ncol(asset_returns) != length(weights)) {
    stop("run_pca_risk: 자산 열 개수와 weights 길이가 같아야 합니다.")
  }
  
  # 비중 정규화
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  # NA 제거 후 공분산 계산
  ret_clean <- asset_returns[stats::complete.cases(asset_returns), ]
  cov_mat   <- stats::cov(ret_clean)
  
  # PCA 수행
  pca_res <- stats::prcomp(ret_clean, center = TRUE, scale. = scale.)
  eig     <- eigen(cov_mat)
  lambda  <- eig$values
  phi     <- eig$vectors
  
  # 포트폴리오 리스크 기여도 계산
  pc_contrib <- numeric(length(lambda))
  for (k in seq_along(lambda)) {
    loading_k     <- sum(weights * phi[, k])
    pc_contrib[k] <- lambda[k] * loading_k^2
  }
  pc_ratio <- pc_contrib / sum(pc_contrib)
  
  ####### ------------- 자동 해석 파트 ---------------- #######
  pc1_load <- phi[, 1]
  pc2_load <- phi[, 2]
  pc3_load <- phi[, 3]
  
  asset_names <- colnames(asset_returns)
  
  top_pc1 <- asset_names[order(abs(pc1_load), decreasing = TRUE)][1:min(4, length(asset_names))]
  top_pc2 <- asset_names[order(abs(pc2_load), decreasing = TRUE)][1:min(3, length(asset_names))]
  top_pc3 <- asset_names[order(abs(pc3_load), decreasing = TRUE)][1:min(3, length(asset_names))]
  
  pc1_ratio <- pc_ratio[1]
  pc2_ratio <- pc_ratio[2]
  pc3_ratio <- pc_ratio[3]
  
  cat("\n========================================\n")
  cat(" PCA 기반 리스크 자동 해석\n")
  cat("========================================\n")
  
  cat(sprintf("\n[1] PC1 요인이 전체 포트폴리오 변동성의 %.1f%%를 설명합니다.\n",
              pc1_ratio * 100))
  cat("    → PC1을 구성하는 주요 자산: ", paste(top_pc1, collapse=", "), "\n")
  
  if (all(grepl("SPY|SCHD|QQQ|TQQQ", top_pc1))) {
    cat("    → 해석: 미국 주식(대형주/성장/배당/나스닥) 공통 요인이 리스크의 핵심 원천입니다.\n")
  }
  
  cat(sprintf("\n[2] PC2 요인은 전체 변동성의 %.1f%%를 설명합니다.\n",
              pc2_ratio * 100))
  cat("    → PC2 구성 주요 자산: ", paste(top_pc2, collapse=", "), "\n")
  
  if (any(grepl("GLD", top_pc2)) && any(grepl("IEF", top_pc2))) {
    cat("    → 해석: 금(GLD)과 국채(IEF)의 방어적 요인입니다.\n")
    cat("      시장 급락 시 손실을 완충하는 역할을 하는 요인입니다.\n")
  }
  
  cat(sprintf("\n[3] PC3 요인은 전체 변동성의 %.1f%%를 설명합니다.\n",
              pc3_ratio * 100))
  cat("    → PC3 구성 주요 자산: ", paste(top_pc3, collapse=", "), "\n")
  
  if (("GLD" %in% top_pc3) && ("IEF" %in% top_pc3)) {
    cat("    → 해석: 금과 채권의 상대 가치 요인(인플레이션 vs 금리)입니다.\n")
  }
  
  cat("\n----------------------------------------\n")
  cat(" 기술적 출력 (PC별 설명력 / 로딩 매트릭스)\n")
  cat("----------------------------------------\n\n")
  
  print(summary(pca_res))
  
  pc_table <- data.frame(
    PC = paste0("PC", seq_along(lambda)),
    PortVar_Contribution = pc_ratio
  )
  print(pc_table)
  
  cat("\n[로딩 행렬]\n")
  print(pca_res$rotation)
  
  invisible(list(
    pca = pca_res,
    pc_ratio = pc_ratio,
    rotation = pca_res$rotation
  ))
}

###############################################################
# CSV 기반 PCA 대시보드  (덮어쓰기 제거 + Date 제거 보장 버전)
###############################################################
run_pca_dashboard_from_file <- function(asset_returns_file, weights, scale. = TRUE) {
  
  # 1) CSV 로드
  asset_df <- read.csv(asset_returns_file, stringsAsFactors = FALSE)
  
  # Date 컬럼 제거 (PCA 입력에 절대 포함되면 안 됨)
  if ("Date" %in% names(asset_df)) {
    asset_df <- asset_df[, names(asset_df) != "Date", drop = FALSE]
  }
  
  # 컬럼명 정리
  names(asset_df) <- trimws(names(asset_df))
  
  # weights 이름 체크
  if (is.null(names(weights)) || any(names(weights) == "")) {
    cat("[PCA] weights에 names가 없어 PCA를 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # ---------- [FIX] PCA용 이름 매핑 (weights -> returns 컬럼명) ----------
  name_map <- c(
    "SPY_ETC" = "SPY",   # returns 컬럼이 SPY인 경우
    "GOLD"    = "GLD"    # returns 컬럼이 GLD인 경우
  )
  weights_pca <- weights
  nm <- names(weights_pca)
  nm <- ifelse(nm %in% names(name_map), unname(name_map[nm]), nm)
  names(weights_pca) <- nm
  # ---------------------------------------------------------------
  
  # 공통 자산만 사용
  common_assets <- intersect(names(asset_df), names(weights_pca))
  if (length(common_assets) < 2) {
    cat("[PCA] 매칭되는 자산이 부족하여 PCA를 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # 공통자산으로 정렬 (열과 weights가 같은 순서가 되도록)
  asset_returns_pca <- asset_df[, common_assets, drop = FALSE]
  weights_use       <- weights_pca[common_assets]
  
  # 최종 방어
  stopifnot(ncol(asset_returns_pca) == length(weights_use))
  stopifnot(all(colnames(asset_returns_pca) == names(weights_use)))
  
  run_pca_risk(asset_returns_pca, weights_use, scale. = scale.)
}

###############################################################################
# (추가 모듈) 1단계 리스크 고도화:
#  - Stress Test Replay
#  - VaR / CVaR
#  - DRIFT 기반 동적 리밸런싱 신호
###############################################################################
suppressMessages({
  library(dplyr)
  library(readr)
})

###############################################################################
# 1) Stress Test Replay
###############################################################################
run_stress_replay_from_file <- function(
    asset_file      = "asset_returns_monthly.csv",
    weights,
    current_nav,
    monthly_contrib = 0,
    crisis_periods  = list(
      `2008_GFC`  = c("2007-10-01", "2009-03-01"),
      `2020_COVID`= c("2019-12-01", "2020-06-01")
    )
) {
  if (!file.exists(asset_file)) {
    cat("[리스크] Stress Test: 자산 수익률 파일을 찾을 수 없습니다. (", asset_file, ")\n")
    return(invisible(NULL))
  }
  
  rets <- read_csv(asset_file, show_col_types = FALSE)
  if (!"Date" %in% colnames(rets)) {
    stop("Stress Test: asset_returns_monthly.csv에 'Date' 컬럼이 필요합니다.")
  }
  rets$Date <- as.Date(rets$Date)
  
  asset_cols <- setdiff(colnames(rets), "Date")
  R_mat      <- as.matrix(rets[, asset_cols])
  
  if (length(weights) != ncol(R_mat)) {
    stop("Stress Test: weights 길이와 자산 열 개수가 다릅니다.")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  .calc_mdd_from_path <- function(nav_vec) {
    peak <- cummax(nav_vec)
    dd   <- nav_vec / peak - 1
    list(
      mdd_value = min(dd, na.rm = TRUE),
      mdd_start = which.max(peak[1:which.min(dd)]),
      mdd_end   = which.min(dd)
    )
  }
  
  cat("\n[리스크] Stress Test Replay 실행...\n")
  cat("========================================\n")
  cat(" 과거 위기 구간을 현재 포트 비중으로 리플레이합니다.\n")
  cat("  - 파일: ", asset_file, "\n")
  cat("  - 현재 기준 평가금: ", format(round(current_nav), big.mark = ","), "원\n")
  cat("========================================\n\n")
  
  for (nm in names(crisis_periods)) {
    rng <- crisis_periods[[nm]]
    s_date <- as.Date(rng[1])
    e_date <- as.Date(rng[2])
    
    sub <- rets %>%
      filter(Date >= s_date & Date <= e_date) %>%
      arrange(Date)
    
    if (nrow(sub) == 0) {
      cat("[경고] ", nm, " 구간(Date: ", format(s_date), " ~ ", format(e_date), ") 데이터가 없습니다.\n\n")
      next
    }
    
    R_sub <- as.matrix(sub[, asset_cols])
    port_ret <- as.numeric(R_sub %*% weights)  # 월간 포트 수익률
    
    nav <- numeric(length(port_ret))
    nav[1] <- current_nav * (1 + port_ret[1]) + monthly_contrib
    if (length(port_ret) > 1) {
      for (i in 2:length(port_ret)) {
        nav[i] <- nav[i-1] * (1 + port_ret[i]) + monthly_contrib
      }
    }
    
    mdd_info <- .calc_mdd_from_path(nav)
    mdd_pct  <- mdd_info$mdd_value * 100
    
    cat("● 시나리오:", nm, "\n")
    cat("   기간 :", format(min(sub$Date)), "~", format(max(sub$Date)), " (", nrow(sub), "개 월 수익률)\n")
    cat("   최종 평가금:", format(round(tail(nav, 1)), big.mark = ","), "원\n")
    cat("   최대낙폭(MDD): ", sprintf("%.2f%%", mdd_pct), "\n")
    cat("   위기 구간 동안 수익률 분포 (요약):\n")
    print(summary(port_ret))
    cat("----------------------------------------\n\n")
  }
  
  invisible(NULL)
}

###############################################################################
# 2) VaR / CVaR 계산
###############################################################################
run_var_cvar_from_file <- function(
    asset_file  = "asset_returns_monthly.csv",
    weights,
    current_nav,
    alpha       = 0.95
) {
  if (!file.exists(asset_file)) {
    cat("[리스크] VaR/CVaR: 자산 수익률 파일을 찾을 수 없습니다. (", asset_file, ")\n")
    return(invisible(NULL))
  }
  
  rets <- read_csv(asset_file, show_col_types = FALSE)
  if (!"Date" %in% colnames(rets)) {
    stop("VaR/CVaR: asset_returns_monthly.csv에 'Date' 컬럼이 필요합니다.")
  }
  rets$Date <- as.Date(rets$Date)
  
  asset_cols <- setdiff(colnames(rets), "Date")
  R_mat      <- as.matrix(rets[, asset_cols])
  
  if (length(weights) != ncol(R_mat)) {
    stop("VaR/CVaR: weights 길이와 자산 열 개수가 다릅니다.")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  port_ret <- as.numeric(R_mat %*% weights)   # 월간 포트 수익률
  port_ret <- port_ret[is.finite(port_ret)]
  
  if (length(port_ret) < 20) {
    cat("[리스크] VaR/CVaR: 수익률 표본이 너무 적습니다. (", length(port_ret), ")\n")
    return(invisible(NULL))
  }
  
  q <- stats::quantile(port_ret, probs = 1 - alpha, na.rm = TRUE) # 하위 tail
  var_pct  <- -as.numeric(q)
  cvar_pct <- -mean(port_ret[port_ret <= q], na.rm = TRUE)
  
  var_amt  <- current_nav * var_pct
  cvar_amt <- current_nav * cvar_pct
  
  cat("\n[리스크] VaR / CVaR 계산 (월간 기준 수익률)\n")
  cat("========================================\n")
  cat(" 신뢰수준(α) :", alpha * 100, "%\n")
  cat(" 표본 개수   :", length(port_ret), "개월\n")
  cat("----------------------------------------\n")
  cat(" VaR  (", alpha * 100, "%)  : 약 ",
      sprintf("%.2f%%", var_pct * 100), " (",
      format(round(var_amt), big.mark = ","), "원 손실 가능)\n", sep = "")
  cat(" CVaR(", alpha * 100, "%)  : 약 ",
      sprintf("%.2f%%", cvar_pct * 100), " (",
      format(round(cvar_amt), big.mark = ","), "원 평균 손실)\n", sep = "")
  cat("========================================\n\n")
  
  invisible(list(
    var_pct  = var_pct,
    cvar_pct = cvar_pct,
    var_amt  = var_amt,
    cvar_amt = cvar_amt
  ))
}

###############################################################################
# 3) DRIFT 기반 동적 리밸런싱 신호
###############################################################################
run_drift_rebal_signal <- function(
    target_weights,
    current_weights,
    threshold = 0.05
) {
  if (length(target_weights) != length(current_weights)) {
    stop("DRIFT: target_weights와 current_weights의 길이가 다릅니다.")
  }
  
  if (!is.null(names(target_weights)) && !is.null(names(current_weights))) {
    all_names <- union(names(target_weights), names(current_weights))
    target_weights  <- target_weights[all_names]
    current_weights <- current_weights[all_names]
  }
  
  target_weights[target_weights < 0]   <- 0
  current_weights[current_weights < 0] <- 0
  
  if (sum(target_weights) <= 0 || sum(current_weights) <= 0) {
    stop("DRIFT: 비중 합이 0 이하입니다.")
  }
  
  target_norm  <- target_weights / sum(target_weights)
  current_norm <- current_weights / sum(current_weights)
  
  diff <- current_norm - target_norm  # +: 목표보다 초과, -: 부족
  df <- data.frame(
    Asset          = names(target_norm),
    Target_Weight  = round(target_norm * 100, 2),
    Current_Weight = round(current_norm * 100, 2),
    Drift_pctpt    = round(diff * 100, 2)
  )
  
  cat("\n[리스크] DRIFT 기반 리밸런싱 신호\n")
  cat("========================================\n")
  cat(" (양수: 목표보다 비중 과다 → 매도 후보)\n")
  cat(" (음수: 목표보다 비중 부족 → 매수 후보)\n")
  cat("----------------------------------------\n")
  print(df, row.names = FALSE)
  cat("----------------------------------------\n")
  
  idx <- which(abs(diff) >= threshold)
  if (length(idx) == 0) {
    cat("※ 모든 자산의 드리프트가 ±", threshold * 100,
        "%p 이내입니다. 당장 리밸런싱 필요 신호는 없습니다.\n\n", sep = "")
    return(invisible(df))
  }
  
  cat("※ 리밸런싱 후보 (|Drift| >=", threshold * 100, "%p 이상):\n", sep = "")
  for (i in idx) {
    nm    <- names(diff)[i]
    d_val <- diff[i] * 100
    if (d_val > 0) {
      cat(" -", nm, ": 목표보다 약 +", sprintf("%.2f", d_val),
          "%p 초과 → 일부 매도하여 다른 자산으로 이동 고려\n")
    } else {
      cat(" -", nm, ": 목표보다 약 ", sprintf("%.2f", d_val),
          "%p 부족 → 여유 자금/타 자산 매도로 비중 확대 고려\n")
    }
  }
  cat("========================================\n\n")
  
  invisible(df)
}

# ============================================================
#  GARCH 기반 변동성 위험 경보(Alert) 시스템
# ============================================================
run_garch_vol_alert <- function(dd, min_obs = 250,
                                normal_thr = 1.2,
                                alert_thr  = 1.8) {
  cat("\n[리스크] GARCH 기반 변동성 위험 경보(Alert) 실행...\n")
  cat(strrep("=", 80), "\n")
  
  if (!.has_rugarch) {
    cat("[경고] 'rugarch' 패키지가 설치되어 있지 않아 GARCH 경보를 건너뜁니다.\n")
    cat("       install.packages('rugarch') 후 다시 실행하세요.\n")
    return(invisible(NULL))
  }
  
  # 1) 수익률 벡터 확보 ----------------------------------------
  if (!("Return_TWR" %in% names(dd))) {
    cat("[경고] dd에 'Return_TWR' 컬럼이 없어 GARCH 분석을 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  ret <- as.numeric(dd$Return_TWR)
  ret <- ret[is.finite(ret)]
  
  if (length(ret) < min_obs) {
    cat("[경고] GARCH 적합을 위한 최소 관측치(", min_obs,
        "개) 보다 적어 분석을 건너뜁니다. (현재:", length(ret), "개)\n")
    return(invisible(NULL))
  }
  
  # 2) GARCH(1,1) 모형 정의 및 적합 ----------------------------
  rugarch <- asNamespace("rugarch")
  
  spec <- rugarch$ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit <- try(
    rugarch$ugarchfit(spec, data = ret, solver = "hybrid"),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    cat("[경고] GARCH 적합 중 오류가 발생하여 경보 계산을 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # 3) 내일(1-step ahead) 변동성 예측 ---------------------------
  fore <- rugarch$ugarchforecast(fit, n.ahead = 1)
  sigma_forecast <- as.numeric(rugarch$sigma(fore))[1]
  sigma_hist     <- stats::sd(ret, na.rm = TRUE)
  
  ratio <- sigma_forecast / sigma_hist
  
  # 4) 경보 레벨 분류 ------------------------------------------
  level <- if (ratio <= normal_thr) {
    "NORMAL"
  } else if (ratio <= alert_thr) {
    "ALERT"
  } else {
    "CRITICAL"
  }
  
  # 5) 해석용 메시지 생성 --------------------------------------
  msg <- switch(
    level,
    "NORMAL" = paste0(
      "🟢 상태: NORMAL\n",
      "   - 예측 변동성이 평소 수준 대비 크게 높지 않습니다.\n",
      "   - 기존 전략(적립, 리밸런싱, TQQQ 비중) 그대로 유지해도 무방한 상태입니다.\n"
    ),
    "ALERT" = paste0(
      "🟡 상태: ALERT\n",
      "   - 예측 변동성이 장기 평균보다 눈에 띄게 높은 구간입니다.\n",
      "   - 행동 가이드:\n",
      "     · TQQQ 비중 추가 확대는 신중히 검토\n",
      "     · 새로운 레버리지·개별주 공격 매수는 자제\n",
      "     · 예정된 적립식은 그대로 진행하되, 리밸런싱 시 방어자산(GLD/채권) 우선 고려\n"
    ),
    "CRITICAL" = paste0(
      "🔴 상태: CRITICAL\n",
      "   - 예측 변동성이 평소 대비 매우 높은 수준입니다.\n",
      "   - 과거 위기(코로나, 금융위기 등)에서 관측된 수준에 가까울 수 있습니다.\n",
      "   - 행동 가이드:\n",
      "     · TQQQ·고변동 자산 비중 축소 또는 신규매수 일시 중단 검토\n",
      "     · 현금·채권·금 비중을 단기적으로 늘리는 완충 전략 가능\n",
      "     · 감정적 매매(공포/욕심)에 휘둘리지 않도록, 시스템 규칙을 우선\n"
    )
  )
  
  # 6) 결과 출력 -----------------------------------------------
  cat("■ GARCH(1,1) 기반 변동성 요약\n")
  cat("   - 역사적(장기) 일간 변동성(σ_hist): ",
      sprintf("%.4f", sigma_hist), " (약 ", sprintf("%.2f", sigma_hist * 100), "%)\n", sep = "")
  cat("   - 내일 예측 변동성(σ_forecast):     ",
      sprintf("%.4f", sigma_forecast), " (약 ", sprintf("%.2f", sigma_forecast * 100), "%)\n", sep = "")
  cat("   - 예측/역사적 변동성 비율:         ",
      sprintf("%.2f", ratio), "배\n", sep = "")
  cat("   - 경보 레벨:                        ", level, "\n\n", sep = "")
  
  cat(msg, "\n")
  cat(strrep("=", 80), "\n\n")
  
  invisible(
    list(
      level          = level,
      sigma_hist     = sigma_hist,
      sigma_forecast = sigma_forecast,
      ratio          = ratio
    )
  )
}

###############################################################################
# risk_module.R 끝
###############################################################################
=======
###############################################################################
# risk_module.R  (리스크 분석 모듈 모음)
###############################################################################

suppressMessages(library(dplyr))

# ---- GARCH용 패키지 체크 ------------------------------------
.has_rugarch <- requireNamespace("rugarch", quietly = TRUE)

###############################################################################
# 0) 파일 기반 팩터 회귀 (Date 키 고정 버전)
###############################################################################
run_factor_model_from_files <- function(asset_returns_file, factors_file, weights) {
  
  asset_df  <- readr::read_csv(asset_returns_file,  show_col_types = FALSE)
  factor_df <- readr::read_csv(factors_file,        show_col_types = FALSE)
  
  # --- 키는 Date로 고정 (YM은 절대 쓰지 않음) ---
  if (!("Date" %in% names(asset_df)))  stop("asset_returns_file에 Date 컬럼이 없습니다.")
  if (!("Date" %in% names(factor_df))) stop("factors_file에 Date 컬럼이 없습니다.")
  
  # --- 포트수익률(Y) 생성: asset returns × weights ---
  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop("weights는 names가 있어야 합니다. 예: c(SPY=0.4, SCHD=0.2, ...)")
  }
  
  asset_cols <- setdiff(names(asset_df), "Date")
  common_assets <- intersect(asset_cols, names(weights))
  if (length(common_assets) < 2) stop("asset_returns 자산명과 weights names 매칭이 부족합니다.")
  
  w <- weights[common_assets]
  w <- w / sum(w)
  
  port_ret <- as.numeric(as.matrix(asset_df[, common_assets]) %*% as.numeric(w))
  port_df  <- data.frame(Date = asset_df$Date, port = port_ret)
  
  # --- 팩터(X): YM은 버리고, MKT/VALUE/GROWTH/MOM만 사용 ---
  need <- c("MKT","VALUE","GROWTH","MOM")
  miss <- setdiff(need, names(factor_df))
  if (length(miss) > 0) stop(paste("factors_file에 컬럼이 없습니다:", paste(miss, collapse=", ")))
  
  fact_df2 <- factor_df %>% dplyr::select(Date, dplyr::all_of(need))
  
  # --- Date로 병합 ---
  dat <- dplyr::inner_join(port_df, fact_df2, by = "Date") %>% stats::na.omit()
  if (nrow(dat) < 24) stop("팩터회귀 표본이 너무 적습니다(24개월 미만).")
  
  fit <- lm(port ~ MKT + VALUE + GROWTH + MOM, data = dat)
  
  cat("========================================\n")
  cat(" 팩터 회귀모형 결과\n")
  cat(" 포트 수익률 ~ MKT + VALUE + GROWTH + MOM\n")
  cat(" (표본:", nrow(dat), "개월)\n")
  cat("========================================\n\n")
  print(summary(fit))
  
  invisible(fit)
}

###############################################################################
# 0. dd로부터 "현금흐름 보정 일별 운용수익률" 계산  (방탄/Date 정렬 유지)
###############################################################################
# dd: 최소한 아래 컬럼을 가져야 함
#   - Date  : 날짜 (Date 형)
#   - Sum   : 전체 평가금액
#   - Profit: 총수익금 (Sum - 투자원금)
#
# 개념:
#   Invested_t  = Sum_t - Profit_t            (누적 투자원금)
#   Flow_t      = Invested_t - Invested_{t-1} (t일에 외부에서 새로 들어온/나간 돈)
#   Sum_t       = (Sum_{t-1} + Flow_t) * (1 + r_t)
#   ⇒ r_t       = Sum_t / (Sum_{t-1} + Flow_t) - 1
#
# [중요 수정]
# - 기존 버전처럼 NA 제거/클리핑으로 "벡터 길이를 줄여 반환"하면
#   메인에서 dd와 Return 매칭이 깨질 수 있습니다.
# - 따라서 Date별 Return을 NA 포함으로 유지하여 data.frame으로 반환합니다.
###############################################################################
compute_daily_returns_from_dd <- function(dd, ret_clip = 0.5) {
  dd <- dd %>% arrange(Date)
  
  if (!all(c("Date","Sum","Profit") %in% colnames(dd))) {
    stop("compute_daily_returns_from_dd: dd에 'Date','Sum','Profit' 컬럼이 필요합니다.")
  }
  
  dd2 <- dd %>%
    mutate(
      Invested      = Sum - Profit,             # 누적 투자원금
      Invested_lag  = dplyr::lag(Invested),
      Sum_lag       = dplyr::lag(Sum),
      Flow          = Invested - Invested_lag,  # t일에 새로 들어온 순 현금
      Gross_base    = Sum_lag + Flow,           # 운용 대상 자산
      Return        = dplyr::if_else(
        !is.na(Gross_base) & Gross_base > 0,
        Sum / Gross_base - 1,
        NA_real_
      )
    )
  
  # ±50% 이상은 데이터 오류/극단값 가능성이 커서 제거(원하면 주석 처리 가능)
  dd2 <- dd2 %>%
    mutate(Return = dplyr::if_else(!is.na(Return) & abs(Return) < ret_clip, Return, NA_real_))
  
  # ✅ Date별 Return 유지 (NA 포함)
  dd2 %>% dplyr::select(Date, Return)
}

###############################################################################
# 1. 포트폴리오 μ, σ 추정 (현금흐름 보정 버전)
###############################################################################
# dd: Date, Sum, Profit 포함한 데이터프레임
#
# 출력:
#   - mu_daily, sigma_daily   : 일간 기대수익률, 변동성
#   - mu_annual, sigma_annual : 연환산 기대수익률, 변동성 (252 영업일 가정)
###############################################################################
estimate_mu_sigma_from_dd <- function(dd) {
  dd <- dd %>% arrange(Date)
  
  if (nrow(dd) < 2) {
    stop("estimate_mu_sigma_from_dd: dd에 최소 2개 이상의 행이 필요합니다.")
  }
  
  ret_df <- compute_daily_returns_from_dd(dd)
  r_daily <- ret_df$Return
  r_daily <- r_daily[is.finite(r_daily)]
  
  if (length(r_daily) == 0) {
    stop("estimate_mu_sigma_from_dd: 유효한 일별 수익률이 없습니다.")
  }
  if (length(r_daily) < 10) {
    warning("estimate_mu_sigma_from_dd: 유효한 일별 수익률이 10개 미만입니다. 추정치 신뢰도가 낮을 수 있습니다.")
  }
  
  mu_daily    <- mean(r_daily, na.rm = TRUE)
  sigma_daily <- sd(r_daily,   na.rm = TRUE)
  
  mu_annual    <- mu_daily * 252
  sigma_annual <- sigma_daily * sqrt(252)
  
  list(
    mu_daily     = mu_daily,
    sigma_daily  = sigma_daily,
    mu_annual    = mu_annual,
    sigma_annual = sigma_annual
  )
}

###############################################################################
# 2. 적립식 몬테카를로 (현역기 - accumulation phase)
###############################################################################
# 입력:
#   dd              : Date, Sum, Profit 포함
#   years           : 시뮬레이션 기간(년)
#   monthly_contrib : 매월 적립금(원)
#   n_sims          : 시뮬레이션 횟수
###############################################################################
run_mc_from_dd <- function(dd,
                           years = 10,
                           monthly_contrib = 5000000,
                           n_sims = 5000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)        # 현재 평가액
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12   # 월 단위
  sim_terminal <- numeric(n_sims)
  set.seed(123)
  
  for (i in seq_len(n_sims)) {
    V <- V0
    for (m in 1:(years * 12)) {
      V <- V + monthly_contrib   # 매월 적립
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V <- V * exp(r)
    }
    sim_terminal[i] <- V
  }
  
  qs       <- quantile(sim_terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(sim_terminal)
  
  cat("========================================\n")
  cat(" 몬테카를로 기반", years, " 년 후 평가액 분포\n")
  cat("  (", n_sims, "회 시뮬레이션, 월 적립",
      format(monthly_contrib, big.mark = ","), "원)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수 (나쁜 장세) :",
      format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 50% 분위수 (중앙값)   :",
      format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 90% 분위수 (좋은 장세) :",
      format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균값(Mean)          :",
      format(round(mean_val, 0), big.mark = ","), "원\n")
  cat(" 연 기대수익률(추정)   :",
      round(mu * 100, 2), "%,  연 변동성(추정) :", round(sigma * 100, 2), "%\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = sim_terminal,
    summary = list(
      q10          = as.numeric(qs[1]),
      q50          = as.numeric(qs[2]),
      q90          = as.numeric(qs[3]),
      mean         = mean_val,
      mu_annual    = mu,
      sigma_annual = sigma
    )
  ))
}

###############################################################################
# 3. 미래 최대낙폭(MDD) 분포 시뮬레이션
###############################################################################
run_future_mdd_from_dd <- function(dd,
                                   years = 10,
                                   monthly_contrib = 5000000,
                                   n_sims = 2000) {
  est <- estimate_mu_sigma_from_dd(dd)
  
  V0    <- tail(dd$Sum, 1)
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  dt_year <- 1 / 12
  m_per_year <- 12
  n_steps <- years * m_per_year
  
  mdd_vec <- numeric(n_sims)
  set.seed(456)
  
  for (i in seq_len(n_sims)) {
    V <- numeric(n_steps + 1)
    V[1] <- V0
    peak <- V0
    dd_path <- numeric(n_steps + 1)
    dd_path[1] <- 0
    
    for (t in 1:n_steps) {
      # 적립
      V[t] <- V[t] + monthly_contrib
      # 수익률
      z <- rnorm(1)
      r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
      V[t + 1] <- V[t] * exp(r)
      
      # drawdown 갱신
      if (V[t + 1] > peak) {
        peak <- V[t + 1]
      }
      dd_path[t + 1] <- (V[t + 1] / peak) - 1
    }
    
    mdd_vec[i] <- min(dd_path, na.rm = TRUE)  # 가장 낮은 DD
  }
  
  mdd_percent <- mdd_vec * 100
  qs <- quantile(mdd_percent, c(0.1, 0.5, 0.9))
  
  cat("========================================\n")
  cat(" 미래", years, " 년 동안 최대낙폭(MDD) 분포 (적립 포함)\n")
  cat("  (", n_sims, "경로 시뮬레이션)\n", sep = "")
  cat("----------------------------------------\n")
  cat(" 10% 분위수(하위 10%: 비교적 크게 빠지는 경우)   :",
      round(qs[1], 2), "%\n")
  cat(" 50% 분위수(중앙값: 보통 한 번쯤 겪을 만한 MDD)  :",
      round(qs[2], 2), "%\n")
  cat(" 90% 분위수(상위 10%: 상대적으로 덜 빠지는 경우) :",
      round(qs[3], 2), "%\n")
  cat("----------------------------------------\n")
  cat(" * 음수 값이므로 -30% 라는 뜻은 피크 대비 30% 하락을 의미\n")
  cat("========================================\n\n")
  
  invisible(list(
    mdd_raw      = mdd_vec,
    mdd_percent  = mdd_percent,
    summary      = list(q10 = qs[1], q50 = qs[2], q90 = qs[3]),
    mu_annual    = mu,
    sigma_annual = sigma
  ))
}

###############################################################################
# 4. 은퇴 후 인출 시나리오 Monte Carlo (decumulation phase)
###############################################################################
run_mc_withdraw_from_dd <- function(dd,
                                    years = 30,
                                    annual_withdraw = 200000000,
                                    n_sims = 5000,
                                    withdraw_freq = c("annual", "monthly"),
                                    initial_value = NULL) {
  withdraw_freq <- match.arg(withdraw_freq)
  
  est <- estimate_mu_sigma_from_dd(dd)
  
  # 시작자산: initial_value가 지정되면 그 값 사용, 아니면 dd의 마지막 Sum
  V0 <- if (is.null(initial_value)) {
    tail(dd$Sum, 1)
  } else {
    initial_value
  }
  
  mu    <- est$mu_annual
  sigma <- est$sigma_annual
  
  set.seed(789)
  
  ruin_flag <- logical(n_sims)
  terminal  <- numeric(n_sims)
  
  if (withdraw_freq == "annual") {
    dt_year <- 1
    n_steps <- years
    withdraw_step <- annual_withdraw
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1년 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  } else if (withdraw_freq == "monthly") {
    dt_year <- 1 / 12
    n_steps <- years * 12
    withdraw_step <- annual_withdraw / 12
    for (i in seq_len(n_sims)) {
      V <- V0
      ruined <- FALSE
      for (t in 1:n_steps) {
        # 1개월 수익
        z <- rnorm(1)
        r <- (mu - 0.5 * sigma^2) * dt_year + sigma * sqrt(dt_year) * z
        V <- V * exp(r)
        # 인출
        V <- V - withdraw_step
        if (V <= 0) {
          ruined <- TRUE
          V <- 0
          break
        }
      }
      ruin_flag[i] <- ruined
      terminal[i]  <- V
    }
  }
  
  ruin_prob <- mean(ruin_flag)
  qs <- quantile(terminal, c(0.1, 0.5, 0.9))
  mean_val <- mean(terminal)
  
  cat("========================================\n")
  cat(" 은퇴 후 인출 시나리오 Monte Carlo\n")
  cat("  기간 :", years, "년,  연 인출액:",
      format(annual_withdraw, big.mark = ","), "원\n", sep = "")
  cat("  시작자산 :", format(round(V0, 0), big.mark = ","), "원\n")
  cat("  가정: 연 기대수익률", round(mu * 100, 2),
      "%, 연 변동성", round(sigma * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 파산 확률 (자산이 0 이하로 떨어질 확률) :", round(ruin_prob * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat(" 기간 종료 시 잔고 10% 분위수 :", format(round(qs[1], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 50% 분위수 :", format(round(qs[2], 0), big.mark = ","), "원\n")
  cat(" 기간 종료 시 잔고 90% 분위수 :", format(round(qs[3], 0), big.mark = ","), "원\n")
  cat(" 평균 잔고(Mean)              :", format(round(mean_val, 0), big.mark = ","), "원\n")
  cat("========================================\n\n")
  
  invisible(list(
    terminal_values = terminal,
    ruin_flag       = ruin_flag,
    ruin_prob       = ruin_prob,
    summary         = list(
      q10  = as.numeric(qs[1]),
      q50  = as.numeric(qs[2]),
      q90  = as.numeric(qs[3]),
      mean = mean_val
    ),
    mu_annual       = mu,
    sigma_annual    = sigma
  ))
}

###############################################################################
# 5. 팩터별 분석 (Factor Regression) - 방탄 버전
###############################################################################
run_factor_model <- function(port_ret, factors_df) {
  
  # ---------------------------
  # ✅ 방탄 1) factors_df 비정상 입력 방어
  # ---------------------------
  if (is.null(port_ret) || length(port_ret) == 0) {
    stop("run_factor_model: port_ret 길이가 0입니다.")
  }
  if (is.null(factors_df) || nrow(factors_df) == 0) {
    stop("run_factor_model: factors_df가 비어있습니다.")
  }
  
  # ---------------------------
  # ✅ 방탄 2) 날짜/키 컬럼 제거 (회귀 X에 들어가면 망함)
  # ---------------------------
  drop_cols <- intersect(names(factors_df), c("YM", "Ym", "ym", "DATE", "Date", "date"))
  if (length(drop_cols) > 0) {
    factors_df <- factors_df[, setdiff(names(factors_df), drop_cols), drop = FALSE]
  }
  
  # ---------------------------
  # ✅ 방탄 3) 숫자형 컬럼만 남기기
  # ---------------------------
  is_num <- vapply(factors_df, function(x) is.numeric(x) || is.integer(x), logical(1))
  factors_df <- factors_df[, is_num, drop = FALSE]
  
  if (ncol(factors_df) == 0) {
    stop("run_factor_model: 회귀에 사용할 numeric 팩터 컬럼이 0개입니다. (YM/Date만 있었던 케이스)")
  }
  
  # ---------------------------
  # ✅ 방탄 4) 길이 일치 / NA 정리
  # ---------------------------
  if (length(port_ret) != nrow(factors_df)) {
    stop(sprintf("run_factor_model: 길이 불일치 port_ret=%d, factors_df=%d",
                 length(port_ret), nrow(factors_df)))
  }
  
  dat <- data.frame(port = port_ret, factors_df)
  dat <- na.omit(dat)
  
  if (nrow(dat) < 12) {
    stop("run_factor_model: 유효표본이 너무 적습니다(NA 제거 후 12개 미만).")
  }
  
  form <- as.formula(paste("port ~", paste(colnames(dat)[-1], collapse = " + ")))
  fit <- lm(form, data = dat)
  
  cat("========================================\n")
  cat(" 팩터 회귀모형 결과\n")
  cat(" 포트 수익률 ~", paste(colnames(dat)[-1], collapse = " + "), "\n")
  cat("========================================\n\n")
  print(summary(fit))
  
  invisible(fit)
}

###############################################################################
# 6. quantmod 기반 월간 자산수익률 + 팩터 CSV 업데이트
###############################################################################
update_factor_data <- function(symbols = c("SPY","SCHD","QQQ","TQQQ","GLD","IEF"),
                               start_date = "2010-01-01",
                               save_path = getwd()) {
  
  suppressMessages(library(quantmod))
  suppressMessages(library(dplyr))
  suppressMessages(library(readr))
  suppressMessages(library(tidyr))
  
  setwd(save_path)
  
  # 1) 최신 데이터 다운로드
  getSymbols(symbols, from = start_date, auto.assign = TRUE)
  
  # 2) 월간 자산 수익률 계산
  monthly_list <- lapply(symbols, function(sym) {
    px <- Ad(get(sym))
    ret <- monthlyReturn(px, type = "arithmetic")
    colnames(ret) <- sym
    ret
  })
  
  all_monthly_xts <- do.call(merge, monthly_list)
  asset_returns <- data.frame(Date = as.Date(index(all_monthly_xts)),
                              coredata(all_monthly_xts)) %>%
    drop_na()
  
  write_csv(asset_returns, "asset_returns_monthly.csv")
  
  # 3) 팩터 계산
  factors <- asset_returns %>%
    mutate(
      MKT    = SPY,
      VALUE  = SCHD - SPY,
      GROWTH = QQQ  - SPY,
      MOM    = TQQQ - QQQ,
      YM     = format(Date, "%Y-%m")
    ) %>%
    select(Date, YM, MKT, VALUE, GROWTH, MOM)
  
  write_csv(factors, "factors_monthly.csv")
  
  cat("[팩터/자산수익률 데이터 자동 업데이트 완료]\n")
}

###############################################################
# (1) CSV 불러와서 팩터 회귀 & 요약 출력하는 함수
###############################################################
run_factor_dashboard_from_file <- function(dd, factor_file = "factors_monthly.csv") {
  if (!file.exists(factor_file)) {
    cat("[리스크] 팩터 파일(", factor_file, ")을 찾을 수 없어 분석을 건너뜁니다.\n")
    return(NULL)
  }
  
  suppressMessages(library(dplyr))
  suppressMessages(library(readr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(broom))
  
  # 1) 팩터 데이터 읽기
  factors <- read_csv(factor_file, show_col_types = FALSE)
  
  # 2) dd(포트폴리오 평가금)에서 월간 수익률 추출
  dd_month <- dd %>%
    mutate(YM = format(Date, "%Y-%m")) %>%
    group_by(YM) %>%
    summarise(Sum = last(Sum, order_by = Date), .groups="drop") %>%
    mutate(Return = Sum / lag(Sum) - 1) %>%
    drop_na()
  
  # 3) 팩터 데이터와 merge
  merged <- inner_join(dd_month, factors, by = "YM")
  
  # 4) 회귀 준비
  fit <- lm(Return ~ MKT + VALUE + GROWTH + MOM, data = merged)
  reg_summary <- summary(fit)   # ✅ [FIX] reg_summary 미정의 버그 수정
  
  cat("\n========================================\n")
  cat(" [리스크] 팩터별 요인 민감도(Factor Exposure) 분석\n")
  cat("========================================\n\n")
  
  s <- reg_summary
  print(coef(s))
  cat("\nR2:", s$r.squared, "AdjR2:", s$adj.r.squared, "\n")
  
  # 계수만 뽑기
  coef_df <- tidy(fit)
  
  # 5) 시각화
  p <- ggplot(coef_df %>% filter(term != "(Intercept)"), aes(x = term, y = estimate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "포트폴리오 요인 민감도(Factor Exposure)",
         x = "Factor", y = "민감도 (회귀계수)") +
    theme_minimal(base_size = 13)
  
  print(p)
  
  return(list(model = fit, summary = reg_summary, coef = coef_df))
}

###############################################################
# PCA 기반 리스크 분해 + 자동 해석 메시지 포함
###############################################################
run_pca_risk <- function(asset_returns, weights, scale. = TRUE) {
  suppressMessages(library(dplyr))
  
  asset_returns <- as.data.frame(asset_returns)
  
  if (ncol(asset_returns) != length(weights)) {
    stop("run_pca_risk: 자산 열 개수와 weights 길이가 같아야 합니다.")
  }
  
  # 비중 정규화
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  # NA 제거 후 공분산 계산
  ret_clean <- asset_returns[stats::complete.cases(asset_returns), ]
  cov_mat   <- stats::cov(ret_clean)
  
  # PCA 수행
  pca_res <- stats::prcomp(ret_clean, center = TRUE, scale. = scale.)
  eig     <- eigen(cov_mat)
  lambda  <- eig$values
  phi     <- eig$vectors
  
  # 포트폴리오 리스크 기여도 계산
  pc_contrib <- numeric(length(lambda))
  for (k in seq_along(lambda)) {
    loading_k     <- sum(weights * phi[, k])
    pc_contrib[k] <- lambda[k] * loading_k^2
  }
  pc_ratio <- pc_contrib / sum(pc_contrib)
  
  ####### ------------- 자동 해석 파트 ---------------- #######
  pc1_load <- phi[, 1]
  pc2_load <- phi[, 2]
  pc3_load <- phi[, 3]
  
  asset_names <- colnames(asset_returns)
  
  top_pc1 <- asset_names[order(abs(pc1_load), decreasing = TRUE)][1:min(4, length(asset_names))]
  top_pc2 <- asset_names[order(abs(pc2_load), decreasing = TRUE)][1:min(3, length(asset_names))]
  top_pc3 <- asset_names[order(abs(pc3_load), decreasing = TRUE)][1:min(3, length(asset_names))]
  
  pc1_ratio <- pc_ratio[1]
  pc2_ratio <- pc_ratio[2]
  pc3_ratio <- pc_ratio[3]
  
  cat("\n========================================\n")
  cat(" PCA 기반 리스크 자동 해석\n")
  cat("========================================\n")
  
  cat(sprintf("\n[1] PC1 요인이 전체 포트폴리오 변동성의 %.1f%%를 설명합니다.\n",
              pc1_ratio * 100))
  cat("    → PC1을 구성하는 주요 자산: ", paste(top_pc1, collapse=", "), "\n")
  
  if (all(grepl("SPY|SCHD|QQQ|TQQQ", top_pc1))) {
    cat("    → 해석: 미국 주식(대형주/성장/배당/나스닥) 공통 요인이 리스크의 핵심 원천입니다.\n")
  }
  
  cat(sprintf("\n[2] PC2 요인은 전체 변동성의 %.1f%%를 설명합니다.\n",
              pc2_ratio * 100))
  cat("    → PC2 구성 주요 자산: ", paste(top_pc2, collapse=", "), "\n")
  
  if (any(grepl("GLD", top_pc2)) && any(grepl("IEF", top_pc2))) {
    cat("    → 해석: 금(GLD)과 국채(IEF)의 방어적 요인입니다.\n")
    cat("      시장 급락 시 손실을 완충하는 역할을 하는 요인입니다.\n")
  }
  
  cat(sprintf("\n[3] PC3 요인은 전체 변동성의 %.1f%%를 설명합니다.\n",
              pc3_ratio * 100))
  cat("    → PC3 구성 주요 자산: ", paste(top_pc3, collapse=", "), "\n")
  
  if (("GLD" %in% top_pc3) && ("IEF" %in% top_pc3)) {
    cat("    → 해석: 금과 채권의 상대 가치 요인(인플레이션 vs 금리)입니다.\n")
  }
  
  cat("\n----------------------------------------\n")
  cat(" 기술적 출력 (PC별 설명력 / 로딩 매트릭스)\n")
  cat("----------------------------------------\n\n")
  
  print(summary(pca_res))
  
  pc_table <- data.frame(
    PC = paste0("PC", seq_along(lambda)),
    PortVar_Contribution = pc_ratio
  )
  print(pc_table)
  
  cat("\n[로딩 행렬]\n")
  print(pca_res$rotation)
  
  invisible(list(
    pca = pca_res,
    pc_ratio = pc_ratio,
    rotation = pca_res$rotation
  ))
}

###############################################################
# CSV 기반 PCA 대시보드  (덮어쓰기 제거 + Date 제거 보장 버전)
###############################################################
run_pca_dashboard_from_file <- function(asset_returns_file, weights, scale. = TRUE) {
  
  # 1) CSV 로드
  asset_df <- read.csv(asset_returns_file, stringsAsFactors = FALSE)
  
  # Date 컬럼 제거 (PCA 입력에 절대 포함되면 안 됨)
  if ("Date" %in% names(asset_df)) {
    asset_df <- asset_df[, names(asset_df) != "Date", drop = FALSE]
  }
  
  # 컬럼명 정리
  names(asset_df) <- trimws(names(asset_df))
  
  # weights 이름 체크
  if (is.null(names(weights)) || any(names(weights) == "")) {
    cat("[PCA] weights에 names가 없어 PCA를 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # ---------- [FIX] PCA용 이름 매핑 (weights -> returns 컬럼명) ----------
  name_map <- c(
    "SPY_ETC" = "SPY",   # returns 컬럼이 SPY인 경우
    "GOLD"    = "GLD"    # returns 컬럼이 GLD인 경우
  )
  weights_pca <- weights
  nm <- names(weights_pca)
  nm <- ifelse(nm %in% names(name_map), unname(name_map[nm]), nm)
  names(weights_pca) <- nm
  # ---------------------------------------------------------------
  
  # 공통 자산만 사용
  common_assets <- intersect(names(asset_df), names(weights_pca))
  if (length(common_assets) < 2) {
    cat("[PCA] 매칭되는 자산이 부족하여 PCA를 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # 공통자산으로 정렬 (열과 weights가 같은 순서가 되도록)
  asset_returns_pca <- asset_df[, common_assets, drop = FALSE]
  weights_use       <- weights_pca[common_assets]
  
  # 최종 방어
  stopifnot(ncol(asset_returns_pca) == length(weights_use))
  stopifnot(all(colnames(asset_returns_pca) == names(weights_use)))
  
  run_pca_risk(asset_returns_pca, weights_use, scale. = scale.)
}

###############################################################################
# (추가 모듈) 1단계 리스크 고도화:
#  - Stress Test Replay
#  - VaR / CVaR
#  - DRIFT 기반 동적 리밸런싱 신호
###############################################################################
suppressMessages({
  library(dplyr)
  library(readr)
})

###############################################################################
# 1) Stress Test Replay
###############################################################################
run_stress_replay_from_file <- function(
    asset_file      = "asset_returns_monthly.csv",
    weights,
    current_nav,
    monthly_contrib = 0,
    crisis_periods  = list(
      `2008_GFC`  = c("2007-10-01", "2009-03-01"),
      `2020_COVID`= c("2019-12-01", "2020-06-01")
    )
) {
  if (!file.exists(asset_file)) {
    cat("[리스크] Stress Test: 자산 수익률 파일을 찾을 수 없습니다. (", asset_file, ")\n")
    return(invisible(NULL))
  }
  
  rets <- read_csv(asset_file, show_col_types = FALSE)
  if (!"Date" %in% colnames(rets)) {
    stop("Stress Test: asset_returns_monthly.csv에 'Date' 컬럼이 필요합니다.")
  }
  rets$Date <- as.Date(rets$Date)
  
  asset_cols <- setdiff(colnames(rets), "Date")
  R_mat      <- as.matrix(rets[, asset_cols])
  
  if (length(weights) != ncol(R_mat)) {
    stop("Stress Test: weights 길이와 자산 열 개수가 다릅니다.")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  .calc_mdd_from_path <- function(nav_vec) {
    peak <- cummax(nav_vec)
    dd   <- nav_vec / peak - 1
    list(
      mdd_value = min(dd, na.rm = TRUE),
      mdd_start = which.max(peak[1:which.min(dd)]),
      mdd_end   = which.min(dd)
    )
  }
  
  cat("\n[리스크] Stress Test Replay 실행...\n")
  cat("========================================\n")
  cat(" 과거 위기 구간을 현재 포트 비중으로 리플레이합니다.\n")
  cat("  - 파일: ", asset_file, "\n")
  cat("  - 현재 기준 평가금: ", format(round(current_nav), big.mark = ","), "원\n")
  cat("========================================\n\n")
  
  for (nm in names(crisis_periods)) {
    rng <- crisis_periods[[nm]]
    s_date <- as.Date(rng[1])
    e_date <- as.Date(rng[2])
    
    sub <- rets %>%
      filter(Date >= s_date & Date <= e_date) %>%
      arrange(Date)
    
    if (nrow(sub) == 0) {
      cat("[경고] ", nm, " 구간(Date: ", format(s_date), " ~ ", format(e_date), ") 데이터가 없습니다.\n\n")
      next
    }
    
    R_sub <- as.matrix(sub[, asset_cols])
    port_ret <- as.numeric(R_sub %*% weights)  # 월간 포트 수익률
    
    nav <- numeric(length(port_ret))
    nav[1] <- current_nav * (1 + port_ret[1]) + monthly_contrib
    if (length(port_ret) > 1) {
      for (i in 2:length(port_ret)) {
        nav[i] <- nav[i-1] * (1 + port_ret[i]) + monthly_contrib
      }
    }
    
    mdd_info <- .calc_mdd_from_path(nav)
    mdd_pct  <- mdd_info$mdd_value * 100
    
    cat("● 시나리오:", nm, "\n")
    cat("   기간 :", format(min(sub$Date)), "~", format(max(sub$Date)), " (", nrow(sub), "개 월 수익률)\n")
    cat("   최종 평가금:", format(round(tail(nav, 1)), big.mark = ","), "원\n")
    cat("   최대낙폭(MDD): ", sprintf("%.2f%%", mdd_pct), "\n")
    cat("   위기 구간 동안 수익률 분포 (요약):\n")
    print(summary(port_ret))
    cat("----------------------------------------\n\n")
  }
  
  invisible(NULL)
}

###############################################################################
# 2) VaR / CVaR 계산
###############################################################################
run_var_cvar_from_file <- function(
    asset_file  = "asset_returns_monthly.csv",
    weights,
    current_nav,
    alpha       = 0.95
) {
  if (!file.exists(asset_file)) {
    cat("[리스크] VaR/CVaR: 자산 수익률 파일을 찾을 수 없습니다. (", asset_file, ")\n")
    return(invisible(NULL))
  }
  
  rets <- read_csv(asset_file, show_col_types = FALSE)
  if (!"Date" %in% colnames(rets)) {
    stop("VaR/CVaR: asset_returns_monthly.csv에 'Date' 컬럼이 필요합니다.")
  }
  rets$Date <- as.Date(rets$Date)
  
  asset_cols <- setdiff(colnames(rets), "Date")
  R_mat      <- as.matrix(rets[, asset_cols])
  
  if (length(weights) != ncol(R_mat)) {
    stop("VaR/CVaR: weights 길이와 자산 열 개수가 다릅니다.")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    weights <- weights / sum(weights)
  }
  
  port_ret <- as.numeric(R_mat %*% weights)   # 월간 포트 수익률
  port_ret <- port_ret[is.finite(port_ret)]
  
  if (length(port_ret) < 20) {
    cat("[리스크] VaR/CVaR: 수익률 표본이 너무 적습니다. (", length(port_ret), ")\n")
    return(invisible(NULL))
  }
  
  q <- stats::quantile(port_ret, probs = 1 - alpha, na.rm = TRUE) # 하위 tail
  var_pct  <- -as.numeric(q)
  cvar_pct <- -mean(port_ret[port_ret <= q], na.rm = TRUE)
  
  var_amt  <- current_nav * var_pct
  cvar_amt <- current_nav * cvar_pct
  
  cat("\n[리스크] VaR / CVaR 계산 (월간 기준 수익률)\n")
  cat("========================================\n")
  cat(" 신뢰수준(α) :", alpha * 100, "%\n")
  cat(" 표본 개수   :", length(port_ret), "개월\n")
  cat("----------------------------------------\n")
  cat(" VaR  (", alpha * 100, "%)  : 약 ",
      sprintf("%.2f%%", var_pct * 100), " (",
      format(round(var_amt), big.mark = ","), "원 손실 가능)\n", sep = "")
  cat(" CVaR(", alpha * 100, "%)  : 약 ",
      sprintf("%.2f%%", cvar_pct * 100), " (",
      format(round(cvar_amt), big.mark = ","), "원 평균 손실)\n", sep = "")
  cat("========================================\n\n")
  
  invisible(list(
    var_pct  = var_pct,
    cvar_pct = cvar_pct,
    var_amt  = var_amt,
    cvar_amt = cvar_amt
  ))
}

###############################################################################
# 3) DRIFT 기반 동적 리밸런싱 신호
###############################################################################
run_drift_rebal_signal <- function(
    target_weights,
    current_weights,
    threshold = 0.05
) {
  if (length(target_weights) != length(current_weights)) {
    stop("DRIFT: target_weights와 current_weights의 길이가 다릅니다.")
  }
  
  if (!is.null(names(target_weights)) && !is.null(names(current_weights))) {
    all_names <- union(names(target_weights), names(current_weights))
    target_weights  <- target_weights[all_names]
    current_weights <- current_weights[all_names]
  }
  
  target_weights[target_weights < 0]   <- 0
  current_weights[current_weights < 0] <- 0
  
  if (sum(target_weights) <= 0 || sum(current_weights) <= 0) {
    stop("DRIFT: 비중 합이 0 이하입니다.")
  }
  
  target_norm  <- target_weights / sum(target_weights)
  current_norm <- current_weights / sum(current_weights)
  
  diff <- current_norm - target_norm  # +: 목표보다 초과, -: 부족
  df <- data.frame(
    Asset          = names(target_norm),
    Target_Weight  = round(target_norm * 100, 2),
    Current_Weight = round(current_norm * 100, 2),
    Drift_pctpt    = round(diff * 100, 2)
  )
  
  cat("\n[리스크] DRIFT 기반 리밸런싱 신호\n")
  cat("========================================\n")
  cat(" (양수: 목표보다 비중 과다 → 매도 후보)\n")
  cat(" (음수: 목표보다 비중 부족 → 매수 후보)\n")
  cat("----------------------------------------\n")
  print(df, row.names = FALSE)
  cat("----------------------------------------\n")
  
  idx <- which(abs(diff) >= threshold)
  if (length(idx) == 0) {
    cat("※ 모든 자산의 드리프트가 ±", threshold * 100,
        "%p 이내입니다. 당장 리밸런싱 필요 신호는 없습니다.\n\n", sep = "")
    return(invisible(df))
  }
  
  cat("※ 리밸런싱 후보 (|Drift| >=", threshold * 100, "%p 이상):\n", sep = "")
  for (i in idx) {
    nm    <- names(diff)[i]
    d_val <- diff[i] * 100
    if (d_val > 0) {
      cat(" -", nm, ": 목표보다 약 +", sprintf("%.2f", d_val),
          "%p 초과 → 일부 매도하여 다른 자산으로 이동 고려\n")
    } else {
      cat(" -", nm, ": 목표보다 약 ", sprintf("%.2f", d_val),
          "%p 부족 → 여유 자금/타 자산 매도로 비중 확대 고려\n")
    }
  }
  cat("========================================\n\n")
  
  invisible(df)
}

# ============================================================
#  GARCH 기반 변동성 위험 경보(Alert) 시스템
# ============================================================
run_garch_vol_alert <- function(dd, min_obs = 250,
                                normal_thr = 1.2,
                                alert_thr  = 1.8) {
  cat("\n[리스크] GARCH 기반 변동성 위험 경보(Alert) 실행...\n")
  cat(strrep("=", 80), "\n")
  
  if (!.has_rugarch) {
    cat("[경고] 'rugarch' 패키지가 설치되어 있지 않아 GARCH 경보를 건너뜁니다.\n")
    cat("       install.packages('rugarch') 후 다시 실행하세요.\n")
    return(invisible(NULL))
  }
  
  # 1) 수익률 벡터 확보 ----------------------------------------
  if (!("Return_TWR" %in% names(dd))) {
    cat("[경고] dd에 'Return_TWR' 컬럼이 없어 GARCH 분석을 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  ret <- as.numeric(dd$Return_TWR)
  ret <- ret[is.finite(ret)]
  
  if (length(ret) < min_obs) {
    cat("[경고] GARCH 적합을 위한 최소 관측치(", min_obs,
        "개) 보다 적어 분석을 건너뜁니다. (현재:", length(ret), "개)\n")
    return(invisible(NULL))
  }
  
  # 2) GARCH(1,1) 모형 정의 및 적합 ----------------------------
  rugarch <- asNamespace("rugarch")
  
  spec <- rugarch$ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit <- try(
    rugarch$ugarchfit(spec, data = ret, solver = "hybrid"),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    cat("[경고] GARCH 적합 중 오류가 발생하여 경보 계산을 건너뜁니다.\n")
    return(invisible(NULL))
  }
  
  # 3) 내일(1-step ahead) 변동성 예측 ---------------------------
  fore <- rugarch$ugarchforecast(fit, n.ahead = 1)
  sigma_forecast <- as.numeric(rugarch$sigma(fore))[1]
  sigma_hist     <- stats::sd(ret, na.rm = TRUE)
  
  ratio <- sigma_forecast / sigma_hist
  
  # 4) 경보 레벨 분류 ------------------------------------------
  level <- if (ratio <= normal_thr) {
    "NORMAL"
  } else if (ratio <= alert_thr) {
    "ALERT"
  } else {
    "CRITICAL"
  }
  
  # 5) 해석용 메시지 생성 --------------------------------------
  msg <- switch(
    level,
    "NORMAL" = paste0(
      "🟢 상태: NORMAL\n",
      "   - 예측 변동성이 평소 수준 대비 크게 높지 않습니다.\n",
      "   - 기존 전략(적립, 리밸런싱, TQQQ 비중) 그대로 유지해도 무방한 상태입니다.\n"
    ),
    "ALERT" = paste0(
      "🟡 상태: ALERT\n",
      "   - 예측 변동성이 장기 평균보다 눈에 띄게 높은 구간입니다.\n",
      "   - 행동 가이드:\n",
      "     · TQQQ 비중 추가 확대는 신중히 검토\n",
      "     · 새로운 레버리지·개별주 공격 매수는 자제\n",
      "     · 예정된 적립식은 그대로 진행하되, 리밸런싱 시 방어자산(GLD/채권) 우선 고려\n"
    ),
    "CRITICAL" = paste0(
      "🔴 상태: CRITICAL\n",
      "   - 예측 변동성이 평소 대비 매우 높은 수준입니다.\n",
      "   - 과거 위기(코로나, 금융위기 등)에서 관측된 수준에 가까울 수 있습니다.\n",
      "   - 행동 가이드:\n",
      "     · TQQQ·고변동 자산 비중 축소 또는 신규매수 일시 중단 검토\n",
      "     · 현금·채권·금 비중을 단기적으로 늘리는 완충 전략 가능\n",
      "     · 감정적 매매(공포/욕심)에 휘둘리지 않도록, 시스템 규칙을 우선\n"
    )
  )
  
  # 6) 결과 출력 -----------------------------------------------
  cat("■ GARCH(1,1) 기반 변동성 요약\n")
  cat("   - 역사적(장기) 일간 변동성(σ_hist): ",
      sprintf("%.4f", sigma_hist), " (약 ", sprintf("%.2f", sigma_hist * 100), "%)\n", sep = "")
  cat("   - 내일 예측 변동성(σ_forecast):     ",
      sprintf("%.4f", sigma_forecast), " (약 ", sprintf("%.2f", sigma_forecast * 100), "%)\n", sep = "")
  cat("   - 예측/역사적 변동성 비율:         ",
      sprintf("%.2f", ratio), "배\n", sep = "")
  cat("   - 경보 레벨:                        ", level, "\n\n", sep = "")
  
  cat(msg, "\n")
  cat(strrep("=", 80), "\n\n")
  
  invisible(
    list(
      level          = level,
      sigma_hist     = sigma_hist,
      sigma_forecast = sigma_forecast,
      ratio          = ratio
    )
  )
}

###############################################################################
# risk_module.R 끝
###############################################################################
>>>>>>> a79f440cc1b5f5452b5e45f7ed21a423f2ebdf27:risk_module.r
