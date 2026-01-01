# ============================================================
# 최근 Max Sharpe (Long-only, sum=1, Rf=0)
# - Efficient Frontier 기반
# - Sharpe: 월간 + 연환산(월간*sqrt(12)) 모두 출력
# ============================================================

# 0) 패키지 ---------------------------------------------------
pkgs <- c("tidyverse", "quadprog")
# quadprog은 포트폴리오 분산 최소화와 같은 이차계획 문제를 정확히 푸는 R 패키지로,
# 현대 포트폴리오 이론(Markowitz)의 수학적 구현을 담당
newp <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newp) > 0) install.packages(newp)

library(tidyverse)
library(quadprog)

# 1) 데이터 로드 ---------------------------------------------
df <- read.csv("asset_returns_monthly.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

# 2) 최근 170개월 --------------------------------------------
df170 <- df %>%
  arrange(Date) %>%
  tail(170)

# 3) 수익률 매트릭스 -----------------------------------------
R <- df170 %>% select(-Date) %>% as.matrix()

# 전부 NA인 열 제거
R <- R[, colSums(is.na(R)) < nrow(R), drop = FALSE]

# 결측 있는 행 제거 (공통 구간)
R <- R[complete.cases(R), , drop = FALSE]

# 4) 표본 추정치 (월 기준) -----------------------------------
mu  <- colMeans(R)
Sig <- cov(R)
n   <- ncol(R)

# 5) 목표수익률에서 최소분산 포트폴리오 ---------------------
#    제약: sum(w)=1, mu'w >= target, w>=0
solve_minvar_for_target <- function(target_m, Sig, mu) {
  
  n <- length(mu)
  
  Dmat <- 2 * Sig
  dvec <- rep(0, n)
  
  # quadprog: A' w >= b
  Amat <- cbind(
    rep(1, n),   # sum(w) = 1  (등식)
    mu,          # mu'w >= target
    diag(n)      # w >= 0
  )
  
  bvec <- c(1, target_m, rep(0, n))
  
  sol <- solve.QP(
    Dmat = Dmat,
    dvec = dvec,
    Amat = Amat,
    bvec = bvec,
    meq  = 1
  )
  
  w <- sol$solution
  
  # 수치 안정화
  w <- pmax(w, 0)
  if (sum(w) > 0) w <- w / sum(w)
  
  return(w)
}

# 6) 목표수익률 그리드 ---------------------------------------
mu_min <- min(mu, na.rm = TRUE)
mu_max <- max(mu, na.rm = TRUE)

targets <- seq(mu_min, mu_max, length.out = 120)

# 7) Efficient Frontier 탐색 ---------------------------------
results <- lapply(targets, function(tg) {
  
  w <- tryCatch(
    solve_minvar_for_target(tg, Sig = Sig, mu = mu),
    error = function(e) NULL
  )
  
  if (is.null(w)) return(NULL)
  
  ret_m <- sum(mu * w)
  vol_m <- sqrt(as.numeric(t(w) %*% Sig %*% w))
  if (!is.finite(vol_m) || vol_m <= 0) return(NULL)
  
  sharpe_m <- ret_m / vol_m
  
  tibble(
    target_m = tg,
    ret_m    = ret_m,
    vol_m    = vol_m,
    sharpe_m = sharpe_m,
    w        = list(w)
  )
})

results <- bind_rows(results)

if (nrow(results) == 0) {
  stop("유효한 최적해를 찾지 못했습니다.")
}

# 8) Max Sharpe 선택 -----------------------------------------
best <- results %>% arrange(desc(sharpe_m)) %>% slice(1)

w_best <- unlist(best$w)
names(w_best) <- colnames(R)

# 9) 연환산 지표 ---------------------------------------------
ret_a <- best$ret_m * 12
vol_a <- best$vol_m * sqrt(12)

sharpe_m <- best$sharpe_m
sharpe_a <- sharpe_m * sqrt(12)

# 10) 출력 ---------------------------------------------------
cat("\n=== 최근 171개월: 제약하(롱온리, 합=1) Max Sharpe ===\n")
cat(sprintf("사용 데이터: %d개월, 자산 수: %d\n", nrow(R), ncol(R)))

cat("\n[가중치]\n")
print(sort(w_best, decreasing = TRUE))

cat(sprintf(
  "\nAnn.Return: %.2f%% | Ann.Vol: %.2f%% | Sharpe(Ann): %.3f | Sharpe(M): %.3f\n",
  ret_a * 100, vol_a * 100, sharpe_a, sharpe_m
))


# 결과 해석 (171개월 Max Sharpe, 롱온리/합=1/Rf=0) 2026년 1월 1일 기준
# 가중치: QQQ 49.90% GLD 23.16% SCHD 23.03% IEF 3.92% TQQQ/SPY ≈ 0 (수치오차 수준) 
# 성과(연환산): Ann.Return 14.14% Ann.Vol 11.74% Sharpe(Ann) 1.205