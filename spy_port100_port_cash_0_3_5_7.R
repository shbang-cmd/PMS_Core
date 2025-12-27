# =========================================================
# FIXED: SPY100 vs PORT + Emergency Cash (0/3/5/7%)
# - avoids xts "subscript out of bounds" by forcing colnames
# - 2012-01-01 ~ today
# - Cash is 0% return (idle emergency fund)
# =========================================================

# 0) packages ----
pkgs <- c("quantmod", "PerformanceAnalytics", "xts", "zoo")
newp <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newp) > 0) install.packages(newp, dependencies = TRUE)

library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)

# 1) settings ----
tickers <- c("SPY","SCHD","QQQ","TQQQ","GLD","IEF")
start_date <- as.Date("2012-01-01")

w_port <- c(SPY=0.40, SCHD=0.20, QQQ=0.15, TQQQ=0.10, GLD=0.10, IEF=0.05)
stopifnot(all(names(w_port) == tickers), abs(sum(w_port) - 1) < 1e-8)

cash_levels <- c(0, 0.03, 0.05, 0.07)

# 2) download prices (Adjusted) ----
suppressWarnings(getSymbols(tickers, src = "yahoo", from = start_date, auto.assign = TRUE))

prices <- do.call(merge, lapply(tickers, function(tk) Ad(get(tk))))
colnames(prices) <- tickers                      # ★ 핵심: 컬럼명 강제 고정
prices <- na.locf(prices, na.rm = FALSE)
prices <- na.omit(prices)

# 3) daily returns (log) ----
rets <- Return.calculate(prices, method = "log")
rets <- na.omit(rets)
stopifnot(all(colnames(rets) == tickers))         # ★ 재발 방지

# 4) benchmark: SPY100 ----
spy100 <- rets[, "SPY", drop = FALSE]
colnames(spy100) <- "SPY100"

# 5) base portfolio (100% invested), daily rebalancing assumption ----
port100 <- Return.portfolio(R = rets, weights = w_port, rebalance_on = "months", geometric = TRUE)
colnames(port100) <- "PORT_CASH0"

# 6) create cash scenarios (Cash return = 0) ----
#    Use arithmetic returns for linear blending, then convert back to log returns.
port100_arith <- exp(port100) - 1

make_port_cash <- function(cash_w) {
  invest_w <- 1 - cash_w
  total_arith <- invest_w * port100_arith + cash_w * 0
  out <- log(1 + total_arith)
  colnames(out) <- paste0("PORT_CASH", round(cash_w * 100))
  out
}

ports <- lapply(cash_levels, make_port_cash)

# merge: SPY100 + PORT_CASH0/3/5/7
rets_cmp <- do.call(merge, c(list(spy100), ports))
rets_cmp <- na.omit(rets_cmp)

# =========================================================
# [Table 1] absolute stats
# =========================================================
stats_tbl <- rbind(
  "Annualized Return" = apply(rets_cmp, 2, Return.annualized, scale = 252, geometric = TRUE),
  "Annualized StdDev" = apply(rets_cmp, 2, StdDev.annualized, scale = 252),
  "Sharpe (Rf=0)"     = apply(rets_cmp, 2, SharpeRatio.annualized, Rf = 0, scale = 252),
  "Max Drawdown"      = apply(rets_cmp, 2, maxDrawdown)
)
stats_tbl <- round(stats_tbl, 4)

cat("===== [표 1] 절대 성과 요약: SPY100 vs PORT + Cash(0/3/5/7) =====\n")
print(stats_tbl)

# =========================================================
# [Table 2] delta vs SPY100
# =========================================================
delta_tbl <- sweep(stats_tbl, 1, stats_tbl[, "SPY100"], "-")
delta_tbl <- round(delta_tbl, 4)

cat("\n===== [표 2] SPY100 대비 차이 (각 열 - SPY100) =====\n")
print(delta_tbl)

# =========================================================
# Charts
# =========================================================
# Wealth index (2012-01-01 = 100)
wealth <- cumprod(1 + (exp(rets_cmp) - 1)) * 100

# Make the portfolio lines thicker than SPY100 (optional)
# SPY100 thin, portfolios thick
lwd_vec <- rep(2, ncol(wealth))
names(lwd_vec) <- colnames(wealth)
lwd_vec["SPY100"] <- 1

chart.TimeSeries(
  wealth,
  legend.loc = "topleft",
  lwd = lwd_vec,
  main = "Wealth Index (2012-01-01=100): SPY100 vs PORT + Cash(0/3/5/7)"
)

chart.Boxplot(rets_cmp, main = "Daily Returns Boxplot (Log Returns): SPY100 vs PORT + Cash")

chart.Drawdown(rets_cmp, legend.loc = "bottomleft",
               main = "Drawdowns (Daily, Log Returns): SPY100 vs PORT + Cash")

charts.PerformanceSummary(rets_cmp,
                          main = "Performance Summary: SPY100 vs PORT + Cash(0/3/5/7)",
                          geometric = TRUE)

# (Optional) Top drawdowns for each series
cat("\n===== Drawdown Top 5 (each) =====\n")
for (nm in colnames(rets_cmp)) {
  cat("\n---", nm, "---\n")
  print(table.Drawdowns(rets_cmp[, nm, drop = FALSE], top = 5))
}
