# ============================================================
# Yahoo 기반 "오늘(한국) 주식시장 개장 여부" 판단 함수
# - 원리: KOSPI 지수(^KS11) 또는 삼성전자(005930.KS)의
#         최신 거래일(last trading date)이 '오늘'이면 개장일.
# - 장 시작 전(특히 09:00 이전)에는 오늘 데이터가 아직 없을 수 있어
#   기본적으로 "오늘 날짜 == 최신 거래일"을 사용하고,
#   원하시면 'close_only=TRUE'로 "장 마감 후에만 TRUE"처럼 보수화 가능
# ============================================================

pkg <- c("quantmod", "xts")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(quantmod)

is_korea_market_open_yahoo <- function(
    date = Sys.Date(),
    tz = "Asia/Seoul",
    symbol_primary = "^KS11",        # KOSPI 지수
    symbol_fallback = "005930.KS",   # 삼성전자(보조)
    lookback_days = 20,
    close_only = FALSE,             # TRUE면 "장 마감 후에만 오늘=개장"으로 판단
    close_hour = 15, close_min = 35 # KST 기준, 장 종료(대략) 후 여유 포함
) {
  d <- as.Date(date)
  
  # 1) 주말은 무조건 휴장
  wday <- as.POSIXlt(as.POSIXct(d, tz = tz))$wday  # 0=일,6=토
  if (wday %in% c(0, 6)) return(FALSE)
  
  # 2) close_only 옵션: 장 마감 전에는 "개장 확인"을 보류(FALSE)
  if (isTRUE(close_only)) {
    now_kst <- as.POSIXct(Sys.time(), tz = tz)
    cutoff  <- as.POSIXct(paste(d, sprintf("%02d:%02d:00", close_hour, close_min)),
                          tz = tz)
    if (now_kst < cutoff) return(FALSE)
  }
  
  # 3) Yahoo에서 최근 거래일 확인 (primary -> fallback)
  fetch_last_trade_date <- function(sym) {
    from <- d - lookback_days
    xt <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, auto.assign = FALSE, warnings = FALSE),
      error = function(e) NULL
    )
    if (is.null(xt) || NROW(xt) == 0) return(NA_Date_)
    as.Date(tail(index(xt), 1))
  }
  
  last1 <- fetch_last_trade_date(symbol_primary)
  if (is.na(last1)) last1 <- fetch_last_trade_date(symbol_fallback)
  
  if (is.na(last1)) {
    # Yahoo 장애/네트워크 문제 가능 → 보수적으로 휴장(FALSE) 처리
    return(FALSE)
  }
  
  # 4) 최신 거래일이 오늘이면 "개장일"
  identical(last1, d)
}

# ------------------------
# 사용 예시
# ------------------------

# (1) "오늘 개장일인가?"  → 장중에도 TRUE가 나올 수 있음(오늘 데이터가 이미 올라온 경우)
if (is_korea_market_open_yahoo()) {
  cat("✅ 오늘은 한국 주식시장 개장일(거래일)로 판단됩니다.\n")
} else {
  cat("⛔ 오늘은 휴장 또는 아직 확인 불가로 판단됩니다.\n")
}

# (2) 보수적으로: 장 마감 이후에만 TRUE (PMS를 16시 이후에 돌리면 권장)
if (is_korea_market_open_yahoo(close_only = TRUE)) {
  cat("✅ (장 마감 기준) 오늘은 개장일로 확정됩니다.\n")
} else {
  cat("⛔ (장 마감 기준) 오늘은 휴장/미확정입니다.\n")
}

# 코드에서 사용법 : 아래와 같이 하면 거래소 시장이 안열리는날 더 이상 실행안하고 종료됨
# if (!is_korea_market_open_yahoo(close_only = TRUE)) {
#   quit(save = "no", status = 0)
# }

