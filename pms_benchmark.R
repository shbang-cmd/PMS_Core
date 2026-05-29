# PMS vs S&P500/NASDAQ100 Benchmark 서로 비교해보기 위함

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)
library(readr)
library(zoo)
library(DBI)
library(RSQLite)
library(httr)
library(rvest)
library(stringr)
library(patchwork)

############################################################
# 0. DB 연결 + 테이블 자동 생성
############################################################

db_path <- "c:/PMS_Core/naver_stock_price.sqlite"

con <- dbConnect(
  RSQLite::SQLite(),
  db_path
)
on.exit(dbDisconnect(con), add = TRUE)  

if (!"stock_daily_prices" %in% dbListTables(con)) {
  cat("stock_daily_prices 테이블이 없어 새로 생성합니다.\n")
  
  dbExecute(con, "
    CREATE TABLE stock_daily_prices (
      ticker TEXT NOT NULL,
      date TEXT NOT NULL,
      open REAL,
      high REAL,
      low REAL,
      close REAL,
      volume REAL,
      updated_at TEXT,
      PRIMARY KEY (ticker, date)
    )
  ")
}

print(dbListTables(con))

############################################################
# 1. 네이버 종목코드 정리
############################################################

clean_naver_code <- function(ticker) {
  code <- toupper(gsub("\\.K[QS]$", "", ticker))
  code <- gsub("[^0-9A-Z]", "", code)
  
  if (nchar(code) != 6) {
    stop("Invalid code length: ", ticker)
  }
  
  code
}

############################################################
# 2. 네이버 일별 시세 조회
############################################################

get_naver_daily_prices <- function(ticker, years_back = 3, max_pages = 150) {
  
  code <- clean_naver_code(ticker)
  start_date <- Sys.Date() - years(years_back)
  
  all_data <- data.frame()
  
  for (page in 1:max_pages) {
    
    url <- paste0(
      "https://finance.naver.com/item/sise_day.naver?code=",
      code,
      "&page=",
      page
    )
    
    resp <- httr::GET(
      url,
      httr::add_headers(`User-Agent` = "Mozilla/5.0")
    )
    
    html <- rvest::read_html(
      httr::content(resp, as = "text", encoding = "EUC-KR")
    )
    
    tables <- rvest::html_table(html, fill = TRUE)
    
    if (length(tables) == 0) next
    
    tbl <- tables[[1]]
    
    names(tbl) <- c(
      "date", "close", "diff", "open", "high", "low", "volume"
    )
    
    tmp <- tbl %>%
      filter(!is.na(date), str_detect(date, "\\d{4}\\.\\d{2}\\.\\d{2}")) %>%
      mutate(
        ticker = code,
        date = ymd(str_replace_all(date, "\\.", "-")),
        open = as.numeric(gsub(",", "", open)),
        high = as.numeric(gsub(",", "", high)),
        low = as.numeric(gsub(",", "", low)),
        close = as.numeric(gsub(",", "", close)),
        volume = as.numeric(gsub(",", "", volume))
      ) %>%
      select(ticker, date, open, high, low, close, volume)
    
    all_data <- bind_rows(all_data, tmp)
    
    if (nrow(tmp) > 0 && min(tmp$date, na.rm = TRUE) < start_date) {
      break
    }
    
    Sys.sleep(0.2)
  }
  
  all_data %>%
    filter(date >= start_date) %>%
    distinct(ticker, date, .keep_all = TRUE) %>%
    arrange(ticker, date)
}

############################################################
# 3. 가격 DB 저장 / 업데이트
############################################################
save_prices_to_db <- function(con, ticker, years_back = 3) {
  
  code <- clean_naver_code(ticker)
  
  df <- get_naver_daily_prices(
    ticker     = code,
    years_back = years_back
  )
  
  if (nrow(df) == 0) {
    stop("가져온 데이터가 없습니다: ", code)
  }
  
  df <- df %>%
    mutate(
      date       = as.character(date),
      updated_at = as.character(Sys.time())
    )

  dbBegin(con)

    tryCatch({
    
    for (i in seq_len(nrow(df))) {
      dbExecute(
        con,
        "
        INSERT OR REPLACE INTO stock_daily_prices
        (ticker, date, open, high, low, close, volume, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        ",
        params = list(
          df$ticker[i],
          df$date[i],
          df$open[i],
          df$high[i],
          df$low[i],
          df$close[i],
          df$volume[i],
          df$updated_at[i]
        )
      )
    }
    
    # ✅ [수정 3] 모든 INSERT 성공 시 한 번에 커밋
    dbCommit(con)
    cat(code, " 저장 완료: ", nrow(df), "건\n")
    
  }, error = function(e) {
    
    dbRollback(con)
    cat("[오류]", code, "저장 실패 → 롤백 완료:", conditionMessage(e), "\n")
    
  })
}


ensure_price_exists <- function(con, ticker) {
  
  code <- clean_naver_code(ticker)
  
  n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM stock_daily_prices
    WHERE ticker = ?
    ",
    params = list(code)
  )$n[1]
  
  if (n == 0) {
    cat(code, " 데이터가 없어 새로 다운로드합니다.\n")
    save_prices_to_db(con, code, years_back = 3)
  } else {
    cat(code, " 데이터 존재: ", n, "건\n")
  }
}

get_price_history <- function(con, ticker) {
  
  code <- clean_naver_code(ticker)
  
  data <- dbGetQuery(
    con,
    "
    SELECT *
    FROM stock_daily_prices
    WHERE ticker = ?
    ORDER BY date
    ",
    params = list(code)
  )
  
  if (nrow(data) == 0) {
    stop(code, " 데이터가 DB에 없습니다.")
  }
  
  data
}

############################################################
# 4. ETF 데이터 준비
############################################################

tickers <- c("379800", "379810") # KODEX S&P500, KODEX NASDAQ100

for (ticker in tickers) {
  ensure_price_exists(con, ticker)
}

sp500 <- get_price_history(con, "379800") %>%
  transmute(
    Date = as.Date(date),
    SP500_Close = close
  )

nasdaq <- get_price_history(con, "379810") %>%
  transmute(
    Date = as.Date(date),
    NASDAQ_Close = close
  )

############################################################
# 5. PMS 파일 읽기
############################################################

pms <- read_csv(
  "c:/PMS_Core/output_sum.csv",
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    Sum = as.numeric(Sum),
    Profit = as.numeric(Profit)
  ) %>%
  arrange(Date) %>%
  mutate(
    Base_Profit = first(Profit),
    Invested = Sum - Profit,
    Sum_Adjusted = Sum - Base_Profit,
    Profit_Adjusted = Profit - Base_Profit
  )

############################################################
# 6. PMS + ETF 결합
############################################################

bench <- pms %>%
  left_join(sp500, by = "Date") %>%
  left_join(nasdaq, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    SP500_Close = zoo::na.locf(SP500_Close, na.rm = FALSE),
    NASDAQ_Close = zoo::na.locf(NASDAQ_Close, na.rm = FALSE)
  ) %>%
  filter(!is.na(SP500_Close), !is.na(NASDAQ_Close))

bench <- bench %>%
  mutate(
    Invest_Flow = Invested - lag(Invested, default = 0),
    Invest_Flow = pmax(Invest_Flow, 0)
  )

bench <- bench %>%
  mutate(
    SP500_Units = Invest_Flow / SP500_Close,
    NASDAQ_Units = Invest_Flow / NASDAQ_Close,
    
    SP500_Total_Units = cumsum(replace_na(SP500_Units, 0)),
    NASDAQ_Total_Units = cumsum(replace_na(NASDAQ_Units, 0)),
    
    SP500_Benchmark = SP500_Total_Units * SP500_Close,
    NASDAQ_Benchmark = NASDAQ_Total_Units * NASDAQ_Close,
    
    PMS_Return_Real = Sum / Invested - 1,
    PMS_Return_Adjusted = Sum_Adjusted / Invested - 1,
    
    SP500_Return = SP500_Benchmark / Invested - 1,
    NASDAQ_Return = NASDAQ_Benchmark / Invested - 1,
    
    PMS_vs_SP500_Real = Sum - SP500_Benchmark,
    PMS_vs_NASDAQ_Real = Sum - NASDAQ_Benchmark,
    
    PMS_vs_SP500_Adjusted = Sum_Adjusted - SP500_Benchmark,
    PMS_vs_NASDAQ_Adjusted = Sum_Adjusted - NASDAQ_Benchmark
  )

############################################################
# 7. 성과 함수
############################################################

calc_mdd <- function(x) {
  drawdown <- x / cummax(x) - 1
  abs(min(drawdown, na.rm = TRUE))
}

calc_sharpe <- function(x, rf_daily = 0.03 / 252) {
  
  r <- x / lag(x) - 1
  r <- r[!is.na(r)]
  
  if (length(r) < 2 || sd(r, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  excess <- r - rf_daily
  
  # annualize 제거
  mean(excess, na.rm = TRUE) /
    sd(excess, na.rm = TRUE)
}

############################################################
# 8. PMS vs ETF 성과표
############################################################

perf_df <- bench %>%
  select(
    Date,
    PMS = Sum_Adjusted,
    SP500 = SP500_Benchmark,
    NASDAQ = NASDAQ_Benchmark
  ) %>%
  filter(PMS > 0, SP500 > 0, NASDAQ > 0)

result <- data.frame(
  Strategy = c("PMS", "KODEX S&P500", "KODEX NASDAQ100"),
  
  Final_Return = round(c(
    tail(perf_df$PMS, 1) / first(perf_df$PMS) - 1,
    tail(perf_df$SP500, 1) / first(perf_df$SP500) - 1,
    tail(perf_df$NASDAQ, 1) / first(perf_df$NASDAQ) - 1
  ) * 100, 2),
  
  MDD = round(c(
    calc_mdd(perf_df$PMS),
    calc_mdd(perf_df$SP500),
    calc_mdd(perf_df$NASDAQ)
  ) * 100, 2),
  
  Sharpe = round(c(
    calc_sharpe(perf_df$PMS),
    calc_sharpe(perf_df$SP500),
    calc_sharpe(perf_df$NASDAQ)
  ), 2)
)

print(result)

############################################################
# 9. 성과 막대그래프
############################################################


############################################################
# 10. 100 기준 누적 성과 비교
############################################################


############################################################
# 11. 3개월 Rolling 비교 - 완전한 3개월 구간만 계산
############################################################

rolling_months <- 3

rolling_base <- perf_df %>%
  arrange(Date) %>%
  mutate(Date = as.Date(Date))

max_data_date <- max(rolling_base$Date, na.rm = TRUE)

# 마지막 시작일은 "최종 데이터일 - 3개월"까지만 허용
last_start_date <- max_data_date %m-% months(rolling_months)

rolling_base_valid <- rolling_base %>%
  filter(Date <= last_start_date)

rolling_result <- data.frame()

rolling_list <- lapply(seq_len(nrow(rolling_base_valid)), function(i) {
  
  start_date      <- as.Date(rolling_base_valid$Date[i])
  target_end_date <- start_date %m+% months(rolling_months)
  
  tmp <- rolling_base %>%
    filter(Date >= start_date, Date <= target_end_date)
  
  # ✅ [수정 2] next 대신 NULL 반환
  #   - 기존: for 루프 안에서 next로 건너뜀
  #   - 수정: lapply 안에서는 next를 쓸 수 없으므로 NULL 반환
  #     → 마지막 bind_rows()에서 NULL은 자동으로 무시됨
  if (nrow(tmp) < 20) return(NULL)
  
  data.frame(
    Start_Date    = start_date,
    End_Date      = max(tmp$Date),
    
    PMS_Return    = tail(tmp$PMS,    1) / first(tmp$PMS)    - 1,
    SP500_Return  = tail(tmp$SP500,  1) / first(tmp$SP500)  - 1,
    NASDAQ_Return = tail(tmp$NASDAQ, 1) / first(tmp$NASDAQ) - 1,
    
    PMS_MDD       = calc_mdd(tmp$PMS),
    SP500_MDD     = calc_mdd(tmp$SP500),
    NASDAQ_MDD    = calc_mdd(tmp$NASDAQ),
    
    PMS_Sharpe    = calc_sharpe(tmp$PMS),
    SP500_Sharpe  = calc_sharpe(tmp$SP500),
    NASDAQ_Sharpe = calc_sharpe(tmp$NASDAQ)
  )
})

# ✅ [수정 3] list 결과를 마지막에 한 번만 합치기
#   - NULL(nrow < 20 으로 건너뛴 구간)은 bind_rows()가 자동 무시
rolling_result <- bind_rows(rolling_list)


# cat("\n최종 데이터일: ", as.character(max_data_date), "\n")
# cat("Rolling 마지막 시작일: ", as.character(max(rolling_result$Start_Date)), "\n")
# cat("Rolling 마지막 종료일: ", as.character(max(rolling_result$End_Date)), "\n")


rolling_return_long <- rolling_result %>%
  select(
    Start_Date,
    PMS = PMS_Return,
    SP500 = SP500_Return,
    NASDAQ = NASDAQ_Return
  ) %>%
  pivot_longer(
    cols = -Start_Date,
    names_to = "Strategy",
    values_to = "Return"
  )

rolling_mdd_long <- rolling_result %>%
  select(
    Start_Date,
    PMS = PMS_MDD,
    SP500 = SP500_MDD,
    NASDAQ = NASDAQ_MDD
  ) %>%
  pivot_longer(
    cols = -Start_Date,
    names_to = "Strategy",
    values_to = "MDD"
  )

rolling_sharpe_long <- rolling_result %>%
  select(
    Start_Date,
    PMS = PMS_Sharpe,
    SP500 = SP500_Sharpe,
    NASDAQ = NASDAQ_Sharpe
  ) %>%
  pivot_longer(
    cols = -Start_Date,
    names_to = "Strategy",
    values_to = "Sharpe"
  )


############################################################
# 12. Rolling 그래프
############################################################

############################################################
# Rolling 3개 그래프 통합 표시
# - Return / MDD / Sharpe
# - PMS 우월 구간 회색 표시
# - PMS 우월구간 비율 제목 표시
# - 전체 제목: PMS vs S&P500/NASDAQ100 Benchmark
############################################################

############################################################
# 1. PMS 우월 여부 계산
############################################################

rolling_flag <- rolling_result %>%
  arrange(Start_Date) %>%
  mutate(
    PMS_Win_Return =
      PMS_Return > SP500_Return &
      PMS_Return > NASDAQ_Return,
    
    PMS_Win_MDD =
      PMS_MDD < SP500_MDD &
      PMS_MDD < NASDAQ_MDD,
    
    PMS_Win_Sharpe =
      PMS_Sharpe > SP500_Sharpe &
      PMS_Sharpe > NASDAQ_Sharpe,
    
    next_date = lead(Start_Date, default = max(Start_Date) + 1)
  )

############################################################
# 2. PMS 우월 구간 비율 계산
############################################################

pms_win_rate_return <- mean(rolling_flag$PMS_Win_Return, na.rm = TRUE)
pms_win_rate_mdd    <- mean(rolling_flag$PMS_Win_MDD, na.rm = TRUE)
pms_win_rate_sharpe <- mean(rolling_flag$PMS_Win_Sharpe, na.rm = TRUE)

############################################################
# 3. 회색 음영 구간 만들기
############################################################

pms_win_return <- rolling_flag %>%
  filter(PMS_Win_Return) %>%
  transmute(
    xmin = as.Date(Start_Date),
    xmax = as.Date(next_date),
    ymin = -Inf,
    ymax = Inf
  )

pms_win_mdd <- rolling_flag %>%
  filter(PMS_Win_MDD) %>%
  transmute(
    xmin = as.Date(Start_Date),
    xmax = as.Date(next_date),
    ymin = -Inf,
    ymax = Inf
  )

pms_win_sharpe <- rolling_flag %>%
  filter(PMS_Win_Sharpe) %>%
  transmute(
    xmin = as.Date(Start_Date),
    xmax = as.Date(next_date),
    ymin = -Inf,
    ymax = Inf
  )

############################################################
# 4. 각 지표별 최대 격차 시점 찾기
############################################################

max_gap_return <- rolling_result %>%
  mutate(
    Gap = pmax(PMS_Return, SP500_Return, NASDAQ_Return, na.rm = TRUE) -
      pmin(PMS_Return, SP500_Return, NASDAQ_Return, na.rm = TRUE)
  ) %>%
  slice_max(Gap, n = 1, with_ties = FALSE)

max_gap_mdd <- rolling_result %>%
  mutate(
    Gap = pmax(PMS_MDD, SP500_MDD, NASDAQ_MDD, na.rm = TRUE) -
      pmin(PMS_MDD, SP500_MDD, NASDAQ_MDD, na.rm = TRUE)
  ) %>%
  slice_max(Gap, n = 1, with_ties = FALSE)

max_gap_sharpe <- rolling_result %>%
  mutate(
    Gap = pmax(PMS_Sharpe, SP500_Sharpe, NASDAQ_Sharpe, na.rm = TRUE) -
      pmin(PMS_Sharpe, SP500_Sharpe, NASDAQ_Sharpe, na.rm = TRUE)
  ) %>%
  slice_max(Gap, n = 1, with_ties = FALSE)

############################################################
# 5. 최대 격차 시점 라벨 데이터
############################################################

label_return <- rolling_return_long %>%
  filter(Start_Date == max_gap_return$Start_Date) %>%
  mutate(
    Label = paste0(
      Strategy,
      ": ",
      percent(Return, accuracy = 0.1)
    )
  )

label_mdd <- rolling_mdd_long %>%
  filter(Start_Date == max_gap_mdd$Start_Date) %>%
  mutate(
    Label = paste0(
      Strategy,
      ": ",
      percent(MDD, accuracy = 0.1)
    )
  )

label_sharpe <- rolling_sharpe_long %>%
  filter(Start_Date == max_gap_sharpe$Start_Date) %>%
  mutate(
    Label = paste0(
      Strategy,
      ": ",
      round(Sharpe, 2)
    )
  )

############################################################
# 6. Return 그래프
############################################################

p_return <- ggplot(
  rolling_return_long,
  aes(
    x = Start_Date,
    y = Return,
    color = Strategy
  )
) +
  geom_rect(
    data = pms_win_return,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "gray70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = max_gap_return$Start_Date,
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_point(
    data = label_return,
    aes(
      x = Start_Date,
      y = Return,
      color = Strategy
    ),
    size = 3
  ) +
  geom_text(
    data = label_return,
    aes(
      x = Start_Date,
      y = Return,
      label = Label,
      color = Strategy
    ),
    hjust = -0.05,
    vjust = -0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  labs(
    title = paste0(
      "① 수익률 비교 : 3개월 Rolling 수익률 | PMS 우월구간: ",
      percent(pms_win_rate_return, accuracy = 0.1),
      " | 최대격차: ",
      percent(max_gap_return$Gap, accuracy = 0.1)
    ),
    subtitle = "회색 구간: PMS 수익률이 S&P500과 NASDAQ100보다 모두 높은 시작일",
    x = NULL,
    y = "Return",
    color = NULL
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

############################################################
# 7. MDD 그래프
############################################################

p_mdd <- ggplot(
  rolling_mdd_long,
  aes(
    x = Start_Date,
    y = MDD,
    color = Strategy
  )
) +
  geom_rect(
    data = pms_win_mdd,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "gray70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = max_gap_mdd$Start_Date,
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_point(
    data = label_mdd,
    aes(
      x = Start_Date,
      y = MDD,
      color = Strategy
    ),
    size = 3
  ) +
  geom_text(
    data = label_mdd,
    aes(
      x = Start_Date,
      y = MDD,
      label = Label,
      color = Strategy
    ),
    hjust = -0.05,
    vjust = -0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  labs(
    title = paste0(
      "②  최대낙폭 비교 : 3개월 Rolling MDD | PMS 우월구간: ",
      percent(pms_win_rate_mdd, accuracy = 0.1),
      " | 최대격차: ",
      percent(max_gap_mdd$Gap, accuracy = 0.1)
    ),
    subtitle = "회색 구간: PMS MDD가 S&P500과 NASDAQ100보다 모두 낮은 시작일",
    x = NULL,
    y = "MDD",
    color = NULL
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  )

############################################################
# 8. Sharpe 그래프
############################################################

p_sharpe <- ggplot(
  rolling_sharpe_long,
  aes(
    x = Start_Date,
    y = Sharpe,
    color = Strategy
  )
) +
  geom_rect(
    data = pms_win_sharpe,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "gray70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = max_gap_sharpe$Start_Date,
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_point(
    data = label_sharpe,
    aes(
      x = Start_Date,
      y = Sharpe,
      color = Strategy
    ),
    size = 3
  ) +
  geom_text(
    data = label_sharpe,
    aes(
      x = Start_Date,
      y = Sharpe,
      label = Label,
      color = Strategy
    ),
    hjust = -0.05,
    vjust = -0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  labs(
    title = paste0(
      "③ 샤프지수 비교 : 3개월 Rolling Sharpe | PMS 우월구간: ",
      percent(pms_win_rate_sharpe, accuracy = 0.1),
      " | 최대격차: ",
      round(max_gap_sharpe$Gap, 2)
    ),
    subtitle = "회색 구간: PMS Sharpe가 S&P500과 NASDAQ100보다 모두 높은 시작일",
    x = "시작일",
    y = "Sharpe",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  )

############################################################
# 9. 세 그래프 한 장으로 결합 + 전체 제목
############################################################

combined_plot <- p_return / p_mdd / p_sharpe +
  plot_layout(
    heights = c(1, 1, 1)
  ) +
  plot_annotation(
    title = "PMS vs S&P500/NASDAQ100 Benchmark",
    subtitle = "3개월 Rolling 기준: 수익률, MDD, Sharpe 비교(각 시작일마다 향후 3개월 동안 투자했을 때 성과)",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 12
      )
    )
  )

print(combined_plot)

############################################################
# 10. 최대 격차 요약표
############################################################

max_gap_summary <- data.frame(
  Metric = c(
    "Rolling Return",
    "Rolling MDD",
    "Rolling Sharpe"
  ),
  Start_Date = c(
    as.character(max_gap_return$Start_Date),
    as.character(max_gap_mdd$Start_Date),
    as.character(max_gap_sharpe$Start_Date)
  ),
  Gap = c(
    percent(max_gap_return$Gap, accuracy = 0.1),
    percent(max_gap_mdd$Gap, accuracy = 0.1),
    round(max_gap_sharpe$Gap, 2)
  )
)

print(max_gap_summary)


################################################### DISCONNECT DB

if (DBI::dbIsValid(con)) {
  dbDisconnect(con)
}

# 프로그램 후기 : 처음에는 단순한 호기심에서 시작되었다. 내가 몇 년 동안 만들고 실제로 운용해 온 PMS(Portfolio Monitoring System)가 과연 시장을 이기고 있는가, 아니면 그저 스스로 만족하는 체계에 불과한가 하는 질문이었다. 처음에는 막연한 기대도 있었다. 수많은 시행착오와 R 코드 작성, 자료 수집, 종목 선정과 비중 조절, 위험 관리와 리밸런싱 규칙 등을 거쳐 만든 시스템이니 최소한 단순히 S&P500 ETF를 사서 들고 가는 것보다는 나을 것이라는 기대 말이다. 인간은 본능적으로 노력한 것에 가치를 부여한다. 많은 시간을 들여 만든 것은 더 우수해야 한다고 믿고 싶어한다. 그것이 심리학에서 말하는 매몰비용 효과인지, 창조자 편향인지, 아니면 단순한 자존심인지는 모르지만, 어쨌든 나 역시 그런 기대를 가지고 있었던 것 같다.
# 
# 그래서 실제 데이터를 이용해 비교를 시작했다. PMS의 투자원금 흐름을 그대로 따라가며 동일한 현금흐름을 S&P500 ETF와 NASDAQ100 ETF에 투자했다고 가정했고, 단순 평가액뿐 아니라 MDD, 샤프지수까지 계산해 보았다. 처음 결과를 봤을 때는 약간 허무했다. PMS는 확실히 부드럽게 움직였다. 낙폭도 작았고, 그래프도 상당히 안정적이었다. 그러나 최종 성과는 S&P500이나 NASDAQ100에 미치지 못했다. 특히 최근 AI와 빅테크 중심 상승장에서 NASDAQ은 거의 압도적으로 보였다. 처음에는 이런 생각이 들었다. "몇 년 동안 만든 시스템이 그냥 미국 ETF 하나만도 못한 건가?"
# 
# 하지만 조금 더 들여다보기 시작하면서 생각이 바뀌었다. PMS는 실패한 것이 아니라 애초에 역할이 달랐던 것이 아닐까 하는 생각이 들었다. PMS는 성장 엔진이라기보다 충격 흡수 장치에 가까웠다. MDD는 가장 낮았고 샤프도 생각보다 상당히 우수했다. 단독으로는 성장률이 부족했지만, 혼합 포트폴리오에 넣었을 때는 오히려 수익률을 크게 깎지 않으면서 위험을 줄이는 역할을 했다. 마치 자동차의 엔진이 아니라 서스펜션 같은 존재였다. 평소에는 눈에 띄지 않지만 위기가 왔을 때 진가를 발휘하는 것 말이다.
# 
# 그러나 여기서 또 다른 생각이 들었다. 이런 해석도 결국 결과를 보고 억지로 끼워 맞추는 것이 아닐까 하는 의문이었다. 최근 몇 년은 AI와 미국 빅테크가 강했던 시기였다. 만약 비교 기간이 2000년 닷컴버블 이후였다면 어땠을까. 만약 2008년 금융위기 직후였다면? 혹은 금과 원자재가 폭발적으로 상승했던 시기였다면? 아마 전혀 다른 결론이 나왔을 것이다. NASDAQ이 최고라는 말 대신 위험하다는 말이 나왔을 것이고, PMS가 시장을 이겼다면 천재적인 시스템이라는 평가를 했을지도 모른다. 그런데 사실 미래를 모르는 상태에서는 둘 다 똑같은 이야기일 것이다.
# 
# 그 순간 조금 섬뜩한 생각이 들었다. 인간은 결과를 보고 의미를 만든다. 성공하면 원인을 설명하고, 실패하면 또 다른 원인을 설명한다. 그리고 그 설명은 놀라울 정도로 그럴듯하다. 나심 탈레브가 말했던 사후편향(Hindsight Bias)이 바로 이런 것이 아닐까. 일이 벌어지고 나면 모든 것이 원래 예정된 것처럼 보인다. 하지만 사건이 일어나기 전에는 아무도 몰랐다. 그래서 투자에서 가장 위험한 것은 틀리는 것이 아니라, 결과를 보고 자신이 원래부터 알고 있었다고 믿는 것일지도 모른다.
# 
# 결국 처음 질문은 "어떤 전략이 최고인가?"였는데, 마지막에는 전혀 다른 질문으로 바뀌었다. "어떤 미래가 와도 내가 버틸 수 있는 구조인가?"라는 질문이다. 미래에 NASDAQ이 계속 강세일지, 큰 조정이 올지, 미국이 앞으로도 세계를 계속 지배할지 아무도 모른다. 그건 운의 영역이다. 그러나 미래를 모른다는 사실 자체를 인정하고 그 위에 구조를 만드는 것은 실력의 영역일 수 있다.
# 
# 돌아보면 PMS를 만들면서 얻은 가장 큰 수확은 높은 수익률이 아니라 이런 깨달음이었는지도 모르겠다. 투자란 미래를 예측하는 게임이 아니라, 미래를 모른다는 사실을 견딜 수 있는 시스템을 만드는 과정이라는 것. 그리고 어쩌면 내가 만든 PMS는 S&P500을 이기기 위한 도구가 아니라, 미래를 모른다는 사실을 받아들이기 위한 도구였는지도 모르겠다.