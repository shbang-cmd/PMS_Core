# ============================================================
# pms_benchmark.R
#
# PMS vs KODEX S&P500 / KODEX NASDAQ100 Benchmark
#
# 2026-09-11 수정
#
# 기존:
# finance.naver.com/item/sise_day.naver
# + EUC-KR HTML 크롤링
#
# 변경:
# api.stock.naver.com/chart/domestic/item/{code}
# + JSON API
#
# 기존 SQLite DB / Benchmark / Rolling MDD / Sharpe 구조 유지
# ============================================================


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
library(jsonlite)
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


if (!"stock_daily_prices" %in% dbListTables(con)) {
  
  cat(
    "stock_daily_prices 테이블이 없어 새로 생성합니다.\n"
  )
  
  dbExecute(
    con,
    "
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
    "
  )
  
}


print(
  dbListTables(con)
)



############################################################
# 1. 네이버 종목코드 정리
############################################################

clean_naver_code <- function(ticker) {
  
  code <- toupper(
    gsub(
      "\\.K[QS]$",
      "",
      ticker
    )
  )
  
  
  code <- gsub(
    "[^0-9A-Z]",
    "",
    code
  )
  
  
  if (nchar(code) != 6) {
    
    stop(
      "Invalid code length: ",
      ticker
    )
    
  }
  
  
  code
  
}



############################################################
# 2. 네이버 일별 시세 조회
#
# 2026-09-11
#
# 기존:
#
# https://finance.naver.com/item/sise_day.naver
#
# + HTML
# + EUC-KR
# + html_table()
#
# 폐기
#
# 새 방식:
#
# https://api.stock.naver.com/chart/domestic/item/{code}
#
# JSON API
#
############################################################

get_naver_daily_prices <- function(
    ticker,
    years_back = 3,
    max_retry = 3,
    retry_wait = 1
) {
  
  
  code <- clean_naver_code(
    ticker
  )
  
  
  start_date <-
    Sys.Date() -
    lubridate::years(
      years_back
    )
  
  
  end_date <-
    Sys.Date()
  
  
  start_text <-
    format(
      start_date,
      "%Y%m%d"
    )
  
  
  end_text <-
    format(
      end_date,
      "%Y%m%d"
    )
  
  
  url <- paste0(
    
    "https://api.stock.naver.com/chart/domestic/item/",
    code,
    
    "?periodType=dayCandle",
    
    "&startDateTime=",
    start_text,
    
    "&endDateTime=",
    end_text
    
  )
  
  
  last_error <- NULL
  
  
  
  ############################################################
  # JSON 내부에서 일별 가격 배열 탐색
  ############################################################
  
  find_price_list <- function(obj) {
    
    
    if (is.null(obj)) {
      
      return(NULL)
      
    }
    
    
    # 일반적으로 예상되는 구조
    if (
      is.list(obj) &&
      !is.null(obj$priceInfo)
    ) {
      
      return(
        obj$priceInfo
      )
      
    }
    
    
    if (
      is.list(obj) &&
      !is.null(obj$priceInfos)
    ) {
      
      return(
        obj$priceInfos
      )
      
    }
    
    
    if (
      is.list(obj) &&
      !is.null(obj$result)
    ) {
      
      
      if (
        !is.null(
          obj$result$priceInfo
        )
      ) {
        
        return(
          obj$result$priceInfo
        )
        
      }
      
      
      if (
        !is.null(
          obj$result$priceInfos
        )
      ) {
        
        return(
          obj$result$priceInfos
        )
        
      }
      
    }
    
    
    # 응답 자체가 일별 record 배열일 경우
    if (
      is.list(obj) &&
      length(obj) > 0
    ) {
      
      
      first_item <- obj[[1]]
      
      
      if (
        is.list(first_item) &&
        !is.null(first_item$localDate)
      ) {
        
        return(obj)
        
      }
      
    }
    
    
    NULL
    
  }
  
  
  
  ############################################################
  # 최대 3회 재시도
  ############################################################
  
  for (attempt in 1:max_retry) {
    
    
    result <- tryCatch({
      
      
      resp <- httr::GET(
        
        url,
        
        
        httr::add_headers(
          
          `User-Agent` =
            paste0(
              "Mozilla/5.0 ",
              "(Windows NT 10.0; Win64; x64) ",
              "AppleWebKit/537.36"
            ),
          
          
          Accept =
            "application/json,text/plain,*/*",
          
          
          Referer =
            paste0(
              "https://stock.naver.com/domestic/stock/",
              code,
              "/price"
            )
          
        ),
        
        
        # 회사 PC SSL 인증서 문제 대응
        httr::config(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        ),
        
        
        httr::timeout(
          15
        )
        
      )
      
      
      httr::stop_for_status(
        resp
      )
      
      
      json_text <- httr::content(
        
        resp,
        
        as = "text",
        
        encoding = "UTF-8"
        
      )
      
      
      obj <- jsonlite::fromJSON(
        
        json_text,
        
        simplifyVector = FALSE
        
      )
      
      
      price_list <-
        find_price_list(
          obj
        )
      
      
      if (
        is.null(price_list) ||
        length(price_list) == 0
      ) {
        
        stop(
          "네이버 JSON에서 일별 가격 데이터 찾지 못함"
        )
        
      }
      
      
      
      ##########################################################
      # JSON list -> data.frame
      ##########################################################
      
      df <- dplyr::bind_rows(
        
        lapply(
          price_list,
          function(x) {
            
            
            local_date <-
              as.character(
                x$localDate
              )
            
            
            # 혹시 YYYY-MM-DD로 반환될 경우도 대응
            if (
              grepl(
                "-",
                local_date,
                fixed = TRUE
              )
            ) {
              
              parsed_date <-
                as.Date(
                  local_date
                )
              
            } else {
              
              parsed_date <-
                as.Date(
                  local_date,
                  format = "%Y%m%d"
                )
              
            }
            
            
            data.frame(
              
              ticker =
                code,
              
              
              date =
                parsed_date,
              
              
              open =
                as.numeric(
                  gsub(
                    ",",
                    "",
                    as.character(
                      x$openPrice
                    ),
                    fixed = TRUE
                  )
                ),
              
              
              high =
                as.numeric(
                  gsub(
                    ",",
                    "",
                    as.character(
                      x$highPrice
                    ),
                    fixed = TRUE
                  )
                ),
              
              
              low =
                as.numeric(
                  gsub(
                    ",",
                    "",
                    as.character(
                      x$lowPrice
                    ),
                    fixed = TRUE
                  )
                ),
              
              
              close =
                as.numeric(
                  gsub(
                    ",",
                    "",
                    as.character(
                      x$closePrice
                    ),
                    fixed = TRUE
                  )
                ),
              
              
              volume =
                as.numeric(
                  gsub(
                    ",",
                    "",
                    as.character(
                      x$accumulatedTradingVolume
                    ),
                    fixed = TRUE
                  )
                ),
              
              
              stringsAsFactors =
                FALSE
              
            )
            
          }
        )
        
      )
      
      
      
      ##########################################################
      # 데이터 정리
      ##########################################################
      
      df <- df %>%
        
        filter(
          !is.na(date),
          !is.na(close),
          close > 0
        ) %>%
        
        filter(
          date >= start_date,
          date <= end_date
        ) %>%
        
        distinct(
          ticker,
          date,
          .keep_all = TRUE
        ) %>%
        
        arrange(
          ticker,
          date
        )
      
      
      
      if (
        nrow(df) == 0
      ) {
        
        stop(
          "유효한 네이버 일별 가격 데이터 없음"
        )
        
      }
      
      
      df
      
      
    }, error = function(e) {
      
      
      last_error <<-
        conditionMessage(e)
      
      
      NULL
      
      
    })
    
    
    
    if (
      !is.null(result)
    ) {
      
      return(
        result
      )
      
    }
    
    
    
    if (
      attempt < max_retry
    ) {
      
      Sys.sleep(
        retry_wait
      )
      
    }
    
  }
  
  
  
  stop(
    paste0(
      code,
      " 네이버 일별 시세 조회 최종 실패: ",
      last_error
    )
  )
  
}



############################################################
# 3. 가격 DB 저장 / 업데이트
############################################################

save_prices_to_db <- function(
    con,
    ticker,
    years_back = 3
) {
  
  
  code <- clean_naver_code(
    ticker
  )
  
  
  df <- get_naver_daily_prices(
    
    ticker =
      code,
    
    years_back =
      years_back
    
  )
  
  
  if (
    nrow(df) == 0
  ) {
    
    stop(
      "가져온 데이터가 없습니다: ",
      code
    )
    
  }
  
  
  
  df <- df %>%
    
    mutate(
      
      date =
        as.character(
          date
        ),
      
      updated_at =
        as.character(
          Sys.time()
        )
      
    )
  
  
  
  dbBegin(
    con
  )
  
  
  tryCatch({
    
    
    for (
      i in seq_len(
        nrow(df)
      )
    ) {
      
      
      dbExecute(
        
        con,
        
        "
        INSERT OR REPLACE INTO stock_daily_prices

        (
          ticker,
          date,
          open,
          high,
          low,
          close,
          volume,
          updated_at
        )

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
    
    
    
    dbCommit(
      con
    )
    
    
    cat(
      code,
      " 저장 완료: ",
      nrow(df),
      "건\n"
    )
    
    
  }, error = function(e) {
    
    
    dbRollback(
      con
    )
    
    
    cat(
      "[오류]",
      code,
      "저장 실패 → 롤백 완료:",
      conditionMessage(e),
      "\n"
    )
    
    
    stop(e)
    
  })
  
}



############################################################
# DB 최신 데이터 확인 / 업데이트
############################################################

ensure_price_updated <- function(
    con,
    ticker,
    years_back = 3
) {
  
  
  code <- clean_naver_code(
    ticker
  )
  
  
  info <- dbGetQuery(
    
    con,
    
    "
    SELECT
      COUNT(*) AS n,
      MAX(date) AS last_date

    FROM stock_daily_prices

    WHERE ticker = ?
    ",
    
    
    params = list(
      code
    )
    
  )
  
  
  n <-
    info$n[1]
  
  
  last_date <-
    info$last_date[1]
  
  
  
  ############################################################
  # DB에 데이터가 없으면 전체 다운로드
  ############################################################
  
  if (
    n == 0 ||
    is.na(last_date) ||
    is.null(last_date)
  ) {
    
    
    cat(
      code,
      " 데이터가 없어 새로 다운로드합니다.\n"
    )
    
    
    save_prices_to_db(
      
      con,
      
      code,
      
      years_back =
        years_back
      
    )
    
    
    return(
      invisible(TRUE)
    )
    
  }
  
  
  
  last_date <-
    as.Date(
      last_date
    )
  
  
  today <-
    Sys.Date()
  
  
  
  cat(
    
    code,
    
    " DB 데이터: ",
    
    n,
    
    "건 | 마지막 날짜: ",
    
    as.character(
      last_date
    ),
    
    "\n",
    
    sep = ""
    
  )
  
  
  
  ############################################################
  # 기존 로직 유지
  #
  # 마지막 저장 날짜가 오늘 이전이면
  # 네이버 데이터를 다시 확인
  #
  # INSERT OR REPLACE 방식이라
  # 기존 데이터는 유지되고 최신 자료만 덮어씀
  ############################################################
  
  if (
    last_date < today
  ) {
    
    
    cat(
      code,
      " 최신 시세를 확인하여 DB를 업데이트합니다.\n"
    )
    
    
    save_prices_to_db(
      
      con,
      
      code,
      
      years_back =
        years_back
      
    )
    
    
  } else {
    
    
    cat(
      code,
      " DB가 오늘 날짜 기준 최신 상태입니다.\n"
    )
    
  }
  
  
  invisible(TRUE)
  
}



############################################################
# DB에서 가격 가져오기
############################################################

get_price_history <- function(
    con,
    ticker
) {
  
  
  code <- clean_naver_code(
    ticker
  )
  
  
  data <- dbGetQuery(
    
    con,
    
    "
    SELECT *

    FROM stock_daily_prices

    WHERE ticker = ?

    ORDER BY date
    ",
    
    
    params = list(
      code
    )
    
  )
  
  
  if (
    nrow(data) == 0
  ) {
    
    stop(
      code,
      " 데이터가 DB에 없습니다."
    )
    
  }
  
  
  data
  
}



############################################################
# 4. ETF 데이터 준비
############################################################

tickers <- c(
  
  "379800",   # KODEX S&P500
  
  "379810"    # KODEX NASDAQ100
  
)



for (
  ticker in tickers
) {
  
  
  ensure_price_updated(
    
    con,
    
    ticker,
    
    years_back = 3
    
  )
  
}



sp500 <- get_price_history(
  
  con,
  
  "379800"
  
) %>%
  
  transmute(
    
    Date =
      as.Date(
        date
      ),
    
    SP500_Close =
      close
    
  )



nasdaq <- get_price_history(
  
  con,
  
  "379810"
  
) %>%
  
  transmute(
    
    Date =
      as.Date(
        date
      ),
    
    NASDAQ_Close =
      close
    
  )



############################################################
# 5. PMS 파일 읽기
############################################################

pms <- read_csv(
  
  "c:/PMS_Core/output_sum.csv",
  
  show_col_types =
    FALSE
  
) %>%
  
  mutate(
    
    Date =
      as.Date(
        Date
      ),
    
    Sum =
      as.numeric(
        Sum
      ),
    
    Profit =
      as.numeric(
        Profit
      )
    
  ) %>%
  
  arrange(
    Date
  ) %>%
  
  mutate(
    
    Base_Profit =
      first(
        Profit
      ),
    
    Invested =
      Sum -
      Profit,
    
    Sum_Adjusted =
      Sum -
      Base_Profit,
    
    Profit_Adjusted =
      Profit -
      Base_Profit
    
  )



############################################################
# 6. PMS + ETF 결합
############################################################

bench <- pms %>%
  
  left_join(
    
    sp500,
    
    by = "Date"
    
  ) %>%
  
  left_join(
    
    nasdaq,
    
    by = "Date"
    
  ) %>%
  
  arrange(
    Date
  ) %>%
  
  mutate(
    
    SP500_Close =
      zoo::na.locf(
        SP500_Close,
        na.rm = FALSE
      ),
    
    NASDAQ_Close =
      zoo::na.locf(
        NASDAQ_Close,
        na.rm = FALSE
      )
    
  ) %>%
  
  filter(
    
    !is.na(
      SP500_Close
    ),
    
    !is.na(
      NASDAQ_Close
    )
    
  )



############################################################
# 투자 현금흐름
############################################################

bench <- bench %>%
  
  mutate(
    
    Invest_Flow =
      Invested -
      lag(
        Invested,
        default = 0
      ),
    
    Invest_Flow =
      pmax(
        Invest_Flow,
        0
      )
    
  )



############################################################
# 동일 현금흐름으로 ETF 투자 가정
############################################################

bench <- bench %>%
  
  mutate(
    
    SP500_Units =
      Invest_Flow /
      SP500_Close,
    
    
    NASDAQ_Units =
      Invest_Flow /
      NASDAQ_Close,
    
    
    SP500_Total_Units =
      cumsum(
        replace_na(
          SP500_Units,
          0
        )
      ),
    
    
    NASDAQ_Total_Units =
      cumsum(
        replace_na(
          NASDAQ_Units,
          0
        )
      ),
    
    
    SP500_Benchmark =
      SP500_Total_Units *
      SP500_Close,
    
    
    NASDAQ_Benchmark =
      NASDAQ_Total_Units *
      NASDAQ_Close,
    
    
    PMS_Return_Real =
      Sum /
      Invested -
      1,
    
    
    PMS_Return_Adjusted =
      Sum_Adjusted /
      Invested -
      1,
    
    
    SP500_Return =
      SP500_Benchmark /
      Invested -
      1,
    
    
    NASDAQ_Return =
      NASDAQ_Benchmark /
      Invested -
      1,
    
    
    PMS_vs_SP500_Real =
      Sum -
      SP500_Benchmark,
    
    
    PMS_vs_NASDAQ_Real =
      Sum -
      NASDAQ_Benchmark,
    
    
    PMS_vs_SP500_Adjusted =
      Sum_Adjusted -
      SP500_Benchmark,
    
    
    PMS_vs_NASDAQ_Adjusted =
      Sum_Adjusted -
      NASDAQ_Benchmark
    
  )



############################################################
# 7. 성과 함수
############################################################

calc_mdd <- function(x) {
  
  
  drawdown <-
    x /
    cummax(x) -
    1
  
  
  abs(
    min(
      drawdown,
      na.rm = TRUE
    )
  )
  
}



calc_sharpe <- function(
    x,
    rf_daily = 0.03 / 252
) {
  
  
  r <-
    x /
    lag(x) -
    1
  
  
  r <-
    r[
      !is.na(r)
    ]
  
  
  if (
    length(r) < 2 ||
    sd(
      r,
      na.rm = TRUE
    ) == 0
  ) {
    
    return(
      NA_real_
    )
    
  }
  
  
  excess <-
    r -
    rf_daily
  
  
  # 기존 프로그램과 동일:
  # annualize 하지 않은 일간 Sharpe
  
  mean(
    excess,
    na.rm = TRUE
  ) /
    sd(
      excess,
      na.rm = TRUE
    )
  
}



############################################################
# 8. PMS vs ETF 성과표
############################################################

perf_df <- bench %>%
  
  select(
    
    Date,
    
    PMS =
      Sum_Adjusted,
    
    SP500 =
      SP500_Benchmark,
    
    NASDAQ =
      NASDAQ_Benchmark
    
  ) %>%
  
  filter(
    
    PMS > 0,
    
    SP500 > 0,
    
    NASDAQ > 0
    
  )



result <- data.frame(
  
  Strategy = c(
    
    "PMS",
    
    "KODEX S&P500",
    
    "KODEX NASDAQ100"
    
  ),
  
  
  Final_Return = round(
    
    c(
      
      tail(
        perf_df$PMS,
        1
      ) /
        first(
          perf_df$PMS
        ) -
        1,
      
      
      tail(
        perf_df$SP500,
        1
      ) /
        first(
          perf_df$SP500
        ) -
        1,
      
      
      tail(
        perf_df$NASDAQ,
        1
      ) /
        first(
          perf_df$NASDAQ
        ) -
        1
      
    ) *
      100,
    
    2
    
  ),
  
  
  MDD = round(
    
    c(
      
      calc_mdd(
        perf_df$PMS
      ),
      
      calc_mdd(
        perf_df$SP500
      ),
      
      calc_mdd(
        perf_df$NASDAQ
      )
      
    ) *
      100,
    
    2
    
  ),
  
  
  Sharpe = round(
    
    c(
      
      calc_sharpe(
        perf_df$PMS
      ),
      
      calc_sharpe(
        perf_df$SP500
      ),
      
      calc_sharpe(
        perf_df$NASDAQ
      )
      
    ),
    
    2
    
  )
  
)


print(
  result
)



############################################################
# 9. 성과 막대그래프
############################################################

# 기존 코드의 빈 section 유지



############################################################
# 10. 100 기준 누적 성과 비교
############################################################

# 기존 코드의 빈 section 유지



############################################################
# 11. 3개월 Rolling 비교
#
# 완전한 3개월 구간만 계산
############################################################

rolling_months <-
  3



rolling_base <- perf_df %>%
  
  arrange(
    Date
  ) %>%
  
  mutate(
    
    Date =
      as.Date(
        Date
      )
    
  )



max_data_date <-
  
  max(
    rolling_base$Date,
    na.rm = TRUE
  )



# 마지막 시작일은
# 최종 데이터일 - 3개월

last_start_date <-
  
  max_data_date %m-%
  months(
    rolling_months
  )



rolling_base_valid <-
  
  rolling_base %>%
  
  filter(
    Date <= last_start_date
  )



rolling_list <- lapply(
  
  seq_len(
    nrow(
      rolling_base_valid
    )
  ),
  
  function(i) {
    
    
    start_date <-
      
      as.Date(
        rolling_base_valid$Date[i]
      )
    
    
    target_end_date <-
      
      start_date %m+%
      months(
        rolling_months
      )
    
    
    tmp <- rolling_base %>%
      
      filter(
        
        Date >= start_date,
        
        Date <= target_end_date
        
      )
    
    
    # 데이터가 너무 적으면 제외
    if (
      nrow(tmp) < 20
    ) {
      
      return(NULL)
      
    }
    
    
    data.frame(
      
      Start_Date =
        start_date,
      
      
      End_Date =
        max(
          tmp$Date
        ),
      
      
      PMS_Return =
        tail(
          tmp$PMS,
          1
        ) /
        first(
          tmp$PMS
        ) -
        1,
      
      
      SP500_Return =
        tail(
          tmp$SP500,
          1
        ) /
        first(
          tmp$SP500
        ) -
        1,
      
      
      NASDAQ_Return =
        tail(
          tmp$NASDAQ,
          1
        ) /
        first(
          tmp$NASDAQ
        ) -
        1,
      
      
      PMS_MDD =
        calc_mdd(
          tmp$PMS
        ),
      
      
      SP500_MDD =
        calc_mdd(
          tmp$SP500
        ),
      
      
      NASDAQ_MDD =
        calc_mdd(
          tmp$NASDAQ
        ),
      
      
      PMS_Sharpe =
        calc_sharpe(
          tmp$PMS
        ),
      
      
      SP500_Sharpe =
        calc_sharpe(
          tmp$SP500
        ),
      
      
      NASDAQ_Sharpe =
        calc_sharpe(
          tmp$NASDAQ
        )
      
    )
    
  }
  
)



rolling_result <-
  
  bind_rows(
    rolling_list
  )



############################################################
# Rolling Long Format
############################################################

rolling_return_long <- rolling_result %>%
  
  select(
    
    Start_Date,
    
    PMS =
      PMS_Return,
    
    SP500 =
      SP500_Return,
    
    NASDAQ =
      NASDAQ_Return
    
  ) %>%
  
  pivot_longer(
    
    cols =
      -Start_Date,
    
    names_to =
      "Strategy",
    
    values_to =
      "Return"
    
  )



rolling_mdd_long <- rolling_result %>%
  
  select(
    
    Start_Date,
    
    PMS =
      PMS_MDD,
    
    SP500 =
      SP500_MDD,
    
    NASDAQ =
      NASDAQ_MDD
    
  ) %>%
  
  pivot_longer(
    
    cols =
      -Start_Date,
    
    names_to =
      "Strategy",
    
    values_to =
      "MDD"
    
  )



rolling_sharpe_long <- rolling_result %>%
  
  select(
    
    Start_Date,
    
    PMS =
      PMS_Sharpe,
    
    SP500 =
      SP500_Sharpe,
    
    NASDAQ =
      NASDAQ_Sharpe
    
  ) %>%
  
  pivot_longer(
    
    cols =
      -Start_Date,
    
    names_to =
      "Strategy",
    
    values_to =
      "Sharpe"
    
  )



############################################################
# 12. Rolling 그래프
############################################################


############################################################
# 1. PMS 우월 여부 계산
############################################################

rolling_flag <- rolling_result %>%
  
  arrange(
    Start_Date
  ) %>%
  
  mutate(
    
    PMS_Win_Return =
      
      PMS_Return >
      SP500_Return &
      
      PMS_Return >
      NASDAQ_Return,
    
    
    PMS_Win_MDD =
      
      PMS_MDD <
      SP500_MDD &
      
      PMS_MDD <
      NASDAQ_MDD,
    
    
    PMS_Win_Sharpe =
      
      PMS_Sharpe >
      SP500_Sharpe &
      
      PMS_Sharpe >
      NASDAQ_Sharpe,
    
    
    next_date =
      
      lead(
        
        Start_Date,
        
        default =
          max(
            Start_Date
          ) +
          1
        
      )
    
  )



############################################################
# 2. PMS 우월 구간 비율
############################################################

pms_win_rate_return <-
  
  mean(
    rolling_flag$PMS_Win_Return,
    na.rm = TRUE
  )


pms_win_rate_mdd <-
  
  mean(
    rolling_flag$PMS_Win_MDD,
    na.rm = TRUE
  )


pms_win_rate_sharpe <-
  
  mean(
    rolling_flag$PMS_Win_Sharpe,
    na.rm = TRUE
  )



############################################################
# 3. 회색 음영 구간
############################################################

pms_win_return <- rolling_flag %>%
  
  filter(
    PMS_Win_Return
  ) %>%
  
  transmute(
    
    xmin =
      as.Date(
        Start_Date
      ),
    
    xmax =
      as.Date(
        next_date
      ),
    
    ymin =
      -Inf,
    
    ymax =
      Inf
    
  )



pms_win_mdd <- rolling_flag %>%
  
  filter(
    PMS_Win_MDD
  ) %>%
  
  transmute(
    
    xmin =
      as.Date(
        Start_Date
      ),
    
    xmax =
      as.Date(
        next_date
      ),
    
    ymin =
      -Inf,
    
    ymax =
      Inf
    
  )



pms_win_sharpe <- rolling_flag %>%
  
  filter(
    PMS_Win_Sharpe
  ) %>%
  
  transmute(
    
    xmin =
      as.Date(
        Start_Date
      ),
    
    xmax =
      as.Date(
        next_date
      ),
    
    ymin =
      -Inf,
    
    ymax =
      Inf
    
  )



############################################################
# 4. 각 지표별 최대 격차
############################################################

max_gap_return <- rolling_result %>%
  
  mutate(
    
    Gap =
      
      pmax(
        PMS_Return,
        SP500_Return,
        NASDAQ_Return,
        na.rm = TRUE
      ) -
      
      pmin(
        PMS_Return,
        SP500_Return,
        NASDAQ_Return,
        na.rm = TRUE
      )
    
  ) %>%
  
  slice_max(
    
    Gap,
    
    n = 1,
    
    with_ties = FALSE
    
  )



max_gap_mdd <- rolling_result %>%
  
  mutate(
    
    Gap =
      
      pmax(
        PMS_MDD,
        SP500_MDD,
        NASDAQ_MDD,
        na.rm = TRUE
      ) -
      
      pmin(
        PMS_MDD,
        SP500_MDD,
        NASDAQ_MDD,
        na.rm = TRUE
      )
    
  ) %>%
  
  slice_max(
    
    Gap,
    
    n = 1,
    
    with_ties = FALSE
    
  )



max_gap_sharpe <- rolling_result %>%
  
  mutate(
    
    Gap =
      
      pmax(
        PMS_Sharpe,
        SP500_Sharpe,
        NASDAQ_Sharpe,
        na.rm = TRUE
      ) -
      
      pmin(
        PMS_Sharpe,
        SP500_Sharpe,
        NASDAQ_Sharpe,
        na.rm = TRUE
      )
    
  ) %>%
  
  slice_max(
    
    Gap,
    
    n = 1,
    
    with_ties = FALSE
    
  )



############################################################
# 5. 최대 격차 라벨
############################################################

label_return <- rolling_return_long %>%
  
  filter(
    
    Start_Date ==
      max_gap_return$Start_Date
    
  ) %>%
  
  mutate(
    
    Label =
      paste0(
        
        Strategy,
        
        ": ",
        
        percent(
          Return,
          accuracy = 0.1
        )
        
      )
    
  )



label_mdd <- rolling_mdd_long %>%
  
  filter(
    
    Start_Date ==
      max_gap_mdd$Start_Date
    
  ) %>%
  
  mutate(
    
    Label =
      paste0(
        
        Strategy,
        
        ": ",
        
        percent(
          MDD,
          accuracy = 0.1
        )
        
      )
    
  )



label_sharpe <- rolling_sharpe_long %>%
  
  filter(
    
    Start_Date ==
      max_gap_sharpe$Start_Date
    
  ) %>%
  
  mutate(
    
    Label =
      paste0(
        
        Strategy,
        
        ": ",
        
        round(
          Sharpe,
          2
        )
        
      )
    
  )



############################################################
# 6. Return 그래프
############################################################

p_return <- ggplot(
  
  rolling_return_long,
  
  aes(
    
    x =
      Start_Date,
    
    y =
      Return,
    
    color =
      Strategy
    
  )
  
) +
  
  geom_rect(
    
    data =
      pms_win_return,
    
    aes(
      
      xmin =
        xmin,
      
      xmax =
        xmax,
      
      ymin =
        ymin,
      
      ymax =
        ymax
      
    ),
    
    inherit.aes =
      FALSE,
    
    fill =
      "gray70",
    
    alpha =
      0.25
    
  ) +
  
  geom_line(
    linewidth = 1.1
  ) +
  
  geom_vline(
    
    xintercept =
      max_gap_return$Start_Date,
    
    linetype =
      "dashed",
    
    alpha =
      0.5
    
  ) +
  
  geom_point(
    
    data =
      label_return,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        Return,
      
      color =
        Strategy
      
    ),
    
    size =
      3
    
  ) +
  
  geom_text(
    
    data =
      label_return,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        Return,
      
      label =
        Label,
      
      color =
        Strategy
      
    ),
    
    hjust =
      -0.05,
    
    vjust =
      -0.5,
    
    size =
      3.5,
    
    show.legend =
      FALSE
    
  ) +
  
  labs(
    
    title =
      paste0(
        
        "① 수익률 비교 : 3개월 Rolling 수익률 | PMS 우월구간: ",
        
        percent(
          pms_win_rate_return,
          accuracy = 0.1
        ),
        
        " | 최대격차: ",
        
        percent(
          max_gap_return$Gap,
          accuracy = 0.1
        )
        
      ),
    
    subtitle =
      "회색 구간: PMS 수익률이 S&P500과 NASDAQ100보다 모두 높은 시작일",
    
    x =
      NULL,
    
    y =
      "Return",
    
    color =
      NULL
    
  ) +
  
  scale_y_continuous(
    
    labels =
      percent_format(
        accuracy = 1
      )
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    legend.position =
      "top"
    
  )



############################################################
# 7. MDD 그래프
############################################################

p_mdd <- ggplot(
  
  rolling_mdd_long,
  
  aes(
    
    x =
      Start_Date,
    
    y =
      MDD,
    
    color =
      Strategy
    
  )
  
) +
  
  geom_rect(
    
    data =
      pms_win_mdd,
    
    aes(
      
      xmin =
        xmin,
      
      xmax =
        xmax,
      
      ymin =
        ymin,
      
      ymax =
        ymax
      
    ),
    
    inherit.aes =
      FALSE,
    
    fill =
      "gray70",
    
    alpha =
      0.25
    
  ) +
  
  geom_line(
    linewidth = 1.1
  ) +
  
  geom_vline(
    
    xintercept =
      max_gap_mdd$Start_Date,
    
    linetype =
      "dashed",
    
    alpha =
      0.5
    
  ) +
  
  geom_point(
    
    data =
      label_mdd,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        MDD,
      
      color =
        Strategy
      
    ),
    
    size =
      3
    
  ) +
  
  geom_text(
    
    data =
      label_mdd,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        MDD,
      
      label =
        Label,
      
      color =
        Strategy
      
    ),
    
    hjust =
      -0.05,
    
    vjust =
      -0.5,
    
    size =
      3.5,
    
    show.legend =
      FALSE
    
  ) +
  
  labs(
    
    title =
      paste0(
        
        "② 최대낙폭 비교 : 3개월 Rolling MDD | PMS 우월구간: ",
        
        percent(
          pms_win_rate_mdd,
          accuracy = 0.1
        ),
        
        " | 최대격차: ",
        
        percent(
          max_gap_mdd$Gap,
          accuracy = 0.1
        )
        
      ),
    
    subtitle =
      "회색 구간: PMS MDD가 S&P500과 NASDAQ100보다 모두 낮은 시작일",
    
    x =
      NULL,
    
    y =
      "MDD",
    
    color =
      NULL
    
  ) +
  
  scale_y_continuous(
    
    labels =
      percent_format(
        accuracy = 1
      )
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    legend.position =
      "none"
    
  )



############################################################
# 8. Sharpe 그래프
############################################################

p_sharpe <- ggplot(
  
  rolling_sharpe_long,
  
  aes(
    
    x =
      Start_Date,
    
    y =
      Sharpe,
    
    color =
      Strategy
    
  )
  
) +
  
  geom_rect(
    
    data =
      pms_win_sharpe,
    
    aes(
      
      xmin =
        xmin,
      
      xmax =
        xmax,
      
      ymin =
        ymin,
      
      ymax =
        ymax
      
    ),
    
    inherit.aes =
      FALSE,
    
    fill =
      "gray70",
    
    alpha =
      0.25
    
  ) +
  
  geom_line(
    linewidth = 1.1
  ) +
  
  geom_vline(
    
    xintercept =
      max_gap_sharpe$Start_Date,
    
    linetype =
      "dashed",
    
    alpha =
      0.5
    
  ) +
  
  geom_point(
    
    data =
      label_sharpe,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        Sharpe,
      
      color =
        Strategy
      
    ),
    
    size =
      3
    
  ) +
  
  geom_text(
    
    data =
      label_sharpe,
    
    aes(
      
      x =
        Start_Date,
      
      y =
        Sharpe,
      
      label =
        Label,
      
      color =
        Strategy
      
    ),
    
    hjust =
      -0.05,
    
    vjust =
      -0.5,
    
    size =
      3.5,
    
    show.legend =
      FALSE
    
  ) +
  
  labs(
    
    title =
      paste0(
        
        "③ 샤프지수 비교 : 3개월 Rolling Sharpe | PMS 우월구간: ",
        
        percent(
          pms_win_rate_sharpe,
          accuracy = 0.1
        ),
        
        " | 최대격차: ",
        
        round(
          max_gap_sharpe$Gap,
          2
        )
        
      ),
    
    subtitle =
      "회색 구간: PMS Sharpe가 S&P500과 NASDAQ100보다 모두 높은 시작일",
    
    x =
      "시작일",
    
    y =
      "Sharpe",
    
    color =
      NULL
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    legend.position =
      "none"
    
  )



############################################################
# 9. 세 그래프 한 장으로 결합
############################################################

combined_plot <-
  
  p_return /
  p_mdd /
  p_sharpe +
  
  plot_layout(
    
    heights =
      c(
        1,
        1,
        1
      )
    
  ) +
  
  plot_annotation(
    
    title =
      "PMS vs S&P500/NASDAQ100 Benchmark",
    
    subtitle =
      paste0(
        "3개월 Rolling 기준: 수익률, MDD, Sharpe 비교",
        "(각 시작일마다 향후 3개월 동안 투자했을 때 성과)"
      ),
    
    theme =
      theme(
        
        plot.title =
          element_text(
            
            size =
              18,
            
            face =
              "bold"
            
          ),
        
        plot.subtitle =
          element_text(
            
            size =
              12
            
          )
        
      )
    
  )



print(
  combined_plot
)



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
    
    as.character(
      max_gap_return$Start_Date
    ),
    
    as.character(
      max_gap_mdd$Start_Date
    ),
    
    as.character(
      max_gap_sharpe$Start_Date
    )
    
  ),
  
  
  Gap = c(
    
    percent(
      max_gap_return$Gap,
      accuracy = 0.1
    ),
    
    percent(
      max_gap_mdd$Gap,
      accuracy = 0.1
    ),
    
    round(
      max_gap_sharpe$Gap,
      2
    )
    
  )
  
)



print(
  max_gap_summary
)



############################################################
# 마지막 상태 출력
############################################################

cat(
  "\nBenchmark 최종 데이터일:",
  as.character(
    max(
      bench$Date,
      na.rm = TRUE
    )
  ),
  "\n"
)


cat(
  "KODEX S&P500 DB 최종일:",
  as.character(
    max(
      sp500$Date,
      na.rm = TRUE
    )
  ),
  "\n"
)


cat(
  "KODEX NASDAQ100 DB 최종일:",
  as.character(
    max(
      nasdaq$Date,
      na.rm = TRUE
    )
  ),
  "\n"
)



############################################################
# DB 연결 종료
############################################################

if (
  DBI::dbIsValid(con)
) {
  
  dbDisconnect(
    con
  )
  
}



############################################################
# 프로그램 후기
############################################################

# 처음에는 단순한 호기심에서 시작되었다.
# 내가 몇 년 동안 만들고 실제로 운용해 온 PMS가
# 과연 시장을 이기고 있는가,
# 아니면 그저 스스로 만족하는 체계에 불과한가 하는 질문이었다.
#
# PMS의 투자원금 흐름을 그대로 따라가며
# 동일한 현금흐름을 S&P500 ETF와 NASDAQ100 ETF에
# 투자했다고 가정하고 성과를 비교한다.
#
# 단순 수익률뿐 아니라 MDD와 Sharpe,
# 그리고 3개월 Rolling 성과를 함께 비교함으로써
# 수익률과 위험을 동시에 살펴본다.
#
# 중요한 질문은
#
# "어떤 전략이 최고인가?"
#
# 보다는
#
# "어떤 미래가 와도 내가 버틸 수 있는 구조인가?"
#
# 에 더 가깝다.
