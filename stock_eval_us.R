# quantmod패키지의 getSymbols()함수가 안되는 경우가 있어 네이버 크롤링으로 수정(2026.03.19)
# SSL에러로 인해 웹조회방식으로 바꿈(2026.05.11)
# 네이버 증권홈페이지 개편으로 조회방식 수정(2026.09.11)

library(quantmod)
library(writexl)
library(dplyr)
library(tidyverse)
library(httr)
library(readr)
library(purrr)
library(stringr)
library(jsonlite)
library(ggplot2)


# ============================================================
# 오늘 날짜
# ============================================================

today <- format(Sys.Date(), "%Y-%m-%d")


# ============================================================
# input_stock_us.csv 읽기
# ============================================================

# # 깃허브에 저장된 주식 정보를 가져오는 경우
#
# url <- paste0(
#   "https://raw.githubusercontent.com/",
#   "shbang-cmd/stock_eval/main/input_stock_us.csv"
# )
#
# data_en <- read_csv(
#   url,
#   comment = "#",
#   locale = locale(encoding = "UTF-8"),
#   show_col_types = FALSE
# )


# 로컬 하드디스크에서 읽기

full_path <- normalizePath(
  file.path(
    getwd(),
    "input_stock_us.csv"
  ),
  winslash = "/",
  mustWork = FALSE
)


data_en <- read_csv(
  full_path,
  comment = "#",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)


# 실제 미국주식 보유 행 수
n_stock <- nrow(data_en)


# ============================================================
# 출력 파일
# ============================================================

output_file <- paste0(
  "output_stock_us_",
  today,
  ".xlsx"
)


if (file.exists(output_file)) {
  
  file.remove(output_file)
  
}


# ============================================================
# 미국주식 현재가
#
# Yahoo Finance JSON API
#
# 예:
#
# https://query1.finance.yahoo.com/v8/finance/chart/SPY
#
# meta$regularMarketPrice 사용
#
# 일시적인 통신 오류를 고려하여 최대 3회 재시도
# ============================================================

get_us_price <- function(
    symbol,
    max_retry = 3,
    retry_wait = 1
) {
  
  symbol <- trimws(symbol)
  
  
  if (
    is.na(symbol) ||
    symbol == ""
  ) {
    
    warning(
      "빈 미국주식 ticker"
    )
    
    return(NA_real_)
  }
  
  
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    URLencode(
      symbol,
      reserved = TRUE
    ),
    "?range=5d&interval=1d&includePrePost=false"
  )
  
  
  last_error <- NULL
  
  
  for (attempt in 1:max_retry) {
    
    
    price <- tryCatch({
      
      
      res <- httr::GET(
        
        url,
        
        httr::add_headers(
          
          `User-Agent` =
            paste0(
              "Mozilla/5.0 ",
              "(Windows NT 10.0; Win64; x64) ",
              "AppleWebKit/537.36"
            ),
          
          Accept =
            "application/json,text/plain,*/*"
          
        ),
        
        
        # 회사 PC SSL 인증서 문제 대응
        httr::config(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        ),
        
        
        httr::timeout(10)
        
      )
      
      
      httr::stop_for_status(res)
      
      
      txt <- httr::content(
        res,
        as = "text",
        encoding = "UTF-8"
      )
      
      
      obj <- jsonlite::fromJSON(
        txt,
        simplifyVector = FALSE
      )
      
      
      # -----------------------------------------------
      # Yahoo 응답 확인
      # -----------------------------------------------
      
      result <- obj$chart$result
      
      
      if (
        is.null(result) ||
        length(result) == 0
      ) {
        
        stop(
          paste0(
            "Yahoo에서 ticker를 찾을 수 없음: ",
            symbol
          )
        )
      }
      
      
      meta <- result[[1]]$meta
      
      
      # -----------------------------------------------
      # 1순위:
      # regularMarketPrice
      # -----------------------------------------------
      
      p <- meta$regularMarketPrice
      
      
      # -----------------------------------------------
      # regularMarketPrice가 없으면
      # 실제 chart close 중 가장 최근 값 사용
      # -----------------------------------------------
      
      if (
        is.null(p) ||
        length(p) == 0 ||
        is.na(p)
      ) {
        
        
        quote_data <-
          result[[1]]$indicators$quote[[1]]$close
        
        
        quote_data <- unlist(
          quote_data
        )
        
        
        quote_data <- quote_data[
          !is.na(quote_data)
        ]
        
        
        if (length(quote_data) > 0) {
          
          p <- tail(
            quote_data,
            1
          )
          
        }
        
      }
      
      
      # -----------------------------------------------
      # 그래도 가격 없으면 실패
      # -----------------------------------------------
      
      if (
        is.null(p) ||
        length(p) == 0 ||
        is.na(p)
      ) {
        
        stop(
          paste0(
            "현재가 수신 실패: ",
            symbol
          )
        )
      }
      
      
      p <- as.numeric(p)
      
      
      if (
        is.na(p) ||
        p <= 0
      ) {
        
        stop(
          paste0(
            "비정상 가격: ",
            symbol
          )
        )
      }
      
      
      p
      
      
    }, error = function(e) {
      
      
      last_error <<- e$message
      
      
      NA_real_
      
      
    })
    
    
    # 정상 수신
    if (!is.na(price)) {
      
      return(price)
      
    }
    
    
    # 재시도
    if (attempt < max_retry) {
      
      Sys.sleep(
        retry_wait
      )
      
    }
    
  }
  
  
  warning(
    sprintf(
      "Yahoo 가격 조회 최종 실패: %s (%s)",
      symbol,
      last_error
    )
  )
  
  
  NA_real_
  
}



# ============================================================
# USD/KRW 환율
#
# 새 네이버 JSON API
#
# https://api.stock.naver.com/marketindex/exchange/FX_USDKRW
#
# 주요 항목:
#
# exchangeInfo$closePrice
# exchangeInfo$fluctuations
# exchangeInfo$fluctuationsRatio
# ============================================================

get_usdkrw_naver <- function(
    max_retry = 3,
    retry_wait = 1
) {
  
  
  url <-
    "https://api.stock.naver.com/marketindex/exchange/FX_USDKRW"
  
  
  last_error <- NULL
  
  
  for (attempt in 1:max_retry) {
    
    
    result <- tryCatch({
      
      
      res <- httr::GET(
        
        url,
        
        httr::add_headers(
          
          `User-Agent` =
            paste0(
              "Mozilla/5.0 ",
              "(Windows NT 10.0; Win64; x64) ",
              "AppleWebKit/537.36"
            ),
          
          Accept =
            "application/json,text/plain,*/*"
          
        ),
        
        
        httr::config(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        ),
        
        
        httr::timeout(10)
        
      )
      
      
      httr::stop_for_status(res)
      
      
      txt <- httr::content(
        res,
        as = "text",
        encoding = "UTF-8"
      )
      
      
      obj <- jsonlite::fromJSON(
        txt,
        simplifyVector = FALSE
      )
      
      
      info <- obj$exchangeInfo
      
      
      if (is.null(info)) {
        
        stop(
          "exchangeInfo 없음"
        )
        
      }
      
      
      # -----------------------------------------------
      # 환율
      # -----------------------------------------------
      
      rate_text <- info$closePrice
      
      
      rate <- as.numeric(
        gsub(
          ",",
          "",
          rate_text,
          fixed = TRUE
        )
      )
      
      
      if (
        is.na(rate) ||
        rate <= 0
      ) {
        
        stop(
          "USD/KRW 환율 파싱 실패"
        )
        
      }
      
      
      # -----------------------------------------------
      # 전일대비
      # -----------------------------------------------
      
      diff_text <- info$fluctuations
      
      
      if (
        is.null(diff_text) ||
        is.na(diff_text)
      ) {
        
        diff_text <- NA_character_
        
      }
      
      
      list(
        
        rate = rate,
        
        diff = diff_text,
        
        pct = info$fluctuationsRatio,
        
        traded_at = info$localTradedAt
        
      )
      
      
    }, error = function(e) {
      
      
      last_error <<- e$message
      
      
      NULL
      
      
    })
    
    
    if (!is.null(result)) {
      
      return(result)
      
    }
    
    
    if (attempt < max_retry) {
      
      Sys.sleep(
        retry_wait
      )
      
    }
    
  }
  
  
  stop(
    paste0(
      "네이버 USD/KRW 환율 조회 최종 실패: ",
      last_error
    )
  )
  
}



# ============================================================
# S&P500
#
# 새 네이버 해외지수 JSON API
#
# Reuters Code
#
# .INX = S&P500
#
# 반환할 값:
#
# spx_value
# spx_diff
# spx_diff_label
# spx_pct
# ============================================================

get_spx_naver <- function(
    max_retry = 3,
    retry_wait = 1
) {
  
  
  url <- paste0(
    "https://stock.naver.com/",
    "api/polling/worldstock/index",
    "?reutersCodes=.INX"
  )
  
  
  last_error <- NULL
  
  
  # ----------------------------------------------------------
  # JSON 안에서 .INX 자료를 재귀적으로 찾는 함수
  #
  # 네이버가 response wrapper 구조를 조금 바꿔도
  # reutersCode=.INX 객체만 찾으면 동작하도록 함
  # ----------------------------------------------------------
  
  find_spx_record <- function(x) {
    
    
    if (!is.list(x)) {
      
      return(NULL)
      
    }
    
    
    if (
      !is.null(x$reutersCode) &&
      identical(
        as.character(x$reutersCode),
        ".INX"
      )
    ) {
      
      return(x)
      
    }
    
    
    for (item in x) {
      
      
      if (is.list(item)) {
        
        
        found <- find_spx_record(item)
        
        
        if (!is.null(found)) {
          
          return(found)
          
        }
        
      }
      
    }
    
    
    NULL
    
  }
  
  
  
  for (attempt in 1:max_retry) {
    
    
    result <- tryCatch({
      
      
      res <- httr::GET(
        
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
            "https://stock.naver.com/"
          
        ),
        
        
        httr::config(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        ),
        
        
        httr::timeout(10)
        
      )
      
      
      httr::stop_for_status(res)
      
      
      txt <- httr::content(
        res,
        as = "text",
        encoding = "UTF-8"
      )
      
      
      obj <- jsonlite::fromJSON(
        txt,
        simplifyVector = FALSE
      )
      
      
      # .INX에 해당하는 객체 탐색
      spx_data <- find_spx_record(
        obj
      )
      
      
      if (is.null(spx_data)) {
        
        stop(
          "S&P500(.INX) 자료를 JSON에서 찾지 못함"
        )
        
      }
      
      
      # -----------------------------------------------
      # 현재 S&P500
      # -----------------------------------------------
      
      price_text <- spx_data$closePrice
      
      
      price <- as.numeric(
        gsub(
          ",",
          "",
          price_text,
          fixed = TRUE
        )
      )
      
      
      if (
        is.na(price) ||
        price <= 0
      ) {
        
        stop(
          "S&P500 현재가 파싱 실패"
        )
        
      }
      
      
      # -----------------------------------------------
      # 전일 대비
      # -----------------------------------------------
      
      diff_text <-
        spx_data$compareToPreviousClosePrice
      
      
      pct_text <-
        spx_data$fluctuationsRatio
      
      
      if (
        is.null(diff_text) ||
        is.na(diff_text)
      ) {
        
        diff_text <- NA_character_
        
      }
      
      
      if (
        is.null(pct_text) ||
        is.na(pct_text)
      ) {
        
        pct_text <- NA_character_
        
      }
      
      
      list(
        
        spx_value =
          price,
        
        spx_diff =
          diff_text,
        
        spx_diff_label =
          diff_text,
        
        spx_pct =
          pct_text,
        
        market_status =
          spx_data$marketStatus,
        
        traded_at =
          spx_data$localTradedAt
        
      )
      
      
    }, error = function(e) {
      
      
      last_error <<- e$message
      
      
      NULL
      
      
    })
    
    
    if (!is.null(result)) {
      
      return(result)
      
    }
    
    
    if (attempt < max_retry) {
      
      Sys.sleep(
        retry_wait
      )
      
    }
    
  }
  
  
  stop(
    paste0(
      "네이버 S&P500 조회 최종 실패: ",
      last_error
    )
  )
  
}



# ============================================================
# 미국주식 평가 계산 준비
# ============================================================

tickername <- character(n_stock)

security <- character(n_stock)

current_price <- numeric(n_stock)

amount <- numeric(n_stock)

profits <- numeric(n_stock)



# ============================================================
# 미국주식 현재가 수신
# ============================================================

for (i in 1:n_stock) {
  
  
  tickername[i] <-
    as.character(
      data_en$종목명[i]
    )
  
  
  symbol <-
    as.character(
      data_en$종목번호[i]
    )
  
  
  security[i] <-
    as.character(
      data_en$보유증권사[i]
    )
  
  
  purchase_price <-
    data_en$매수가격[i]
  
  
  quantity <-
    data_en$수량[i]
  
  
  # Yahoo 실시간/최근 현재가
  current_price[i] <-
    get_us_price(
      symbol
    )
  
  
  # 평가액
  amount[i] <-
    current_price[i] *
    quantity
  
  
  # 수익금
  profits[i] <-
    (
      current_price[i] -
        purchase_price
    ) *
    quantity
  
  
  Sys.sleep(0.5)
  
}



# ============================================================
# 안전장치
#
# 미국주식 가격 하나라도 조회 실패하면
# 잘못된 PMS 자료를 만들지 않고 중단
# ============================================================

if (anyNA(current_price)) {
  
  
  failed_stocks <-
    data_en$종목명[
      is.na(current_price)
    ]
  
  
  failed_codes <-
    data_en$종목번호[
      is.na(current_price)
    ]
  
  
  fail_msg <- paste0(
    
    failed_stocks,
    
    "(",
    
    failed_codes,
    
    ")",
    
    collapse = ", "
    
  )
  
  
  stop(
    paste0(
      "\n",
      "============================================\n",
      "미국주식 시세 수신 실패\n",
      "============================================\n",
      "\n",
      "stock_eval_us.R 실행을 중단합니다.\n",
      "\n",
      "실패 종목: ",
      fail_msg,
      "\n\n",
      "잘못된 평가금/자산비중 파일은 생성하지 않았습니다.\n"
    )
  )
  
}



# ============================================================
# 평가 데이터 계산
# ============================================================

data_en$종목명 <-
  tickername


data_en$보유증권사 <-
  security


data_en$현재가 <-
  current_price


data_en$평가금 <-
  amount



# ============================================================
# 전체 평가액 / 수익금
# ============================================================

total_sum <- sum(
  amount
)


total_profit <- sum(
  profits
)



# ============================================================
# 비중 및 수익률
# ============================================================

stock_ratio <-
  data_en$평가금 /
  total_sum


stock_profit_ratio <-
  profits /
  (
    data_en$평가금 -
      profits
  )


data_en$비중 <-
  stock_ratio


data_en$수익금 <-
  profits


data_en$수익률 <-
  stock_profit_ratio



# ============================================================
# 평가금 순 정렬
# ============================================================

data_en <- data_en %>%
  
  arrange(
    desc(평가금)
  )



# ============================================================
# USD 기준 합계 행
# ============================================================

summary_row <- data.frame(
  
  종목명 =
    paste(
      "(",
      today,
      "USD 합계",
      ")"
    ),
  
  종목번호 =
    NA,
  
  보유증권사 =
    NA,
  
  매수가격 =
    NA,
  
  수량 =
    NA,
  
  현재가 =
    NA,
  
  평가금 =
    total_sum,
  
  비중 =
    sum(
      stock_ratio
    ),
  
  수익금 =
    total_profit,
  
  수익률 =
    total_profit /
    (
      total_sum -
        total_profit
    )
  
)


data <- rbind(
  data_en,
  summary_row
)



# ============================================================
# 네이버 USD/KRW 환율
# ============================================================

fx <- get_usdkrw_naver()


exchange_rate <-
  fx$rate


exchange_diff <-
  fx$diff



# ============================================================
# 원화 환산 합계 행
# ============================================================

summary_row_en <- data.frame(
  
  종목명 =
    paste(
      "(",
      "환율",
      exchange_rate,
      "적용시 KRW 기준",
      ")"
    ),
  
  종목번호 =
    NA,
  
  보유증권사 =
    NA,
  
  매수가격 =
    NA,
  
  수량 =
    NA,
  
  현재가 =
    NA,
  
  평가금 =
    total_sum *
    exchange_rate,
  
  비중 =
    NA,
  
  수익금 =
    total_profit *
    exchange_rate,
  
  수익률 =
    total_profit /
    (
      total_sum -
        total_profit
    )
  
)


data <- rbind(
  data,
  summary_row_en
)



# ============================================================
# Excel 저장
# ============================================================

writexl::write_xlsx(
  data,
  output_file
)



# ============================================================
# 그래프용 데이터
# ============================================================

data_en_output <- data



# ============================================================
# 증권사별 평가액
# ============================================================

new_data_en <- data_en %>%
  
  group_by(
    보유증권사
  ) %>%
  
  summarize(
    
    sec_tot =
      sum(평가금),
    
    비중 =
      sum(비중),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    desc(sec_tot)
  )


print(
  new_data_en
)



# ============================================================
# 평가금 많은 종목
# ============================================================

new_stock_data <- data_en %>%
  
  arrange(
    desc(평가금)
  ) %>%
  
  select(
    종목명,
    평가금,
    비중
  )


print(
  new_stock_data
)



# ============================================================
# 증권사별 평가액 그래프
# ============================================================

new_data <- data_en %>%
  
  group_by(
    보유증권사
  ) %>%
  
  summarize(
    
    sec_tot =
      sum(평가금),
    
    .groups = "drop"
    
  ) %>%
  
  filter(
    !is.na(보유증권사)
  ) %>%
  
  arrange(
    desc(sec_tot)
  )


print(
  new_data
)


p_sec <- ggplot(
  
  data = new_data,
  
  aes(
    
    x = reorder(
      보유증권사,
      -sec_tot
    ),
    
    y =
      sec_tot /
      1000000
    
  )
  
) +
  
  labs(
    x = "증권사",
    y = "보유액합계(백만$)"
  ) +
  
  geom_col()


print(
  p_sec
)



# ============================================================
# 종목별 평가액
#
# summary 행을 이용하지 않고
# 원본 미국주식 data_en만 이용하므로
# 예전처럼 첫 번째 행 2번 삭제할 필요 없음
# ============================================================

new_data <- data_en %>%
  
  group_by(
    종목명
  ) %>%
  
  summarize(
    
    종목평가합산 =
      sum(평가금),
    
    합산수량 =
      sum(수량),
    
    수익금합산 =
      sum(수익금),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    desc(종목평가합산)
  )


new_data$rate <-
  new_data$종목평가합산 /
  sum(
    new_data$종목평가합산
  )



# ============================================================
# 미국주식 종목별 평가금 그래프
# ============================================================

p_us <- ggplot(
  
  new_data,
  
  aes(
    
    x = reorder(
      종목명,
      -종목평가합산
    ),
    
    y =
      종목평가합산 /
      1000000,
    
    fill =
      수익금합산 /
      종목평가합산
    
  )
  
) +
  
  scale_x_discrete(
    guide =
      guide_axis(
        angle = 30
      )
  ) +
  
  geom_text(
    
    aes(
      
      label =
        round(
          종목평가합산 /
            sum(
              종목평가합산
            ),
          2
        )
      
    ),
    
    vjust = -0.1
    
  ) +
  
  geom_col() +
  
  scale_fill_gradient2(
    
    low = "red",
    
    high = "blue",
    
    midpoint = 0
    
  ) +
  
  labs(
    
    title =
      "미국 주식 종목별 평가금(단위:백만$, 그래프위 숫자는 비중)"
    
  )


print(
  p_us
)



# ============================================================
# 완료 메시지
# ============================================================

print(
  paste0(
    n_stock,
    "개 미국종목의 수익금 계산이 완료되었습니다. 결과는 ",
    output_file,
    " 에 저장되었습니다."
  )
)



# ============================================================
# S&P500
#
# 기존:
#
# finance.naver.com/world/sise.naver?symbol=SPI@SPX
# read_html(..., encoding="EUC-KR")
#
# 폐기
#
# 새 Naver JSON API 사용
# ============================================================

spx <- get_spx_naver()



# ============================================================
# 기존 다른 PMS 코드와의 호환을 위해
#
# 다음 이름 그대로 유지
#
# spx$spx_value
# spx$spx_diff
# spx$spx_diff_label
# spx$spx_pct
# ============================================================


cat(
  "\nS&P500 지수 :",
  spx$spx_value,
  
  "\n전일대비 :",
  spx$spx_diff_label,
  
  "\n일간변동률 :",
  spx$spx_pct,
  "%\n"
)


cat(
  "\nUSD/KRW 환율 :",
  exchange_rate,
  
  "\n환율 전일대비 :",
  exchange_diff,
  
  "\n"
)
