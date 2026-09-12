# # 한국주식은 quantmod 패키지 대신 네이버 주식 홈페이지에서 실시간으로 받아오도록 개선(quantmod 라이브러리는 20분 지연시세라서 국내 주식시장 오픈 이후 20분간은 느리기 때문)
# ============================================================
# stock_eval.R
#
# 한국주식 현재가:
# 네이버 증권 JSON API에서 실시간 수신
#
# 2026-09-11 수정
# - 기존 finance.naver.com HTML / EUC-KR 크롤링 방식 폐기
# - m.stock.naver.com JSON API 사용
# - 회사 PC 등의 SSL 인증서 문제 대응
# - 한 종목이라도 가격 조회 실패 시 프로그램 중단
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(openxlsx)
library(scales)
library(ggplot2)
library(tidyverse)
library(quantmod)
library(httr)
library(jsonlite)


today <- format(Sys.Date(), "%Y-%m-%d")


# ============================================================
# input_stock.csv 읽기
# ============================================================

# # 깃허브에 저장된 주식 정보를 가져오는 경우(public repository)
# # 파일 형식 :
# # raw.githubusercontent.com/{사용자아이디}/{프로젝트명}/main/{파일명}
#
# url <- "https://raw.githubusercontent.com/shbang-cmd/stock_eval/main/input_stock.csv"
#
# data <- read_csv(
#   url,
#   comment = "#",
#   locale = locale(encoding = "UTF-8"),
#   show_col_types = FALSE
# )


# 로컬하드에 저장된 input_stock.csv 를 가져오는 경우

full_path <- normalizePath(
  file.path(getwd(), "input_stock.csv"),
  winslash = "/",
  mustWork = FALSE
)


data <- read_csv(
  full_path,
  comment = "#",   # 맨앞이 #으로 시작하면 무시함
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)


# ============================================================
# 출력 파일
# ============================================================

output_file <- paste0(
  "output_stock_",
  today,
  ".xlsx"
)


if (file.exists(output_file)) {
  file.remove(output_file)
}


# ============================================================
# 네이버 국내주식 실시간 현재가 함수
#
# 예:
# 005930.KS
#      ↓
# 005930
#      ↓
# https://m.stock.naver.com/api/stock/005930/basic
#
# JSON:
# closePrice = "73,200"
#
#      ↓
# 73200
# ============================================================

get_price_naver <- function(
    ticker,
    max_retry = 3,
    retry_wait = 1
) {
  
  # ----------------------------------------------------------
  # 종목코드 정리
  # ----------------------------------------------------------
  
  # .KS 또는 .KQ 제거
  code <- toupper(
    gsub(
      "\\.K[QS]$",
      "",
      ticker
    )
  )
  
  
  # 영숫자만 남김
  # 우선주 코드 00680K 등도 지원
  code <- gsub(
    "[^0-9A-Z]",
    "",
    code
  )
  
  
  # 국내 종목코드는 기본적으로 6자리
  if (nchar(code) != 6) {
    
    warning(
      sprintf(
        "가격 조회 실패: %s - 잘못된 종목코드(%s)",
        ticker,
        code
      )
    )
    
    return(NA_real_)
  }
  
  
  # ----------------------------------------------------------
  # 네이버 JSON API
  # ----------------------------------------------------------
  
  url <- paste0(
    "https://m.stock.naver.com/api/stock/",
    code,
    "/basic"
  )
  
  
  last_error <- NULL
  
  
  # ----------------------------------------------------------
  # 최대 max_retry 회 재시도
  # ----------------------------------------------------------
  
  for (attempt in 1:max_retry) {
    
    price <- tryCatch({
      
      # ------------------------------------------------------
      # 네이버 요청
      #
      # 회사 PC 보안 프로그램 / 프록시 환경에서 발생하는
      # self-signed certificate 오류 대응을 위해
      # 이 요청에 한해서 SSL 검증 해제
      # ------------------------------------------------------
      
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
            "application/json, text/plain, */*",
          
          Referer =
            "https://m.stock.naver.com/"
        ),
        
        
        httr::config(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        ),
        
        
        httr::timeout(10)
      )
      
      
      # HTTP 오류 확인
      httr::stop_for_status(resp)
      
      
      # ------------------------------------------------------
      # JSON 문자열 읽기
      # ------------------------------------------------------
      
      json_text <- httr::content(
        resp,
        as = "text",
        encoding = "UTF-8"
      )
      
      
      # ------------------------------------------------------
      # JSON 파싱
      # ------------------------------------------------------
      
      json_data <- jsonlite::fromJSON(
        json_text,
        simplifyVector = TRUE
      )
      
      
      # ------------------------------------------------------
      # 현재가 읽기
      # ------------------------------------------------------
      
      price_text <- json_data$closePrice
      
      
      if (
        is.null(price_text) ||
        length(price_text) == 0 ||
        is.na(price_text) ||
        price_text == ""
      ) {
        
        stop(
          "closePrice 항목 없음"
        )
      }
      
      
      # 예:
      #
      # "15,430"
      #
      #       ↓
      #
      # "15430"
      
      price_text <- gsub(
        ",",
        "",
        price_text,
        fixed = TRUE
      )
      
      
      current_price <- suppressWarnings(
        as.numeric(price_text)
      )
      
      
      # ------------------------------------------------------
      # 가격 유효성 확인
      # ------------------------------------------------------
      
      if (
        is.na(current_price) ||
        current_price <= 0
      ) {
        
        stop(
          paste(
            "가격 숫자 변환 실패:",
            price_text
          )
        )
      }
      
      
      # 정상 가격 반환
      current_price
      
      
    }, error = function(e) {
      
      last_error <<- e$message
      
      NA_real_
      
    })
    
    
    # --------------------------------------------------------
    # 정상적으로 가격을 받았으면 즉시 반환
    # --------------------------------------------------------
    
    if (!is.na(price)) {
      
      return(price)
      
    }
    
    
    # --------------------------------------------------------
    # 실패 시 재시도 전 잠시 대기
    # --------------------------------------------------------
    
    if (attempt < max_retry) {
      
      Sys.sleep(
        retry_wait
      )
    }
    
  }
  
  
  # ----------------------------------------------------------
  # 모든 재시도 실패
  # ----------------------------------------------------------
  
  warning(
    sprintf(
      "네이버 가격 조회 최종 실패: %s [%s] (%s)",
      ticker,
      code,
      last_error
    )
  )
  
  
  NA_real_
}



# ============================================================
# 수익금 계산 준비
# ============================================================

tickername <- character()

security <- character()

current_price <- numeric()

amount <- numeric()

profits <- numeric()



# ============================================================
# 종목별 현재가 조회
# ============================================================

for (i in 1:nrow(data)) {
  
  tickername[i] <- as.character(
    data$종목명[i]
  )
  
  
  symbol <- as.character(
    data$종목번호[i]
  )
  
  
  security[i] <- as.character(
    data$보유증권사[i]
  )
  
  
  purchase_price <-
    data$매수가격[i]
  
  
  quantity <-
    data$수량[i]
  
  
  # ----------------------------------------------------------
  # 네이버 실시간 현재가
  # ----------------------------------------------------------
  
  current_price[i] <-
    get_price_naver(
      symbol
    )
  
  
  # ----------------------------------------------------------
  # 평가금액
  # ----------------------------------------------------------
  
  amount[i] <-
    current_price[i] *
    quantity
  
  
  # ----------------------------------------------------------
  # 수익금
  # ----------------------------------------------------------
  
  profits[i] <-
    (
      current_price[i] -
        purchase_price
    ) *
    quantity
  
  
  # ----------------------------------------------------------
  # 네이버 서버 요청 간격
  # ----------------------------------------------------------
  
  Sys.sleep(0.5)
}



# ============================================================
# 중요 안전장치
#
# 한 종목이라도 현재가를 받지 못하면
# 잘못된 자산비중 파일을 만들지 않고 프로그램 중단
# ============================================================

if (anyNA(current_price)) {
  
  
  failed_stocks <-
    data$종목명[
      is.na(current_price)
    ]
  
  
  failed_codes <-
    data$종목번호[
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
      "\n\n",
      "============================================\n",
      "네이버 시세 수신 실패\n",
      "============================================\n",
      "\n",
      "stock_eval.R 실행을 중단합니다.\n",
      "\n",
      "실패 종목:\n",
      fail_msg,
      "\n\n",
      "잘못된 평가금/자산비중 Excel 파일은 생성하지 않았습니다.\n"
    )
  )
}



# ============================================================
# 평가 결과 계산
# ============================================================

data$종목명 <-
  tickername


data$보유증권사 <-
  security


data$현재가 <-
  current_price


data$평가금 <-
  amount



# ============================================================
# 전체 합계
# ============================================================

total_sum <- sum(
  amount
)


total_profit <- sum(
  profits
)



# ============================================================
# 비중
# ============================================================

data$비중 <-
  data$평가금 /
  total_sum



# ============================================================
# 수익금
# ============================================================

data$수익금 <-
  profits



# ============================================================
# 수익률
# ============================================================

data$수익률 <-
  profits /
  (
    data$평가금 -
      profits
  )



# ============================================================
# 평가금액 순 정렬
# ============================================================

data <- data %>%
  
  arrange(
    desc(평가금)
  )



# ============================================================
# 합계 행 추가
# ============================================================

summary_row <- data.frame(
  
  종목명 =
    paste(
      "(",
      today,
      "합계",
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
      data$비중,
      na.rm = TRUE
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
  data,
  summary_row
)



# ============================================================
# Excel 저장
# ============================================================

wb <- createWorkbook()



addWorksheet(
  wb,
  "Sheet 1"
)



writeData(
  wb,
  sheet = "Sheet 1",
  data
)



conditionalFormatting(
  
  wb,
  
  sheet = "Sheet 1",
  
  cols = 7:10,
  
  rows = 2:(nrow(data) + 1),
  
  type = "databar",
  
  showValue = TRUE
)



setColWidths(
  
  wb,
  
  "Sheet 1",
  
  cols = 1:ncol(data),
  
  widths = "auto"
)



saveWorkbook(
  
  wb,
  
  file = output_file,
  
  overwrite = TRUE
)



cat(
  "\n",
  nrow(data) - 1,
  "개 국내 종목의 네이버 실시간 시세수신 및 수익금 계산 완료.",
  "\n",
  "결과:",
  output_file,
  "\n\n"
)



# ============================================================
# 시각화
# ============================================================

data_ko <- data



new_data <- data %>%
  
  group_by(
    보유증권사
  ) %>%
  
  summarize(
    sec_tot = sum(평가금),
    .groups = "drop"
  ) %>%
  
  filter(
    !is.na(보유증권사)
  ) %>%
  
  arrange(
    desc(sec_tot)
  )



p <- ggplot(
  
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
    y = "보유액합계(백만원)"
  ) +
  
  geom_text(
    
    aes(
      
      label =
        round(
          sec_tot /
            1000000,
          1
        )
      
    ),
    
    vjust = -0.1
    
  ) +
  
  geom_col() +
  
  labs(
    
    title =
      "한국주식 증권사별 보유액 합계(단위:백만원)"
    
  )

print(p)
