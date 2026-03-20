# quantmod패키지의 getSymbols()함수가 안되는 경우가 있어 네이버 크롤링으로 수정(2026.03.19)

library(quantmod)
library(writexl)
library(dplyr)
library(tidyverse)
library(rvest)
library(httr)
library(readr)
library(purrr)
library(stringr)

# # SSL 인증서 검증을 끕니다 (0 또는 FALSE)
# set_config(config(ssl_verifypeer = 0L))

# 오늘의 날짜 문자열 생성
today <- format(Sys.Date(), "%Y-%m-%d")

# # 깃허브에 저장된 주식 정보를 가져오는 경우(public repository)
# 파일 형식 : raw.githubusercontent.com/{사용자아이디}/{프로젝트명}/main/{파일명}
# url <- "https://raw.githubusercontent.com/shbang-cmd/stock_eval/main/input_stock_us.csv"
# data_en <- read_csv(url, comment = "#", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# 로컬하드에 저장된 input_stock.csv 를 가져오는 경우
full_path <- normalizePath(file.path(getwd(), "input_stock_us.csv"), winslash = "/", mustWork = FALSE)

data_en <- read_csv(full_path,
                    comment = "#",  # 맨앞이 #으로 시작하면 무시함
                    locale = locale(encoding = "UTF-8"),
                    show_col_types = FALSE)


output_file <- paste(paste("output_stock_us_", today, sep = ""), ".xlsx", sep = "") # 출력파일명 뒤에 날짜삽입

# Check its existence
if (file.exists(output_file)) {
  file.remove(output_file) # 파일이 이미 존재하면 지운다.
}


# 수익금 계산을 위한 빈 벡터 생성
tickername <- NA
security <- NA
current_price <- NA
amount <- NA
profits <- NA

# 주식 정보를 순회하면서 수익금 계산

# quantmod패키지의 getSymbols() 함수이용하는 버전
# for (i in 1:nrow(data_en)) {
#   tickername[i] <- as.character(data_en$종목명[i])
#   symbol <- as.character(data_en$종목번호[i])
#   security[i] <- as.character(data_en$보유증권사[i])
#   purchase_price <- data_en$매수가격[i]
#   quantity <- data_en$수량[i]
# 
#   # 현재 주식 가격 가져오기
#   #getSymbols(symbol, src = "yahoo", from = Sys.Date(), to = Sys.Date())
#   getSymbols(symbol, src = "yahoo", from = Sys.Date()-6, to = Sys.Date()) # 뉴욕과 시차때문에 from Date에서 며칠전 날짜로 설정해줌(오래동안 실행해본 경험에서 나왔음)
# 
#   current_price[i] <- as.numeric(last(get(symbol)[,4])) # symbol 종목의 open, high, low, close 가격에서 4번째 위치한 종가를 가져온다.
# 
#   amount[i] <- current_price[i] * quantity  # 종목별 평가액
# 
#   # 수익금 계산
#   profits[i] <- (current_price[i] - purchase_price) * quantity
#   
#   Sys.sleep(0.5) # 안정성을 위해 약간 delay
# }


# 네이버 크롤링으로 미국주식 주가 받아오는 버전
get_us_price <- function(ticker) {
  library(httr)
  library(jsonlite)
  
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    URLencode(ticker),
    "?range=1d&interval=1d&includePrePost=false"
  )
  
  res <- GET(
    url,
    user_agent("Mozilla/5.0"),
    config(
      ssl_verifypeer = 0L,
      ssl_verifyhost = 0L
    )
  )
  
  if (status_code(res) != 200) {
    stop("요청 실패: 상태코드 ", status_code(res))
  }
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  obj <- fromJSON(txt, simplifyDataFrame = FALSE)
  
  result <- obj$chart$result
  if (is.null(result) || length(result) == 0) {
    stop("티커를 찾을 수 없습니다: ", ticker)
  }
  
  meta <- result[[1]]$meta
  
  # 우선순위: regularMarketPrice -> previousClose
  price <- meta$regularMarketPrice
  if (is.null(price) || is.na(price)) {
    price <- meta$previousClose
  }
  
  if (is.null(price) || is.na(price)) {
    stop("현재가를 가져올 수 없습니다: ", ticker)
  }
  
  return(as.numeric(price))
}


for (i in 1:nrow(data_en)) {
  
  tickername[i] <- as.character(data_en$종목명[i])
  symbol <- as.character(data_en$종목번호[i])
  security[i] <- as.character(data_en$보유증권사[i])
  purchase_price <- data_en$매수가격[i]
  quantity <- data_en$수량[i]
  current_price[i] <- get_us_price(symbol)
  amount[i] <- current_price[i] * quantity  # 종목별 평가
  profits[i] <- (current_price[i] - purchase_price) * quantity  # 수익금 계산
  Sys.sleep(0.5) # 안정성을 위해 약간 delay
  
  
  # tickername[i] <- as.character(data_en$종목명[i])
  # symbol <- as.character(data_en$종목번호[i])
  # security[i] <- as.character(data_en$보유증권사[i])
  # purchase_price <- data_en$매수가격[i]
  # quantity <- data_en$수량[i]
  # 
  # # 현재 주식 가격 가져오기
  # #getSymbols(symbol, src = "yahoo", from = Sys.Date(), to = Sys.Date())
  # getSymbols(symbol, src = "yahoo", from = Sys.Date()-6, to = Sys.Date()) # 뉴욕과 시차때문에 from Date에서 며칠전 날짜로 설정해줌(오래동안 실행해본 경험에서 나왔음)
  # 
  # current_price[i] <- as.numeric(last(get(symbol)[,4])) # symbol 종목의 open, high, low, close 가격에서 4번째 위치한 종가를 가져온다.
  # 
  # amount[i] <- current_price[i] * quantity  # 종목별 평가액
  # 
  # # 수익금 계산
  # profits[i] <- (current_price[i] - purchase_price) * quantity
  # 
  # Sys.sleep(0.5) # 안정성을 위해 약간 delay
}









# 데이터 프레임에 수익금 추가
data_en$종목명 <- tickername
data_en$보유증권사 <- security
data_en$현재가 <- current_price
data_en$평가금 <- amount

total_sum <- sum(amount) # 평가액 합산
total_profit <- sum(profits) # 총 수익금 계산

stock_ratio <- NA
stock_profit_ratio <- NA

for (i in 1:nrow(data_en)) {
  stock_ratio[i] <- (data_en$평가금[i] / total_sum)
  stock_profit_ratio[i] <- (profits[i] / (data_en$평가금[i] - profits[i]))
}

data_en$비중 <- stock_ratio
data_en$수익금 <- profits
data_en$수익률 <- stock_profit_ratio

data_en <- data_en %>% arrange(desc(평가금))

# 오늘의 날짜로 시작하는 행을 추가하고 총 수익금 입력
summary_row <- data.frame(종목명 = paste("(", today, "USD 합계", ")"), 종목번호 = NA, 보유증권사 = NA, 매수가격 = NA, 수량 = NA, 현재가 = NA, 평가금 = total_sum, 비중 = sum(stock_ratio), 수익금 = total_profit, 수익률 = total_profit / (total_sum - total_profit))
data <- rbind(data_en, summary_row)


url <- "https://finance.naver.com/marketindex/"  # 네이버 시장지표 URL

# 웹페이지 가져오기
page <- read_html(url)

naver_finance_values <- page %>%
  html_nodes(".value") %>%
  html_text()

exchange_rate <- as.numeric(gsub(",", "", naver_finance_values[1]))   # 1번째가 환율


# 환율 전일 대비
exchange_diff <- {
  box <- page %>% html_node("#exchangeList .on")
  num <- box %>% html_node(".change") %>% html_text(trim = TRUE)
  cls <- box %>% html_node(".head_info") %>% html_attr("class")
  if (str_detect(cls, "up")) paste0("+", num)
  else if (str_detect(cls, "dn")) paste0("-", num)
  else paste0("±", num)
}

summary_row_en <-NA
summary_row_en <- data.frame(종목명 = paste("( 환율", exchange_rate, "적용시 KRW 기준", ")"), 종목번호 = NA, 보유증권사 = NA, 매수가격 = NA, 수량 = NA, 현재가 = NA, 평가금 = total_sum * exchange_rate, 비중 = NA, 수익금 = total_profit * exchange_rate, 수익률 = total_profit / (total_sum - total_profit))
data <- rbind(data, summary_row_en)

#cat("환율 : ", exchange_rate)

# 결과를 엑셀 파일로 저장
write_xlsx(data, output_file)

#cat(nrow(data)-1, "개 미국종목의 수익금 계산이 완료되었습니다. 결과는", output_file, "에 저장되었습니다.")

data_en <- data
#View(data_en)

# 증권사별 평가액
new_data_en <- data_en %>%
  group_by(보유증권사) %>%
  summarize(sec_tot = sum(평가금), 비중 = sum(비중)) %>%
  arrange(desc(sec_tot))
new_data_en

# 평가금 많은 종목
new_data_en <- data_en %>%
  group_by(평가금) %>%
  summarize(sec_name = 종목명, 비중 = 비중) %>%
  arrange(desc(평가금))
new_data_en



# 아래 통계는 콘솔과 plots창에 표시됨
# 증권사별 평가액
new_data <- data %>%
  group_by(보유증권사) %>%
  summarize(sec_tot = sum(평가금)) %>%
  arrange(desc(sec_tot))
new_data <- new_data %>% filter(!is.na(보유증권사))  # NA 제거
new_data
ggplot(data = new_data, aes(x = reorder(보유증권사, -sec_tot), y = sec_tot/1000000)) +
  labs(x = "증권사", y = "보유액합계(백만)") +
  #geom_text(aes(label=sec_tot/1000000/exchange_rate[-1]), vjust = -0.1) +
  geom_col()


# 종목별 평가액
new_data <- data %>%
  group_by(종목명) %>%
  summarize(종목평가합산 = sum(평가금), 합산수량 = sum(수량), 수익금합산 = sum(수익금)) %>%
  arrange(desc(종목평가합산))
new_data <- new_data[-1,]    # 첫번째 행 제거
new_data <- new_data[-1,]    # 첫번째 행 제거
new_data$rate = new_data$종목평가합산 / sum(new_data$종목평가합산)
#print(new_data, n=30)

p_us <- ggplot(new_data, aes(x = reorder(종목명, -종목평가합산), y = 종목평가합산/1000000, fill=수익금합산/종목평가합산)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  #labs(x = "종목", y = "종목별 합계(백만원)") +
  geom_text(aes(label= round(종목평가합산/sum(종목평가합산), 2) ), vjust = -0.1) +
  geom_col() +
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       midpoint = 0) +
  labs(
    title = "미국 주식 종목별 평가금(단위:백만$, 그래프위 숫자는 비중)"
  )

print(p_us)
print(paste0(nrow(data)-1, "개 미국종목의 수익금 계산이 완료되었습니다. 결과는", output_file, "에 저장되었습니다."))


# S&P500지수를 가져와서 spx 전역변수에 저장(나중에 생성형 인공지능AI에서 벤치마크 분석을 위함)
# get_spx_quantmod <- function() {
#   suppressWarnings(
#     quantmod::getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
#   ) -> spx
# 
#   # 종가 기준
#   close_today <- as.numeric(Cl(spx)[NROW(spx)])
#   close_prev  <- as.numeric(Cl(spx)[NROW(spx) - 1])
# 
#   diff <- close_today - close_prev
#   pct  <- round(diff / close_prev * 100, 2)
# 
#   diff_label <- if (diff > 0) {
#     paste0("+", round(diff, 2))
#   } else if (diff < 0) {
#     paste0("-", round(abs(diff), 2))
#   } else {
#     paste0("±0")
#   }
# 
#   list(
#     spx_value = round(close_today, 2),
#     spx_diff = round(diff, 2),
#     spx_diff_label = diff_label,
#     spx_pct = pct
#   )
# }

# 네이버에서 S&P500지수를 가져오는 버전
# get_spx_naver <- function() {
#   # 1. 네이버 금융 S&P 500 일별 시세 페이지 (심볼: SPI@SPX)
#   url <- "https://finance.naver.com"
#   
#   # 2. 웹 요청 (브라우저인 것처럼 위장하여 차단 방지)
#   res <- GET(url, user_agent("Mozilla/5.0"))
#   
#   if (status_code(res) != 200) {
#     stop("네이버 서버 연결에 실패했습니다.")
#   }
#   
#   # 3. HTML 읽기 (네이버는 EUC-KR 인코딩)
#   doc <- read_html(content(res, as = "text", encoding = "euc-kr"))
#   
#   # 4. 데이터 테이블 추출 (시세가 들어있는 type_1 클래스 테이블 선택)
#   tbl <- doc %>% 
#     html_element("table.type_1") %>% 
#     html_table()
#   
#   # 5. 데이터 전처리 (비어있는 행 제거)
#   spx_tbl <- tbl %>% filter(!is.na(종가) & 종가 != "")
#   
#   if (nrow(spx_tbl) < 2) {
#     stop("데이터를 가져오지 못했습니다. URL을 확인하세요.")
#   }
#   
#   # 6. 숫자 변환 함수 (쉼표 제거 및 숫자화)
#   clean_num <- function(x) as.numeric(gsub(",", "", x))
#   
#   close_today <- clean_num(spx_tbl$종가[1])
#   close_prev  <- clean_num(spx_tbl$종가[2])
#   
#   # 7. 변동폭 및 등락률 계산
#   diff <- close_today - close_prev
#   pct  <- round((diff / close_prev) * 100, 2)
#   
#   diff_label <- if (diff > 0) {
#     paste0("+", format(round(diff, 2), nsmall = 2))
#   } else if (diff < 0) {
#     paste0("-", format(round(abs(diff), 2), nsmall = 2))
#   } else {
#     "±0"
#   }
#   
#   # 8. 결과값 리스트 반환
#   return(list(
#     spx_value = round(close_today, 2),
#     spx_diff = round(diff, 2),
#     spx_diff_label = diff_label,
#     spx_pct = pct
#   ))
# }
# 
# spx <- get_spx_quantmod()




url <- "https://finance.naver.com/world/sise.naver?symbol=SPI@SPX"
page <- read_html(url, encoding = "EUC-KR")

# 현재가
price <- page %>%
  html_node("p.no_today") %>%
  html_text(trim = TRUE) %>%
  str_squish()

# 전일대비 영역 전체
exday_text <- page %>%
  html_node("p.no_exday") %>%
  html_text(trim = TRUE) %>%
  str_squish()

# 등락률: 괄호 안의 xx.xx%
pct_change <- str_extract(exday_text, "[+-]?[0-9.]+%")
pct_change <- str_remove(pct_change, "%")

# 전일대비: 등락률 괄호 앞 숫자
change <- exday_text %>%
  str_remove("\\([^()]*%\\)") %>%
  str_extract("[+-]?[0-9,]+\\.?[0-9]*")

# cat("S&P500 현재가:", price, "\n")
# cat("전일대비:", change, "\n")
# cat("등락률:", pct_change, "\n")

spx$spx_value <- price
spx$spx_diff <- change
spx$spx_diff_label <- paste0(
  str_extract(exday_text, "[+-](?=[0-9.]+%)"),
  str_extract(exday_text, "[0-9,]+\\.?[0-9]*")
)
spx$spx_pct <- pct_change

# 사용 예
# spx <- get_spx_quantmod()
# cat("S&P500 지수 :", spx$spx_value,
#     "(전일대비:", spx$spx_diff_label,
#     ", 일간변동률:", spx$spx_pct, "%)\n")

