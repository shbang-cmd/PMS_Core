# 한국주식은 quantmod 패키지 대신 네이버 주식 홈페이지에서 실시간으로 받아오도록 개선(quantmod 라이브러리는 20분 지연시세라서 국내 주식시장 오픈 이후 20분간은 느리기 때문)
library(rvest)
library(dplyr)
library(readr)
library(openxlsx)
library(scales)
library(ggplot2)
library(tidyverse)
library(quantmod)

today <- format(Sys.Date(), "%Y-%m-%d")

# # 깃허브에 저장된 주식 정보를 가져오는 경우(public repository)
# 파일 형식 : raw.githubusercontent.com/{사용자아이디}/{프로젝트명}/main/{파일명}
# url <- "https://raw.githubusercontent.com/shbang-cmd/stock_eval/main/input_stock.csv"
# data <- read_csv(url, comment = "#", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# 로컬하드에 저장된 input_stock.csv 를 가져오는 경우
full_path <- normalizePath(file.path(getwd(), "input_stock.csv"), winslash = "/", mustWork = FALSE)

data <- read_csv(full_path, 
                 comment = "#",   # 맨앞이 #으로 시작하면 무시함
                 locale = locale(encoding = "UTF-8"), 
                 show_col_types = FALSE)

output_file <- paste0("output_stock_", today, ".xlsx")
if (file.exists(output_file)) file.remove(output_file)

# 네이버에서 실시간 현재가를 가져오는 함수
get_price_naver <- function(ticker) {
  tryCatch({
    # ① .KS, .KQ 제거
    code <- gsub("\\.K[QS]$", "", ticker)
    # ② 숫자만 추출
    code <- gsub("\\D", "", code)
    
    # ③ 6자리 zero-padding (예: "680" → "000680")
    if (nchar(code) > 0) {
      code <- sprintf("%06d", as.numeric(code))
    } else {
      stop("Invalid ticker format")
    }
    
    url  <- paste0("https://finance.naver.com/item/sise.naver?code=", code)
    html <- read_html(httr::GET(url, httr::add_headers(
      "User-Agent" = "Mozilla/5.0"
    )))
    
    node <- html_node(html, css = "#_nowVal")
    if (is.na(node) || length(node) == 0) {
      node <- html_node(html, xpath = '//*[@id="_nowVal"]')
    }
    if (is.na(node) || length(node) == 0) {
      node <- html_node(html, xpath = '//*[@id="chart_area"]//p[contains(@class,"no_today")]//span[contains(@class,"blind")]')
    }
    
    price <- node |>
      html_text() |>
      gsub(",", "", x = _, fixed = TRUE) |>
      as.numeric()
    
    if (is.na(price)) stop("가격 파싱 실패")
    price
  }, error = function(e) {
    warning(sprintf("가격 조회 실패: %s (%s)", ticker, e$message))
    NA_real_
  })
}

# 수익금 계산
tickername <- character()
security <- character()
current_price <- numeric()
amount <- numeric()
profits <- numeric()

for (i in 1:nrow(data)) {
  tickername[i] <- as.character(data$종목명[i])
  symbol <- as.character(data$종목번호[i])
  security[i] <- as.character(data$보유증권사[i])
  purchase_price <- data$매수가격[i]
  quantity <- data$수량[i]
  
  # 🔹 실시간 현재가 가져오기
  current_price[i] <- get_price_naver(symbol)
  
  amount[i] <- current_price[i] * quantity
  profits[i] <- (current_price[i] - purchase_price) * quantity
  
  Sys.sleep(0.5) # 안정성을 위해 약간 delay
}

data$종목명 <- tickername
data$보유증권사 <- security
data$현재가 <- current_price
data$평가금 <- amount

total_sum <- sum(amount, na.rm = TRUE)
total_profit <- sum(profits, na.rm = TRUE)

data$비중 <- data$평가금 / total_sum
data$수익금 <- profits
data$수익률 <- profits / (data$평가금 - profits)

data <- data %>% arrange(desc(평가금))

summary_row <- data.frame(
  종목명 = paste("(", today, "합계", ")"),
  종목번호 = NA,
  보유증권사 = NA,
  매수가격 = NA,
  수량 = NA,
  현재가 = NA,
  평가금 = total_sum,
  비중 = sum(data$비중, na.rm = TRUE),
  수익금 = total_profit,
  수익률 = total_profit / (total_sum - total_profit)
)
data <- rbind(data, summary_row)

# 엑셀 저장
wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeData(wb, sheet = "Sheet 1", data)

conditionalFormatting(wb, sheet = "Sheet 1", cols = 7:10, rows = 2:(nrow(data)+1), type = "databar", showValue = TRUE)
setColWidths(wb, "Sheet 1", cols = 1:ncol(data), widths = "auto")
saveWorkbook(wb, file = output_file, overwrite = TRUE)

cat(nrow(data)-1, "개 국내 종목의 네이버 실시간 시세수신 및 수익금 계산 완료. 결과:", output_file, "\n")

# 시각화 
data_ko <- data
new_data <- data %>%
  group_by(보유증권사) %>%
  summarize(sec_tot = sum(평가금)) %>%
  filter(!is.na(보유증권사)) %>%
  arrange(desc(sec_tot))

p <- ggplot(data = new_data, aes(x = reorder(보유증권사, -sec_tot), y = sec_tot/1000000)) +
  labs(x = "증권사", y = "보유액합계(백만원)") +
  geom_text(aes(label=round(sec_tot/1000000, 1)), vjust = -0.1) +
  geom_col() +
  labs(
    title = "한국주식 증권사별 보유액 합계(단위:백만원)"
  )

print(p)
