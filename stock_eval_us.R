pkg = c("quantmod", "readxl", "writexl", "dplyr", "tidyverse", "rvest", "httr")
new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

library(quantmod)
library(writexl)
library(dplyr)
library(tidyverse)
library(rvest)
library(httr)
library(readr)


# 오늘의 날짜 문자열 생성
today <- format(Sys.Date(), "%Y-%m-%d")
#today <- format(Sys.Date()-1, "%Y-%m-%d")

url <- "https://raw.githubusercontent.com/shbang-cmd/stock_eval/main/input_stock_us.csv"

data_en <- read_csv(url, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

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
for (i in 1:nrow(data_en)) {
  tickername[i] <- as.character(data_en$종목명[i])
  symbol <- as.character(data_en$종목번호[i])
  security[i] <- as.character(data_en$보유증권사[i])
  purchase_price <- data_en$매수가격[i]
  quantity <- data_en$수량[i]
  
  # 현재 주식 가격 가져오기
  #getSymbols(symbol, src = "yahoo", from = Sys.Date(), to = Sys.Date())
  getSymbols(symbol, src = "yahoo", from = Sys.Date()-6, to = Sys.Date()) # 뉴욕과 시차때문에 from Date에서 하루 전 날짜로 설정해줌
  
  current_price[i] <- as.numeric(last(get(symbol)[,4])) # symbol 종목의 open, high, low, close 가격에서 4번째 위치한 종가를 가져온다.
  
  amount[i] <- current_price[i] * quantity  # 종목별 평가액
  
  # 수익금 계산
  profits[i] <- (current_price[i] - purchase_price) * quantity
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



#exchange_rate <- c(1, 1330.9)  # 환율 API가 잘 안되어 수동으로 입력

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
  else if (str_detect(cls, "down")) paste0("-", num)
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
                       midpoint = 0)

print(p_us)

print(paste0(nrow(data)-1, "개 미국종목의 수익금 계산이 완료되었습니다. 결과는", output_file, "에 저장되었습니다."))

