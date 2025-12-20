# ============================================================
# 한국주식: 네이버 실시간 현재가 수집 + 평가/수익 계산 + 엑셀 저장
# 결과물: log/output_stock_YYYY-MM-DD.xlsx
# ============================================================

today <- format(Sys.Date(), "%Y-%m-%d")

log_dir <- "log"
if (!dir.exists(log_dir)) dir.create(log_dir)

output_xlsx <- file.path(log_dir, paste0("output_stock_", today, ".xlsx"))

if (file.exists(output_xlsx)) file.remove(output_xlsx)


# 2) 입력 데이터 로딩 (중요: GitHub raw URL 사용) ----

# (절대 X) :  https://github.com/.../blob/.../input_stock.csv# 
# 이렇게 :   https://raw.githubusercontent.com/.../master/input_stock.csv

url_ko <- "https://raw.githubusercontent.com/shbang-cmd/PMS_Core/master/input_stock.csv"

data <- read_csv(
  url_ko,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# 안전장치
if (nrow(data) == 0) stop("input_stock.csv 로딩 결과가 0행입니다. raw URL/파일 내용을 확인하세요.")


# 3) 네이버 실시간 현재가 함수(재시도 포함) ----
get_price_naver <- function(ticker, retry = 3, sleep_sec = 0.15) {
  
  # ① .KS, .KQ 제거
  code <- gsub("\\.K[QS]$", "", ticker)
  # ② 숫자만 추출
  code <- gsub("\\D", "", code)
  
  # ③ 6자리 zero-padding
  if (nchar(code) == 0) {
    warning(sprintf("가격 조회 실패: %s (Invalid ticker format)", ticker))
    return(NA_real_)
  }
  code <- sprintf("%06d", as.numeric(code))
  
  url_naver <- paste0("https://finance.naver.com/item/sise.naver?code=", code)
  
  for (k in 1:retry) {
    price <- tryCatch({
      res <- httr::GET(
        url_naver,
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
        ),
        httr::timeout(10)
      )
      
      # 네이버가 일시적으로 막거나 응답이 비정상이면 실패 처리
      if (httr::status_code(res) != 200) stop("HTTP ", httr::status_code(res))
      
      html <- read_html(res)
      
      # 여러 후보 셀렉터 시도 (네이버 DOM 변동 대응)
      node <- html_node(html, css = "#_nowVal")
      if (is.null(node)) {
        node <- html_node(html, xpath = '//*[@id="_nowVal"]')
      }
      if (is.null(node)) {
        node <- html_node(html, xpath = '//*[@id="chart_area"]//p[contains(@class,"no_today")]//span[contains(@class,"blind")]')
      }
      if (is.null(node)) stop("가격 노드 탐색 실패")
      
      txt <- html_text(node, trim = TRUE)
      txt <- gsub(",", "", txt, fixed = TRUE)
      p <- suppressWarnings(as.numeric(txt))
      if (is.na(p)) stop("가격 파싱 실패")
      p
    }, error = function(e) {
      NA_real_
    })
    
    if (!is.na(price)) return(price)
    
    # 재시도 전 잠깐 쉼 (차단/빈값 완화)
    Sys.sleep(sleep_sec)
  }
  
  warning(sprintf("가격 조회 실패: %s (retries exhausted)", ticker))
  NA_real_
}

# 4) 계산용 벡터 미리 할당(0길이 문제 원천차단) ----
n <- nrow(data)
tickername    <- rep(NA_character_, n) # 길이가 n이고 모든 원소가 문자형 결측치(NA)로 채워진 벡터를 미리 만드는" 동작을 수행
security      <- rep(NA_character_, n)
current_price <- rep(NA_real_, n)
amount        <- rep(NA_real_, n)
profits       <- rep(NA_real_, n)

# 5) 루프: 실시간 가격 + 평가/수익 계산 ----
for (i in 1:n) {
  tickername[i] <- as.character(data$종목명[i])
  symbol        <- as.character(data$종목번호[i])
  security[i]   <- as.character(data$보유증권사[i])
  
  # 쉼표 포함 숫자/문자 타입까지 안전 처리
  purchase_price <- readr::parse_number(as.character(data$매수가격[i]))
  quantity       <- readr::parse_number(as.character(data$수량[i]))
  
  # 실시간 현재가
  current_price[i] <- get_price_naver(symbol)
  
  # 현재가가 NA면 계산도 NA로 둠
  if (is.na(current_price[i]) || is.na(quantity) || is.na(purchase_price)) {
    amount[i]  <- NA_real_
    profits[i] <- NA_real_
  } else {
    amount[i]  <- current_price[i] * quantity
    profits[i] <- (current_price[i] - purchase_price) * quantity
  }
}

# 6) 결과 컬럼 반영 ----
data$종목명     <- tickername
data$보유증권사 <- security
data$현재가     <- current_price
data$평가금     <- amount

total_sum    <- sum(amount, na.rm = TRUE)
total_profit <- sum(profits, na.rm = TRUE)

# 0으로 나누기 방지
if (is.na(total_sum) || total_sum <= 0) stop("총평가금(total_sum)이 0 이하입니다. 현재가 수집/수량/가격을 확인하세요.")

data$비중   <- data$평가금 / total_sum
data$수익금 <- profits

# 수익률 = 수익금 / 매수원금  (매수원금 = 평가금 - 수익금)
base_cost <- (data$평가금 - data$수익금)
data$수익률 <- ifelse(is.na(base_cost) | base_cost == 0, NA_real_, data$수익금 / base_cost)

data <- data %>% arrange(desc(평가금))

# 합계행 추가
summary_row <- data.frame(
  종목명     = paste0("(", today, " 합계)"),
  종목번호   = NA,
  보유증권사 = NA,
  매수가격   = NA,
  수량       = NA,
  현재가     = NA,
  평가금     = total_sum,
  비중       = sum(data$비중, na.rm = TRUE),
  수익금     = total_profit,
  수익률     = ifelse((total_sum - total_profit) == 0, NA_real_, total_profit / (total_sum - total_profit))
)

data_out <- rbind(data, summary_row)

# 7) 엑셀 저장 ----
wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeData(wb, sheet = "Sheet 1", x = data_out)

conditionalFormatting(
  wb, sheet = "Sheet 1",
  cols = 7:10,
  rows = 2:(nrow(data_out) + 1),
  type = "databar",
  showValue = TRUE
)

setColWidths(wb, "Sheet 1", cols = 1:ncol(data_out), widths = "auto")
saveWorkbook(wb, file = output_xlsx, overwrite = TRUE)

cat(nrow(data_out) - 1, "개 국내 종목 네이버 실시간 시세수신 및 계산 완료.\n",
    "XLSX:", output_xlsx, "\n")

# 8) 시각화(증권사별 보유액) ----
# 합계행/NA 제거
plot_data <- data_out %>%
  filter(!is.na(보유증권사), !is.na(평가금)) %>%
  group_by(보유증권사) %>%
  summarize(sec_tot = sum(평가금), .groups = "drop") %>%
  arrange(desc(sec_tot))

p <- ggplot(plot_data, aes(x = reorder(보유증권사, -sec_tot), y = sec_tot / 1000000)) +
  labs(x = "증권사", y = "보유액합계(백만원)") +
  geom_text(aes(label = round(sec_tot / 1000000, 1)), vjust = -0.1) +
  geom_col()

print(p)

