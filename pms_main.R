###############################################
# PMS MAIN (No Risk - Clean Start)
# - stock_eval.R / stock_eval_us.R 필요
# - log/ 에 국내/해외 평가파일 생성
# - log/output_sum.csv 누적
# - 그래프 1장 + 종목별 수익 테이블 출력
# - [추가] 전일(휴일 포함 최근 파일) 읽어서 전일대비 표시
###############################################

# ------------------------------------------------------------
# 0) 패키지
# ------------------------------------------------------------
pkg <- c(
  "readr","readxl","dplyr","stringr","scales",
  "openxlsx","ggplot2","xts","PerformanceAnalytics","DT"
)
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(readr); library(readxl); library(dplyr); library(stringr); library(scales)
library(openxlsx); library(ggplot2); library(xts); library(PerformanceAnalytics)
library(DT)

options(scipen = 999)

# ------------------------------------------------------------
# 1) 경로/폴더
# ------------------------------------------------------------
ROOT <- "c:/PMS_Core"
setwd(ROOT)

log_dir <- file.path(ROOT, "log")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

today <- Sys.Date()
date_ymd <- format(today, "%Y-%m-%d")

# ------------------------------------------------------------
# 2) 유틸(방탄)
# ------------------------------------------------------------
msg <- function(...) cat(sprintf(...), "\n")

safe_run <- function(expr) {
  tryCatch(expr, error = function(e) {
    msg("[ERROR] %s", e$message)
    NULL
  })
}

ensure_file <- function(path, context="") {
  if (!file.exists(path)) {
    msg("[STOP] %s 파일 없음: %s", context, path)
    return(FALSE)
  }
  TRUE
}

# [추가] 오늘보다 이전 날짜 중 "가장 최근" 파일 찾기 (휴일 대응)
get_prev_file <- function(log_dir, prefix, today = Sys.Date(), ext = "xlsx") {
  pattern <- paste0("^", prefix, "\\d{4}-\\d{2}-\\d{2}\\.", ext, "$")
  files <- list.files(log_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  
  dates <- as.Date(sub(
    paste0(".*", prefix, "(\\d{4}-\\d{2}-\\d{2})\\.", ext, "$"),
    "\\1",
    files
  ))
  
  idx <- which(!is.na(dates) & dates < today)
  if (length(idx) == 0) return(NA_character_)
  
  files[idx][which.max(dates[idx])]
}

# ------------------------------------------------------------
# 3) 평가 스크립트 실행 (국내/해외)
# ------------------------------------------------------------
msg("\n[%s] 실행 시작 ***********************************************", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

safe_run(source("stock_eval.R", encoding = "UTF-8"))
safe_run(source("stock_eval_us.R", encoding = "UTF-8"))

# stock_eval_us.R 에서 exchange_rate 를 만든다고 하셨지만, 혹시 없으면 방탄
if (!exists("exchange_rate")) {
  msg("[WARN] exchange_rate 변수가 없습니다. US 환산이 필요하면 stock_eval_us.R에서 exchange_rate를 생성하세요. (임시로 1 적용)")
  exchange_rate <- 1
}

file_ko_xlsx <- file.path(log_dir, paste0("output_stock_",    date_ymd, ".xlsx"))
file_us_xlsx <- file.path(log_dir, paste0("output_stock_us_", date_ymd, ".xlsx"))

if (!ensure_file(file_ko_xlsx, "국내") || !ensure_file(file_us_xlsx, "해외")) {
  quit(save="no")
}

# ------------------------------------------------------------
# 4) 오늘 국내/해외 데이터 로드
# ------------------------------------------------------------
data_ko <- read_excel(file_ko_xlsx)
data_us <- read_excel(file_us_xlsx)

data_ko_items <- data_ko %>% head(-1)
data_us_items <- data_us %>% head(-2)

# ------------------------------------------------------------
# 5) 합산 CSV(output_sum.csv) 누적
# ------------------------------------------------------------
get_last <- function(df, col) {
  if (!(col %in% names(df))) stop("컬럼 없음: ", col)
  tail(df[[col]], 1)
}

sum_ko    <- safe_run(get_last(data_ko, "평가금"))
profit_ko <- safe_run(get_last(data_ko, "수익금"))
sum_us    <- safe_run(get_last(data_us, "평가금"))
profit_us <- safe_run(get_last(data_us, "수익금"))

if (any(sapply(list(sum_ko,profit_ko,sum_us,profit_us), is.null))) {
  msg("[STOP] 합계행 컬럼(평가금/수익금) 확인 필요")
  quit(save="no")
}

sum_total    <- round(sum_ko + sum_us, 0)
profit_total <- round(profit_ko + profit_us, 0)

# output_sum_csv <- file.path(log_dir, "output_sum.csv")
# today_row <- data.frame(Date=today, Sum=sum_total, Profit=profit_total)

# if (file.exists(output_sum_csv)) {
#   dd <- read_csv(output_sum_csv,
#                  col_types = cols(Date=col_date(), Sum=col_double(), Profit=col_double()),
#                  show_col_types = FALSE) %>%
#     filter(Date != today) %>%
#     bind_rows(today_row) %>%
#     arrange(Date)
# } else {
#   dd <- today_row
# }

output_sum_csv <- file.path(log_dir, "output_sum.csv")
today_row <- data.frame(Date = as.Date(today), Sum = as.numeric(sum_total), Profit = as.numeric(profit_total))

if (file.exists(output_sum_csv)) {
  dd <- read_csv(output_sum_csv, show_col_types = FALSE)
  
  # ✅ Date 타입 강제 통일 (문자/Date 혼재 방탄)
  dd <- dd %>%
    mutate(
      Date   = as.Date(Date),
      Sum    = as.numeric(Sum),
      Profit = as.numeric(Profit)
    ) %>%
    filter(Date != as.Date(today)) %>%
    bind_rows(today_row) %>%
    arrange(Date)
} else {
  dd <- today_row
}

write_csv(dd, output_sum_csv)
msg("[OK] output_sum.csv 갱신: %s", output_sum_csv)





write_csv(dd, output_sum_csv)
msg("[OK] output_sum.csv 갱신: %s", output_sum_csv)

# ------------------------------------------------------------
# 6) 전체 자산 그래프(평가액 추이) + 수익률 보조축
# ------------------------------------------------------------
dd <- dd %>% mutate(Return = Profit / (Sum - Profit))
sum_left  <- dd$Sum / 10000000
ret_right <- dd$Return * 100

sum_range    <- range(sum_left,  na.rm = TRUE)
return_range <- range(ret_right, na.rm = TRUE)

if (length(unique(ret_right[is.finite(ret_right)])) < 2 || diff(return_range) == 0) {
  a <- 1
  b <- sum_range[1]
} else {
  a <- diff(sum_range) / diff(return_range)
  b <- sum_range[1] - a * return_range[1]
}

plot_title <- paste0("PMS 평가액 추이 (", format(min(dd$Date), "%Y-%m-%d"), " ~ ",
                     format(max(dd$Date), "%Y-%m-%d"), ")")

dd$Date <- as.Date(dd$Date)

p_nav <- ggplot(dd, aes(x = Date)) +
  geom_line(aes(y = sum_left), color = "gray50", linewidth = 1) +
  geom_point(aes(y = sum_left), size = 3) +
  geom_line(aes(y = a * ret_right + b), color = "darkgreen", linewidth = 1) +
  geom_point(aes(y = a * ret_right + b), color = "darkgreen", size = 2) +
  scale_x_date(date_breaks = "2 months", labels = scales::label_date_short()) +
  scale_y_continuous(
    name = "총자산(천만원)",
    sec.axis = sec_axis(~ (. - b) / a, name = "총수익률(%)")
  ) +
  labs(title = plot_title, x = "Date") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_nav)

# ------------------------------------------------------------
# 7) 종목별 테이블(국내+해외) : 수익/수익률/비중
# ------------------------------------------------------------
normalize_items <- function(df, market_label, fx = 1) {
  
  need <- c("종목명","종목번호","보유증권사",
            "매수가격","수량","현재가","평가금","비중","수익금","수익률")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("컬럼 누락: ", paste(miss, collapse=", "))
  
  df %>%
    transmute(
      시장코드 = market_label,     # ⭐ 조인/계산용(HTML 넣기 전)
      종목명 = as.character(종목명),
      종목번호 = as.character(종목번호),
      보유증권사 = as.character(보유증권사),
      
      # 금액은 모두 한화 기준
      매수가격 = readr::parse_number(as.character(매수가격)) * fx,
      수량     = readr::parse_number(as.character(수량)),
      현재가   = readr::parse_number(as.character(현재가)) * fx,
      평가금   = readr::parse_number(as.character(평가금)) * fx,
      수익금   = readr::parse_number(as.character(수익금)) * fx,
      
      비중     = as.numeric(비중),
      수익률   = as.numeric(수익률)
    )
}

rt <- bind_rows(
  safe_run(normalize_items(data_ko_items, "KR", fx = 1)),
  safe_run(normalize_items(data_us_items, "US", fx = exchange_rate))
) %>%
  arrange(desc(평가금))

# 비중 재계산(한화 기준)
total_value_krw <- sum(rt$평가금, na.rm = TRUE)
rt$비중 <- rt$평가금 / total_value_krw

# ------------------------------------------------------------
# 8) [추가] 전일(휴일 포함) 파일 읽어서 전일대비 계산
# ------------------------------------------------------------
prev_ko <- get_prev_file(log_dir, "output_stock_",    today = Sys.Date())
prev_us <- get_prev_file(log_dir, "output_stock_us_", today = Sys.Date())

if (is.na(prev_ko) || is.na(prev_us)) {
  msg("[INFO] 전일 파일을 찾지 못해 전일대비는 NA로 표시됩니다.")
  rt$전일평가금 <- NA_real_
  rt$전일대비  <- NA_real_
} else {
  prev_ko_df <- read_excel(prev_ko) %>% head(-1)
  prev_us_df <- read_excel(prev_us) %>% head(-2)
  
  prev_ko_map <- prev_ko_df %>%
    transmute(
      시장코드 = "KR",
      종목번호 = as.character(종목번호),
      전일평가금 = readr::parse_number(as.character(평가금))
    )
  
  prev_us_map <- prev_us_df %>%
    transmute(
      시장코드 = "US",
      종목번호 = as.character(종목번호),
      전일평가금 = readr::parse_number(as.character(평가금)) * exchange_rate
    )
  
  prev_map <- bind_rows(prev_ko_map, prev_us_map) %>%
    distinct(시장코드, 종목번호, .keep_all = TRUE)
  
  rt <- rt %>%
    left_join(prev_map, by = c("시장코드", "종목번호")) %>%
    mutate(
      전일대비 = 평가금 - 전일평가금
    )
  
  msg("[OK] 전일 파일 사용: KR=%s / US=%s", basename(prev_ko), basename(prev_us))
}

# ------------------------------------------------------------
# 9) 국기 아이콘(시장 표시) - 마지막에(조인 끝난 뒤)
# ------------------------------------------------------------
flag_kr <- "https://flagcdn.com/w20/kr.png"
flag_us <- "https://flagcdn.com/w20/us.png"

rt$시장 <- ifelse(
  rt$시장코드 == "KR",
  sprintf('<img src="%s" style="height:14px; margin-right:6px;"> KR', flag_kr),
  sprintf('<img src="%s" style="height:14px; margin-right:6px;"> US', flag_us)
)

msg("\n[요약] 오늘 총자산: %s 원 / 총수익금: %s 원",
    comma(sum_total), comma(profit_total))

# ------------------------------------------------------------
# 10) DT 출력 (전일대비도 표시 + 음수 빨간색)
# ------------------------------------------------------------
print(
  datatable(
    rt %>% select(-시장코드),   # 시장코드는 내부키라 숨김
    escape = FALSE,
    extensions = 'FixedHeader',
    options = list(
      pageLength = 100,
      autoWidth = TRUE,
      fixedHeader = TRUE,
      dom = 'ftip',
      initComplete = JS(
        "function(settings, json) {
          $(this.api().table().node()).css('width', '100%');
          this.api().columns.adjust();
        }"
      )
    )
  ) %>%
    formatCurrency(
      columns = c("매수가격", "현재가", "평가금", "전일평가금", "전일대비", "수익금"),
      currency = "", mark = ",", digits = 0
    ) %>%
    formatPercentage(columns = c("비중", "수익률"), digits = 2) %>%
    formatStyle(columns = names(rt %>% select(-시장코드)), fontSize = '110%') %>%
    formatStyle(
      columns = '수익금',
      valueColumns = '수익금',
      color = styleInterval(c(0), c('red', 'black'))
    ) %>%
    formatStyle(
      columns = '수익률',
      valueColumns = '수익률',
      color = styleInterval(c(0), c('red', 'black'))
    ) %>%
    formatStyle(
      columns = '전일대비',
      valueColumns = '전일대비',
      color = styleInterval(c(0), c('red', 'black'))
    )
)

msg("\n[%s] 실행 종료 ***********************************************\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
