# gemini에게 미리 만들어 놓은 prompt를 통해 질의하고 답변을 email로 보냄
# 선행작업 : gemini에 무료가입하여 api 키를 받음, gmail 앱 패스워드도 받음, PC의 미리 환경 변수에 저장

pkg <- c("blastula", "httr2", "jsonlite", "glue", "quantmod", "xtx")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
library(blastula)
library(httr2)
library(glue)
library(jsonlite)
library(quantmod)
library(xts)

#source("is_korea_market_open_yahoo.R")

# Yahoo 기반 "오늘(한국) 주식시장 개장 여부" 판단 함수
# - 원리: KOSPI 지수(^KS11) 또는 삼성전자(005930.KS)의
#         최신 거래일(last trading date)이 '오늘'이면 개장일.
# - 장 시작 전(특히 09:00 이전)에는 오늘 데이터가 아직 없을 수 있어
#   기본적으로 "오늘 날짜 == 최신 거래일"을 사용하고,
#   원하시면 'close_only=TRUE'로 "장 마감 후에만 TRUE"처럼 보수화 가능
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

# 거래소 시장이 안열리는날 더 이상 실행안하고 종료
if (!is_korea_market_open_yahoo(close_only = TRUE)) {
  cat("휴장일이라 메일 송신 안합니다.\n")
  quit(save = "no", status = 0) 
  # save = "no"를 설정하지 않으면 시스템이 종료되지 않고 대기 상태에 빠져 전체 자동화 프로세스가 멈추는 리스크가 발생, status = 0 : "임무 성공(Success)"의 의미
}

# ------------------------------------------------------------
# 유틸
# ------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || !nzchar(as.character(x))) y else x
}  # 만약 앞의 값(x)이 없거나 비어 있다면, 뒤의 값(y)을 기본값으로 사용

# ------------------------------------------------------------
# 환경변수 : 미리 PC에 세팅을 해놔야 함
# 검색에서 "환경 변수"라고 입력하면 찾을 수 있음
 # GMAIL_USER : 지메일 주소(예시 : sample@gmail.com)
 # GMAIL_APP_PASSWORD : 지메일 앱 암호
 # GEMINI_API_KEY : 제미나이 키
 # GEMINI_MODEL : 안넣어도 됨
# ------------------------------------------------------------
gmail_user      <- Sys.getenv("GMAIL_USER")
gmail_pw_envvar <- "GMAIL_APP_PASSWORD" 

gemini_key   <- Sys.getenv("GEMINI_API_KEY") %||% ""
gemini_model <- Sys.getenv("GEMINI_MODEL") %||% "gemini-2.5-flash"
# GEMINI_MODEL에 "models/gemini-2.5-flash" 형태여도 자동 처리됨

if (!nzchar(gemini_key)) {
  stop("환경변수 GEMINI_API_KEY가 비어 있습니다.")
}

# ------------------------------------------------------------
# 프롬프트 파일 읽기
# ------------------------------------------------------------
prompt_path <- "C:\\PMS_Core\\reports\\gemini_prompt.txt"
if (!file.exists(prompt_path)) stop("프롬프트 파일이 없습니다: ", prompt_path)

prompt_text <- readLines(prompt_path, encoding = "UTF-8")
prompt_text <- paste(prompt_text, collapse = "\n")
if (!nzchar(prompt_text)) stop("프롬프트 내용이 비어 있습니다.")

# ------------------------------------------------------------
# Gemini 호출 함수
# ------------------------------------------------------------
call_gemini_text <- function(model, api_key, prompt,
                             # max_tokens = 4000,
                             max_tokens = 8000,  # 4000일때보다 길게 출력
                             temperature = 0.7) {
  
  # gemini-2.5-flash 외에도 더 좋은 기능의 무료 모델이 있으며 교체해서 사용
  model_name <- trimws(as.character(model)) %||% "gemini-2.5-flash"
  # model_name <- trimws(as.character(model)) %||% "gemini-1.5-pro"
  api_key    <- trimws(as.character(api_key)) %||% ""
  
  if (!nzchar(api_key)) stop("API 키가 비어 있습니다.")
  
  # models/ 접두사 자동 제거 (404 방지 핵심)
  model_name <- sub("^models/", "", model_name)
  
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model_name,
    ":generateContent"
  )
  
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(parts = list(list(text = prompt)))
      ),
      generationConfig = list(
        maxOutputTokens = as.integer(max_tokens),
        temperature = temperature
      )
    ))
  
  resp <- req_perform(req)
  js   <- resp_body_json(resp, simplifyVector = FALSE)
  
  # 안전한 텍스트 추출
  if (is.null(js$candidates) || length(js$candidates) == 0) {
    stop("Gemini 응답에 candidates가 없습니다.")
  }
  
  parts <- js$candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    stop("Gemini 응답에 content.parts가 없습니다.")
  }
  
  texts <- vapply(parts, function(p) p$text %||% "", FUN.VALUE = character(1))
  result <- paste(texts[nzchar(texts)], collapse = "\n")
  
  if (!nzchar(result)) stop("Gemini가 빈 텍스트를 반환했습니다.")
  
  return(result)
}

# ------------------------------------------------------------
# 실행
# ------------------------------------------------------------
ai_response <- "응답 실패"

tryCatch({
  ai_response <- call_gemini_text(
    model  = gemini_model,
    api_key = gemini_key,
    prompt = prompt_text
  )
  
  cat("\n===== Gemini 응답 =====\n")
  cat(ai_response, "\n")
  
}, error = function(e) {
  cat("\n❌ 에러 발생:\n")
  cat(e$message, "\n")
})

# ------------------------------------------------------------
# PDF 첨부 경로: reports/Daily_Risk_{YYYYMMDD}.pdf
# ------------------------------------------------------------
now_kst <- Sys.time()
yyyymmdd <- format(now_kst, "%Y%m%d")

pdf_path <- sprintf("C:\\PMS_Core\\reports\\Daily_Risk_%s.pdf", yyyymmdd)
if (!file.exists(pdf_path)) stop("첨부할 PDF 파일이 없습니다: ", pdf_path)

# ------------------------------------------------------------
# 메일 작성 + 첨부(add_attachment) + 발송
# ------------------------------------------------------------
cat("메일 작성 시작.\n")
to <- "seminago@naver.com"  # 수신 메일 지정
# 만약 수신인이 2명이상이면 아래 예시처럼 사용
# to <- c("manager1@jsfund.com", "manager2@jsfund.com", "cio@jsfund.com")

subject <- sprintf("[PMS 자산현황 AI보고] %s", format(now_kst, "%Y-%m-%d"))

email <- compose_email(
  body = md(glue("
안녕하세요.

아래는 {format(now_kst, '%Y-%m-%d %H:%M:%S')} 기준, AI 자동 응답 결과입니다.
(첨부: Daily Risk PDF 1건)

---

{ai_response}

---

(본 메일은 자동 발송되었습니다.)
"))
)

# 첨부는 smtp_send 옵션이 아니라, 이메일 객체에 붙입니다.
email <- add_attachment(email, file = pdf_path)

smtp_send(
  email,
  from = gmail_user,
  to = to,
  subject = subject,
  credentials = creds_envvar(
    user = gmail_user,
    provider = "gmail",
    pass_envvar = gmail_pw_envvar
  )
)

message("완료: AI 응답 생성 → PDF 첨부 → Gmail 발송 성공 | to=", to, " | file=", pdf_path)
cat("메일 발송 정상 종료되었습니다..\n")
