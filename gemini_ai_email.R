# ============================================================
# gemini_ai_email.R
# - Gemini로 프롬프트 질의 -> 응답 이메일 발송 + Daily Risk PDF 첨부
# - 선행: GEMINI_API_KEY / GMAIL_USER / GMAIL_APP_PASSWORD 환경변수 설정
#         제미나이에게 물어볼 때는 물어보는 text + 제미나이 API KEY 의 조합이 필요
#         지메일로 이메일을 보낼 때는 지메일 ID + 지메일 API KEY의 조합이 필요
#         (지메일이 아니라 DAUM 메일 SMTP로 보낸다고 해도 앱 key를 미리 받아놔야 함)
# - 운영(배치/Rscript) 안정형
# ============================================================

# ------------------------------------------------------------
# 0) 기본 옵션 (배치에서 CRAN 미러 고정, 타임아웃)
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(timeout = 120)

# ------------------------------------------------------------
# 1) 패키지 로딩 (운영 스크립트에서는 자동 설치 비추천)
#    - 설치는 RStudio에서 1회 수행
# ------------------------------------------------------------
pkg <- c("blastula", "httr2", "jsonlite", "glue", "quantmod", "xts")

library(blastula)

missing <- setdiff(pkg, rownames(installed.packages()))
if (length(missing)) {
  stop(
    "필수 패키지가 설치되어 있지 않습니다: ",
    paste(missing, collapse = ", "),
    "\nRStudio에서 아래를 1회 실행하세요:\n",
    "options(repos=c(CRAN='https://cloud.r-project.org'))\n",
    "install.packages(c(", paste0("'", missing, "'", collapse = ", "), "), dependencies=TRUE)\n"
  )
}

suppressPackageStartupMessages({
  library(blastula)
  library(httr2)
  library(glue)
  library(jsonlite)
  library(quantmod)
  library(xts)
})

# ------------------------------------------------------------
# 2) 유틸
# ------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || !nzchar(as.character(x))) y else x
}

# ------------------------------------------------------------
# 3) Yahoo 기반 "오늘(한국) 주식시장 개장 여부" 판단 함수
# ------------------------------------------------------------
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
    cutoff  <- as.POSIXct(paste(d, sprintf("%02d:%02d:00", close_hour, close_min)), tz = tz)
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
    # Yahoo 장애/네트워크 문제 가능 → 보수적으로 휴장(FALSE)
    return(FALSE)
  }
  
  identical(last1, d)
}

# ------------------------------------------------------------
# 4) 거래소 휴장일이면 종료 (메일 송신 안 함)
# ------------------------------------------------------------
if (!is_korea_market_open_yahoo(close_only = TRUE)) {
  cat("휴장일(또는 장 마감 전/야후 조회 실패)이라 메일 송신 안합니다. Market is closed (or before market close, or Yahoo data unavailable). Skipping email delivery.\n")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------
# 5) 환경변수 로드 및 검증
# ------------------------------------------------------------
gmail_user      <- Sys.getenv("GMAIL_USER") %||% ""
gmail_pw_envvar <- "GMAIL_APP_PASSWORD"

gemini_key   <- Sys.getenv("GEMINI_API_KEY") %||% ""
gemini_model <- Sys.getenv("GEMINI_MODEL") %||% "gemini-2.5-flash"
gemini_model <- sub("^models/", "", trimws(gemini_model))  # models/ 접두사 제거

if (!nzchar(gmail_user)) stop("환경변수 GMAIL_USER가 비어 있습니다.")
if (!nzchar(Sys.getenv(gmail_pw_envvar))) stop("환경변수 GMAIL_APP_PASSWORD가 비어 있습니다.")
if (!nzchar(gemini_key)) stop("환경변수 GEMINI_API_KEY가 비어 있습니다.")

# ------------------------------------------------------------
# 6) 프롬프트 파일 읽기
# ------------------------------------------------------------
prompt_path <- "C:\\PMS_Core\\reports\\gemini_prompt.txt"
if (!file.exists(prompt_path)) stop("프롬프트 파일이 없습니다: ", prompt_path)

prompt_text <- readLines(prompt_path, encoding = "UTF-8")
prompt_text <- paste(prompt_text, collapse = "\n")




if (!nzchar(prompt_text)) stop("프롬프트 내용이 비어 있습니다.")

# ------------------------------------------------------------
# 7) Gemini 호출 함수
# ------------------------------------------------------------
call_gemini_text <- function(model, api_key, prompt, max_tokens = 8000, temperature = 0.7) {
  model_name <- trimws(as.character(model)) %||% "gemini-2.5-flash"
  api_key    <- trimws(as.character(api_key)) %||% ""
  
  if (!nzchar(api_key)) stop("API 키가 비어 있습니다.")
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
      contents = list(list(parts = list(list(text = prompt)))),
      generationConfig = list(
        maxOutputTokens = as.integer(max_tokens),
        temperature = temperature
      )
    ))
  
  resp <- req_perform(req)
  js   <- resp_body_json(resp, simplifyVector = FALSE)
  
  if (is.null(js$candidates) || length(js$candidates) == 0) {
    stop("Gemini 응답에 candidates가 없습니다.")
  }
  
  parts <- js$candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    stop("Gemini 응답에 content.parts가 없습니다.")
  }
  
  texts  <- vapply(parts, function(p) p$text %||% "", FUN.VALUE = character(1))
  result <- paste(texts[nzchar(texts)], collapse = "\n")
  if (!nzchar(result)) stop("Gemini가 빈 텍스트를 반환했습니다.")
  
  result
}

# ------------------------------------------------------------
# 8) 실행: Gemini 응답 생성 (실패해도 메일은 보내되 본문에 실패 로그 포함)
# ------------------------------------------------------------
ai_response <- ""
ai_error    <- ""

tryCatch({
  ai_response <- call_gemini_text(
    model   = gemini_model,
    api_key = gemini_key,
    prompt  = prompt_text
  )
  cat("\n===== Gemini 응답 =====\n")
  cat(ai_response, "\n")
}, error = function(e) {
  ai_error <- e$message %||% "Unknown error"
  ai_response <- paste0(
    "❌ Gemini 응답 생성 실패\n",
    "- Error: ", ai_error, "\n",
    "- 안내: GEMINI_API_KEY/모델명/네트워크를 확인하세요.\n"
  )
  cat("\n❌ 에러 발생:\n")
  cat(ai_error, "\n")
})

# ------------------------------------------------------------
# 9) PDF 첨부 경로: reports/Daily_Risk_{YYYYMMDD}.pdf
# ------------------------------------------------------------
now_kst  <- as.POSIXct(Sys.time(), tz = "Asia/Seoul")
yyyymmdd <- format(now_kst, "%Y%m%d")

pdf_path <- sprintf("C:\\PMS_Core\\reports\\Daily_Risk_%s.pdf", yyyymmdd)
pdf_exists <- file.exists(pdf_path)

# 정책: PDF 없으면 메일은 보내되 본문에 경고 표시 (원하시면 stop으로 바꿔도 됨)
pdf_notice <- ""
if (!pdf_exists) {
  pdf_notice <- paste0(
    "\n\n⚠️ 첨부 PDF 파일이 없습니다.\n",
    "- expected: ", pdf_path, "\n"
  )
}

# ------------------------------------------------------------
# 10) 메일 작성 + 첨부(add_attachment) + 발송
# ------------------------------------------------------------
cat("메일 작성 시작.\n")

to <- "seminago@naver.com"  # 수신 메일 지정 (필요시 벡터로 다중 수신)
# 코드를 따라하시는 위의 수신인 메일주소를 반드시 바꿀 것(소중한 자산 포트폴리오 현황을 필자에게 보고하시는 일이 없도록^^)
                   
subject <- sprintf("[PMS 자산현황 AI보고 %d] %s", floor(tail(read.csv("C:\\PMS_Core\\output_sum.csv")[[2]], 1) / 1000000), format(now_kst, "%Y-%m-%d"))

email <- compose_email(
  body = md(glue("
안녕하세요.

아래는 {format(now_kst, '%Y-%m-%d %H:%M:%S')} (KST) 기준, AI 자동 응답 결과입니다.
(첨부: Daily Risk PDF 1건)

---

{ai_response}{pdf_notice}

---

(본 메일은 자동 발송되었습니다.)
"))
)

# PDF가 있으면 첨부
if (pdf_exists) {
  email <- add_attachment(email, file = pdf_path)
}

# 발송
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

message("완료: AI 응답 생성 → (PDF 있으면 첨부) → Gmail 발송 성공 | to=", to,
        " | pdf=", ifelse(pdf_exists, pdf_path, "NONE"))
cat("메일 발송 정상 종료되었습니다.\n")


# 배치파일로 실행될때는 강제 종료
# 인터렉티브 모드(RStudio 등)가 아닐 때만 R 세션을 종료
if (!interactive()) {
  quit(save = "no")
}

