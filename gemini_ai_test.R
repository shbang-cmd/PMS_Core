# ============================================================
# Gemini API: 텍스트 응답 수신 전용 (최소 안정 버전)
# ============================================================

pkg <- c("httr2", "jsonlite")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(httr2)
library(jsonlite)

# ------------------------------------------------------------
# 0) 유틸
# ------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || !nzchar(as.character(x))) y else x
}

# ------------------------------------------------------------
# 1) 환경변수
# ------------------------------------------------------------
gemini_key   <- Sys.getenv("GEMINI_API_KEY") %||% ""
gemini_model <- Sys.getenv("GEMINI_MODEL") %||% "gemini-2.5-flash"
# GEMINI_MODEL에 "models/gemini-2.5-flash" 형태여도 자동 처리됨

if (!nzchar(gemini_key)) {
  stop("환경변수 GEMINI_API_KEY가 비어 있습니다.")
}

# ------------------------------------------------------------
# 2) 프롬프트 파일 읽기
# ------------------------------------------------------------
prompt_path <- "C:\\PMS_Core\\reports\\gemini_prompt.txt"
if (!file.exists(prompt_path)) stop("프롬프트 파일이 없습니다: ", prompt_path)

prompt_text <- readLines(prompt_path, encoding = "UTF-8")
prompt_text <- paste(prompt_text, collapse = "\n")
if (!nzchar(prompt_text)) stop("프롬프트 내용이 비어 있습니다.")

# ------------------------------------------------------------
# 3) Gemini 호출 함수
# ------------------------------------------------------------
call_gemini_text <- function(model, api_key, prompt,
                             # max_tokens = 4000,
                             max_tokens = 8000,  # 4000일때보다 길게 출력
                             temperature = 0.7) {
  
  model_name <- trimws(as.character(model)) %||% "gemini-2.5-flash"
  # model_name <- trimws(as.character(model)) %||% "gemini-1.5-pro"
  api_key    <- trimws(as.character(api_key)) %||% ""
  
  if (!nzchar(api_key)) stop("API 키가 비어 있습니다.")
  
  # ✅ models/ 접두사 자동 제거 (404 방지 핵심)
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
# 4) 실행
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

