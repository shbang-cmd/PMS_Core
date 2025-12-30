# chatgpt에게 미리 만들어 놓은 prompt를 통해 문의 하고 답변을 email로 보냄
# chatgpt에 가입하여 api 키를 받아야 하고 gmail 앱 패스워드를 받아 놔야 함

pkg <- c("blastula", "httr2", "jsonlite", "glue")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
library(blastula)
library(httr2)
library(glue)

`%||%` <- function(x, y) if (is.null(x) || !nzchar(as.character(x))) y else x

# ------------------------------------------------------------
# 0) 환경변수
# ------------------------------------------------------------
cat("AI 조회 후 메일발송 루틴 시작합니다.\n")

gmail_user <- Sys.getenv("GMAIL_USER")
gmail_pw_envvar <- "GMAIL_APP_PASSWORD"
openai_key <- Sys.getenv("OPENAI_API_KEY")

openai_model <- Sys.getenv("OPENAI_MODEL")
if (!nzchar(openai_model)) openai_model <- "gpt-4.1-mini"

if (!nzchar(gmail_user)) stop("GMAIL_USER 환경변수가 비어있습니다.")
if (!nzchar(Sys.getenv(gmail_pw_envvar))) stop("GMAIL_APP_PASSWORD 환경변수가 비어있습니다.")
if (!nzchar(openai_key)) stop("OPENAI_API_KEY 환경변수가 비어있습니다.")

# ------------------------------------------------------------
# 1) 프롬프트 파일 읽기
# ------------------------------------------------------------
prompt_path <- "C:\\PMS_Core\\reports\\gemini_prompt.txt"
if (!file.exists(prompt_path)) stop("프롬프트 파일이 존재하지 않습니다: ", prompt_path)

user_prompt <- readLines(prompt_path, encoding = "UTF-8")
if (length(user_prompt) == 0) stop("프롬프트 파일이 비어 있습니다: ", prompt_path)
user_prompt <- paste(user_prompt, collapse = "\n")

# ------------------------------------------------------------
# 2) Responses JSON에서 "사용자에게 보여줄 텍스트" 추출
# ------------------------------------------------------------
extract_response_text <- function(js) {
  out <- js$output_text %||% ""
  if (nzchar(out)) return(out)
  
  texts <- character(0)
  if (!is.null(js$output) && length(js$output) > 0) {
    for (item in js$output) {
      if (!is.null(item$type) && identical(item$type, "message")) {
        if (!is.null(item$content) && length(item$content) > 0) {
          for (c1 in item$content) {
            if (!is.null(c1$type) && identical(c1$type, "output_text") &&
                !is.null(c1$text) && nzchar(c1$text)) {
              texts <- c(texts, c1$text)
            }
          }
        }
      }
    }
  }
  
  if (length(texts) > 0) return(paste(texts, collapse = "\n\n"))
  ""
}

# ------------------------------------------------------------
# 3) OpenAI 호출
# ------------------------------------------------------------
call_openai_text <- function(model, api_key, prompt_text,
                             max_output_tokens = 2500,
                             retry_once = TRUE) {
  
  do_call <- function(mot) {
    req <- request("https://api.openai.com/v1/responses") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      req_body_json(list(
        model = model,
        input = prompt_text,
        text = list(format = list(type = "text")),
        max_output_tokens = mot
      ))
    
    resp <- req_perform(req)
    
    if (resp_status(resp) >= 400) {
      stop("OpenAI API 오류 (HTTP ", resp_status(resp), ")\n", resp_body_string(resp))
    }
    
    js <- resp_body_json(resp, simplifyVector = FALSE)
    list(js = js, text = extract_response_text(js))
  }
  
  r1 <- do_call(max_output_tokens)
  if (nzchar(r1$text)) return(r1$text)
  
  if (isTRUE(retry_once) &&
      identical(r1$js$status %||% "", "incomplete") &&
      identical(r1$js$incomplete_details$reason %||% "", "max_output_tokens")) {
    
    r2 <- do_call(max_output_tokens * 3L)
    if (nzchar(r2$text)) return(r2$text)
    r1 <- r2
  }
  
  sprintf("⚠️ AI 텍스트 출력이 비어 있습니다. status=%s",
          r1$js$status %||% "NA")
}
cat("AI prompt 조회 시작.\n")
ai_comment <- call_openai_text(
  model = openai_model,
  api_key = openai_key,
  prompt_text = user_prompt
)

# ------------------------------------------------------------
# 4) PDF 첨부 경로: reports/Daily_Risk_{YYYYMMDD}.pdf
# ------------------------------------------------------------
now_kst <- Sys.time()
yyyymmdd <- format(now_kst, "%Y%m%d")

pdf_path <- sprintf("C:\\PMS_Core\\reports\\Daily_Risk_%s.pdf", yyyymmdd)
if (!file.exists(pdf_path)) stop("첨부할 PDF 파일이 없습니다: ", pdf_path)

# ------------------------------------------------------------
# 5) 메일 작성 + 첨부(add_attachment) + 발송
# ------------------------------------------------------------
cat("메일 작성 시작.\n")
to <- "seminago@naver.com"  # <-- 수정

subject <- sprintf("[오늘의 PMS 현황 보고드립니다] %s", format(now_kst, "%Y-%m-%d"))

email <- compose_email(
  body = md(glue("
안녕하세요.

아래는 {format(now_kst, '%Y-%m-%d %H:%M:%S')} 기준, AI 자동 응답 결과입니다.
(첨부: Daily Risk PDF 1건)

---

{ai_comment}

---

(본 메일은 자동 발송되었습니다.)
"))
)

# ✅ 첨부는 smtp_send 옵션이 아니라, 이메일 객체에 붙입니다.
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
