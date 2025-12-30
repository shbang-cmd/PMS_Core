library(httr2)

call_openai_text <- function(prompt, model = "gpt-5") {
  req <- request("https://api.openai.com/v1/responses") |>
    req_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      input = prompt
    ))
  
  resp <- req |> req_perform()
  
  # 상태코드/에러 메시지 확인(실전 방탄)
  if (resp_status(resp) >= 400) {
    stop(resp_body_string(resp))
  }
  
  js <- resp_body_json(resp)
  
  # 가장 권장: output_text
  if (!is.null(js$output_text) && nzchar(js$output_text)) return(js$output_text)
  
  # (방탄) output 구조에서 text 긁기
  out <- js$output
  if (is.list(out)) {
    parts <- c()
    for (item in out) {
      if (!is.null(item$content) && is.list(item$content)) {
        for (c1 in item$content) {
          if (!is.null(c1$text)) parts <- c(parts, c1$text)
        }
      }
    }
    if (length(parts)) return(paste(parts, collapse = "\n"))
  }
  
  return(NA_character_)
}

# ===== 실행부 =====
prompt_path <- "reports/gemini_prompt.txt"
out_path    <- "reports/openai_commentary.txt"

prompt <- paste(readLines(prompt_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

ai_text <- call_openai_text(prompt, model = "gpt-5")
cat(ai_text)

writeLines(ai_text, out_path, useBytes = TRUE)
message("Saved: ", out_path)
