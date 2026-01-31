# =========================================================
# PMS output_sum.csv (Date, Sum, Profit) -> 1-page report
# - Sum(평가액총액) 기준으로 일간 수익률 계산
# - PerformanceAnalytics 1장 요약 차트 + 핵심 표 출력
# =========================================================

# 0) packages ----
pkgs <- c("xts", "zoo", "PerformanceAnalytics", "readr")
newp <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newp) > 0) install.packages(newp, dependencies = TRUE)

library(xts)
library(zoo)
library(PerformanceAnalytics)
library(readr)

# 1) path ----
csv_path <- "C:/PMS_Core/output_sum.csv"   # <- 경로만 본인 PC에 맞게 수정

# 2) read ----
df <- read_csv(csv_path, show_col_types = FALSE)

# 3) normalize columns ----
stopifnot(all(c("Date","Sum") %in% names(df)))

df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]

# Sum이 쉼표 포함 문자열일 수도 있으니 안전하게 숫자화
df$Sum <- as.numeric(gsub(",", "", as.character(df$Sum)))

# 4) xts of portfolio value ----
x_port <- xts(df$Sum, order.by = df$Date)
colnames(x_port) <- "PORT"  # 이 시계열 데이터는 내 포트폴리오 수익률

# 5) returns ----
# log 수익률(누적 곱에 안정적)
R <- Return.calculate(x_port, method = "log")
R <- na.omit(R)

# 6) 핵심 표(운용보고서 1페이지 상단에 들어갈 것들) ----
cat("\n===== Annualized Returns =====\n")
print(table.AnnualizedReturns(R))

# 출력예시
# ===== Annualized Returns =====
#   > print(table.AnnualizedReturns(R))
# PORT
# Annualized Return         0.5252
# Annualized Std Dev        0.1546
# Annualized Sharpe (Rf=0%) 3.3976


cat("\n===== Drawdown Summary =====\n")
print(table.Drawdowns(R))

# From     Trough         To   Depth Length To Trough Recovery
# 1 2025-03-04 2025-04-09 2025-05-20 -0.1246     50        25       25
# 2 2025-12-08 2025-12-18 2025-12-23 -0.0279     13         8        5
# 3 2025-12-24 2026-01-02 2026-01-07 -0.0254     11         6        5
# 4 2024-10-22 2024-11-04 2024-11-06 -0.0213     12        10        2
# 5 2024-11-19 2024-11-20 2024-11-22 -0.0185      4         2        2
# 해석 예시: 최대 낙폭(Depth) = -0.1246 = -12.46%# 
# “한 번이라도 최고점 찍은 뒤, 최악으로 빠졌던 순간이 **-12.46%**였다”는 뜻입니다. 
# 즉, 선생님 포트폴리오는 이 기간 동안
# ‘최악의 순간에도 -12%대 하락’을 버티면 되는 구조였습니다.# 
# 그리고 기간 해석:#   
# From(시작) 2025-03-04 
# Trough(바닥) 2025-04-09 
# To(회복 완료) 2025-05-20 
# Length 50 거래일 
# 떨어지는 데 25일 
# 회복하는 데 25일

cat("\n===== Risk (Downside) =====\n")

risk_tbl <- data.frame(
  Metric = c("MaxDrawdown", "ES_95 (CVaR)", "VaR_95"),
  Value  = c(
    as.numeric(maxDrawdown(R)),
    as.numeric(ES(R, p = 0.95, method = "historical")),
    as.numeric(VaR(R, p = 0.95, method = "historical"))
  )
)

print(risk_tbl)

# 출력 예시
# Metric       Value
# 1  MaxDrawdown  0.12458390
# 2 ES_95 (CVaR) -0.01963120
# 3       VaR_95 -0.01018214
# 해석 예시 :
# ✅ MaxDrawdown = 0.1246 (≈ 12.46%)# 
# 드로우다운 요약표의 -12.46%와 같은 내용입니다.# 
# (표기상 부호가 빠진 형태로 나온 것뿐입니다.)# 
# ✅ VaR_95 = -0.01018 (하루 -1.02% 수준)# 
# 아주 쉽게 말하면,# 
# “나쁜 날 상위 5% 정도에서는
# 하루에 -1.02% 정도 손실이 날 수 있다(경계값)”#  
# 즉, ‘이 정도는 각오해야 하는 데일리 손실선’ 느낌입니다.# 
# ✅ ES_95 (CVaR) = -0.01963 (하루 -1.96% 수준)# 
# ES(Expected Sortfall)는 한 단계 더 강합니다.# 
# “진짜로 망한 날들(최악의 5% 구간)만 모아서 평균을 내면
# 하루 평균 손실이 -1.96% 정도였다”# 
# VaR이 “커트라인”이라면,
# ES는 “커트라인 아래로 떨어진 애들 평균”입니다.# 
# 그래서 ES가 VaR보다 더 나쁘게 나오는 게 정상입니다.


# 7) 1장 차트(누적수익률 + 드로우다운 + 월별수익률) ----
charts.PerformanceSummary(R, main = "PMS | Performance Summary (PORT)")
# 항상 3개의 핵심 그래프를 한 세트로 보여줍니다. 순서는 약간 다를 수 있음.
# ✅ ① 맨 위 그래프# 
# 👉 누적 수익률 그래프입니다# 
# 의미는 딱 하나입니다.# 
# 👉 “처음에 1원(또는 1)을 넣었으면 지금 얼마가 되었는가?”# 
# 입니다.# 
# 그래프가# 
# 오른쪽 위로 꾸준히 올라가면
# → 운용이 잘 된 것입니다.# 
# 중간중간 출렁이는 것은
# → 변동성입니다.# 
# 📈 내 PMS 포트폴리오의 성장 곡선
# # 
# ✅ ② 가운데 그래프#
# 👉 드로우다운(drawdown, 낙폭) 그래프입니다# 
# 이게 실무에서는 가장 중요합니다.# 
# 의미는 다음과 같습니다.# 
# 👉 “과거 최고점 대비, 지금 얼마나 빠져 있는가?”
# 예를 들어,# 
# 과거 최고점이 100# 
# 지금이 85라면# 
# → 드로우다운은 -15% 입니다.# 
# 그래프 해석은 이렇게 하시면 됩니다.# 
# 0선에 붙어 있을수록 좋고# 
# 아래로 깊게 내려갈수록
# → 그 시기에 투자자가 가장 고통스러웠다는 뜻입니다.# 
# 하락 리스크가 바로 이 그래프에서 나옵니다. 
# 아주 쉽게 말하면,# 
# 📉 “멘탈이 가장 많이 흔들렸던 구간을 보여주는 그래프”
# 
# ✅ ③ 맨 아래 그래프# 
# 👉 월별 수익률 히트맵(막대/칸 그림)입니다# 
# 👉 “각 달마다 수익이 났는지, 손실이 났는지”# 
# 위쪽(양수) → 수익이 난 달# 
# 아래쪽(음수) → 손실이 난 달# 
# 이 그래프는 이런 질문에 답을 줍니다.# 
# 매달 조금씩 꾸준한 전략인가?#   
#   아니면 몇 달의 큰 수익에 의존하는 전략인가?#   
#   즉,# 
# 📊 전략의 ‘운 좋은 몇 달’ 의존도를 보는 그림

# 일별 수익률 R -> 월별 수익률로 변환 (위의 지표는 일단위었음)
R_month <- apply.monthly(R, Return.cumulative)

# 월 기준 CVaR
ES_month_95  <- ES(R_month, p = 0.95, method = "historical")
VaR_month_95 <- VaR(R_month, p = 0.95, method = "historical")

print(ES_month_95)
print(VaR_month_95)
 

# 8) (옵션) PNG 저장 ----
# png_out <- "C:/PMS_Core/pms_1page.png"
# png(png_out, width = 1400, height = 900, res = 150)
# charts.PerformanceSummary(R, main = "PMS | Performance Summary (PORT)")
# dev.off()
# cat("\nSaved:", png_out, "\n")
