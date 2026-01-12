# ============================================================
# 2026 Portfolio Efficiency Analysis (Risk-Off Removed)
# - Efficient Frontier + 3-Point Comparison 
# - 현대 포트폴리오 이론(MPT)에 따른 최적의 대안을 제시
# ============================================================

# 0) 패키지 로드 ----
pkgs <- c("quantmod","PerformanceAnalytics","xts","zoo","fPortfolio","timeSeries")
newp <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newp) > 0) install.packages(newp, dependencies = TRUE)

library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(fPortfolio)
library(timeSeries)

options(scipen = 999)

# 1) 설정 ----
# CASH는 배제했음
tickers <- c("SPY","SCHD","QQQ","TQQQ","GLD","IEF")
w_user <- c(SPY=0.40, SCHD=0.20, QQQ=0.15, TQQQ=0.10, GLD=0.10, IEF=0.05)
# "가성비는 조금 떨어지지만, 높은 수익을 위해 위험을 기꺼이 감수하는 전형적인 공격형 포트폴리오". 수학적 최적해(Tangency)를 억지로 따르기보다는, TQQQ가 가져올 수 있는 최대 80% 이상의 MDD(최대 낙폭)를 견딜수 있다면 선택.

from  <- "2011-11-01"
to    <- Sys.Date()
use_n <- 170  # 최근 170개월

# 2) 데이터 처리 ----
suppressWarnings(getSymbols(tickers, src="yahoo", from=from, to=to, auto.assign=TRUE))
prices <- do.call(merge, lapply(tickers, function(sym) Ad(get(sym))))
colnames(prices) <- tickers
prices <- na.omit(na.locf(prices))

prices_m <- to.monthly(prices, indexAt="lastof", OHLC=FALSE)
R_m <- na.omit(Return.calculate(prices_m, method="log"))
R10_m <- tail(R_m, min(use_n, NROW(R_m)))
R_ts  <- timeSeries(coredata(R10_m), charvec = as.character(index(R10_m)))

# 3) Efficient Frontier & Portfolios ----
spec <- portfolioSpec(); setType(spec) <- "MV"; constraints <- "LongOnly"
frontier <- portfolioFrontier(data = R_ts, spec = spec, constraints = constraints)

safe_ext <- function(x) as.numeric(x)[1]

# My Portfolio
w_aligned <- w_user[colnames(R10_m)]
ret_my <- as.numeric(R10_m %*% w_aligned)
mu_my <- mean(ret_my); sd_my <- sd(ret_my); sh_my <- mu_my / sd_my

# Tangency / MinVar
minv <- minvariancePortfolio(data = R_ts, spec = spec, constraints = constraints)
tan  <- tangencyPortfolio(data = R_ts, spec = spec, constraints = constraints)
tan_mu <- safe_ext(getTargetReturn(tan)); tan_r <- safe_ext(getTargetRisk(tan))
minv_mu <- safe_ext(getTargetReturn(minv)); minv_r <- safe_ext(getTargetRisk(minv))
tan_sh <- tan_mu / tan_r; minv_sh <- minv_mu / minv_r

# 4) Risk-Off -> GLD 로직 (제거됨) : 시장붕괴상황에 금매수하는 로직은 배제함 ----

# 5) 성과 요약 행렬(cmp) 생성 ----
safe_val <- function(x) {
  v <- as.numeric(x)
  if (length(v) == 0 || is.na(v[1])) return(0)
  return(v[1])
}

# 3개 점만 바인딩
cmp <- rbind(
  My_Portfolio       = c(safe_val(mu_my),   safe_val(sd_my),   safe_val(sh_my)),
  Tangency_MaxSharpe = c(safe_val(tan_mu),  safe_val(tan_r),   safe_val(tan_sh)),
  MinVariance        = c(safe_val(minv_mu), safe_val(minv_r),  safe_val(minv_sh))
)
colnames(cmp) <- c("Return", "Risk", "Sharpe")

# 6) 그래프 시각화 (Risk-Off 제거된 최종 버전) ----
op <- par(no.readonly = TRUE)
par(mar = c(7, 7, 5, 3)) 

# 축 이름(xlab, ylab) 설정
frontierPlot(frontier, frontier="upper", col=c("gray85", "gray85"), labels=FALSE, title=FALSE,
             xlab="", ylab="")

title(xlab = "[ 변동성 (위험) ]", line = 4, cex.lab = 1.2, font.lab = 2)
title(ylab = "[ 수익성 (수익) ]", line = 4, cex.lab = 1.2, font.lab = 2)

mtext("2026 Portfolio Efficiency Analysis", side=3, line=2.5, cex=1.3, font=2)
mtext(paste("Analysis Period:", from, "~", to), side=3, line=1, cex=0.9, col="gray30")

# 좌표 지정 (3개 점만 사용)
x_my <- cmp[1,2]; y_my <- cmp[1,1]
x_tn <- cmp[2,2]; y_tn <- cmp[2,1]
x_mv <- cmp[3,2]; y_mv <- cmp[3,1]

# 점과 이름 (모양으로 구분)
points(x_my, y_my, pch=15, col="black",      cex=1.8) # 네모
points(x_tn, y_tn, pch=17, col="firebrick2", cex=1.8) # 세모
points(x_mv, y_mv, pch=18, col="royalblue3", cex=1.8) # 다이아

text(x_my, y_my, "My Portfolio",  pos=1, cex=0.8, font=2, offset=0.8)
text(x_tn, y_tn, "Max Sharpe",   pos=3, cex=0.8, font=2, col="firebrick2", offset=0.8)
text(x_mv, y_mv, "Min Variance", pos=2, cex=0.8, font=2, col="royalblue3", offset=0.8)

# 가이드 라인 및 Sharpe 개선도 (Max Sharpe 방향만 남김)
segments(x_my, y_my, x_tn, y_tn, lty=3, lwd=1.5, col="gray50")

y_offset <- (max(cmp[,1]) - min(cmp[,1])) * 0.15 # 수치 겹침 방지 오프셋
text((x_my+x_tn)/2, (y_my+y_tn)/2 - y_offset, labels=sprintf("ΔSharpe\n%+.3f", cmp[2,3]-cmp[1,3]), cex=0.8, col="firebrick3", font=2)


# 범례 (3개 항목만 남김)
legend("topleft", legend=c("My Portfolio (Sq)", "Max Sharpe (Tri)", "Min Variance (Dia)"),
       col=c("black", "firebrick2", "royalblue3"), pch=c(15, 17, 18), bty="n", cex=0.8, y.intersp=1.5)

# 결과 출력
print(round(cmp, 6))

cat("\n[ Max Sharpe를 위한 최적 비중 ]\n")
print(round(getWeights(tan), 4) * 100)



# 결과 출력

# > print(round(cmp, 6))
# Return     Risk   Sharpe
# My_Portfolio       0.012318 0.044793 0.275004
# Tangency_MaxSharpe 0.011243 0.034809 0.322997
# MinVariance        0.002801 0.016447 0.170294
# 
# > cat("\n[ Max Sharpe를 위한 최적 비중 ]\n")
# 
# [ Max Sharpe를 위한 최적 비중 ]
# 
# > print(round(getWeights(tan), 4) * 100)
# SPY  SCHD   QQQ  TQQQ   GLD   IEF 
# 0.00 21.49 53.93  0.00 21.92  2.65 
  
  
  
  # 1. 가로축(X)과 세로축(Y)의 수학적 본질
  # 이 그래프는 '리스크-수익 평면(Risk-Return Plane)'을 시각화한 것.
  # 가로축 [변동성 (위험)]: 통계학적으로 수익률의 표준편차(Standard Deviation)를 의미합니다. 자산 가격이 평균으로부터 얼마나 멀리, 자주 이탈하는지를 측정. 수치가 클수록(오른쪽) 자산의 가격 불확실성이 높다는 것을 뜻함.
  # 세로축 [수익성 (수익)]: 분석 기간 동안의 평균 로그 수익률(Mean Log-Return)을 의미. 위로 갈수록 투자 자본의 증식 속도가 빠름을 나타냄.
  # 2. 효율적 투자선(Efficient Frontier): 최적해의 집합
  # 상단의 회색 곡선은 현대 포트폴리오 이론(MPT)의 핵심인 '지배 원리'를 만족하는 지점들의 연결.
  # 수학적으로는 주어진 리스크 수준에서 수익을 극대화하거나, 목표 수익 수준에서 리스크를 최소화하는 최적화 문제(Optimization Problem)의 해집합.
  # 투자자가 가질 수 있는 가장 효율적인 자산 조합의 한계선을 의미하며, 이 선 아래에 위치한 모든 점은 '비효율적'인 상태로 간주.
  # 3. 주요 지점(Point)의 전략적 해석
  # ① 검은 네모 (My Portfolio): 현 상태의 효율성 진단
    # 만약 이 점이 회색 곡선에서 멀리 떨어져 있다면, 현재 자산 구성이 불필요한 리스크를 감수하고 있거나, 리스크 대비 낮은 수익을 얻고 있다는 객관적 증거.
  # ② 빨간 세모 (Max Sharpe): 샤프 지수의 극대화 지점
  # 자본시장선(CML)과 효율적 투자선이 접하는 접점 포트폴리오(Tangency Portfolio).
  # 무위험 자산 대비 초과 수익률을 변동성으로 나눈 값인 샤프 지수(Sharpe Ratio)가 가장 높은 지점. 즉, 투자의 '가성비'가 물리적으로 가장 완벽한 수학적 정답지에 해당.
  # ③ 파란 다이아몬드 Min Variance): 리스크의 하한선
  # 수익률과 상관없이 포트폴리오 전체의 분산(Variance)을 최소화한 조합.
  # 수학적 계산 결과, 자산 간의 상관관계(Correlation)를 이용해 서로의 변동성을 가장 많이 상쇄시킨 지점.
  # 4. ΔSharpe (개선 수치)의 의미: 효율성 개선의 폭
  # 검은 네모에서 빨간 세모로 이어지는 점선 위의 수치는 '전략적 이동의 가치'를 수치화한 것.
  # 단순히 수익을 높이는 것이 아니라, 단위 리스크당 효율성을 얼마나 개선할 수 있는지를 나타냄.
  # 예를 들어 이 수치가 높을수록, 현재의 자산 비중을 수학적 최적 비중으로 재조정(Rebalancing)했을 때 얻을 수 있는 통계적 이득이 크다는 것을 의미.
  # 5. 종합 결론 및 투자 시사점
  # 이 그래프는 "현재 포트폴리오의 비효율성을 제거할 수 있는 방향"을 제시.
  # 만약 검은 네모가 빨간 세모의 우측에 있다면, 이는 TQQQ와 같은 고변동성 자산의 비중이 과다하여 포트폴리오 전체의 효율을 깎아 먹고 있다는 뜻. 이때 하단에 출력된 [최적 비중] 결과값(예: 채권 및 금 비중 확대 등)을 참고하여 비중을 조절하면, 수익은 보전하면서도 변동성 스트레스가 낮은 '수학적 최적 투자'를 실현할 수 있음.
  # 데이터 분석 결과는 단순히 과거를 보여주는 것을 넘어, 통계적 신뢰 구간 안에서 가장 유리한 의사결정 경로를 가이드해 주는 강력한 도구.  마코위츠의 포트폴리오 선정 이론을 참고하시면 이 그래프의 수리적 배경을 더 깊이 이해하실 수 있음.
  # 하지만 감내할 수 있는 손실과 변동성을 감안해서 참고해야 함. 미래가 과거 사건을 그대로 재현한다고 보증할 수 없음.


 # 표본수 170개월(= 2026년 초 기준으로 SCHD가 출시된 이후 지난 개월수)이 적절한지, 통계적으로 의마가 있는지에 대한 제미나이 답변 :

# 통계적 관점에서 170개의 월간 수익률 데이터(약 14년치)는 금융 데이터를 분석하고 포트폴리오 전략을 세우기에 상당히 의미 있고 충분한 표본수라고 평가할 수 있습니다.
# 그 이유와 함께 해석 시 주의해야 할 점을 정리해 드립니다.
# 1. 통계적 신뢰도 측면 (중심극한정리)
# 표본의 크기: 일반적으로 통계학에서는 표본이 30개 이상이면 정규분포를 가정한 분석이 가능하다고 보며, 100개 이상이면 상당히 안정적인 추정치를 얻을 수 있다고 간주합니다. 170개는 평균(Return)과 표준편차(Risk)를 계산할 때 오차 범위(Standard Error)를 충분히 줄일 수 있는 수준입니다.
# 자유도: 효율적 투자선(Efficient Frontier)을 그릴 때 필요한 공분산 행렬(Covariance Matrix)의 안정성을 확보하기에 170개는 적절한 수치입니다.
# 2. 시장 사이클의 포함 여부
# 통계적 숫자보다 중요한 것은 그 기간 동안 "어떤 사건들이 있었느냐"입니다. 지난 170개월(2011년 말 ~ 2026년 초)은 다음과 같은 굵직한 시장 사이클을 모두 포함하고 있습니다.
# 강세장: 2010년대 미국 기술주 중심의 장기 호황 (QQQ, TQQQ의 성과 반영)
# 위기 및 하락장: 2015년 중국 증시 폭락, 2018년 금리 인상기 하락, 2020년 코로나19 팬데믹, 2022년 고인플레이션 및 금리 인상 폭락장
# 회복기: 2023~2025년 AI 붐 및 시장 회복기
# 이처럼 다양한 변동성을 겪은 데이터이기 때문에, 단순히 숫자가 많은 것을 넘어 "다양한 시장 환경에 대한 포트폴리오의 방어력과 공격력"을 입증하기에 충분한 기간입니다.
# 3. 분석 시 주의할 점 (한계점)
# 표본수가 170개로 충분함에도 불구하고, 결과 해석 시 다음 사항을 고려해야 합니다.
# 생존 편향 및 특정 기간 편향: SCHD가 출시된 이후의 14년은 역사적으로 미국 증시가 타 국가나 타 자산군에 비해 압도적으로 우세했던 '미국 예외주의' 기간이었습니다. 과거의 170개월 성과가 미래의 170개월 성과를 보장하지는 않습니다.
# 이상치(Outliers)의 영향: 코로나19 당시의 폭락과 급등 같은 극단적인 데이터가 포함되어 있어, 평균 수익률이 다소 왜곡될 수 있습니다. (작성하신 코드의 log 수익률 방식은 이러한 왜곡을 완화하는 데 도움이 됩니다.)
# TQQQ와 같은 레버리지 자산: TQQQ는 변동성이 극심하여 170개월의 데이터 중 특정 구간(횡보장)에서 수익률 갉아먹기(Decay) 현상이 강하게 나타날 수 있습니다. 이는 일반적인 선형 통계 모델로 예측하기 어려운 변수입니다.
# 결론
# 170개의 표본은 포트폴리오 최적화(MPT)를 수행하기에 매우 우수한 데이터셋입니다. 이 정도 규모의 표본을 바탕으로 나온 Max Sharpe 비중이나 Min Variance 결과는 통계적으로 유의미하며, 실제 자산 배분 전략을 수정하는 근거로 삼기에 충분히 합리적입니다.