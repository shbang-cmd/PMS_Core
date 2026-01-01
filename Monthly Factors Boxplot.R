library(tidyverse)

# --- 입력: factors_monthly.csv ---
df <- read.csv("factors_monthly.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

# 숫자 팩터만 long
long <- df %>%
  pivot_longer(cols = where(is.numeric), names_to = "Factor", values_to = "Return") %>%
  filter(!is.na(Return))

# --- 리스크/비대칭 지표 계산 ---
factor_metrics <- long %>%
  group_by(Factor) %>%
  summarise(
    n = n(),
    mean = mean(Return),
    median = median(Return),
    sd_m = sd(Return),
    vol_ann = sd(Return) * sqrt(12),
    
    q01 = quantile(Return, 0.01),
    q05 = quantile(Return, 0.05),
    q25 = quantile(Return, 0.25),
    q75 = quantile(Return, 0.75),
    
    iqr = q75 - q25,
    
    # 하위 25% 평균(꼬리 손실 체감)
    down_avg = mean(Return[Return < q25]),
    
    # 왜도(표본왜도; 외부패키지 없이 계산)
    skew = {
      x <- Return
      m <- mean(x); s <- sd(x)
      if (is.na(s) || s == 0) NA_real_ else mean(((x - m) / s)^3)
    },
    
    # Mean-Median 괴리(비대칭의 간단 지표)
    mean_minus_median = mean - median,
    
    .groups = "drop"
  )

# --- “쓸 만함” 점수: (양(+)의 평균) - (꼬리 손실 페널티) - (비대칭 페널티) ---
# *정답은 없고 PMS 관점에서 “꼬리위험/비대칭이 과한 팩터는 감점”으로 설계
factor_metrics <- factor_metrics %>%
  mutate(
    # 꼬리위험 강도(절대값으로): down_avg가 더 음수일수록 위험
    tail_risk = abs(pmin(down_avg, 0)),
    
    # 비대칭 리스크: 음(-) 왜도일수록(왼쪽 꼬리) 위험, mean-median 양(+) 괴리 클수록 “가끔 크게 벌고 자주 찔끔” 가능
    asym_risk = abs(pmin(skew, 0)) + abs(mean_minus_median),
    
    # 간단 점수(가중치는 취향; PMS는 보수적으로 tail에 더 큰 페널티)
    score = (pmax(mean, 0)) - 0.8 * tail_risk - 0.4 * asym_risk
  )

# --- 코멘트 자동 생성 규칙 ---
comment_one_liner <- function(mean, median, skew, down_avg, q05, vol_ann) {
  # 라벨 판정
  tail <- if (down_avg < -0.03) "꼬리손실 큼" else if (down_avg < -0.015) "꼬리손실 보통" else "꼬리손실 낮음"
  skew_lbl <- if (!is.na(skew) && skew < -0.5) "음(-)왜도 강함" else if (!is.na(skew) && skew < -0.2) "음(-)왜도" else "왜도 약함/중립"
  premium <- if (mean > 0.005) "프리미엄 양호" else if (mean > 0) "프리미엄 미약" else "프리미엄 불리"
  mm <- if (abs(mean - median) > 0.003) "Mean-Median 괴리" else "분포 대칭에 가까움"
  
  sprintf(
    "%s · %s(%s) · %s · 5%%VaR %.1f%% · 연변동성 %.1f%%",
    premium,
    tail, skew_lbl,
    mm,
    q05 * 100,
    vol_ann * 100
  )
}

factor_metrics <- factor_metrics %>%
  rowwise() %>%
  mutate(
    comment = comment_one_liner(mean, median, skew, down_avg, q05, vol_ann)
  ) %>%
  ungroup()

# --- “쓸 만한 팩터” 랭킹/출력 ---
# score가 높을수록: "양(+)프리미엄 대비 꼬리·비대칭 페널티가 작은" 팩터
ranked <- factor_metrics %>%
  arrange(desc(score)) %>%
  select(Factor, score, mean, vol_ann, down_avg, skew, q05, comment)

print(ranked, n = Inf)

# (선택) 최상위 2개만 한 줄 요약
top2 <- ranked %>% slice(1:2) %>% transmute(line = paste0(Factor, ": ", comment))
cat(paste(top2$line, collapse = "\n"))


# 팩터의 의미 세부 설명 :
#
# 0️⃣ 먼저 한 문장 요약
# 
# 팩터란 “종목을 고르는 기준”이 아니라
# “시장이 반복적으로 보상하거나 처벌해 온 위험의 성질”입니다.
# 
# GROWTH / VALUE / MOM / MKT는
# **서로 다른 ‘위험의 얼굴’**을 나타냅니다.
# 
# 1️⃣ MKT (Market Factor)
# ① 정확한 정의
# 
# MKT = 전체 주식시장의 초과수익률
# 
# 즉,
# 
# 시장 전체를 들고 갔을 때
# 
# 무위험자산 대비 추가로 얻은 보상
# 
# 📌 모든 팩터의 기준점(바닥) 입니다.
# 
# ② 이 팩터가 보상하는 위험
# 
# 경기 침체
# 
# 금융위기
# 
# 시스템 리스크
# 
# 👉 “시장이 망할 수도 있다는 위험”
# 
# ③ PMS 관점의 의미
# 
# 피할 수 없는 리스크
# 
# 모든 장기 투자자는 결국 MKT에 노출됨
# 
# 다른 팩터의 성과는 대부분 MKT 위에 얹힌 효과
# 
# 📌 PMS 해석 한 줄:
#   
#   “MKT는 선택이 아니라 전제 조건이다.”
# 
# 2️⃣ VALUE (Value Factor)
# ① 정확한 정의
# 
# VALUE = 저평가 주식 − 고평가 주식
# 
# 전통적으로는:
#   
#   낮은 P/B, P/E
# 
# 높은 배당
# 
# 자산 대비 싼 가격
# 
# 📌 “싸 보이는 주식”이 아닙니다.
# 👉 **“시장이 외면한 주식”**입니다.
# 
# ② 이 팩터가 보상하는 위험
# 
# 장기간 소외
# 
# 구조적 쇠퇴 기업 보유
# 
# ‘지금 당장은 틀린 선택처럼 보이는 고통’
# 
# 👉 즉, 인내 리스크
# 
# ③ PMS 관점의 의미
# 
# 대부분의 달에는 재미없음
# 
# 가끔 크게 보상
# 
# 분포가 비교적 정직
# 
# 📌 PMS 해석 한 줄:
#   
#   “VALUE는 ‘참는 대가’를 보상받는 팩터다.”
# 
# 3️⃣ GROWTH (Growth Factor)
# ① 정확한 정의
# 
# GROWTH = 고성장·고평가 주식 − 저성장·저평가 주식
# 
# 특징:
#   
#   높은 매출/이익 성장률
# 
# 높은 밸류에이션
# 
# 미래 기대가 이미 가격에 반영
# 
# 📌 VALUE의 정확한 반대편에 위치
# 
# ② 이 팩터가 보상하는 위험
# 
# 기대가 꺾일 위험
# 
# 금리 상승 리스크
# 
# 성장 스토리 붕괴
# 
# 👉 기대 붕괴 리스크
# 
# ③ PMS 관점의 의미
# 
# 평소엔 비교적 안정
# 
# 특정 국면(금리 급등)에서 크게 흔들림
# 
# VALUE보다 심리적으로 들고 가기 쉬움
# 
# 📌 PMS 해석 한 줄:
#   
#   “GROWTH는 미래에 대한 신뢰에 베팅하는 팩터다.”
# 
# 4️⃣ MOM (Momentum Factor)
# ① 정확한 정의
# 
# MOM = 최근 성과가 좋은 주식 − 나쁜 주식
# 
# 보통:
#   
#   최근 6~12개월 수익률 기준
# 
# 오직 “가격 움직임”만 봄
# 
# 📌 펀더멘털과 무관
# 
# ② 이 팩터가 보상하는 위험
# 
# 군집 행동
# 
# 추세 붕괴
# 
# 갑작스러운 리버설
# 
# 👉 Crash risk (꼬리위험)
# 
# ③ PMS 관점의 의미
# 
# 평균 수익은 큼
# 
# 중앙값은 거의 0
# 
# 한 번씩 전체 포트폴리오를 흔드는 손실
# 
# 📌 PMS 해석 한 줄:
#   
#   “MOM은 평소엔 보상을 주지만, 위기에는 가장 먼저 배신한다.”
# 
# 5️⃣ 네 팩터를 한 테이블로 정리하면
# 팩터	무엇의 보상인가	주된 리스크	PMS 성격
# MKT	시장 자체	시스템 리스크	필수
# VALUE	인내	소외·지연	장기 핵심
# GROWTH	기대	금리·버블	완충
# MOM	추세	꼬리 붕괴	제한적
# 6️⃣ 왜 이 정의가 PMS에서 중요한가?
#   
#   팩터를 이렇게 이해하지 않으면:
#   
#   MOM을 VALUE처럼 들고 가고
# 
# VALUE를 단기 전략으로 쓰고
# 
# GROWTH를 안전자산처럼 착각하게 됩니다.
# 
# 📌 팩터의 정의 = 사용법
# 
# 7️⃣ 핵심 요약 (기억해야 할 문장 4개)
# 
# MKT: “시장에 남아 있는 대가”
# 
# VALUE: “참아낸 사람의 보상”
# 
# GROWTH: “미래를 믿는 대가”
# 
# MOM: “추세를 믿는 대가”



# 팩터 분석 설명 :
#
# 1️⃣ 먼저, 팩터 투자를 한 문장으로 정리하면
# 
# 팩터 투자란
# “대부분의 시간에는 별일 없지만,
# 특정한 상황에서 반복적으로 보상(또는 벌)을 받는 구조”에 베팅하는 것입니다.
# 
# 즉,
# 
# ETF/주식: 항상 들고 가는 자산
# 
# 팩터: 특정 성질의 위험을 대신 떠안는 계약
# 
# 👉 그래서 팩터는
# 평균 수익보다 “언제, 어떻게 깨지는가”가 훨씬 중요합니다.
# 
# 2️⃣ 왜 평균 수익률만 보면 안 되는가?
#   
#   예를 들어 보겠습니다.
# 
# 두 팩터 A, B가 있다고 가정
# 구분	평균 수익	느낌
# 팩터 A	+0.6%/월	좋아 보임
# 팩터 B	+0.4%/월	덜 좋아 보임
# 
# 👉 평균만 보면 A가 더 좋아 보입니다.
# 
# 그런데 분포를 보면:
#   
#   항목	팩터 A	팩터 B
# 평소(중앙값)	거의 0	+0.3%
# 나쁜 달	-8%	-3%
#   특징	가끔 크게 벌고, 가끔 크게 깨짐	조용히 조금씩
# 
# 👉 **PMS 관점에서는 B가 더 “쓸 만한 팩터”**입니다.
# 
# 이 차이를 잡아내기 위해 지금 분석을 하는 것입니다.
# 
# 3️⃣ 이번 팩터 분석이 보는 핵심 질문 3가지
# 질문 ①
# 
# “이 팩터는 평소에 돈을 벌어주는가?”
# 
# → Median(중앙값)
# 
# 중앙값 ≈ 0 → 대부분의 달은 보상 없음
# 
# 중앙값 > 0 → 평소에도 보상
# 
# 📌 팩터는 Median이 매우 중요
# (Mean은 몇 번의 큰 사건에 속기 쉬움)
# 
# 질문 ②
# 
# “나쁜 달에는 얼마나 아픈가?”
# 
# → DownAvg / Q1 / 5% VaR
# 
# Q1: 하위 25% 달의 경계
# 
# DownAvg: “안 좋은 달들만 평균 내면 얼마나 깨지는가”
# 
# 5% VaR: 정말 운 나쁜 달
# 
# 📌 이건 투자자 입장에서 이렇게 느껴집니다:
#   
#   “이 팩터를 들고 있다가
# 운 나쁜 해를 만나면
# 심리적으로 버틸 수 있는가?”
# 
# 질문 ③
# 
# “보상 구조가 정직한가, 아니면 함정이 있는가?”
# 
# → Mean–Median 괴리 + 왜도(skew)
# 
# 대표적인 위험한 구조
# 
# Median ≈ 0
# 
# Mean > 0
# 
# Skew < 0
# 
# 👉 뜻:
#   
#   “대부분의 시간엔 아무 일 없고,
# 가끔 크게 벌지만,
# 깨질 땐 아주 세게 깨진다”
# 
# 📌 모멘텀(MOM) 팩터에서 자주 나타나는 구조
# 
# 4️⃣ 이제 MKT / VALUE / GROWTH / MOM을 직관적으로 해석해보면
# 
# (실제 결과는 데이터 기간에 따라 다르지만, 구조는 보통 이렇습니다)
# 
# 🔹 MKT (시장)
# 
# ✔ Mean, Median 모두 양호
# 
# ✔ Downside는 있지만 “이유를 알고 감내하는 리스크”
# 
# ✔ 왜도 과하지 않음
# 
# 👉 PMS 기준:
#   
#   “모든 위험의 기준점.
# 피할 수는 없고, 관리할 뿐.”
# 
# 🔹 VALUE (가치)
# 
# ✔ Median ≈ 0 또는 약간 +
#   
#   ✔ Mean > 0
# 
# ⚠ Downside 존재하지만 완만
# 
# 👉 해석:
#   
#   “대부분의 시간엔 재미없지만
# 오래 들고 가면 보상받는 구조”
# 
# 📌 장기 PMS에 가장 잘 맞는 팩터
# 
# 🔹 GROWTH (성장)
# 
# ✔ Downside가 상대적으로 얕음
# 
# ✔ 분포가 비교적 대칭
# 
# ⚠ Mean은 크지 않을 수 있음
# 
# 👉 해석:
#   
#   “드라마틱하진 않지만
# 포트폴리오 완충재 역할”
# 
# 🔹 MOM (모멘텀)
# 
# ⚠ Median ≈ 0
# 
# ✔ Mean은 큼
# 
# ❌ Downside 큼
# 
# ❌ 음(-)왜도 강함
# 
# 👉 진짜 의미:
#   
#   “평소엔 좋아 보이지만
# 한 번씩 PMS 전체를 흔드는 팩터”
# 
# 📌 개인 투자자에게 가장 위험한 팩터
# 
# 5️⃣ 그래서 ‘쓸 만한 팩터’란 무엇인가?
#   
#   PMS 관점에서 쓸 만한 팩터란:
#   
#   ❌ 평균 수익이 가장 큰 팩터
# ✅ “버틸 수 있는 손실 구조를 가진 팩터”
# 
# 즉,
# 
# Median이 너무 0에 가깝지 않고
# 
# Downside tail이 과하지 않으며
# 
# Mean–Median 괴리가 크지 않은 것
# 
# 6️⃣ 이 분석이 실제 투자 결정에 주는 힌트
# 
# 이 분석은 **“팩터를 얼마나, 어떤 방식으로 쓸 수 있는가”**를 알려줍니다.
# 
# 예:
#   
#   VALUE → 장기 핵심
# 
# GROWTH → 완충용
# 
# MOM → 소량·조건부·타이밍 제한
# 
# MKT → 기본 바닥
# 
# 📌 **“팩터를 ETF처럼 들고 가면 안 되는 이유”**가 바로 여기 있습니다.
# 
# 7️⃣ 핵심 요약 (한 문단)
# 
# 이 팩터 분석은
# “얼마나 벌 수 있는가”가 아니라
# “언제, 얼마나 아프게 맞는가”를 먼저 보여주는 분석입니다.
# 
# PMS 관점에서는
# 아프게 맞는 구조를 미리 아는 것 자체가 수익입니다.