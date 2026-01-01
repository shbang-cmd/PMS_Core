# 월 수익률 및 종목별 상관관계 분석

# 1) 상관행렬 계산 (Date 제외)
cor_mat <- df %>%
  select(-Date) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

# 2) 긴 형태로 변환 (표 좌표용)
cor_long <- as.data.frame(cor_mat) %>%
  tibble::rownames_to_column("Row") %>%
  tidyr::pivot_longer(-Row, names_to = "Col", values_to = "Cor") %>%
  mutate(
    Row = factor(Row, levels = ticker_order),
    Col = factor(Col, levels = ticker_order),
    x = as.numeric(Col),
    # 아래에서 y 기준점(y_cor_top)을 잡은 뒤 행마다 내려가게 배치
    is_diag = (as.character(Row) == as.character(Col)),
    label = ifelse(is_diag, "", sprintf("%.2f", Cor)),
    cor_class = dplyr::case_when(
      is_diag ~ "diag",
      Cor > 0.7 ~ "high",
      Cor < 0.3 ~ "low",
      TRUE ~ "mid"
    )
  )

# 3) 상관관계 표 들어갈 y 기준점/간격(원하는 밀도에 맞게 조절)
#    y_cor_start를 "표의 맨 위(첫 행)"로 쓰는 게 편합니다.
y_range <- range(long2$Return, na.rm = TRUE)
dy <- diff(y_range); if (!is.finite(dy) || dy == 0) dy <- 0.1

stats2 <- stats2 %>%
  mutate(
    y_mean_txt   = Min - 0.05 * dy,
    y_sharpe_txt = Min - 0.10 * dy,
    y_iqr_txt    = Min - 0.15 * dy,
    y_down_txt   = Min - 0.20 * dy,
    y_n_txt      = Min - 0.25 * dy
  )

# 표 상단 시작점(첫 행 y), 표 행간격(step)
y_cor_top <- min(stats2$y_n_txt, na.rm = TRUE) - 0.12 * dy
row_step  <- 0.020 * dy  # 표가 촘촘하면 0.015~0.02 추천

cor_long <- cor_long %>%
  mutate(
    y = y_cor_top - (as.numeric(Row) - 1) * row_step
  )

# 4) 헤더(열 이름)도 따로 데이터로 만듦
cor_header <- tibble::tibble(
  Ticker = factor(ticker_order, levels = ticker_order),
  x = seq_along(ticker_order),
  y = y_cor_top + 1.2 * row_step
)

# (선택) 행 헤더(왼쪽 라벨)
cor_rowlab <- tibble::tibble(
  Ticker = factor(ticker_order, levels = ticker_order),
  x = 0.2,  # 왼쪽 여백 위치(0 또는 0.2 등)
  y = y_cor_top - (seq_along(ticker_order) - 1) * row_step
)

# 5) 본 플롯 + 상관관계 표 레이어 추가
p <- ggplot() +
  # --- (기존 상단 분포 레이어들) ---
  geom_boxplot(
    data = long2,
    aes(x = Ticker_num - 0.15, y = Return, fill = Vol_a, group = Ticker),
    width = 0.25, outlier.shape = NA, alpha = 0.7, color = "grey30"
  ) +
  geom_jitter(
    data = long2,
    aes(x = Ticker_num + 0.20, y = Return),
    width = 0.08, height = 0, alpha = 0.25, size = 1.2, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick", alpha = 0.6) +
  geom_point(data = stats2, aes(x = Ticker_num - 0.15, y = Mean), shape = 17, size = 3, color = "blue3") +
  geom_point(data = stats2, aes(x = Ticker_num - 0.15, y = Median), shape = 16, size = 2.5, color = "black") +
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Q1,     label = sprintf("%.1f%%", Q1*100)), hjust = 1, size = 3,   color = "grey30") +
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Median, label = sprintf("%.1f%%", Median*100)), hjust = 1, size = 3.5, fontface = "bold") +
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Q3,     label = sprintf("%.1f%%", Q3*100)), hjust = 1, size = 3,   color = "grey30") +
  
  geom_text(data = stats2, aes(x = Ticker_num, y = y_mean_txt,   label = sprintf("Mean %.1f%%", Mean*100)), size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_sharpe_txt, label = sprintf("Sharpe %.2f", Sharpe)),   size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_iqr_txt,    label = sprintf("IQR %.1f%%", IQR*100)),   size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_down_txt,   label = sprintf("Down %.1f%%", DownAvg*100)), size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_n_txt,      label = sprintf("n=%d", N)),               size = 3.2) +
  
  # --- (여기부터 상관관계 표) ---
  geom_text(
    data = cor_long,
    aes(x = x, y = y, label = label, color = cor_class),
    size = 2.6
  ) +
  scale_color_manual(
    values = c(high = "red", low = "blue", mid = "black", diag = "black"),
    guide = "none"
  ) +
  geom_text(
    data = cor_header,
    aes(x = x, y = y, label = as.character(Ticker)),
    size = 2.7, fontface = "bold", vjust = 0
  ) +
  geom_text(
    data = cor_rowlab,
    aes(x = x, y = y, label = as.character(Ticker)),
    size = 2.7, fontface = "bold", hjust = 1
  ) +
  
  # --- 스케일/테마 ---
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = scales::pretty_breaks(n = 10)
  ) +
  scale_x_continuous(
    breaks = stats2$Ticker_num,
    labels = stats2$Ticker
  ) +
  scale_fill_gradient(
    low = "steelblue", high = "firebrick",
    name = "Ann. Vol\n(sd×√12)"
  ) +
  labs(
    title = paste("Portfolio Analysis & Correlation Matrix", period_txt),
    subtitle = "Top: Returns distribution | Bottom: correlation table (text)",
    x = NULL, y = "Monthly Return (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(
    # x=0.2 같은 행 라벨이 보이도록 왼쪽 범위 확장
    xlim = c(0, max(stats2$Ticker_num) + 0.5),
    ylim = c(
      min(cor_long$y, na.rm = TRUE) - 1.5 * row_step,
      max(long2$Return, na.rm = TRUE) + 0.05 * dy
    )
  )

print(p)

# 자산배분·리스크 관리에서 상관계수 해석의 실무 기준은 대략 다음과 같습니다.
# 
# 1️⃣ Cor > 0.7 (🔴)
# 
# 거의 같은 자산처럼 움직임
# 
# 분산 효과 거의 없음
# 
# 예:
#   
#   S&P500 ↔ NASDAQ
# 
# 국내 대형주 ETF ↔ KOSPI200
# 
# 👉 같은 리스크 팩터
# 
# 2️⃣ 0.3 ~ 0.7 (⚫)
# 
# 완전 동일하진 않지만 방향성 공유
# 
# 분산 효과는 제한적
# 
# 정상적인 “주식–주식” 관계
# 
# 👉 보조 분산
# 
# 3️⃣ Cor < 0.3 (🔵)
# 
# 거의 독립적
# 
# 진짜 분산 효과
# 
# 예:
#   
#   주식 ↔ 채권
# 
# 주식 ↔ 금
# 
# 글로벌 주식 ↔ 로컬 방어 자산
# 
# 👉 포트폴리오 안정성의 핵심
