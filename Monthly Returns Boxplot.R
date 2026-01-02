# ==========================================
# 0. 패키지
# ==========================================
pkg <- c("tidyverse", "ggplot2", "scales")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

library(tidyverse)
library(ggplot2)
library(scales)

# 분석 기간 텍스트
period_txt <- "(2020 - 2025)"

# ==========================================
# 1. Long 변환
# ==========================================
stopifnot(exists("df"))
stopifnot("Date" %in% names(df))

long2_raw <- df %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "Return") %>%
  filter(!is.na(Return))

ticker_order <- unique(long2_raw$Ticker)

# ==========================================
# 2. stats2 계산 + (핵심) 텍스트 y좌표 생성
# ==========================================
stats2 <- long2_raw %>%
  group_by(Ticker) %>%
  summarise(
    Mean    = mean(Return, na.rm = TRUE),
    Median  = median(Return, na.rm = TRUE),
    SD      = sd(Return, na.rm = TRUE),
    Sharpe  = (mean(Return, na.rm = TRUE) / sd(Return, na.rm = TRUE)) * sqrt(12),
    Min     = min(Return, na.rm = TRUE),
    Max     = max(Return, na.rm = TRUE),
    Q1      = quantile(Return, 0.25, na.rm = TRUE),
    Q3      = quantile(Return, 0.75, na.rm = TRUE),
    IQR     = Q3 - Q1,
    DownAvg = mean(Return[Return < 0], na.rm = TRUE),
    N       = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Ticker = factor(Ticker, levels = ticker_order),
    Ticker_num = as.numeric(Ticker),
    Vol_a = SD * sqrt(12)
  )

# dy(세로 스케일) 만들기: 전체 Return 범위 기반
y_min_all <- min(long2_raw$Return, na.rm = TRUE)
y_max_all <- max(long2_raw$Return, na.rm = TRUE)
dy <- y_max_all - y_min_all
if (!is.finite(dy) || dy == 0) dy <- 0.1  # 안전장치

# 하단 요약 텍스트 y좌표: Q1 아래쪽에 계단식으로 배치
# (겹치면 step 크기만 조절하면 됩니다)
stats2 <- stats2 %>%
  mutate(
    y_mean_txt   = Q1 - 0.18 * dy,
    y_sharpe_txt = Q1 - 0.23 * dy,
    y_iqr_txt    = Q1 - 0.28 * dy,
    y_down_txt   = Q1 - 0.33 * dy,
    y_n_txt      = Q1 - 0.38 * dy
  )

# ==========================================
# 3. long2에 Vol_a / Ticker_num 붙이기 (boxplot fill용)
# ==========================================
long2 <- long2_raw %>%
  mutate(Ticker = factor(Ticker, levels = ticker_order)) %>%
  left_join(stats2 %>% select(Ticker, Vol_a, Ticker_num), by = "Ticker")

# ==========================================
# 4. 상관관계 계산 -> cor_long 만들기
# ==========================================
# wide 형태로 만들어 cor()
wide_ret <- long2_raw %>%
  select(Date, Ticker, Return) %>%
  pivot_wider(names_from = Ticker, values_from = Return) %>%
  arrange(Date)

ret_mat <- wide_ret %>% select(-Date)

cor_mat <- cor(ret_mat, use = "pairwise.complete.obs", method = "pearson")

cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column("Row") %>%
  pivot_longer(-Row, names_to = "Col", values_to = "cor") %>%
  mutate(
    Row = factor(Row, levels = ticker_order),
    Col = factor(Col, levels = ticker_order),
    x = as.numeric(Col),
    label = ifelse(is.na(cor), "", sprintf("%.2f", cor)),
    cor_class = case_when(
      as.character(Row) == as.character(Col) ~ "diag",
      cor >= 0.7 ~ "high",
      cor < 0.3 ~ "low",
      TRUE ~ "mid"
    )
  )

# ==========================================
# 5. 상관관계 표 레이아웃(아래 영역 y좌표)
# ==========================================
row_step  <- 0.03 * dy

# 상관표 시작 y (요약 텍스트보다 아래로 충분히 내림)
y_cor_top <- min(stats2$y_n_txt, na.rm = TRUE) - 0.10 * dy

cor_long <- cor_long %>%
  mutate(y = y_cor_top - (as.numeric(Row) - 1) * row_step)

cor_header <- tibble(
  Ticker = factor(ticker_order, levels = ticker_order),
  x = seq_along(ticker_order),
  y = y_cor_top + 1.2 * row_step
)

cor_rowlab <- tibble(
  Ticker = factor(ticker_order, levels = ticker_order),
  x = 0.2,
  y = y_cor_top - (seq_along(ticker_order) - 1) * row_step
)

# ==========================================
# 6. Plot
# ==========================================
p <- ggplot() +
  # --- [1] 상단 분포 영역 ---
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
  
  # 평균/중앙값
  geom_point(data = stats2, aes(x = Ticker_num - 0.15, y = Mean), shape = 17, size = 3, color = "blue3") +
  geom_point(data = stats2, aes(x = Ticker_num - 0.15, y = Median), shape = 16, size = 2.5, color = "black") +
  
  # Q1/Median/Q3 텍스트
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Q1,     label = sprintf("%.1f%%", Q1*100)),     hjust = 1, size = 3,   color = "grey30") +
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Median, label = sprintf("%.1f%%", Median*100)), hjust = 1, size = 3.5, fontface = "bold") +
  geom_text(data = stats2, aes(x = Ticker_num - 0.32, y = Q3,     label = sprintf("%.1f%%", Q3*100)),     hjust = 1, size = 3,   color = "grey30") +
  
  # 하단 요약 통계량(이제 y_*_txt가 존재하므로 에러 없음)
  geom_text(data = stats2, aes(x = Ticker_num, y = y_mean_txt,   label = sprintf("Mean %.1f%%", Mean*100)),        size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_sharpe_txt, label = sprintf("Sharpe %.2f", Sharpe)),          size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_iqr_txt,    label = sprintf("IQR %.1f%%", IQR*100)),          size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_down_txt,   label = sprintf("Down %.1f%%", DownAvg*100)),     size = 3.2) +
  geom_text(data = stats2, aes(x = Ticker_num, y = y_n_txt,      label = sprintf("n=%d", N)),                      size = 3.2) +
  
  # --- [2] 하단 상관관계 표 ---
  geom_text(data = cor_long, aes(x = x, y = y, label = label, color = cor_class), size = 3.4) +
  scale_color_manual(values = c(high = "red", low = "blue", mid = "black", diag = "black"), guide = "none") +
  geom_text(data = cor_header, aes(x = x, y = y, label = as.character(Ticker)), size = 3.4, fontface = "bold", vjust = 0) +
  geom_text(data = cor_rowlab, aes(x = x, y = y, label = as.character(Ticker)), size = 3.4, fontface = "bold", hjust = 1) +
  
  # --- [3] 스타일 ---
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = stats2$Ticker_num, labels = stats2$Ticker) +
  scale_fill_gradient(low = "steelblue", high = "firebrick", name = "Ann. Vol\n(sd×√12)") +
  labs(
    title = paste("Portfolio Analysis & Correlation Matrix", period_txt),
    subtitle = "Top: Returns distribution (Triangle=Mean, Dot=Median) | Bottom: Correlation Table",
    x = NULL, y = "Monthly Return (%)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(
    xlim = c(0, max(stats2$Ticker_num) + 0.5),
    ylim = c(min(cor_long$y, na.rm = TRUE) - 1.5 * row_step,
             y_max_all + 0.05 * dy)
  )

print(p)
