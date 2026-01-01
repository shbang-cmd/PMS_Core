# =========================
# One-Chart "Full" Version + geom_jitter()
# - Jitter points show observation density
# - Box fill: Annualized Volatility (low=blue, high=red)
# - Order: low vol -> high vol
# - Text: Q1 / Median / Q3, Mean(%), IQR(%), Downside avg(%), N
# - Markers: Median (●), Mean (▲) + line connecting Mean-Median
# - Title: start~end period
# =========================
# =========================
# One-Chart "Full" Version + geom_jitter() + Red outliers
# =========================

pkgs <- c("tidyverse", "scales")
newp <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newp) > 0) install.packages(newp)
library(tidyverse)
library(scales)

# 1) read data
df <- read.csv("asset_returns_monthly.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

# 2) period text
start_date <- min(df$Date, na.rm = TRUE)
end_date   <- max(df$Date, na.rm = TRUE)
period_txt <- sprintf("(%s ~ %s)", format(start_date, "%Y-%m"), format(end_date, "%Y-%m"))

# 3) wide -> long
long <- df %>%
  pivot_longer(cols = -Date, names_to = "Ticker", values_to = "Return") %>%
  filter(!is.na(Return))

# 4) stats per ticker
stats <- long %>%
  group_by(Ticker) %>%
  summarise(
    N      = n(),
    Mean   = mean(Return, na.rm = TRUE),
    Vol_a  = sd(Return, na.rm = TRUE) * sqrt(12),
    Q1     = quantile(Return, 0.25, na.rm = TRUE),
    Median = quantile(Return, 0.50, na.rm = TRUE),
    Q3     = quantile(Return, 0.75, na.rm = TRUE),
    Min    = min(Return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(IQR = Q3 - Q1)

# downside avg
downside <- long %>%
  left_join(stats %>% select(Ticker, Q1), by = "Ticker") %>%
  filter(Return < Q1) %>%
  group_by(Ticker) %>%
  summarise(DownAvg = mean(Return, na.rm = TRUE), .groups = "drop")

stats <- stats %>%
  left_join(downside, by = "Ticker") %>%
  mutate(DownAvg = ifelse(is.na(DownAvg), Q1, DownAvg))

# 5) order by volatility
ticker_order <- stats %>% arrange(Vol_a) %>% pull(Ticker)

long2 <- long %>%
  left_join(stats %>% select(Ticker, Vol_a), by = "Ticker") %>%
  mutate(Ticker = factor(Ticker, levels = ticker_order))

stats2 <- stats %>%
  mutate(Ticker = factor(Ticker, levels = ticker_order))

# 6) bottom text positions
y_range <- range(long2$Return, na.rm = TRUE)
dy <- diff(y_range); if (!is.finite(dy) || dy == 0) dy <- 0.1

stats2 <- stats2 %>%
  mutate(
    y_mean_txt = Min - 0.03 * dy,
    y_iqr_txt  = Min - 0.07 * dy,
    y_down_txt = Min - 0.11 * dy,
    y_n_txt    = Min - 0.15 * dy
  )

# 7) plot
ggplot(long2, aes(x = Ticker, y = Return)) +
  
  # jitter: observation density
  geom_jitter(
    width = 0.15,
    height = 0,
    alpha = 0.25,
    size = 1.2,
    color = "black"
  ) +
  
  # boxplot with RED outliers
  geom_boxplot(
    aes(fill = Vol_a),
    outlier.colour = "red",
    outlier.alpha  = 0.8,
    outlier.size   = 2
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_fill_gradient(
    low = "steelblue",
    high = "firebrick",
    name = "Ann. Vol\n(sd×√12)"
  ) +
  
  # mean–median line
  geom_segment(
    data = stats2,
    aes(x = Ticker, xend = Ticker, y = Median, yend = Mean),
    linewidth = 0.6,
    alpha = 0.6
  ) +
  
  geom_point(data = stats2, aes(y = Median), shape = 16, size = 2.4) +
  geom_point(data = stats2, aes(y = Mean),   shape = 17, size = 2.8) +
  
  # quartile labels
  geom_text(data = stats2, aes(y = Q1, label = sprintf("Q1 %.1f%%", Q1*100)), vjust = 1.25, size = 3) +
  geom_text(data = stats2, aes(y = Median, label = sprintf("Med %.1f%%", Median*100)),
            vjust = -0.7, fontface = "bold", size = 3) +
  geom_text(data = stats2, aes(y = Q3, label = sprintf("Q3 %.1f%%", Q3*100)), vjust = -1.2, size = 3) +
  
  # bottom info
  geom_text(data = stats2, aes(y = y_mean_txt, label = sprintf("Mean %.1f%%", Mean*100)), size = 3) +
  geom_text(data = stats2, aes(y = y_iqr_txt,  label = sprintf("IQR %.1f%%", IQR*100)),  size = 3) +
  geom_text(data = stats2, aes(y = y_down_txt, label = sprintf("Down %.1f%%", DownAvg*100)), size = 3) +
  geom_text(data = stats2, aes(y = y_n_txt,    label = sprintf("n=%d", N)), size = 3) +
  
  labs(
    title = paste("Monthly Returns Distribution (Volatility-sorted)", period_txt),
    subtitle = "Fill=Annualized Vol.  Red dots = outliers (tail risk).  ●=Median, ▲=Mean.",
    x = NULL,
    y = "Monthly Return"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  coord_cartesian(
    ylim = c(min(stats2$y_n_txt, na.rm = TRUE), max(long2$Return, na.rm = TRUE))
  )
