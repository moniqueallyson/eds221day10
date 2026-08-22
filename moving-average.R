library(tidyverse)
library(ggplot2)

qs_data <- read_csv("data/QuebradaSonadora_Fall1984.csv")

qs_smoothed <- tibble(
  window_start = seq(ymd("1984-09-04"), ymd("1984-11-28"), by = "9 days"),
  k_mgl = NA,
  mg_mgl = NA
)

dates <- seq(ymd("1984-09-04"), ymd("1984-11-28"), by = "9 days")

for (i in 1:nrow(qs_smoothed)) {
  w1 <- qs_smoothed$window_start[i]
  w2 <- qs_smoothed$window_start[i] + 9
  k <- qs_data$k_mgl[qs_data$sample_date >= w1 & qs_data$sample_date < w2]
  mean_k <- mean(k, na.rm = TRUE)
  qs_smoothed$k_mgl[i] <- mean_k
  mg <- qs_data$mg_mgl[qs_data$sample_date >= w1 & qs_data$sample_date < w2]
  mean_mg <- mean(mg, na.rm = TRUE)
  qs_smoothed$mg_mgl[i] <- mean_mg
}

qs_long <- qs_smoothed |>
  pivot_longer(
    cols = !window_start,
    names_to = "ion",
    values_to = "concentration"
  )

ggplot(
  data = qs_long,
  mapping = aes(x = window_start, y = concentration, color = ion)
) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "2 week")
