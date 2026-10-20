library(arrow)
library(binsreg)
library(ggplot2)

parcels <- as.data.frame(read_parquet("MA_parcels_sample.parquet"))
print(nrow(parcels))

set.seed(407)
m1 <- binsreg(
  y = parcels$total_value,
  x = parcels$lot_size_acres,
  w = parcels[, "n_rooms", drop = FALSE],
  line = c(0, 0)
)
summary(m1)

set.seed(407)
m2 <- binsreg(
  y = parcels$total_value,
  x = parcels$lot_size_acres,
  w = parcels[, "n_rooms", drop = FALSE],
  line = c(2, 2)
)
summary(m2)

g1 <- m1$bins_plot +
  labs(title = "line = c(0, 0)",
       x = "Lot size (acres)", y = "Fitted total value")

g2 <- m2$bins_plot +
  labs(title = "line = c(2, 2)",
       x = "Lot size (acres)", y = "Fitted total value")

print(g1)
print(g2)

dir.create("results", showWarnings = FALSE)

ggsave("results/line00.png", g1,
       width = 9, height = 6, dpi = 300)

ggsave("results/line22.png", g2,
       width = 9, height = 6, dpi = 300)

capture.output(
  summary(m1), summary(m2),
  file = "results/model_summary.txt"
)
