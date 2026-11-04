# Figures and simulation results for the confidence-interval slides.
# The student demo repeats the teaching code in full and needs no source files.
library(ggplot2)

navy <- "#172d42"
blue <- "#005a8b"
orange <- "#b34b12"
theme_set(theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = navy),
        axis.title = element_text(color = navy),
        legend.position = "top"))

mu <- 1
sigma <- sqrt(10)
n <- 500
B <- 10000
zcrit <- qnorm(.975)
set.seed(2143)
ci <- data.frame(trial = seq_len(B), estimate = NA_real_,
                 se = NA_real_, lower = NA_real_, upper = NA_real_)
for (i in seq_len(B)) {
  x <- rnorm(n, mean = mu, sd = sigma)
  ci$estimate[i] <- mean(x)
  ci$se[i] <- sd(x) / sqrt(n)
  ci$lower[i] <- ci$estimate[i] - zcrit * ci$se[i]
  ci$upper[i] <- ci$estimate[i] + zcrit * ci$se[i]
}
ci$covers <- ci$lower <= mu & mu <= ci$upper
coverage <- mean(ci$covers)
mcse <- sqrt(coverage * (1 - coverage) / B)
first100 <- ci[1:100, ]
first100$result <- factor(ifelse(first100$covers, "Contains mu", "Misses mu"),
                         levels = c("Contains mu", "Misses mu"))
p_coverage <- ggplot(first100, aes(trial, estimate, color = result)) +
  geom_hline(yintercept = mu, linewidth = .8, color = navy) +
  geom_linerange(aes(ymin = lower, ymax = upper), linewidth = .7) +
  geom_point(size = 1.1) +
  scale_color_manual(values = c(blue, orange)) +
  labs(x = "Independent sample / interval", y = "Mean and 95% interval",
       color = NULL,
       subtitle = paste(sum(first100$covers),
                        "of these 100 intervals contain the fixed population mean, mu = 1."))

normal_grid <- data.frame(z = seq(-4, 4, length.out = 2001))
normal_grid$density <- dnorm(normal_grid$z)
central <- subset(normal_grid, abs(z) <= zcrit)
left <- subset(normal_grid, z <= zcrit)
p_normal <- ggplot(normal_grid, aes(z, density)) +
  geom_area(data = central, fill = "#d2e7ef") +
  geom_line(color = blue, linewidth = 1) +
  geom_vline(xintercept = c(-zcrit, zcrit), linetype = 2, color = orange) +
  annotate("text", x = 0, y = .17, label = "Central area = 0.95", size = 6) +
  annotate("text", x = c(-2.8, 2.8), y = .065,
           label = c("0.025", "0.025"), size = 5, color = orange) +
  scale_x_continuous(breaks = c(-4, -zcrit, 0, zcrit, 4),
                     labels = c("-4", "-1.96", "0", "1.96", "4")) +
  labs(x = "Standard Normal value, z", y = "Density")
p_quantile <- ggplot(normal_grid, aes(z, density)) +
  geom_area(data = left, fill = "#d2e7ef") +
  geom_line(color = blue, linewidth = 1) +
  geom_vline(xintercept = zcrit, linetype = 2, color = orange) +
  annotate("text", x = 0, y = .11, label = "Area to the left = 0.975", size = 5.7) +
  annotate("text", x = 2.85, y = .065, label = "0.025", size = 5, color = orange) +
  scale_x_continuous(breaks = c(-4, -2, 0, zcrit, 4),
                     labels = c("-4", "-2", "0", "1.96", "4")) +
  labs(x = "qnorm(0.975) finds the cutoff with 97.5% to its left", y = "Density")

levels <- c(.90, .95, .99)
widths <- data.frame(level = factor(paste0(100 * levels, "%"),
                                    levels = c("99%", "95%", "90%")),
                     estimate = ci$estimate[1], se = ci$se[1])
widths$critical <- qnorm(1 - (1 - levels) / 2)
widths$lower <- widths$estimate - widths$critical * widths$se
widths$upper <- widths$estimate + widths$critical * widths$se
p_widths <- ggplot(widths, aes(y = level, x = estimate)) +
  geom_segment(aes(x = lower, xend = upper, yend = level),
               linewidth = 2, color = blue) +
  geom_point(size = 3.5, color = navy) +
  labs(y = "Confidence level", x = "Same observed sample mean and estimated SE")

t_grid <- rbind(data.frame(z = normal_grid$z, density = normal_grid$density,
                           distribution = "Standard Normal"),
                data.frame(z = normal_grid$z, density = dt(normal_grid$z, 9),
                           distribution = "t, 9 df"))
p_t <- ggplot(t_grid, aes(z, density, color = distribution, linetype = distribution)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(blue, orange)) +
  labs(x = "Standardized sample mean", y = "Density", color = NULL, linetype = NULL)

compare_normal <- function(n, B = 10000) {
  stats <- replicate(B, {
    x <- rnorm(n, mean = mu, sd = sigma)
    c(estimate = mean(x), se = sd(x) / sqrt(n))
  })
  c(z = mean(abs(stats["estimate", ] - mu) <= qnorm(.975) * stats["se", ]),
    t = mean(abs(stats["estimate", ] - mu) <= qt(.975, n - 1) * stats["se", ]))
}
set.seed(2110)
normal_coverage <- rbind(compare_normal(10), compare_normal(500))
normal_table <- data.frame(
  n = rep(c(10, 500), each = 2),
  Cutoff = rep(c("Normal", "t"), 2),
  Critical = c(qnorm(.975), qt(.975, 9), qnorm(.975), qt(.975, 499)),
  Simulated = as.vector(t(normal_coverage)),
  Theory = c(2 * pt(qnorm(.975), 9) - 1, .95,
             2 * pt(qnorm(.975), 499) - 1, .95)
)

# Same target mean in both populations; only the population shape changes.
coverage_t <- function(n, population, B = 10000) {
  contains_mu <- replicate(B, {
    x <- if (population == "Normal") rnorm(n, 1, 1) else rexp(n, rate = 1)
    margin <- qt(.975, df = n - 1) * sd(x) / sqrt(n)
    mean(x) - margin <= 1 && 1 <= mean(x) + margin
  })
  p <- mean(contains_mu)
  c(coverage = p, mcse = sqrt(p * (1 - p) / B))
}
set.seed(2111)
robustness <- expand.grid(n = c(10, 30, 100, 500),
                          population = c("Normal", "Exponential"),
                          stringsAsFactors = FALSE)
robustness$coverage <- NA_real_
robustness$mcse <- NA_real_
for (i in seq_len(nrow(robustness))) {
  result <- coverage_t(robustness$n[i], robustness$population[i])
  robustness$coverage[i] <- result["coverage"]
  robustness$mcse[i] <- result["mcse"]
}
p_robustness <- ggplot(robustness, aes(n, coverage, color = population,
                                     linetype = population)) +
  geom_hline(yintercept = .95, color = navy, linetype = 3) +
  geom_line(linewidth = .9) +
  geom_errorbar(aes(ymin = coverage - 2 * mcse, ymax = coverage + 2 * mcse),
                width = .03) +
  geom_point(size = 2.8) +
  scale_color_manual(values = c(orange, blue)) +
  scale_x_log10(breaks = c(10, 30, 100, 500)) +
  scale_y_continuous(breaks = seq(.86, .96, .02),
                     labels = function(x) paste0(round(100 * x), "%"),
                     limits = c(.86, .97)) +
  labs(x = "Observations in each sample (log scale)", y = "Simulated coverage",
       color = "Population", linetype = "Population")
