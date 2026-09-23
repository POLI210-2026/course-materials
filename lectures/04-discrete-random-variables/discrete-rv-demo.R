# POLI 210, September 23, 2026. Instructor review draft; base R only.
# Run the full script once. Then rerun named sections for demonstrations.
# N always counts repeated experiments, not units within one experiment.

## ---- demo-setup ----
N <- 20000L
theory_color <- "#263d4a"
simulation_color <- "#176b8e"
event_color <- "#985326"

## ---- assignment-pmf ----
assignments <- expand.grid(unit1 = c("C", "T"),
                           unit2 = c("C", "T"),
                           unit3 = c("C", "T"))
assignments$number_treated <- rowSums(assignments == "T")
xs <- 0:3
pmf <- as.numeric(table(factor(assignments$number_treated,
                              levels = xs))) / nrow(assignments)
data.frame(x = xs, probability = pmf)

## ---- assignment-sim ----
set.seed(92301)
N <- 20000L
X <- replicate(N, {
  assignment <- sample(c("C", "T"), 3, replace = TRUE)
  sum(assignment == "T")
})
head(X)

## ---- assignment-direct ----
set.seed(92302)
X_direct <- sample(0:3, N, replace = TRUE,
                   prob = c(1, 3, 3, 1) / 8)
set.seed(92303)
X_binom <- rbinom(N, size = 3, prob = 0.5)

## ---- pmf-comparison ----
relative_frequency <- function(z) {
  as.numeric(table(factor(z, levels = 0:3))) / length(z)
}
comparison <- data.frame(
  x = xs, exact = pmf,
  individual_assignments = relative_frequency(X),
  direct_pmf = relative_frequency(X_direct),
  binomial = relative_frequency(X_binom))
round(comparison, 4)

## ---- pmf-plot ----
par(mar = c(3, 3.6, 1.6, .8), mgp = c(2, .6, 0),
    mfrow = c(1, 2), cex = .85)
barplot(pmf, names.arg = xs, col = theory_color,
        ylim = c(0, .45), ylab = "Probability", xlab = "Number treated",
        main = "Exact PMF", border = NA)
barplot(relative_frequency(X), names.arg = xs, col = simulation_color,
        ylim = c(0, .45), ylab = "Relative frequency", xlab = "Number treated",
        main = "20,000 simulated experiments", border = NA)
par(mfrow = c(1, 1))

## ---- expectation ----
mu <- sum(xs * pmf)
c(exact_mean = mu, simulated_mean = mean(X),
  probability_all_same = mean(X == 0 | X == 3))

## ---- poll-exact ----
poll_size <- 300L
support <- 0.46
poll_mean <- poll_size * support
poll_lead <- pbinom(150, size = poll_size, prob = support,
                    lower.tail = FALSE)
c(expected_supporters = poll_mean, probability_sample_lead = poll_lead)

## ---- poll-plot ----
counts <- 0:poll_size
poll_pmf <- dbinom(counts, size = poll_size, prob = support)
par(mar = c(3.2, 3.8, 1.6, 1), mgp = c(2.3, .7, 0), cex = .85)
plot(counts, poll_pmf, type = "h", lwd = 2,
     col = ifelse(counts > 150, event_color, theory_color),
     xlim = c(100, 180), ylim = c(0, .052),
     xlab = "Supporters in one poll of 300", ylab = "Probability",
     main = "Known support = 46%: central portion of the binomial PMF")
abline(v = 150.5, lty = 2, col = "gray50")
legend("topright", legend = c("150 or fewer", "Strict majority: 151 or more"),
       col = c(theory_color, event_color), lwd = 2, bty = "n", cex = .8)

## ---- cdf-plot ----
par(mar = c(3.1, 3.5, 1.4, 1), mgp = c(2.2, .7, 0), cex = .85)
cuts <- c(-.6, 0:3, 3.6)
heights <- c(0, cumsum(pmf))
plot(NA, xlim = c(-.6, 3.6), ylim = c(-.03, 1.05),
     xlab = "x", ylab = "F(x) = P(X <= x)",
     main = "CDF of the number treated")
segments(cuts[-length(cuts)], heights, cuts[-1], heights,
         lwd = 2, col = theory_color)
points(xs, cumsum(pmf), pch = 16, col = theory_color)
points(xs, c(0, cumsum(pmf)[1:3]), pch = 21, bg = "white", col = theory_color)

## ---- poll-sim ----
set.seed(92304)
polls <- rbinom(N, size = 300, prob = 0.46)
c(simulated_mean = mean(polls),
  simulated_probability_lead = mean(polls > 150),
  exact_probability_lead = poll_lead)

## ---- transform ----
Y <- (X - 1.5)^2
c(prob_Y_0.25 = mean(Y == .25), prob_Y_2.25 = mean(Y == 2.25),
  mean_Y = mean(Y))

## ---- variance ----
v <- sum((xs - mu)^2 * pmf)
c(exact_variance = v, shortcut = sum(xs^2 * pmf) - mu^2,
  simulated_squared_deviation = mean((X - mu)^2),
  sample_variance = var(X), sample_sd = sd(X))

## ---- spread-plot ----
par(mfrow = c(1, 3), mar = c(3, 3, 2, .6), mgp = c(1.9, .5, 0), cex = .75)
plot_one <- function(values, probs, title, color) {
  plot(NA, xlim = c(-.3, 4.3), ylim = c(0, .55), xlab = "x", ylab = "p(x)",
       main = title, xaxt = "n")
  axis(1, at = 0:4)
  segments(values, 0, values, probs, lwd = 5, col = color)
  abline(v = sum(values * probs), lty = 2, col = "gray50")
}
plot_one(xs, pmf, "Mean 1.5; variance 0.75", theory_color)
plot_one(c(0, 3), c(.5, .5), "Same mean; variance 2.25", event_color)
plot_one(xs + 1, pmf, "Mean 2.5; variance 0.75", simulation_color)
par(mfrow = c(1, 1))

## ---- variance-rules ----
set.seed(92305)
Z <- rbinom(N, size = 3, prob = .5) # independent of the draws in X
c(var_sum_independent = var(X + Z),
  sum_of_variances = var(X) + var(Z),
  var_sum_copied = var(X + X),
  four_times_variance = 4 * var(X))

## ---- geometric-sim ----
set.seed(92306)
G <- replicate(N, {
  failures <- 0L
  while (sample(c(FALSE, TRUE), 1, prob = c(.8, .2)) == FALSE) {
    failures <- failures + 1L
  }
  failures
})
c(mean_failures = mean(G), mean_total_calls = mean(G + 1),
  simulated_more_than_five_calls = mean(G >= 5),
  exact_more_than_five_calls = pgeom(4, prob = .2, lower.tail = FALSE))

## ---- geometric-direct ----
set.seed(92307)
G_direct <- rgeom(N, prob = .2)
c(mean_failures = mean(G_direct), probability_more_than_five_calls = mean(G_direct >= 5))

## ---- other-distributions ----
set.seed(92308)
reds <- rhyper(N, m = 5, n = 5, k = 6)
c(exact_four_red = dhyper(4, m = 5, n = 5, k = 6),
  simulated_four_red = mean(reds == 4))
set.seed(92309)
calls_per_hour <- rpois(N, lambda = 2)
c(exact_at_least_three = ppois(2, lambda = 2, lower.tail = FALSE),
  simulated_at_least_three = mean(calls_per_hour >= 3))
