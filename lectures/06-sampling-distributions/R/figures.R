# Original scientific figures for Seminar 6. Examples are hypothetical.
# The student live-demo QMD is independent of this supporting file.
blue <- "#176B87"
rust <- "#A44A3F"
green <- "#567D46"
light_blue <- "#CEE3EB"
ink <- "#172D42"

fig_par <- function() {
  par(mar = c(4.0, 4.2, 1.8, .8), mgp = c(2.6, .8, 0), las = 1,
      cex = 1.1, col.axis = ink, col.lab = ink, fg = ink)
}

pmf_reminder <- function() {
  set.seed(100701)
  B <- 10000
  draws <- rbinom(B, size = 4, prob = .6)
  k <- 0:4
  estimated <- as.numeric(table(factor(draws, levels = k))) / B
  fig_par()
  plot(k, estimated, type = "h", lwd = 8, col = light_blue,
       ylim = c(0, .4), xlab = "Supporters among four people",
       ylab = "Probability", xaxt = "n")
  axis(1, at = k)
  points(k, dbinom(k, 4, .6), pch = 19, col = rust, cex = 1.1)
  legend("topleft", c("Simulated PMF", "Exact binomial PMF"),
         col = c(light_blue, rust), lwd = c(8, NA), pch = c(NA, 19),
         bty = "n", cex = .95)
}

histogram_area <- function() {
  # Twenty illustrative observations, with bin counts 2, 6, 8, 4.
  draws <- c(.4, 1.3, 2.1, 2.4, 2.7, 3.1, 3.4, 3.8,
             4.1, 4.3, 4.5, 4.8, 5.1, 5.3, 5.6, 5.9, 6.2, 6.6, 7.1, 7.7)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 2))
  colors <- c(light_blue, light_blue, rust, light_blue)
  for (density_scale in c(FALSE, TRUE)) {
    fig_par(); par(cex = 1, mar = c(4, 4.3, 2.2, .8))
    result <- hist(draws, breaks = seq(0, 8, by = 2),
                   probability = density_scale, col = colors, border = "white",
                   xlab = "Hypothetical margin (pp)",
                   ylab = if (density_scale) "Density (per pp)" else "Number of draws",
                   main = if (density_scale) "Density histogram" else "Count histogram",
                   ylim = if (density_scale) c(0, .29) else c(0, 11.6),
                   xaxt = "n", yaxs = "i")
    axis(1, at = seq(0, 8, 2))
    heights <- if (density_scale) result$density else result$counts
    text(result$mids, heights, if (density_scale) sprintf("%.2f", heights) else heights,
         pos = 3, cex = .95)
    text(5, if (density_scale) .1 else 4,
         if (density_scale) "Area\n0.40" else "8 draws", col = "white", cex = .93)
    rug(draws, col = ink, ticksize = .035)
  }
}

histogram_binwidth <- function() {
  set.seed(100721)
  draws <- rnorm(80, mean = 3, sd = 2)
  widths <- c(2, .25)
  histograms <- lapply(widths, function(width) {
    hist(draws, breaks = seq(-8, 14, by = width), plot = FALSE)
  })
  smooth <- density(draws, bw = .65, from = -4, to = 10)
  top <- 1.1 * max(unlist(lapply(histograms, function(z) z$density)), smooth$y)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 3))
  for (j in 1:3) {
    fig_par(); par(cex = .96, mar = c(4, 4.1, 2.1, .5))
    if (j <= 2) {
      plot(histograms[[j]], freq = FALSE, col = light_blue, border = "white",
           main = paste("Bin width =", widths[j]),
           xlab = "Margin (pp)", ylab = "Density", xlim = c(-4, 10), ylim = c(0, top))
    } else {
      plot(smooth, col = rust, lwd = 3, main = "Gaussian, h = 0.65",
           xlab = "Margin (pp)", ylab = "Density", xlim = c(-4, 10),
           ylim = c(0, top), bty = "l")
    }
    rug(draws, col = adjustcolor(ink, .55))
  }
}

kernel_bumps <- function() {
  fig_par()
  observations <- c(-2, -1, -.5, 1, 2)
  h <- .65
  grid <- seq(-4, 4, length.out = 1001)
  bumps <- sapply(observations, function(xi) dnorm(grid, xi, h) / length(observations))
  plot(grid, rowSums(bumps), type = "n", ylim = c(0, .28),
       xlab = "Value", ylab = "Density")
  matlines(grid, bumps, col = adjustcolor(blue, .5), lty = 2, lwd = 2)
  lines(grid, rowSums(bumps), col = rust, lwd = 3)
  rug(observations, col = ink, lwd = 2)
  legend("topright", c("Each bump: area 1/5", "Sum: area 1"),
         col = c(blue, rust), lty = c(2, 1), lwd = c(2, 3),
         bty = "n", cex = .9)
}

bandwidth_comparison <- function() {
  set.seed(100703)
  observations <- c(rnorm(70, -1.5, .6), rnorm(70, 1.5, .6))
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 3))
  for (h in c(.15, .5, 1.5)) {
    fig_par(); par(cex = .92)
    plot(density(observations, bw = h), col = blue, lwd = 3,
         main = paste("Bandwidth =", h), xlim = c(-4, 4),
         ylim = c(0, .55), xlab = "Same observed values", ylab = "Density")
    rug(observations, col = adjustcolor(ink, .3))
  }
}

boundary_caution <- function() {
  set.seed(100704)
  x <- runif(3000, 0, 10)
  fig_par()
  plot(density(x), xlim = c(-2, 12), ylim = c(0, .13), main = "",
       xlab = "Uniform waiting time (minutes)", ylab = "Density (per minute)",
       col = blue, lwd = 3)
  lines(c(-2, 0, 0, 10, 10, 12), c(0, 0, .1, .1, 0, 0),
        col = rust, lwd = 3, lty = 2)
  legend("topright", c("Kernel estimate", "Known uniform PDF"),
         col = c(blue, rust), lty = c(1, 2), lwd = 3, bty = "n", cex = .9)
}

curve_overlay <- function() {
  set.seed(100705)
  x <- rnorm(2000, 3, 5)
  fig_par()
  hist(x, breaks = 35, probability = TRUE, col = light_blue, border = "white",
       xlab = "Margin (pp)", ylab = "Density (per pp)", main = "",
       ylim = c(0, .10))
  lines(density(x), col = blue, lwd = 2)
  curve(dnorm(x, mean = 3, sd = 5), add = TRUE, col = rust, lwd = 3, lty = 2)
  legend("topright", c("Kernel estimate", "Known Normal PDF"),
         col = c(blue, rust), lwd = c(2, 3), lty = c(1, 2), bty = "n", cex = .9)
}

debate_samples <- function() {
  set.seed(100706)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 2))
  fig_par()
  barplot(c(.6, .3, .1), names.arg = c("A", "B", "Unsure"),
          col = c(blue, rust, green), ylim = c(0, .8),
          ylab = "Population probability", main = "Hypothetical debate verdict")
  props <- replicate(6, mean(rbinom(100, 1, .6)))
  fig_par()
  plot(1:6, props, ylim = c(.4, .8), pch = 19, col = blue,
       xlab = "Repeated sample of 100", ylab = "Sample proportion for A")
  abline(h = .6, col = rust, lty = 2, lwd = 2)
  text(1:6, props, sprintf("%.2f", props), pos = 3, cex = .95)
}

poll_sampling <- function() {
  set.seed(100707)
  phat <- rbinom(20000, 100, .6) / 100
  fig_par()
  hist(phat, breaks = seq(-.005, 1.005, by = .01), probability = TRUE,
       col = light_blue, border = "white", main = "",
       xlim = c(.4, .8), xlab = "Proportion favoring A in a sample of 100",
       ylab = "Density (per proportion unit)")
  abline(v = .6, col = rust, lwd = 2, lty = 2)
}

sample_size_replications <- function() {
  set.seed(100708)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 3))
  settings <- data.frame(n = c(25, 25, 100), B = c(200, 20000, 20000))
  for (j in 1:3) {
    n <- settings$n[j]; B <- settings$B[j]
    draws <- replicate(B, mean(runif(n, 0, 10)))
    fig_par(); par(cex = .9)
    hist(draws, breaks = seq(0, 10, by = .1), probability = TRUE,
         col = light_blue, border = "white", xlim = c(3, 7), ylim = c(0, 1.5),
         xlab = "Sample mean", ylab = "Density",
         main = paste0("n = ", n, ", B = ", B))
    abline(v = 5, col = rust, lty = 2)
  }
}

lln_paths <- function() {
  set.seed(100709)
  n <- 3000
  paths <- replicate(5, cumsum(rbinom(n, 1, .6)) / seq_len(n))
  fig_par()
  matplot(seq_len(n), paths, type = "l", lty = 1, lwd = 1.3,
          col = c(blue, rust, green, "#7A679B", "#A28136"),
          log = "x", ylim = c(0, 1),
          xlab = "Sample size n (log scale)", ylab = "Running sample proportion")
  abline(h = .6, lwd = 2, lty = 2, col = ink)
}

clt_panels <- function(family = c("uniform", "exponential")) {
  family <- match.arg(family)
  set.seed(if (family == "uniform") 100710 else 100711)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 3))
  B <- 15000
  for (n in c(1, 5, 30)) {
    if (family == "uniform") {
      xb <- replicate(B, mean(runif(n, 0, 10)))
      mu <- 5; sigma <- 10 / sqrt(12)
    } else {
      xb <- replicate(B, mean(rexp(n, rate = .2)))
      mu <- 5; sigma <- 5
    }
    z <- (xb - mu) / (sigma / sqrt(n))
    fig_par(); par(cex = .9)
    hist(z, breaks = seq(floor(min(z)), ceiling(max(z)) + .2, by = .2),
         probability = TRUE, col = light_blue, border = "white",
         xlim = c(-4, 5), ylim = c(0, if (family == "uniform") .5 else 1),
         xlab = "Standardized sample mean", ylab = "Density", main = paste("n =", n))
    curve(dnorm(x), add = TRUE, col = rust, lwd = 2)
  }
}

cdf_checkpoint_plot <- function() {
  set.seed(100718)
  n <- 5
  B <- 20000
  sample_means <- replicate(B, mean(rexp(n, rate = .2)))
  z_means <- (sample_means - 5) / (5 / sqrt(n))
  cutoffs <- c(-2, -1, 0, 1, 2)
  estimated <- sapply(cutoffs, function(z) mean(z_means <= z))
  normal <- pnorm(cutoffs)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  fig_par()
  par(mar = c(3.7, 4.2, 2.0, .8), cex = 1.05)
  plot(ecdf(z_means), verticals = TRUE, do.points = FALSE,
       xlim = c(-3, 3), ylim = c(0, 1), col = blue, lwd = 2,
       main = "Exponential sample means: n = 5, B = 20,000",
       xlab = "Cutoff z", ylab = "Cumulative probability")
  curve(pnorm(x), add = TRUE, col = rust, lwd = 2, lty = 2)
  points(cutoffs, estimated, pch = 19, col = blue)
  points(cutoffs, normal, pch = 1, col = rust)
  p_zero <- mean(z_means <= 0)
  segments(0, .5, 0, p_zero, col = green, lwd = 5)
  segments(.05, (p_zero + .5) / 2, .65, .36, col = green, lwd = 1.5)
  text(.7, .32, sprintf("At z = 0: %.3f vs. 0.500\nAbsolute gap = %.3f",
                        p_zero, abs(p_zero - .5)), adj = 0, col = ink, cex = .95)
  legend("topleft", c("Simulated CDF", "Standard Normal CDF"),
         col = c(blue, rust), lwd = 2, lty = c(1, 2), bty = "n", cex = .95)
  invisible(data.frame(cutoff = cutoffs, simulated = estimated,
                       normal = normal, absolute_gap = abs(estimated - normal)))
}

rare_event_clt <- function() {
  fig_par()
  k <- 0:30
  z <- (k / 30 - .02) / sqrt(.02 * .98 / 30)
  plot(z, dbinom(k, 30, .02), type = "h", lwd = 4, col = blue,
       xlim = c(-1.5, 6), ylim = c(0, .65),
       xlab = "Standardized sample proportion", ylab = "Probability mass")
  legend("topright", c("n = 30, p = 0.02", "P(no successes) = 0.98^30 = 0.545"),
         bty = "n", cex = .9)
}

chi_one <- function() {
  set.seed(100712)
  q <- rnorm(30000)^2
  fig_par()
  hist(q, breaks = seq(0, ceiling(max(q)) + .1, by = .2),
       probability = TRUE, xlim = c(0, 8), ylim = c(0, 2),
       col = light_blue, border = "white", main = "",
       xlab = "Squared standardized deviation", ylab = "Density")
  curve(dchisq(x, 1), from = .005, to = 8, add = TRUE, col = rust, lwd = 3, n = 1500)
  legend("topright", c("Simulated Z squared", "Chi-squared PDF, df = 1"),
         col = c(light_blue, rust), lwd = c(8, 3), bty = "n", cex = .9)
}

chi_family <- function() {
  fig_par()
  grid <- seq(.002, 22, length.out = 2000)
  plot(grid, dchisq(grid, 2), type = "l", col = blue, lwd = 3,
       ylim = c(0, .52), xlab = "Total squared standardized discrepancy Q",
       ylab = "Density")
  lines(grid, dchisq(grid, 5), col = rust, lwd = 3, lty = 2)
  lines(grid, dchisq(grid, 10), col = green, lwd = 3, lty = 3)
  legend("topright", c("df = 2", "df = 5", "df = 10"),
         col = c(blue, rust, green), lty = 1:3, lwd = 3, bty = "n")
}

variance_distribution <- function() {
  set.seed(100713)
  n <- 10; sigma <- 4
  q <- replicate(20000, (n - 1) * var(rnorm(n, 12, sigma)) / sigma^2)
  fig_par()
  hist(q, breaks = 55, probability = TRUE, col = light_blue, border = "white",
       xlim = c(0, 30), main = "", xlab = "(n - 1) S squared / sigma squared",
       ylab = "Density", ylim = c(0, .12))
  curve(dchisq(x, n - 1), add = TRUE, col = rust, lwd = 3)
  legend("topright", c("Repeated samples, n = 10", "Chi-squared PDF, df = 9"),
         col = c(light_blue, rust), lwd = c(8, 3), bty = "n", cex = .9)
}

denominator_distribution <- function() {
  fig_par()
  d <- seq(.001, 2.5, length.out = 1500)
  # Change of variable Q = nu D^2 supplies Jacobian 2 nu D.
  df <- c(4, 30)
  density_d <- function(x, nu) dchisq(nu * x^2, df = nu) * 2 * nu * x
  plot(d, density_d(d, df[1]), type = "l", col = blue, lwd = 3,
       ylim = c(0, 3.2), xlab = "Random denominator D = sqrt(Q / df)",
       ylab = "Density")
  lines(d, density_d(d, df[2]), col = rust, lwd = 3, lty = 2)
  abline(v = 1, lty = 3, col = ink)
  legend("topright", c("df = 4", "df = 30"), col = c(blue, rust),
         lty = c(1, 2), lwd = 3, bty = "n")
}

t_shapes <- function() {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 2))
  fig_par(); par(cex = 1)
  curve(dnorm(x), -5, 5, ylim = c(0, .42), lwd = 3, col = ink,
        xlab = "Standardized value", ylab = "Density", main = "Center")
  curve(dt(x, 4), add = TRUE, col = rust, lwd = 3, lty = 2)
  curve(dt(x, 30), add = TRUE, col = blue, lwd = 2, lty = 3)
  legend("topleft", c("Normal", "t, df = 4", "t, df = 30"),
         col = c(ink, rust, blue), lty = 1:3, lwd = c(3, 3, 2),
         bty = "n", cex = .85)
  fig_par(); par(cex = 1)
  curve(dnorm(x), 2, 5, ylim = c(0, .07), lwd = 3, col = ink,
        xlab = "Standardized value", ylab = "Density", main = "Right tail")
  curve(dt(x, 4), add = TRUE, col = rust, lwd = 3, lty = 2)
  curve(dt(x, 30), add = TRUE, col = blue, lwd = 2, lty = 3)
}

actual_t <- function() {
  set.seed(100714)
  n <- 10; mu <- 12; sigma <- 4
  draws <- replicate(20000, {
    sample_x <- rnorm(n, mu, sigma)
    c(z = (mean(sample_x) - mu) / (sigma / sqrt(n)),
      t = (mean(sample_x) - mu) / (sd(sample_x) / sqrt(n)))
  })
  fig_par()
  plot(density(draws["t", ]), xlim = c(-5, 5), ylim = c(0, .43),
       main = "", xlab = "Statistic across repeated samples", ylab = "Density",
       col = blue, lwd = 3)
  curve(dt(x, n - 1), add = TRUE, col = rust, lwd = 2, lty = 2)
  curve(dnorm(x), add = TRUE, col = ink, lwd = 2, lty = 3)
  legend("topright", c("Simulated t statistic", "t PDF, df = 9", "Normal PDF"),
         col = c(blue, rust, ink), lty = 1:3, lwd = c(3, 2, 2),
         bty = "n", cex = .9)
}
