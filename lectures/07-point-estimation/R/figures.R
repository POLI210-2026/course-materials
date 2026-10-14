# Original course figures: base R only, fixed seeds, no external data.
ink <- "#172D42"
blue <- "#176B87"
red <- "#B04C3E"
gold <- "#957000"
plot_style <- function(...) {
  old <- par(no.readonly = TRUE)
  par(mar = c(4, 4.2, 2.2, 1), las = 1,
             fg = ink, col.axis = ink, col.lab = ink, ...)
  # Undo the automatic font reduction for three-panel figures.
  par(cex = 1.05, cex.axis = .9, cex.lab = .95)
  old
}

shared_legend <- function(labels, colors, types) {
  # Draw in the reserved outer top margin, clear of all three data panels.
  par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), new = TRUE)
  plot(0, type = "n", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "",
       xlim = c(0, 1), ylim = c(0, 1))
  legend("top", labels, col = colors, lty = types, lwd = 2,
         horiz = TRUE, bty = "n", cex = .85, inset = .005)
}

sampling_flow <- function() {
  old <- par(mar = c(0, 0, 0, 0)); on.exit(par(old))
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(0, 4))
  rect(.1, 1.2, 2.3, 2.8, col = "#E8F1F5", border = blue)
  text(1.2, 2, "Population +\nstudy design", cex = 1.2)
  ys <- c(3.3, 2, .7)
  for (j in seq_along(ys)) {
    y <- ys[j]
    arrows(2.4, 2, 3.1, y, length = .08, col = blue)
    rect(3.2, y - .4, 5.5, y + .4, col = "#F2F5F6", border = blue)
    text(4.35, y, paste("Study", j), cex = 1.15)
    arrows(5.6, y, 6.1, y, length = .08, col = blue)
    text(6.6, y, bquote(hat(theta)[.(j)]), cex = 1.35)
    arrows(7.1, y, 7.8, 2, length = .08, col = blue)
  }
  rect(7.9, 1.1, 9.9, 2.9, col = "#E8F1F5", border = blue)
  text(8.9, 2, "Sampling\ndistribution", cex = 1.15)
}

turnout_estimates <- function(B = 10000, n = 200) {
  set.seed(210071)
  rbinom(B, n, .378) / n - rbinom(B, n, .315) / n
}
turnout_plot <- function() {
  old <- plot_style(); on.exit(par(old))
  x <- 100 * turnout_estimates()
  hist(x, breaks = 30, freq = FALSE, col = "#D4E7EF", border = "white",
       xlab = "Difference in turnout (percentage points)", ylab = "Density", main = "10,000 hypothetical repeated studies")
  abline(v = 6.3, col = red, lwd = 2, lty = 2)
  legend("topright", "True difference: 6.3 pp", col = red, lty = 2, lwd = 2, bty = "n")
}

consistency_plot <- function() {
  old <- plot_style(mfrow = c(1, 3), oma = c(0, 0, 2.2, 0)); on.exit(par(old))
  xx <- seq(0, 10, length.out = 1500)
  for (n in c(4, 16, 64)) {
    plot(xx, dnorm(xx, 5, 2 / sqrt(n)), type = "l", lwd = 2, col = blue,
         ylim = c(0, 1.7), xlab = "Estimate", ylab = "Density", main = paste("n =", n))
    lines(xx, dnorm(xx, 5, 2), col = gold, lwd = 2, lty = 2)
    lines(xx, dnorm(xx, 7, 2 / sqrt(n)), col = red, lwd = 2, lty = 3)
    abline(v = 5, lty = 3, col = "#666666")
  }
  shared_legend(c("Mean", "First observation", "Mean + 2"), c(blue, gold, red), 1:3)
}

.t_cache <- new.env(parent = emptyenv())
t_draws <- function(kind, B = 20000, sizes = c(5, 30, 100)) {
  key <- paste(kind, B, paste(sizes, collapse = "-"))
  if (exists(key, envir = .t_cache, inherits = FALSE)) return(.t_cache[[key]])
  stopifnot(kind %in% c("Normal", "Exponential"))
  set.seed(if (kind == "Normal") 210072 else 210073)
  result <- lapply(sizes, function(n) replicate(B, {
    x <- if (kind == "Normal") rnorm(n, 5, 5) else rexp(n, rate = 1 / 5)
    (mean(x) - 5) / (sd(x) / sqrt(n))
  }))
  names(result) <- sizes
  .t_cache[[key]] <- result
  result
}
t_cdf_plot <- function(kind) {
  values <- t_draws(kind)
  old <- plot_style(mfrow = c(1, 3), oma = c(0, 0, 2.2, 0)); on.exit(par(old))
  grid <- seq(-5, 5, length.out = 1500)
  for (i in seq_along(values)) {
    n <- as.integer(names(values)[i])
    plot(grid, pnorm(grid), type = "l", lwd = 2, lty = 3, col = gold,
         ylim = c(0, 1), xlab = "Studentized mean", ylab = "CDF", main = paste("n =", n))
    lines(grid, pt(grid, df = n - 1), lwd = 2, lty = 2, col = red)
    lines(grid, ecdf(values[[i]])(grid), col = blue, lwd = 2)
  }
  shared_legend(c("Simulation", "t reference", "N(0, 1)"), c(blue, red, gold), 1:3)
}
t_tail_table <- function(kind, sizes = c(5, 30, 100)) {
  values <- t_draws(kind)
  data.frame(n = sizes,
             left_sim = vapply(values, function(z) mean(z < -2), numeric(1)),
             right_sim = vapply(values, function(z) mean(z > 2), numeric(1)),
             each_t_tail = pt(-2, df = sizes - 1), each_Normal_tail = pnorm(-2),
             row.names = NULL)
}
population_plot <- function() {
  old <- plot_style(); on.exit(par(old))
  grid <- seq(-12, 30, length.out = 1200)
  plot(grid, dnorm(grid, 5, 5), type = "l", lwd = 2, col = blue,
       ylim = c(0, .21), ylab = "Density", xlab = "Observation", main = "Same mean (5) and SD (5); different shapes")
  lines(grid, dexp(grid, rate = 1 / 5), lwd = 2, col = red, lty = 2)
  legend("topright", c("Normal", "Exponential"), lty = 1:2, col = c(blue, red), lwd = 2, bty = "n")
}
cdf_concentration <- function() {
  old <- plot_style(mfrow = c(1, 3), oma = c(0, 0, 2.2, 0)); on.exit(par(old))
  grid <- seq(-1, 7, length.out = 1500)
  for (n in c(10, 50, 250)) {
    plot(grid, pnorm(grid, 2, 3 / sqrt(n)), type = "l", lwd = 2, col = blue,
         xlab = "Estimate", ylab = "CDF", main = paste("n =", n))
    lines(grid, pnorm(grid, 4, 3 / sqrt(n)), col = red, lwd = 2, lty = 2)
    abline(v = 2, lty = 3)
  }
  shared_legend(c("Centered at truth", "Centered at 4", "Truth = 2"), c(blue, red, "black"), 1:3)
}
