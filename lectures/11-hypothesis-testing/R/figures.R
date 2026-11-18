# Internal rendering helper. The student demo is self-contained.
# Reuse its code so slide numbers/plots and demonstration results agree.
.demo_script <- tempfile(fileext = '.R')
knitr::purl('hypothesis-testing-demo.qmd', output = .demo_script, quiet = TRUE)
source(.demo_script, local = TRUE)
unlink(.demo_script)

z_ref <- data.frame(z = seq(-4, 4, length.out = 1601))
z_ref$density <- dnorm(z_ref$z)
p_rejection <- ggplot(z_ref, aes(z, density)) +
  geom_area(data = subset(z_ref, z < -qnorm(.975)), fill = orange, alpha = .6) +
  geom_area(data = subset(z_ref, z > qnorm(.975)), fill = orange, alpha = .6) +
  geom_line(color = blue, linewidth = 1) +
  geom_vline(xintercept = c(-1, 1) * qnorm(.975), linetype = 2) +
  annotate('text', x = c(-2.8, 0, 2.8), y = .17,
           label = c('Reject\n2.5%', 'Do not reject\n95%', 'Reject\n2.5%'), size = 4.5) +
  labs(x = 'Standardized statistic under H0', y = 'Density')

power_panels <- expand.grid(difference = seq(-9, 14, length.out = 1201),
                           delta = c(0, 3, 6))
power_se <- 10 * sqrt(2 / 50)
power_cut <- qnorm(.975) * power_se
power_panels$density <- dnorm(power_panels$difference, power_panels$delta, power_se)
power_panels$null_density <- dnorm(power_panels$difference, 0, power_se)
power_panels$panel <- factor(power_panels$delta, levels = c(0, 3, 6),
  labels = sprintf('True difference = %s; rejection = %.2f', c(0, 3, 6), normal_power(c(0, 3, 6), 50)))
p_power_regions <- ggplot(power_panels, aes(difference, density)) +
  geom_line(aes(y = null_density), color = 'gray65', linewidth = .7, linetype = 2) +
  geom_area(data = subset(power_panels, difference < -power_cut), fill = orange, alpha = .65) +
  geom_area(data = subset(power_panels, difference > power_cut), fill = orange, alpha = .65) +
  geom_line(color = blue, linewidth = 1) +
  geom_vline(xintercept = c(-power_cut, power_cut), linetype = 3) +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10)) +
  labs(x = 'Estimated difference (score points)', y = 'Density',
       subtitle = 'Same null cutoffs in every panel; n = 50 per group; population SD = 10') +
  theme(strip.text = element_text(size = 10))
