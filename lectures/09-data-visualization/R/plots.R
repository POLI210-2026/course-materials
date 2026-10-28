source("R/setup.R")
p_scatter <- ggplot(e2014, aes(dvp, dv)) + geom_point(alpha = .55, size = 1.8) +
  labs(x = "Prior Democratic House vote (%)", y = "Democratic House vote, 2014 (%)")
p_color <- ggplot(e2014, aes(dvp, dv)) +
  geom_point(aes(color = incumbent), alpha = .7, size = 1.8) +
  scale_color_manual(values = inc_colors) + labs(x = "Prior Democratic House vote (%)",
    y = "Democratic House vote, 2014 (%)", color = "Incumbent")
p_fit <- p_color + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black")
p_facets <- ggplot(recent, aes(dvp, dv)) + geom_point(alpha = .4, size = .9) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#777777") +
  facet_wrap(~year, nrow = 2) + labs(x = "Prior Democratic House vote (%)", y = "Democratic House vote (%)")
p_pres <- ggplot(pres, aes(dpres, dv)) + geom_point(alpha = .14, size = .7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  labs(x = "Democratic presidential vote (%)", y = "Democratic House vote (%)")
p_ideology <- ggplot(matched, aes(dpres, dv)) +
  geom_point(aes(color = nominate_dim1), alpha = .35, size = .8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  scale_color_gradient2(low = "#2166ac", mid = "#dedede", high = "#b35806", midpoint = 0) +
  labs(x = "Democratic presidential vote (%)", y = "Democratic House vote (%)",
       color = "Following member's\nNOMINATE dimension 1")
p_ideology_recent <- ggplot(filter(matched, year == 2014), aes(dpres, dv)) +
  geom_point(aes(color = nominate_dim1), alpha = .75, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  scale_color_gradient2(low = "#2166ac", mid = "#dedede", high = "#b35806", midpoint = 0) +
  labs(x = "Democratic presidential vote (%)", y = "Democratic House vote, 2014 (%)",
       color = "Following member's\nNOMINATE dimension 1")
p_hist <- ggplot(e2014, aes(dpres)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "white", fill = "#2166ac") +
  labs(x = "Democratic presidential vote (%)", y = "Districts in the teaching extract")
p_counts <- ggplot(e2014, aes(incumbent)) + geom_bar(fill = "#2166ac") +
  labs(x = "Incumbent", y = "Districts in the teaching extract")
means <- e2014 |> group_by(incumbent) |>
  summarise(mean_dv = mean(dv), n = n(), .groups = "drop")
p_means <- ggplot(means, aes(incumbent, mean_dv)) + geom_col(fill = "#2166ac", width = .7) +
  labs(x = "Incumbent", y = "Mean Democratic House vote (%)")
vote_long <- e2014 |> select(incumbent, dv, dvp) |>
  pivot_longer(c(dv, dvp), names_to = "election", values_to = "vote") |>
  mutate(election = factor(election, levels = c("dvp", "dv"), labels = c("Prior", "2014")))
vote_summary <- vote_long |> group_by(incumbent, election) |>
  summarise(mean_vote = mean(vote), n = n(), se = sd(vote)/sqrt(n()), .groups = "drop")
dodge <- position_dodge(width = .8)
p_dodge <- ggplot(vote_summary, aes(incumbent, mean_vote, fill = election)) +
  geom_col(position = dodge, width = .7) +
  scale_fill_manual(values = c(Prior = "#b3cde3", `2014` = "#2166ac")) +
  labs(x = "Incumbent", y = "Mean Democratic House vote (%)", fill = "Election")
p_error <- p_dodge + geom_errorbar(aes(ymin = mean_vote - se, ymax = mean_vote + se),
                                   position = dodge, width = .18)
p_box <- ggplot(house, aes(party, nominate_dim1, fill = party)) + geom_boxplot(width = .55) +
  scale_fill_manual(values = party_colors, guide = "none") +
  labs(x = "Party", y = "NOMINATE dimension 1")
p_qq <- ggplot(house, aes(sample = nominate_dim1)) + stat_qq(alpha = .6) +
  stat_qq_line(color = "#b35806") + facet_wrap(~party) +
  labs(x = "Standard Normal quantile", y = "Observed NOMINATE quantile")
