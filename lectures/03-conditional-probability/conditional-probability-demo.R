# POLI 210, Seminar 3: Conditional probability and Bayes' rule
# Brad LeVeck | September 16, 2026
# All political examples use hypothetical models, not empirical estimates.
# Run sections in order. Each simulation sets its seed ONCE, before repetition.
# The ## ---- labels let the slides/notes display and run this same code.
# Base R only. In RStudio, select a section and use Cmd+Enter / Ctrl+Enter.

## ---- arrest-analytic
prior <- 0.15
true_arrest <- 0.90
false_arrest <- 0.20
p_arrest <- prior * true_arrest + (1 - prior) * false_arrest
p_guilty_given_arrest <- prior * true_arrest / p_arrest
c(arrest = p_arrest, guilty_given_arrest = p_guilty_given_arrest)

## ---- arrest-sim
set.seed(2100916)
n <- 100000
guilty <- sample(c(TRUE, FALSE), n, replace = TRUE,
                 prob = c(0.15, 0.85))
arrested <- logical(n)
for (i in seq_len(n)) {
  p <- if (guilty[i]) 0.90 else 0.20
  arrested[i] <- sample(c(TRUE, FALSE), 1, prob = c(p, 1 - p))
}

## ---- arrest-results
c(analytic = p_guilty_given_arrest,
  simulated = sum(guilty & arrested) / sum(arrested))
mean(guilty[arrested])  # Same fraction, using logical subsetting

## ---- pick-one
pick_one <- function(x) {
  x[sample.int(length(x), size = 1)]
}
## ---- pick-one-explanation
# Sampling an index also works when x is a single number, e.g., c(3).
# In contrast, sample(3, 1) draws from 1:3, not just from the value 3!

## ---- monty-function
monty_once <- function() {
  car <- pick_one(1:3)
  first <- 1  # By symmetry, fix our initial choice
  goats <- setdiff(1:3, c(first, car))
  opened <- pick_one(goats)
  switched <- setdiff(1:3, c(first, opened))
  c(stay = first == car, switch = switched == car,
    opened = opened, car = car)
}

## ---- monty-one
set.seed(210)
monty_once()

## ---- monty-repeat
set.seed(2100917)
n_games <- 100000
games <- replicate(n_games, monty_once())
head(t(games))  # One row per game when we transpose

## ---- monty-results
monty_comparison <- rbind(
  analytic = c(stay = 1/3, switch = 2/3),
  simulated = rowMeans(games[c("stay", "switch"), ])
)
round(monty_comparison, 4)

## ---- monty-conditional
opened_3 <- games["opened", ] == 3
sum(opened_3)  # Denominator: games in which Monty opened door 3
mean(games["switch", opened_3])

## ---- monty-convergence
trials <- seq_len(n_games)
running_switch <- cumsum(games["switch", ]) / trials
keep <- seq(100, n_games, by = 100)
par(mar = c(4, 4, 0.7, 0.8), las = 1)
plot(trials[keep], running_switch[keep], type = "l",
     xlab = "Games simulated", ylab = "Switch win proportion",
     ylim = c(0.55, 0.78), lwd = 2, col = "#005a8b", xaxt = "n")
axis(1, at = seq(0, n_games, by = 20000),
     labels = format(seq(0, n_games, by = 20000), big.mark = ",",
                     scientific = FALSE, trim = TRUE))
abline(h = 2/3, lty = 2, lwd = 2)
legend("bottomright", c("Simulation", "Analytical: 2/3"),
       lty = c(1, 2), lwd = 2, col = c("#005a8b", "black"), bty = "n")

## ---- coalition-function
coalition_once <- function(start = 1, total = 4, p_gain = 0.5) {
  support <- start
  while (support > 0 && support < total) {
    step <- sample(c(-1, 1), size = 1,
                   prob = c(1 - p_gain, p_gain))
    support <- support + step
  }
  support == total
}

## ---- coalition-repeat
set.seed(2100918)
n_contests <- 20000
coalition_wins <- replicate(n_contests, coalition_once())
c(analytic = 1/4, simulated = mean(coalition_wins))

## ---- coalition-starts
set.seed(2100919)
estimates <- sapply(0:4, function(i) {
  mean(replicate(n_contests, coalition_once(start = i)))
})
data.frame(start = 0:4, analytic = (0:4)/4,
           simulated = round(estimates, 4))

## ---- optional-coalition-bias
# Optional extension, after the core lecture.
# Book's general formula: [1 - ((1-p)/p)^i] / [1 - ((1-p)/p)^N].
# This expression applies for 0 < p < 1 and p != 1/2.
set.seed(2100920)
p_gain <- 0.60
ratio <- (1 - p_gain) / p_gain
biased_analytic <- (1 - ratio) / (1 - ratio^4)
biased_sim <- mean(replicate(n_contests,
                            coalition_once(p_gain = p_gain)))
c(analytic = biased_analytic, simulated = biased_sim)
