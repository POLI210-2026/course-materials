# POLI 210, Seminar 2 — September 9, 2026
# Run sections interactively in RStudio. All examples use base R.
# For each simulation: describe ONE trial before running many trials.

# 1. Enumerate equally likely ordered outcomes.
S <- expand.grid(first = 1:6, second = 1:6)
S
E <- S$first + S$second == 7
S[E, ]
sum(E)
mean(E)  # exact here, because every equally likely outcome appears once

# 2. Generate draws, not a list of all outcomes.
set.seed(210)
sample(1:6, 12, replace = TRUE)
sample(1:6, 2, replace = FALSE)
# Predict first: is sample(2:12, 1) a correct model of a two-dice sum?

# 3. Two-dice simulation: each aligned pair of vector entries is one trial.
set.seed(211)
N <- 20000
first <- sample(1:6, N, replace = TRUE)
second <- sample(1:6, N, replace = TRUE)
hit <- first + second == 7
head(data.frame(first, second, hit))
c(estimate = mean(hit), exact = 1/6)
plot(seq_len(N), cumsum(hit)/seq_len(N), type = "l", col = "#005a8b",
     xlab = "Trials", ylab = "Event proportion", ylim = c(.10, .25))
abline(h = 1/6, col = "#b5482e", lty = 2)

# 4. Experiment: independently assign three participants.
set.seed(212)
assignment <- sample(c("C", "T"), 3, replace = TRUE)
assignment
any(assignment == "C") & any(assignment == "T")
set.seed(213)
mixed <- replicate(20000, {
  a <- sample(c("C", "T"), 3, replace = TRUE)
  any(a == "C") & any(a == "T")
})
c(estimate = mean(mixed), exact = 6/8)
# New design: guarantee exactly one treated person.
sample(c("T", "C", "C"), 3, replace = FALSE)

# 5. Hypothetical polling; 1 = approve, 0 = do not approve.
set.seed(214)
responses <- sample(c(0, 1), 20, replace = TRUE, prob = c(.40, .60))
mean(responses)
# Two independent responses: agreement is 00 or 11, with unequal weights.
exact_agreement <- .40^2 + .60^2
set.seed(2141)
response1 <- sample(c(0, 1), N, replace = TRUE, prob = c(.40, .60))
response2 <- sample(c(0, 1), N, replace = TRUE, prob = c(.40, .60))
c(estimate = mean(response1 == response2), exact = exact_agreement)

# Repeated polls: distinguish sample size (100) from repetitions (10,000).
set.seed(215)
poll_share <- replicate(10000, {
  x <- sample(c(0, 1), 100, replace = TRUE, prob = c(.40, .60))
  mean(x)
})
mean(poll_share < .50)
hist(poll_share, breaks = seq(0, 1, .02), col = "#bddde9", border = "white",
     main = "", xlim = c(.35, .85), xlab = "Approval share", ylab = "Polls")
abline(v = c(.50, .60), col = c("#b5482e", "#005a8b"), lty = c(2, 1))

# 6. Count and simulate committees: four incumbents, two newcomers.
choose(4, 2) / choose(6, 2)
set.seed(216)
both_incumbents <- replicate(20000, {
  committee <- sample(1:6, 2, replace = FALSE)
  all(committee <= 4)
})
mean(both_incumbents)
# Pause for the in-class exercise before running this answer.
choose(2, 1) * choose(4, 1) / choose(6, 2)
one_newcomer <- replicate(20000, {
  committee <- sample(1:6, 2, replace = FALSE)
  sum(committee > 4) == 1
})
mean(one_newcomer)
