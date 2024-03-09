library(data.table)
# Research problem 1 Have effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 decision_accuracy ], y = the.dat[Group == "Control", decision_accuracy ], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", decision_accuracy  := round(x = rnorm(n = .N, mean = 6, sd = 1.87), digits = 1)]
sim.dat[Group == "Treatment", decision_accuracy  := round(x = rnorm(n = .N, mean = 6.18, sd = 1.85), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

fwrite(sim.dat, file = "decision_accuracy_effect.csv")


data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

fwrite(exp.results, file = "experiment_results_RQ1_effect.csv")

# Research problem 2 have effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 decision_time ], y = the.dat[Group == "Control", decision_time ], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", decision_time := round(x = rnorm(n = .N, mean = 80, sd = 80), digits = 1)]
sim.dat[Group == "Treatment", decision_time := round(x = rnorm(n = .N, mean = 78, sd = 10), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

fwrite(sim.dat, file = "decision_time_effect.csv")

data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

fwrite(exp.results, file = "experiment_results_RQ2_effect.csv")

# Research problem 3 have effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 sleep_quality], y = the.dat[Group == "Control", sleep_quality], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", sleep_quality := round(x = rnorm(n = .N, mean = 0.5, sd = 0.3), digits = 1)]
sim.dat[Group == "Treatment", sleep_quality := round(x = rnorm(n = .N, mean = 0.525, sd = 0.05), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

fwrite(sim.dat, file = "sleep_quality_effect.csv")

data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

fwrite(exp.results, file = "experiment_results_RQ3_effect.csv")

# Research problem 1 no effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 decision_accuracy], y = the.dat[Group == "Control", decision_accuracy], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", decision_accuracy := round(x = rnorm(n = .N, mean = 6, sd = 1.87), digits = 1)]
sim.dat[Group == "Treatment", decision_accuracy := round(x = rnorm(n = .N, mean = 6, sd = 1.87), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

fwrite(sim.dat, file = "decision_accuracy_no_effect.csv")


data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

fwrite(exp.results, file = "experiment_results_RQ1_no_effect.csv")


# Research problem 2 no effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 decision_time], y = the.dat[Group == "Control", decision_time], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", decision_time := round(x = rnorm(n = .N, mean = 80, sd = 80), digits = 1)]
sim.dat[Group == "Treatment", decision_time := round(x = rnorm(n = .N, mean = 80, sd = 80), digits = 1)]
dim(sim.dat)

fwrite(sim.dat, file = "decision_time_no_effect.csv")

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

fwrite(exp.results, file = "experiment_results_RQ2_no_effect.csv")

# Research problem 3 no effect
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 sleep_quality], y = the.dat[Group == "Control", sleep_quality], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

B <- 1000
n <- 4548
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", sleep_quality := round(x = rnorm(n = .N, mean = 0.5, sd = 0.1), digits = 1)]
sim.dat[Group == "Treatment", sleep_quality := round(x = rnorm(n = .N, mean = 0.5, sd = 0.1), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]

fwrite(sim.dat, file = "sleep_quality_no_effect.csv")


data.table(data = round(x = exp.results[1:10, ], digits = 3),
           rownames = F)

exp.results[, mean(p < 0.05)]
exp.results[, summary(effect)]
alpha = 0.05
exp.results[, quantile(x = effect, probs = c(alpha, 1 -
                                               alpha))]

f_p <- sum(exp.results >= 0.05) / 1000 * 100
t_n <- sum(exp.results < 0.05) / 1000 * 100
print(paste("% of False Positives:", f_p))
print(paste("% of True Negatives:",t_n))

fwrite(exp.results, file = "experiment_results_RQ3_no_effect.csv")
getwd()
