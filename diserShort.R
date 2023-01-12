# Packages
library(MASS)
library(car)
library(dplyr)


# trans x2 to dummy
trans <- function(datta){
  datta$dummi <- ifelse(datta$x2 >= mean(datta$x2), 1, 0)
  datta <- subset(datta, select = c(x1, dummi))
  names(datta) <- c("x1", "x2")
  return(datta)
}


# Inverse logit
inv.logit <- function(x){
  return(exp(x)/(1 + exp(x)))
}

# MCAR
mcar <- function(datta){
  datta$miss <- rbinom(n, 1, prob = 0.2)
  datta <- subset(datta, miss == 0)
}


# MAR
mar <- function(datta){
  datta1 <- subset(datta, x2 == 1)
  datta0 <- subset(datta, x2 == 0)
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.09)
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.03)
  datta <- rbind(datta1, datta0)
  datta <- subset(datta, miss == 0)
}


# MNAR
mnar <- function(datta){
  datta1 <- subset(datta, y >= mean(datta$y))
  datta0 <- subset(datta, y < mean(datta$y))
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.09)
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.03)
  datta <- rbind(datta1, datta0)
  datta <- subset(datta, miss == 0)
}
`

# IR 5%
ir5 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.05)
  v <- n - u
  datta1 <- datta[sample(nrow(datta1), u), ]
  datta0 <- datta[sample(nrow(datta0), v), ]
  datta <- rbind(datta1, datta0)
}


# IR 10%
ir10 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.1)
  v <- n - u
  datta1 <- datta[sample(nrow(datta1), u), ]
  datta0 <- datta[sample(nrow(datta0), v), ]
  datta <- rbind(datta1, datta0)
}


# IR 30%
ir30 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.3)
  v <- n - u
  datta1 <- datta[sample(nrow(datta1), u), ]
  datta0 <- datta[sample(nrow(datta0), v), ]
  datta <- rbind(datta1, datta0)
}


# IR 50%
ir50 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  datta1 <- datta[sample(nrow(datta1), n/2), ]
  datta0 <- datta[sample(nrow(datta0), n/2), ]
  datta <- rbind(datta1, datta0)
}


# Complete data
full <- function(datta){
  datta <- datta[sample(nrow(datta), n[j]), ]
  return(datta)
}

# Specifications
set.seed(123456)
reps <- 100
results <- matrix(NA, nrow = reps, ncol = 8)

b0 <- 0.9
b1 <- 0.3
b2 <- 0.2
b3 <- 0.7
n <- c(120, 80)

samples = 50000
r = 0.3

my_data = mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
my_data <- as.data.frame(my_data)
names(my_data) <- c("x1", "x2")
my_data <- trans(my_data)

for(j in 1:length(n)){
  for(i in 1:reps){
    my_data <- my_data %>% mutate(y = rbinom(samples, 1, inv.logit(b0 + b1*x1 + b2*x2 + b3*(x1*x2))))
    mod1 <- glm(y ~ x1*x2, data = full(my_data), family = binomial (link = logit))
    lrt <- car::Anova(mod1)
    results[i, 1] <- mod1$coef[1]
    results[i, 2] <- mod1$coef[2]
    results[i, 3] <- mod1$coef[3]
    results[i, 4] <- mod1$coef[4]
    results[i, 5] <- summary(mod1)$coefficient[4, 4]
    results[i, 6] <- lrt$"Pr(>Chisq)"[3]
    results[i, 7] <- 1
    results[i, 8] <- n[j]
    results <- as.data.frame(results)
    names(results) <- c("b0", "b1", "b2", "b3", "Wald", "lrt", "cond", "sample")
    results <- round(results, 2)
  }
  print(colMeans(results))
}
