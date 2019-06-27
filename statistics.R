# -----------------------------
# Understand statistics
# ------------------------------

rm(list=ls())
library(tidyverse)
library(magrittr)

# Create linear models:
mtcars %<>% 
  transmute(gpm = 200/mpg,
            disp,
            wt,
            hp)
mod1 <- lm(gpm ~ disp,data = mtcars)
mod2 <- lm(gpm ~ wt,data = mtcars)
mod3 <- lm(gpm ~ hp,data = mtcars)
mod4 <- lm(gpm ~ disp + wt + hp,data = mtcars)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


plot(mtcars$wt,mtcars$gpm)
abline(1,3)


# OLS (Ordinary Least squares) method
X <- cbind(1,mtcars$wt)
y <- mtcars$gpm
beta <- c(1.23,3)

plot(X[,2],y)
lines(X[,2], X%*%beta)

# How to find the best beta values?
# Minimise the following objective function:
# Sum of squared residuals
SSR <- function(y,X,beta){
  sum((y - X%*%beta)^2)
}
SSR(y,X, beta = c(1.23,2.98))

# Create 3D plot of SSR:
library(reshape2)
library(rgl)
b1 <- seq(-10,10,0.1)
b2 <- seq(0,5,0.05)
df <- expand.grid(b1=b1, b2=b2)
df$y <- apply(df,1,function(betas) SSR(y,X,betas))
plot3d(df)
# This is the minimum:
points3d(1.23,2.98,69.63, col="yellow", size=16)

# OLS: solve the following equation by beta:
t(X) %*% X %*% beta == t(X) %*% y
# This is the solution, beta:
beta <- solve(t(X) %*% X) %*% t(X) %*% y

# What is the R-squared of this regression?
# (Explained Variance / Total Variance)
Py <- X %*% beta
Explained <- sum((Py - mean(y))^2)
Total <- sum((y - mean(y))^2)
Explained/Total

# In the simple regression case:
beta_hat  <- cov(X[,2], Y) / var(X[,2])
alpha_hat <- mean(y) - beta_hat * mean(X[,2])


# ---- Maximum Likelihood -----------
# OLS is identical to maximum likelihood if the errors are normally distributed:

loglik <- function(y,mu,s2){
  n <- length(y)
  A <- -n/2*log(2*pi*s2)
  B <- -1/(2*s2) * sum((y-mu)^2)
  A + B
}

loglik(y = y, mu = 9,s2 = 2)

# Create 3D plot of log-likelihood:
mu <- seq(9,12,0.1)
s2 <- seq(5,40,1)
df <- expand.grid(mu=mu, s2=s2)
df$y <- apply(df,1,function(x) loglik(y,x[1], x[2]))
plot3d(df)
# This is the maximum point:
points3d(10.8,10,-82.97,col='red', size=16)

# Estimates from maximizing log-likelihood:
mu_est <- mean(mtcars$gpm)
s2_est <- 1/n * sum( (y-mu_est)^2 )
