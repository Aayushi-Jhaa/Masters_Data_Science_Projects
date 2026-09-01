X <- read.csv("Sleep_Assignment_1.csv", header = TRUE)
str(X)
head(X)
install.packages("GGally")
library(GGally)
GGally::ggpairs(X, progress = FALSE)
#1b
Y <- X
Y[, 2] <- log(X[, 2] + 10)
Y[, 3] <- log(X[, 3] + 1)
colnames(Y) <- c("Correct", "Sleep_diff", "Study_hours")
library(GGally)
GGally::ggpairs(Y, progress = FALSE)
#1c
#mean
ybar <- colMeans(Y)
#covariance matrix
Sy   <- cov(Y)
#correlation matrix
Ry   <- cor(Y)
ybar
Sy
Ry
#1f
Y_lm <- lm(Correct ~ Sleep_diff + Study_hours, data=Y)
summary(Y_lm)
#2
Sigma <- matrix(c(5,4,4,5),2,2,byrow=TRUE)
Sigma_half <- matrix(c(2,1,1,2),2,2)
Sigma_half %*% Sigma_half  # returns Sigma
#2a
Sigma <- matrix(c(5,4,4,5), nrow = 2, byrow = TRUE)
# Spectral decomposition
eig <- eigen(Sigma)          # Sigma = P Λ P^T
P <- eig$vectors
Lambda <- diag(eig$values)
P        
Lambda  
Sigma_half <- P %*% diag(sqrt(diag(Lambda))) %*% t(P)
Sigma_half
round(Sigma_half %*% Sigma_half, 6)
#2c
install.packages("mvtnorm")
install.packages("car")
library(mvtnorm)
library(car)
S<-diag(2)
mu<-c(0,0)
x<-rmvnorm(500,mean=mu, sigma=S)
plot(x, xlim=c(-6,10), ylim=c(-6,10))
ellipse(mu,S,sqrt(qchisq(0.95,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.68,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.3,2)), add=TRUE)
#1
mu <- c(2,4)
S  <- diag(2)
x  <- mvtnorm::rmvnorm(500, mean = mu, sigma = S)
plot(x, xlim=c(-6,10), ylim=c(-6,10), main="(1) mu=(2,4), S=I")
car::ellipse(mu,S,sqrt(qchisq(0.95,2)), add=TRUE)
car::ellipse(mu,S,sqrt(qchisq(0.68,2)), add=TRUE)
car::ellipse(mu,S,sqrt(qchisq(0.30,2)), add=TRUE)
#2
mu <- c(2,4)
S  <- diag(c(9,1))     # Λ
x  <- rmvnorm(500, mean = mu, sigma = S)
plot(x, xlim=c(-6,10), ylim=c(-6,10), main="(2) mu=(2,4), S=Λ")
ellipse(mu,S,sqrt(qchisq(0.95,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.68,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.30,2)), add=TRUE)
#3
mu <- c(2,4)
S  <- matrix(c(5,4,4,5), 2, 2, byrow = TRUE)   # Σ = P Λ P^T
x  <- rmvnorm(500, mean = mu, sigma = S)
plot(x, xlim=c(-6,10), ylim=c(-6,10), main="(3) mu=(2,4), S=Σ")
ellipse(mu,S,sqrt(qchisq(0.95,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.68,2)), add=TRUE)
ellipse(mu,S,sqrt(qchisq(0.30,2)), add=TRUE)


