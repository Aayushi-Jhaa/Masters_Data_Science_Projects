NBA_0203 <- read.csv("NBA_0203.csv")
head(NBA_0203)
#1a
S<-cov(NBA_0203[,2:14])
eig <- eigen(S)
eig_values <- eig$values
eig_vectors <- eig$vectors
pr <- prcomp(NBA_0203[, 2:14], scale = FALSE)
cbind(eigenvalues = eig_values, sdev_squared = pr$sdev^2)
#1b
x_i <- t(NBA_0203[226, 2:14])
xb  <- colMeans(NBA_0203[, 2:14])
# k = 1 reconstruction (for comparison)
alpha_1 <- eig_vectors[, 1]
y_1     <- as.numeric(t(alpha_1) %*% (x_i - xb))
approx_1 <- xb + y_1 * alpha_1
total_diff_1 <- sum(abs(x_i - approx_1))
# k = 4 reconstruction
alpha_4 <- eig_vectors[, 1:4]
y_4     <- t(alpha_4) %*% (x_i - xb)
approx_4 <- xb + alpha_4 %*% y_4
total_diff_4 <- sum(abs(x_i - approx_4))
total_diff_1
total_diff_4
#1d
pr_std <- prcomp(NBA_0203[,2:14], scale = TRUE)
summary(pr_std)
cumsum(pr_std$sdev^2 / sum(pr_std$sdev^2))
pr_std$sdev^2 > 1
sum(pr_std$sdev^2 > 1)
#1e
y<-prcomp(NBA_0203[,2:14], scale=TRUE)$x[,1:4]
Centers<-which(NBA_0203$player_height>=210)
par(mfrow = c(1, 2))
plot(y[,1],y[,2], pch=16,col="red")
points(y[Centers,1],y[Centers,2], pch=16,col="blue")
plot(y[,3],y[,4], pch=16,col="red")
points(y[Centers,3],y[Centers,4], pch=16,col="blue")
# 1f
# rotation (loadings) for the PC chosen in e) → PC1
alpha1 <- pr_std$rotation[, 1]
# three largest positive loadings
largest3  <- sort(alpha1, decreasing = TRUE)[1:3]
# three most negative loadings
smallest3 <- sort(alpha1)[1:3]
largest3
smallest3
biplot(pr_std, choices = c(1,2), scale = 0)

