
data(iris)
library(dplyr)
X <- as.matrix(select(iris, -Species, -Sepal.Length))

y <- as.matrix(select(iris, Sepal.Length))
# vector of ones with same length as rows in Boston
int <- rep(1, length(y))

# Add intercept column to X
X <- cbind(int, X)


# magic happens
betas <- solve(t(X) %*% X) %*% t(X) %*% y

#predicted ys
yhat <- as.vector(X%*%betas)

#residuals
resid <- y - yhat

#total sum of squares
TSS <- sum((y - mean(y))^2)

#sum of squared errors
SSE <- sum(resid^2)

#sum of squares (model)

SSM <- TSS-SSE

#degrees of freedom for error
df.e <- length(y) - ncol(X) -1
s2 <- as.vector(SSE/df.e)

SEs <- sqrt(diag(solve(t(X) %*% X)))*sqrt(s2)
