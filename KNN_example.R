
library(boot)
n <- 100

x.1 <- rnorm(n)
x.2 <- rnorm(n)
X <- x.1 + x.2
prob <- inv.logit(X)
y<-rbinom(n, size=1, prob)

dat <- cbind(x.1, x.2, y)

Knn <- function(dat, x_ind, y_ind,k){
  distmat <- as.matrix(dist(dat[,x_ind], method = "euclidean", upper=TRUE))
  Ys <- dat[,y_ind]
  preds <- rep(NA, n)
  for (i in 1:n){
    neighbors <- tail(sort(distmat[i,]),n-1)
    neighbor_indices <- as.numeric(names(head(sort(neighbors),k)))
    pred_prob <- mean(Ys[neighbor_indices])
    pred <- ifelse(pred_prob >.5, 1, 0)
    preds[i] <- pred
  }
  return(preds)
}

Y_hats <- Knn(dat,x_ind = c(1,2),y_ind=3,k=5)
y_test <- y-Y_hats


