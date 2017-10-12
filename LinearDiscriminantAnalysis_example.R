data(iris)

library(dplyr)

X <- as.matrix(select(iris, -Species))

y <- select(iris, Species)

#calculate mean vectors by class
#within class scatter
mean_vectors <- aggregate(. ~ Species, data = iris, mean)[,-1]
groups <- split(iris, iris$Species)

wi_class_scat <- matrix(0, nrow=4, ncol=4)
for (g in 1:length(groups)){
  group <- data.matrix(groups[[g]][,-ncol(groups[[g]])])
  mean_vec <- as.numeric(mean_vectors[g,])
  mean_mat <-  matrix(mean_vec,nrow=nrow(group),ncol=length(mean_vec),byrow=TRUE)
  wi_class_scat_grp <- t(group - mean_mat) %*% (group - mean_mat)
  wi_class_scat <- wi_class_scat + wi_class_scat_grp
}


#calculate mean vectors by class
#between class scatter
overall_mean <- apply(select(iris, -Species), 2,mean)

bt_class_scat <- matrix(0, nrow=4, ncol=4)
for (g in 1:length(groups)){
  n <-   nrow(data.matrix(groups[[g]][,-ncol(groups[[g]])]))
  mean_vec <- as.numeric(mean_vectors[g,])
  bt_class_scat_grp <- n*(mean_vec - overall_mean) %*% t(mean_vec - overall_mean)
  bt_class_scat <- bt_class_scat + bt_class_scat_grp
}

#get the eigenvals and eigenvectors of S_w^1S_b

S_mat <- solve(wi_class_scat)%*%bt_class_scat
S.e <- eigen(S_mat)
e_vals <- S.e$values
e_vecs <- S.e$vectors

#get variance explained

e_vals_var <- as_data_frame(e_vals) %>% mutate( var_exp = value/sum(value))

#Find the vectors above a threshold to create LDA subspace transformation matrix

thresh <- .005
e_vals_num <- nrow(filter(e_vals_var, var_exp > thresh))
W <- e_vecs[,1:e_vals_num] 

#transform observations into this space

X_lda <- X %*% W

dat <- as.data.frame(cbind(y,X_lda))
names(dat) <- c("Species", "x","y")

library(ggplot2)
ggplot(dat, aes(x=x, y=y, color=Species)) +geom_point() +theme_classic()



