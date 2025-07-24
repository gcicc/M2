# Set covariance matrix to mimic data
my.sigma <- rbind(c(1, -.4, 0),
                  c(-.4, 1, 0),
                  c(0, 0, 1))


require(mvtnorm)
# Create some data
set.seed(123)
my.data <- data.frame(rmvnorm(10000, sigma = my.sigma))
# run prinicpal components
# Running this multiple times will give different results
# PC vectors are orthogonal, but the sign can be flipped
my.pca <- prcomp(my.data)
my.pca

# Exercise 1: hold a single row fixed and bootstrap sample

set.seed(123)
subject.1.check <- bind_rows(
apply(matrix(1:1000), 1, function(x){
  
  x <- 1
  temp <- bind_rows(my.data %>% slice(x),
                    my.data %>% slice(-x) %>% sample_n(999, replace = TRUE))
  pca <- prcomp(temp)
  
  temp$PCA.db <- data.frame(prcomp(temp)$x) %>% mutate(PCA.db = PC1 + PC2) %>% pull(PCA.db)
  temp %>% slice(1)
}))

# Bimodal
hist(subject.1.check$PCA.db, breaks=100)

# four groups
set.seed(123)
subject.2.check <- bind_rows(
  apply(matrix(1:1000), 1, function(x){
    
    x <- 3
    temp <- bind_rows(my.data %>% slice(x),
                      my.data %>% slice(-x) %>% sample_n(999, replace = TRUE))
    temp$PCA.db <- data.frame(prcomp(temp)$x) %>% mutate(PCA.db = PC1 + PC2) %>% pull(PCA.db)
    temp$DB <- -0.7*temp$X1 + 0.7*temp$X2 + temp$X3
    temp %>% slice(1)
  }))

hist(subject.2.check$PCA.db, breaks=100)

# Conclusion - pca rotations need to be controled in terms of sign


check.1 <- bind_rows(
apply(matrix(1:1000), 1, function(x){
  
  x <- 1
  temp <- bind_rows(my.data %>% slice(x),
                    my.data %>% slice(-x) %>% sample_n(999, replace = TRUE))
  pca <- prcomp(temp)
  # Extract the first two principal components
  pca$rotation[,1:2]
  # If first positive of PC1 is positive, flip the sign
  if(pca$rotation[1,1] > 0){
    pca$rotation[,1] <- -pca$rotation[,1]
  }
  # if third position of PC2 is negative, flip the sign
  if(pca$rotation[3,2] < 0){
    pca$rotation[,2] <- -pca$rotation[,2]
  }
  
  # scale temp
  temp.scale <- scale(temp)
  # Multiply the data by the principal component matrix
  PC.1.2 <- temp.scale %*% pca$rotation[,1:2]
  # Add the two principal components together
  temp$DB <- PC.1.2[,1] + PC.1.2[,2]
  temp %>% slice(1)
}))

hist(check.1$DB, breaks=100)



check <- bind_rows(
  apply(matrix(1:1000), 1, function(x){
    
    x <- 3
    temp <- bind_rows(my.data %>% slice(x),
                      my.data %>% slice(-x) %>% sample_n(999, replace = TRUE))
    pca <- prcomp(temp)
    # Extract the first two principal components
    pca$rotation[,1:2]
    # If first positive of PC1 is positive, flip the sign
    if(pca$rotation[1,1] > 0){
      pca$rotation[,1] <- -pca$rotation[,1]
    }
    # if third position of PC2 is negative, flip the sign
    if(pca$rotation[3,2] < 0){
      pca$rotation[,2] <- -pca$rotation[,2]
    }
    
    # scale temp
    temp.scale <- scale(temp)
    # Multiply the data by the principal component matrix
    PC.1.2 <- temp.scale %*% pca$rotation[,1:2]
    temp$DB <- PC.1.2[,1] + PC.1.2[,2]
    temp %>% slice(1)
  }))

hist(check$DB, breaks=100)

