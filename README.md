# ML.R-edx
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

library(caret)
set.seed(1)

myRMSE <- function(size){
  set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  
RMSE <- replicate(n = size, {
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)test_set <- dat %>% slice(test_index)
    
# Train linear model
fit <- lm(y ~ x, data = train_set)
    
# Loss Function
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
f<-sapply(n, myRMSE)
f

 
-------------
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
--------
set.seed(1, sample.kind="Rounding")

n <- c(100, 500, 1000, 5000, 10000)

# function for question 2
ques2 <- sapply(n,function(n){
  # build data set
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  # builds 100 linear models, returns vector of RMSEs
  nicks <- replicate(100, {
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    model <- lm(y ~ x, data=train)
    y_hat <- predict.lm(model, test)
    RMSE(test$y, y_hat)
  })
  
  # calculate mean and standard deviation
  c(ss= mean(nicks),xx= sd(nicks))
})
ques2
