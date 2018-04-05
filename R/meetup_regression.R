
## load packages
install.packages(c("ggplot","magrittr","lubridate","dplyr","glmnet"))
library(ggplot2); library(magrittr); library(lubridate) ; library(dplyr)

# load data
data <- data.table::fread("clds.txt")
data <- data[,c(1,2,7,8)]
y <- data$Members
x <- data[,c(3,4)]
lambda <- 10^seq(10, -2, length = 100)

# split data 
train <- sample(569,200,replace = F)
test <- -train

# number of polynomial degrees
polys <- 10
train_score <- rep(0,polys-1)
test_score <- rep(0,polys-1)
test_rsq <- rep(0,polys-1)
train_rsq <- rep(0,polys-1)

# No regularisation

for(i in 1:(polys-1)){
  
  x_train <- cbind(poly(data$Date[train],degree = i+1),x[train,]) %>%  as.data.frame()
  names(x_train)[1:(i+1)] <- paste0("deg",1:(i+1))
  data_i <- x_train
  data_i$y_train <- y[train]
  
  lm <- lm(as.formula(paste0("y_train ~ ",paste0(names(x_train),collapse="+"))),data=data_i)
  
  train_pred <- predict(lm)
  train_score[i] <- sqrt(mean(c(train_pred -  data_i$y_train)^2))
  train_rsq[i] <- rsq(train_pred, data_i$y_train)
  
  
  x_test <- cbind(poly(data$Date[test],degree = i+1),x[test,]) %>% as.data.frame()
  names(x_test) <- names(lm$coefficients[-1])
  y_test <- y[test]
  
  test_pred <- predict(lm, newdata=x_test)
  test_score[i] <- sqrt(mean(c(test_pred - y_test)^2))
  test_rsq[i] <- rsq(test_pred,y_test)
  
}

df <- data.frame("RMSE"=c(test_score,train_score),"R2"=c(test_rsq,train_rsq),
                 "Sample"=c(rep("Test",polys-1),rep("Train",polys-1)),
                 "Degree"=c(2:polys,2:polys),
                 "Regularisation" = "Ordinary")
dfsave <- df
df <- reshape2::melt(df,measure.vars=c("RMSE","R2"))

ggplot(df,aes(x=Degree,y=value,color=Sample)) + ggtitle("No Regularisation") +
  geom_line() + scale_y_log10() +
  geom_point() + 
  facet_wrap(~variable,scales = "free_y") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 


####

## lasso


for(i in 1:(polys-1)){
  

  x_train <- cbind(poly(data$Date[train],degree = i+1),x[train,]) %>% as.matrix()
  y_train <- y[train]
  lm <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
  cv.out <- cv.glmnet(x_train, y_train, alpha = 1)
  
  train_pred <- predict(lm,newx = x_train, s=cv.out$lambda.min)
  train_score[i] <- sqrt(mean(c(train_pred - y_train)^2))
  train_rsq[i] <- rsq(train_pred,y_train)
  
  x_test <- cbind(poly(data$Date[test],degree = i+1),x[test,]) %>% as.matrix()
  y_test <- y[test]
  
  test_pred <- predict(lm, newx =x_test, s=cv.out$lambda.min)
  test_score[i] <- sqrt(mean(c(test_pred - y_test)^2))
  test_rsq[i] <- rsq(test_pred,y_test)
  
  
}

df <- data.frame("RMSE"=c(test_score,train_score),"R2"=c(test_rsq,train_rsq),
                 "Sample"=c(rep("Test",polys-1),rep("Train",polys-1)),
                 "Degree"=c(2:polys,2:polys),
                 "Regularisation" = "Lasso")
dfsave <- rbind(dfsave,df)

df <- reshape2::melt(df,measure.vars=c("RMSE","R2"))

ggplot(df,aes(x=Degree,y=value,color=Sample)) + ggtitle("Lasso Regularisation") +
  geom_line() + scale_y_log10() +
  geom_point() + 
  facet_wrap(~variable,scales = "free_y") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


####

## ridge


for(i in 1:(polys-1)){

  x_train <- cbind(poly(data$Date[train],degree = i+1),x[train,]) %>% as.matrix()
  y_train <- y[train]
  lm <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
  cv.out <- cv.glmnet(x_train, y_train, alpha = 0)
  
  train_pred <- predict(lm,newx = x_train, s=cv.out$lambda.min) %>% as.matrix()
  train_score[i] <- sqrt(mean(c(train_pred - y_train)^2))
  train_rsq[i] <- rsq(train_pred,y_train)
  
  x_test <- cbind(poly(data$Date[test],degree = i+1),x[test,]) %>% as.matrix()
  y_test <- y[test]
  
  test_pred <- predict(lm, newx=x_test, s=cv.out$lambda.min)
  test_score[i] <- sqrt(mean(c(test_pred - y_test)^2))
  test_rsq[i] <- rsq(test_pred,y_test)
 
  
}

df <- data.frame("RMSE"=c(test_score,train_score),"R2"=c(test_rsq,train_rsq),
                 "Sample"=c(rep("Test",polys-1),rep("Train",polys-1)),
                 "Degree"=c(2:polys,2:polys),
                 "Regularisation" = "Ridge")
dfsave <- rbind(dfsave,df)

df <- reshape2::melt(df,measure.vars=c("RMSE","R2"))

ggplot(df,aes(x=Degree,y=value,color=Sample)) + ggtitle("Ridge Regularisation") +
  geom_line() + scale_y_log10() +
  geom_point() + 
  facet_wrap(~variable,scales = "free_y") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



## full_compare

df <- reshape2::melt(dfsave,measure.vars=c("RMSE","R2"))

ggplot(df,aes(x=Degree,y=value,color=Sample)) + ggtitle("Ridge Regularisation") +
  geom_line() + scale_y_log10() + 
  geom_point() + 
  facet_grid(variable~Regularisation,scales = c("free")) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
