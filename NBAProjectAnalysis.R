### Constructing desired data structure
x <- read.csv("C:\\Users\\Jacob\\Downloads\\NBA_train.csv")
colnames(x) <- c(colnames(x)[1:8],"2P","2PA","3P","3PA",colnames(x)[13:20])
colnames(x) <- make.names(colnames(x))
x$FG_per <- 100*x$FG/x$FGA
x$X2P_per <- 100*x$X2P/x$X2PA
x$X3P_per <- 100*x$X3P/x$X3PA
Expand <-function(x,y){
  x1 <- subset(x,SeasonEnd!=1980:1980+(y-1))[,1:4]
  for(p in 1:(y)){
    x1[paste0(colnames(x)[3:ncol(x)],p)] <- -1
  for (j in 1:nrow(x1)) {
    z <- x[x[,1] == x1[,1][j]-p,]
    z1 <- z[z[,2] == x1[,2][j],] 
    if(nrow(z1) == 0) next
    for(i in 3:ncol(x)){
      z2 <- z1[,i]
      x1[,paste0(colnames(x)[i],p)][j] <- z2
    }
  }
  }
  return(subset(x1,x1[,ncol(x1)] != -1))
}
y <- read.csv("C:\\Users\\Jacob\\Downloads\\NBA_test.csv")
Expand1 <-function(x1,x2,y){
  x1 <- x1[,1:4]
  for(p in 2:(y+1)){
    x1[paste0(colnames(x2)[3:ncol(x)],p)] <- -1
    for (j in 1:nrow(x1)) {
      z <- x2[x2[,1] == x1[,1][j]-p,]
      q <- factor(x1[,2][j],levels = levels(z[,2]))
      z1 <- z[z[,2] == q,]
      if(length(z1[,3])!=1) next
      for(i in 3:ncol(x2)){
        z2 <- z1[,i]
        x1[,paste0(colnames(x2)[i],p)][j] <- z2
      }
    }
  }
  return(subset(x1,x1[,ncol(x1)] != -1))
}
x_1 <- Expand(x,1)
x_1$FG1 <- NULL
x_1$FGA1 <- NULL
x_1$PTS1 <- NULL
y <- x_1[,4]
x <- x_1[,5:ncol(x_1)]
y_test <- y[x_1$SeasonEnd == 2011]
y_train <- y[x_1$SeasonEnd < 2011]
x_test <- x[x_1$SeasonEnd == 2011,]
x_train <- x[x_1$SeasonEnd < 2011,]
model_train <- model.matrix(y_train~., data = x_train)
model_test <- model.matrix(y_test~., data = x_test)

### Libraries

library(leaps)
library(glmnet)
library(pls)
library(caret)
library(regclass)

### Diagnostics

cor(cbind(data.frame(W = y),x))
MSE_naive <- mean((x_train$W1-y_train)^2)
trControl1 <- trainControl(method  = "cv",number  = 10)
set.seed(1)

### Linear Regression Model

lm_fit = lm(y_train ~ .,data = x_train)
summary(lm_fit)
VIF(lm_fit)
MSE_linear <- mean(train(y_train ~ ., data = cbind(y_train,x_train), 
                         trControl = trControl1, method = "lm")$resample$RMSE^2)
lm_pred <- predict.lm(lm_fit,newdata = x_test)

### Linear Regression with BIC

BIC_fit <-regsubsets(y_train ~ ., data = x_train,nvmax = 18)
BIC_sum <- summary(BIC_fit)
plot(BIC_sum$bic, ylab = "BIC",xlab = "Number of predictors", type = "l")
MSE_BIC <- mean(train(y_train ~ ., data = cbind(y_train,x_train[,names(coef(BIC_fit,4))[-1]]), 
                      trControl = trControl1, method = "lm")$resample$RMSE^2)
BIC_fit2 <- lm(y_train ~ ., data = x_train[,names(coef(BIC_fit,4))[-1]])
VIF(BIC_fit2)
BIC_pred <- predict.lm(BIC_fit2,newdata = x_test)

### Ridge Model

cv_fitr <-cv.glmnet( model_train, y_train, alpha = 0) 
plot(cv_fitr)
lambdar_min <- cv_fitr$lambda.min
ridge_fit <- glmnet(model_train,y_train, alpha = 0, lambda =lambdar_min)
ridge_betas <- predict(ridge_fit ,type = "coefficients", s = lambdar_min)[-2,]
MSE_ridge <- cv_fitr$cvm[cv_fitr$lambda == cv_fitr$lambda.min]
ridge_pred <- predict.glmnet(ridge_fit,newx = model_test)

### Lasso Model

cv_fitl <- cv.glmnet( model_train, y_train, alpha = 1) 
plot(cv_fitl)
lambdal_min <- cv_fit$lambda.min
lasso_fit <- glmnet(model_train,y_train, alpha = 1, lambda =lambdal_min)
lasso_betas <- predict(lasso_fit ,type = "coefficients", s = lambdal_min)[-2,]
MSE_lasso <- cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min]
lasso_pred <- predict.glmnet(lasso_fit,newx = model_test)

### PCR model

pcr_fit <- pcr(y_train ~ ., data = x_train, scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")
pcr_fit <- pcr(y_train ~ ., 5, data = x_train)
MSE_pcr <- MSEP(pcr_fit, ncomp = 5)$val[2]
pcr_pred <- predict(pcr_fit,x_test, ncomp = 5)

### PLS model

pls_fit <- plsr(y_train ~ ., data = x_train, scale = TRUE, validation = "CV")
validationplot(pls_fit, val.type = "MSEP")
pls_fit <- plsr(y_train ~ ., 5,data = x_train)
MSE_pls <- MSEP(pls_fit, ncomp = 5)$val[2]
pls_pred <- predict(pls_fit,x_test,ncomp = 5)

### Final Observations

PTS <- x_train$FT1 + 2*x_train$X2P1 + 3*x_train$X3P1
x_PTS <- data.frame(oppPTS1 = x_train$oppPTS1,PTS)
PTS_fit <- lm(y_train ~ ., data = x_PTS)
summary(PTS_fit)

PTdiff <- PTS - x_train$oppPTS1
PTdiff_fit <- lm(y_train ~ ., data = data.frame(PTdiff))
summary(PTdiff_fit)

### Performing final prediction

MSE_final <- mean((y_test - pls_pred)^2)
MSE_nocavs <- mean((y_test[-5]- pls_pred[-5])^2)
