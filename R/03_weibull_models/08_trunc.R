library(stats4)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(pracma)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
train<-read.csv(file="./Intermediate_Data/DIP_train.csv", header=TRUE, sep=",")
test<-read.csv(file="./Intermediate_Data/DIP_test.csv", header=TRUE, sep=",")

test <- subset(test, test$cum_time > 0)
test$color<-ifelse(test$DEF_PRTY=="RED",1,0)
train <- subset(train, train$cum_time > 0)
train$color<-ifelse(train$DEF_PRTY=="RED",1,0)
attach(train)


liklihoodfunction <- function(a) {
  beta = a[1]
  alpha = a[2]
  alpha1 = a[3]
  alpha2 = a[4]
  alpha3 = a[5]

  
  likeli <- 0
  for (i in 1:nrow(train)) {
    x <-
      -(((cum_time[i]) ^ (beta - 1)) * beta / ((
        exp(
          alpha + alpha1 * TOT_DFLT_MGT[i] + alpha2 * DEF_LGTH[i]+ alpha3* initial[i]
        )
      ) ^ beta))
    
    
    log <- log(color[i] + (1 - 2 * color[i]) * exp(x * time_interval[i]))
    likeli = likeli + log
    
    
  }
  if (beta < 1) {
    tikhonov = beta
    return(-likeli + 1000000 * tikhonov)
  } else{
    return(-likeli)
  }
  
}

a <- c(1, 2, -0.05, 0, 0)

hal <- fminsearch(liklihoodfunction, a)
hal
summary(hal)
beta = hal$xval[1]
alpha = hal$xval[2]
alpha1 = hal$xval[3]
alpha2 = hal$xval[4]
alpha3 = hal$xval[5]

attach((train))
train$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * TOT_DFLT_MGT + alpha2 * DEF_LGTH + alpha3*initial
    )
  ) ^ beta
attach((test))

test$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * TOT_DFLT_MGT + alpha2 * DEF_LGTH + alpha3*initial
    )
  ) ^ beta


model <- glm (color ~ hazard, data = train, family = "binomial")
summary(model)
test$predict <- predict(model, newdata = test, type = 'response')
train$predict <- predict(model, train, type = 'response')
#confusion matrix
xtab = table(test$color, test$predict > 0.3)
xtab
library(caret)
confusionMatrix(test$color , test$predict > 0.5)
accuracy(test$color, test$predict > 0.64)


library(Metrics)
library(ROCR)
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
AUC <- auc(test$color, test$predict)
plot(
  ROCRperf,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "=")
)
hal$xval
