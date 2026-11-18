library(stats4)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(pracma)
library(BaTFLED3D)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
train<-read.csv(file="./Intermediate_Data/surface_train.csv", header=TRUE, sep=",")
test<-read.csv(file="./Intermediate_Data/surface_test.csv", header=TRUE, sep=",")


test <- subset(test, test$cum_time > 0)
test$color<-ifelse(test$DEF_PRTY=="RED",1,0)
train <- subset(train, train$cum_time > 0)
train$color<-ifelse(train$DEF_PRTY=="RED",1,0)

train$DEF_AMPLTD<-abs(train$DEF_AMPLTD)
test$DEF_AMPLTD<-abs(test$DEF_AMPLTD)
train$initial<-abs(train$initial)
test$initial<-abs(test$initial)

test<-filter(test,test$CLASS>4)
test$CLASS=factor(test$CLASS, levels=5)
test$CLASS<-droplevels(test$CLASS)
test$class4<-ifelse(test$CLASS==5,0,1)



train<-filter(train,train$CLASS>4)
train$CLASS=factor(train$CLASS, levels=5)
train$CLASS<-droplevels(train$CLASS)
train$class4<-ifelse(train$CLASS==5,0,1)

train<-subset(train,train$initial!=train$DEF_AMPLTD)
train<-subset(train,train$weight>0)
test<-subset(test,test$weight>0)
meantime<-mean(train$time_interval)
attach(train)


liklihoodfunction <- function(a) {
  options(digits = 10)
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
          alpha + alpha1 * weight[i] + alpha2 * DEF_LGTH[i]+ alpha3* initial[i]
        )
      ) ^ beta))
    
    
    log <- log(color[i] + (1 - 2 * color[i]) * exp(x *meantime))
    likeli = likeli + log
    
    
  }
  # if (beta < 1) {
   tikhonov = beta
   return(-likeli+8*alpha1)
  # } else{
   # return(-likeli)
  #}
  
}

a <- c(2, 2, -0.05, 0, 0)

hal.surface <- fminsearch(liklihoodfunction, a)
hal.surface
summary(hal.surface)
beta = hal.surface$xval[1]
alpha = hal.surface$xval[2]
alpha1 = hal.surface$xval[3]
alpha2 = hal.surface$xval[4]
alpha3 = hal.surface$xval[5]
attach((train))
train$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * weight + alpha2 * DEF_LGTH + alpha3*initial
    )
  ) ^ beta
attach((test))

test$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * weight + alpha2 * DEF_LGTH + alpha3*initial
    )
  ) ^ beta


model.surface <- glm (color ~ hazard, data = train, family = "binomial")
summary(model)
test$predict <- predict(model.surface, newdata = test, type = 'response')
train$predict <- predict(model.surface, train, type = 'response')
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
png("./Output/AUC_weibull surface.png")
plot(
  ROCRperf,
  colorize = 1,
  text.adj = c(0, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "="), cex.lab=1.5, lwd=2)
axis(1, font = 2)
axis(2, font = 2)
dev.off()
png("./Output/recall_weibull surface.png")
x=performance(ROCRpred, measure = "rec")
plot(x)
axis(1, font = 2)
axis(2, font = 2)
dev.off()
hal.surface$xval
stargazer(model.surface, dep.var.labels=c("Defect Type")           
          
          , title="Logistic regression Results for surface hazards in 90 days interval",covariate.labels = c("Hazard") ,digits=3 ,ci=FALSE, ci.level=0.90,label = "surface weibull hazard logistic model" ,single.row=TRUE,out="./Output/weibull_hazard_logistic_model_result_surface.tex")


