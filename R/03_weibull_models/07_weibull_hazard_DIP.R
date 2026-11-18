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
train<-read.csv(file="./Intermediate_Data/DIP_train.csv", header=TRUE, sep=",")
test<-read.csv(file="./Intermediate_Data/DIP_test.csv", header=TRUE, sep=",")

test <- subset(test, test$cum_time > 0)
test$color<-ifelse(test$DEF_PRTY=="RED",1,0)
train <- subset(train, train$cum_time > 0)
train$color<-ifelse(train$DEF_PRTY=="RED",1,0)

train$DEF_AMPLTD<-abs(train$DEF_AMPLTD)
test$DEF_AMPLTD<-abs(test$DEF_AMPLTD)
train$initial<-abs(train$initial)
test$initial<-abs(test$initial)

test<-filter(test,test$CLASS>3)
test$CLASS=factor(test$CLASS, levels=4:5)
test$CLASS<-droplevels(test$CLASS)
test$class4<-ifelse(test$CLASS==5,0,1)


train<-filter(train,train$CLASS>3)
train$CLASS=factor(train$CLASS, levels=4:5)
train$CLASS<-droplevels(train$CLASS)
train$class4<-ifelse(train$CLASS==5,0,1)
meantime<-mean(train$time_interval)

attach(train)



liklihoodfunction <- function(a) {
  options(digits = 10)
  beta = a[1]
  alpha = a[2]
  alpha1 = a[3]
  alpha2 = a[4]
  alpha3 = a[5]
  alpha4 = a[6]
  
  likeli <- 0
  for (i in 1:nrow(train)) {
    x <-
      -(((cum_time[i]) ^ (beta - 1)) * beta / ((
        exp(
          alpha + alpha1 * weight[i] + alpha2 * DEF_LGTH[i]+ alpha3* initial[i]+ alpha4*class4[i]
        )
      ) ^ beta))
    
    
    log <- log(color[i] + (1 - 2 * color[i]) * exp(x *meantime))
    likeli = likeli + log
    
    
  }
  # if (beta < 1) {
   tikhonov = beta
   return(-likeli)
  # } else{
   # return(-likeli)
  #}
  
}

a <- c(2, 2, -0.05, 0, 0,0)

hal.DIP <- fminsearch(liklihoodfunction, a)
hal.DIP
summary(hal.DIP)
beta = hal.DIP$xval[1]
alpha = hal.DIP$xval[2]
alpha1 = hal.DIP$xval[3]
alpha2 = hal.DIP$xval[4]
alpha3 = hal.DIP$xval[5]
alpha4 = hal.DIP$xval[6]
attach((train))
train$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * weight + alpha2 * DEF_LGTH + alpha3*initial+alpha4*class4
    )
  ) ^ beta
attach((test))

test$hazard <-
  (beta * (cum_time ^ (beta - 1))) / (
    exp(
      alpha + alpha1 * weight + alpha2 * DEF_LGTH + alpha3*initial+alpha4*class4
    )
  ) ^ beta


model.DIP <- glm (color ~ hazard, data = train, family = "binomial")
summary(model.DIP )
test$predict <- predict(model.DIP , newdata = test, type = 'response')
train$predict <- predict(model.DIP , train, type = 'response')
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
png("./Output/AUC_weibull DIP.png")
plot(
  ROCRperf,
  colorize = 1,
  text.adj = c(0, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "="), cex.lab=1.5, lwd=2)
axis(1, font = 2)
axis(2, font = 2)
dev.off()
png("./Output/recall_weibull DIP.png")
x=performance(ROCRpred, measure = "rec")
plot(x)
axis(1, font = 2)
axis(2, font = 2)
dev.off()


a<-rbind(as.numeric(hal.DIP$xval),as.numeric(c(hal.surface$xval,"NA")),as.numeric(hal.xlevel$xval))

colnames(a)<-(c("Beta","Intercept","Weight","Defect length", "Initial amplitude", "Class4" ))
rownames(a)<-c("DIP","Surface", "Xlevel")
stargazer(model.DIP, model.surface, model.xlevel , dep.var.labels = "Defect tag",column.labels =c("DIP defect", "Surface defect", "Xlevel defect"), notes.align = "l"
          , title="Logistic regression Results for DIP hazards in 90 days interval",covariate.labels = c("Hazard"), notes = "\\parbox[t]{12cm}{This table presents binary logistic regression on different defects hazard rate to categorize red and yellow tag defects base on estimated hazard rate by GLL weibull model.} " ,digits=3 ,ci=FALSE, ci.level=0.90,label = "DIP weibull hazard logistic model" ,single.row=TRUE,out="./Output/weibull_hazard_logistic_model_result_DIP.tex")

stargazer(a, type = "latex", title="GLL Weibull Coefficients estimation",label = "tab: weibull coeffi" ,digits=3, out="./Output/weibull coeff.tex")
