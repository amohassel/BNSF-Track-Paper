  library(stats4)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(stargazer)
  library(pracma)
  library(BaTFLED3D)
  library(purrr)
  # Set directory
  # data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
  data_dir <- Sys.getenv("BNSF_DIR", ".")
  setwd(file.path(data_dir, "90"))
  train<-read.csv(file="./Intermediate_Data/xlevel_train.csv", header=TRUE, sep=",")
  test<-read.csv(file="./Intermediate_Data/xlevel_test.csv", header=TRUE, sep=",")
  
  
  
  
  ###changing red to one and yellow to zero
  
  test <- subset(test, test$cum_time > 0)
  test$color<-ifelse(test$DEF_PRTY=="RED",1,0)
  train <- subset(train, train$cum_time > 0)
  train$color<-ifelse(train$DEF_PRTY=="RED",1,0)
  
  
  ###converting defect amplitude and initial amplitude to absolute values
  
  train$DEF_AMPLTD<-abs(train$DEF_AMPLTD)
  test$DEF_AMPLTD<-abs(test$DEF_AMPLTD)
  train$initial<-abs(train$initial)
  test$initial<-abs(test$initial)
  
  ####seting class values
  
  test<-filter(test,test$CLASS>3)
  test$CLASS=factor(test$CLASS, levels=4:5)
  test$CLASS<-droplevels(test$CLASS)
  test$class4<-ifelse(test$CLASS==5,0,1)
  
  
  
  train<-filter(train,train$CLASS>3)
  train$CLASS=factor(train$CLASS, levels=4:5)
  train$CLASS<-droplevels(train$CLASS)
  train$class4<-ifelse(train$CLASS==5,0,1)
  
  ####removing weight if it is equal to zero
  
   train<-subset(train,train$initial<train$DEF_AMPLTD)
   test<-subset(test,test$weight>0)
   train<-subset(train,train$weight>0)
   
   # a<-quantile(train$DEF_AMPLTD,probs=0.75)-quantile(train$DEF_AMPLTD,0.25)
   # up<-quantile(train$DEF_AMPLTD,probs=0.75)+1.5*a
   # lw<-quantile(train$DEF_AMPLTD,probs=0.25)-1.5*a
   # 
   # train$DEF_AMPLTD<-subset(train$DEF_AMPLTD<up)
   # train$DEF_AMPLTD<-subset(train$DEF_AMPLTD>lw)
   
   
  attach(train)
  
  #### likelihood function
   
   liklihoodfunction <- function(a) {
     beta = a[1]
     alpha = a[2]
     alpha1 = a[3]
     alpha2 = a[4]
     alpha3 = a[5]
     alpha4= a[6]
     
     
     likeli <- 0
     for (i in 1:nrow(train)) {
       x <-exp(-(cum_time[i]/exp(alpha + alpha1 *log(weight)[i] + alpha2 * DEF_LGTH[i]+ alpha3* initial[i]+alpha4* class4[i])) ^ beta)
       
       y=exp(-(start_date[i]/exp(alpha + alpha1 *log(weight)[i] + alpha2 * DEF_LGTH[i]+ alpha3* initial[i]+alpha4* class4[i])) ^ beta)
       
       
       log <- (log((1-color[i])*x+color[i]*(y-x)))
       likeli = likeli + log
       
       
     }
     
     return(-likeli-20*beta)
   }
   
   
   
   a <- c(2, 2, -0.05, 0, 0,1)
   
   hal <- fminsearch(liklihoodfunction, a)
   hal
   
  #optim(a,liklihoodfunction,method="SANN")
  summary(hal)
  beta = hal$xval[1]
  alpha = hal$xval[2]
  alpha1 = hal$xval[3]
  alpha2 = hal$xval[4]
  alpha3 = hal$xval[5]
  alpha4 = hal$xval[6]
  attach((train))
  train$hazard <-exp(-(cum_time/exp(alpha + alpha1 *  log(weight) + alpha2 * DEF_LGTH+ alpha3* initial+alpha4*class4)) ^ beta)
  attach((test))
  
  test$hazard <-exp(-(cum_time/exp(alpha + alpha1 *  log(weight)+ alpha2 * DEF_LGTH+ alpha3* initial+alpha4*class4)) ^ beta)
  
  
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
  
