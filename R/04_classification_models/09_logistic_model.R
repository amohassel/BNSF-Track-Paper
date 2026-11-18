library(ggplot2)
library(lubridate)
library(Metrics)
library(ROCR)
library(stargazer)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
# Read CSV into R

####### surface
defect<- read.csv(file="./Intermediate_Data/surface_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)

#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)
defect$color<-ifelse(defect$DEF_PRTY=="YEL",0,1)
n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]

defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")
  ggsave("./Output/surface_logistic_model_plot.png")
  
defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size
model.1<-glm(color~factor(CLASS)+cum_time,data=train,family="binomial")
summary(model.1)
test$predict <- predict(model.1, newdata = test, type = 'response')
train$predict <- predict(model.1, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.2<-glm(color~factor(CLASS)+TOT_DFLT_MGT,data=train,family="binomial")
summary(model.2)
test$predict <- predict(model.2, newdata = test, type = 'response')
train$predict <- predict(model.2, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.3<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,data=train,family="binomial")
summary(model.3)
test$predict <- predict(model.3, newdata = test, type = 'response')
train$predict <- predict(model.3, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.4<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),data=train,family="binomial")
summary(model.4)
test$predict <- predict(model.4, newdata = test, type = 'response')
train$predict <- predict(model.4, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.5<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,data=train,family="binomial")
summary(model.5)
test$predict <- predict(model.5, newdata = test, type = 'response')
train$predict <- predict(model.5, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))



model.7<-glm((color)~factor(CLASS)+cum_time+DEF_LGTH,data=train,family="binomial")
summary(model.7)
test$predict <- predict(model.7, newdata = test, type = 'response')
train$predict <- predict(model.7, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

png("./Output/AUC_surface.png")
plot(
  ROCRperf,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "=")
)
dev.off()

stargazer(model.1,model.2,model.3,model.4,model.5,model.7, dep.var.labels=c("Defect Type","Defect Type","Defect Type","Defect Type","Defect Type","Defect Type"),notes = "\\parbox[t]{\\textwidth}{This table presents different logistic regression models for surface defect to idetify significant variables and appropriate model to predict defcet tags.} ", notes.align = "l"           
          
          , title="Logistic regression Results for surface defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3","Line 1", "Line 2","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "surface logistic model", font.size ="tiny" ,single.row=TRUE,out="./Output/logistic_model_result_surface.tex")



####### DIP
defect<- read.csv(file="./Intermediate_Data/DIP_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)
defect$color<-ifelse(defect$DEF_PRTY=="YEL",0,1)
#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)
defect=defect[defect$CLASS>3,]
n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]

defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")
  ggsave("./Output/DIP_linear_model_plot.png")

defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size
model.1<-glm(color~factor(CLASS)+cum_time,data=train,family="binomial")
summary(model.1)
test$predict <- predict(model.1, newdata = test, type = 'response')
train$predict <- predict(model.1, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.2<-glm(color~factor(CLASS)+cum_time+TOT_DFLT_MGT,data=train,family="binomial")
summary(model.2)
test$predict <- predict(model.2, newdata = test, type = 'response')
train$predict <- predict(model.2, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.3<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,data=train,family="binomial")
summary(model.3)
test$predict <- predict(model.3, newdata = test, type = 'response')
train$predict <- predict(model.3, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.4<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),data=train,family="binomial")
summary(model.4)
test$predict <- predict(model.4, newdata = test, type = 'response')
train$predict <- predict(model.4, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.5<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,data=train,family="binomial")
summary(model.5)
test$predict <- predict(model.5, newdata = test, type = 'response')
train$predict <- predict(model.5, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.7<-glm((color)~factor(CLASS)+cum_time+DEF_LGTH,data=train,family="binomial")
summary(model.7)
test$predict <- predict(model.7, newdata = test, type = 'response')
train$predict <- predict(model.7, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))
png("./Output/AUC_DIP.png")
plot(
  ROCRperf,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "=")
)
dev.off()



library(stargazer)

stargazer(model.1,model.2,model.3,model.4,model.5,model.7, dep.var.labels=c("Defect Type","Defect Type","Defect Type","Defect Type","Defect Type","Defect Type"), notes = "\\parbox[t]{\\textwidth}{This table presents different logistic regression models for DIP defect to idetify significant variables and appropriate model to predict defcet tags.} ", notes.align = "l"
          
          
          , title="Logistic regression Results for DIP defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3", "Segment 4","Line 1", "Line 2","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "DIP logistic model", font.size ="tiny" ,single.row=TRUE,out="./Output/logistic_model_result_DIP.tex")

####### Xlevel
defect<- read.csv(file="./Intermediate_Data/xlevel_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)
defect$color<-ifelse(defect$DEF_PRTY=="YEL",0,1)
#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)
defect=defect[defect$CLASS>3,]
defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))
defect$CLASS = relevel(defect$CLASS, ref ="Class 5")
n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")
  ggsave("./Output/xlevel_logistic_model_plot.png")



defect$TEST_FSPD= numeric(defect$TEST_FSPD)


# defect size
model.1<-glm(color~factor(CLASS)+cum_time,data=train,family="binomial")
summary(model.1)
test$predict <- predict(model.1, newdata = test, type = 'response')
train$predict <- predict(model.1, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.2<-glm(color~factor(CLASS)+cum_time+TOT_DFLT_MGT,data=train,family="binomial")
summary(model.2)
test$predict <- predict(model.2, newdata = test, type = 'response')
train$predict <- predict(model.2, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.3<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,data=train,family="binomial")
summary(model.3)
test$predict <- predict(model.3, newdata = test, type = 'response')
train$predict <- predict(model.3, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.4<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),data=train,family="binomial")
summary(model.4)
test$predict <- predict(model.4, newdata = test, type = 'response')
train$predict <- predict(model.4, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.5<-glm(color~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,data=train,family="binomial")
summary(model.5)
test$predict <- predict(model.5, newdata = test, type = 'response')
train$predict <- predict(model.5, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

model.7<-glm((color)~factor(CLASS)+cum_time+DEF_LGTH,data=train,family="binomial")
summary(model.7)
test$predict <- predict(model.7, newdata = test, type = 'response')
train$predict <- predict(model.7, train, type = 'response')
ROCRpred <- prediction(test$predict, test$color)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
(AUC <- auc(test$color, test$predict))

png("./Output/AUC_xlevel.png")
plot(
  ROCRperf,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  main = paste("AUC", round(print(AUC), 3), sep = "=")
)

dev.off()


stargazer(model.1,model.2,model.3,model.4,model.5,model.7,dep.var.labels=c("Defect Type","Defect Type","Defect Type","Defect Type","Defect Type","Defect Type"), notes = "\\parbox[t]{\\textwidth}{This table presents different logistic regression models for xlevel defect to idetify significant variables and appropriate model to predict defcet tags.} ", notes.align = "l"
          
          
          , title="Logistsic regression Results for xlevel defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3", "Segment 4","Line 1", "Line 2","Line 3","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "xlevel logistic model", font.size ="tiny" ,single.row=TRUE,out="./Output/logistic_model_result_xlevel.tex")

defect$TEST_FSPD=factor(defect$TEST_FSPD)




#stargazer(model365,model90,dep.var.labels=c("Defect Amplitude","Defect Amplitude")
          
          
#          , title="Regression Results for surface defect in 365 and 90 days interval",covariate.labels = c("Class 4","Cumulative time", "Defect length") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "comparing", font.size ="tiny" ,single.row=TRUE,out="./Output/comparing_linear_model.tex")


