library(ggplot2)
library(lubridate)
library(dplyr)
library(stargazer)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R

####### surface
defect<- read.csv(file="./90/Intermediate_Data/surface_with_tonnage.csv", header=TRUE, sep=",")
inspection<- read.csv(file="./Input/Inspection_Run_Header_Modified.csv", header=TRUE, sep=",")
inspection$TEST_DT<-dmy(inspection$TEST_DT)
defect$TEST_DT<-ymd(defect$TEST_DT)

inspection$quarter<-quarter(inspection$TEST_DT)
defect<-defect[,-1]
inspection<-inspection[,-1]


for (i in 1:nrow(defect)){
  a<-filter(inspection,defect$MILEPOST[i]>=inspection$MILEPOST_START & defect$MILEPOST[i]<=inspection$MILEPOST_END , inspection$LINE_SEG_NBR==defect$LINE_SEG_NBR[i], defect$TRACK_SDTK_NBR[i]==inspection$TRACK_SDTK_NBR, quarter(defect$TEST_DT[i])==quarter(inspection$TEST_DT) )
  defect$mindex[i]<-nrow(a)

}


var<-defect[,-c(4,5,6,7,10,14)]
cor(var)

#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)

n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]

defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size

model-lm((DEF_AMPLTD)~factor(CLASS)+cum_time+TOT_DFLT_MGT+DEF_LGTH+mindex,defect)
summary(model)


