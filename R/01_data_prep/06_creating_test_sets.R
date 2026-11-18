# investigate inspection data

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)

# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
defect<-read.csv(file="./Intermediate_Data/xlevel_with_tonnage.csv", header=TRUE, sep=",")
defect<-arrange(defect,MILEPOST,TEST_DT)
miles<-data.frame(table(defect$MILEPOST))
#### remove if all record of a mile post is red
for (i in 1: nrow(miles)){
  post<-which(defect$MILEPOST==miles$Var1[i])
  
  if (all(defect$DEF_PRTY[post]=="RED")) { defect<- defect[-post,]}
  
}

## remove to the time we reach just one red for each milepost

for (i in 2: nrow(defect)){
if (all(defect$DEF_PRTY[i-1]=="RED",defect$DEF_PRTY[i]=="RED")) {defect<-defect[-i,]}
}


## modifing the frequancy column

for (i in 1: nrow(defect)){ 
  defect$Frequancey[i]<-length(which(defect$MILEPOST==defect$MILEPOST[i] & defect$LINE_SEG_NBR==defect$LINE_SEG_NBR[i] & defect$TRACK_SDTK_NBR==defect$TRACK_SDTK_NBR[i] & defect$CLASS==defect$CLASS[i]))
}

### making test and train set
miles<-data.frame(table(defect$MILEPOST))
n<-round(0.8*nrow(miles))
index<-sample(nrow(miles),n)
testm<-miles[-index,]
# creating train set
train<-c()
for ( i in 1:length(index)){
  k=which(defect$MILEPOST==miles$Var1[index[i]])
  train=rbind(train,defect[k,])
}

#creating the test set
test<-c()
for ( i in 1:nrow(testm)){
  k=which(defect$MILEPOST==testm$Var1[i])
  test=rbind(test,defect[k,])
}

write.csv(test,"./Intermediate_Data/xlevel_test.csv")
write.csv(train,"./Intermediate_Data/xlevel_train.csv")

