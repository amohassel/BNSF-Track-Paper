# This file clean each defect type base on the following assumption
#1-Discretizing milepost in 0.04 sequences
#2-Cleaning repeated defect
#3-Cleaning records in less than 7 days 
#4-Cleaning records greater than 365 days
#5-Removing decreasing trends


library(lubridate)
library(dplyr)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
# Read CSV into R
Cleaning_set <- read.csv(file="./Intermediate_Data/xlevel.csv", header=TRUE, sep=",")
#converting date from factor to date
Cleaning_set$TEST_DT<-dmy(Cleaning_set$TEST_DT)
# sorting
Cleaning_set=arrange(Cleaning_set,MILEPOST,TEST_DT)

ftom=0.02; # 100 foot to mile.


#discretizing mileposts for re indexing defects
a<-seq(min(Cleaning_set$MILEPOST)-ftom,max(Cleaning_set$MILEPOST)-ftom,0.04)
b<-seq(min(Cleaning_set$MILEPOST)+ftom,max(Cleaning_set$MILEPOST)+ftom,0.04)
nmi<-cbind(a,b)
for(i in 1:nrow(Cleaning_set)){
  for (j in 1:nrow(nmi)){
    if (nmi[j,1]<=Cleaning_set$MILEPOST[i] && Cleaning_set$MILEPOST[i]<nmi[j,2]){
      Cleaning_set$`Variable Name`[i]=(nmi[j,1]+nmi[j,2])/2
    }
  }
  
}

#replace milepost and delete variable column

Cleaning_set$MILEPOST=Cleaning_set$`Variable Name`;
Cleaning_set<-Cleaning_set[,-c(1,16)]
backup<-Cleaning_set



#finding and removing single mileposts
Cleaning_set=cbind(Cleaning_set,0);
colnames(Cleaning_set)[15]<-"Frequancey"




Cleaning_set=arrange(Cleaning_set,CLASS,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,TEST_DT)
for (i in 1:nrow(Cleaning_set))
{Cleaning_set[i,15]=length(which(Cleaning_set$MILEPOST==Cleaning_set$MILEPOST[i] & Cleaning_set$LINE_SEG_NBR==Cleaning_set$LINE_SEG_NBR[i] & Cleaning_set$TRACK_SDTK_NBR==Cleaning_set$TRACK_SDTK_NBR[i] & Cleaning_set$CLASS==Cleaning_set$CLASS[i]))};
Cleaning_set<-subset(Cleaning_set,Cleaning_set[,15]>1)# repeated mileposts index

Cleaning_set=arrange(Cleaning_set,CLASS,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,TEST_DT)


#calculate time_interval between dates
for (i in 2:nrow(Cleaning_set)){if(Cleaning_set$MILEPOST[i]==Cleaning_set$MILEPOST[i-1] && Cleaning_set$LINE_SEG_NBR[i]==Cleaning_set$LINE_SEG_NBR[i-1]  && Cleaning_set$CLASS[i]==Cleaning_set$CLASS[i-1]){Cleaning_set[i,16]=Cleaning_set$TEST_DT[i]-Cleaning_set$TEST_DT[i-1]}else{Cleaning_set[i,16]=0}}
colnames(Cleaning_set)[16]<-"time_interval"
Cleaning_set[1,16]=0

Cleaning_set=arrange(Cleaning_set,CLASS,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,TEST_DT)# sorting

# find repeated data within 7 days, get the highest amplitude and delet the rest

for(i in 2:nrow(Cleaning_set)){
  if (Cleaning_set$TEST_DT[i] - Cleaning_set$TEST_DT[i - 1] < 8 &
      Cleaning_set$MILEPOST[i] == Cleaning_set$MILEPOST[i - 1] &
      Cleaning_set$LINE_SEG_NBR[i] == Cleaning_set$LINE_SEG_NBR[i - 1] &
      Cleaning_set$CLASS[i] == Cleaning_set$CLASS[i - 1]) {
    if (Cleaning_set$TEST_DT[i] > Cleaning_set$TEST_DT[i - 1]) {
      Cleaning_set = Cleaning_set[-(i - 1), ]
    } else if (Cleaning_set$TEST_DT[i] == Cleaning_set$TEST_DT[i - 1]) {
      c <-
        which.min(list(Cleaning_set$DEF_AMPLTD[i], Cleaning_set$DEF_AMPLTD[i - 1]))
      d <-
        which.max(list(Cleaning_set$DEF_AMPLTD[i], Cleaning_set$DEF_AMPLTD[i - 1]))
      if (c == 1) {
        Cleaning_set = Cleaning_set[-i, ]
      } else {
        Cleaning_set = Cleaning_set[-(i - 1), ]
      }
    }
  }
}


# find repeated data greater than 365 days, get the highest amplitude and delet the rest
Cleaning_set=filter(Cleaning_set,time_interval<=90)
Cleaning_set=arrange(Cleaning_set,CLASS,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,TEST_DT)# sorting

# removing decreasing data trend 

for ( i in 2: nrow(Cleaning_set)){
  if(Cleaning_set$MILEPOST[i]==Cleaning_set$MILEPOST[i-1] & Cleaning_set$LINE_SEG_NBR[i]==Cleaning_set$LINE_SEG_NBR[i-1] & Cleaning_set$TRACK_SDTK_NBR[i]==Cleaning_set$TRACK_SDTK_NBR[i-1] & abs(Cleaning_set$DEF_AMPLTD[i])-abs(Cleaning_set$DEF_AMPLTD[i-1])<0){
    Cleaning_set=Cleaning_set[-i,]
  }
  
}




#name of the file to be saved
write.csv(Cleaning_set, file="./Intermediate_Data/clean-xlevel.csv")


