# this file will modify the date in tonnage file and then add some extera 
#columns to the defect file which are going to add to defect file from tonnage


library(lubridate)
library(dplyr)
library(tidyr)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R
# modify the input file to the clean_"defect".csv
data_set<- read.csv(file="./90/Intermediate_Data/clean-DIP_without_removing.csv", header=TRUE, sep=",")
tonnage<- read.csv(file="./Input/TONNAGE_SAMPLE_DATA _Header_Modified.csv", header=TRUE, sep=",")
tonnage$day<-01
tonnage=unite(tonnage,date,YEAR,MONTH,day,sep="-")
tonnage$date=ymd(tonnage$date)
tonnage$date=tonnage$date+30
data_set<-data_set[,-1]
data_set=cbind(data_set,TOT_CAR_EAST=0,TOT_CAR_WEST=0,TOT_TRN_EAST=0,TOT_TRN_WEST=0,TOT_DFLT_MGT=0)
#converting date from factor to date
data_set$TEST_DT<-ymd(data_set$TEST_DT)

# sorting
data_set=arrange(data_set,CLASS,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,TEST_DT)# sorting



#assignig tonnage to the defect file
# modify row number 17:21 and fro without cleaning 19:23
mileposts=unique(data_set$MILEPOST)

for (i in 1:length(mileposts)){
  def<-which(data_set$MILEPOST==mileposts[i])
  a<-which(tonnage$LINE_SEG_NBR==data_set$LINE_SEG_NBR[def[1]] & tonnage$TRACK_SDTK_NBR==data_set$TRACK_SDTK_NBR[def[1]] & tonnage$MILEPOST_START<=data_set$MILEPOST[def[1]] & tonnage$MILEPOST_END>=data_set$MILEPOST[def[1]] )
  
  for (j in 1:length(def)){
    if (j==1){data_set[def[j],c(19,20,21,22,23)]=0}
    else if (j>1){z<-which(tonnage$date[a]<=data_set$TEST_DT[def[j]] & tonnage$date[a]>=data_set$TEST_DT[def[1]]-as.difftime(day(data_set$TEST_DT[def[1]]),units = "days")  )
    if(length(z)>0){ data_set[def[j],19]=sum(tonnage$TOT_CAR_EAST[a[z]])
    data_set[def[j],20]=sum(tonnage$TOT_CAR_WEST[a[z]])
    data_set[def[j],21]=sum(tonnage$TOT_TRN_EAST[a[z]])
    data_set[def[j],22]=sum(tonnage$TOT_TRN_WEST[a[z]])
    data_set[def[j],23]=sum(tonnage$TOT_DFLT_MGT[a[z]])}}
    else{data_set[def[j],19]=0
    data_set[def[j],20]=0
    data_set[def[j],21]=0
    data_set[def[j],22]=0
    data_set[def[j],23]=0}
  }} 



#adding weight coloumn to the data

data_set$weight[1]=0
for (i in 2:nrow(data_set)){
  if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
    
    data_set$weight[i]=data_set$TOT_DFLT_MGT[i]-data_set$TOT_DFLT_MGT[i-1]
    
  } else{
    
    data_set$weight[i]=0
  }
  
  
}

#adding average weight coloumn to the data

data_set$average_weight[1]=0
for (i in 2:nrow(data_set)){
  if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
    
    data_set$average_weight[i]=data_set$weight[i]/data_set$time_interval[i]
    
  } else{
    
    data_set$average_weight[i]=0
  }
  
  
}

# adding initial to the data
data_set$initial[1]=0
for (i in 2:nrow(data_set)){
   if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
     
     data_set$initial[i]=data_set$DEF_AMPLTD[i-1]
     
   } else{
     
     data_set$initial[i]=0
   }
  
  
}



# adding cumulative time to the data
data_set$cum_time[1]=0
for (i in 2:nrow(data_set)){
  if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
    
    data_set$cum_time[i]=data_set$cum_time[i-1]+data_set$time_interval[i]
    
  } else{
    
    data_set$cum_time[i]=0
  }
  
  
}





# adding start date to the data
data_set$start_date[1]=0
for (i in 2:nrow(data_set)){
  if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
    
    data_set$start_date[i]=data_set$cum_time[i-1]
    
  } else{
    
    data_set$start_date[i]=0
  }
  
  
}



# adding growth to the data
data_set$growth[1]=0
for (i in 2:nrow(data_set)){
  if ( data_set$LINE_SEG_NBR[i]==data_set$LINE_SEG_NBR[i-1] && data_set$MILEPOST[i]==data_set$MILEPOST[i-1] && data_set$TRACK_SDTK_NBR[i]==data_set$TRACK_SDTK_NBR[i-1] && data_set$CLASS[i]==data_set$CLASS[i-1]){
    
    data_set$growth[i]=abs(data_set$DEF_AMPLTD[i])-abs(data_set$DEF_AMPLTD[i-1])
    
  } else{
    
    data_set$growth[i]=0
  }
  
  
}























#name of the file to be saved
write.csv(data_set, file="./90/Intermediate_Data/DIP_with_tonnage__without_removing.csv")


