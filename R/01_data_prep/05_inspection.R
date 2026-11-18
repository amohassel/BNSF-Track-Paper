# investigate inspection data

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)

# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R
inspection<- read.csv(file="./Input/Inspection_Run_Header_Modified.csv", header=TRUE, sep=",")
defect<- read.csv(file="./Input/Training_Dataset_Header_Modified.csv", header=TRUE, sep=",")
tonnage<- read.csv(file="./Input/TONNAGE_SAMPLE_DATA _Header_Modified.csv", header=TRUE, sep=",")
# tonnage$day<-01
# tonnage=unite(tonnage,date,YEAR,MONTH,day,sep="-")
# tonnage$date=ymd(tonnage$date)
#tonnage$date=tonnage$date+30
inspection<-inspection[,-1]
inspection$TEST_DT<-dmy(inspection$TEST_DT)
inspection$month<-month(inspection$TEST_DT)
inspection$year<-year(inspection$TEST_DT)
inspection$quarter<-quarter(inspection$TEST_DT)

defect$TEST_DT<-dmy(defect$TEST_DT)
defect$month<-month(defect$TEST_DT)
defect$year<-year(defect$TEST_DT)
defect$quarter<-quarter(defect$TEST_DT)
tonnage$quarter<-quarter(tonnage$MONTH)

summary(inspection)
## number of inspections in each segment and line in each year

line1_inspection=inspection %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())


line2_inspection=inspection %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

line3_inspection=inspection %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

line4_inspection=inspection %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

totali<-bind_rows(line1=line1_inspection,line2=line2_inspection,line3=line3_inspection,line4=line4_inspection,.id="name")

# total passing weight from each segment and line in each year

weightline1<-tonnage %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline2<-tonnage %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline3<-tonnage %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline4<-tonnage %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

totalw<-bind_rows(line1=weightline1,line2=weightline2,line3=weightline3,line4=weightline4,.id="name")
colnames(totalw)[3]<-"year"

## defect type and iteration

defecetline1<-defect %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline2<-defect %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline3<-defect %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline4<-defect %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())
## joinig tables togather
totald=bind_rows(line1=defecetline1,line2=defecetline2,line3=defecetline3,line4=defecetline4,.id="name")
total1=left_join(totald,totalw,by=c("name","TRACK_SDTK_NBR","year"))
total<-left_join(total1, totali,by=c("name","TRACK_SDTK_NBR","year"))
total=total[complete.cases(total),]
total<-arrange(total,name,TRACK_SDTK_NBR,year)
total<-as.data.frame(total)

ggplot(total,aes(x=weight,y=number_of_inspection))+
  geom_point()+
  facet_grid(.~name+TRACK_SDTK_NBR)

ggplot(total,aes(x=TRACK_SDTK_NBR,y=number_of_inspection,size=weight))+
  geom_point()+
  facet_grid(.~factor(name)+year)

model<-lm(number_of_inspection~weight,data=total)
summary(model)

write.csv(total, file="./Intermediate_Data/total_inspection.csv")

stargazer(total, type = "text", title="Descriptive statistics", digits=1, out="./Output/inspection_stat.tex")
stargazer(total, type = "latex", title="Descriptive statistics", digits=1, out="./Output/inspection_stat.tex")


#################inspection of every one mile
a<-seq(0,519,1)
b<-seq(1,520,1)
ab<-cbind(a,b)
ab<-data.frame(ab)
segment<-unique(inspection$LINE_SEG_NBR)
line<-unique(inspection$TRACK_SDTK_NBR)
# replicate the mile to consider all segment , line , year , and quarter year
ab<-do.call("rbind", replicate(length(segment), ab, simplify = FALSE))
ab$segment<-c(rep(1,520),rep(2,520),rep(3,520),rep(4,520))
ab<-do.call("rbind", replicate(length(line), ab, simplify = FALSE))
ab$line<-c(rep(rep(0,520),4),rep(rep(1,520),4),rep(rep(2,520),4),rep(rep(3,520),4),rep(rep(4,520),4),rep(rep(5,520),4),rep(rep(6,520),4))
n<-nrow(ab)
ab<-do.call("rbind", replicate(length(unique(inspection$year)), ab, simplify = FALSE))
ab$year<-c(rep(2007,n),rep(2008,n),rep(2009,n),rep(2010,n),rep(2011,n),rep(2012,n),rep(2013,n))
n<-nrow(ab)
ab<-do.call("rbind", replicate(4, ab, simplify = FALSE))
ab$quarter<-c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))
# find the number of inspection in every one mile
for(i in 1:nrow(ab)){
  temp<-(which(inspection$MILEPOST_START<=ab$b[i] & inspection$MILEPOST_END>=ab$a[i] & inspection$LINE_SEG_NBR==ab$segment[i] & inspection$TRACK_SDTK_NBR==ab$line[i] & inspection$year==ab$year[i] & inspection$quarter==ab$quarter[i]))
  ab$count[i]<-length(unique(inspection$TEST_DT[temp]))
  }
#find the amount of passing tonnage in every one mile
for(i in 1:nrow(ab)){
  temp<-(which(tonnage$MILEPOST_START<=ab$b[i] & tonnage$MILEPOST_END>=ab$a[i] & tonnage$LINE_SEG_NBR==ab$segment[i] & tonnage$TRACK_SDTK_NBR==ab$line[i] & tonnage$YEAR==ab$year[i] & tonnage$quarter==ab$quarter[i]))
  ab$tonnage[i]<-sum(tonnage$TOT_DFLT_MGT[temp])
}
# find the number of yellow and red defect in every one mile
for(i in 1:nrow(ab)){
  ab$yellow[i]<-length(which(defect$MILEPOST>=ab$a[i] & defect$MILEPOST<=ab$b[i] & defect$LINE_SEG_NBR==ab$segment[i] & defect$TRACK_SDTK_NBR==ab$line[i] & defect$year==ab$year[i] & defect$quarter==ab$quarter[i] & defect$DEF_PRTY=="YEL" ))
  
}

for(i in 1:nrow(ab)){
  ab$red[i]<-length(which(defect$MILEPOST>=ab$a[i] & defect$MILEPOST<=ab$b[i] & defect$LINE_SEG_NBR==ab$segment[i] & defect$TRACK_SDTK_NBR==ab$line[i]& defect$year==ab$year[i] & defect$quarter==ab$quarter[i] & defect$DEF_PRTY=="RED"))
  
}
# find the percent of yellow and red defect
s<-sum(ab$red)
ab$percent_yellow<-ab$yellow/s

s<-sum(ab$red)
ab$percent_red<-ab$red/s



write.csv(ab, file="./Intermediate_Data/mile_inspection.csv")

# linear model for inspection
model.ins<-lm(count~(tonnage),data=ab)
summary(model.ins)  
stargazer(model.ins, dep.var.labels=c("Inspection") 
          
          , title="Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/inspection_result.tex")
################################################################################################################################
#Run This Part##########################################################

ab<-read.csv(file="./Intermediate_Data/mile_inspection.csv", header=TRUE, sep=",")
model.ins<-lm(count~(tonnage),data=ab)
summary(model.ins) 
#combine year and quarter
ab$time<-paste(ab$year,ab$quarter,sep="_")
ab$time<-yq(ab$time)
#remove zero from data frame 
ab<-subset(ab,ab$tonnage>0)
ggplot(ab,aes(x=tonnage,y=count,col=factor(segment)))+
  geom_jitter()+
  facet_grid(~year)+
  geom_smooth(method=lm)+
  ggsave("./Output/ton_ins_segment.png")

ggplot(ab,aes(x=tonnage,y=count))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Tonnage",y="Number of inspection")+
  ggsave("./Output/ton_ins.png")

ggplot(ab,aes(x=time,y=count,size=tonnage))+
  geom_jitter()+
  facet_grid(~segment)+
  ggsave("./Output/time_ins_ton.png")


data<-ab%>%
  group_by(segment,time)%>%
  summarize(avg_ins=mean(count), avg_ton=mean(tonnage),avg_yel=mean(yellow),avg_red=mean(red))

ggplot(data,aes(x=time,y=avg_ton,size=avg_ins))+
  geom_jitter()+
    facet_grid(.~segment)+
  ggsave("./Output/avg_ton_ins_segment.png")

# clrs = c("avg_ins"="black","avg_ton"="blue","avg_yel"="yellow","avg_red"="red") 
# ggplot(data = data, aes(x = time)) + 
#   geom_jitter(aes(y = avg_ins,fill ="Inspection"),colour="black" ,shape=1) + 
#   geom_jitter(aes(y = avg_ton), colour="blue",shape=3)+
#   geom_jitter(aes(y = avg_yel), colour="yellow",shape=6)+
#   geom_jitter(aes(y = avg_red), colour="red",shape=8)+
#   facet_grid(.~segment)+
# labs(y ="")+
#   ggsave("./Output/avg_time_ins_ton_yel_red.png")


kk<-gather(data,var,value,-c(time,segment))

ggplot(data = kk, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~segment)+
  labs(y ="")+
  ggsave("./Output/avg_time_ins_ton_yel_red_legend_fixed.png")


#### cumulative number of inspection and weight 


ab=arrange(ab,segment,line,a,year,quarter)

for (i in 2:nrow(ab)){
  ab$cum_tonnage[1]<-ab$tonnage[1]
  ab$cum_ins[1]<-ab$count[1]
  ab$cum_yellow[1]<-ab$yellow[1]
  ab$cum_red[1]<-ab$red[1]
  if ( ab$segment[i]==ab$segment[i-1] && ab$line[i]==ab$line[i-1] && ab$a[i]==ab$a[i-1] && ab$b[i]==ab$b[i-1]){
    ab$cum_tonnage[i]=ab$tonnage[i]+ab$cum_tonnage[i-1]
    ab$cum_ins[i]=ab$count[i]+ab$cum_ins[i-1]
    ab$cum_yellow[i]<-ab$yellow[i]+ab$cum_yellow[i-1]
    ab$cum_red[i]<-ab$red[i]+ab$cum_red[i-1]
  } else{
    
    ab$cum_tonnage[i]=ab$tonnage[i]
    ab$cum_ins[i]=ab$count[i]
    ab$cum_yellow[i]<-ab$yellow[i]
    ab$cum_red[i]<-ab$red[i]
  }
}

data_cum<-ab[,c(2,3,4,5,14,15,16,17,18)]
data_cum<-gather(data_cum,var,value,-c(a,b,time,segment,line))

ggplot(data = data_cum, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~segment)+
  labs(y ="")+
  ggsave("./Output/cum_time_ins_ton_yel_red_segment.png")

ggplot(data = data_cum, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~segment+line)+
  labs(y ="")+
  ggsave("./Output/cum_time_ins_ton_yel_red_segment_line.png")

ggplot(ab,aes(x=cum_tonnage,y=cum_ins))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative number of inspection")+
  ggsave("./Output/cum_ton_ins.png")

model.cumins<-lm(cum_ins~(cum_tonnage),data=ab)
summary(model.cumins)  
stargazer(model.cumins, dep.var.labels=c("Inspection") 
          
          , title="Cumulative Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/cum_inspection_result.tex")

sum<-ab%>%
  group_by(a,b,line,segment)%>%
  summarize(suminsp=sum(count),sumtonnage=sum(tonnage),sumyellow=sum(yellow),sumred=sum(red))
sum$peryellow=sum$sumyellow/sum$suminsp
sum$perred=sum$sumred/sum$suminsp

ggplot(sum,aes(x=sumtonnage,y=suminsp))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative number of inspection")+
  ggsave("./Output/sum_ton_ins.png")

ggplot(sum,aes(x=sumtonnage,y=sumred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative red tag")+
  ggsave("./Output/sum_numred_weight.png")

ggplot(sum,aes(x=sumtonnage,y=sumyellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative yellow tag")+
  ggsave("./Output/sum_numyellow_weight.png")

ggplot(sum,aes(x=sumtonnage,y=perred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative percentage of red tag")+
  ggsave("./Output/sum_perred_weight.png")

ggplot(sum,aes(x=sumtonnage,y=peryellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative percentage of yellow tag")+
  ggsave("./Output/sum_perred_weight.png")

ggplot(sum,aes(x=suminsp,y=sumyellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative inspection",y="Cumulative number of yellow tag")+
  ggsave("./Output/sum_numyellow_inspection.png")

ggplot(sum,aes(x=suminsp,y=sumred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative inspection",y="Cumulative number of red tag")+
  ggsave("./Output/sum_numred_inspection.png")






model.sumins<-lm(suminsp~(sumtonnage),data=sum)
summary(model.sumins)  
stargazer(model.sumins, dep.var.labels=c("Inspection") 
          
          , title="summation Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/sum_inspection_result.tex")


sum<-sum[,-c(9,10)]
aa<-gather(sum,var,value,-c(a,b,line,segment))

ggplot(data = aa, aes(x = a)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~segment+line)+
  labs(y ="")+
  ggsave("./Output/sum_time_ins_ton_yel_red_segment_line.png")
  
