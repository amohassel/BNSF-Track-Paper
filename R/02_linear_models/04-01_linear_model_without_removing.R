

# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R
defect<- read.csv(file="./Intermediate_Data/dip_with_tonnage_without_removing.csv", header=TRUE, sep=",")
defect<-defect[,-1]

#removing decreasing trend and inspection >365
defect<-subset(defect,defect$index365== 0 )
defect<-subset(defect, defect$weight!=0)
defect<-subset(defect,defect$decindex==0 )

#defect<-subset(defect,defect$time_interval<100)


plot(defect$weight,defect$growth)
model.1<-lm(growth~weight,defect)
summary(model.1)

plot(defect$DEF_LGTH,defect$growth)
model.2<-lm(growth~weight+DEF_LGTH,defect)
summary(model.2)

plot(defect$TEST_FSPD,defect$growth)
plot(defect$TEST_PSPD,defect$growth)
model.3<-lm(growth~weight+DEF_LGTH+TEST_FSPD+TEST_PSPD,defect)
summary(model.3)

plot(factor(defect$LINE_SEG_NBR),defect$growth)
model.4<-lm(growth~weight+DEF_LGTH+TEST_FSPD+TEST_PSPD+factor(LINE_SEG_NBR),defect)
summary(model.4)

plot(factor(defect$TRACK_SDTK_NBR),defect$growth)
model.5<-lm(growth~weight+DEF_LGTH+TEST_FSPD+TEST_PSPD+factor(LINE_SEG_NBR)+factor(TRACK_SDTK_NBR),defect)
summary(model.5)

plot(I(defect$TOT_CAR_EAST+defect$TOT_CAR_WEST),defect$growth)
model.6<-lm(growth~weight+DEF_LGTH+TEST_FSPD+TEST_PSPD+factor(LINE_SEG_NBR)+I(TOT_CAR_EAST+TOT_CAR_WEST),defect)
summary(model.6)

plot(I(defect$TOT_TRN_EAST+defect$TOT_TRN_WEST),defect$growth)
model.7<-lm(growth~weight+DEF_LGTH+TEST_FSPD+TEST_PSPD+factor(LINE_SEG_NBR)+I(TOT_CAR_EAST+TOT_CAR_WEST)+I(TOT_TRN_EAST+TOT_TRN_WEST),defect)
summary(model.7)

plot(defect$time_interval,defect$growth)

model.8<-lm(defect$growth~defect$time_interval)

summary(model.8)

#summary of the defect table
stargazer(defect, type = "latex", title="Descriptive statistics", digits=1, out="./Output/defect_stat.tex")


stargazer(total, type = "latex", title="Descriptive statistics", digits=1, out="./Output/inspection stat.tex")

library(stargazer)
stargazer(model.1, model.2, model.3,model.4, model.5, dep.var.labels=c("Defect Amplitude","Defect Amplitude","Defect Amplitude"),
          
          
           title="Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/Result_linear_dip01.tex")
library(stargazer)
stargazer(model.6, model.7, model.8, dep.var.labels=c("Defect Amplitude","Defect Amplitude","Defect Amplitude"),
          
          
          title="Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/Result_linear_dip02.tex")

