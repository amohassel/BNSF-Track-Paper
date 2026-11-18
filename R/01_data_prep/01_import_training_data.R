
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R
Training_Dataset <- read.csv(file="./Input/Training_Dataset.csv", header=TRUE, sep=",")
Clean_Training_Dataset<-Training_Dataset[-c(1,2,3),-1]

# Write the intermediate files for three main variables (DIP, surface, xlevel)
# I advise to double check the name of these file since it seems there are some other files with similar name

#Filtering DIP
DIP <- Clean_Training_Dataset[which(Clean_Training_Dataset$DFCT_TYPE =='DIP'),]
write.csv(DIP, file="./90/Intermediate_Data/DIP.csv")

#Filtering surface
surface <- Clean_Training_Dataset[which(Clean_Training_Dataset$DFCT_TYPE =='SURFACE'),]
write.csv(surface, file="./90/Intermediate_Data/surface.csv")

#Filtering xlevel
xlevel <- Clean_Training_Dataset[which(Clean_Training_Dataset$DFCT_TYPE =='XLEVEL'),]
write.csv(xlevel, file="./90/Intermediate_Data/xlevel.csv")


