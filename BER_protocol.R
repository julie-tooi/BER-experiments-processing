#Adding libs
library("readxl")
library("ggplot2")
library("clusterSim")
library("psych")
library("tibble")
library("dplyr")
library("openxlsx")

# Import
path_RD <- readline(prompt = "Enter path to Excel file:")
RawDF <- read_excel(path = path_RD)
#example of path on Windows: "C:/Users/jozerova/Desktop/program_for_StepOne_data.xls"
 
# Delete lines and rename col
RawDF <- RawDF[-c(1,2,3,4,5,6,7),]
colnames(RawDF)[1] <- "Well"
colnames(RawDF)[2] <- "Reading"


#Input channel of detection and Delete columns with other channels - here only FAM
channel <- readline(prompt = "Enter channel of detection (FAM or SYBR, JOE or VIC, TAMRA or NEX, ROX):")
if (channel == "FAM" | channel == "SYBR"){
  RawDF$...4 <- NULL
  RawDF$...5 <- NULL
  RawDF$...6 <- NULL
  colnames(RawDF)[3] <- channel
}

#typeof(RawDF...
RawDF$FAM <- as.numeric(RawDF$FAM)
RawDF$Reading <- as.numeric(RawDF$Reading)
RawDF$Well <- as.factor(RawDF$Well)
RawDF <- subset(RawDF, Reading != 1)

#data processing
DF_pr <- data.frame(2:60)
colnames(DF_pr)[1] <- "Reading"
#here we need a working function!
raw_vec <- subset(RawDF, Well == "B7", select = "FAM")
x <- raw_vec[c(1),]
x <- as.numeric(x)
vec_pr <- raw_vec - x
DF_pr <- cbind(DF_pr, vec_pr)
colnames(DF_pr)[7] <- "B7"
  
#this not work correctly T_T
#adding_data <- function (Well, col){
#  raw_vec <- subset(RawDF, Well == Well, select = "FAM")
#  x <- raw_vec[c(1),]
#  x <- as.numeric(x)
#  vec_pr <- raw_vec - x
#  DF_pr <- cbind(DF_pr, vec_pr)
#  colnames(DF_pr)[col] <- Well
#}
#adding_data("B3", 3)

#plot with data (StepOne like)
ggplot(DF_pr, aes(x = Reading))+                    
  geom_line(aes(y=B2), colour="red")+
  geom_line(aes(y=B3), colour="red")+
  geom_line(aes(y=B4), colour="red")+
  geom_line(aes(y=B5), colour="blue")+
  geom_line(aes(y=B6), colour="blue")+
  geom_line(aes(y=B7), colour="blue")+  
  scale_linetype_discrete(name="Raw Fluorescence")+
  xlab("Reading") + ylab("Fluorescence")+
  theme_bw()

#boxplot for Fl(Tmax) - data extract
Bp_data <- data.frame(1:3)
colnames(Bp_data)[1] <- "Repeat"

max_data <- subset(DF_pr, Reading == 60)
max_data_vec <- rbind(max_data$B2, max_data$B3, max_data$B4)

Bp_data <- cbind(Bp_data, max_data_vec)
colnames(Bp_data)[2] <- "U+UDG+"

max_data_vec <- rbind(max_data$B5, max_data$B6, max_data$B7)
Bp_data <- cbind(Bp_data, max_data_vec)
colnames(Bp_data)[3] <- "U-UDG+"

write.xlsx(Bp_data, "DataFI(Tmax)_DATE_OF_EXP.xlsx")

#boxplot
boxplot(Bp_data$`U+UDG+`, Bp_data$`U-UDG+`)