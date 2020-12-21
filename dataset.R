setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project-5")
getwd()
library("readxl") 

#up<- read_excel("2017.xlsx")
#write.csv(up,"up2017.csv")
up2017<- read.csv("up2017.csv")

dim(up2017)
str(up2017)
colSums(is.na(up2017))
up2017<- na.omit(up2017)

up2017$total_assest =gsub("\\Rs", "", up2017$total_assest)
up2017$total_assest =gsub("\\,", "", up2017$total_assest)
up2017$liabilities =gsub("\\Rs", "", up2017$liabilities)
up2017$liabilities =gsub("\\,", "", up2017$liabilities)
#View(myneta)
str(up2017)


up2017$total_assest =as.numeric(up2017$total_assest)
up2017$liabilities =as.numeric(up2017$liabilities)
up2017$CAND_AGE =as.numeric(up2017$CAND_AGE)
str(up2017)
colSums(is.na(up2017))

dim((up2017))
colSums(is.na(up2017))
write.csv(up2017,"up.csv")
View(up2017)
