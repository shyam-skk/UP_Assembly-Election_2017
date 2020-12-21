setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project-5")
getwd()

library("readxl") 

#install.packages("xlsx")
#library(xlsx)
#myneta <- read_excel("myneta.xlsx")
#colSums(is.na(myneta)) 
#myneta<- na.omit(myneta)
#colSums(is.na(myneta)) 
#View(myneta)
myneta <- read_excel("myneta.xlsx")
eci<-read_excel("eci.xlsx")
write.csv(myneta,"myneta.csv")
write.csv(eci,"eci.csv")

myneta <- read.csv("myneta.csv")
dim(myneta)
str(myneta)
colSums(is.na(myneta))
myneta<- na.omit(myneta)

myneta$Total_Assets =gsub("\\Rs", "", myneta$Total_Assets)
myneta$Total_Assets =gsub("\\,", "", myneta$Total_Assets)
myneta$Liabilities =gsub("\\Rs", "", myneta$Liabilities)
myneta$Liabilities =gsub("\\,", "", myneta$Liabilities)
#View(myneta)
str(myneta)


myneta$Total_Assets =as.numeric(myneta$Total_Assets)
myneta$Liabilities =as.numeric(myneta$Liabilities)
str(myneta)
colSums(is.na(myneta))
myneta<- na.omit(myneta)
dim((myneta))
colSums(is.na(myneta))

eci<-read.csv("eci.csv")
dim(eci)
str(eci)
colSums(is.na(myneta)) 




#up<-merge(x=eci, y=myneta, by.x="CAND_NAME", by.y="Name")
up2017<-merge(eci, myneta, c("CAND_NAME"))
dim(up2017)
View(up2017)
unique(up)
