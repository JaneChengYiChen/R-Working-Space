library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

dat <- read_excel("C:/Users/janecheng/Downloads/Report_2018-09-01.xlsx")
dat2 <- dat[,-c(1:2,14)]
dat2 <- cbind(dat2[,1:4],dat$original_link,dat2[,5:ncol(dat2)]) #插入original_link
colnames(dat2)[5]=c("original_link")
dat2$month <- c("11") #填入月份，每月要自己手動更改
dat2$enterprise <- dat$brand
dat2$hit_web <- ifelse(grepl("babyMother|Gossiping|寶寶大小事|好康與分享|育兒",dat2$source)==TRUE,1,0)
#先將所有的FBsource_ori標為非brand，在挑選brand的FB頻道
dat2$source_ori <- ifelse(grepl("Facebook",dat2$source_ori)==TRUE, paste("Facebook非brand"), dat2$source_ori)
dat2$source_ori<- ifelse(grepl("卡洛塔妮|新安琪兒|桂格|美強生|雀巢|wyeth|卡洛|雪印|新安琪兒|Abbott",dat2$source)== TRUE, paste("Facebookbrand"), dat2$source_ori)

#重新標記brand

#illuma
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("illuma",dat2$title)==TRUE),paste("illuma"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("illuma",dat2$content)==TRUE),paste("illuma"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("illuma",dat2$source)==TRUE),paste("illuma"),paste(dat2$brand))

#S26
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("S-26|s-26|S26|s26",dat2$title)==TRUE),paste("S26"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("S-26|s-26|S26|s26",dat2$content)==TRUE),paste("S26"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="wyeth"& grepl("S-26|s-26|S26|s26",dat2$source)==TRUE),paste("S26"),paste(dat2$brand))


#Abbott心美力
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("心美",dat2$title)==TRUE),paste("Abbott心美力"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("心美",dat2$content)==TRUE),paste("Abbott心美力"),paste(dat2$brand))

#Abbott親護
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("親護",dat2$title)==TRUE),paste("Abbott親護"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("親護",dat2$content)==TRUE),paste("Abbott親護"),paste(dat2$brand))

#Abbott經典
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("經典",dat2$title)==TRUE),paste("Abbott經典"),paste(dat2$brand))
dat2$brand <-ifelse((dat2$enterprise=="Abbott"& grepl("經典",dat2$content)==TRUE),paste("Abbott經典"),paste(dat2$brand))


write.csv(dat2,"C:/Users/janecheng/Desktop/INFdata11月_除比對.csv")
