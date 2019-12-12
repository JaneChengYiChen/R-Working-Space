library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(xlsx)


dat <- read_excel("C:/Users/janecheng/Desktop/INF_nov.xlsx")

#記得在輸入之前要先確認每個sheet的欄位名稱都要一致
ts1 <- read_excel("C:/Users/janecheng/Desktop/X_nov.xlsx",sheet=1)
ts2 <-read_excel("C:/Users/janecheng/Desktop/X_nov.xlsx",sheet=2)
ts3 <-read_excel("C:/Users/janecheng/Desktop/X_nov.xlsx",sheet=3)
ts4 <-read_excel("C:/Users/janecheng/Desktop/X_nov.xlsx",sheet=4)
ts5 <-read_excel("C:/Users/janecheng/Desktop/X_nov.xlsx",sheet=5)

#-清理檔案-
dat2 <- dat[,-c(1:2,14)]
dat2 <- cbind(dat2[,1:4],dat$ori_link,dat2[,5:ncol(dat2)]) #插入ori_link
colnames(dat2)[5]=c("ori_link")
dat2$month <-c("11") #填入月份，每月要自己手動更改
dat2$enterprise <- c("nestle")
dat2$hit_channel <- ifelse(grepl("babyMother|Gossiping|寶寶大小事|好康與分享|育兒",dat2$source)==TRUE,1,0)
dat2$source_ori <- ifelse(grepl("Facebook",dat2$source_ori)==TRUE, paste("Facebook_non_brand"), dat2$source_ori)
dat2$source_ori<- ifelse(grepl("nestle|能恩",dat2$source)== TRUE, paste("Facebook_brand"), dat2$source_ori)
colnames(dat2)[6]=c("主回文")

#重新標記brand

#能恩水解
dat2$brand <-ifelse((grepl("水解",dat2$title)==TRUE),paste("能恩水解"),paste(dat2$brand))
dat2$brand <-ifelse((grepl("水解",dat2$content)==TRUE),paste("能恩水解"),paste(dat2$brand))

#能恩非水解
dat2$brand <-ifelse((grepl("非水解",dat2$title)==TRUE),paste("能恩非水解"),paste(dat2$brand))
dat2$brand <-ifelse((grepl("非水解",dat2$content)==TRUE),paste("能恩非水解"),paste(dat2$brand))

#能恩全護
dat2$brand <-ifelse((grepl("全護",dat2$title)==TRUE),paste("能恩全護"),paste(dat2$brand))
dat2$brand <-ifelse((grepl("全護",dat2$content)==TRUE),paste("能恩全護"),paste(dat2$brand))

#寶兒
dat2$brand <-ifelse((grepl("寶兒",dat2$title)==TRUE),paste("寶兒"),paste(dat2$brand))
dat2$brand <-ifelse((grepl("寶兒",dat2$content)==TRUE),paste("寶兒"),paste(dat2$brand))

#-清理X的檔案-
ts <- rbind(ts1,ts2,ts3,ts4,ts5)
rm(ts1,ts2,ts3,ts4,ts5)
ts2 <- ts[,-c(2:6)]
ts2$評價 <- c("Positive")
ts2$title <- ts$title
ts2$content <- ts$回覆文案
ts2$ori_link<-ts$連結
ts2$主回文 <- c("additional_materials")
ts2$回文數 <- c("additional_materials")
ts2$source <- ts$討論區
ts2$source_ori <- c("討論區")
ts2$source_ori <- ifelse(grepl("PTT|Facebook|FacrBook",ts2$source),paste("social_media"),paste(ts2$source_ori))
ts2$發佈時間 <- ts$日期
ts2$點閱數<-c("additional_materials")
ts2$作者<-c("additional_materials")
ts2$month<-c("11")#每個月都要更改
ts2$enterprise<-c("nestle")
ts2$hit_channel= ifelse(grepl("babyMother|Gossiping|寶寶大小事|好康與分享|育兒",ts2$source)==TRUE,1,0)


#-創造可以比對的條件-
dat2$titlecontent <- paste(str_sub(dat2$title,1,3),str_sub(dat$content,1,3),sep="")
ts2$titlecontent <- paste(str_sub(ts2$title,1,3),str_sub(ts2$content,1,3),sep="")

ts2$is_in_data <- c("none") #預設都是none，等等有比對到再改填入y
dat2$is_in_data<- c("none") #預設都是none，等等有比對到再改填入y
temp1 <- vector(length = nrow(dat2))
temp2 <- NULL

#判定X的資料有沒有跟B的資料比對到
#標到X的excel
for (j in 1:nrow(ts2)){
  for (i in 1:nrow(dat2)){
    temp1[i] <- ts2$titlecontent[j] == dat2$titlecontent[i]
    temp2 <- which(temp1 == TRUE)
    ifelse(ts2$titlecontent[j] == dat2$titlecontent[temp2], ts2$is_in_data[j] <- c("y"), ts2$is_in_data[j] <- c("none"))
  }
}

#標到B的excel
for (j in 1:nrow(dat2)){
       for (i in 1:nrow(ts2)){
             temp1[i] <- dat2$titlecontent[j] == ts2$titlecontent[i]
             temp2 <- which(temp1 == TRUE)
             ifelse(dat2$titlecontent[j]==ts2$titlecontent[temp2], dat2$is_in_data[j]<-c("y"),dat2$is_in_data[j]<-c("none"))
         }
   }

#將沒有收錄到的資料併入B
insert <- which(ts2$is_in_data == "none")
ts3 <- ts2[insert, ] %>% as.data.frame()

colnames(ts3)[1]<- c("brand")
dat3 <- rbind(dat2,ts3)

#將有收錄的資料標記為Positive
dat3$評價<-ifelse(grepl("y",dat3$is_in_data),paste("Positive"),paste(dat3$評價))

#重整架構
dat3 <- dat3[,-c(16:17)]

#輸出檔案
write.csv(dat3,"C:/Users/janecheng/Desktop/INFdata2.csv")
write.xlsx(dat3,"C:/Users/janecheng/Desktop/INF_月.xlsx")
