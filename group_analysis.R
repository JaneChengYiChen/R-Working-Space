library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(xlsx)

rm(a,a_1,b,c,d,e,f)

C1 <- read.csv("C:/Users/janecheng/Desktop/all_plus_channel.csv")
all_channel <- read_excel("C:/Users/janecheng/Desktop/總頻道0826.xlsx")
tag <-read_excel("C:/Users/janecheng/Desktop/通路品牌Tag.xlsx")
colnames(all_channel) = c("link","channel","cat","unicode")

channel <- sort(table(C1$channel),decreasing = TRUE) %>% as.data.frame()
colnames(channel) = c("channel","FREQ")
cat <- read_excel("C:/Users/janecheng/Desktop/分類.xlsx")
count = channel %>% left_join(all_channel,channel,by='channel')
count <- count[,-c(3,5)]

name = cat[,1] #維度名稱
key = cat[,1] %>% as.data.frame() #關鍵字

a <- count$cat %>% as.data.frame()

for (i in 1:nrow(key))
   {
         x = ifelse(grepl(key[i,],count[,3]) == TRUE,1,0)
         a = cbind(a,x)
}

name2 = as.character(t(name))
colnames(a) = c("ID",name2)

C1_channel = cbind(channel,a)
write.csv(C1_channel,"C:/Users/janecheng/Desktop/第一群(94人)_頻道類別.csv")
