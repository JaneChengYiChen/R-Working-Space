library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(xlsx)

fastn = read_excel("C:/Users/Cheng Yi Chen/Desktop/fastn.xlsx")
DTM = read_excel("C:/Users/Cheng Yi Chen/Desktop/DTM.xlsx")

#合併內容
fastn$content = paste(fastn$標題,fastn$內容)
fastn = fastn[,-c(1:21)]

name = DTM[,2] #維度名稱
key = DTM[,3] %>% as.data.frame() #關鍵字
data = fastn[,1] %>% as.data.frame() #要標記的資料

temp = NULL

for (i in 1:nrow(key)){
                 x = ifelse(grepl(key[i,],data[,1])==TRUE,name[i,],"")
                 temp = paste(temp,x)
             }

temp = temp %>% as.data.frame
final = cbind(temp,fastn)