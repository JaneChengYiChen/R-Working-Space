  library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(xlsx)

fastn = read_excel("C:/Users/janecheng/Desktop/fastn.xlsx") 
DTM = read_excel("C:/Users/janecheng/Desktop/DTM.xlsx")

fastn$content <- paste(fastn$標題,fastn$內容)
fastn <- fastn[,-c(1:21)]

name = DTM[,2] #維度名稱
key = DTM[,3] %>% as.data.frame() #關鍵字
data = fastn[,1] %>% as.data.frame() #要標記的資料

#維度標記
for (i in 1:nrow(key))
  {
         x = ifelse(grepl(key[i,],data[,1]) == TRUE,1,0)
         col = cbind(col,x)
}

#放維度名稱
name2 = as.character(t(name))
colnames(col) = c("ID",name2)

#跟原先標記的文章合併
bindtest = cbind(col,fasten)