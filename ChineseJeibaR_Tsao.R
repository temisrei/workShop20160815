install.packages("jiebaR")
install.packages("wordcloud")
install.packages("stringr")
install.packages("tm")
library(jiebaR)
library(wordcloud)
library(stringr)
library(tm)

getwd() ## 路徑確認與設定
setwd("/Users/rei/Projects/workShop20160815")

TripText = NULL
#import Tab text file
TripText <- read.delim(file='85Cafe/85CafeTwn_full.txt', header=FALSE, stringsAsFactors=FALSE)

TripText01 <- TripText$V1
TripText02 <- TripText$V2

cc = worker()

words01=NULL
words02=NULL

for (i in 1:length(TripText02)) {
  words02[i]=paste(cc[TripText02[i]],collapse = '  |  ')
  #words01[i]=paste(TripText01[i],"  ",words02[i],".")
}

par(family=("Heiti TC Light")) ## MAC 要另外處理

wordList=paste(words02,collapse= "")
wordcloud(wordList,scale = c(8, 0.2),min.freq=3,max.words=100)
png(file="85Cafe/85cafewc.png",width=12,height=8,units="in",res=300) ## 另存圖片

wordcloud(wordList,scale = c(8, 0.2),min.freq=3,max.words=100)
dev.off()

TripText$V2 = words02
write.table(TripText, file = "85Cafe/85CafeTwn_full_R.txt", sep="\t",row.names=F,col.names=F)

