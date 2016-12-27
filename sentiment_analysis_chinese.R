#import libraries to work with
install.packages("plyr")
install.packages("stringr")
install.packages("e1071")
install.packages("mlbench")
library(plyr)
library(stringr)
library(e1071)
library(mlbench)
library(rpart)

getwd() ## 路徑確認與設定
setwd("/Users/rei/Projects/workShop20160815")

#load up positive and negative words
positives = readLines("AFINN/positive-words-Chinese.txt")
negatives = readLines("AFINN/negative-words-Chinese.txt")

#####################  #load up TripAdvisor reviewr ###############################
TripText = NULL
TripText <- read.delim(file='85Cafe/85CafeTwn_full_R.txt', header=FALSE, stringsAsFactors=FALSE)
TripText01 <- TripText$V1
TripText02 <- TripText$V2

#function to calculate number of words in each category within a sentence
sentiment_score <- function(sentences, negTerms, posTerms){
  final_scores <- matrix('', 0, 3)
  scores <- laply(sentences, function(sentence,  negTerms, posTerms ){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    posMatches <- match(words, posTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    posMatches <- sum(!is.na(posMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(negMatches, posMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  },  negTerms, posTerms)
  return(scores)
}    

score = as.data.frame(sentiment_score(TripText02, positives, negatives))
colnames(score) <- c('sentence', 'neg', 'pos')
write.table(score, file = "85Cafe/85CafeTwn_ful_R.csv", sep = ",",row.names=FALSE)
