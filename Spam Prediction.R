



#Reading file
sms = read.csv("C:/Users/User/Downloads/spamraw.csv")
str(sms)
sms$text = as.character(sms$text)
str(sms)
table(sms$type)
#making and printing vcorpus data
library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]]) #Double bracket is must
lapply(sms_corpus[1:2],as.character)

#data cleaning
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower)) #converting to lower case letters
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers) #removing numbers
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords()) #remvoing stop words
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation) #remving punctuation
library(SnowballC)
wordStem(c("learn","learned","learning"))
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)#removing spaces after doing above process
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus,
                               control = list(tolower = TRUE,
                                              removeNumbers = TRUE,
                                              stopwords = TRUE,
                                              removePunctuatio = TRUE,
                                              stemming = TRUE))
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms[1:4169,]$type
sms_test_labels <- sms[4170:5559,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Visualization1
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)

#Visualization2
spam <- subset(sms,type == "spam")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))

#Visualization3
ham <- subset(sms,type == "ham")
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

#showing repeated words
findFreqTerms(sms_dtm_train,5)
sms_freq_words <- findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)

#making frequency matrix for test and training
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]
convert_counts <- function(x){
  x <- ifelse(x>0,"Yes","No") 
}
sms_train <- apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)
library(e1071)
library(gmodels)
sms_classifier <- naiveBayes(sms_train,sms_train_labels)
sms_test_pred <- predict(sms_classifier,sms_test)
table(sms_test_pred,sms_test_labels)