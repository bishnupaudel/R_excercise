# Train and evaluate model using Naive Bayes Classification

# Collecting data
setwd("C:/Users")

data <- read.csv("C:/Users/ribis/Documents/Udemy/spam.csv", header=T, stringsAsFactors = F)
head(data)
data <- data[1:2]
head(data)
str(data)
data$type <- factor(data$type)
str(data)
table(data$type)
prop.table(table(data$type))
prop.table(table(data$type))*100
library(tm)
data_cor <- Corpus(VectorSource(data$text))
inspect(data_cor[1:3])
#as.character(data_cor[[1]]) # to view in character form by lines
data_cor <- lapply(data_cor[1:2], as.character)
data_clean <- tm_map(data_cor, tolower)
inspect(data_clean[1:3])
data_clean <- tm_map(data_clean, removeNumbers)
data_clean <- tm_map(data_clean, removeWords, stopwords)
data_clean <- tm_map(data_clean, removePunctuation)
data_clean <- tm_map(data_clean, stripWhitespace)

library(SnowballC)
data_clean <- tm_map(data_clean, stemDocument)
inspect(data_clean)
data_clean <- tm_map(data_cor, stripWhitespace)
data_dtm <- DocumentTermMatrix(data_clean)
data_dtm
dim(data_dtm)

# spliting data
data_dtm_train <- data_dtm[1:4179, ]
data_dtm_test <-  data_dtm[4180:5572,]
data_train_labels <- data[1:4179, ]$type
data_test_labels <- data[4180:5572,]$type
table(data_train_labels)
prop.table(table(data_train_labels))
table(data_test_labels)
prop.table(table(data_test_labels))

library(wordcloud)
library(class)
wordcloud(data_clean, min.freq=40, random.order = F, random.color = F, col = c("red", "purple"))
spam <- subset(data, type=="spam")
wordcloud(spam$text, min.freq = 40, random.color = F, random.order = F, col=c("red", "green"))
ham <- subset(data, type=="ham")
wordcloud(ham$text, min.freq = 40, max.word=40, random.order = F, random.color = F, col=c("red", "green"))

# Finding frequently repeated words
data_frequent_words <- findFreqTerms(data_dtm_train, 5)
str(data_frequent_words)

data_dtm_freq_train <- data_dtm_train[, data_frequent_words]
str(data_dtm_train)

data_dtm_freq_test <- data_dtm_test[, data_frequent_words]
str(data_dtm_freq_test)

#Apply corpous on ham and spam matrix
ham_cor <- VCorpus(VectorSource(ham$text))
spam_cor <- VCorpus(VectorSource(spam$text))
ham_cor_dtm <- DocumentTermMatrix(ham_cor, control=list(tolower=T, removeNumbers=T, stopwords=T, removePunctuation=T, stemming=T))
spam_cor_dtm <- DocumentTermMatrix(spam_cor, control=list(tolower=T, removeNumbers=T, stopwords=T, removePunctuaion=T, stemming=T))                                  

# Find frequent words
ham_frequent_words <- findFreqTerms(ham_cor_dtm, lowfreq = 0, highfreq = Inf)
head(ham_frequent_words)
tail(ham_frequent_words)

# counting repeated or not using Naivebayes
convert_counts <- function(x){
  ifelse(x>0, "Yes", "No")
}

data_train <- apply(data_dtm_freq_train, MARGIN=2, convert_counts)
data_test <- apply(data_dtm_freq_test, MARGIN=2, convert_counts)

library(e1071)
data_classifier <- naiveBayes(data_train, data_train_labels)
data_test_predict <- predict(data_classifier, data_test)
table(data_test_predict)
library(gmodels)
CrossTable(data_test_labels, data_test_predict, prop.r=T, prop.c=F, prop.t=F, prop.chisq = F, dnn=c("Actual", "Predicted"))


# Model improvement

new_data_classifier <- naiveBayes(data_train, data_train_labels, laplace = 2)
new_data_classifier_predict <- predict(new_data_classifier, data_test)
CrossTable(data_test_labels, new_data_classifier_predict, prop.r=T, prop.c=F, prop.t=F, prop.chisq = F, dnn=c("Actual", "Predicted"))

