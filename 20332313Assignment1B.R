library(syuzhet)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(tm)
library(caret)
library(e1071)


speech <- paste(readLines("C:/Users/Kiran/Desktop/La Trobe/Semester 3/Customer Analytics and Social Media/Assignments/Assignment 1/Case-study-B-dataset/hotel_tweets.csv"), collapse=" ")
speech

sentence <- get_sentences(speech)
sentiment <- get_sentiment(sentence)

simple_plot(sentiment)

nrc_lexicon <- get_sentiments("nrc")

nrc_Data <- get_nrc_sentiment(sentence)

angry <- which(nrc_Data$anger > 0)

sentence[angry]


td <- data.frame(t(nrc_Data))
tdSum <- data.frame(rowSums(td))
names(tdSum)[1] <- "Count"
tdSum <- cbind("sentiment" = rownames(tdSum), tdSum)
rownames(tdSum) <- NULL
tdSum <- tdSum[1:8,]

ggplot(data=tdSum, aes(x = sentiment)) +
  geom_bar(aes(weight=Count, fill=sentiment)) +
  ggtitle("Speech Sentiments") + guides(fill=FALSE)

#sentence_df <- data.frame(text = sentence, stringsAsFactors = FALSE)
sentence_df <- tibble(sentence = 1:length(sentence),
                       content = sentence_df)

sentence_df %>% head(5)


tidy_df<- sentence_df %>%
          mutate_all(as.character) %>%
          unnest_tokens(output=word, input=content)
tidy_df

#For joy
nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")

joy_words <- tidy_df %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

joy_words

joy_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Joy Emotion")


#For anger
nrc_anger <- get_sentiments("nrc") %>% filter(sentiment == "anger")

anger_words <- tidy_df %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

anger_words

anger_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("anger Emotion")

#For anticipation
nrc_anticipation <- get_sentiments("nrc") %>% filter(sentiment == "anticipation")

anticipation_words <- tidy_df %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

anticipation_words

anticipation_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Anticipation Emotion")

#For disgust
nrc_disgust <- get_sentiments("nrc") %>% filter(sentiment == "disgust")

disgust_words <- tidy_df %>%
  inner_join(nrc_disgust) %>%
  count(word, sort = TRUE)

disgust_words

disgust_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Disgust Emotion")

#For fear
nrc_fear <- get_sentiments("nrc") %>% filter(sentiment == "fear")

fear_words <- tidy_df %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

fear_words

fear_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Fear Emotion")

#For sadness
nrc_sadness <- get_sentiments("nrc") %>% filter(sentiment == "sadness")

sadness_words <- tidy_df %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = TRUE)

sadness_words

sadness_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Sadness Emotion")

#For surprise
nrc_surprise <- get_sentiments("nrc") %>% filter(sentiment == "surprise")

surprise_words <- tidy_df %>%
  inner_join(nrc_surprise) %>%
  count(word, sort = TRUE)

surprise_words

surprise_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("surprise Emotion")

#For trust
nrc_trust <- get_sentiments("nrc") %>% filter(sentiment == "trust")

trust_words <- tidy_df %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)

trust_words


trust_words %>%
  head(5) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, fill = "orange", stat = "identity") +
  geom_text(aes(label=n), hjust = -0.3, size=3.5) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("trust Emotion")

#######################################
##########            #################
##########Task - B - 2#################
##########            #################
#######################################


hotel_tweets_train <- read.csv("C:/Users/Kiran/Desktop/La Trobe/Semester 3/Customer Analytics and Social Media/Assignments/Assignment 1/Case-study-B-dataset/hotel_tweets.csv",nrows=200)
hotel_tweets_test <- read.csv("C:/Users/Kiran/Desktop/La Trobe/Semester 3/Customer Analytics and Social Media/Assignments/Assignment 1/Case-study-B-dataset/hotel_tweets.csv",skip = 200)
names(hotel_tweets_test) <- c("Negative","Positive")
hotel_tweets_test

tweetsPositive <- hotel_tweets_train$Positive
tweetsNegative <- hotel_tweets_train$Negative
tweetsPositiveTest <- hotel_tweets_test$Positive
tweetsNegativeTest <- hotel_tweets_test$Negative

tweetsTrain <- c(tweetsPositive, tweetsNegative)
tweetsTest <- c(tweetsPositiveTest, tweetsNegativeTest)
tweetsAll <- c(tweetsTrain, tweetsTest)

tweetsAll

class(hotel_tweets_train$Negative)
class(hotel_tweets_test$Negative)


sentimentTrain <- c(rep("positive",length(tweetsPositive)),rep("negative",length(tweetsNegative)))
sentimentTesting <- c(rep("positive",length(tweetsPositiveTest)),rep("negative",length(tweetsNegativeTest)))
sentimentAll <- as.factor(c(sentimentTrain,sentimentTesting))

tweetsCorpus <- Corpus(VectorSource(tweetsAll))

tweetsCorpus <- tm_map(tweetsCorpus, removeNumbers)
tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, stopwords("english"))

tweetsDTM <- DocumentTermMatrix(tweetsCorpus)

#SVM Classifier
tweetsMatrix <- as.matrix(tweetsDTM)

#training data
svmClassifier <- svm(tweetsMatrix[1:400,], as.factor(sentimentAll[1:400]))

svmPredicted <- predict(svmClassifier, tweetsMatrix[401:526,])
svmPredicted

table(svmPredicted, sentimentTesting)

confusionMatrix(as.factor(svmPredicted), as.factor(sentimentTesting))
