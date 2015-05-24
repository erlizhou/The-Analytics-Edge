NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

SimpleMod = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)
PredTest = predict(SimpleMod, newdata=NewsTest, type="response")
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTrain$hour = NewsTrain$PubDate$hour
NewsTest$hour = NewsTest$PubDate$hour
summary(NewsTrain)
head(NewsTrain)

logit <- glm(Popular~WordCount+NewsDesk+SectionName+SubsectionName+Weekday+hour, data=NewsTrain, family=binomial)
logitpredict <- predict(logit, newdata=NewsTest,type="response")
logitSubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = logitpredict)
write.csv(logitSubmission, "SubmissionSimpleLogistic.csv", row.names=FALSE)

library(tm)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))
HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount
HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)
PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = 0.1*PredTest + 0.9*logitpredict)
write.csv(MySubmission, "Combined.csv", row.names=FALSE)
