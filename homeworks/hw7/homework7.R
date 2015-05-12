# part 1
setwd("c:/users/erlizhou/Desktop/MOOC/The_Analytics_Edge/homeworks/hw7")
require(ggplot2)
require(ggmap)
require(maps)
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")
polling <- read.csv("PollingImputed.csv")
head(polling)
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(TestPredictionBinary)
summary(TestPrediction)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
dim(predictionMap)
dim(statesMap)
?merge
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Prediction 2012")
subset(predictionDataFrame, Test.State == "Florida")
?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black", linetype=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                      breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black", size=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                      breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black", alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                      breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# part 2
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
dim(users)
dim(edges)
146/59*2
head(users)
table(users$locale)
table(users$gender, users$locale)
require(igraph)
?graph.data.frame
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g))
V(g)$size = degree(g)/2+2
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
table(users$school)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
table(users$school)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
?igraph.plotting

# part 3
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
head(tweets)
corpus = Corpus(VectorSource(tweets$Tweet))
require(tm)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
frequencies
require(wordcloud)
?wordcloud
colnames(allTweets)
rownames(allTweets)
str(allTweets)
sum(allTweets)
colSums(allTweets)
rowSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))
?brewer.pal
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues"))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[5:9]) 

# part 4
parole <- read.csv("parole.csv")
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)
table(parole$male, parole$violator)
table(parole$state, parole$crime)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)
