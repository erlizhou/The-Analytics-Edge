songs = read.csv("songs.csv")
summary(songs)
table(songs$year)
table(songs$artistname)["Michael Jackson"]
MJ = subset(songs, artistname=="Michael Jackson")
which(MJ$songtitle=="Beat It")
MJ[13,]$Top10
which(MJ$songtitle=="You Rock My World")
MJ[1,]$Top10
which(MJ$songtitle=="Billie Jean")
MJ[5,]$Top10
which(MJ$songtitle=="You Are Not Alone")
MJ[4,]$Top10
table(songs$timesignature)
which.max(songs$tempo)
songs$songtitle[6206]
SongsTrain = subset(songs, year <= 2009)
nrow(SongsTrain)
SongsTest = subset(songs, year == 2010)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% "energy") ]
SongsLog3 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog3)
