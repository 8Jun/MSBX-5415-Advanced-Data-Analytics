songs <- file.choose()
songs <- read.csv(songs)

str(songs)

#Q1
nrow(subset(songs, year==2010))
#Answer: 373

#Q2
nrow(subset(songs, artistname=="Michael Jackson"))
#Answer: 18

#Q3
MJ <- subset(songs, artistname=="Michael Jackson")
as.data.frame(MJ[c(which(MJ$Top10==1)),2])
#Answer: You Rock My World, You Are Not Alone, Black or White, Remember the Time, In The Closet

#Q4
unique(songs$timesignature)
#Answer: 3 4 5 7 1 0

#Q5
table(songs$timesignature)
#Answer: 4

#Q6
songs[which.max(songs$tempo),2]
#Answer: Wanna Be Startin' Somethin'

#Q7
SongsTrain <- subset(songs, year<2010)
SongsTest <- subset(songs, year==2010)

dim(SongsTrain)
#Answer: 7201

#Q8
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% nonvars) ]

Model_1 <- glm(Top10 ~., data = SongsTrain, family = binomial)
summary(Model_1)
#Answer: 4827

#Q9
#Answer: The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10

#Q10
#Answer: Mainstream listeners tend to prefer less complex songs

#Q11
Answer: Mainstream listeners prefer songs with heavy instrumentation

#Q12
#Answer: False

#Q13
#cor = correlation function
cor(SongsTrain$loudness, SongsTrain$energy)
#Answer: 0.7399067
#Model_1 suffers from multicollinearity b/c these two variables are higly correlated

#Q14
Model_2 <- glm(Top10 ~. - loudness, data = SongsTrain, family = binomial)
summary(Model_2)
#Answer: Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.

#Q15
Model_3 <- glm(Top10 ~. - energy, data = SongsTrain, family = binomial)
summary(Model_3)
#Answer: True

#Q16
predictTest <- predict(Model_3, newdata = SongsTest, type="response")
#type = "response" gives the predicted probabilities

#confMat = confusion matrix with threshold of 0.45

# Confusion Matrix definition - A confusion matrix is a table that is often used to describe the 
# performance of a classification model (or "classifier") on a set of test data for which 
# the true values are known

# true positives (TP): These are cases in which we predicted yes
# true negatives (TN): We predicted no
# false positives (FP): We predicted yes, but they don't actuallymake Top 10 (Also known as a "Type I error.")
# false negatives (FN): We predicted no, but they actually make Top 10 (Also known as a "Type II error.")


confMat <- table(SongsTest$Top10, predictTest >= 0.45)
confMat

accuracy <- (confMat[1, 1]+confMat[2, 2])/sum(confMat
accuracy
#Answer: 0.8793566

#Q17
confMat
accuracy_baseline <- (309+5)/sum(confMat
accuracy_baseline
#Answer: 0.8418231

#Q18
confMat
#True positives
confMat[2, 2]
#Answer: 19

#Q19
#False positives
confMat[1, 2]
#Answer: 5

#Q20

sensitivity <- confMat[2, 2]/sum(confMat[2, ])
sensitivity
#Answer: 0.3220339

#Q21
confMat
specificity <- confMat[1, 1] / sum(confMat[1, ])
specificity
#Answer: 0.9840764

#Q22

















