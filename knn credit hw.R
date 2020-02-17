germ <- file.choose()
germ <- read.csv(germ)

# KNN prediction using knn function in class package
library(class)
set.seed(200, sample.kind = "Rounding")

germ$Default = as.factor(germ$Default)

train <- sample(nrow(germ), nrow(germ)/2)
germ.train <- germ[train, ]
germ.test <- germ[-train, ]
# Note that knn function does model fitting and prediction in one
# function call k = 11 specifies the number of neighbors used
germ.knn <- knn(scale(germ.train[, sapply(germ, is.numeric)]), 
                scale(germ.test[, sapply(germ, is.numeric)]), 
                germ.train$Default, k = 21)
# generate the confusion matrix from knn prediction
table(germ.test$Default, germ.knn)
