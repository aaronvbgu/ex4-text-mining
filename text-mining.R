# setwd("~/Downloads/EX4/ex4-text-mining")
# install.packages("readr")
# install.packages("party")
# install.packages("iterators")
# install.packages("scales")
# install.packages("pbkrtest")
# install.packages("caret", dependencies = TRUE)
# install.packages(Metrics)
# install.packages("stringdist")
# install.packages("stringi")
library(readr)
train <- read_csv("train.csv")
test  <- read_csv("test.csv")


### Preprocess the train data
library(stringdist)
# Jaccard distance
train$jaccard_title <- stringdist(train$query ,train$product_title, method="jaccard") 
train$jaccard_description <- stringdist(train$query, train$product_description, method="jaccard")
# Jaro Winker distance
train$jw_title <- stringdist(train$query, train$product_title, method="jw") 
train$jw_description <- stringdist(train$query, train$product_description, method="jw") 
# Damerau–Levenshtein (Optimal string alignment distance)
train$osa_title <- stringdist(train$query, train$product_title, method="osa") 
train$osa_description <- stringdist(train$query, train$product_description, method="osa")
 

### Preprocess the test data as well
# Jaccard distance
test$jaccard_title <- stringdist(test$query ,test$product_title, method="jaccard") 
test$jaccard_description <- stringdist(test$query, test$product_description, method="jaccard")
# Jaro Winker distance
test$jw_title <- stringdist(test$query, test$product_title, method="jw") 
test$jw_description <- stringdist(test$query, test$product_description, method="jw")
# Damerau–Levenshtein (Optimal string alignment distance)
test$osa_title <- stringdist(test$query, test$product_title, method="osa") 
test$osa_description <- stringdist(test$query, test$product_description, method="osa")


library(stringi)
train$count <- sapply(train$query,
                      function(x) {
                        stri_stats_latex(str=x)['Words']
                      })
test$count <- sapply(test$query,
                     function(x) {
                       stri_stats_latex(str=x)['Words']
                       })

### Preprocessing
train$median_relevance <- factor(train$median_relevance)


### Train/test evaluation
inTraining <- sample(1:nrow(train), .75*nrow(train))
training <- train[ inTraining,]
testing  <- train[-inTraining,]


### Model creation
library(randomForest)
# Note: jaccard_description is not good- causing an error in randomforest
model <- randomForest(median_relevance ~ jw_title + jw_description + jaccard_title + osa_title + osa_description + count,
                      data=training,
                      ntree=100)


### Classification for evaluation
results <- predict(model, newdata=testing)


### Evaluation of results
library(Metrics)
ScoreQuadraticWeightedKappa(testing$median_relevance, results, 1, 4)
Newsubmission = data.frame(id=testing$id, prediction=results)
write.csv(Newsubmission, "model5.csv", row.names=F) 
