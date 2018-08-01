df <- read.csv("winequality-red.csv")
library(corrplot)
library(ggplot2)
library(ggpubr)
# no NA's
apply(df, 2, function(x) sum(is.na(x)))
# check correlations - focus on quality
corrplot(cor(df))
ggscatter(df, x = "quality", y = "alcohol", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

# count of each rating
ggplot(df, aes(x = quality)) + geom_bar()
# turn quality into a factor?
df$quality <- as.factor(df$quality)
# test and train
index <- sample(1:nrow(df),size = 0.8*nrow(df))
train <- df[index,]
test <- df[-index,]
# try randomForest
library(randomForest)
rf_model <- randomForest(quality ~., data = train)
rf_prediction <- predict(rf_model, test)
sqrt(mean((rf_prediction-test$quality)^2)) # pointless if quality is a factor
# get the caret and e1071 package to make a confusion matrix
library(caret)
library(e1071)
lf_matrix <- confusionMatrix(rf_prediction, test$quality)
# linear model doesn't work on factors - so make "good" and "bad" wine column (logical)
df2 <- read.csv("winequality-red.csv")
df2$quality <- ifelse(df2$quality>5,1,0) # df2$quality has to be integer - won't work if already converted to factor
df2$quality <- as.factor(df2$quality)
train2 <- df2[index,]
test2 <- df2[-index,]
lm_model <- glm(quality ~., data = train2, family = "binomial")
lm_prediction <- predict(lm_model, test2, type = "response")
lm_prediction <- round(lm_prediction, digits = 0)
#the below or round(), either way
#lm_prediction <- ifelse(lm_prediction>0.5,1,0)
lm_prediction <- as.factor(lm_prediction)
lm_matrix <- confusionMatrix(lm_prediction, test2$quality)
