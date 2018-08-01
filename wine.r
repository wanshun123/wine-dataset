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

### random forest

library(randomForest)
rf_model <- randomForest(quality ~., data = train)
rf_prediction <- predict(rf_model, test)
sqrt(mean((rf_prediction-test$quality)^2)) # pointless if quality is a factor
# get the caret and e1071 package to make a confusion matrix
library(caret)
library(e1071)
rf_matrix <- confusionMatrix(rf_prediction, test$quality)

### linear model

# linear model doesn't work on factors - so make "good" and "bad" wine column (will be factor even though it's logical 1 or 0)
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
# random forest on binomial "good" or "bad" df2 - same thing
rf2_model <- randomForest(quality ~., data = train2)
rf2_prediction <- predict(rf2_model, test2)
rf2_matrix <- confusionMatrix(rf2_prediction, test2$quality)

### stepwise regression
# df as loaded straight from cv, quality not changed to factors

df <- read.csv("winequality-red.csv")

null_model <- glm(quality ~ 1, data = train)
full_model <- glm(quality ~ ., data = train)
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
stepwise_purchase_prediction <- predict(step_model)
stepwise_purchase_prediction <- round(stepwise_purchase_prediction, digits = 0)
stepwise_purchase_prediction <- as.factor(stepwise_purchase_prediction)
sw_matrix <- confusionMatrix(stepwise_purchase_prediction, train$quality)
