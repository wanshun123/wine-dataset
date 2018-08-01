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
# index of rows
index <- sample(1:nrow(df),size = 0.8*nrow(df))
#train <- df[index,]
#test <- df[-index,]

### random forest

library(randomForest)
# for a random forest, quality has to be a factor
df_rf <- df
df_rf$quality <- as.factor(df_rf$quality)
train_rf <- df_rf[index,]
test_rf <- df_rf[-index,]
rf_model <- randomForest(quality ~., data = train_rf)
rf_prediction <- predict(rf_model, test_rf)
sqrt(mean((rf_prediction-test$quality)^2)) # pointless if quality is a factor
# get the caret and e1071 package to make a confusion matrix
library(caret)
library(e1071)
rf_matrix <- confusionMatrix(rf_prediction, test_rf$quality)

### linear model

# linear model doesn't work on factors or integers - so make "good" and "bad" wine column (will be factor even though it's logical 1 or 0)
df_lm <- df
df_lm$quality <- ifelse(df_lm$quality>5,1,0) # df_lm$quality has to be integer - won't work if already converted to factor
df_lm$quality <- as.factor(df_lm$quality)
train_lm <- df_lm[index,]
test_lm <- df_lm[-index,]
lm_model <- glm(quality ~., data = train_lm, family = "binomial")
lm_prediction <- predict(lm_model, test_lm, type = "response")
# ROC curve
ROC_lm <- roc(test_lm$quality, lm_prediction)
ROC_lm_auc <- auc(ROC_lm)
# and confusion matrix, requiring a definite prediction
lm_prediction <- round(lm_prediction, digits = 0)
#lm_prediction <- ifelse(lm_prediction>0.5,1,0) - this or round(), either way
lm_prediction <- as.factor(lm_prediction)
lm_matrix <- confusionMatrix(lm_prediction, test_lm$quality)

# linear model predicting 3, 4, 5, 6, 7, 8
df_lm <- df
#df_lm$quality <- ifelse(df_lm$quality>5,1,0) # df_lm$quality has to be integer - won't work if already converted to factor
#df_lm$quality <- as.factor(df_lm$quality)
train_lm <- df_lm[index,]
test_lm <- df_lm[-index,]
lm_model <- glm(quality ~., data = train_lm)
lm_prediction <- predict(lm_model, test_lm)
lm_prediction <- round(lm_prediction, digits = 0)
#lm_prediction <- as.factor(lm_prediction)
#test_lm$quality <- as.factor(test_lm$quality)
lm_matrix <- confusionMatrix(lm_prediction, test_lm$quality)

# random forest on binomial "good" or "bad" df_lm - same thing
rf2_model <- randomForest(quality ~., data = train_lm)
rf2_prediction <- predict(rf2_model, test_lm)
rf2_matrix <- confusionMatrix(rf2_prediction, test_lm$quality)
# and for returning probability
rf2_prediction_prob <- predict(rf2_model, test_lm, type='prob')
# ROC curve
ROC_rf <- roc(test_lm$quality, rf2_prediction_prob[,2])
ROC_rf_auc <- auc(ROC_rf)

### stepwise regression
# df as loaded straight from cv, quality not changed to factors (?)

df_sw <- df
#df_sw$quality <- as.factor(df_sw$quality)

null_model <- glm(quality ~ 1, data = df_sw)
full_model <- glm(quality ~ ., data = df_sw)
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
stepwise_purchase_prediction <- predict(step_model, type = "response")
stepwise_purchase_prediction <- round(stepwise_purchase_prediction, digits = 0)
#stepwise_purchase_prediction <- as.factor(stepwise_purchase_prediction)
sw_matrix <- confusionMatrix(stepwise_purchase_prediction, df_sw$quality)

#library(pROC)
#stepwise_purchase_prediction <- predict(step_model, type = "response")
#ROC <- multiclass.roc(df_sw$quality, stepwise_purchase_prediction)

### stepwise regression to determine good or bad

df2_sw <- df
df2_sw$quality <- ifelse(df_sw$quality>5,1,0)

null_model <- glm(quality ~ 1, data = df2_sw)
full_model <- glm(quality ~ ., data = df2_sw)
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
stepwise_purchase_prediction <- predict(step_model, type = "response")
ROC_sw <- roc(df2_sw$quality, stepwise_purchase_prediction)
ROC_sw_auc <- auc(ROC_sw)
