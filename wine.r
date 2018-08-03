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

### tuning random forest with caret and ranger (ranger method means random forest)

library(ranger)

ranger_model <- train(
  quality ~ .,
  tuneLength = 10,
  data = train_svm, method = "ranger",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE)
)

ranger_predict <- predict(ranger_model, test_svm)
mean(ranger_predict == test_svm$quality)
ranger_matrix <- confusionMatrix(ranger_predict, test_svm$quality)

# mtry is number of randomly selected variables used at each split
# tuneLength tells caret how many different variations to try

myControl <- trainControl(
  method = "cv", number = 10,
  #summaryFunction = twoClassSummary,
  #classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

library(glmnet)

#levels(svm_df$quality) <- make.names(levels(factor(svm_df$quality)))

model <- train(
  quality ~ ., train_svm,
  method = "glmnet",
  trControl = myControl
)

glm_predict <- predict(model, test_svm)
mean(glm_predict == test_svm$quality)
glm_matrix <- confusionMatrix(glm_predict, test_svm$quality)

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
lm_prediction <- as.factor(lm_prediction)
test_lm$quality <- as.factor(test_lm$quality)
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

stepwise_purchase_prediction <- as.factor(stepwise_purchase_prediction)
df_sw$quality <- as.factor(df_sw$quality)
sw_matrix <- confusionMatrix(stepwise_purchase_prediction, df_sw$quality)

library(pROC)
#stepwise_purchase_prediction <- predict(step_model, type = "response")
#ROC <- multiclass.roc(df_sw$quality, stepwise_purchase_prediction)

### stepwise regression to determine good or bad

df2_sw <- df
df2_sw$quality <- ifelse(df2_sw$quality>5,1,0)

null_model <- glm(quality ~ 1, data = df2_sw)
full_model <- glm(quality ~ ., data = df2_sw)
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
stepwise_purchase_prediction <- predict(step_model, type = "response")
ROC_sw <- roc(df2_sw$quality, stepwise_purchase_prediction)
ROC_sw_auc <- auc(ROC_sw)

stepwise_purchase_prediction <- round(stepwise_purchase_prediction, digits = 0)
stepwise_purchase_prediction <- as.factor(stepwise_purchase_prediction)
df2_sw$quality <- as.factor(df2_sw$quality)
sw_matrix2 <- confusionMatrix(stepwise_purchase_prediction, df2_sw$quality)


### SVM

svm_df <- df
svm_df$quality <- as.factor(svm_df$quality)
train_svm <- svm_df[index,]
test_svm <- svm_df[-index,]
svm_model <- svm(quality ~., train_svm, type = "C-classification")
svm_predict <- predict(svm_model, test_svm)
svm_predict <- as.factor(svm_predict)
svm_matrix <- confusionMatrix(svm_predict, test_svm$quality)

tune_out <- 
    tune.svm(x = train_svm[, -12], y = train_svm[, 12], 
             type = "C-classification", cost = 10^(-1:2), 
             gamma = c(0.1, 1, 10), coef0 = c(0.1, 1, 10))

#list optimal values
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
tune_out$best.parameters$coef0

svm_model <- svm(quality~ ., data = train_svm, type = "C-classification", 
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma, 
                 coef0 = tune_out$best.parameters$coef0)
                 
accuracy <- rep(NA, 100)
                 
for (i in 1:100){
    index <- sample(1:nrow(df),size = 0.8*nrow(df))
    train_svm <- svm_df[index,]
    test_svm <- svm_df[-index,]
    svm_model <- svm(quality ~ ., data = train_svm, type = "C-classification")
    pred_test <- predict(svm_model, test_svm)
    accuracy[i] <- mean(pred_test == test_svm$quality)
}

for (i in 1:100){
    index <- sample(1:nrow(df),size = 0.8*nrow(df))
    train_svm <- svm_df[index,]
    test_svm <- svm_df[-index,]
    svm_model <- svm(quality~ ., data = train_svm, type = "C-classification", 
                 kernel = "radial",
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma, 
                 coef0 = tune_out$best.parameters$coef0)
    pred_test <- predict(svm_model, test_svm)
    accuracy[i] <- mean(pred_test == test_svm$quality)
}

### rpart

library(rpart)
rpart_model <- rpart(quality ~., train_svm)
rpart_prediction <- predict(rpart_model, test_svm, type = "class", control = rpart.control(cp = 0))
rpart_matrix <- confusionMatrix(rpart_prediction, test_svm$quality)

### bagging trees?

library(ipred)

bagging_model <- bagging(formula = quality ~ ., 
                         data = train_svm,
                         coob = TRUE)
bagging_predict <- predict(bagging_model, test_svm)
bagging_matrix <- confusionMatrix(bagging_predict, test_svm$quality)

### boosting trees?

library(gbm)

#cv.folds necessary if doing gbm.perf
boosting_model <- gbm(formula = quality ~ ., 
                    distribution = "multinomial", 
                    data = train_svm,
                    n.trees = 10000,
                    cv.folds = 2)

summary(boosting_model)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = boosting_model, 
                         method = "cv")
                         
boosting_predict <- predict(boosting_model, test_svm, n.trees = ntree_opt_cv)
#boosting_matrix <- confusionMatrix(boosting_predict, test_svm$quality)
