# Loading libraries

install.packages("readxl")
install.packages("xlsx")
library(readxl)
library(xlsx)

setwd("C:/Users/Lukasz/Desktop/Credit Risk Scoring")

# Loading data
log_data_12 <- read_excel('checking_12.xlsx')
log_data_34 <- read_excel('checking_34.xlsx')

test_12 <- read_excel('checking_12_test.xlsx')
test_12 <- test_12[1:5]

test_34 <- read_excel('checking_34_test.xlsx')
test_34 <- test_34[1:5]


# Changing variables to categorical building logistic regression models
for(i in colnames(log_data_12)){
  log_data_12[[i]] <- as.factor(log_data_12[[i]])
}

for(i in colnames(log_data_34)){
  log_data_34[[i]] <- as.factor(log_data_34[[i]])
}

# Building logistic regression models
log_model_12 <- glm(Good ~., data = log_data_12, family = "binomial")
log_model_12

log_model_34 <- glm(Good ~., data = log_data_34, family = "binomial")
log_model_34

# Predicting values based on the derived model for scorecard
res_12_log <- predict(log_model_12, log_data_12, type="response")
res_12_log

res_34_log <- predict(log_model_34, log_data_34, type="response")
res_34_log

# Building scorecards for logistic models and saving data to file, so it is easy to open and read in Excel
scorecard_12_log <- log_data_12
scorecard_12_log$Score <- res_12_log
write.xlsx(scorecard_12_log, "scorecard_12_log.xlsx")

scorecard_34_log <- log_data_34
scorecard_34_log$Score <- res_34_log
write.xlsx(scorecard_34_log, "scorecard_34_log.xlsx")

# Validating scorecards for logistic regression

for(i in colnames(test_12)){
  test_12[[i]] <- as.factor(test_12[[i]])
}
for(i in colnames(test_34)){
  test_34[[i]] <- as.factor(test_34[[i]])
}

# Predicting values for test set with logistic regression
res_12_test <- predict(log_model_12, test_12, type="response")
res_34_test <- predict(log_model_34, test_34, type="response")

#for logistic 12
roc_log_12 <- matrix(ncol = 3)
gini_12 <- 0

test_range <- seq(0.999,0.001,-0.0001)

for(i in test_range){
  conf_12 <- table(Actual_Value = test_12$Good, Predicted_value = res_12_test > i)
  roc_log_12 <- rbind(roc_log_12, c(conf_12[4]/(conf_12[4] + conf_12[2]), 1 - conf_12[1]/(conf_12[1] + conf_12[3]), i))
}
gini_12 <- (as.numeric(roc_log_12[721:9281,1]) + as.numeric(roc_log_12[721:9281,3]) - 1)*0.0001
sum(2*gini_12)

plot(roc_log_12[,2], roc_log_12[,1], type="l")
lines(roc_log_12[,3], roc_log_12[,3])

#for logistic 34
roc_log_34 <- matrix(ncol = 3)

test_range <- seq(0.999,0.001,-0.0001)

for(i in test_range){
  conf_34 <- table(Actual_Value = test_34$Good, Predicted_value = res_34_test > i)
  roc_log_34 <- rbind(roc_log_34, c(conf_34[4]/(conf_34[4] + conf_34[2]), 1 - conf_34[1]/(conf_34[1] + conf_34[3]), i)) 
}
gini_34 <- (as.numeric(roc_log_34[100:8900, 1]) + as.numeric(roc_log_34[100:8900,3]) - 1)*0.0001
sum(2*gini_12)

plot(roc_log_34[,2], roc_log_34[,1], type="l")
lines(roc_log_34[,3], roc_log_34[,3])

# Building linear regression models
for(i in colnames(log_data_12)){
  log_data_12[[i]] <- as.integer(log_data_12[[i]])
}

for(i in colnames(log_data_34)){
  log_data_34[[i]] <- as.integer(log_data_34[[i]])
}

log_data_12$History <- log_data_12$History - 1
log_data_12$Good <- log_data_12$Good - 1
log_data_34$History <- log_data_34$History - 1
log_data_34$Good <- log_data_34$Good - 1

lin_model_12 <- glm(Good ~., data = log_data_12)
lin_model_12

lin_model_34 <- glm(Good ~., data = log_data_34)
lin_model_34

res_12_lin <- predict(lin_model_12, log_data_12, type="response")
res_12_lin

res_34_lin <- predict(lin_model_34, log_data_34, type="response")
res_34_lin

# Building scorecards for linear models
scorecard_12_lin <- log_data_12
scorecard_12_lin$Score <- res_12_log
write.xlsx(scorecard_12_lin, "scorecard_12_lin.xlsx")

scorecard_34_lin <- log_data_34
scorecard_34_lin$Score <- res_34_lin
write.xlsx(scorecard_34_lin, "scorecard_34_lin.xlsx")

# Validating scorecards for linear regression

for(i in colnames(test_12)){
  log_data_12[[i]] <- as.numeric(log_data_12[[i]])
}
