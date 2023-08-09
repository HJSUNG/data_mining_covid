setwd('C:/Rdirectory/data_mining/data_mining_covid');

#=================================================================================================================

covid_train = read.csv("covid_train.csv", header=T);

covid_train$is_dead = ifelse(covid_train$is_dead == 2,0,1);

str(covid_train);
head(covid_train);

#=================================================================================================================

covid_test = read.csv("covid_test.csv", header=T);

covid_test$is_dead = ifelse(covid_test$is_dead == 2,0,1);

str(covid_test);
head(covid_test);

#=================================================================================================================

covid_dead_train = read.csv("covid_dead_train.csv", header=T);

str(covid_dead_train);
head(covid_dead_train);

#=================================================================================================================

covid_dead_test = read.csv("covid_dead_test.csv", header=T);

str(covid_dead_test);
head(covid_dead_test);

#=================================================================================================================

logisticFit = glm(is_dead~., data =covid_train, family = binomial);
summary(logisticFit);

prediction = predict(logisticFit, newdata=covid_test, type = "response");
print(prediction);
prediction=round(prediction);

comparison=cbind(covid_test,prediction);
comparison=as.data.frame(comparison);
comparison
# print(comparison);

print(paste("test 건수 : ",nrow(covid_test)));
predictCorrect = comparison[comparison$is_dead == comparison$prediction,];
print(paste("사망여부 예측성공 건수 : ", nrow(predictCorrect)));
print(paste("사망여부 예측 정확도 : " ,nrow(predictCorrect)/nrow(covid_test))); # 60.9%

# true-positive, true-negative, false-positive, false-negative rate 계산
tp = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 1, ])/nrow(covid_test),2);
tn = round(nrow(comparison[comparison$is_dead == 0 & comparison$prediction == 0, ])/nrow(covid_test),2);
fp = round(nrow(comparison[comparison$is_dead == 0 & comparison$prediction == 1, ])/nrow(covid_test),2);
fn = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 0, ])/nrow(covid_test),2);

# true-positive, true-negative, false-positive, false-negative rate 계산  
confusion_matrix = matrix(c(tp, fn, fp, tn), nrow = 2, byrow = TRUE, dimnames = list(c("Actual Positive", "Actual Negative"), c("Predicted Positive", "Predicted Negative")))
confusion_matrix;


#=================================================================================================================

logisticFit_dead = glm(day_cnt~., data =covid_dead_train, family = gaussian );
summary(logisticFit_dead);

prediction_dead = predict(logisticFit_dead, newdata=covid_dead_test, type = "response");
prediction_dead;
prediction_dead = round(prediction_dead);

comparison_dead=cbind(covid_dead_test,prediction_dead);
comparison_dead=as.data.frame(comparison_dead);
# print(comparison_dead);

print(paste("test 건수 : ", nrow(covid_dead_test)));

# 투병일수 예측성공 기준 설정
deadPredictCorrectCreteria = 10;

deadPredictCorrect = comparison_dead[abs(comparison_dead$day_cnt-comparison_dead$prediction_dead)<=deadPredictCorrectCreteria, 0];
print(paste("투병일수 예측성공 건수(",deadPredictCorrectCreteria,"일) : " , nrow(deadPredictCorrect)));
print(paste("투병일수 예측 정확도(",deadPredictCorrectCreteria,"일) : ", nrow(deadPredictCorrect) / nrow(covid_dead_test)));

# 5일 : 51.7%, 7일 : 72.6%, 10일 : 92.1%

