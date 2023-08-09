setwd('C:/Rdirectory/data_mining/data_mining_covid');

#=================================================================================================================

covid_train = read.csv("covid_train.csv", header=T);

covid_train$sex = as.factor(covid_train$sex);
covid_train$patient_type = as.factor(covid_train$patient_type);
covid_train$pneumonia = as.factor(covid_train$pneumonia);
covid_train$age = as.numeric(covid_train$age);
covid_train$diabetes = as.factor(covid_train$diabetes);
covid_train$copd = as.factor(covid_train$copd);
covid_train$asthma = as.factor(covid_train$asthma);
covid_train$inmsupr = as.factor(covid_train$inmsupr);
covid_train$hypertension = as.factor(covid_train$hypertension);
covid_train$other_disease = as.factor(covid_train$other_disease);
covid_train$cardiovascular = as.factor(covid_train$cardiovascular);
covid_train$obesity = as.factor(covid_train$obesity);
covid_train$renal_chronic = as.factor(covid_train$renal_chronic);
covid_train$tobacco = as.factor(covid_train$tobacco);
covid_train$contact_other_covid = as.factor(covid_train$contact_other_covid);
covid_train$is_dead = as.factor(covid_train$is_dead);

str(covid_train);
head(covid_train);

#=================================================================================================================

covid_test = read.csv("covid_test.csv", header=T);

covid_test$sex = as.factor(covid_test$sex);
covid_test$patient_type = as.factor(covid_test$patient_type);
covid_test$pneumonia = as.factor(covid_test$pneumonia);
covid_test$age = as.numeric(covid_test$age);
covid_test$diabetes = as.factor(covid_test$diabetes);
covid_test$copd = as.factor(covid_test$copd);
covid_test$asthma = as.factor(covid_test$asthma);
covid_test$inmsupr = as.factor(covid_test$inmsupr);
covid_test$hypertension = as.factor(covid_test$hypertension);
covid_test$other_disease = as.factor(covid_test$other_disease);
covid_test$cardiovascular = as.factor(covid_test$cardiovascular);
covid_test$obesity = as.factor(covid_test$obesity);
covid_test$renal_chronic = as.factor(covid_test$renal_chronic);
covid_test$tobacco = as.factor(covid_test$tobacco);
covid_test$contact_other_covid = as.factor(covid_test$contact_other_covid);
covid_test$is_dead = as.factor(covid_test$is_dead);

str(covid_test);
head(covid_test);

#=================================================================================================================

covid_dead_train = read.csv("covid_dead_train.csv", header=T);

covid_dead_train$sex = as.factor(covid_dead_train$sex);
covid_dead_train$patient_type = as.factor(covid_dead_train$patient_type);
covid_dead_train$pneumonia = as.factor(covid_dead_train$pneumonia);
covid_dead_train$age = as.numeric(covid_dead_train$age);
covid_dead_train$diabetes = as.factor(covid_dead_train$diabetes);
covid_dead_train$copd = as.factor(covid_dead_train$copd);
covid_dead_train$asthma = as.factor(covid_dead_train$asthma);
covid_dead_train$inmsupr = as.factor(covid_dead_train$inmsupr);
covid_dead_train$hypertension = as.factor(covid_dead_train$hypertension);
covid_dead_train$other_disease = as.factor(covid_dead_train$other_disease);
covid_dead_train$cardiovascular = as.factor(covid_dead_train$cardiovascular);
covid_dead_train$obesity = as.factor(covid_dead_train$obesity);
covid_dead_train$renal_chronic = as.factor(covid_dead_train$renal_chronic);
covid_dead_train$tobacco = as.factor(covid_dead_train$tobacco);
covid_dead_train$contact_other_covid = as.factor(covid_dead_train$contact_other_covid);
covid_dead_train$day_cnt = as.numeric(covid_dead_train$day_cnt);

str(covid_dead_train);
head(covid_dead_train);

#=================================================================================================================

covid_dead_test = read.csv("covid_dead_test.csv", header=T);

covid_dead_test$sex = as.factor(covid_dead_test$sex);
covid_dead_test$patient_type = as.factor(covid_dead_test$patient_type);
covid_dead_test$pneumonia = as.factor(covid_dead_test$pneumonia);
covid_dead_test$age = as.numeric(covid_dead_test$age);
covid_dead_test$diabetes = as.factor(covid_dead_test$diabetes);
covid_dead_test$copd = as.factor(covid_dead_test$copd);
covid_dead_test$asthma = as.factor(covid_dead_test$asthma);
covid_dead_test$inmsupr = as.factor(covid_dead_test$inmsupr);
covid_dead_test$hypertension = as.factor(covid_dead_test$hypertension);
covid_dead_test$other_disease = as.factor(covid_dead_test$other_disease);
covid_dead_test$cardiovascular = as.factor(covid_dead_test$cardiovascular);
covid_dead_test$obesity = as.factor(covid_dead_test$obesity);
covid_dead_test$renal_chronic = as.factor(covid_dead_test$renal_chronic);
covid_dead_test$tobacco = as.factor(covid_dead_test$tobacco);
covid_dead_test$contact_other_covid = as.factor(covid_dead_test$contact_other_covid);
covid_dead_test$day_cnt = as.numeric(covid_dead_test$day_cnt);

str(covid_dead_test);
head(covid_dead_test);

#=================================================================================================================

# install.packages("partykit");
# install.packages("CHAID", repos="http://R-Forge.R-project.org");

library(partykit);
library(CHAID);

# chaid 함수는 범주형 변수들로만 이루어진 데이터를 처리하기 위해 설계 > numeric 변수인 age 제거

covid_train = covid_train[,!names(covid_train) %in% c("age")];
covid_test = covid_test[,!names(covid_test) %in% c("age")];

covid_dead_train = covid_dead_train[,!names(covid_dead_train) %in% c("age")];
covid_dead_test = covid_dead_test[,!names(covid_dead_test) %in% c("age")];

chaidFit = chaid(is_dead~., data = covid_train, control = chaid_control(minsplit = 5000, minprob = 0.4));

plot(chaidFit);
print(chaidFit);

prediction = predict(chaidFit, newdata=covid_test[]);
# prediction = predict(chaidFit, newdata=covid_test[], type="prob");
summary(prediction);

comparison=cbind(covid_test,prediction);
comparison=as.data.frame(comparison);
# print(comparison);

print(paste("test 건수 : ",nrow(covid_test)));
predictCorrect = comparison[comparison$is_dead == comparison$prediction,];
print(paste("사망여부 예측성공 건수 : ", nrow(predictCorrect))); #3579
print(paste("사망여부 예측 정확도 : " ,nrow(predictCorrect)/nrow(covid_test))); # 60.4%

# true-positive, true-negative, false-positive, false-negative rate 계산
tp = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 1, ])/nrow(covid_test),2);
tn = round(nrow(comparison[comparison$is_dead == 2 & comparison$prediction == 2, ])/nrow(covid_test),2);
fp = round(nrow(comparison[comparison$is_dead == 2 & comparison$prediction == 1, ])/nrow(covid_test),2);
fn = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 2, ])/nrow(covid_test),2);

# true-positive, true-negative, false-positive, false-negative rate 계산  
confusion_matrix = matrix(c(tp, fn, fp, tn), nrow = 2, byrow = TRUE, dimnames = list(c("Actual Positive", "Actual Negative"), c("Predicted Positive", "Predicted Negative")))
confusion_matrix;

