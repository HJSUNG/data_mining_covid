setwd('C:/Rdirectory/data_mining/data_mining_covid');

#=================================================================================================================

covid_train = read.csv("covid_train.csv", header=T);

str(covid_train);
head(covid_train);

#=================================================================================================================

covid_test = read.csv("covid_test.csv", header=T);

str(covid_test);
head(covid_test);

#=================================================================================================================

covid_dead_train = read.csv("covid_dead_train.csv", header=T);

# covid_dead_train$sex = as.numeric(covid_dead_train$sex);
# covid_dead_train$patient_type = as.numeric(covid_dead_train$patient_type);
# covid_dead_train$pneumonia = as.numeric(covid_dead_train$pneumonia);
# covid_dead_train$age = as.numeric(covid_dead_train$age);
# covid_dead_train$diabetes = as.numeric(covid_dead_train$diabetes);
# covid_dead_train$copd = as.numeric(covid_dead_train$copd);
# covid_dead_train$asthma = as.numeric(covid_dead_train$asthma);
# covid_dead_train$inmsupr = as.numeric(covid_dead_train$inmsupr);
# covid_dead_train$hypertension = as.numeric(covid_dead_train$hypertension);
# covid_dead_train$other_disease = as.numeric(covid_dead_train$other_disease);
# covid_dead_train$cardiovascular = as.numeric(covid_dead_train$cardiovascular);
# covid_dead_train$obesity = as.numeric(covid_dead_train$obesity);
# covid_dead_train$renal_chronic = as.numeric(covid_dead_train$renal_chronic);
# covid_dead_train$tobacco = as.numeric(covid_dead_train$tobacco);
# covid_dead_train$contact_other_covid = as.numeric(covid_dead_train$contact_other_covid);
# covid_dead_train$day_cnt = as.numeric(covid_dead_train$day_cnt);

str(covid_dead_train);
head(covid_dead_train);

#=================================================================================================================

covid_dead_test = read.csv("covid_dead_test.csv", header=T);

# covid_dead_test$sex = as.numeric(covid_dead_test$sex);
# covid_dead_test$patient_type = as.numeric(covid_dead_test$patient_type);
# covid_dead_test$pneumonia = as.numeric(covid_dead_test$pneumonia);
# covid_dead_test$age = as.numeric(covid_dead_test$age);
# covid_dead_test$diabetes = as.numeric(covid_dead_test$diabetes);
# covid_dead_test$copd = as.numeric(covid_dead_test$copd);
# covid_dead_test$asthma = as.numeric(covid_dead_test$asthma);
# covid_dead_test$inmsupr = as.numeric(covid_dead_test$inmsupr);
# covid_dead_test$hypertension = as.numeric(covid_dead_test$hypertension);
# covid_dead_test$other_disease = as.numeric(covid_dead_test$other_disease);
# covid_dead_test$cardiovascular = as.numeric(covid_dead_test$cardiovascular);
# covid_dead_test$obesity = as.numeric(covid_dead_test$obesity);
# covid_dead_test$renal_chronic = as.numeric(covid_dead_test$renal_chronic);
# covid_dead_test$tobacco = as.numeric(covid_dead_test$tobacco);
# covid_dead_test$contact_other_covid = as.numeric(covid_dead_test$contact_other_covid);
# covid_dead_test$day_cnt = as.numeric(covid_dead_test$day_cnt);

str(covid_dead_test);
head(covid_dead_test);

#=================================================================================================================

# install.packages("neuralnet");
library(neuralnet);

nn1 = neuralnet(is_dead~., data=covid_train, algorithm = "rprop+", act.fct = 'logistic',linear.output = TRUE, hidden =3);
plot(nn1);
summary(nn1);
# saveRDS(nn1, "nn1.rds");

prediction = predict(nn1, covid_test[], type="response");
computation = compute(nn1, covid_test[1:15]);
plot(covid_test$is_dead~computation$net.result)

prediction = round(prediction)
# prediction

computation = round(computation$net.result)
# computation

comparison_prediction=cbind(covid_test,prediction);
comparison_prediction=as.data.frame(comparison_prediction);
# print(comparison_prediction);

comparison_computation=cbind(covid_test,computation);
comparison_computation=as.data.frame(comparison_computation);
# print(comparison_computation);


# prediction 함수 통한 예측
print(paste("test 건수 : ",nrow(covid_test)));
predictCorrect_prediction = comparison_prediction[comparison_prediction$is_dead == comparison_prediction$prediction,];
print(paste("사망여부 예측성공 건수 : ", nrow(predictCorrect_prediction)));
print(paste("사망여부 예측 정확도 : " ,nrow(predictCorrect_prediction)/nrow(covid_test))); # 61.2%

# computation 함수 통한 예측
print(paste("test 건수 : ",nrow(covid_test)));
predictCorrect_computation = comparison_computation[comparison_computation$is_dead == comparison_computation$computation,];
print(paste("사망여부 예측성공 건수 : ", nrow(predictCorrect_computation)));
print(paste("사망여부 예측 정확도 : " ,nrow(predictCorrect_computation)/nrow(covid_test))); # 61.3%

#=================================================================================================================

# 학습수렴 안함
nn1_dead = neuralnet(day_cnt~., data=covid_dead_train, algorithm = "rprop+", act.fct = 'logistic',linear.output = TRUE, hidden =3, stepmax = 100000);
