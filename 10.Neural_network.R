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

str(covid_dead_train);
head(covid_dead_train);

#=================================================================================================================

covid_dead_test = read.csv("covid_dead_test.csv", header=T);

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
