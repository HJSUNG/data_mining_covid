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

# install.packages("RWeka");
# install.packages("caret");
# install.packages("partykit");
library(RWeka);
library(caret);
library(partykit);

# sex, patient_type : factor형 변수인데 level이 1개라 에러가 남 > column 제거거

covid_train = covid_train[,!names(covid_train) %in% c("sex", "patient_type")];
covid_test = covid_test[,!names(covid_test) %in% c("sex", "patient_type")];

covid_dead_train = covid_dead_train[,!names(covid_dead_train) %in% c("sex", "patient_type")];
covid_dead_test = covid_dead_test[,!names(covid_dead_test) %in% c("sex", "patient_type")];

#=================================================================================================================

# train = createFolds(covid_train$is_dead, k=10);

# C45Fit = train(is_dead ~ ., data=covid_train, trControl=trainControl(method = "cv", indexOut = train));
# saveRDS(C45Fit, "C45Fit.rds");

# train 오래 걸리므로, 저장한 모델 불러와서 사용 
# C45Fit = readRDS("C45Fit.rds");
# C45Fit
# str(C45Fit)
# C45Fit$finalModel
# plot(C45Fit$finalModel)

# J48 = Rweka 패키지에서 C4.5 알고리즘을 사용한 decision tree 만드는 Function

J48Fit = J48(is_dead ~ ., data = covid_train, control = Weka_control(C = 0.1));
plot(J48Fit);text(J48Fit);
print(J48Fit);

prediction = predict(J48Fit, newdata=covid_test[], type="class");
summary(prediction);

comparison=cbind(covid_test,prediction);
comparison=as.data.frame(comparison);
# print(comparison);

print(paste("test 건수 : ",nrow(covid_test)));
predictCorrect = comparison[comparison$is_dead == comparison$prediction,];
print(paste("사망여부 예측성공 건수 : ", nrow(predictCorrect)));
print(paste("사망여부 예측 정확도 : " ,nrow(predictCorrect)/nrow(covid_test))); # confidenceFactor/ 0.1 : 91.4%, 0.5: 90.8%

# true-positive, true-negative, false-positive, false-negative rate 계산
tp = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 1, ])/nrow(covid_test),2);
tn = round(nrow(comparison[comparison$is_dead == 2 & comparison$prediction == 2, ])/nrow(covid_test),2);
fp = round(nrow(comparison[comparison$is_dead == 2 & comparison$prediction == 1, ])/nrow(covid_test),2);
fn = round(nrow(comparison[comparison$is_dead == 1 & comparison$prediction == 2, ])/nrow(covid_test),2);

# true-positive, true-negative, false-positive, false-negative rate 계산  
confusion_matrix = matrix(c(tp, fn, fp, tn), nrow = 2, byrow = TRUE, dimnames = list(c("Actual Positive", "Actual Negative"), c("Predicted Positive", "Predicted Negative")))
confusion_matrix;

#=================================================================================================================

#C4.5 알고리즘은 Factor 형 변수를 다루기 위해 만들어진 알고리즘이라, numeric class 를 지원하지 않음
