setwd('C:/Rdirectory/data_mining/data_mining_covid');

#=================================================================================================================

# 1. 원본 데이터에서 결측치 있는 record 제거

covid_original = read.csv("covid_original.csv", header=T);

covid_original$sex = as.factor(covid_original$sex);
covid_original$patient_type = as.factor(covid_original$patient_type);
covid_original$intubated = as.factor(covid_original$intubated);
covid_original$pneumonia = as.factor(covid_original$pneumonia);
covid_original$age = as.numeric(covid_original$age);
covid_original$pregnancy = as.factor(covid_original$pregnancy);
covid_original$diabetes = as.factor(covid_original$diabetes);
covid_original$copd = as.factor(covid_original$copd);
covid_original$asthma = as.factor(covid_original$asthma);
covid_original$inmsupr = as.factor(covid_original$inmsupr);
covid_original$hypertension = as.factor(covid_original$hypertension);
covid_original$other_disease = as.factor(covid_original$other_disease);
covid_original$cardiovascular = as.factor(covid_original$cardiovascular);
covid_original$obesity = as.factor(covid_original$obesity);
covid_original$renal_chronic = as.factor(covid_original$renal_chronic);
covid_original$tobacco = as.factor(covid_original$tobacco);
covid_original$contact_other_covid = as.factor(covid_original$contact_other_covid);
covid_original$covid_res = as.factor(covid_original$covid_res);
covid_original$icu = as.factor(covid_original$icu);
covid_original$is_dead = as.factor(covid_original$is_dead);
covid_original$day_cnt = as.numeric(covid_original$day_cnt);

str(covid_original);

covid_original_without_missing = covid_original[
  (covid_original$intubated == 1 | covid_original$intubated == 2)
  & (covid_original$pneumonia == 1 | covid_original$pneumonia == 2)
  & (covid_original$pregnancy == 1 | covid_original$pregnancy == 2)
  & (covid_original$diabetes == 1 | covid_original$diabetes == 2)
  & (covid_original$copd == 1 | covid_original$copd == 2)
  & (covid_original$asthma == 1 | covid_original$asthma == 2)
  & (covid_original$inmsupr == 1 | covid_original$inmsupr == 2)
  & (covid_original$hypertension == 1 | covid_original$hypertension == 2)
  & (covid_original$other_disease == 1 | covid_original$other_disease == 2)
  & (covid_original$cardiovascular == 1 | covid_original$cardiovascular == 2)
  & (covid_original$obesity == 1 | covid_original$obesity == 2)
  & (covid_original$renal_chronic == 1 | covid_original$renal_chronic == 2)
  & (covid_original$tobacco == 1 | covid_original$tobacco == 2)
  & (covid_original$contact_other_covid == 1 | covid_original$contact_other_covid == 2)
  & (covid_original$icu == 1 | covid_original$icu == 2)
  ,];


nrow(covid_original_without_missing); # 23158
# write.csv(covid_original_without_missing, "covid_without_missing.csv", row.names = FALSE);

#=================================================================================================================

# 사망여부 = 1 인 레코드만 모아서 따로 생성

covid_original_dead_without_missing = covid_original_without_missing[covid_original_without_missing$is_dead==1,];  
nrow(covid_original_dead_without_missing); # 4020
# write.csv(covid_original_dead_without_missing, "covid_dead_without_missing.csv", row.names = FALSE);

#=================================================================================================================

# 미사용 변수 일괄 제거

covid_original_without_missing = covid_original_without_missing[,!names(covid_original_without_missing) %in% c("id", "entry_date","date_symptoms", "date_died", "day_cnt", "intubated", "pregnancy", "covid_res", "icu")];

# covid_orginal_without_missing > train 20000개, test 3158개로 분리

covid_train = covid_original_without_missing[1:20000,]; # 20000개
covid_test = covid_original_without_missing[-(1:20000),]; #3158개

# test는 그대로 사용, train은 is_dead 불균형 해소 위해 ROSE 패키지 이용하여 오버샘플링 진행

# install.packages("ROSE");
library(ROSE);

train_oversampling_cnt = nrow(covid_train) - 2*nrow(covid_train[covid_train$is_dead == 1, ]); #12356

train_oversampling_data = ROSE(is_dead ~ ., data = covid_train, N=50000, seed=10 )$data;
train_oversampling_data_dead = train_oversampling_data[train_oversampling_data$is_dead==1,];

covid_train_oversampled = rbind(covid_train, train_oversampling_data_dead[1:train_oversampling_cnt,]); #32356개

write.csv(covid_train_oversampled, "covid_train.csv", row.names = F);

test_oversampling_cnt = nrow(covid_test) - 2*nrow(covid_test[covid_test$is_dead == 1, ]); #2762

test_oversampling_data = ROSE(is_dead ~ ., data = covid_test, N=50000, seed=20 )$data;
test_oversampling_data_dead = test_oversampling_data[test_oversampling_data$is_dead==1,];

covid_test_oversampled = rbind(covid_test, test_oversampling_data_dead[1:test_oversampling_cnt,]); #5920개

write.csv(covid_test_oversampled, "covid_test.csv", row.names = F);

nrow(covid_test_oversampled[covid_test_oversampled$is_dead==2,])


#=================================================================================================================

# 미사용 변수 일괄 제거


covid_original_dead_without_missing = covid_original_dead_without_missing[,!names(covid_original_dead_without_missing) %in% c("id", "entry_date","date_symptoms", "date_died", "is_dead", "intubated", "pregnancy", "covid_res", "icu")];

# covid_original_dead_without_missing > train 3600개, test 420개로 분리

covid_dead_train = covid_original_dead_without_missing[1:3600,];
covid_dead_test = covid_original_dead_without_missing[-(1:3600),];

write.csv(covid_dead_train, "covid_dead_train.csv", row.names = F);
write.csv(covid_dead_test, "covid_dead_test.csv", row.names = F);









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