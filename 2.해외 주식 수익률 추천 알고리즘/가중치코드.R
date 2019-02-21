library(randomForest)
library(MASS)
feature.data<-read.csv("new_feature.csv")		#전체 데이터 읽어옴
company<-aggregate(feature.data$수익률,by=list(feature.data$Company),FUN=sum)   #회사 120개명을 추출하기 위해 aggregate 함수 사용해서 회사 추출
name<-company$Group.1
Date_s<-feature.data$Date[109:156]   #2013-01-01~2016-12-01 저장 
feature.data<-na.omit(feature.data)
feature.data$Date<-as.Date.character(feature.data$Date)
Date<-feature.data$Date  #훈련 데이터,실험 데이터 나누기 위해 Date 저장  
c_number<-as.numeric(feature.data$Company) #회사별로 나누기 위해 회사에 각 숫자 저장 
feature.data<-feature.data[,-c(1,2,3,4)]  #정수형이 아닌 데이터 제거
max1=apply(feature.data,2,max)  # 정규화 위해 최댓값 최솟값을 통해 전체 데이터 정규화 
min1=apply(feature.data,2,min)
s_data=scale(feature.data,center=min1,scale=max1-min1)
s_data<-as.data.frame(s_data)
s_data<-cbind(s_data,Date)
s_data<-cbind(s_data,c_number)
tr_s<-'2011-01-01'
tr_e<-'2015-12-01'
tt_s<-'2016-01-01'
tt_e<-'2016-12-01'
train.data<-s_data[s_data$Date>=tr_s & s_data$Date<=tr_e,]  #훈련 데이터 2004~2012 실험 데이터 2013~2016
test.data<-s_data[s_data$Date>=tt_s & s_data$Date<=tt_e,]
Date<-test.data$Date
train.data<-train.data[,-c(27,28)]  # 정수형 데이터 외 제거
test.data<-test.data[,-c(27,28)]
fit_rf=randomForest(수익률~.,data=train.data,ntree=100,mtry=5,importance=T,na.action=na.omit) #각 설명변수들의 수익률에 영향을 미치는 정도(가중치)를 랜덤포레스트로 구함
impor<-importance(fit_rf,type=2)
impor<-as.data.frame(impor)

write.csv(impor,"inc_8.csv")
