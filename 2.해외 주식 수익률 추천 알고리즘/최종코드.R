##최종 코드
library(MASS)
library(neuralnet)  #필요한 라이브러리
library(dplyr)
feature.data<-read.csv("new_feature.csv")
company<-aggregate(feature.data$수익률,by=list(feature.data$Company),FUN=sum)   #회사 120개를 추출하기 위해 aggregate 함수 사용해서 회사 추출
name<-company$Group.1		#name에 회사 저장
Date_s<-feature.data$Date[109:156]   #2013-01-01~2016-12-01 저장 
feature.data<-na.omit(feature.data)	#결측값 제거
feature.data$Date<-as.Date.character(feature.data$Date)	#기존 데이터형태로 저장된 값을 문자로 바꿈
Date<-feature.data$Date  #훈련 데이터,실험 데이터 나누기 위해 Date 저장  
c_number<-as.numeric(feature.data$Company) #회사별로 나누기 위해 회사에 각 숫자 지정 
feature.data<-feature.data[,-c(1,2,3,4)]  #정수형이 아닌 데이터 제거(마지막 4개의 컬럼)
max1=apply(feature.data,2,max)  # 정규화 위해 최댓값 통해  정규화 
min1=apply(feature.data,2,min)  # 정규화 위해 최소값 통해 정규화 
s_data=scale(feature.data,center=min1,scale=max1-min1)	# 정규화 한 데이터
s_data<-as.data.frame(s_data)
s_data<-cbind(s_data,Date)
s_data<-cbind(s_data,c_number)	# 정규화 한 데이터로 dataframe만들고 +날짜+지정 회사번호
tr_s<-'2004-01-01'
tr_e<-'2012-12-01'
tt_s<-'2013-01-01'
tt_e<-'2016-12-01'	#훈련& 실험데이터 지정
train.data<-s_data[s_data$Date>=tr_s & s_data$Date<=tr_e,]  #훈련 데이터 2004~2012 실험 데이터 2013~2016
test.data<-s_data[s_data$Date>=tt_s & s_data$Date<=tt_e,]
train.data<-train.data[,-c(24,25)]  #신경망 만들기 위해 정수형 데이터 외 제거(날짜, 회사지정번호)
test.data<-test.data[,-c(24,25)] 
inc<-read.csv("inc_8.csv") # 사전에 랜덤포레스트가 가장 높게 나왔을 때의 가중치 적용(각 변수의 수익률에 영향을 주는 정도)
for(i in 1:22){
  feature.data[,i]<-feature.data[,i]*inc$Inc[i]
}

max1=apply(feature.data,2,max)  # 가중치 적용한 값을 최댓값 최솟값을 이용(다시 지정)
min1=apply(feature.data,2,min)	
s_data=scale(feature.data,center=min1,scale=max1-min1)
s_data<-as.data.frame(s_data)	
s_data<-cbind(s_data,Date)
s_data<-cbind(s_data,c_number)
train.data<-s_data[s_data$Date>=tr_s & s_data$Date<=tr_e,]  #훈련 데이터 2004~2012 실험 데이터 2013~2016
test.data<-s_data[s_data$Date>=tt_s & s_data$Date<=tt_e,]
train.data<-train.data[,-c(24,25)]  #신경망 만들기 위해 정수형 데이터 외 제거
test.data<-test.data[,-c(24,25)]
detach(package:dplyr)  #dplyr을 쓰면 신경망이 안돌아가므로 dplyr 제거 
n=names(train.data)  #form 을 만들기 위해 훈련데이터의 열 이름을 n으로 선언 
form=as.formula(paste('수익률~',paste(n[!n %in% '수익률'],collapse='+')))  #수익률을 설명하기 위한 설명변수 모두 사용 
fit_nn<-neuralnet(form,data=train.data,hidden=c(5,2),linear.output=T)  #(5,3) or (6,2) or (6,3)  신경망 모델 
pred<-compute(fit_nn,test.data[,1:22])  #설명변수 1:22 로 부터 만든 예측 모델의 뉴런과 예측값 정보  
yhat_nn<-pred$net.result*(max(feature.data$수익률)-min(feature.data$수익률))+min(feature.data$수익률)  #예측 데이터를 다시 실제값으로 변환
real_nn<-test.data$수익률*(max(feature.data$수익률)-min(feature.data$수익률))+min(feature.data$수익률)  #실제 데이터를 다시 실제값으로 변환
mean((real_nn-yhat_nn)^2)	#손실
cor(yhat_nn,test.data$수익률)  #상관관계  

group<-function(x){
  library(dplyr)  #각 회사 분류하기 위해 선언 
  gro_com<-filter(s_data,c_number==x)  #c_number를 통해 분류하여 각 회사별로 행 추출 
  c_number<-gro_com$c_number
  Date<-gro_com$Date
  gro_com<-gro_com[,-c(24,25)]  #위와 반복  
  if(is.na(gro_com[1,])){  #결측값을 통해 행이 하나도 없을 수 있으므로  만약 gro_comdl 하나도 없다면 48개의 행을 가진 행렬 생성  
    r<-matrix(NA,ncol=1,nrow=48)
    c(r)
  }
  else{
    detach(package:dplyr)  #compute사용을 위해 dplyr제거 
    pred.com<-compute(fit_nn,gro_com[,1:22])  #위에서 만든 모델로 설명변수 1:22개를 통해 수익률 예측 
    pred<-pred.com$net.result*(max(feature.data$수익률)-min(feature.data$수익률))+min(feature.data$수익률) #예측값인 pred.com$net.result를 pred에 저장 
    pred<-as.data.frame(pred)
    k<-data.frame(Date_s)  #위에서 저장한 2013-01-01~2016-12-01 추출
    k$Date_s<-as.Date(k$Date_s)
    Date<-as.data.frame(Date) 
    pr<-c()
    for(i in 1:48){   
      ifelse(k$Date_s[i]==Date[1:nrow(Date),],pr<-rbind(pr,pred),pr<-rbind(pr,NA))
    }
    pr<-head(pr,48)  #위에서 부터 48개(2013-01-01~2016-12-01)를 추출하여 2013~2016
    c(pr)
  }
}
k<-matrix(NA,ncol=1,nrow=48)
for(i in 1:120){  
  c<-group(i)   #120개의 회사를 추출  
  c<-as.data.frame(c)
  colnames(c)<-name[i]
  k<-cbind(k,c)
}
day<-as.data.frame(Date_s)          
top5<-matrix(NA,ncol=5,nrow=1)		#전체 데이터를 담기 위한 dataframe
top<-matrix(NA,ncol=5,nrow=1)		#매 월마다 상위 5개의 예측 수익률을 뽑아내기 위한 dataframe
cols_top<-c("예측순위1","예측순위2","예측순위3","예측순위4","예측순위5")
colnames(top)<-cols_top
colnames(top5)<-cols_top	#둘 다 rank1~rank5 열
sum=0
top_sum<-c()
for(i in 1:48){
  final<-sort(k[i,])  #k의 각 행을 오름차순으로 정렬 (예측 수익률)
  for(j in 0:4){
    sum<-sum+final[1,length(final)-j]
  }
  top<-colnames(final[length(final)-0:4])   # 제일 높은 값부터 5개 회사 추출  
  top_sum<-rbind(top_sum,sum)	#최종 수익률의 합
  sum=0
  top5<-rbind(top5,top)  #추출된 5개의 회사(top)를  top5에 쌓는다  
}
top5<-top5[-1,]
colnames(day)<-c("월")
colnames(top_sum)<-c("5개 회사의 수익률 합")
top5<-cbind(day,top5)		
top5<-cbind(top5,top_sum)


write.csv(top5,"예측3.csv")



