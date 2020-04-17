#na를 0으로
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

################ 주소 - 전처리 #####################
total.1 <- read.csv("total_1.csv")
head(total.1)

total.1$새주소부번 <- ifelse(total.1$새주소부번==0,"", total.1$새주소부번)
total.11 <- paste(total.1$시도명칭, total.1$시군구명칭,  total.1$도로명칭, total.1$새주소본번)
total.11 <-as.data.frame(total.11)
head(total.11)
total.11$total.11 <- as.character(total.11$total.11)

total.2 <- total.1$새주소부번
total.12 <- cbind(total.11, total.1$새주소부번, stringsAsFactors=FALSE)
colnames(total.12) <- c("주소1", "주소2")
head(total.12)
write.csv(total.12, "total.12.csv", row.names = FALSE)

total.3 <- data.frame()
for (i in 1:nrow(total.12)) {
  if (total.12[i,2] != "") {
    total.3[i,1] <- paste(total.12$주소1[i], total.12$주소2[i], sep="-")
  } else {
    total.3[i,1] <- total.12$주소1[i]
  }
}
str(total.3)


## 주소 도로명 경위도 합치기 ##
dir <- ("C:/Users/user/Desktop/빅캠_데이터")
file_list<- list.files(dir)
data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read.csv(file_list[i])
  data <- rbind(data, temp)
}
write.csv(data, "weedo.csv")
head(data)


#########################################
############### 최종도로 ################
## 최종도로 전처리 ##
## 도로명 ##
library(dplyr)
final.doro <- read.csv("최종도로.csv")

temp <- which(final.doro$도로명칭=="")
final.doro.na <- final.doro[-temp,] #NA 제거  

#도로 구별 도로명칭별 그룹  
head(final.doro)
group.doro <- final.doro.na %>% group_by(시군구명칭, 도로명칭) %>%
  summarise("X좌표min"=min(x), "X좌표max"=max(x), 
            "Y좌표min"=min(y), "Y좌표max"=max(y))
head(group.doro)
group.doro <- as.data.frame(group.doro)
write.csv(group.doro, "group.doro.csv", row.names = FALSE)

###############################################
######### 도로 구별 도로명칭별 그룹 ###########
group.doro <- read.csv("group.doro.csv")
head(group.doro)



################### 아파트-전처리 #####################
##상권_아파트
apt <- read.csv("상권_아파트.csv")
head(apt)
colnames(apt)[3] <- "도로명칭"

#아파트 도로명칭+경위도
doro.apt <- merge(x=apt, y=group.doro, by=c("도로명칭"))
head(doro.apt)
write.csv(doro.apt, "apt.csv", row.names=FALSE)

##상권배후지_아파트
apt.bae <- read.csv("상권배후지_아파트.csv")
colnames(apt.bae)[3] <- "도로명칭"

#아파트 도로명칭+경위도
doro.apt.bae <- merge(x=apt.bae, y=group.doro, by="도로명칭")
head(doro.apt.bae)
write.csv(doro.apt.bae, "apt.bae.csv")

## 전처리_기준년월코드가 높은 걸로 ##
#상권_아파트
apt <- read.csv("apt.csv")
apt.year <- apt %>% group_by(도로명칭) %>% filter(기준_년월_코드==max(기준_년월_코드))
apt.year <- as.data.frame(apt.year)
write.csv(apt.year, "apt.csv", row.names = FALSE)

#상권배후지_아파트
apt.bae <- read.csv("apt.bae.csv")
apt.bae.year <- apt.bae %>% group_by(도로명칭) %>% filter(기준년월.코드==max(기준년월.코드))
apt.bae.year <- as.data.frame(apt.bae.year)
write.csv(apt.bae.year, "apt.bae.csv", row.names = FALSE)


#######################################
## 최종 아파트 데이터 ##
apt <- read.csv("apt.csv")
apt.bae <- read.csv("apt.bae.csv")
head(apt)
head(apt.bae)
apt.bae<-apt.bae[,-1]
########################################


################ 자동차등록현황 ###################
## 성연령별 ##
gender <- read.csv("자동차등록현황_성_연령별.csv")
head(gender)
str(gender)

gender.sum <- apply(gender, 2, sum)
gender.sum <- as.data.frame(gender.sum)
gender.male <- gender.sum[4:12,]
gender.female <- gender.sum[14:22,]

library(RColorBrewer)
pct <- round(gender.male/sum(gender.male)*100,2)
pct.f <- round(gender.female/sum(gender.female)*100,2)

library(ggplot2)
gender2 <- data.frame()
gender2[1:9,1] <- pct.f 
gender2[10:18,1] <- pct
gender2[1:9,2] <- "Female"
gender2[10:18,2] <- "Male"
gender2[1:9,3] <- c("10대","20대","30대","40대","50대","60대","70대","80대","90대")
gender2[10:18,3] <- c("10대","20대","30대","40대","50대","60대","70대","80대","90대")

gender3 <- data.frame()
gender3[1:9,1] <- gender.female
gender3[10:18,1] <- gender.male
gender3[1:9,2] <- "Female"
gender3[10:18,2] <- "Male"
gender3[1:9,3] <- c("10대","20대","30대","40대","50대","60대","70대","80대","90대")
gender3[10:18,3] <- c("10대","20대","30대","40대","50대","60대","70대","80대","90대")

ggplot(data=gender3, aes(x="", y=V1, fill=V3)) +
  facet_grid(facets=.~V2) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = brewer.pal(9,"Set3"), guide=guide_legend(title="연령대"))+
  coord_polar(theta="y") +
  ggtitle("성별 자동차 등록현황(연령)")



################## 연료별 ######################3#
ryo <- read.csv("자동차등록현황_연료별.csv")
head(ryo)
ryo.group <- ryo %>% filter(차종별=="계")
ryo.group <- ryo.group[-c(1:3),-c(2:3)]

ryo.total.a <- data.frame()
ryo.total <- data.frame()
for (i in 1:7){
  ryo.total.a <- ryo.total 
  ryo.group.1 <- ryo.group[,c(1,i+1)]
  ryo.group.1[,3] <- colnames(ryo.group)[i+1]
  colnames(ryo.group.1)[2] <- "자동차등록대수" 
  
  ryo.total <- rbind(ryo.total.a, ryo.group.1)
} 
write.csv(ryo.total, "자동차등록현황_연료별 그래프.csv", row.names=FALSE)
ryo.total <- read.csv("자동차등록현황_연료별 그래프.csv")
head(ryo.total)

#집단 그래프
ggplot(data=ryo.total, aes(x=기간, y=자동차등록대수, color=V3, group=V3))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("기간별 연료별 자동차 등록대수")

#집단 비율 그래프
ryo.total.prop <- ddply(ryo.total, "기간", transform,
                        비율=round(자동차등록대수/sum(자동차등록대수),1))
ggplot(data=ryo.total.prop, aes(x=기간, y=비율, fill=V3, group=V3), desc=V3)+
  geom_area(colour=NA, alpha=0.5)+
  scale_fill_brewer(palette = "Set3")+
  ggtitle("기간별 연료별 자동차 등록대수")

#전기차 그래프
elec <- ryo.total[which(ryo.total$V3=="전기"),]
elec
ggplot(data=elec, aes(x=기간, y=자동차등록대수))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("전기 자동차 등록대수")+
  theme(plot.title=element_text(hjust=0.5, size=15))+
  geom_text(aes(label = 자동차등록대수),vjust=-0.6, size=6)+
  theme_bw()
ggplot(data=elec, aes(x=기간, y=자동차등록대수))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("전기 자동차 등록대수")+
  theme(plot.title=element_text(hjust=0.5, size=15))+
  geom_text(aes(label = 자동차등록대수),vjust=-0.6, size=6)+
  theme_bw()

elec1 <- elec[,1:2]
str(elec1)
elec2 <- ts(elec1$자동차등록대수, start=2010)
plot(elec2)



#막대그래프
mac <- ryo.total[which(ryo.total$기간=="2017"),]
ggplot(data=mac, aes(x=reorder(V3,-자동차등록대수), y=자동차등록대수, fill=V3))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Set3" )+
  geom_text(aes(label = 자동차등록대수),vjust=-0.6, size=6)+
  xlab("연료별 자동차")+
  theme_bw()+
  ggtitle("연료별 자동차 등록대수(2017)")

## 구별 ##
gu <- read.csv("자동차등록현황_구별.csv")
head(gu)
group.gu <- gu %>% group_by(자치구) %>%
  summarise("계"=mean(계), "승용차_자가용"=mean(승용차_자가용), "승용차_영업용"=mean(승용차_영업용), 
            "승합차_자가용"=mean(승합차_자가용), "승합차_영업용"=mean(승합차_영업용)
            )
group.gu <- as.data.frame(group.gu)
group.gu <- group.gu[-which(group.gu$자치구=="서울시"),]
row.names(group.gu) <- 1:25
colnames(group.gu)[1] <- "시군구명칭"


################## 2. 아파트 #######################
## 아파트 구별 합계 ##
apt.na <- apt
apt.na[is.na(apt.na)] <- 0
head(apt.na)

group.apt <- apt.na %>% group_by(시군구명칭) %>% 
  summarise("아파트단지수"=sum(아파트_단지_수), 
            "아파트면적66제곱미터미만세대수"=sum(아파트_면적_66_제곱미터_미만_세대_수),
            "아파트면적66제곱미터세대수"=sum(아파트_면적_66_제곱미터_세대_수),
            "아파트면적99제곱미터세대수"=sum(아파트_면적_99_제곱미터_세대_수),
            "아파트면적132제곱미터세대수"=sum(아파트_면적_132_제곱미터_세대_수),
            "아파트면적165제곱미터세대수"=sum(아파트_면적_165_제곱미터_세대_수),
            "아파트가격1억미만세대수"=sum(아파트_가격_1_억_미만_세대_수),
            "아파트가격1억세대수"=sum(아파트_가격_1_억_세대_수),
            "아파트가격2억세대수"=sum(아파트_가격_2_억_세대_수),
            "아파트가격3억세대수"=sum(아파트_가격_3_억_세대_수),
            "아파트가격4억세대수"=sum(아파트_가격_4_억_세대_수),
            "아파트가격5억세대수"=sum(아파트_가격_5_억_세대_수),
            "아파트가격6억이상세대수"=sum(아파트_가격_6_억_이상_세대_수),
            "아파트평균면적"=mean(아파트_평균_면적),
            "아파트평균시가"=mean(아파트_평균_시가)
            )
group.apt <- as.data.frame(group.apt)
head(group.apt)

apt.bae.na <- apt.bae
apt.bae.na[is.na(apt.bae.na)] <- 0
head(apt.bae.na)

group.apt.bae <- apt.bae.na %>% group_by(시군구명칭) %>% 
  summarise("배후아파트단지수"=sum(아파트.단지.수), 
            "배후아파트면적66제곱미터미만세대수"=sum(아파트.면적.66제곱미터.미만.세대.수),
            "배후아파트면적66제곱미터세대수"=sum(아파트.면적.66제곱미터.세대.수),
            "배후아파트면적99제곱미터세대수"=sum(아파트.면적.99제곱미터.세대.수),
            "배후아파트면적132제곱미터세대수"=sum(아파트.면적.132제곱미터.세대.수),
            "배후아파트면적165제곱미터세대수"=sum(아파트.면적.165제곱미터.세대.수),
            "배후아파트가격1억미만세대수"=sum(아파트.가격.1억.미만.세대.수),
            "배후아파트가격1억세대수"=sum(아파트.가격.1억.세대.수),
            "배후아파트가격2억세대수"=sum(아파트.가격.2억.세대.수),
            "배후아파트가격3억세대수"=sum(아파트.가격.3억.세대.수),
            "배후아파트가격4억세대수"=sum(아파트.가격.4억.세대.수),
            "배후아파트가격5억세대수"=sum(아파트.가격.5억.세대.수),
            "배후아파트가격6억이상세대수"=sum(아파트.가격.6억.이상.세대.수),
            "배후아파트평균면적"=mean(아파트.평균.면적),
            "배후아파트평균시가"=mean(아파트.평균.시가)
  )
group.apt.bae <- as.data.frame(group.apt.bae)
head(group.apt.bae)

# 배후지+상권 합치기
group.apt.total <- merge(x=group.apt, y=group.apt.bae, by="시군구명칭")
head(group.apt.total)
write.csv(group.apt.total, "group.apt.total.csv", row.names = FALSE)

#구별 
total.apt.gu <- merge(x=group.gu, y=group.apt.total, by="시군구명칭")

## 회귀분석 데이터 ##
lm.data <- read.csv("회귀_세대원수별세대수.csv")
lm.data1 <- read.csv("회귀_주차장수.csv")
lm.data2 <- read.csv("회귀_인구밀도.csv")
lm.data3 <- read.csv("국민기초생활보장수급자.csv")

#회귀_세대원수별 세대수: 구별 전처리
lm.total <- lm.data %>% filter(동=="소계")
lm.total.14 <- lm.total[1:300,] %>% group_by(자치구) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.14 <- as.data.frame(lm.total.14)
lm.total.14[,2] <- "2014"

lm.total.15 <- lm.total[301:600,] %>% group_by(자치구) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.15 <- as.data.frame(lm.total.15)
lm.total.15[,2] <- "2015"

lm.total.16 <- lm.total[601:900,] %>% group_by(자치구) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.16 <- as.data.frame(lm.total.16)
lm.total.16[,2] <- "2016"

lm.total.17 <- lm.total[901:1200,] %>% group_by(자치구) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.17 <- as.data.frame(lm.total.17)
lm.total.17[,2] <- "2017"

lm.total.total <- rbind(lm.total.14, lm.total.15, lm.total.16, lm.total.17)
head(lm.total.total)

lm.total1 <- lm.data1 %>% filter(동=="소계")
lm.total2 <- lm.data2 %>% filter(동=="소계")
lm.total3 <- lm.data3 %>% filter(동=="소계")

lm.tot <- merge(lm.total.total, lm.total1, by=c("자치구", "기간"))
lm.tot <- merge(lm.tot, lm.total2, by=c("자치구", "기간"))
lm.tot <- merge(lm.tot, lm.total3, by=c("자치구", "기간"))

lm.final <- merge(gu[,1:3],lm.tot, by=c("자치구", "기간"))
head(lm.final)
str(lm.final)
lm.final <- lm.final[,-c(9,12:28)]
write.csv(lm.final, "회귀분석.csv", row.names=FALSE)

lmlm <- read.csv("회귀분석.csv")
lmlm <- lmlm[,-c(1,2,3)]
head(lmlm)
lm.final <- lm(자동차등록대수~.-자치구-X10.19세-X20.29세-X70.79세-X80세이상, data=lmlm)
summary(lm.final)

#변수 선택법
head(lmlm)
str(lmlm)
lmlm2 <- lmlm[,-c(1,14,15,16,21,22)]
write.csv(lmlm2, "승연.csv", row.names=FALSE)
fit2=step(lm(자동차등록대수~1, data=lmlm2), 
          scope=formula(lm(자동차등록대수~., data=lmlm2)), direction="forward")
formula(fit2)
lm.final2 <- lm(formula(fit2), data=lmlm2)
summary(lm.final2)

train.1 <- lmlm2[1:75,]
test.1 <- lmlm2[76:100,]
lm.train <- lm(formula(fit2), data=train.1)
summary(lm.train)
pred.1 <- predict(lm.train, test.1)
R2(test.1$자동차등록대수, pred.1)

library(corrgram)
corrgram(cor(lmlm2[,-c(1)]),type="corr",upper.panel=panel.conf)

library(car)
vif(lm.final2)


## 동별 ##
head(lm.data)
head(lm.data1)
head(lm.data2)
head(lm.data3)
#회귀_세대원수별세대수
lm.dong <- lm.data %>% filter(동!="소계")
dong.14 <- lm.dong[which(substr(lm.dong$기간,1,4) == "2014"),] %>% group_by(동,자치구) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.14 <- as.data.frame(dong.14)
dong.14[,3] <- "2014"

dong.15 <- lm.dong[which(substr(lm.dong$기간,1,4) == "2015"),] %>% group_by(동,자치구) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.15 <- as.data.frame(dong.15)
dong.15[,3] <- "2015"

dong.16 <- lm.dong[which(substr(lm.dong$기간,1,4) == "2016"),] %>% group_by(동,자치구) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.16 <- as.data.frame(dong.16)
dong.16[,3] <- "2016"

dong.17 <- lm.dong[which(substr(lm.dong$기간,1,4) == "2017"),] %>% group_by(동,자치구) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.17 <- as.data.frame(dong.17)
dong.17[,3] <- "2017"

dong.total <- rbind(dong.14, dong.15, dong.16, dong.17)
dong.total <- dong.total[which(dong.total$기간=="2017"),]
rownames(dong.total) <- 1:nrow(dong.total)

#주차장수
jucha <- lm.data1 %>% filter(동!="소계")
jucha <- jucha[which(jucha$기간=="2017"),1:5]
rownames(jucha) <- 1:nrow(jucha)
head(jucha)

#인구밀도
dense <- lm.data2 %>% filter(동!="소계")
dense <- dense[which(dense$기간=="2017"),]
rownames(dense) <- 1:nrow(dense)
head(dense)

#수급자
su <- lm.data3  %>% filter(동!="소계"&동!="합계"&동!="본청")
su <- su[which(su$기간=="2017"),]
rownames(su) <- 1:nrow(su)
head(su)
str(su)
colnames(su)[4] <- "총수급자"

#합치기
lm.dong <- merge(dong.total, dense, by=c("자치구", "동", "기간"))
lm.dong <- merge(lm.dong, su, by=c("자치구", "동", "기간"))
lm.dong <- merge(lm.dong, jucha, by=c("자치구", "동", "기간"))
head(lm.dong)
write.csv(lm.dong, "회귀분석_동별(2017).csv")

## 동별 테스트셋
lm.dong <- read.csv("test2017.csv")
head(lm.dong)
lm.final <- lm(formula(fit2), data=lmlm2)
summary(lm.final)
pred.2 <- predict(lm.final, lm.dong)
head(pred.2)

#표준화
lm.scale <- scale(lmlm2)
lm.scale <- as.data.frame(lm.scale)
lm.scale.lm <- lm(formula(fit2), data=lm.scale)
summary(lm.scale.lm)
pred.scale <- predict(lm.scale.lm, lm.dong)
head(pred.scale)

#정규화
normalize <-function(x) {
  return ((x-min(x)) / (max(x) -min(x)))
}
normal <- normalize(lmlm2)

lm.normal.lm <- lm(formula(fit2), data=normal)
summary(lm.normal.lm)
pred.normal <- predict(lm.normal.lm, lm.dong)
str(lm.dong)
lm.dong[,23] <- pred.normal
colnames(lm.dong)[23] <- "자동차등록대수"
write.csv(lm.dong, "동별자동차등록대수(예측).csv", row.names=FALSE)

#정확도 테스트
normal.train <- normal[1:75,]
lm.normal.train <- lm(formula(fit2), data=normal.train)
pred.normal.test <- predict(lm.normal.train, test.1)
R2(pred.normal.test, test.1$자동차등록대수)
## 8/6 ##
#자동차등록대수에 영향을 미치는 변수로 회귀분석 진행
#트레인과 테스트 사이의 차이가 커서->정규화 진행->테스트 진행
#정규화한 데이터로 동별 회귀분석


###################################
## 동별자동차등록대수_예측
dong.car <- read.csv("동별자동차등록대수(예측).csv")
head(dong.car)

## gender->평균 => 연령&성별 가중치
gender.mean <- apply(gender, 2, mean)
gender.mean <- as.data.frame(gender.mean)
head(gender.mean)


write.csv(apt.total,"apt_total.csv", row.names=FALSE)

apt.dong <- merge(apt.total, final.doro[,c(4,7)], by="도로명칭", all=FALSE)
apt.dong.na <- unique(apt.dong)
head(final.doro)

levels(apt.dong$읍면동명칭)
levels(lm.dong$동)
 

# apt.dong.car <- merge(apt.dong.na,lm.dong, )
levels(final.doro$읍면동명칭)
levels(apt.total$도로명칭)


## 2. 랜포 ##
random.data <- read.csv("회귀분석.csv")
library(caret)

idx <- createDataPartition(random.data$자동차등록대수, p=.8, list=F)

data.train <- random.data[idx,]
data.test <- random.data[-idx,]

#교차검증
control =trainControl(method='cv', search='grid', number = 5)

#랜포
rf.model <- train(자동차등록대수~.-자치구, data=data.train, 
                         tuneGrid=data.frame(.mtry=c(4,6,8)),
                         trControl=control,
                         method="rf")
control.rf <- predict(rf.model, data.test)
R2(control.rf, data.test$자동차등록대수)
control.rf

##3.xgboost##



### (2).도로명칭 -> 동별로 ###
# 도로명칭에 따라 상세 주소가 있을것
head(apt)
head(apt.bae)
colnames(apt.bae) <- colnames(apt)

apt.total <- rbind(apt, apt.bae)
apt.total <- apt.total[,-c(2:3,19:23)] %>% group_by(도로명칭) %>% summarise_all(sum)
apt.total <- as.data.frame(apt.total)
head(apt.total)

#apt.merge: 최종도로+아파트에 있는 도로명칭 merge
apt.merge <- merge(final.doro[,c(6,9,12:13)], apt.total, by="도로명칭")
str(apt.merge)
head(apt.merge)
str(apt.merge)
write.csv(apt.merge, "아파트_도로명칭.csv", row.names=FALSE)

#apt.unique: 도로명칭은 법정동이 똑같을 것
apt.unique <- apt.merge[-which(duplicated(apt.merge$도로명칭)),]
head(apt.unique)
write.csv(apt.unique,"apt_unique.csv", row.names = FALSE)


## 법정동(관할주민센터~->다시 크롤링)
bub <- read.csv("법정동.csv")
head(bub)
bub <- bub[,-1]

bub.na <- na.zero(bub)

bub.na <- bub.na[which(substr(bub.na$법정동, 1,5)=="서울특별시"),]
bub.na$법정동 <- as.character(bub.na$법정동)
bub.na <- bub.na[!duplicated(bub.na$도로명칭),] #중복빼기
for (i in 1:nrow(bub.na)) {
  bub.na[i,21] <- strsplit(bub.na$법정동, " ")[[i]][3]
}
bub.na.final <- bub.na
bub.na.final$V21 <- substr(bub.na.final$V21, 1, nchar(bub.na.final$V21)-1)

#법정동있는거
bub.2 <- read.csv("법정동2.csv")
bub.2 <- na.zero(bub.2)
bub.2$법정동 <- as.character(bub.2$법정동)

for (i in 1:nrow(bub.2)) {
  bub.2[i,21] <- strsplit(bub.2$법정동, " ")[[i]][3]
}
bub.2.final <- bub.2
bub.2.final$V21 <- substr(bub.2.final$V21, 1, nchar(bub.2.final$V21)-1)
head(bub.2.final)

## 도로명칭별 법정동 최종 ##
colnames(bub.na.final) <- colnames(bub.2.final)
bub.final <- rbind(bub.2.final,bub.na.final)
head(bub.final)
write.csv(bub.final, "도로명칭별_법정동_최종.csv",row.names=FALSE)


## 
head(dong.car)
bub.final <- read.csv("도로명칭별_법정동_최종.csv")
head(bub.final)
bub.final <- bub.final[,-c(19:20)]
bub.final[which(bub.final$법정동=="가락본동"),]
bub.mean <- bub.final %>% group_by(법정동) %>% summarise_if(is.integer, mean)
bub.mean <- as.data.frame(bub.mean)

dong.group <- dong.car %>% group_by(동) %>% summarise_all(mean)
dong.group <- as.data.frame(dong.group)
head(dong.group)

## 2. 동별 예측 데이터
pre.apt.dong <- merge(x=bub.mean, y=dong.group[,c(1,23)], by.x="법정동", by.y="동")
head(pre.apt.dong)

# (1). 회귀
pre.dong <- lm(자동차등록대수~.-법정동, data=pre.apt.dong) 
summary(pre.dong)

# (2). 랜포
library(caret)
pre.rf <- pre.apt.dong
idx <- createDataPartition(pre.rf$자동차등록대수, p=.8, list=F)

data.train <- pre.rf[idx,]
data.test <- pre.rf[-idx,]

#교차검증
control =trainControl(method='cv', search='grid', number = 5)

#랜포
rf.model <- train(자동차등록대수~.-법정동, data=data.train, 
                         tuneGrid=data.frame(.mtry=c(4,6,8)),
                         trControl=control,
                         method="rf")
control.rf <- predict(rf.model, data.test)
R2(control.rf, data.test$자동차등록대수)

control.rf





#######################################################
############## 3. 집객시설과 유동인구 #################
## 집객시설 ##
jip <- read.csv("상권_집객시설.csv")
head(jip)
jip.bae <- read.csv("상권배후지_집객시설.csv")
head(group.doro)
head(jip.bae)


#합치고 기준년월코드 201712추출
jip.total <- rbind(jip, jip.bae)
jip.year <- jip.total %>% filter(기준_년월_코드==201712)
head(jip.year)

write.csv(jip.year,"집객시설_total.csv", row.names=FALSE)


####################################
## 최종 집객시설 파일 ##
jip <- read.csv("집객시설_total.csv")
####################################


## 유동인구 ##
##유동인구 전처리
flow.bae <- read.csv("상권배후지_추정유동인구.csv")
head(flow.bae)
str(flow.bae)

flow.bae.year <- flow.bae %>% filter(기준_년월_코드==201712)

## 유동인구 연령대+요일별+시간 -> 연령대 합으로
temp <- flow.bae.year
xy=0
for (i in 1:6){
  xy = xy+10
  temp[,(ncol(temp)+1)] <- apply(temp[,which(substr(name,1,8)==paste("남성연령대",xy,sep="_"))],1,sum)
  next
}
colnames(temp)
colnames(temp)[516:521] <- c("남성_10","남성_20","남성_30","남성_40", "남성_50","남성_60")

xx=0
for (i in 1:6){
  xx = xx+10
  temp[,(ncol(temp)+1)] <- apply(temp[,which(substr(name,1,8)==paste("여성연령대",xx,sep="_"))],1,sum)
  next
}
colnames(temp)[522:527] <- c("여성_10","여성_20","여성_30","여성_40", "여성_50","여성_60")
temp[,"상권여부"] <- 0
write.csv(temp,"유동배후지전처리.csv", row.names=FALSE)


#####################################
## 최종 유동인구 데이터 ##
flow <- read.csv("유동인구_total.csv")
flow <- flow[,-1]
head(flow)
str(flow)
flow <- flow[,-16]

## 유동인구 상권코드명따라 합치기 ##
flow.group <- flow %>% group_by(상권_코드_명) %>% summarise_all(sum)
flow.group <- as.data.frame(flow.group)
head(flow.group)
flow.group <- flow.group[,-c(2:3)]


################################
## 유동인구 + 최종도로 합치기 ##
#최종도로 -> 구별 도로명칭X -> 도로명칭별 x,y
doro.xy.min <- group.doro %>% group_by(도로명칭) %>%
  summarise_at(c("X좌표min", "Y좌표min"), min)
doro.xy.min <- as.data.frame(doro.xy.min)

doro.xy.max <- group.doro %>% group_by(도로명칭) %>%
  summarise_at(c("X좌표max", "Y좌표max"), max)
doro.xy.max <- as.data.frame(doro.xy.max)

#도로명칭별 xy
doro.xy <- merge(doro.xy.max, doro.xy.min, by="도로명칭")

flow.doro <- merge(flow.group, doro.xy, by.x = "상권_코드_명", by.y="도로명칭")
head(flow.doro,20)


################################
## 집객시설 ##
# jip.no.mar = 상권여부 없애고 합치기
jip <- na.zero(jip)
jip.no.mar <- jip[,-24] %>% group_by(상권_코드_명) %>% summarise_all(sum)
jip.no.mar <- as.data.frame(jip.no.mar)
jip.no.mar <- na.zero(jip.no.mar)
jip.no.mar <- jip.no.mar[,-c(2,3)]


## flow.zip = 상권코드에 따라 유동인구와 집객시설 합침 ##
head(flow)
head(flow.doro)
flow.jip <- merge(flow.doro, jip.no.mar, by="상권_코드_명")
flow.jip <- na.zero(flow.jip)
head(flow.jip)
summary(flow.jip)

## car.mean = 자동차 비율에 따라 곱해서 점수열 생성 ##
car.mean <- read.csv("자동차성연령별가중치.csv")
car.mean
## flow.jip = 회귀식 돌릴 데이터 ##
#점수: 유동인구에 가중치 곱
flow.jip$점수 <- 
  (flow.jip$남성_10*car.mean$w[1]
   +flow.jip$남성_20*car.mean$w[2]
   +flow.jip$남성_30*car.mean$w[3]
   +flow.jip$남성_40*car.mean$w[4]
   +flow.jip$남성_50*car.mean$w[5]
   +flow.jip$남성_60*car.mean$w[11]
   +flow.jip$여성_10*car.mean$w[6]
   +flow.jip$여성_20*car.mean$w[7]
   +flow.jip$여성_30*car.mean$w[8]
   +flow.jip$여성_40*car.mean$w[9]
   +flow.jip$여성_50*car.mean$w[10]
   +flow.jip$여성_60*car.mean$w[12])
head(flow.jip)
flow.jip <- flow.jip[,-c(2:13)]
head(flow.jip)


## 회귀분석 ##
str(flow.jip)
summary(flow.jip)

lm.flow.long <- lm(점수~.-상권_코드_명-점수-지하철_역_수-버스_정거장_수-X좌표max-Y좌표max-X좌표min-Y좌표min
                        -유치원_수-슈퍼마켓_수, data=flow.jip)
summary(lm.flow.long)

flow.jip.lm <- flow.jip
head(flow.jip.lm)
flow.jip.lm$집객회귀점수 <- predict(lm.flow.long, flow.jip.lm)
head(flow.jip.lm)
flow.jip.lm[which(flow.jip.lm$상권_코드_명=="가락로28길"),]

library(plyr)
flow.jip.lm <- arrange(flow.jip.lm, desc(집객회귀점수))
head(flow.jip.lm)

#구별 설치수
flow.gu <- merge(final.doro[,c(3,7)], flow.jip.lm, by.x="도로명칭", by.y="상권_코드_명")
flow.gu <- flow.gu[!duplicated(flow.gu),]
flow.gu.no <- flow.gu[,-c(3:6)]
flow.gu.sum <- flow.gu.no[,-1] %>% group_by(시군구명칭) %>% summarise_all(sum)
flow.gu.sum <- as.data.frame(flow.gu.sum)


#집객회귀점수의 비율*100
flow.prop <- flow.gu.sum
flow.prop$설치수 <- flow.prop$집객회귀점수/sum(flow.prop$집객회귀점수)*100
head(flow.prop)
sum(flow.prop$설치수)
str(flow.prop)
write.csv(flow.prop,"집객시설_구별설치수.csv", row.names=FALSE)


## x좌표 y좌표 합치기 ##
head(doro.xy)
flow.xy <- merge(flow.jip.lm, doro.xy,by.x = "상권_코드_명", by.y="도로명칭")
flow.xy <- arrange(flow.xy, desc(회귀점수))
head(flow.xy)
write.csv(flow.xy,"유동인구+집객시설_예측.csv", row.names=FALSE)

#도로명칭에 구별 합쳐서 구별 도로명칭에 몇개설치?
pre.3 <- read.csv("유동인구+집객시설_예측.csv")
head(pre.3)
pre.3.gu <- merge(pre.3, group.doro[,c(1,2)], by.x="상권_코드_명", by.y="도로명칭") 
head(pre.3.gu)
pre.3.gu <- pre.3.gu[!duplicated(pre.3.gu), ]
head(pre.3.gu)
str(pre.3.gu)
#구별 도로명별 회귀점수 높은 순으로 정렬
pre.3.desc <- pre.3.gu %>% group_by(시군구명칭) %>% arrange(시군구명칭,desc(회귀점수))
pre.3.desc <- as.data.frame(pre.3.desc)
head(pre.3.desc)
write.csv(pre.3.desc, "집객시설_구별도로명정렬.csv", row.names=FALSE)

head(dong.car)



#################################
########### 공영주차장 ##########
public <- read.csv("공영주차장.csv")
head(public)
str(public)

public$서울시주소 = paste("서울특별시",public$주소)
head(public)
head(flow.group)

head(final.doro$주소2)





