#na�� 0����
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

################ �ּ� - ��ó�� #####################
total.1 <- read.csv("total_1.csv")
head(total.1)

total.1$���ּҺι� <- ifelse(total.1$���ּҺι�==0,"", total.1$���ּҺι�)
total.11 <- paste(total.1$�õ���Ī, total.1$�ñ�����Ī,  total.1$���θ�Ī, total.1$���ּҺ���)
total.11 <-as.data.frame(total.11)
head(total.11)
total.11$total.11 <- as.character(total.11$total.11)

total.2 <- total.1$���ּҺι�
total.12 <- cbind(total.11, total.1$���ּҺι�, stringsAsFactors=FALSE)
colnames(total.12) <- c("�ּ�1", "�ּ�2")
head(total.12)
write.csv(total.12, "total.12.csv", row.names = FALSE)

total.3 <- data.frame()
for (i in 1:nrow(total.12)) {
  if (total.12[i,2] != "") {
    total.3[i,1] <- paste(total.12$�ּ�1[i], total.12$�ּ�2[i], sep="-")
  } else {
    total.3[i,1] <- total.12$�ּ�1[i]
  }
}
str(total.3)


## �ּ� ���θ� ������ ��ġ�� ##
dir <- ("C:/Users/user/Desktop/��ķ_������")
file_list<- list.files(dir)
data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read.csv(file_list[i])
  data <- rbind(data, temp)
}
write.csv(data, "weedo.csv")
head(data)


#########################################
############### �������� ################
## �������� ��ó�� ##
## ���θ� ##
library(dplyr)
final.doro <- read.csv("��������.csv")

temp <- which(final.doro$���θ�Ī=="")
final.doro.na <- final.doro[-temp,] #NA ����  

#���� ���� ���θ�Ī�� �׷�  
head(final.doro)
group.doro <- final.doro.na %>% group_by(�ñ�����Ī, ���θ�Ī) %>%
  summarise("X��ǥmin"=min(x), "X��ǥmax"=max(x), 
            "Y��ǥmin"=min(y), "Y��ǥmax"=max(y))
head(group.doro)
group.doro <- as.data.frame(group.doro)
write.csv(group.doro, "group.doro.csv", row.names = FALSE)

###############################################
######### ���� ���� ���θ�Ī�� �׷� ###########
group.doro <- read.csv("group.doro.csv")
head(group.doro)



################### ����Ʈ-��ó�� #####################
##���_����Ʈ
apt <- read.csv("���_����Ʈ.csv")
head(apt)
colnames(apt)[3] <- "���θ�Ī"

#����Ʈ ���θ�Ī+������
doro.apt <- merge(x=apt, y=group.doro, by=c("���θ�Ī"))
head(doro.apt)
write.csv(doro.apt, "apt.csv", row.names=FALSE)

##��ǹ�����_����Ʈ
apt.bae <- read.csv("��ǹ�����_����Ʈ.csv")
colnames(apt.bae)[3] <- "���θ�Ī"

#����Ʈ ���θ�Ī+������
doro.apt.bae <- merge(x=apt.bae, y=group.doro, by="���θ�Ī")
head(doro.apt.bae)
write.csv(doro.apt.bae, "apt.bae.csv")

## ��ó��_���س���ڵ尡 ���� �ɷ� ##
#���_����Ʈ
apt <- read.csv("apt.csv")
apt.year <- apt %>% group_by(���θ�Ī) %>% filter(����_���_�ڵ�==max(����_���_�ڵ�))
apt.year <- as.data.frame(apt.year)
write.csv(apt.year, "apt.csv", row.names = FALSE)

#��ǹ�����_����Ʈ
apt.bae <- read.csv("apt.bae.csv")
apt.bae.year <- apt.bae %>% group_by(���θ�Ī) %>% filter(���س��.�ڵ�==max(���س��.�ڵ�))
apt.bae.year <- as.data.frame(apt.bae.year)
write.csv(apt.bae.year, "apt.bae.csv", row.names = FALSE)


#######################################
## ���� ����Ʈ ������ ##
apt <- read.csv("apt.csv")
apt.bae <- read.csv("apt.bae.csv")
head(apt)
head(apt.bae)
apt.bae<-apt.bae[,-1]
########################################


################ �ڵ��������Ȳ ###################
## �����ɺ� ##
gender <- read.csv("�ڵ��������Ȳ_��_���ɺ�.csv")
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
gender2[1:9,3] <- c("10��","20��","30��","40��","50��","60��","70��","80��","90��")
gender2[10:18,3] <- c("10��","20��","30��","40��","50��","60��","70��","80��","90��")

gender3 <- data.frame()
gender3[1:9,1] <- gender.female
gender3[10:18,1] <- gender.male
gender3[1:9,2] <- "Female"
gender3[10:18,2] <- "Male"
gender3[1:9,3] <- c("10��","20��","30��","40��","50��","60��","70��","80��","90��")
gender3[10:18,3] <- c("10��","20��","30��","40��","50��","60��","70��","80��","90��")

ggplot(data=gender3, aes(x="", y=V1, fill=V3)) +
  facet_grid(facets=.~V2) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = brewer.pal(9,"Set3"), guide=guide_legend(title="���ɴ�"))+
  coord_polar(theta="y") +
  ggtitle("���� �ڵ��� �����Ȳ(����)")



################## ���Ằ ######################3#
ryo <- read.csv("�ڵ��������Ȳ_���Ằ.csv")
head(ryo)
ryo.group <- ryo %>% filter(������=="��")
ryo.group <- ryo.group[-c(1:3),-c(2:3)]

ryo.total.a <- data.frame()
ryo.total <- data.frame()
for (i in 1:7){
  ryo.total.a <- ryo.total 
  ryo.group.1 <- ryo.group[,c(1,i+1)]
  ryo.group.1[,3] <- colnames(ryo.group)[i+1]
  colnames(ryo.group.1)[2] <- "�ڵ�����ϴ��" 
  
  ryo.total <- rbind(ryo.total.a, ryo.group.1)
} 
write.csv(ryo.total, "�ڵ��������Ȳ_���Ằ �׷���.csv", row.names=FALSE)
ryo.total <- read.csv("�ڵ��������Ȳ_���Ằ �׷���.csv")
head(ryo.total)

#���� �׷���
ggplot(data=ryo.total, aes(x=�Ⱓ, y=�ڵ�����ϴ��, color=V3, group=V3))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("�Ⱓ�� ���Ằ �ڵ��� ��ϴ��")

#���� ���� �׷���
ryo.total.prop <- ddply(ryo.total, "�Ⱓ", transform,
                        ����=round(�ڵ�����ϴ��/sum(�ڵ�����ϴ��),1))
ggplot(data=ryo.total.prop, aes(x=�Ⱓ, y=����, fill=V3, group=V3), desc=V3)+
  geom_area(colour=NA, alpha=0.5)+
  scale_fill_brewer(palette = "Set3")+
  ggtitle("�Ⱓ�� ���Ằ �ڵ��� ��ϴ��")

#������ �׷���
elec <- ryo.total[which(ryo.total$V3=="����"),]
elec
ggplot(data=elec, aes(x=�Ⱓ, y=�ڵ�����ϴ��))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("���� �ڵ��� ��ϴ��")+
  theme(plot.title=element_text(hjust=0.5, size=15))+
  geom_text(aes(label = �ڵ�����ϴ��),vjust=-0.6, size=6)+
  theme_bw()
ggplot(data=elec, aes(x=�Ⱓ, y=�ڵ�����ϴ��))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("���� �ڵ��� ��ϴ��")+
  theme(plot.title=element_text(hjust=0.5, size=15))+
  geom_text(aes(label = �ڵ�����ϴ��),vjust=-0.6, size=6)+
  theme_bw()

elec1 <- elec[,1:2]
str(elec1)
elec2 <- ts(elec1$�ڵ�����ϴ��, start=2010)
plot(elec2)



#����׷���
mac <- ryo.total[which(ryo.total$�Ⱓ=="2017"),]
ggplot(data=mac, aes(x=reorder(V3,-�ڵ�����ϴ��), y=�ڵ�����ϴ��, fill=V3))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Set3" )+
  geom_text(aes(label = �ڵ�����ϴ��),vjust=-0.6, size=6)+
  xlab("���Ằ �ڵ���")+
  theme_bw()+
  ggtitle("���Ằ �ڵ��� ��ϴ��(2017)")

## ���� ##
gu <- read.csv("�ڵ��������Ȳ_����.csv")
head(gu)
group.gu <- gu %>% group_by(��ġ��) %>%
  summarise("��"=mean(��), "�¿���_�ڰ���"=mean(�¿���_�ڰ���), "�¿���_������"=mean(�¿���_������), 
            "������_�ڰ���"=mean(������_�ڰ���), "������_������"=mean(������_������)
            )
group.gu <- as.data.frame(group.gu)
group.gu <- group.gu[-which(group.gu$��ġ��=="�����"),]
row.names(group.gu) <- 1:25
colnames(group.gu)[1] <- "�ñ�����Ī"


################## 2. ����Ʈ #######################
## ����Ʈ ���� �հ� ##
apt.na <- apt
apt.na[is.na(apt.na)] <- 0
head(apt.na)

group.apt <- apt.na %>% group_by(�ñ�����Ī) %>% 
  summarise("����Ʈ������"=sum(����Ʈ_����_��), 
            "����Ʈ����66�������͹̸������"=sum(����Ʈ_����_66_��������_�̸�_����_��),
            "����Ʈ����66�������ͼ����"=sum(����Ʈ_����_66_��������_����_��),
            "����Ʈ����99�������ͼ����"=sum(����Ʈ_����_99_��������_����_��),
            "����Ʈ����132�������ͼ����"=sum(����Ʈ_����_132_��������_����_��),
            "����Ʈ����165�������ͼ����"=sum(����Ʈ_����_165_��������_����_��),
            "����Ʈ����1��̸������"=sum(����Ʈ_����_1_��_�̸�_����_��),
            "����Ʈ����1�＼���"=sum(����Ʈ_����_1_��_����_��),
            "����Ʈ����2�＼���"=sum(����Ʈ_����_2_��_����_��),
            "����Ʈ����3�＼���"=sum(����Ʈ_����_3_��_����_��),
            "����Ʈ����4�＼���"=sum(����Ʈ_����_4_��_����_��),
            "����Ʈ����5�＼���"=sum(����Ʈ_����_5_��_����_��),
            "����Ʈ����6���̻󼼴��"=sum(����Ʈ_����_6_��_�̻�_����_��),
            "����Ʈ��ո���"=mean(����Ʈ_���_����),
            "����Ʈ��սð�"=mean(����Ʈ_���_�ð�)
            )
group.apt <- as.data.frame(group.apt)
head(group.apt)

apt.bae.na <- apt.bae
apt.bae.na[is.na(apt.bae.na)] <- 0
head(apt.bae.na)

group.apt.bae <- apt.bae.na %>% group_by(�ñ�����Ī) %>% 
  summarise("���ľ���Ʈ������"=sum(����Ʈ.����.��), 
            "���ľ���Ʈ����66�������͹̸������"=sum(����Ʈ.����.66��������.�̸�.����.��),
            "���ľ���Ʈ����66�������ͼ����"=sum(����Ʈ.����.66��������.����.��),
            "���ľ���Ʈ����99�������ͼ����"=sum(����Ʈ.����.99��������.����.��),
            "���ľ���Ʈ����132�������ͼ����"=sum(����Ʈ.����.132��������.����.��),
            "���ľ���Ʈ����165�������ͼ����"=sum(����Ʈ.����.165��������.����.��),
            "���ľ���Ʈ����1��̸������"=sum(����Ʈ.����.1��.�̸�.����.��),
            "���ľ���Ʈ����1�＼���"=sum(����Ʈ.����.1��.����.��),
            "���ľ���Ʈ����2�＼���"=sum(����Ʈ.����.2��.����.��),
            "���ľ���Ʈ����3�＼���"=sum(����Ʈ.����.3��.����.��),
            "���ľ���Ʈ����4�＼���"=sum(����Ʈ.����.4��.����.��),
            "���ľ���Ʈ����5�＼���"=sum(����Ʈ.����.5��.����.��),
            "���ľ���Ʈ����6���̻󼼴��"=sum(����Ʈ.����.6��.�̻�.����.��),
            "���ľ���Ʈ��ո���"=mean(����Ʈ.���.����),
            "���ľ���Ʈ��սð�"=mean(����Ʈ.���.�ð�)
  )
group.apt.bae <- as.data.frame(group.apt.bae)
head(group.apt.bae)

# ������+��� ��ġ��
group.apt.total <- merge(x=group.apt, y=group.apt.bae, by="�ñ�����Ī")
head(group.apt.total)
write.csv(group.apt.total, "group.apt.total.csv", row.names = FALSE)

#���� 
total.apt.gu <- merge(x=group.gu, y=group.apt.total, by="�ñ�����Ī")

## ȸ�ͺм� ������ ##
lm.data <- read.csv("ȸ��_��������������.csv")
lm.data1 <- read.csv("ȸ��_�������.csv")
lm.data2 <- read.csv("ȸ��_�α��е�.csv")
lm.data3 <- read.csv("���α��ʻ�Ȱ���������.csv")

#ȸ��_��������� �����: ���� ��ó��
lm.total <- lm.data %>% filter(��=="�Ұ�")
lm.total.14 <- lm.total[1:300,] %>% group_by(��ġ��) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.14 <- as.data.frame(lm.total.14)
lm.total.14[,2] <- "2014"

lm.total.15 <- lm.total[301:600,] %>% group_by(��ġ��) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.15 <- as.data.frame(lm.total.15)
lm.total.15[,2] <- "2015"

lm.total.16 <- lm.total[601:900,] %>% group_by(��ġ��) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.16 <- as.data.frame(lm.total.16)
lm.total.16[,2] <- "2016"

lm.total.17 <- lm.total[901:1200,] %>% group_by(��ġ��) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
lm.total.17 <- as.data.frame(lm.total.17)
lm.total.17[,2] <- "2017"

lm.total.total <- rbind(lm.total.14, lm.total.15, lm.total.16, lm.total.17)
head(lm.total.total)

lm.total1 <- lm.data1 %>% filter(��=="�Ұ�")
lm.total2 <- lm.data2 %>% filter(��=="�Ұ�")
lm.total3 <- lm.data3 %>% filter(��=="�Ұ�")

lm.tot <- merge(lm.total.total, lm.total1, by=c("��ġ��", "�Ⱓ"))
lm.tot <- merge(lm.tot, lm.total2, by=c("��ġ��", "�Ⱓ"))
lm.tot <- merge(lm.tot, lm.total3, by=c("��ġ��", "�Ⱓ"))

lm.final <- merge(gu[,1:3],lm.tot, by=c("��ġ��", "�Ⱓ"))
head(lm.final)
str(lm.final)
lm.final <- lm.final[,-c(9,12:28)]
write.csv(lm.final, "ȸ�ͺм�.csv", row.names=FALSE)

lmlm <- read.csv("ȸ�ͺм�.csv")
lmlm <- lmlm[,-c(1,2,3)]
head(lmlm)
lm.final <- lm(�ڵ�����ϴ��~.-��ġ��-X10.19��-X20.29��-X70.79��-X80���̻�, data=lmlm)
summary(lm.final)

#���� ���ù�
head(lmlm)
str(lmlm)
lmlm2 <- lmlm[,-c(1,14,15,16,21,22)]
write.csv(lmlm2, "�¿�.csv", row.names=FALSE)
fit2=step(lm(�ڵ�����ϴ��~1, data=lmlm2), 
          scope=formula(lm(�ڵ�����ϴ��~., data=lmlm2)), direction="forward")
formula(fit2)
lm.final2 <- lm(formula(fit2), data=lmlm2)
summary(lm.final2)

train.1 <- lmlm2[1:75,]
test.1 <- lmlm2[76:100,]
lm.train <- lm(formula(fit2), data=train.1)
summary(lm.train)
pred.1 <- predict(lm.train, test.1)
R2(test.1$�ڵ�����ϴ��, pred.1)

library(corrgram)
corrgram(cor(lmlm2[,-c(1)]),type="corr",upper.panel=panel.conf)

library(car)
vif(lm.final2)


## ���� ##
head(lm.data)
head(lm.data1)
head(lm.data2)
head(lm.data3)
#ȸ��_��������������
lm.dong <- lm.data %>% filter(��!="�Ұ�")
dong.14 <- lm.dong[which(substr(lm.dong$�Ⱓ,1,4) == "2014"),] %>% group_by(��,��ġ��) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.14 <- as.data.frame(dong.14)
dong.14[,3] <- "2014"

dong.15 <- lm.dong[which(substr(lm.dong$�Ⱓ,1,4) == "2015"),] %>% group_by(��,��ġ��) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.15 <- as.data.frame(dong.15)
dong.15[,3] <- "2015"

dong.16 <- lm.dong[which(substr(lm.dong$�Ⱓ,1,4) == "2016"),] %>% group_by(��,��ġ��) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.16 <- as.data.frame(dong.16)
dong.16[,3] <- "2016"

dong.17 <- lm.dong[which(substr(lm.dong$�Ⱓ,1,4) == "2017"),] %>% group_by(��,��ġ��) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)
dong.17 <- as.data.frame(dong.17)
dong.17[,3] <- "2017"

dong.total <- rbind(dong.14, dong.15, dong.16, dong.17)
dong.total <- dong.total[which(dong.total$�Ⱓ=="2017"),]
rownames(dong.total) <- 1:nrow(dong.total)

#�������
jucha <- lm.data1 %>% filter(��!="�Ұ�")
jucha <- jucha[which(jucha$�Ⱓ=="2017"),1:5]
rownames(jucha) <- 1:nrow(jucha)
head(jucha)

#�α��е�
dense <- lm.data2 %>% filter(��!="�Ұ�")
dense <- dense[which(dense$�Ⱓ=="2017"),]
rownames(dense) <- 1:nrow(dense)
head(dense)

#������
su <- lm.data3  %>% filter(��!="�Ұ�"&��!="�հ�"&��!="��û")
su <- su[which(su$�Ⱓ=="2017"),]
rownames(su) <- 1:nrow(su)
head(su)
str(su)
colnames(su)[4] <- "�Ѽ�����"

#��ġ��
lm.dong <- merge(dong.total, dense, by=c("��ġ��", "��", "�Ⱓ"))
lm.dong <- merge(lm.dong, su, by=c("��ġ��", "��", "�Ⱓ"))
lm.dong <- merge(lm.dong, jucha, by=c("��ġ��", "��", "�Ⱓ"))
head(lm.dong)
write.csv(lm.dong, "ȸ�ͺм�_����(2017).csv")

## ���� �׽�Ʈ��
lm.dong <- read.csv("test2017.csv")
head(lm.dong)
lm.final <- lm(formula(fit2), data=lmlm2)
summary(lm.final)
pred.2 <- predict(lm.final, lm.dong)
head(pred.2)

#ǥ��ȭ
lm.scale <- scale(lmlm2)
lm.scale <- as.data.frame(lm.scale)
lm.scale.lm <- lm(formula(fit2), data=lm.scale)
summary(lm.scale.lm)
pred.scale <- predict(lm.scale.lm, lm.dong)
head(pred.scale)

#����ȭ
normalize <-function(x) {
  return ((x-min(x)) / (max(x) -min(x)))
}
normal <- normalize(lmlm2)

lm.normal.lm <- lm(formula(fit2), data=normal)
summary(lm.normal.lm)
pred.normal <- predict(lm.normal.lm, lm.dong)
str(lm.dong)
lm.dong[,23] <- pred.normal
colnames(lm.dong)[23] <- "�ڵ�����ϴ��"
write.csv(lm.dong, "�����ڵ�����ϴ��(����).csv", row.names=FALSE)

#��Ȯ�� �׽�Ʈ
normal.train <- normal[1:75,]
lm.normal.train <- lm(formula(fit2), data=normal.train)
pred.normal.test <- predict(lm.normal.train, test.1)
R2(pred.normal.test, test.1$�ڵ�����ϴ��)
## 8/6 ##
#�ڵ�����ϴ���� ������ ��ġ�� ������ ȸ�ͺм� ����
#Ʈ���ΰ� �׽�Ʈ ������ ���̰� Ŀ��->����ȭ ����->�׽�Ʈ ����
#����ȭ�� �����ͷ� ���� ȸ�ͺм�


###################################
## �����ڵ�����ϴ��_����
dong.car <- read.csv("�����ڵ�����ϴ��(����).csv")
head(dong.car)

## gender->��� => ����&���� ����ġ
gender.mean <- apply(gender, 2, mean)
gender.mean <- as.data.frame(gender.mean)
head(gender.mean)


write.csv(apt.total,"apt_total.csv", row.names=FALSE)

apt.dong <- merge(apt.total, final.doro[,c(4,7)], by="���θ�Ī", all=FALSE)
apt.dong.na <- unique(apt.dong)
head(final.doro)

levels(apt.dong$���鵿��Ī)
levels(lm.dong$��)
 

# apt.dong.car <- merge(apt.dong.na,lm.dong, )
levels(final.doro$���鵿��Ī)
levels(apt.total$���θ�Ī)


## 2. ���� ##
random.data <- read.csv("ȸ�ͺм�.csv")
library(caret)

idx <- createDataPartition(random.data$�ڵ�����ϴ��, p=.8, list=F)

data.train <- random.data[idx,]
data.test <- random.data[-idx,]

#��������
control =trainControl(method='cv', search='grid', number = 5)

#����
rf.model <- train(�ڵ�����ϴ��~.-��ġ��, data=data.train, 
                         tuneGrid=data.frame(.mtry=c(4,6,8)),
                         trControl=control,
                         method="rf")
control.rf <- predict(rf.model, data.test)
R2(control.rf, data.test$�ڵ�����ϴ��)
control.rf

##3.xgboost##



### (2).���θ�Ī -> ������ ###
# ���θ�Ī�� ���� �� �ּҰ� ������
head(apt)
head(apt.bae)
colnames(apt.bae) <- colnames(apt)

apt.total <- rbind(apt, apt.bae)
apt.total <- apt.total[,-c(2:3,19:23)] %>% group_by(���θ�Ī) %>% summarise_all(sum)
apt.total <- as.data.frame(apt.total)
head(apt.total)

#apt.merge: ��������+����Ʈ�� �ִ� ���θ�Ī merge
apt.merge <- merge(final.doro[,c(6,9,12:13)], apt.total, by="���θ�Ī")
str(apt.merge)
head(apt.merge)
str(apt.merge)
write.csv(apt.merge, "����Ʈ_���θ�Ī.csv", row.names=FALSE)

#apt.unique: ���θ�Ī�� �������� �Ȱ��� ��
apt.unique <- apt.merge[-which(duplicated(apt.merge$���θ�Ī)),]
head(apt.unique)
write.csv(apt.unique,"apt_unique.csv", row.names = FALSE)


## ������(�����ֹμ���~->�ٽ� ũ�Ѹ�)
bub <- read.csv("������.csv")
head(bub)
bub <- bub[,-1]

bub.na <- na.zero(bub)

bub.na <- bub.na[which(substr(bub.na$������, 1,5)=="����Ư����"),]
bub.na$������ <- as.character(bub.na$������)
bub.na <- bub.na[!duplicated(bub.na$���θ�Ī),] #�ߺ�����
for (i in 1:nrow(bub.na)) {
  bub.na[i,21] <- strsplit(bub.na$������, " ")[[i]][3]
}
bub.na.final <- bub.na
bub.na.final$V21 <- substr(bub.na.final$V21, 1, nchar(bub.na.final$V21)-1)

#�������ִ°�
bub.2 <- read.csv("������2.csv")
bub.2 <- na.zero(bub.2)
bub.2$������ <- as.character(bub.2$������)

for (i in 1:nrow(bub.2)) {
  bub.2[i,21] <- strsplit(bub.2$������, " ")[[i]][3]
}
bub.2.final <- bub.2
bub.2.final$V21 <- substr(bub.2.final$V21, 1, nchar(bub.2.final$V21)-1)
head(bub.2.final)

## ���θ�Ī�� ������ ���� ##
colnames(bub.na.final) <- colnames(bub.2.final)
bub.final <- rbind(bub.2.final,bub.na.final)
head(bub.final)
write.csv(bub.final, "���θ�Ī��_������_����.csv",row.names=FALSE)


## 
head(dong.car)
bub.final <- read.csv("���θ�Ī��_������_����.csv")
head(bub.final)
bub.final <- bub.final[,-c(19:20)]
bub.final[which(bub.final$������=="��������"),]
bub.mean <- bub.final %>% group_by(������) %>% summarise_if(is.integer, mean)
bub.mean <- as.data.frame(bub.mean)

dong.group <- dong.car %>% group_by(��) %>% summarise_all(mean)
dong.group <- as.data.frame(dong.group)
head(dong.group)

## 2. ���� ���� ������
pre.apt.dong <- merge(x=bub.mean, y=dong.group[,c(1,23)], by.x="������", by.y="��")
head(pre.apt.dong)

# (1). ȸ��
pre.dong <- lm(�ڵ�����ϴ��~.-������, data=pre.apt.dong) 
summary(pre.dong)

# (2). ����
library(caret)
pre.rf <- pre.apt.dong
idx <- createDataPartition(pre.rf$�ڵ�����ϴ��, p=.8, list=F)

data.train <- pre.rf[idx,]
data.test <- pre.rf[-idx,]

#��������
control =trainControl(method='cv', search='grid', number = 5)

#����
rf.model <- train(�ڵ�����ϴ��~.-������, data=data.train, 
                         tuneGrid=data.frame(.mtry=c(4,6,8)),
                         trControl=control,
                         method="rf")
control.rf <- predict(rf.model, data.test)
R2(control.rf, data.test$�ڵ�����ϴ��)

control.rf





#######################################################
############## 3. �����ü��� �����α� #################
## �����ü� ##
jip <- read.csv("���_�����ü�.csv")
head(jip)
jip.bae <- read.csv("��ǹ�����_�����ü�.csv")
head(group.doro)
head(jip.bae)


#��ġ�� ���س���ڵ� 201712����
jip.total <- rbind(jip, jip.bae)
jip.year <- jip.total %>% filter(����_���_�ڵ�==201712)
head(jip.year)

write.csv(jip.year,"�����ü�_total.csv", row.names=FALSE)


####################################
## ���� �����ü� ���� ##
jip <- read.csv("�����ü�_total.csv")
####################################


## �����α� ##
##�����α� ��ó��
flow.bae <- read.csv("��ǹ�����_���������α�.csv")
head(flow.bae)
str(flow.bae)

flow.bae.year <- flow.bae %>% filter(����_���_�ڵ�==201712)

## �����α� ���ɴ�+���Ϻ�+�ð� -> ���ɴ� ������
temp <- flow.bae.year
xy=0
for (i in 1:6){
  xy = xy+10
  temp[,(ncol(temp)+1)] <- apply(temp[,which(substr(name,1,8)==paste("�������ɴ�",xy,sep="_"))],1,sum)
  next
}
colnames(temp)
colnames(temp)[516:521] <- c("����_10","����_20","����_30","����_40", "����_50","����_60")

xx=0
for (i in 1:6){
  xx = xx+10
  temp[,(ncol(temp)+1)] <- apply(temp[,which(substr(name,1,8)==paste("�������ɴ�",xx,sep="_"))],1,sum)
  next
}
colnames(temp)[522:527] <- c("����_10","����_20","����_30","����_40", "����_50","����_60")
temp[,"��ǿ���"] <- 0
write.csv(temp,"������������ó��.csv", row.names=FALSE)


#####################################
## ���� �����α� ������ ##
flow <- read.csv("�����α�_total.csv")
flow <- flow[,-1]
head(flow)
str(flow)
flow <- flow[,-16]

## �����α� ����ڵ������ ��ġ�� ##
flow.group <- flow %>% group_by(���_�ڵ�_��) %>% summarise_all(sum)
flow.group <- as.data.frame(flow.group)
head(flow.group)
flow.group <- flow.group[,-c(2:3)]


################################
## �����α� + �������� ��ġ�� ##
#�������� -> ���� ���θ�ĪX -> ���θ�Ī�� x,y
doro.xy.min <- group.doro %>% group_by(���θ�Ī) %>%
  summarise_at(c("X��ǥmin", "Y��ǥmin"), min)
doro.xy.min <- as.data.frame(doro.xy.min)

doro.xy.max <- group.doro %>% group_by(���θ�Ī) %>%
  summarise_at(c("X��ǥmax", "Y��ǥmax"), max)
doro.xy.max <- as.data.frame(doro.xy.max)

#���θ�Ī�� xy
doro.xy <- merge(doro.xy.max, doro.xy.min, by="���θ�Ī")

flow.doro <- merge(flow.group, doro.xy, by.x = "���_�ڵ�_��", by.y="���θ�Ī")
head(flow.doro,20)


################################
## �����ü� ##
# jip.no.mar = ��ǿ��� ���ְ� ��ġ��
jip <- na.zero(jip)
jip.no.mar <- jip[,-24] %>% group_by(���_�ڵ�_��) %>% summarise_all(sum)
jip.no.mar <- as.data.frame(jip.no.mar)
jip.no.mar <- na.zero(jip.no.mar)
jip.no.mar <- jip.no.mar[,-c(2,3)]


## flow.zip = ����ڵ忡 ���� �����α��� �����ü� ��ħ ##
head(flow)
head(flow.doro)
flow.jip <- merge(flow.doro, jip.no.mar, by="���_�ڵ�_��")
flow.jip <- na.zero(flow.jip)
head(flow.jip)
summary(flow.jip)

## car.mean = �ڵ��� ������ ���� ���ؼ� ������ ���� ##
car.mean <- read.csv("�ڵ��������ɺ�����ġ.csv")
car.mean
## flow.jip = ȸ�ͽ� ���� ������ ##
#����: �����α��� ����ġ ��
flow.jip$���� <- 
  (flow.jip$����_10*car.mean$w[1]
   +flow.jip$����_20*car.mean$w[2]
   +flow.jip$����_30*car.mean$w[3]
   +flow.jip$����_40*car.mean$w[4]
   +flow.jip$����_50*car.mean$w[5]
   +flow.jip$����_60*car.mean$w[11]
   +flow.jip$����_10*car.mean$w[6]
   +flow.jip$����_20*car.mean$w[7]
   +flow.jip$����_30*car.mean$w[8]
   +flow.jip$����_40*car.mean$w[9]
   +flow.jip$����_50*car.mean$w[10]
   +flow.jip$����_60*car.mean$w[12])
head(flow.jip)
flow.jip <- flow.jip[,-c(2:13)]
head(flow.jip)


## ȸ�ͺм� ##
str(flow.jip)
summary(flow.jip)

lm.flow.long <- lm(����~.-���_�ڵ�_��-����-����ö_��_��-����_������_��-X��ǥmax-Y��ǥmax-X��ǥmin-Y��ǥmin
                        -��ġ��_��-���۸���_��, data=flow.jip)
summary(lm.flow.long)

flow.jip.lm <- flow.jip
head(flow.jip.lm)
flow.jip.lm$����ȸ������ <- predict(lm.flow.long, flow.jip.lm)
head(flow.jip.lm)
flow.jip.lm[which(flow.jip.lm$���_�ڵ�_��=="������28��"),]

library(plyr)
flow.jip.lm <- arrange(flow.jip.lm, desc(����ȸ������))
head(flow.jip.lm)

#���� ��ġ��
flow.gu <- merge(final.doro[,c(3,7)], flow.jip.lm, by.x="���θ�Ī", by.y="���_�ڵ�_��")
flow.gu <- flow.gu[!duplicated(flow.gu),]
flow.gu.no <- flow.gu[,-c(3:6)]
flow.gu.sum <- flow.gu.no[,-1] %>% group_by(�ñ�����Ī) %>% summarise_all(sum)
flow.gu.sum <- as.data.frame(flow.gu.sum)


#����ȸ�������� ����*100
flow.prop <- flow.gu.sum
flow.prop$��ġ�� <- flow.prop$����ȸ������/sum(flow.prop$����ȸ������)*100
head(flow.prop)
sum(flow.prop$��ġ��)
str(flow.prop)
write.csv(flow.prop,"�����ü�_������ġ��.csv", row.names=FALSE)


## x��ǥ y��ǥ ��ġ�� ##
head(doro.xy)
flow.xy <- merge(flow.jip.lm, doro.xy,by.x = "���_�ڵ�_��", by.y="���θ�Ī")
flow.xy <- arrange(flow.xy, desc(ȸ������))
head(flow.xy)
write.csv(flow.xy,"�����α�+�����ü�_����.csv", row.names=FALSE)

#���θ�Ī�� ���� ���ļ� ���� ���θ�Ī�� ���ġ?
pre.3 <- read.csv("�����α�+�����ü�_����.csv")
head(pre.3)
pre.3.gu <- merge(pre.3, group.doro[,c(1,2)], by.x="���_�ڵ�_��", by.y="���θ�Ī") 
head(pre.3.gu)
pre.3.gu <- pre.3.gu[!duplicated(pre.3.gu), ]
head(pre.3.gu)
str(pre.3.gu)
#���� ���θ��� ȸ������ ���� ������ ����
pre.3.desc <- pre.3.gu %>% group_by(�ñ�����Ī) %>% arrange(�ñ�����Ī,desc(ȸ������))
pre.3.desc <- as.data.frame(pre.3.desc)
head(pre.3.desc)
write.csv(pre.3.desc, "�����ü�_�������θ�����.csv", row.names=FALSE)

head(dong.car)



#################################
########### ���������� ##########
public <- read.csv("����������.csv")
head(public)
str(public)

public$������ּ� = paste("����Ư����",public$�ּ�)
head(public)
head(flow.group)

head(final.doro$�ּ�2)




