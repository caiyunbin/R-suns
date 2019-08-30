###读取数据
library(readstata13)
library(haven)

ind<-read_dta("F:/a series of documents/yuanlaoshixiangguan/suns/individual.dta",encoding='GB2312')



ind <- as.data.frame(ind)

int <- ind[ind$a44==2,]

int1 <- int[,c('h35_4','h20_3','h35_5','h35_6','h35_11','h35_12','h35_18','h35_19')]

int2 <- na.omit(int1)

int2$h35_4[int2$h35_4==1]<-2
int2$h35_4[int2$h35_4==2]<-2
int2$h35_4[int2$h35_4==3]<-1
int2$h35_4[int2$h35_4==4]<-1
int2$h35_4[int2$h35_4==5]<-1
int2$h35_4[int2$h35_4==6]<-1
int2$h35_4[int2$h35_4==7]<-1
int2$h35_4[int2$h35_4==8]<-1

int2$h35_5[int2$h35_5==5]<-2
int2$h35_5[int2$h35_5==1]<-1

defen1 <- int2$h35_4*int2$h35_5

#第二个人得分
int2$h35_11[int2$h35_11==1]<-2
int2$h35_11[int2$h35_11==2]<-2
int2$h35_11[int2$h35_11==3]<-1
int2$h35_11[int2$h35_11==4]<-1
int2$h35_11[int2$h35_11==5]<-1
int2$h35_11[int2$h35_11==6]<-1
int2$h35_11[int2$h35_11==7]<-1
int2$h35_11[int2$h35_11==8]<-1

int2$h35_12[int2$h35_12==5]<-2
int2$h35_12[int2$h35_12==1]<-1

defen2 <- int2$h35_11*int2$h35_12

#得分三
int2$h35_18[int2$h35_18==1]<-2
int2$h35_18[int2$h35_18==2]<-2
int2$h35_18[int2$h35_18==3]<-1
int2$h35_18[int2$h35_18==4]<-1
int2$h35_18[int2$h35_18==5]<-1
int2$h35_18[int2$h35_18==6]<-1
int2$h35_18[int2$h35_18==7]<-1
int2$h35_18[int2$h35_18==8]<-1

int2$h35_19[int2$h35_19==5]<-2
int2$h35_19[int2$h35_19==1]<-1

defen3 <- int2$h35_18*int2$h35_19

defenzong <- defen1+defen2+defen3






lm <- lm(defenzong ~ h20_3, data = int2)
summary(lm)










is.numeric(use)

use <- int$h20_1+int$h20_2+int$h20_3+int$h20_4

cor.test(int$h20_1,int$a44,method="pearson")

lm.sol <- lm(h35_4~g17,data = int)

summary(lm.sol)

int$h35_4