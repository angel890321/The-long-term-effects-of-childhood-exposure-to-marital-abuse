#getwd()
#setwd("C:/Users/pc/Desktop/資料視覺化")
library(tidyverse)
library(readxl)
library(dplyr)
library(car)
trauma <- read.csv("C:/Users/映均/視覺化分析/trauma.csv")
cata <- subset(trauma, select = c(sex,marriage, family,income))  # 只要婚姻和家庭
ptsd <- subset(trauma, select=c(ptsd1, ptsd2,ptsd3,ptsd4,ptsd5,ptsd6,ptsd7,ptsd8,ptsd9,
                                ptsd10, ptsd11, ptsd12, ptsd13, ptsd14, ptsd15, ptsd16,
                                ptsd17, ptsd18, ptsd19, ptsd20, ptsd21))
#合併檔案
kpm = cbind(cata,ptsd)
View(kpm)
#類別轉factor
kpm1 <- within(kpm, {
  sex  <- factor(sex,labels = c("男","女"))
  marriage <- factor(marriage,labels = c("維持婚姻並同住", "維持婚姻但分居","再婚"))
  family <- factor(family,labels = c("三代同堂", "核心家庭","大家庭","繼親家庭","隔代教養"))
})
kpm2 <- within(tot,{ 
  marriage <- factor(marriage,labels = c("維持婚姻並同住", "維持婚姻但分居","離婚","寡居","再婚","其他"))
  family <- factor(family,labels = c("三代同堂", "核心家庭","大家庭","單親家庭","繼親家庭","隔代教養"))
  })
p1 <- ggplot(kpm, aes(x = sex,fill = sex)) 
p1 +geom_bar(aes(y = ..prop..,group=1))+
  labs(x="男女性別比例")

p2 <- ggplot(kpm, aes(x = family,fill = family))
p2 + geom_bar(aes(y = ..prop..,group=1))+
  labs(x="家庭比例")

p3 <- ggplot(kpm, aes(x = marriage,fill = marriage))
p3 + geom_bar(aes(y = ..prop..,group=1))+
  labs(x="婚姻狀況比例")

p4 <- ggplot(kpm, aes(x = income,fill = income))
p4 + geom_bar(aes(y = ..prop..,group=1))+
  labs(x="收入狀況比例")
