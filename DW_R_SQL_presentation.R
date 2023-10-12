library(tidyverse)
data=read.table("/Users/jojo/Downloads/marketing_campaign.csv", sep = "\t", header = T)
df=data.frame(data)
#Checking for missing values
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)
df1=na.omit(df)
#customer age
df1['Age']= 2023-df1$Year_Birth
#No. of children
df1['Child']=df1$Kidhome+df1$Teenhome
df1['total_spent']=df1$MntMeatProducts+df1$MntFishProducts+df1$MntWines+df1$MntFruits+df1$MntSweetProducts+df1$MntGoldProds
df1['accepted']=df1$AcceptedCmp1+df1$AcceptedCmp2+df1$AcceptedCmp3+df1$AcceptedCmp4+df1$AcceptedCmp5
