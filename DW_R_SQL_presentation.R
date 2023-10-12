library(tidyverse)
data=read.table("G:/My Drive/Data Wrangling with Databases and R/DW_practice/marketing_campaign.csv", sep = "\t", header = T)
df=data.frame(data)
#Checking for missing values
missing.values <- df |> 
  gather(key = "key", value = "val") |> 
  mutate(isna = is.na(val)) |> 
  group_by(key) |> 
  mutate(total = n()) |> 
  group_by(key, total, isna) |> 
  summarise(num.isna = n())

# take out NA value
df1=na.omit(df)

#customer age
df1['Age']= 2023-df1$Year_Birth

#No. of children
df1['Child']=df1$Kidhome+df1$Teenhome
df1['total_spent']=df1$MntMeatProducts+df1$MntFishProducts+df1$MntWines+df1$MntFruits+df1$MntSweetProducts+df1$MntGoldProds
df1['accepted']=df1$AcceptedCmp1+df1$AcceptedCmp2+df1$AcceptedCmp3+df1$AcceptedCmp4+df1$AcceptedCmp5

# get rid of outlier
df1 <- df1[!(df1$ID == '9432'), ]
str(df1)

# total_spent vs income
ggplot(df1, aes(x = Income, y = total_spent)) + 
  geom_point() + 
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE)
cor(df1$Income, df1$total_spent)

# total_spent vs children
# calculating avg spent
children <- df1 %>%
  group_by(Child) %>%
  summarise(avg_spent = mean(total_spent))


ggplot(children, aes(x = Child, y = avg_spent)) + 
  geom_bar(stat = "identity") + 
  theme_minimal()

# correlation  b/t #of child and spent 
cor(children$Child, children$avg_spent)

# number of  campaign participated vs money spent
campaign <- df1 %>%
  group_by(accepted) %>%
  summarise(spent = mean(total_spent))
ggplot(campaign, aes(x = accepted, y = spent)) + 
  geom_bar(stat = "identity")
cor(campaign$accepted,campaign$spent)
