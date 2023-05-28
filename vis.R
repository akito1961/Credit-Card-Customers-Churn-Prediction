install.packages("ggplot2")

library(ggplot2)
library(tidyverse)

## Data Manipulation
churn <- read_csv("BankChurners.csv")
churn <- churn[,- c(1,22,23)]
glimpse(churn)

churn$Gender <- factor(churn$Gender)
churn$Dependent_count <- factor(churn$Dependent_count)
churn$Education_Level <- factor(churn$Education_Level)
churn$Education_Level <- factor(churn$Education_Level)
churn$Marital_Status <- factor(churn$Marital_Status)
churn$Income_Category <- factor(churn$Income_Category)
churn$Card_Category <- factor(churn$Card_Category)
glimpse(churn)

ggplot(data = churn, aes(y = Customer_Age)) +
  geom_histogram(aes(fill = Attrition_Flag))

## Churn Rate
table(churn$Attrition_Flag)

## define churn = attrited customer


###### Attrition_Flag vs cont variables plot
ggplot(data = churn, aes(y = Attrition_Flag, x = Total_Revolving_Bal)) +
  geom_boxplot(aes(fill = Attrition_Flag))

ggplot(data = churn, aes(y = Attrition_Flag, x = Avg_Open_To_Buy)) +
  geom_boxplot(aes(fill = Attrition_Flag))

ggplot(data = churn, aes(y = Attrition_Flag, x = Total_Amt_Chng_Q4_Q1)) +
  geom_boxplot(aes(fill = Attrition_Flag))

##
ggplot(data = churn, aes(y = Attrition_Flag, x = Total_Trans_Amt)) +
  geom_boxplot(aes(fill = Attrition_Flag))
##

##
ggplot(data = churn, aes(y = Attrition_Flag, x = Total_Trans_Ct)) +
  geom_boxplot(aes(fill = Attrition_Flag))
##

ggplot(data = churn, aes(y = Attrition_Flag, x = Total_Ct_Chng_Q4_Q1 )) +
  geom_boxplot(aes(fill = Attrition_Flag))

ggplot(data = churn, aes(y = Attrition_Flag, x = Avg_Utilization_Ratio )) +
  geom_boxplot(aes(fill = Attrition_Flag))

###### Attrition_Flag vs discrete variables plot

ggplot(churn, aes(x = Income_Category)) +
  geom_bar(aes(fill = Attrition_Flag), position = "dodge")

ggplot(churn, aes(x = Card_Category)) +
  geom_bar(aes(fill = Attrition_Flag), position = "dodge")

ggplot(churn, aes(x = Dependent_count)) +
  geom_bar(aes(fill = Attrition_Flag), position = "dodge")

###### Cont distribution
ggplot(churn, aes(x = Dependent_count)) +
  geom_histogram(aes(fill = Attrition_Flag), position = "dodge", stat = "count")

ggplot(churn, aes(x = Total_Trans_Amt)) +
  geom_histogram(aes(fill = Attrition_Flag), position = "dodge", stat = "count")
