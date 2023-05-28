install.packages("klaR")
install.packages("caret")
install.packages("e1071")
install.packages("classInt")
install.packages("arules")


library(tidyverse)
library(caret)
library(klaR)
library(e1071)
library(arules)
library(rpart)
library(rpart.plot)

## Data Manipulation
churn <- read_csv("BankChurners.csv")
churn <- churn[,- c(1,22,23)]
glimpse(churn)

churn$Attrition_Flag <- factor(churn$Attrition_Flag)
churn$Gender <- factor(churn$Gender)
churn$Education_Level <- factor(churn$Education_Level)
churn$Marital_Status <- factor(churn$Marital_Status)
churn$Income_Category <- factor(churn$Income_Category)
churn$Card_Category <- factor(churn$Card_Category)
glimpse(churn)


## Check for Missing Value
sum(is.na(churn))
mean(complete.cases(churn))

## Baseline Prediction (Randomly guessing not churned) 0.8393404
table(churn$Attrition_Flag)
1 - 1627/(1627+8500)

## Discretizing
dis_churn <- discretizeDF(churn)
glimpse(dis_churn)


###### Training Models
## NB originally discrete
nb <- naiveBayes(Attrition_Flag ~ Gender + Education_Level + Marital_Status + Income_Category + Card_Category,
                 data = churn)
nb
pred <- predict(nb, newdata = churn)
confusionMatrix(table(pred, churn$Attrition_Flag),
                mode = "prec_recall")

## NB all discretizing
nb_dis <- naiveBayes(Attrition_Flag ~ .,
                 data = dis_churn)
nb_dis


pred_dis <- predict(nb_dis, newdata = dis_churn)
confusionMatrix(table(pred_dis, churn$Attrition_Flag),
                mode = "prec_recall")


## NB w/ k-fold + 2 selected discrete values
ctrl <- trainControl(method = "cv",
                     number = 10)

set.seed(4042)
nb_caret <- train(Attrition_Flag ~ Card_Category + Marital_Status,
                  data = churn,
                  method = "nb",
                  trControl = ctrl)
nb_caret

pred_cv <- predict(nb_caret, newdata = churn)
confusionMatrix(table(pred_cv, churn$Attrition_Flag),
                mode = "prec_recall")


## feature selection w/ dt on discretized churn
dt_ctrl <- rpart.control(minsplit = 20,
                         xval = 10)
dt <- rpart(Attrition_Flag ~ .,
            data = dis_churn,
            control = dt_ctrl)
rpart.plot(dt)
printcp(dt)

pred_dt <- predict(dt, newdata = dis_churn, "class")
confusionMatrix(table(pred_dt, dis_churn$Attrition_Flag))


nb_dt <- naiveBayes(Attrition_Flag ~ Total_Trans_Ct + Total_Revolving_Bal + Total_Ct_Chng_Q4_Q1 + Months_Inactive_12_mon + Total_Relationship_Count,
                    data = dis_churn)
pred_nb_dt <- predict(nb_dt, newdata = dis_churn)
confusionMatrix(table(pred_nb_dt, dis_churn$Attrition_Flag),
                mode = "prec_recall")
