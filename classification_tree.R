library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)


churn <- read_csv("BankChurners.csv")
glimpse(churn)

churn <- churn[,- c(1,22,23)]
churn
glimpse(churn)

## classification tree
ctrl <- rpart.control(minsplit = 500,
                      xval = 10)

tree <- rpart(Attrition_Flag ~ .,
              data = churn,
              method = "class",
              control = ctrl)
printcp(tree)
rpart.plot(tree)


df_ctrl <- trainControl(method = "cv",
                        number = 50)
df <- train(Attrition_Flag ~ .,
            data = churn,
            method = "rf",
            control = df_ctrl)
df
