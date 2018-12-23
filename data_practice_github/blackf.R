library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)


setwd("C:\\Users\\Idianni\\Desktop\\Desktop\\data_practice_github\\")

dt <- fread("train.csv")

glimpse(dt)

sapply(dt, function(x) sum(is.na(x)))
sapply(dt, function(x) sum(is.null(x)))
sapply(dt, function(x) sum(x == "NULL"))
sapply(dt, function(x) sum(x == "NA"))

p1 <- ggplot(dt, aes(x=Gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(dt, aes(x=Age)) + ggtitle("Age") + xlab("Age") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(dt, aes(x=City_Category)) + ggtitle("City_Category") + xlab("City_Category") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(dt, aes(x=Stay_In_Current_City_Years)) + ggtitle("stay") + xlab("stay") +
grid.arrange(p1, p2, p3, p4, ncol=2)


n_distinct(dt$Product_ID)
n_distinct(dt$User_ID)
n_distinct(dt$Product_Category_1)
n_distinct(dt$Product_Category_2)
n_distinct(dt$Product_Category_3)
table(dt$Product_Category_1)
table(dt$Product_Category_2)
table(dt$Product_Category_3)
hist(dt$Purchase)

dt[,Stay_In_Current_City_Years := as.numeric(gsub("([0-9]+).*$", "\\1", dt$Stay_In_Current_City_Years))]
dt[,Occupation := as.factor(Occupation)]
dt[,Product_Category_1 := as.factor(Product_Category_1)]
dt[,Product_Category_2 := as.factor(Product_Category_2)]
dt[,Product_Category_3 := as.factor(Product_Category_3)]
dt[,Marital_Status := as.factor(Marital_Status)]

dt <- dt[,2:12]
intrain<- createDataPartition(dt$Purchase,p=0.7,list=FALSE)
set.seed(2018)
train<- dt[intrain,]
test<- dt[-intrain,]
LModel <- lm(Purchase ~ .,data=train)
#print(summary(LModel))
anova(LModel, test="Chisq")

#cheking out accuracy


fitted.results <- predict(LModel,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))



# loading test data
test <- fread("test.csv")
sapply(test, function(x) sum(is.na(x)))
glimpse(test)

test <- test[,2:11]
test[,Stay_In_Current_City_Years := as.numeric(gsub("([0-9]+).*$", "\\1", Stay_In_Current_City_Years))]
test[,Occupation := as.factor(Occupation)]
test[,Product_Category_1 := as.factor(Product_Category_1)]
test[,Product_Category_2 := as.factor(Product_Category_2)]
test[,Product_Category_3 := as.factor(Product_Category_3)]
test[,Marital_Status := as.factor(Marital_Status)]