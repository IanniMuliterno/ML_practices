library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(Matrix)
library(xgboost)


setwd("..Desktop\\data_practice_github\\")

dt <- fread("train.csv")

glimpse(dt)
str(dt)

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
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
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
dt[,Product_ID:= as.factor(Product_ID)]
#add feature, how many purchases that person made that month
purchase_peruser <- dt[,.(count = .N),by = User_ID]

dt <- left_join(dt,purchase_peruser, by = "User_ID")



dt <- dt[,2:13]
set.seed(218)
intrain<- sample(1:dim(dt)[1],size = 0.75*dim(dt)[1])
train<- dt[intrain,]
test<- dt[-intrain,]
LModel <- lm(Purchase ~ .,data=train)
#print(summary(LModel))
anova(LModel, test="Chisq")
n_distinct(train$Product_ID)
n_distinct(test$Product_ID)
#cheking out accuracy
levels(test$Product_ID) <- levels(train$Product_ID)
sum(levels(test$Product_ID) == levels(train$Product_ID))



# 
# fitted.results <- predict(LModel,test,type='response')
# misClasificError <- mean(fitted.results != test$Purchase)
# print(paste('Regression Accuracy',1-misClasificError))
str(train)


sparse_matrix <- sparse.model.matrix(Purchase ~ Product_ID + Gender + Age + 
                                    Occupation + City_Category + Stay_In_Current_City_Years +
                                    Marital_Status + Product_Category_1 + Product_Category_2 +
                                      Product_Category_3 + count, 
                                     data = train)

data_matrix <- xgb.DMatrix(data = as.matrix(sparse_matrix), label = train$Purchase)

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

