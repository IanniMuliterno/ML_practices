library(data.table)
library(tidyverse)
library(party)
library(GGally)

#setwd("your repository")
list.files()
dt <- fread("train.csv")
# looking at submission sample
gender_sub <- fread("gender_submission.csv")
out_data <- fread("test.csv")

## EXPLORATORY
##########################
glimpse(dt)
summary(dt$Fare)
hist(dt$Fare)
summary(dt[Survived == 1]$Fare)
summary(dt[Survived == 0]$Fare)

# some plots
# relationship of Fares with survival
ggplot() +
  geom_histogram(data = dt[Survived == 1],aes(Fare), fill = "blue") +
  geom_histogram(data = dt[Survived == 0],aes(Fare), fill = "red")
# can I ignore Fares or Class because they mean the same thing for the output?
ggplot() +
#  geom_histogram(data = dt[Pclass == 1],aes(Fare), fill = "blue") +
  geom_histogram(data = dt[Pclass == 2],aes(Fare), fill = "red") +
  geom_histogram(data = dt[Pclass == 3],aes(Fare), fill = "green") 

prop.table(table(dt$Pclass))
prop.table(table(dt[Survived == 0]$Pclass))
prop.table(table(dt[Survived == 1]$Pclass))

sapply(dt, function(x) sum(is.na(x)))
summary(dt$Age)
mean(dt[Age < 10]$Survived)
summary(dt[Age < 10]$Fare)

ggpairs(dt[,-c("PassengerId","Name","Ticket","Cabin","tkt_code")]) # search for multicolinearity
##############################################


#sub 2 - finding a way to include cabin and ticket info
dt[,cab_code:=as.character(str_extract(Cabin, "[A-Z]+"))]
dt[,tkt_code:=as.character(str_extract(Ticket, "[A-Z]+"))]
dt[is.na(tkt_code),tkt_code:="other"]
# try to fill age
age_dt <- dt[!is.na(Age)]
age_dt[,cab_flag := ifelse(cab_code %in% c("F","B"),1,0)]

summary(fit_age <- lm(Age~Pclass+SibSp+Parch+cab_flag,data = age_dt))
pred_age <- predict(fit_age,age_dt)
pred_age <- unname(pred_age)

dife <- age_dt$Age - pred_age
hist(dife)
summary(age_dt[dife > 0]$Age)#what happens when pred age doesn't reach real age #looks nice for a start

dt[,cab_flag := ifelse(cab_code %in% c("F","B"),1,0)]
dt[is.na(Age),Age:= unname(predict(fit_age,dt[is.na(Age)]))]
dt[Age <= 0, Age := 0]


treino <- dt[sample(.N,round(.N*.85))]
teste <- anti_join(dt,treino) %>% setDT()

fit <- rpart(as.factor(Survived)~Sex+Fare,treino)
summary(fit)

predito <- predict(fit,teste)
predito <- round(predito[,2])

table(predito,teste$Survived)

predito <- predict(fit,out_data)
predito <- round(predito[,2])

sub_1 <- out_data[,"PassengerId"][,Survived := predito]

fwrite(sub_1,"sub_1.csv")

#modelo <- list()
#av <- numeric()

# for(i in 1:10000){
# treino <- dt[sample(.N,round(.N*.85))]
# teste <- anti_join(dt,treino) %>% setDT()
# 
# modelo[[i]] <-  fit2 <- rpart(as.factor(Survived)~Sex+Fare+tkt_code,treino)
# 
# predito <- predict(fit2,teste)
# predito <- round(predito[,2])
# avaliar <- data.table(predd = predito, real = teste$Survived)
# 
# av[i] <- sum(avaliar$predd != avaliar$real)
# if(i%%500 == 0){ print(i)}
# 
#}

out_data[,tkt_code:=as.character(str_extract(Ticket, "[A-Z]+"))]
out_data[,cab_code:=as.character(str_extract(Cabin, "[A-Z]+"))]
out_data[!(tkt_code %in% dt$tkt_code) & !is.na(tkt_code),tkt_code:="other"]

fit2 <- rpart(as.factor(Survived)~Sex+Fare+tkt_code,dt)
predito <- predict(fit,out_data)
predito <- round(predito[,2])

sub_2 <- out_data[,"PassengerId"][,Survived := predito]
fwrite(sub_2,"sub_2.csv")
##############################################################

# glm

dt[,class_1:= ifelse(Pclass == 1,1,0)]
out_data[,class_1:= ifelse(Pclass == 1,1,0)]
out_data[,cab_code:=as.character(str_extract(Cabin, "[A-Z]+"))]
out_data[,cab_flag := ifelse(cab_code %in% c("F","B"),1,0)]

fit3 <- (glm(Survived ~ Sex+class_1+Age+cab_flag,family = binomial,dt))
summary(fit3)
plot(fit3)

predito <- predict(fit3,dt)
predito <- unname(predito)
predito2 <- ifelse(predito <= 0,0,1)
prop.table(table(predito2,dt$Survived))


predito <- predict(fit3,out_data)
predito <- unname(predito)
sub_3 <- out_data[,"PassengerId"][,Survived := ifelse(predito <= 0,0,1)]


fwrite(sub_3,"sub_3.csv")

#############################################
# subs 2 and 3 didn't make a difference in leaderboard position
# BEING LAZY AND TRYING TO ENSEMBLE THE CURRENT SUBMISSIONS TO TRY TO GET A BETTER SCORE

sub_4 <- merge(sub_1,sub_2,all.x = T, by = "PassengerId")
sub_4 <- merge(sub_4,sub_3,all.x = T, by = "PassengerId")
sub_4[is.na(Survived)]  
sub_4[Survived.x != Survived.y]
sub_4[Survived.x == Survived.y, final:= Survived.x]
sub_4[(Survived.x != Survived.y) & is.na(Survived), final := 0]
sub_4[,aux:=ifelse(!is.na(Survived),Survived.y+Survived.x+Survived,0)]
sub_4[is.na(final) & aux >= 2, final := 1]
sub_4[is.na(final), final:= 0]
sub_4 <- sub_4[,c("PassengerId","final")]  
sub_4[,Survived:= final]
sub_4[,final:= NULL]

fwrite(sub_4,"sub_4.csv") # worked; score went from 0.76555 to 0.77511 and I advanced 2397 places on the leaderboard
  # repeat this with R
#https://www.kaggle.com/gunesevitan/advanced-feature-engineering-tutorial-with-titanic

