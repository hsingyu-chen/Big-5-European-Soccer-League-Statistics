#Hsing Yu Chen
#Yu-Tien Shih
#Math 151 Project

#Read in the data file and set it to a data frame called soccer and check the head of soccer
soccer <- read.csv('/Users/soniachen/Library/Mobile Documents/com~apple~CloudDocs/Documents/Math 151/Big_5_European_Leagues_Stats.csv',stringsAsFactors = T)
head(soccer)
str(soccer)
summary(soccer)

#Remove the variables that we will not use
library(dplyr)
soccer <- soccer %>%
  select(-c(Rk, xG, xGA, xGD, xGD.90 , Top.Team.Scorer, Goalkeeper, Attendance))

#Calculate the win rate for each team
soccer$Win.rate <- soccer$W / soccer$MP
summary(soccer$Win.rate)

#Check if there is any missing data
library(Amelia)
missmap(soccer, col = c('yellow', 'black'), y.at = c(1), y.labels = c(''))

#Visualization of the dataset
library(ggplot2)
#Compare win rate and goal difference by country by using scatter plot
ggplot(soccer, aes(Win.rate, GD, color = Country)) + geom_point() + labs(x = 'Win Rate', y = 'Goal Difference', title = 'Win Rate vs. Goal Difference by Country') + theme_bw()
#The overall win rate of each country
ggplot(soccer, aes(Country, Win.rate, fill = Country)) + geom_bar(stat = 'identity') + labs(x = 'Country', y = 'Win Rate') + theme(legend.position = "none")
#Win rate between each country by using boxplot
ggplot(soccer, aes(Country, Win.rate, fill = Country)) + geom_boxplot() + labs(x = 'Country', y = 'Win Rate') + theme(legend.position = "none")
#Goal difference between each country by using boxplot
ggplot(soccer, aes(Country, GD, fill = Country)) + geom_boxplot() + labs(x = 'Country', y = 'Goal Difference') + theme(legend.position = "none")

#Remove wins from dataset for model training
soccer <- soccer %>%
  select(-c(W))

#Split the dataset into train and test
library(caTools)
set.seed(101)
sample <- sample.split(soccer$Pts, SplitRatio = 0.7)
train <- subset(soccer, sample = TRUE)
test <- subset(soccer, sample = FALSE)

#Using Linear Regression to train the model by points
model <- lm(Pts~ + GD,train)
summary(model)

#Predict the values
predicted.value <- predict(model, test)

#Calculate the mean squared error and R squared
mse <- mean((test$Pts - predicted.value) ^ 2)
mse

rsq <- summary(model)$r.squared
rsq

#Build a decision tree
library(rpart)
tree <- rpart(Pts~ + D + L + GD, method = 'class', soccer)
printcp(tree)

#Plot a decision tree
library(rpart.plot)
prp(tree)
