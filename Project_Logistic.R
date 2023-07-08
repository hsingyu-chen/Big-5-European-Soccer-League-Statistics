#Read in the data file and set it to a data frame called soccer and check the head of soccer
soccer <- read.csv('Big_5_European_Leagues_Stats.csv',stringsAsFactors = T)
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

#Make Pts categorical
pt <- function(x){
  x <- as.numeric(x)
  if(x <= 50){
    return('<=50')
  }
  else{
    return('>50')
  }
}

soccer$Pts <- sapply(soccer$Pts, pt)
table(soccer$Pts)
soccer$Pts <- as.factor(soccer$Pts)

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

#Split the dataset into train and test
library(caTools)
set.seed(101)
sample <- sample.split(soccer$Pts, SplitRatio = 0.7)
train <- subset(soccer, sample = TRUE)
test <- subset(soccer, sample = FALSE)

#Using Logistic Regression to train the model by points
model <- glm(Pts~ + GD, binomial(logit), train)
summary(model)

fitted.prob <- predict(model, test, type = 'response')
fitted.result <- ifelse(fitted.prob > 0.5, 1, 0)
table(test$Pts, fitted.prob > 0.5)

