#--------------------------------------
#    > 4 <  【 C5.0算法 】
#    using 'C50' package
#--------------------------------------
setwd("C:/Users/GameR/Desktop/data_mining")
if( ! require('C50'))
  install.packages('C50')
if( ! require('readr'))
  install.packages('readr')
if( ! require('tree'))
  install.packages('tree')
if( ! require('modeldata'))
  install.packages('modeldata')
library(C50)
library(tree)
library(readr)
df <- read_csv(file = "RAW_adults.csv")
set.seed(5457)
vars <- c('age', 'workclass', 'fnlwgt', 'education','education-num', 'marital-status', 'occupation', 'relationship', 'race', 'sex', 'capital-gain', 
          'capital-loss', 'hours-per-week', 'native-country')
#print(str(df[, c(vars, "salary")]))
capture.output(str(df[c(vars, "salary")]))
str(df$salary)
df$salary<-as.factor(df$salary)
df$workclass<-as.factor(df$workclass)
df$age<-as.factor(df$age)
df$`marital-status`<-as.factor(df$`marital-status`)
df$occupation<-as.factor(df$occupation)
df$relationship<-as.factor(df$relationship)
df$education<-as.factor(df$education)
df$race<-as.factor(df$race)
df$sex<-as.factor(df$sex)
df$`native-country`<-as.factor(df$`native-country`)

spl = sample.split(df$salary, SplitRatio = 0.7)
train_data = subset(df, spl==TRUE)
test_data = subset(df, spl==FALSE)
#train_data <- df[-in_train,]
#test_data  <- df[in_train,]
tree_mod <- C5.0(train_data[vars], train_data$salary)

print(tree_mod)
print(summary(tree_mod))
#plot(tree_mod)

#rule_mod <- C5.0(train_data[vars], train_data$salary, rules = TRUE)
#print(rule_mod)
#print(summary(rule_mod))

credit_pred <- predict(tree_mod, newdata = test_data[, vars], type = "class")

sum( credit_pred == test_data$salary ) / length( credit_pred )

#table(predict(tree_mod, test_data[, vars]))

#print(df)
#print(train_data)
#print(test_data)
