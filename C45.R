#    > 3 <  【 C4.5算法 】
#    using 'RWeka' package
#--------------------------------------
setwd("C:/Users/GameR/Desktop/data_mining")

if( ! require('RWeka'))
  install.packages('RWeka')
if( ! require('caret'))
  install.packages('caret')
if( ! require('lattice'))
  install.packages('lattice')

library(RWeka)
library(lattice)
library(caret)
library(readr)
df <- read_csv(file = "RAW_adults.csv")
set.seed(2000)
vars <- c('age', 'workclass', 'fnlwgt', 'education', 'education-num','marital-status', 'occupation', 'relationship', 'race', 'sex', 'capital-gain', 
          'capital-loss', 'hours-per-week', 'native-country')
#print(str(df[, c(vars, "salary")]))
capture.output(str(df[c(vars, "salary")]))

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

dataTrain = subset(df, spl==TRUE)
dataTest = subset(df, spl==FALSE)
resultJ48 <- J48(salary  ~ ., data=dataTrain, control = Weka_control(U=TRUE,M=60))
#J48( Target ~ Attr1 + Attr2 , data= input, control= Weka_control(M=1,U=TRUE))
#規則輸出
resultJ48
print(summary(resultJ48))
#繪製決策樹圖形
#plot(resultJ48)
#plot( model.C45,type="simple")
#WOW(J48)
eval_j48 <- evaluate_Weka_classifier(resultJ48, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
eval_j48
#利用預測集進行預測
pred.C45 <- predict(resultJ48, newdata=dataTest, type="class")   #利用預測集進行預測
print(length( pred.C45 ))
sum( pred.C45 == dataTest$salary ) / length( pred.C45 )
length(pred.C45)

