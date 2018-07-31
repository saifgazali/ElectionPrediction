polling = read.csv("pollingData.csv")
str(polling)

table(polling$Year)

library("mice")

simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
str(simple)
simple$Year

set.seed(144)

#removing null spaces
imputed = complete(mice(simple))

summary(imputed)
polling$SurveyUSA = imputed$SurveyUSA
polling$Rasmussen = imputed$Rasmussen

summary(polling)

Train = subset(polling,Year == 2004 | Year == 2008)
Test = subset(polling,Year == 2012)

#checking the accuracy of baseline model
table(Train$Republican)
#hence 53% accuracy

table(sign(Train$Rasmussen))

table(Train$Republican,sign(Train$Rasmussen))

cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

mod1 = glm(Republican ~ PropR , data=Train, family = "binomial")
summary(mod1)

#making predictions

pred1 = predict(mod1,type="response")
table(Train$Republican,pred1>=0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount,data=Train,family = "binomial")
summary(mod2)

pred2 = predict(mod2,type="response")
table(Train$Republican,pred2>=0.5)

#evaluating the model
table(Test$Republican,sign(Test$Rasmussen))

TestPrediction = predict(mod1, newdata = Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)

subset(Test,TestPrediction >=0.5 & Republican == 0)