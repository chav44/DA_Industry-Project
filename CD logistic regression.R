#read Dataset
data <- read.csv(file.choose(),stringsAsFactors=F)

#create training and validation data from given data
install.packages('caTools')
library(caTools)

set.seed(10)
split <- sample.split(data$ConversionRate, SplitRatio = 0.75)

#get training and test data
cdtrain <- subset(data, split == TRUE)
cdtest <- subset(data, split == FALSE)

#Distribution
table(cdtrain$ConversionRate)

#logistic regression model
model <- glm(ConversionRate ~ ., data = cdtrain, family = binomial(link = "logit"))
summary(model)

#predict function gives the logit
predict(model, newdata = cdtrain, type = 'response')

pred = predict(model, newdata = cdtrain, type = 'response')
p.conversion = round(p)

#install.packages("e1071")
#require(e1071)
#install.packages("caret")
#Confusion Matrix
require(caret)
#table(cdtrain$ConversionRate, predict >0.5)
confusionMatrix(p.conversion,ConversionRate)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, cdtrain$ConversionRate)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#Plot glm 
library(ggplot2)
ggplot(cdtrain,aes(x=Source, y=ConversionRate)) + geom_point()+ stat_smooth(method="glm", family="binomial",se=FALSE)
