##BEST ONE YET 

library(ggplot2)
library(ROCR)
library(popbio)
library(visreg)

airlineReportAll <- read.csv("test.csv")


sum(grepl("neutral or dissatisfied", airlineReportAll$satisfaction))
#  14573 of 25976 are "neutral or dissatisfied"

mean(airlineReportSample)
set.seed(11)

airlineReportSample <- airlineReportAll[sample(nrow(airlineReportAll), 500), ]

airlineData = airlineReportSample[, c(3,4,6,7,8,23,25)]

# Plot the data 
qplot(Flight.Distance, satisfaction, data = airlineData) + geom_point(colour = "#3366FF", size = 3)

#Plot the conditional density plot 
#Computes and plots conditional densities describing how the conditional distribuion of a categorial variable "satisfaction" changes over a numerical variable 'Age'
cdplot(factor(satisfaction) ~ Flight.Distance, data=airlineData, main="Estimated categ prob", ylab='Satisfaction')

 distplot <- ggplot(airlineData, aes(x=Flight.Distance, fill=factor(satisfaction))) +
  geom_density(position="fill") +
  ylab('Probability') +
  theme(legend.position='bottom')
 
#Create a line char comparing Flight.Distance and Satisfaction 

 
 
airlineData$satisfaction <- ifelse(airlineData$satisfaction %in% c("neutral or dissatisfied"), 0, 1)
airlineData$Customer.Type <- ifelse(airlineData$Customer.Type %in% c("disloyal Customer"), 0, 1)
airlineData$Gender <- ifelse(airlineData$Gender %in% c("Male"), 0, 1)
airlineData$Type.of.Travel <- ifelse(airlineData$Type.of.Travel %in% c("Business travel"), 0, 1)
airlineData$Class <- ifelse(airlineData$Class == "Business", 1,
                            ifelse(airlineData$Class == "Eco", 2,
                                   ifelse(airlineData$Class == "Eco Plus", 3, NA)))
#1 for business 2 for eco 3 for eco plus 


# Create the histogram based on Frequency of Satisfied by Distance 
ggplot(data = airlineData, aes(x = Flight.Distance, fill = factor(satisfaction))) +
  geom_histogram(binwidth = 100, position = "dodge") +
  labs(x = "Flight Distance", y = "Frequency", title = "Frequency of Satisfied by Distance")
#Here I found that there is a discrepancy in the total amount of times satisfied and neutral or dissatisifed are reported
#Find the mean satisfaction for a distance 

# Create a new column with the groups of 100 miles of Flight Distance
airlineData$Distance.Group <- cut(airlineData$Flight.Distance, breaks = seq(0, 5500, by = 100))

# Calculate the mean of Satisfaction for each group
mean_satisfaction <- aggregate(satisfaction ~ Distance.Group, data = airlineData, mean)

# Print the result
mean_satisfaction

# Create the scatter plot
ggplot(data = mean_satisfaction, aes(x = Distance.Group, y = satisfaction)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(x = "Flight Distance", y = "Satisfaction", title = "Satisfaction vs Distance")
#Overall there seems to be a general trend in regards to the mean satisfaction based on distance 


#Further analysis looking at how type of travel 
personalTravel <- subset(airlineData, airlineData$Type.of.Travel == 1)

#Find the mean satisfaction for a distance 
# Create a new column with the groups of 100 miles of Flight Distance
personalTravel$Distance.Group <- cut(personalTravel$Flight.Distance, breaks = seq(0, 5500, by = 100))

# Calculate the mean of Satisfaction for each group
mean_satisfactionPT <- aggregate(satisfaction ~ Distance.Group, data = personalTravel, mean)

# Print the result
mean_satisfactionPT

# Create the scatter plot
ggplot(data = mean_satisfactionPT, aes(x = Distance.Group, y = satisfaction)) +
  geom_point() +
  labs(x = "Flight Distance", y = "Satisfaction", title = "Satisfaction vs Distance")
#When we consider adding type of travel to the equation there seems to be an overall lack of satisfaction when traveling personally 

#Further analysis looking at how type of travel 
businessTravel <- subset(airlineData, airlineData$Type.of.Travel == 0)

#Find the mean satisfaction for a distance 
# Create a new column with the groups of 100 miles of Flight Distance
businessTravel$Distance.Group <- cut(businessTravel$Flight.Distance, breaks = seq(0, 5500, by = 100))

# Calculate the mean of Satisfaction for each group
mean_satisfactionBT <- aggregate(satisfaction ~ Distance.Group, data = businessTravel, mean)

# Print the result
mean_satisfactionBT

# Create the scatter plot
ggplot(data = mean_satisfactionBT, aes(x = Distance.Group, y = satisfaction)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Flight Distance", y = "Satisfaction", title = "Satisfaction vs Distance")
#Business travel proved to show more satisfaction but no solid trend. Generally when travelling longer you are more satisfied 



#Why?
#Maybe seat comfort or snacks?

#Further analysis looking at how seat comfort 
# Create the scatter plot
ggplot(data = airlineReportSample, aes(x = Seat.comfort, y = satisfaction)) +
  geom_point() +
  labs(x = "Seat Comfort", y = "Satisfaction", title = "Satisfaction vs Seat Comfort")

# Convert Seat.comfort to a factor with ordered levels
airlineReportSample$Seat.comfort <- factor(airlineReportSample$Seat.comfort, levels = c(1, 2, 3, 4, 5), ordered = TRUE)

airlineReportSample$satisfaction <- ifelse(airlineReportSample$satisfaction %in% c("neutral or dissatisfied"), 0, 1)

# Calculate mean satisfaction for each level of Seat.comfort
meanSeat <- airlineReportSample %>% 
  group_by(Seat.comfort) %>% 
  summarise(mean_satisfactionSEAT = mean(satisfaction))

# View the resulting data frame
meanSeat

# Create scatter plot
ggplot(meanSeat, aes(Seat.comfort, mean_satisfactionSEAT )) + 
  geom_point() + 
  labs(x = "Seat Comfort", y = "Satisfaction")


#Now with food 
#Further analysis looking at how food and drink impact satisfaction
# Create the scatter plot
ggplot(data = airlineReportSample, aes(x = Food.and.drink, y = satisfaction)) +
  geom_point() +
  labs(x = "Food and Drink", y = "Satisfaction", title = "Satisfaction vs Food and Drink")

# Convert Food.and.Drink to a factor with ordered levels
airlineReportSample$Food.and.drink <- factor(airlineReportSample$Food.and.drink, levels = c(1, 2, 3, 4, 5), ordered = TRUE)


# Calculate mean satisfaction for each level of Seat.comfort
meanFAD <- airlineReportSample %>% 
  group_by(Food.and.drink) %>% 
  summarise(mean_satisfactionFAD = mean(satisfaction))

# View the resulting data frame
meanFAD <- meanFAD[-6, ]

# Create scatter plot
ggplot(meanFAD, aes(Food.and.drink, mean_satisfactionFAD)) + 
  geom_point() + 
  labs(x = "Food and Drink", y = "Satisfaction")

#DF with greater than or equal to 4 seat comfort 
goe4seat <- airlineReportSample[airlineReportSample$Seat.comfort >= 4, ]

# View the new data frame
goe4seat

mean(goe4seat$satisfaction)
mean(airlineReportSample$satisfaction)

test <- goe4seat[goe4seat$Food.and.drink >= 4, ]

mean(test$satisfaction)
#Furthermore if you do FAD




head(airlineData)
sum(airlineData$satisfaction)
#201 customers of 500 are satisfied 
mean(airlineData$satisfaction)
#satisfaction rate is .402 40.2 %

#Preform Logistical Regression 

satLogreg = glm(satisfaction ~ ., data = airlineData, family=binomial(link="logit"))
summary(satLogreg)

#Generate confidence intervals for regression coefficients 
confint(satLogreg)

#Split the data as Training and Test sets 
#Building model on trained data and use that model to predict the test data. We will evaluate how the model we build proforms on the test data

splitSat = caret::createDataPartition(airlineData[,7], p = 0.8, list=F, times=1)
head(splitSat)

trainSat = airlineData[splitSat,]
head(trainSat)

testSat = airlineData[!row.names(airlineData) %in% row.names(trainSat),]
head(testSat)

#Apply Logistic Regression on Training Set
trainSatLR = glm(satisfaction ~ ., data=trainSat, family=binomial(link="logit"))

summary(trainSatLR)

#Predict on Test data set 
testSat$Predicted = round(predict(trainSatLR, testSat[,c(1,2,3,4,5,6)], type="response"), 2)
head(testSat)

#Visulization of Logic Regression results 

#As age increases the probability of satisfaction will increase
p1 <- plot(trainSat$Flight.Distance, trainSat$satisfaction, xlab="Flight.Distance", ylab="P(satisfaction)")
trainLR = glm(satisfaction ~ Flight.Distance, data=trainSat, family=binomial(link="logit"))
curve(predict(trainLR,data.frame(Flight.Distance=x),type="resp"),add=TRUE)
points(trainSat$Flight.Distance,fitted(trainLR),pch=20)


popbio::logi.hist.plot(trainSat$Flight.Distance, trainSat$satisfaction, boxp=FALSE,type="hist",col="gray")

logReg = glm(satisfaction ~ ., data=trainSat, family=binomial(link="logit"))
visreg::visreg(logReg, "Flight.Distance", scale="response", partial=FALSE, xlab="Flight.Distance", ylab="P(satisfaction)", rug=2)

#Model evaluation - Reciever Operating Characteristics (ROC) Curve 
#Create a prediction object
#Using age, customer type and 
pred = predict(trainSatLR, testSat[,c(1,2,3,4,5,6)], type="response")
pObject = ROCR::prediction(pred, testSat$satisfaction )

rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
aucObj = ROCR::performance(pObject, measure="auc")  
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))

#Random Model
trainSatRandom = trainSat
set.seed(1235)
trainSatRandom$satisfaction = sample(c(0,1), replace=T, size=nrow(trainSat))
logRegRandom = glm(satisfaction ~ ., data=trainSatRandom, family=binomial(link="logit"))

rand_pred = predict(logRegRandom, testSat[,c(1,2,3,4,5,6)], type="response")
randObject = ROCR::prediction(rand_pred, testSat$satisfaction)
rocRandObj = ROCR::performance(randObject, measure="tpr", x.measure="fpr")
aucRandObj = ROCR::performance(randObject, measure="auc")  
plot(rocRandObj, main = paste("Area under the curve:", round(aucRandObj@y.values[[1]] ,4))) 

#Should Model and Random Model produce similar graphs -->  round(aucRandObj@y.values[[1]] ,4)))  not sure if 1 and 4 are supposed to be 1 and 4

#Asses model fit 
Phat = predict(trainSatLR,testSat,type="response")
head(Phat)

prop.table(xtabs(~ satisfaction, data=testSat))

thresh = 0.5
facHat = cut(Phat, breaks=c(-Inf, thresh, Inf), labels=c(0, 1))
cTab   = xtabs(~ satisfaction + facHat, data=testSat)
addmargins(cTab)

CCR = sum(diag(cTab)) / sum(cTab)
CCR

#updating model
#using just age is not an good model 
lrUpdate = update(trainSatLR, ~ . -Gender)
summary(lrUpdate)

pred = predict(lrUpdate, testSat[,c(1,2,3,4,5,6)], type="response")
pObject = ROCR::prediction(pred, testSat$satisfaction )
rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
aucObj = ROCR::performance(pObject, measure="auc")  
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 

