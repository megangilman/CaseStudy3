library(shiny)
library(maps)
library(mapproj)
library(rsconnect)
library(ggplot2)
library(ROCR)
library(popbio)
library(visreg)


source("helpers.R")





# See above for the definitions of ui and server
# Define UI ----


ui <- fluidPage(
  titlePanel("Airline Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a plot to display"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Plot_the_Data",
                              "Estimated_Cat_Prob",
                              "Conditional_Density_Plot",
                              "Logic_Regression_Results_P1",
                              "Logic_Regression_Results_P2",
                              "Logic_Regression_Results_P3",
                              "Model", 
                              "Random"),
                  selected = "Model")
      
    ),
    
    mainPanel(
      
      plotOutput("selected_plot")
  
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  airlineReportAll <- read.csv("data/test.csv")
  sum(grepl("neutral or dissatisfied", airlineReportAll$satisfaction))
  #  14573 of 25976 are "neutral or dissatisfied"
  
  
  set.seed(11)
  
  airlineReportSample <- airlineReportAll[sample(nrow(airlineReportAll), 500), ]
  
  airlineData = airlineReportSample[, c(3,4,6,7,8,23,25)]
  
  airlineData$satisfaction <- ifelse(airlineData$satisfaction %in% c("neutral or dissatisfied"), 0, 1)
  airlineData$Customer.Type <- ifelse(airlineData$Customer.Type %in% c("disloyal Customer"), 0, 1)
  airlineData$Gender <- ifelse(airlineData$Gender %in% c("Male"), 0, 1)
  airlineData$Type.of.Travel <- ifelse(airlineData$Type.of.Travel %in% c("Business travel"), 0, 1)
  airlineData$Class <- ifelse(airlineData$Class == "Business", 1,
                              ifelse(airlineData$Class == "Eco", 2,
                                     ifelse(airlineData$Class == "Eco Plus", 3, NA)))
  #1 for business 2 for eco 3 for eco plus 
  
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

  
  generate_plot <- function(data) {
  # Plot the data 
  if (input$var == "Plot_the_Data") {
    # Plot the data
    output <-  qplot(Flight.Distance, satisfaction, data = airlineData) + geom_point(colour = "#3366FF", size = 3)
      
  
  #Plot the conditional density plot 
   } else if  (input$var == "Estimated_Cat_Prob"){
  #Computes and plots conditional densities describing how the conditional distribuion of a categorial variable "satisfaction" changes over a numerical variable 'Age'
  cdplot(factor(satisfaction) ~ Flight.Distance, data=airlineData, main="Estimated categ prob", ylab='Satisfaction')
  }else if  (input$var == "Conditional_Density_Plot"){
    output <- ggplot(airlineData, aes(x=Flight.Distance, fill=factor(satisfaction))) +
    geom_density(position="fill") +
    ylab('Probability') +
    theme(legend.position='bottom')
  
   }  else if  (input$var == "Logic_Regression_Results_P1"){
  
     #Visulization of Logic Regression results 
     
     #As age increases the probability of satisfaction will increase
     plot(trainSat$Flight.Distance, trainSat$satisfaction, xlab="Flight.Distance", ylab="P(satisfaction)")
     trainLR = glm(satisfaction ~ Flight.Distance, data=trainSat, family=binomial(link="logit"))
     curve(predict(trainLR,data.frame(Flight.Distance=x),type="resp"),add=TRUE)
     points(trainSat$Flight.Distance,fitted(trainLR),pch=20)
     
    }  else if(input$var == "Logic_Regression_Results_P2"){
     popbio::logi.hist.plot(trainSat$Flight.Distance, trainSat$satisfaction, boxp=FALSE,type="hist",col="gray")
       
     
    
    } else if (input$var == "Logic_Regression_Results_P3"){
     
    
     logReg = glm(satisfaction ~ ., data=trainSat, family=binomial(link="logit"))
     visreg::visreg(logReg, "Flight.Distance", scale="response", partial=FALSE, xlab="Flight.Distance", ylab="P(satisfaction)", rug=2)
     
     
    } else if (input$var == "Model"){
     #Model evaluation - Reciever Operating Characteristics (ROC) Curve 
     #Create a prediction object
     #Using age, customer type and 
     pred = predict(trainSatLR, testSat[,c(1,2,3,4,5,6)], type="response")
     pObject = ROCR::prediction(pred, testSat$satisfaction )
     
     rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
     aucObj = ROCR::performance(pObject, measure="auc")  
     plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))
     } else if (input$var == "Random"){
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
  } 
    
    
 
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
  
  text <- CCR 
  
  
 
return(output)
  }
  
  # Render the plot based on the selection
  output$selected_plot <- renderPlot({
    generate_plot(airlineData)
  })
  
}

# Run app ----
shinyApp(ui = ui, server = server)

# run("my_app") will run the app