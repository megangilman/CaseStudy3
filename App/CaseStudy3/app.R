library(shiny)
library(maps)
library(mapproj)
library(rsconnect)
library(ggplot2)
library(ROCR)
library(popbio)
library(visreg)
library(magrittr)
library(dplyr)




# See above for the definitions of ui and server
# Define UI ----


ui <- fluidPage(
  titlePanel("Airline Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a plot to display"),
      
      selectInput("var", 
                  label = "Algorithmic Analysis Plot to display",
                  choices = c("Plot_the_Data",
                              "Estimated_Cat_Prob",
                              "Conditional_Density_Plot",
                              "Logic_Regression_Results_P1",
                              "Logic_Regression_Results_P2",
                              "Logic_Regression_Results_P3",
                              "Model", 
                              "Random",
                              "None"),
                  selected = "None"),
      
      selectInput("var2", 
                  label = "Exploratory Variable to Display",
                  choices = c("Frequency of Satisfied by Distance",
                              "Mean Satisfaction vrs. Flight Distance",
                              "Mean Satisfaction vrs. Seat Comfort Rating",
                              "None"),
                  selected = "None"), 
      
      selectInput("text_option", "Select text option:", 
                  choices = c("Exploration", "Problem Statement", "Algorithm Explanation")),
      textOutput("selected_text")
      
      
    ),
    
    mainPanel(
      
      plotOutput("selected_plot"),
      
      plotOutput("selected_plot2"),
      
      textOutput("output_text")
  
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
         qplot(Flight.Distance, satisfaction, data = airlineData) + geom_point(colour = "#3366FF", size = 3)
        
        
        #Plot the conditional density plot 
      } else if  (input$var == "Estimated_Cat_Prob"){
        #Computes and plots conditional densities describing how the conditional distribuion of a categorial variable "satisfaction" changes over a numerical variable 'Age'
        cdplot(factor(satisfaction) ~ Flight.Distance, data=airlineData, main="Estimated categ prob", ylab='Satisfaction')
      }else if  (input$var == "Conditional_Density_Plot"){
        ggplot(airlineData, aes(x=Flight.Distance, fill=factor(satisfaction))) +
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
      
      text <- as.character(CCR)
      
    
  generate_plot2 <- function(data) {
    
    if (input$var2 == "Frequency of Satisfied by Distance"){
    # Create the histogram based on Frequency of Satisfied by Distance 
     ggplot(data = airlineData, aes(x = Flight.Distance, fill = factor(satisfaction))) +
      geom_histogram(binwidth = 100, position = "dodge") +
      labs(x = "Flight Distance", y = "Frequency", title = "Frequency of Satisfied by Distance")
    #Here I found that there is a discrepancy in the total amount of times satisfied and neutral or dissatisifed are reported
    #Find the mean satisfaction for a distance 
    } else if (input$var2 ==  "Mean Satisfaction vrs. Flight Distance") { 
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
        labs(x = "Flight Distance", y = "Satisfaction", title = "Satisfaction vs Distance")+ 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      #Overall there seems to be a general trend in regards to the mean satisfaction based on distance 
    } else if (input$var2 ==  "Mean Satisfaction vrs. Seat Comfort Rating") {
      # Convert Seat.comfort to a factor with ordered levels
     
      airlineReportSample$satisfaction <- ifelse(airlineReportSample$satisfaction %in% c("neutral or dissatisfied"), 0, 1)
      
      # Calculate mean satisfaction for each level of Seat.comfort
      meanSeat <- airlineReportSample %>% 
        group_by(Seat.comfort) %>% 
        summarise(mean_satisfactionSEAT = mean(satisfaction))
    
      
      # Create scatter plot
      ggplot(meanSeat, aes(x= Seat.comfort, y =  mean_satisfactionSEAT )) + 
        geom_point() +
        labs(x = "Seat Comfort", y = "Satisfaction")
      
     }
  }
  
  # Render the plot based on the selection
  output$selected_plot <- renderPlot({
    generate_plot(airlineData)
  })
  
  output$selected_plot2 <- renderPlot({
    generate_plot2(airlineSampleData)
  })
  
  output$selected_text <- renderText({
    if (input$text_option == "Exploration") {
      return("Exploration: Plots can be found under “Exploratory Variable to Display” drop down

	When I started my analysis of this data I created a histogram displaying the frequency of satisfied or neutral/dissatisfied. This histogram offered little insight because there was a discrepancy in total frequency. In order to account for this I created a new scatter plot which would display the relationship between Flight Distance and Satisfaction mean. This graph, Mean Satisfaction vrs. Flight Distance demonstrates that there is a linear relationship between an increase in distance and satisfaction, along with some outliers. In order to explore further I looked into a key metric that I thought would impact satisfaction including seat comfort and food and drink ratings. When exploring how seat comfort impacted satisfaction I discovered as shown in Mean Satisfaction vrs. Seat Comfort satisfaction is much higher when the seat comfort rating is 4 or 5. This is important for a business to know. As satisfaction scores are very low when seat comfort is rated 1,2,3. This may be an area an airline should invest more money into. This relationship was linear in that when the rating increased so did the satisfaction. The mean satisfaction of the sample data was 0.402. The mean satisfaction of the sample data only containing seat comfort ratings greater than or equal to 4 went up to .5985401.
")
    } else if (input$text_option == "Problem Statement") {
      return("Problem Statement: 

Discovery: How do we identify the satisfaction of an airline customer? 
Initial Hypothesis: Flight Distance is a key predictor of customer satisfaction
Data: A random sample of 500 airline customer reports.
Model: Logistic regression to identify most influential factors predicting satisfaction.I chose to build logistic regression because it can be used for yes or no prediction. Logistic regression can be considered a classification problem. The probability of an event to occur. Through building a model based on logistic regression the model can be used to predict the probability that an event will occur as a function of other variables. The capability of answering a yes or no question worked well for my problem statement of whether or not the airline customer was satisfied. Yes satisfied, no neutral or dissatisfied. Satisfaction will be represented by the highest probability 1 and the lowest will be 0 not satisfied or neutral. A variety of probabilities between the two can occur. When using yes/no as done with logistic regression a threshold is needed. Typically this threshold is 0.5. Overall logistical regression is capable of finding explanatory values and is easily able to manipulate variables in order to observe their impact. 
Technical Description: Since logistic regression deals with probability, thus the ln of probability of the event occurring divided by the probability of the event not occurring. The total probability is 1. P(y=1) means true. Logiy (P(y=1)) is inverted by the sigmoid function. Logistic Regression is iterative meaning that until the optimized model is developed by iteratively re-weighting least squares.
Diagnostic: In order to find out if a model is good or not use the key metric AUC. The AUC tells you how well the model predicts. Ideally this will be 1. The AUC for logistic regression is a good metric to help guide setting the classifier threshold. Based on the threshold the probabilities will be divided. The model is a good model if the AUC is increasing and approaching one. A good model can predict successfully on the new dataset.
Results: By the end of this case study I have created a model that is able to successfully predict whether or not a customer will be satisfied. I have also identified that seat comfort is really important to customers.
Business Impact: If we can target customers who are predicted to be unsatisfied, staff can target them to improve their satisfaction. 
")
    } else {
      return("Algorithm Explanation: 

Just simply plotting the data as shown in the Plot_the_Data graph does not provide much valuable information. The conditional density plot and Estimated Categ Prob plots compute and plot conditional densities describing how the conditional distribution of a categorical variable ‘satisfaction’ changes over flight distance.These show that as Flight Distance increases so will satisfaction. Logistic Regression: This algorithm utilizes the glm (generalized linear model) function to fit what we are predicting in this case that will be satisfaction. The ‘glm’ is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution. This logistic regression model is predicting satisfaction using Gender, Customer Type, Type of Travel, Class, Flight, Distance, and Departure Delay in Minutes. Confidence intervals for regression coefficients are created and then the data is split into the training and the test sets. For each of the variables Gender, Customer Type, Type of Travel, Class, Flight, Distance, and Departure Delay in Minutes a confidence interval of 2.5% and 97.5% was found using confint. After the data was split I had both a set of training data and testing data. My  model was then built from the training data and then used to predict the test data. After it runs on the test data its performance can be evaluated. In order to evaluate its performance I visualized the results.This demonstrated that as Flight Distance increased the probability of satisfaction increased. This is shown in Logic_Regression_Results_P1, Logic_Regression_Results_P2, and Logic_Regression_Results_P3.After reviewing my results I conducted a model evaluation that began with creating a prediction object. This function is used to transform the input data into a standardized format. The logistic regression model object was then applied on the test data to predict it. Then an ROCR is created (prediction object). Predictor evaluations were then conducted using ‘performance’ function. An ROCR object is created that provides the true positive rate along with the false positive rate. The area under the curve was calculated to be .8352 which is not bad model performance. This is shown by the Model plot. At this point a model has been created on the Airline Response Data. In order to test the effectiveness of my model I then tested a random model performance. To do this random data was created and the model was built on it. None of the independent variables were changed, however the dependent variable (satisfaction) was randomly changed. This is done to test if the model built on random data can predict the satisfaction. The performance of the random model is very bad. As displayed by the Random plot the area under the curve is .7234. I proceeded to access the model fit. The correct classification rate (CCR) of my model is the observations that are correctly predicted divided by the total .76.  76 % of the time the model can predict whether or not a customer was satisfied.Throughout this Case Study I have built a model in logistic regression and have evaluated the metrics. Since the model did not perform well when I changed the dependent variable to random, but worked well when I trained it. That means that my model works well based on the data. If I remove the independent variable Age from my model using the update function my model then works better.
")
    }
  })
  
  
  
}

# Run app ----
shinyApp(ui = ui, server = server)

# run("my_app") will run the app