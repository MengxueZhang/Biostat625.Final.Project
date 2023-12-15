library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(caret)
library(lattice)

# Import the data
framingham <- read.csv("framingham.csv")
framingham1 <- na.omit(framingham)

# Subset data for males
male_data <-framingham1[framingham1$male == 1, ]

# Subset data for females
female_data <- framingham1[framingham1$male == 0, ]
# Display the first few rows of female_data

# male model
male_model <- glm(TenYearCHD ~ age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, data = male_data, family = "binomial")

# female model
female_model <- glm(TenYearCHD ~ age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose,
                    data = female_data,
                    family = binomial)

# Display model summary
summary(male_model)
summary(female_model)

set.seed(123)
# Split Male Data
male_train_indices <- sample(nrow(male_data), 0.7 * nrow(male_data))
male_train <- male_data[male_train_indices, ]
male_test <- male_data[-male_train_indices, ]

# Split Female Data
female_train_indices <- sample(nrow(female_data), 0.7 * nrow(female_data))
female_train <- female_data[female_train_indices, ]
female_test <- female_data[-female_train_indices, ]

model <- glm(TenYearCHD ~ ., data = male_train, family = binomial)
# Summary of the model
summary(model)

fmodel <- glm(TenYearCHD ~ ., data = female_train, family = binomial)
summary(fmodel)

model_male <- glm(formula = TenYearCHD ~ age + cigsPerDay + totChol + sysBP + glucose, 
                  family = binomial(link = "logit"), data = male_train)
summary(model_male)


# Predict the risk for male
predict_risk_m <- function(age, cigsPerDay, totChol, sysBP, glucose) {
  newData_m <- data.frame(age = age, cigsPerDay = cigsPerDay, 
                          totChol = totChol, sysBP = sysBP, glucose = glucose)
  probabilities_m <- predict(model_male, newdata = newData_m, type = "response")
  return(probabilities_m)
}


model_female <- glm(formula = TenYearCHD ~ age + cigsPerDay + prevalentHyp + glucose, 
                    family = binomial(link = "logit"), data = female_train)
summary(model_female)

# Predict the risk for female
predict_risk_f <- function(age, cigsPerDay, prevalentHyp, glucose) {
  newData_f <- data.frame(age = age, cigsPerDay = cigsPerDay, 
                          prevalentHyp = prevalentHyp, glucose = glucose)
  probabilities_f <- predict(model_female, newdata = newData_f, type = "response")
  return(probabilities_f)
}

# Deifine the UI
ui <- fluidPage(
  titlePanel("10-Year Coronary Heart Disease Risk Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = c("Male" = 1, "Female" = 0)),
      numericInput("age", "Age", value = 50, min = 0, max = 120),
      numericInput("cigsPerDay", "Cigarettes Per Day", value = 0, min = 0, max = 100),
      
      uiOutput("genderSpecificInputs"),
      
      numericInput("glucose", "Glucose Level (mg/dL)", value = 80, min = 0, max = 400)
    ),
    mainPanel(
      plotlyOutput("riskPlot"),
      h4(textOutput("riskLevel"))
    )
  )
)

server <- function(input, output) {
  
  output$genderSpecificInputs <- renderUI({
    if (input$gender == 1) {
      list(
        numericInput("totChol", "Total Cholesterol (mg/dL)", value = 200, min = 0, max = 800),
        numericInput("sysBP", "Systolic Blood Pressure (mmHg)", value = 120, min = 0, max = 300)
      )
    } else {
      numericInput("prevalentHyp", "Prevalent Hypertension", value = 0, min = 0, max = 1)
    }
  })
  
  # Calculate the risk
  risk <- reactive({
    if(input$gender == 1) {
      predict_risk_m(input$age, input$cigsPerDay, input$totChol, input$sysBP, input$glucose)
    } else {
      predict_risk_f(input$age, input$cigsPerDay, input$prevalentHyp, input$glucose)
    }
  })
  
  output$riskLevel <- renderText({
    prob <- risk()
    percent <- sprintf("%.2f%%", prob * 100) 
    risk_level <- ifelse(prob < 0.05, "Low",
                         ifelse(prob <= 0.074, "Borderline",
                                ifelse(prob <= 0.199, "Intermediate", "High")))
    
    paste("Risk Level:", risk_level, "-", percent)
  })
  
  output$riskPlot <- renderPlotly({
    prob <- risk()
    val <- round(prob * 100, 2)
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = val,
      title = list(text = "Predicted 10-year CHD Risk"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = c(0, 100)),
        bar = list(color = "87CEFF"),
        steps = list(
          list(range = c(0, 5), color = "#C1FFC1"),   
          list(range = c(5, 7.4), color = "#FFFF00"), 
          list(range = c(7.5, 19.9), color = "#FFA54F"), 
          list(range = c(20, 100), color = "#FF6347") 
        )
      ),
      number = list(suffix = "%")
    ) %>% layout(height = 400, width = 650)
  })
}

# Create the R shiny app
shinyApp(ui = ui, server = server)

