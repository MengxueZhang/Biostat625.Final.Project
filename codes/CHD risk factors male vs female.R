library(dplyr)
library(ggplot2)
library(psych)
library(sas7bdat)
library(cowplot)
library(magrittr)
library(reshape2)

# import the data
framingham <- read.csv("framingham.csv")
framingham1 <- na.omit(framingham)

colSums(is.na(framingham))
framingham1 <- na.omit(framingham)
View(framingham1)
head (framingham1)
dim (framingham1)
str(framingham1)

# Fitting logistic regression model
# TenYearCHD is the outcome
model <- glm(TenYearCHD ~ age + male + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, 
             data = framingham1,family = binomial)
model1 <- glm(TenYearCHD ~ age + cigsPerDay + diabetes + totChol + sysBP + glucose, 
              data = framingham1,family = binomial)

# Summary of the model
summary(model)
summary(model1)  

# Subset data for males
male_data <-framingham1[framingham1$male == 1, ]

# Subset data for females
female_data <- framingham1[framingham1$male == 0, ]

# Display the first few rows of male_data
head(male_data)
View(male_data)

# Display the first few rows of female_data
head(female_data)
View(female_data)

# Summarize the male_data
summary(male_data)

# Summarize the female_data
summary(female_data)

# Male model
male_model <- glm(TenYearCHD ~ age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, data = male_data, family = "binomial")

# Summary of the male model
summary(male_model)

# Female model
model_female <- glm(TenYearCHD ~ age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, 
                    data = female_data, 
                    family = binomial)

# Summary of the female model
summary(model_female)

# Comparison
# Compare the metrics obtained from male and female models
library(caret)
library(lattice)
set.seed(123)  

# Split Male Data
male_train_indices <- sample(nrow(male_data), 0.7 * nrow(male_data))
male_train <- male_data[male_train_indices, ]
male_test <- male_data[-male_train_indices, ]

# Split Female Data
female_train_indices <- sample(nrow(female_data), 0.7 * nrow(female_data))
female_train <- female_data[female_train_indices, ]
female_test <- female_data[-female_train_indices, ]

# Fitting the male model using 'male_train' dataset
model <- glm(TenYearCHD ~ ., data = male_train, family = binomial)

# Summary of the model
summary(model)

# Fitting the male model using 'male_train' dataset
fmodel <- glm(TenYearCHD ~ ., data = female_train, family = binomial)

# Summary of the model
summary(fmodel)

# Predict the probability of TenYearCHD in the 'male_test' dataset 
predictions <- predict(model, newdata = male_test, type = "response")
predictions

# Convert probabilities to binary predictions (0 or 1) using a threshold
threshold <- 0.5  # Adjust threshold as needed
binary_predictions <- ifelse(predictions >= threshold, 1, 0)

# Convert binary_predictions to binary values based on a threshold (e.g., 0.5)
binary_predictions <- ifelse(binary_predictions >= 0.5, 1, 0)
dim(male_test)

# Compute and print confusion matrix
conf_mat <- table(male_test$TenYearCHD, binary_predictions)
conf_mat

# Calculate the accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

# Calculate precision, recall, and F1-score
precision <- conf_mat[2, 2] / sum(conf_mat[, 2])
recall <- conf_mat[2, 2] / sum(conf_mat[2, ])
f1_score <- 2 * precision * recall / (precision + recall)

# Print the metrics of the male model 
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

predictions <- predict(model, newdata = female_test, type = "response")
threshold <- 0.5  # Adjust threshold as needed
binary_predictions <- ifelse(predictions >= threshold, 1, 0)

dim(female_test)
dim(binary_predictions)
length(female_test$TenYearCHD)
length(binary_predictions)

# Convert binary_predictions to binary values based on a threshold (e.g., 0.5)
binary_predictions <- ifelse(binary_predictions >= 0.5, 1, 0)

# Compute and print confusion matrix
conf_mat <- table(female_test$TenYearCHD, binary_predictions)
conf_mat

# Calculate accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

# Calculate precision, recall, and F1-score
precision <- conf_mat[2, 2] / sum(conf_mat[, 2])
recall <- conf_mat[2, 2] / sum(conf_mat[2, ])
f1_score <- 2 * precision * recall / (precision + recall)

# Print the metrics of the female model 
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

# Convert binary variables to characters for better visualization
framingham <- framingham %>% 
  mutate(male = as.character(male),
         currentSmoker = as.character(currentSmoker),
         prevalentHyp = as.character(prevalentHyp),
         diabetes = as.character(diabetes),
         TenYearCHD = as.character(TenYearCHD))
# Plot 1
x <- ggplot(data = framingham1, mapping = aes(x = as.factor(TenYearCHD), y = age, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data = framingham1, mapping = aes(x = as.factor(TenYearCHD), y = totChol, color = TenYearCHD)) +
  geom_boxplot()
plt <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Relationship between TenYearCHD and Age / TotCHOL", fontface='bold')
plot_grid(title, plt, ncol=1, rel_heights=c(0.1, 1))    

# Plot 2
x <- ggplot(data = framingham1, mapping = aes(x = as.factor(TenYearCHD), y = sysBP, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data =framingham1, mapping = aes(x = as.factor(TenYearCHD), y = diaBP, color = TenYearCHD)) +
  geom_boxplot()
plt <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Relationship between TenYearCHD and sysBP / diaBP", fontface='bold')
plot_grid(title, plt, ncol=1, rel_heights=c(0.1, 1))

# Plot3
x <- ggplot(data =framingham1 , mapping = aes(x = as.factor(TenYearCHD), y = glucose, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data = framingham1, mapping = aes(x = as.factor(TenYearCHD), y = cigsPerDay, fill = TenYearCHD)) +
  geom_boxplot()
plt <- plot_grid(x,y) 
title <- ggdraw() + draw_label("3. Relationship between TenYearCHD and Glucose", fontface='bold')
plot_grid(title, plt, ncol=1, rel_heights=c(0.1, 1))
