# Import the data
framingham <- read.csv("framingham.csv")

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Calculate the number of missing values for each variable
missing_data <- framingham %>%
  summarise_all(funs(sum(is.na(.))))

missing_data_long <- gather(missing_data, key = "Variable", value = "Missing")

# Plot the missing data
ggplot(missing_data_long, aes(x = reorder(Variable, Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "skyblue2") +
  coord_flip() + 
  labs(x = "Variables", y = "Numbers of Missing Values", title = "Missing Data by Variable") +
  theme_minimal()

# Remove the missing value
framingham1 <- na.omit(framingham)

# Calculate means and standard deviations
means <- c(mean(framingham1$age), mean(framingham1$cigsPerDay, na.rm = TRUE),
           mean(framingham1$totChol, na.rm = TRUE), mean(framingham1$sysBP, na.rm = TRUE),
           mean(framingham1$diaBP, na.rm = TRUE), mean(framingham1$BMI, na.rm = TRUE),
           mean(framingham1$heartRate, na.rm = TRUE), mean(framingham1$glucose, na.rm = TRUE))

std <- c(sd(framingham1$age, na.rm = TRUE), sd(framingham1$cigsPerDay, na.rm = TRUE),
         sd(framingham1$totChol, na.rm = TRUE), sd(framingham1$sysBP, na.rm = TRUE),
         sd(framingham1$diaBP, na.rm = TRUE), sd(framingham1$BMI, na.rm = TRUE),
         sd(framingham1$heartRate, na.rm = TRUE), sd(framingham1$glucose, na.rm = TRUE))

table_df <- data.frame(
  Characteristic = c("Age (years)", "CigsPerDay", "TotChol (mg/dL)", "SysBP (mmHg)",
                     "DiaBP (mmHg)", "BMI (kg/m^2)", "HeartRate (beats/minute)", "Glucose (mg/dL)"),
  Description = c("Age at the time of medical examination in years",
                  "Number of cigarettes smoked each day",
                  "Total cholesterol",
                  "Systolic Blood Pressure",
                  "Diastolic blood pressure",
                  "Body Mass Index",
                  "Heart Rate",
                  "Blood glucose level"),
  N = rep(nrow(framingham1), 8), 
  Mean = means,
  Standard_Deviation = std
)

# Print the table
print(table_df)