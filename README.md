# Biostat625 Final Project: Predicting Coronary Heart Disease (CHD) Risk through Logistic Regression Modeling

This is the final project by Siqiao Chen, Zhengrui Huang, Mengxue Zhang and Ebo Essilfie-Amoah for Biostat625, fall 2023.

In this project, we first collected our data from www.kaggle.com, followed with a series of data analysis algorithms including data cleaning, exploratory analysis, logistic regression and model diagnostics, 
to find out the predicted fators for CHD in both the full data, and the gender stratified data. In general, 6 out of 15 variables were found significantly correlated with the outcome variable of developing CHD.
For the gender statified analysis, different variables were found to be responsible for the onset of CHD. Finally, the confusion matrix was used to evaluate hot the model's predictions align with the actual outcomes, distinguishing between positive and negative cases.

An R Shiny App was built to visualize our prediction results, which can be viewed and interact with our Shiny app via the link https://10yearchdprediction.shinyapps.io/625rshinyapp/.
 
This repository contains the following materials:

- `README.md`
- `framingham.csv`: Dataset we used
- `CHD risk factors general.R`: Code for analyzing general data
- `CHD risk factors male vs female.R`: Code for analyzing gender-stratified data
- `app.R`: Codes for Shiny app
- `Report/`: R Markdown files and knitted pdf files of final report and figures embedded
