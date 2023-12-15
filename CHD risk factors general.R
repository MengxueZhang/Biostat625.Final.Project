# Research question:
# Which factors are predictive of CHD?
# Is there a difference between male and female for developing CHD?

# Steps:
# 1. data cleaning
# 2. descriptive statistics: summary statistics (table 1), plots
# 3. bivariate analysis: looking for potential predictive factors individually
# 4. full model: looking for potential predictive factors as a whole
# 5. check for collinearity and potential interactions
# 6. final model
# 7. model diagnose

df <- read.csv("framingham.csv")  # 4238 datapoints

# models using full data
# full model
model_full <- glm(TenYearCHD ~ male + age + education +
                    currentSmoker + cigsPerDay + BPMeds +
                    prevalentStroke + prevalentHyp + diabetes + 
                    totChol + sysBP + diaBP + BMI + heartRate + glucose,
                  data = df, family = binomial)
summary(model_full)

# significant covariates in full model only
model_sig1 <- glm(TenYearCHD ~ male + age + cigsPerDay + 
                    totChol + sysBP + glucose,
                  data = df, family = binomial)
summary(model_sig1)


# handling missing data
# delete datapoints with missing data
no_missing_data <- na.omit(df)  # 3656 datapoints

# bivariate analysis
# male # significant
glm_male <- glm(TenYearCHD ~ male, family = binomial, data = no_missing_data)
summary(glm_male)

# age # significant
glm_age <- glm(TenYearCHD ~ age, family = binomial, data = no_missing_data)
summary(glm_age)

# education # significant
glm_education <- glm(TenYearCHD ~ education,
                     family = binomial, data = no_missing_data)
summary(glm_education)

# currentSmoker
glm_cursmk <- glm(TenYearCHD ~ currentSmoker,
                  family = binomial, data = no_missing_data)
summary(glm_cursmk)

# cigsPerDay # significant
glm_cigsperday <- glm(TenYearCHD ~ cigsPerDay,
                      family = binomial, data = no_missing_data)
summary(glm_cigsperday)

# BPMeds # significant
glm_bpmeds <- glm(TenYearCHD ~ BPMeds,
                  family = binomial, data = no_missing_data)
summary(glm_bpmeds)

# prevalentStroke # significant
glm_stroke <- glm(TenYearCHD ~ prevalentStroke,
                  family = binomial, data = no_missing_data)
summary(glm_stroke)

# prevalebtHyp # significant
glm_hyp <- glm(TenYearCHD ~ prevalentHyp,
               family = binomial, data = no_missing_data)
summary(glm_hyp)

# diabetes # significant
glm_diabetes <- glm(TenYearCHD ~ diabetes,
                    family = binomial, data = no_missing_data)
summary(glm_diabetes)

# totChol # significant
glm_totChol <- glm(TenYearCHD ~ totChol,
                   family = binomial, data = no_missing_data)
summary(glm_totChol)

# sysBP # significant
glm_sysBP <- glm(TenYearCHD ~ sysBP,
                 family = binomial, data = no_missing_data)
summary(glm_sysBP)

# diaBP # significant
glm_diaBP <- glm(TenYearCHD ~ diaBP,
                 family = binomial, data = no_missing_data)
summary(glm_diaBP)

# BMI # significant
glm_BMI <- glm(TenYearCHD ~ BMI, family = binomial, data = no_missing_data)
summary(glm_BMI)

# heartRate
glm_heartRate <- glm(TenYearCHD ~ heartRate,
                     family = binomial, data = no_missing_data)
summary(glm_heartRate)

# glucose # significant
glm_glucose <- glm(TenYearCHD ~ glucose,
                   family = binomial, data = no_missing_data)
summary(glm_glucose)

# model building
# full model (no difference with full model using full data)
# only 7 of the covariates are significant
model_full_no_missing <- glm(TenYearCHD ~ male + age + education +
                    currentSmoker + cigsPerDay + BPMeds +
                    prevalentStroke + prevalentHyp + diabetes + 
                    totChol + sysBP + diaBP + BMI + heartRate + glucose,
                  data = no_missing_data, family = binomial)
summary(model_full_no_missing)

# significant covariates in above full model only
# all covariates remain significant
# final model for all
model_sig2 <- glm(TenYearCHD ~ male + age + cigsPerDay + 
                    totChol + sysBP + glucose,
                  data = no_missing_data, family = binomial)
summary(model_sig2)

# A covariate might be significantly associated with a response 
# variable when examined individually, but not significant in a full model
# due to collinearity, interaction, sample size etc.

# check collinearity
# Typically, a VIF value exceeding 5 or 10 is considered a cause for concern
# no collinearity found
car::vif(model_full_no_missing)

# correlation plot
library(psych)
corPlot(no_missing_data)

# potential interactions
# Statistical Tests for Interaction: Incorporate interaction terms in your 
# regression model by multiplying two (or more) predictors together. 
# Then, examine the coefficients and their significance in the model. 
# If the interaction term is significant, it suggests that the relationship 
# between the predictors and the response variable changes based on their 
# combined effect.

# Validation and Model Comparison: Evaluate different models, including 
# those with and without interaction terms, and compare their performance
# using metrics like AIC (Akaike Information Criterion) or BIC (Bayesian 
# Information Criterion). A model with better fit that includes interaction 
# terms might suggest their importance.
# lower AIC: better model fit

# cigsPerDay x currentSmoker # linearly related variables, need to drop one
# in fact, only cigsPerDay is significant in the original model
model21 <- glm(TenYearCHD ~ male + age + education +
                 cigsPerDay:currentSmoker + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model21)

# sysBP x prevalentHyp and diaBP x prevalentHyp # not significant
model22 <- glm(TenYearCHD ~ male + age + education + sysBP:prevalentHyp +
                 diaBP:prevalentHyp +
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model22)
anova(model_full_no_missing, model22, test = "Chisq")

# BMI x sysBP and BMI x heartRate and sysBP x age # not significant
model23 <- glm(TenYearCHD ~ male + age + education + BMI:sysBP +
                 BMI:heartRate + sysBP:age +
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model23)
anova(model_full_no_missing, model23, test = "Chisq")

# all together # none of these are significant
model24 <- glm(TenYearCHD ~ male + age + education + sysBP:prevalentHyp +
                 cigsPerDay:currentSmoker + diaBP:prevalentHyp +
                 BMI:sysBP + BMI:heartRate + sysBP:age +
                 BMI:prevalentHyp + cigsPerDay:male +
                 prevalentHyp:age +
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model24)

# all together, drop currentSmoker
# none significant, and sysBP becomes non significant
anova(model_full_no_missing, model24, test = "Chisq")
model25 <- glm(TenYearCHD ~ male + age + education + sysBP:prevalentHyp +
                 diaBP:prevalentHyp +
                 BMI:sysBP + BMI:heartRate + sysBP:age +
                 BMI:prevalentHyp + cigsPerDay:male +
                 prevalentHyp:age +
                 cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model25)
anova(model_full_no_missing, model25, test = "Chisq")

# low correlations ones
model26 <- glm(TenYearCHD ~ male + age + education + BPMeds:age +
                 prevalentStroke:age +
                 BPMeds:age + totChol:age +
                 cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model26)
anova(model_full_no_missing, model26, test = "Chisq")

# try: # get none significance currently

# diabetes x BMI # not significant
model1 <- glm(TenYearCHD ~ male + age + education + diabetes:BMI + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model1)
anova(model_full_no_missing, model1, test = "Chisq")

# diabetes x age # not significant
model2 <- glm(TenYearCHD ~ male + age + education + diabetes:age + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model2)
anova(model_full_no_missing, model2, test = "Chisq")

# diabetes x male # not significant
model3 <- glm(TenYearCHD ~ male + age + education + diabetes:male + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model3)
anova(model_full_no_missing, model3, test = "Chisq")

# diabetes x cigsPerDay # not significant
model4 <- glm(TenYearCHD ~ male + age + education + diabetes:cigsPerDay + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model4)
anova(model_full_no_missing, model4, test = "Chisq")

# diabetes x BPMeds # not significant
model5 <- glm(TenYearCHD ~ male + age + education + diabetes:BPMeds + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model5)
anova(model_full_no_missing, model5, test = "Chisq")

# diabetes x prevalentStroke # not significant
model6 <- glm(TenYearCHD ~ male + age + education + diabetes:prevalentStroke + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model6)
anova(model_full_no_missing, model6, test = "Chisq")

# diabetes x totChol # not significant
model7 <- glm(TenYearCHD ~ male + age + education + diabetes:totChol + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model7)
anova(model_full_no_missing, model7, test = "Chisq")

# diabetes x sysBP # not significant
model8 <- glm(TenYearCHD ~ male + age + education + diabetes:sysBP + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model8)
anova(model_full_no_missing, model8, test = "Chisq")

# diabetes x glucose # not significant
model9 <- glm(TenYearCHD ~ male + age + education + diabetes:glucose + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model9)
anova(model_full_no_missing, model9, test = "Chisq")

# male x cigsPerDay # not significant
model10 <- glm(TenYearCHD ~ male + age + education + male:cigsPerDay + 
                currentSmoker + cigsPerDay + BPMeds + 
                prevalentStroke + prevalentHyp + diabetes + 
                totChol + sysBP + diaBP + BMI + heartRate + glucose, 
              data = no_missing_data, family = binomial)
summary(model10)
anova(model_full_no_missing, model10, test = "Chisq")

# male x BPMeds # not significant
model11 <- glm(TenYearCHD ~ male + age + education + male:BPMeds + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model11)
anova(model_full_no_missing, model11, test = "Chisq")

# male x totChol # not significant
model12 <- glm(TenYearCHD ~ male + age + education + male:totChol + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model12)
anova(model_full_no_missing, model12, test = "Chisq")

# male x sysBP # not significant
model13 <- glm(TenYearCHD ~ male + age + education + male:sysBP + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model13)
anova(model_full_no_missing, model13, test = "Chisq")

# male x diaBP # not significant
model14 <- glm(TenYearCHD ~ male + age + education + male:diaBP + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model14)
anova(model_full_no_missing, model14, test = "Chisq")

# male x BMI # not significant
model15 <- glm(TenYearCHD ~ male + age + education + male:BMI + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model15)
anova(model_full_no_missing, model15, test = "Chisq")

# male x glucose # not significant
model16 <- glm(TenYearCHD ~ male + age + education + male:glucose + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model16)
anova(model_full_no_missing, model16, test = "Chisq")

# male x prevalentStroke # not significant
model17 <- glm(TenYearCHD ~ male + age + education + male:prevalentStroke + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model17)
anova(model_full_no_missing, model17, test = "Chisq")

# male x prevalentHyp # not significant
model18 <- glm(TenYearCHD ~ male + age + education + male:prevalentHyp + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model18)
anova(model_full_no_missing, model18, test = "Chisq")

# male x education # not significant
model19 <- glm(TenYearCHD ~ male + age + education + male:education + 
                 currentSmoker + cigsPerDay + BPMeds + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model19)
anova(model_full_no_missing, model19, test = "Chisq")

# male x cigsPerDay + male x diabetes # not significant
model20 <- glm(TenYearCHD ~ male + age + education + male:cigsPerDay + 
                 currentSmoker + cigsPerDay + BPMeds + male:diabetes + 
                 prevalentStroke + prevalentHyp + diabetes + 
                 totChol + sysBP + diaBP + BMI + heartRate + glucose, 
               data = no_missing_data, family = binomial)
summary(model20)
anova(model_full_no_missing, model20, test = "Chisq")

# no interactions found
# potential reasons
# Data Limitations: The absence of interactions could be due to limited 
# variability or insufficient data to capture interaction effects. 
# If the dataset lacks diversity or does not cover a wide range of 
# conditions or contexts, interactions might not manifest themselves.

# Sample Size: A small sample size might limit the statistical power to 
# detect interaction effects even if they exist. With smaller datasets, 
# the ability to identify subtle interaction effects might be diminished.


# model diagnosis
# constant variance
# Residuals vs Fitted
plot(as.numeric(fitted(model_sig2)), 
     as.numeric(residuals(model_sig2)), type="p",
     xlab = "Fitted", ylab = "Residual")
lines(lowess(fitted(model_sig2), residuals(model_sig2)), col="red")

# linearity
# partial regression plot
car::avPlots(model_sig2)

# normality
# histogram # bimodal
hist(residuals(model_sig2), breaks = 15)
# qq plot
qq.res = car::qqPlot(residuals(model_sig2))

# multicollinearity
car::vif(model_sig2)

# AIC
AIC(model_full_no_missing)
AIC(model_sig2)

# Perform goodness-of-fit test based on chi-square test for binomial glm
deviance_residuals <- residuals(model_sig2, type = "deviance")
summary(deviance_residuals)
chisq_test <- chisq.test(deviance_residuals[is.finite(deviance_residuals)])
print(chisq_test)
# cannot perform a goodness-of-fit test on this logistic regression
# Goodness-of-fits doesn't always work on logistic regressions

