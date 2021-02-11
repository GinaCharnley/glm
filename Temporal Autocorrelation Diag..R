# Temporal Autocorrelation Diagnositics 
# https://rpubs.com/markpayne/164550

# Accouting for autocorrelation (temporal)
library(dplyr)
library(nlme)
library(MuMIn)
library(car)
library(caret)
library(boot)

outbreak <- data_main[c(1,2,3)]
outbreak <- outbreak %>% group_by(Year) %>% mutate(mean_outbreak = mean(Outbreak))
outbreak <- outbreak[-c(1,2)]
outbreak <- outbreak %>% distinct()

# Plot a time series of the data
plot(outbreak$Year, outbreak$mean_outbreak)

# Is the trend significant?
lm <- lm(mean_outbreak ~ Year, data = outbreak)
summary(lm)

Call:
  lm(formula = mean_outbreak ~ Year, data = outbreak)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.12334 -0.07086 -0.01838  0.07239  0.15892 

Coefficients:
              Estimate Std. Error t value Pr(>|t|) 
(Intercept) 23.330597   8.784091   2.656  0.0180 *
Year        -0.011348   0.004375  -2.594  0.0203 *
      
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.08836 on 15 degrees of freedom
Multiple R-squared:  0.3097,	Adjusted R-squared:  0.2637 
F-statistic:  6.73 on 1 and 15 DF,  p-value: 0.02033
# So year is significant at <0.05
# Added my lm to my time series 
lm <- lm(mean_outbreak ~ Year, data = outbreak) + abline(lm)

# Perform model diagnositics 
par(mfrow=c(2,2))
plot(lm)
# This will plot you;
# A Tukey-Anscombe plot - this checks if assumptions are met, the perfect plot is a smoothed line at height 0
# Normal Q-Q plot - all points should be close to the straight line
# Scale location - perfect should be smoothed horizontal line, showing the variance of the residuals is constant
# Leverage plot - points should be within the dashed line, meaning there are no extreme predictor values
par(mfrow=c(1,1))
plot(residuals(lm))
# then plot a timeseries of the residuals
plot(residuals(lm),type="b") + abline(h=0,lty=3)
integer(0)
# Is there evidence of autocorrelation in the residuals
acf(residuals(lm))

# Estimate linear time trend again, but accounting for AR1 autocorrelation
# Is the trend significant?
lm.ac <- gls(mean_outbreak ~ Year, data = outbreak, correlation = corAR1(form = ~Year), 
             na.action = na.omit)
summary(lm.ac)
Generalized least squares fit by REML
Model: mean_outbreak ~ Year 
Data: outbreak 
AIC       BIC   logLik
-30.78258 -27.95038 19.39129

Correlation Structure: AR(1)
Formula: ~Year 
Parameter estimate(s):
  Phi 
0.9999872 

Coefficients:
             Value Std.Error    t-value     p-value
(Intercept) 21.808521  32.67095  0.6675202  0.5146
Year        -0.010638   0.01514 -0.7026907  0.4930
             
Correlation: 
  (Intr)
Year -0.93 

Standardized residuals: 
  Min           Q1          Med         Q3            Max 
-0.002667366  0.001776784  0.006220934  0.014220404   0.021331044 

Residual standard error: 11.96888 
Degrees of freedom: 17 total; 15 residual

# Year is no longer significant 

coef(lm)
(Intercept)        Year 
23.3305974  -0.0113483 
coef(lm.ac)
(Intercept)        Year 
21.8085211  -0.0106383 

# Perform model diagnostics
plot(fitted(lm.ac),residuals(lm.ac)) + abline(h=0,lty=3)
integer(0)
qqnorm(lm.ac)
# or
qqPlot(residuals(lm), dist = "norm")
# Is there evidence of autocorrelation in the residuals
acf(residuals(lm.ac,type="p"))

# Which of the models is "better" 
model.sel(lm, lm.ac)
Model selection table 
       (Intrc)     Year   class correlation df logLik  AICc delta weight
lm      23.33   -0.01135    lm               3 18.189 -28.5  0.00  0.632      
lm.ac   21.81   -0.01064   gls crAR1(Year)   4 19.391 -27.4  1.08  0.368

Abbreviations:
  correlation: crAR1(Year) = ‘corAR1(~Year)’
Models ranked by AICc(x) 
# lm.ac has a slightly better AIC 

# Testing model performance using LOO 
# https://www.statology.org/leave-one-out-cross-validation-in-r/
# https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/
# https://www.rdocumentation.org/packages/boot/versions/1.3-25/topics/cv.glm

# To evaluate model performance you can use leave-one-out-cross-validation
# This splits the dataset into a training and a testing set 
# Using all but one observation as part of the training set
# Build a model using only data from the training set
# Use the model to predict the response value of the one observation left out 
# Calculate the mean squared error 
# Repeat this n times and calcuate the average MSE for all tests

# Specify the cross-validation method 
ctrl <- trainControl(method = "LOOCV")
# Because this is binomial I need to set my outcome variable as a two level factor 
data_best$Outbreak <- as.factor(data_best$Outbreak)
# Fit my regression model and use LOOCV to evaluate performance 
model <- train(Outbreak ~ Mean_drought + Avg_temp + withdrawal_percap + Population_log + Poverty_headcount, 
               data = data_best, method = "glm", trControl = ctrl)
# View a summary of LOOCV
print(model)
Generalized Linear Model 

748 samples
5 predictor
2 classes: '0', '1' 

No pre-processing
Resampling: Leave-One-Out Cross-Validation 
Summary of sample sizes: 747, 747, 747, 747, 747, 747, ... 
Resampling results:
  
  Accuracy   Kappa    
0.7580214  0.5046756

data_best2$Outbreak <- as.factor(data_best2$Outbreak)
model2 <- train(Outbreak ~ Mean_drought + Avg_temp + withdrawal_percap + Population_log + Poverty_headcount + Year + Basic_handwashing + HDI, 
                data = data_best2, method = "glm", trControl = ctrl)
print(model2)
Generalized Linear Model 

646 samples
8 predictor
2 classes: '0', '1' 

No pre-processing
Resampling: Leave-One-Out Cross-Validation 
Summary of sample sizes: 645, 645, 645, 645, 645, 645, ... 
Resampling results:
  
  Accuracy   Kappa    
0.7755418  0.5027023

# This function calcualtes the estimated K-fold cross-validation prediction error for glms
cv.mse <- cv.glm(data_best, glm_best) 
cv.mse2 <- cv.glm(data_best2, glm_best2) 
# Delta is a vector of two lengths
# The first is the raw cross-validation estimate of predictive error
# The second is the adjusted cross-validation estimate 
# The adjusted is designed to compensate for the bias introduced by not using LOO
cv.mse$delta 
0.1827365 0.1827345
cv.mse2$delta 
0.1653325 0.1653288



