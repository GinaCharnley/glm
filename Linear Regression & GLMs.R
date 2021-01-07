# Linear Regression 

# https://www.datacamp.com/community/tutorials/linear-regression-R?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=255798340456&utm_targetid=dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=1006836&gclid=Cj0KCQjww_f2BRC-ARIsAP3zarGmYtH-aWO42PPt-IHdQpD6SsX4AHpOAaYZ1VurHsbvDtg3x4tSsUIaAnzNEALw_wcB#creating
# to create a simple linear regression with two variables use the following formula
# lm(target variable ~ predictor variable, data = data1)

lm1 <- lm(Deaths~Cases, data = data2)
# then you can access the models output by using the summary() function 
summary(lm1)

Call:
  lm(formula = Deaths ~ Cases, data = data2)

Residuals:
  Min      1Q  Median      3Q     Max 
-3455.0   -42.2   -33.1    -1.6  5573.4 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 33.841256  13.151303   2.573   0.0102 *  
  Cases        0.034412   0.001243  27.683   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 346.1 on 831 degrees of freedom
Multiple R-squared:  0.4798,	Adjusted R-squared:  0.4791 
F-statistic: 766.3 on 1 and 831 DF,  p-value: < 2.2e-16

# if you have more than one variable, you can keep adding them with +
lm2 <- lm(Deaths~Cases + mean_DI_national, data = data2)

summary(lm2)

Call:
  lm(formula = Deaths ~ Cases + mean_DI_national, data = data2)

Residuals:
  Min      1Q  Median      3Q     Max 
-3457.0   -45.8   -31.3    -1.7  5568.7 

Coefficients:
                Estimate Std. Error   t value  Pr(>|t|)    
(Intercept)      39.82225   16.64400   2.393    0.017 *  
  Cases             0.03434    0.00125  27.474   <2e-16 ***
  mean_DI_national  4.60961    7.85705   0.587    0.558    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 346.2 on 830 degrees of freedom
Multiple R-squared:   0.48,	Adjusted R-squared:  0.4787 
F-statistic:   383 on 2 and 830 DF,  p-value: < 2.2e-16

# the Pr(>|t|) column shows you the p-values
# looking at the residuals is a good way to test the quality of the fit 
# most data will not follow a perfectly straight line and therefore residuals are expected
# you can also look at the r(2), models close to 0 have poor fit 
# you can then plot this if you want using your model as your regression line
plot(data2$Cases,data2$Deaths) + abline(lm1)

# you can also plot your residuals 
# when you plot them they should look random 
# otherwise there may be trends or relationships you are unaware off
plot(lm1$residuals)

# now we can think about generalised linear models 
# https://www.theanalysisfactor.com/r-tutorial-glm1/
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm

glm1 <- glm(Deaths ~ Cases + mean_DI_national, data = data2)
summary(glm1)

Call:
  glm(formula = Deaths ~ Cases + mean_DI_national, data = data2)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3457.0    -45.8    -31.3     -1.7   5568.7  

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      39.82225   16.64400   2.393    0.017 *  
  Cases             0.03434    0.00125  27.474   <2e-16 ***
  mean_DI_national  4.60961    7.85705   0.587    0.558    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 119855.1)

Null deviance: 191297225  on 832  degrees of freedom
Residual deviance:  99479721  on 830  degrees of freedom
AIC: 12110

Number of Fisher Scoring iterations: 2

# you can also add offsets into your glm in the following way 
glm_offset <- glm(Outbreak ~ Mean_drought + Slum_percent + Avg_temp + Basic_sanitation 
                  + withdrawal_percap + Poverty_headcount + offset(log(Population)),
                  data = mydata, family = binomial)
# anything in an offset needs to be on a log scale 

# if you want to use this to calculate predicted probability 
# type in in what you would like to predict eg. 
newdata <- data.frame(Cases = 4000, mean_DI_national = -0.6)
# then 
predict(glm1, newdata, type = "response")
1 
174.4103 
# this means predicted probabiltiy is 174.4

# deviance is a measure of goodness of fit 
# null deviance shows how well the response varialbe is predicted by the model 

# Fisher’s Scoring Algorithm needed 2 iterations to perform the fit.
# This doesn’t really tell you a lot that you need to know 
# other than the model did indeed converge, and had no trouble doing it.

# Akaike Information Criterion (AIC) provides a method for assessing the quality of your model
# through comparison of related models.
# based on the Deviance and penalizes you for making the model more complicated.
# If you have more than one similar candidate models (where all of the variables of the simpler model occur in the more complex models)
# then you should select the model that has the smallest AIC.

# to plot the model you can then use a range of values to produced fitted values 
glm1 <- glm(Deaths~Cases, data = data2)
# you need to find your range of values in which to fit the model too 
range(data2$Cases)
[1]      0 106159
# from thid you can create series of cases and get your model to prodict deaths 
xcases <- seq(0,107000, 25) # 0 to 107,000, going up in 25 increments
ycases <- predict(glm1, list(Cases = xcases), type = "response")
plot(data2$Cases, data2$Deaths, pch = 16, xlab = "Cases", ylab = "Deaths") 
+ lines(xcases,ycases) # this fits your regression line 

# some more useful webpages on linear regression and GLMs
# https://datascienceplus.com/perform-logistic-regression-in-r/





