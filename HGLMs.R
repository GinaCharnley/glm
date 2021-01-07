# Hierarachical GLMs
# https://www.rdocumentation.org/packages/hglm/versions/2.2-1/topics/hglm
# https://rdrr.io/rforge/hglm/ 

# in this case I am adding the influence of year to my GLM for cholera outbreak occurance
# and mean drought index for Africa

glmYear <- glm(outbreak~mean_drought + year, data = data, family = binomial)
summary(glmYear)

Call:
  glm(formula = outbreak ~ mean_drought + year, family = binomial, 
      data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.1162   0.1256   0.2031   0.2988   0.6165  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  158.708463  17.461442   9.089  < 2e-16 ***
  mean_drought  -0.163674   0.041937  -3.903 9.51e-05 ***
  year          -0.077691   0.008716  -8.914  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1544.5  on 5723  degrees of freedom
Residual deviance: 1424.6  on 5721  degrees of freedom
AIC: 1430.6

Number of Fisher Scoring iterations: 7

testYear <- data.frame(mean_drought = -1, year = 2005)
predict(glmYear,testYear,type = "response")
1 
0.9569244 

library(ggiraph)
library(ggiraphExtra)
ggPredict(glmYear,se=TRUE,interactive=TRUE)

# when adding variables there are one of three options
# the operators you can used include +, * and : 
# after the ~ always goes your predictor variables 
glm(outbreak~mean_drought * year, data = data, family = binomial) # * indicates that not only do we want each main effect, but also an interaction term between drought and year
# outbreak = a * b*drought + c*year + d*drought*year
glm(outbreak~mean_drought + year, data = data, family = binomial) # + indicates that we only want to know the effect of year on outbreak and not interactions with drought 
# outbreak = a + b*drought + c*year
glm(outbreak~mean_drought : year, data = data, family = bionomial) 
# outbreak = a + b*drought*year






