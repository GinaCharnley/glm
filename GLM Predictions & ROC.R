# calculatng predictions from your GLM and ROC/AUC
# start by creating your GLM from the below data
data2
Country Alpha-3 Year Population_in_thousands Outbreak Mean_drought
1        Algeria     DZA 1970               14464.992        0 -1.383884910
2        Algeria     DZA 1971               14872.253        1 -0.993820564
#etc

glm1 <- glm(Outbreak ~ Mean_drought, family = binomial, data = data2)
summary(glm1)
Call:
  glm(formula = Outbreak ~ Mean_drought, family = binomial, data = data2)

Deviance Residuals: 
  Min      1Q  Median      3Q     Max  
-1.328  -1.059  -0.957   1.283   1.677  

Coefficients:
                Estimate  Std. Error  z value Pr(>|z|)    
  (Intercept)  -0.38957    0.05557    -7.011   2.37e-12 ***
  Mean_drought -0.11020    0.02585    -4.263   2.02e-05 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2324.4  on 1700  degrees of freedom
Residual deviance: 2305.9  on 1699  degrees of freedom
AIC: 2309.9

Number of Fisher Scoring iterations: 4

# you can also find the BIC of your model using 
BIC(glm1)
[1] 1099.416

# there are many things this summary output gives you 
# estimates is the estimated value by which the log odds outbreak would increase
# if mean_drought increased by a unit of 1. 
# then the standard error in the next column shows you how much this would move, if you
# re-ran it with a different dataset 
# the z value is the estimate/SD
# and p values are two-tailed and associated with your z value 

# to just predict a random value you are interested in 
test <- data.frame(Mean_drought = -1)
predict(glm1,test,type="response")
1 
0.4306075

# you can also create a sequence numbers that you want your GLM to predict values for 
range(data2$Mean_drought)
[1] -6.694720  6.674187
x <- seq(-7,7,0.5)
y <- predict(glm1,list(Mean_drought=x), type = "response")
# then you can plot this 
plot(data2$Mean_drought, data2$Outbreak, pch = 16, xlab = "Drought Index", ylab = "Outbreak Occurance", main = "Cholera Outbreak ~ Drought Index, Africa") 
+ lines(x, y)

# the above example just takes one variable out of the model to understand its relationship with the response variable 
# to test all the glm variables against the response variable, use the following 
predictions <- predict(glm1, data, type = "response")
# this will give a list of outbreak predictions that you can plot 

# when using the plot function you can add special characters to your axis 
xlab = expression(paste("Average Annual Temperature, ",degree,"C")) 

# if you want to plot the predicted values stored in the glm 
plot(data2$Mean_drought,data2$Outbreak)
lines(data2$Mean_drought, glm.fit$fitted.values)

# you can also use ggPredict to plot you predictions straight from your GLM 
library(ggiraph)
library(ggiraphExtra)
ggPredict(glm1,se=TRUE,interactive=TRUE) # you can also add show.summary = TRUE, to view the summary at the same time 
# https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_plotcustomize.html 
# extra info on plot aesthetics 

# another term for these predictions are marginal effects
# you can use ggpredict to create marginal effects plots 
# the idea of this is to show the relationship between the model predictor and model response 
# the predictor needs to be specified in the terms agreement 
library(ggeffects)
glm1 <- glm(Outbreak ~ Mean_drought, family = binomial, data = data2)
glm2 <- glm(Outbreak ~ Mean_drought + Population_log, family = binomial, data = data2)
ggpredict(glm1, "Mean_drought") # this would create two axis 
ggpredict(glm2, "Mean_drought", "Population_log") # this would create 2 axis and a legend 
# because ggpredict creates ggplot figures, you can alter your figure (eg. change axis label) using the same code 
# https://strengejacke.wordpress.com/2018/07/03/marginal-effects-for-regression-models-in-r-rstats-dataviz/

# you can also plot in ggplot
library(ggplot2)
ggplot(data,aes(y=outbreak,x=mean_drought)) + geom_point() + geom_smooth(method = "glm")
# or for more complicated models 
library(ggiraph)
library(ggeffects)
hglm1 <- glm(formula = Outbreak ~ Mean_drought + Avg_temp_mean + Avg_daily_prec_mean, 
    family = binomial, data = data2)
mydf <- ggpredict(hglm1, terms = c("Mean_drought [all]",  "Avg_temp_mean", "Avg_daily_prec_mean"))
ggplot(mydf, aes(x=x,y=predicted,color=group)) + stat_smooth(method = "glm") 
+ labs(x = "Mean Drought",y = "Outbreak Occurance",colour = "Average Temperature", title  = "Outbreak ~ Mean Drought + Avg. Temperature + Avg. Precipitation")

# https://cran.r-project.org/web/packages/ggeffects/ggeffects.pdf
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

# or using plot_model
library(sjPlot)
library(sjlabelled)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(glm1)
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

# you can now create a ROC curve and calculate the AUC
library(pROC)
roc(data2$Outbreak,glm1$fitted.values,plot = TRUE, print.auc = TRUE)
Call:
  roc.default(response = data2$Outbreak, predictor = glm1$fitted.values, plot = TRUE, print.auc = TRUE)

Data: glm1$fitted.values in 970 controls (data2$Outbreak 0) < 731 cases (data2$Outbreak 1).
Area under the curve: 0.5537

# as close to 1 as possible is the goal. 

# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# https://www.youtube.com/watch?v=qcvAqAH60Yw 
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

