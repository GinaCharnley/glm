# Logistic Regression and Correlation Plots 
# this kind of regression is used for testing the significance of variables that fall into specific catagories and not for continuous variables
# this is called binomial logistic regression 
# you can also use multinomial logistic regression when the exploratory variable has more than two nominal (unordered) categories.

# one way of exploring your data first is via correlation plots 
library(corrplot)
disaster 
Disaster Displacement   Age Gender  WASH Housing Healthcare `Municipal serv… Environment `Vector/Animal` Occupation Nutrition `Co-morbidity`
<chr>           <dbl> <dbl>  <dbl> <dbl>   <dbl>      <dbl>            <dbl>       <dbl>           <dbl>      <dbl>     <dbl>          <dbl>
  1 Hydrolo…            9     8     12    17       8          0                8          13              16          6         0              0
2 Meteoro…            8     6      2    11       4          2                2           2               4          1         0              1
3 Climato…            2     2      0     1       1          0                0           1               1          0         3              0
4 Geophys…           24     7      3    15      22          3                2           3               9          1         3              3
5 Conflic…           42    17      8    20      19         28                2           0              12          5         7              2
correlations <- cor(disaster[,2:15])
corrplot(correlations, method = "circle")

# to find the logistic regression of the data above you need multinomial logistic regression because it has 5 different nomial catagories 
# the glm() function can be used to fit normal logistic regression, see links 

library(nnet)
multinom_health <- multinom(Disaster ~ Healthcare, data = disaster)
# weights:  15 (8 variable)
initial  value 8.047190 
iter  10 value 3.843559
iter  20 value 1.656918
iter  30 value 1.397352
iter  40 value 1.387576
iter  50 value 1.387499
iter  60 value 1.387357
iter  70 value 1.387145
iter  80 value 1.387101
iter  90 value 1.386837
iter 100 value 1.386821
final  value 1.386821 
stopped after 100 iterations
library(AER)
coeftest(multinom2)
z test of coefficients:
                                      Estimate  Std. Error     z value Pr(>|z|)    
Conflict (n=45):(Intercept)       -1.7685e+02  1.3142e-08 -1.3457e+10   <2e-16 ***
  Conflict (n=45):Healthcare         3.3788e+01  3.6776e-07  9.1874e+07   <2e-16 ***
  Geophysical (n=28):(Intercept)    -5.1690e+01  2.7357e+02 -1.8890e-01   0.8501    
Geophysical (n=28):Healthcare      2.7925e+01  2.7639e+02  1.0100e-01   0.9195    
Hydrological (n=43):(Intercept)   -6.0464e-03  1.4142e+00 -4.3000e-03   0.9966    
Hydrological (n=43):Healthcare    -9.4987e+00  2.9518e-06 -3.2179e+06   <2e-16 ***
  Meteorological (n=15):(Intercept) -9.9311e+00  1.4318e+02 -6.9400e-02   0.9447    
Meteorological (n=15):Healthcare   1.1181e+01  2.6018e+02  4.3000e-02   0.9657    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# this function completes a Wald z-tests to give you the p values 
# you need a comparison to run the test, the model will automatically pick the one alphabetically first eg. climatological here. 
# you can add means to compare and then tell the data.frame that its the reference 
disaster$Disaster <- relevel(disaster$Disaster, ref = "Mean")
# this will only work though if unordered factors are what you want to assign as the reference 
# to unorder 
disaster$Disaster <- factor(disaster$Disaster , ordered = FALSE )



# https://www.datacamp.com/community/tutorials/logistic-regression-R
# https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/
# https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package
# http://www.princeton.edu/~otorres/LogitR101.pdf








