# Statistical Modelling in R #

# LINEAR MODELS #

# All the content in this practical was developed by JJ Valletta and TJ McKinley in 2018 and adapted by Joshua Redmond and Laura Roldan in 2022.


  ## PART 1 - Simple Regression----

### Get ready ----

# Create some data to play with. 
'We will explore the correlation between weight and height. Our null hypothesis says that they are not correlated, our alternative hypothesis says that they are somehow correlated. We will answer the following questions:  
a- is there a correlation? 
b- is it strong or weak?

c- is it positive, or negative?
d- is it significant?
e- is weight a good explanatory variable?

f- how can we predict other values for which we have no actual data?'

set.seed(1453)
N <- 100 ## no. of observations
weight <- runif(n=N, min=60, max=100) ## hypothetical weights in kg
height <- 2.2*weight + rnorm(n=N, mean=0, sd=10) ## hypothetical heights in cm

df <- data.frame(height=height, weight=weight)## create a dataframe with our variables
head(df) # look at it

plot(weight, height, pch=19, xlab='Weight (kg)', ylab='Height (cm)', col='grey')


### Task 1. Correlation coefficient ----
cor(x=df$weight, y=df$height) # Answers questions a and b, yes, there is a correlation between the variables and it is quite strong 


### Task 2. Linear model fit ----
fit <- lm(height ~ weight) # answers question c, d, and e
summary(fit)

# the correlation is positive
# highly significant which means we can reject the null hypothesis. A small p-value with three *** suggests that the slope has a statistically significant difference from zero if we take the alpha = 0.001 (99.9%)
# and the variation in weight explains 86.08% of the variation in height.


### Task 3. Plot model fit ----
plot(weight, height, pch=19, xlab='Weight (kg)', ylab='Height (cm)', col='grey')
lines(weight, predict(fit), col='black', lwd=3)


### Task 4. Predictions ----
'One of the key benefits of fitting a statistical model is that we can use it to produce predictions of the dependent variable for new values of the independent variable(s). For example, in the fruitflies example above, we may want to produce estimates of average longevity for given values of thorax. To do this we must create a new data.frame object containing all the values of independent variables that we want to use for our prediction.'
# Pick the 97% confidence intervals (range of values within which we expect the population parameter to fall with a 97% confidence)
confint(fit, level=0.97) 

### One point predictions
newdata <- data.frame(weight = 85)
predict(fit, newdata, interval = "confidence", level = 0.97)

# better yet... compute the predictions running from the sample minimum to the sample maximum of the independent variable
min.weight <- min(df$weight, na.rm = TRUE)
max.weight <- max(df$weight, na.rm = TRUE)
newdf <- data.frame(weight=seq(min.weight, max.weight,length.out=10))
pr <- predict(fit, newdata = newdf, interval="confidence")
combo <- cbind(newdf, pr)
combo

## plot these predictions
library(ggplot2)
ggplot(data = combo, mapping=aes(x=weight, y=fit, ymin=lwr, ymax=upr)) + geom_line() + geom_ribbon(alpha=0.5) +
  labs(x="Weight rate", y="Height rate")


### Task 5. Model Checking ----

hist(fit$residuals) # The residuals are fairly normally distributed which is a good sign.

## Diagnostic plots

par(mfrow=c(2, 2))
plot(fit, pch=19, col='darkgrey')

# 1. Residuals vs fitted: we check that the variance is constant along the fitted line, and that there are no systematic patterns in the residuals. Our plot is good, bad plots look like a U or a line that curves down or up.
# 2. Q-Q plot: we are checking that the assumption of normally distributed errors is reasonable. Points should follow the dashed line.
# 3. Scale location: same as 1 but different scale (standardised residuals)
# 5. Residuals vs leverage: Leverage: a measure of how isolated individual points are in relation to other points
# Cook’s Distance: a measure of how influential a point is to the regression
# These measures help us identify potential outliers.

#### If you want them one by one, use which=1, 2, 3, 4, 5

plot(fit, pch=19, col='darkgrey', which=5)

par(mfrow=c(1, 1))


## PRACTICAL EXERCISES ----

# Now it's your turn. You'll find some exercises below that are a notch more complicated that the previous examples. Try to reproduce the code or fill the blanks. 

### Simple Regression----

# Context: A cost of increased reproduction in terms of reduced longevity has been shown for female fruitflies, but not for males. We have data from an experiment that used a factorial design to assess whether increased sexual activity affected the lifespan of male fruitflies.

# The flies used were an outbred stock. Sexual activity was manipulated by supplying individual males with one or eight receptive virgin females per day. The longevity of these males was compared with that of two control types. The first control consisted of two sets of individual males kept with one or eight newly inseminated females. Newly inseminated females will not usually remate for at least two days, and thus served as a control for any effect of competition with the male for food or space. The second control was a set of individual males kept with no females. There were 25 males in each of the five groups, which were treated identically in number of anaesthetisations (using CO2) and provision of fresh food medium. 

#### Get ready----

# Open the data

ff <- readRDS('data/fruitfly.rds')

# Overview of your data

head(ff)

# Install the Tydiverse package

install.packages("tidyverse")
library(tidyverse)


#### Task 1 - Plot the relationship ---- 

# Produce a scatterplot of longevity against thorax. What does the relationship look like? Fill the blanks (?????).

plot(???? ~ ?????, data = ff, 
     pch=19, col='darkgrey')

# or using tidyverse

ggplot(ff) +
  geom_point(aes(x = ?????, y = ?????))


#### Task  2 - Fit the model----

# Fit a linear model with lifespan as response variable and thorax length as explanatory variable.

fit <- 
  summary(fit)


#### Task  3 - Fit summary ----

# Display a summary of the fit, together with the 97% confidence interval for the estimated parameters.

confint(fit, level=?????)


#### Task  4 - Diagnostic plots ----

# Show the diagnostic plots for the model.

par(mfrow=c(2, 2))
plot(?????, pch=19, col='darkgrey')
par(mfrow=c(1, 1))

## produce predicted longevity for thorax = 0.8mm




## PART 2 - Regression with Categorical Explanatory Variables----


### Task 1 - Use a plot to see how longevity relates to type----


### Task 2 - Plot the relationship between thorax size and longevity - and colour the points by type----
# HINT you can control colour of your points with the "col" parameter - just pass the string name of the point
ggplot(mapping = aes(x = thorax, y
                     = longevity, colour = type)) +
  geom_point(data = ff)


### Task 3 - As before, fit a linear model using a single binary categorical variable----
fit <- lm(longevity ~ type, ff)
summary(fit)


### Task 4---- 
# What do you think is going on behind the scenes? Write a quick explanation of how the program handles categorical variables which don't have an order


### Task 5----
# Try using the model to predict a category which does not exist in the training data? What happens?
# The model should throw an error - this is because of the way in which the categorical feature is transformed into a dummy variable in the fitting of the model
newdata <- data.frame(type = "Other")
predict(fit, newdata, interval = "confidence", level = 0.97)


### Task 6----
# Plot the 97% confidence interval for the categorical variable - how can you interpret this?


### Task 7----
# Do the same but for just thorax size from the model which includes both type and thorax - how does the thorax size line change when compared to the line from the simple linear model?


# Now try adding another term - build a linear model for longevity using type and thorax length


# Plot the confidence interval for each line - and compare to your earlier simple model - how does the thorax length line change?




## PART 3 -Multiple Regression----

## Get ready----
set.seed(451)

## no. of observations
N <- 100 

## hypothetical weights in kg
weight <- runif(n=N, min=60, max=100) 

## hypothetical mean heights of parents in cm
heightParents <- runif(n=N, min=130, max=210) 

## hypothetical heights in cm
height <- 0.1*weight + 1.05*heightParents + rnorm(n=N, mean=0, sd=10) 

## store as df
df <- data.frame(weight=weight, heightParents=heightParents, height=height) 

## Plot
library(scatterplot3d) ## library needed for 3D plotting
scatterplot3d(weight, heightParents, height, pch=19, xlab='Weight (kg)', 
              ylab='Height of Parents (cm)', zlab='Height (cm)', color='grey')


### Task 1:----

# Fit a linear model 

fit <- lm(height ~ weight + heightParents, df)
summary(fit)

### Interpretation ----
# Same as before, the (Intercept)= -1.656 cm, weight= 0.1179 cm/kg and heightParents= 1.046 cm/cm are the β0, β1 and β2 parameters.

# The β0 parameter (intercept) is the expected height of someone that weighs 0 kg and whose parents have a mean height of 0 cm. This parameter is not supposed to make sense, it is used to set the starting point of the line.
# The β1 parameter tells us about the relationship between height and weight. For every 1 kg increase in weight on average the height increases by 0.1179 cm (or whatever number you got, we are generating random numbers for our data frame).
# The β2 parameter tells us about the relationship between height and heightParents. For every 1 cm increase in mean height of parents on average the person’s height increases by 1.046 cm.


### Task 6:----

# Interpret: Compare the total variation explained by this model with the one that only used thorax as a covariate. What can you say about the importance of the type of companion?

# SOLUTION:
# The R Squared statistic (also known as the coefficient of determination) is the proportion of the total variation that is explained by the regression. Which in this case is 60.2%. This is considerably higher than the 40% achieved with just thorax, suggesting that type of companion is indeed an important covariate and thus should be included in the model.


## PRACTICAL EXERCISES ----

  ### Multiple Regression----

#### Task 1 - Fit the model----
# Fit a linear model with lifespan as response variable and thorax length and sleep as explanatory variables

fit <- ?????(????? ~ ????? + ?????, ff)
summary(fit)

####Task 2 - Fit summary ----

# Display a summary of the fit, together with the 97% confidence intervals for the estimated parameters

confint(?????, level=0.97)


#### Task 3 - Interpret ----

# What does it all mean?
  # Are the variables correlated? How?
  # What parameters are significant?
  # Is your model a good explanation?


#### Task 4 - Plot ----

# Plot the mean regression line for each type of companion together with the observed data.
newdata <- expand.grid(
  thorax = seq(min(ff$thorax), 
               max(ff$thorax), length.out = 50),
  type = levels(ff$type)
)
newdata <- cbind(newdata, 
                 longevity = predict(fit, newdata))
ggplot(mapping = aes(x = ?????, y
                     = ?????, colour = type)) +
  ?????(data = ff) +
  ?????(data = newdata)


#### Task 5 - Diagnostic plots ----

# Show the diagnostic plots for the model, what can you say about the model fit?






#### Task 6 - Interpret ----
# What is the total variation explained by thorax and sleep?

