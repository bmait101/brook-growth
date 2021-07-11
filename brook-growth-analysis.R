# ////////////////////////////////
# Sadie-Brook Maitland's Growth Analysis
#
# Created by Bryan M Maitland
# Date Created: 27 NOV 2016
# Last Modified: 27 NOV 2016
# ////////////////////////////////

library(tidyverse)

# Set WD
# Read in data. Contains Brook's mass (lbs.) with corresponding date of
#  measurment and number of days since birth or whelping.
dat <- read_csv("BrookWeight.csv")

# Convert date to POSIXct format
# Plot to check data; always look at your data first!
dat %>% 
  ggplot(aes(x = DaysSinceBirth, y = Weight)) + 
  geom_point() + 
  theme_bw()

# ////////////////////////////////////////////////////
# ////////////////////////////////////////////////////
# BEGIN Logistic Function Modeling section
# ////////////////////////////////////////////////////

# Over time, Brook's growth should look like a logistic function:
#   put on pounds fast and early, then level off. 
# A logistic growth model can be applied using nls
#   nls = non-linear least squares

# Data from Brian Chenge online example:
# Here's the data
mass<-
  c(6.25,10,20,23,26,27.6,29.8,31.6,37.2,41.2,48.7,54,54,63,66,72,72.2,76,75) #Wilson's mass in pounds
days.since.birth<-
  c(31,62,93,99,107,113,121,127,148,161,180,214,221,307,452,482,923,955,1308) #days since Wilson's birth

data<-data.frame(mass,days.since.birth) #create the data frame
plot(mass~days.since.birth, data=data) #always look at your data first!

# Logistic growth function can be written as:
#   y <- phi1 / (1 + exp ( -(phi2 + phi3 * x)))
# y = Brook's mass (or can be population size or responce var showing logistic growth)
# phi1 = 1st parameter = asymptote (e.g. Brook's stable adult mass)
# phi2 = 2nd parameter = 
# phi3 = 3rd parameter = growth param = describes how quickly y appraoches asymptote
# x = the input variable, in our case, days since Brook's birth

# One important difference b/w "nls" and other models (e.g. ordinary least squares)
#   is that "nls" requires initial starting parameters. This is b/w R will 
#   iteratively evauluate and tweak model parameters to minimize model error
#   (hence the least-squares part), but R needs a place to start.

# 1. To do so, call coefficients of a linear model (slope and intercept) using 
#   the logit transform (log of the odds) and scaling by y by a 
#   reasonable approx. of the asymptote (e.g 100 lbs.)
coef(lm(logit(mass/100)~days.since.birth,data=data))

# 2. Use values from above in nls function as starting params
#   Here, we comstruct the model using the starting params, 
#   Trace returns the iterations. 
wilson <-nls (mass ~ phi1 / (1 + exp ( -(phi2 + phi3 * days.since.birth))),
            start = list(phi1 = 100, phi2 = -1.096, phi3 = .002), 
            data=data,trace=TRUE)
# The 1st column is the erroe (sums of squared error?) and the 
#   remaining columns are the model params.
#   R took 11 iterations to reach model params it was happy with

# 4. Lets check the summary
summary(wilson)
# We can see the initial params are not too far off. 

# 5. Create model predictings and plot the data
# Set parameters
phi1 <- coef(wilson)[1]
phi2 <- coef(wilson)[2]
phi3 <- coef(wilson)[3]
# Construct a range of x values bounded by the data
x <- c(min(data$days.since.birth):max(data$days.since.birth)) 
# Predicted mass of x from above line
y <- phi1 / (1 + exp(-(phi2 + phi3 * x)))
# Create the prediction data frame
predict <- data.frame(x,y) 
# Plot it
ggplot(data = data, aes(x = days.since.birth, y = mass))+
  geom_point(color = 'blue', size=5) +
  theme_bw() +
  labs(x ='Days Since Birth', y = 'Mass (lbs)') +
  scale_x_continuous(breaks = c(0,250,500,750,1000,1250)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24)) +
  geom_line(data = predict, aes(x = x, y = y), size = 1)

# Looks like a good model. It suggests that Wilson should asymptote at 71.57 lbs
#   (loose some weight Wilson!). 
# The model is under-predicting (under-estimating) his weight in the later region
#   of data, prob b/c of the 2 data points near inflection point. Still, good!

# ////////////////////////////////////////////////////
# END Logistic Function Modeling section
# ////////////////////////////////////////////////////






