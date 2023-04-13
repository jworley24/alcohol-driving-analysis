library(wooldridge)
library(ggplot2)

data(wage2)

# get summaries of the variables
summary(wage2)

#Look at a plot of wage vs education
# Use gplot() -- "quick plot"
qplot(x=educ, y=wage, data=wage2)

# Calculate beta1hat
beta = cov(educ, wage)/var(educ)
x = wage2$educ
y = wage2$wage
beta1hat = cov(x, y)/var(x)

# Calculate beta0hat
beta0hat = mean(y) - (beta1hat*mean(x))

# Plot our estimated line
qplot(x=educ, y=wage, data=wage2) +
  geom_abline(intercept=beta0hat, slope=beta1hat, color="blue")

#HW Question 6
data(wine)
beta6 = cov(alcohol, deaths)/var(alcohol)
x6 = wine$alcohol
y6 = wine$deaths
beta1hat6 = cov(x6, y6)/var(x6)

#HW Question 7
wine$log_deaths = log(wine$deaths)
wine$log_alcohol = log(wine$alcohol)
lm(log_deaths ~ log_alcohol, data=wine)

summary(wine)