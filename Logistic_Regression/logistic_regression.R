# get data
library("MASS")
data(menarche)

# check data
str(menarche)

# review data
summary(menarche)

# review curve
plot(Menarche/Total ~ Age, data=menarche)

# fit the logit model
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age,family=binomial(logit), data=menarche)

# create the chart
plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")

# review the fit
summary(glm.out)

# odds increase by  exp(1.632) = 5.11 times for every year in age, using he age coefficient
# check the fit, not significant
1-pchisq(26.703,23)

# A chi square of 26.7 on 23 degrees of freedom yields a p-value of 0.269. The null 
# hypothesis (i.e., the model) is not rejected. The fitted values are not significantly 
# different from the observed values

# Gorilla Model

# get data
file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
read.csv(file) -> gorilla
str(gorilla)

# check variable correlation
cor(gorilla)
with(gorilla, tapply(W, seen, mean))
with(gorilla, tapply(C, seen, mean))
with(gorilla, tapply(CW, seen, mean))

# fit model
glm.out <- glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)
summary(glm.out)
# The first gives us what amount to regression coefficients with standard 
# errors and a z-test, as we saw in the single variable example above.
# None of the coefficients are significantly different from zero (but a few are close). 
# The deviance was reduced by 8.157 points on 7 degrees of freedom, for a p-value of... 

# check the reduciton in deviance from null to residual
1 - pchisq(8.157, df=7)


anova(glm.out, test="Chisq")
# Overall, the model appears to have performed poorly, showing no significant 
# reduction in deviance (no significant difference from the null model).
# Gorilla Fitted
# The second print out shows the same overall reduction in deviance, 
# from 65.438 to 57.281 on 7 degrees of freedom. In this print out, however, the 
# reduction in deviance is shown for each term, added sequentially first to last. 
# Of note is the three-way interaction term, which produced a nearly significant 
# reduction in deviance of 3.305 on 1 degree of freedom (p = 0.069).

plot(glm.out$fitted)
abline(v=30.5,col="red")
abline(h=.3,col="green")
abline(h=.5,col="green")
text(15,.9,"seen = 0")
text(40,.9,"seen = 1")