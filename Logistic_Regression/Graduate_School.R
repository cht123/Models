# http://www.ats.ucla.edu/stat/r/dae/logit.htm

# load functions
source("~/Classes/DSS/Functions.R")

# load packages
x <- c("aod", "ggplot2")
pkg_ck(x)

# get data
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# check data
head(mydata)
summary(mydata)
sapply(mydata, sd)

# make sure there aren't any nulls for the categorical variables
xtabs(~admit + rank, data = mydata)

# convert rank to a factor       
mydata$rank <- factor(mydata$rank)

# create model
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")


wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
summary(mylogit)

# slopes
# For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
# For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
# The indicator variables for rank have a slightly different interpretation. For example, having attended
# an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds 
# of admission by -0.675.

# get the confidence intervals for the coefficients
## CIs using profiled log-likelihood
confint(mylogit)

# CIs using standard errors
confint.default(mylogit)


wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
# test whether the slpoes are different 2 and 3 place ranks
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# for a one unit increase in gpa, the odds of being admitted to graduate
# school (versus not being admitted) increase by a factor of 2.23

# make a prediction
newdata <- data.frame(gre =700,gpa =3.995,rank = factor(1))
predict(mylogit, newdata, type = "response")


# create a line for rank impact
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
head(newdata1)

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

# create a line for impact of gre
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))



newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

newdata3

# plot the result
library(ggplot2)
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
 ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),size = 1)

# check how well the model fits
with(mylogit, null.deviance - deviance)
## [1] 41.5
#The degrees of freedom for the difference between the two models is equal to the 
#number of predictor variables in the mode, and can be obtained using:
  
  with(mylogit, df.null - df.residual)
## [1] 5
#Finally, the p-value can be obtained using:
  
  with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  
#  The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of 
#  less than 0.001 tells us that our model as a whole fits significantly better than 
#  an empty model. This is sometimes called a likelihood ratio test (the deviance residual
#  is -2*log likelihood). 