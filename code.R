# questions:
# “Is an automatic or manual transmission better for MPG”
# "Quantify the MPG difference between automatic and manual transmissions"

# 2 pages, pdf, figures in appendix (total 5 pages), executive summary (1 paragraph)
# exploratory
# model selction strategy
# regression
# interp. coef.
# diagnostic
# answer questions, quantify the uncertainty

library(tidyr)

?mtcars # am effect on mpg
str(mtcars)
summary(mtcars)
pairs(mtcars)

# checking correclation between variables
x <- as.data.frame(cor(mtcars))  # it's OK, as either binomial data (0-1) or ordered nominal with meaningful numerical values
x$var1 <- rownames(x)
x <- gather(x, var2, cor, -var1)
x$var2 <- as.character(x$var2)
x <- x[x$var1 < x$var2,]
x$cor <- abs(x$cor)
x <- x[order(x$cor, decreasing = T),]
head(x, 10)
# it looks like one should be careful with including in as regressors following pairs:
# cyl & disp -> measure the same (number of cylinders and their total volume)
# disp & wt -> different things
# cyl & hp -> cyl & gross horsepower
# cyl & vs -> cyl & v engine or straight engine
# am & gear -> transsmission type & number of forward gears
summary(mtcars[mtcars$am == 0,"gear"])  # 3-4
summary(mtcars[mtcars$am == 1,"gear"])  # 4-5

# order of regressors:
x[x$var1 == "mpg" | x$var2 == "mpg",]
# wt, cyl, disp, hp, drat, ve, am, carb, gear, qsec
# we see am far on the list, let's start with most promising variables and try to see if am is of any help
# strategy 1:
# am, am+wt, am+wt+cyl, am+wt+cyl+hp, am+wt+cyl+hp+drat, am+wt+cyl+hp+drat+ve
# strategy 2:
# all vars, then eleminate highest p-values
plot(subset(mtcars, select = c(mpg, am, wt, cyl, hp, drat)))  # cor between the most correlated variables

attach(mtcars)

# exploratory
# first let's take a look at am relation with mpg alone
par(mfrow = c(1, 1))
boxplot(mpg ~ am)
# promising, but we should check for confounders, as this is most probably not a randomized study
fit1 <- lm(mpg ~ am)
plot(am, mpg)
abline(fit1)
summary(fit1)
par(mfrow = c(2, 2))
plot(fit1)  # not very helpful, we see it just shows there's a difference in means between two groups

# let's add wt
fit2 <- lm(mpg ~ am + wt)
summary(fit2)  # am is irrelevant here, so maybe wt is the confounder? -> come back in a moment
plot(fit2)  # heteroscedasticity -> looks like nonlinear, maybe we could add (^2) or interactions?
# outlayer: 17
fit2 <- lm(mpg ~ am + wt, data = mtcars[-17,])
summary(fit2)
plot(fit2)

# adding interactions - much better
fit2a <- lm(mpg ~ am * wt)
summary(fit2a)
plot(fit2a)
# residuals looks much better, no much patter
# outlayers: 17, 18, 20, 8 (espcially for normality)
fit2a <- lm(mpg ~ am * wt, data = mtcars[-c(17,18,20,8),])
summary(fit2a)
plot(fit2a)

# plot the outlayers
outlayers <- c(17,18,20,8)
col <- sapply(1:length(mpg), function(x) as.numeric(x %in% outlayers) + 1)
par(mfrow=c(1,1))
plot(mpg ~ wt, col=col)
par(mfrow=c(2,2))

# adding wt^2 much better too
fit2b <- lm(mpg ~ am + wt + I(wt^2))
summary(fit2b)
plot(fit2b)

# let's see what we have here
# add titles and legend here
par(mfrow = c(1, 2))
# 0/1 - automatic, 1/2 - manual (am value / color)

# model fit2b
# red = automatic
# green = manual
plot(wt, mpg, xlim = range(wt), ylim = c(0, 35), col = am + 2, lwd = 2)
curve(fit2b$coefficient[1] + fit2b$coefficient[3] * x + fit2b$coefficient[4] * x^2,
      xlim = range(wt), ylim=c(0,35), add = T, col = 2)
curve(fit2b$coefficient[1] + fit2b$coefficient[2] + fit2b$coefficient[3] * x + fit2b$coefficient[4] * x^2,
      xlim = range(wt), ylim=c(0,35), add = T, col = 3)

# model fit2a
plot(wt, mpg, xlim = range(wt), ylim = c(0, 35), col = am + 2, lwd=2)
curve(fit2a$coefficient[1] + fit2a$coefficient[3] * x,
      xlim = range(wt), ylim=c(0,35), add = T, col = 2)
curve(fit2a$coefficient[1] + fit2a$coefficient[2] + (fit2a$coefficient[3] + fit2a$coefficient[4]) * x,
      xlim = range(wt), ylim=c(0,35), add = T, col = 3)
par(mfrow = c(2, 2))
# we can see the problem: it looks like:
# am is 0 (automatic) for heavier cars, and 1 for lighter cars
# so wt is a confounder here
# it looks like what we see is negative quadratic relation between wt and mpg
boxplot(mpg ~ am)
boxplot(lm(mpg ~ wt)$residuals ~ am)  # look how removing wt effects make am irrelevant
    # it can't help explaing mpg by shifting the regression line, as the data shows non-linearity
boxplot(lm(mpg ~ wt + I(wt^2))$residuals ~ am)  # ... but after accounting for nonlinearty am can be of help again
boxplot(lm(mpg ~ wt + am:wt)$residuals ~ am)

# just checking
summary(fit2)$coefficients
summary(fit2a)$coefficients
summary(fit2b)$coefficients

# follow this: say that fit2b is better, more reasonable
# but maybe explore fit2a for fun

fit3 <- lm(mpg ~ am + wt + cyl)
summary(fit3)
plot(fit3)

fit3a <- lm(mpg ~ am * wt + cyl)
summary(fit3a)
plot(fit3a)

anova(fit1, fit2, fit2a, fit3a)  # 2/1, 2a/2, 3a/2a

fit3b <- lm(mpg ~ am + wt + I(wt^2) + cyl)
summary(fit3b)
plot(fit3b)

par(mfrow = c(1, 1))
plot(wt, mpg, xlim = range(wt), ylim = c(0, 35), col = cyl / 2, lwd = 2) # similar story

fit4a <- lm(mpg ~ am * wt + cyl + hp)
summary(fit4a)
plot(fit4a)

fit4b <- lm(mpg ~ am + wt + I(wt^2) + cyl + hp)
summary(fit4b)
plot(fit4b)


par(mfrow = c(1, 1))
plot(hp, mpg) # , xlim = range(wt), ylim = c(0, 35), col = cyl / 2, lwd = 2) # similar story
summary(lm(mpg ~ hp + I(hp^2)))

anova(fit1, fit2, fit2a, fit3a, fit4a)  # 2/1, 2a/2, 3a/2a
anova(fit1, fit2, fit2b, fit3b, fit4b)  # 2/1, 2a/2, 3a/2a
# it looks like we could go with the fit3a model
# let's check correlation
data <- data.frame(resid = fit3a$residuals, subset(mtcars, select = c(hp, drat, qsec, vs, gear, carb)))
head(data)
cor(data)

fit5 <- lm(mpg ~ am * wt + cyl + hp + drat)
summary(fit5)

# strategy 2
summary(lm(mpg ~ wt*am + ., data = mtcars))
summary(lm(mpg ~ I(wt^2) + ., data = mtcars))
