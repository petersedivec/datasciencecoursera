# Regression Models Quiz #1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mean(x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

fit <- lm(y ~ x - 1)
summary(fit)

data(mtcars)
summary(mtcars)
fit <- lm(mpg ~ wt, mtcars)
summary(fit)

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x-mean(x))/sd(x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit <- lm(y ~ x)
summary(fit)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# Quiz #2
#1 & 2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)

#3
head(mtcars)
fit <- lm(mpg ~ I(wt-mean(wt)), mtcars)
summary(fit)
fit$coef[1]-qt(0.975, df=fit$df)*coef(summary(fit))[1,2]

#5
head(mtcars)
fit <- lm(mpg ~ I(wt-3), mtcars)
summary(fit)
fit$coef[1]-qt(0.975, df=fit$df)*coef(summary(fit))[1,2]

#6
head(mtcars)
fit <- lm(mpg ~ I(wt-mean(wt)), mtcars)
summary(fit)
(fit$coef[2]+c(-1,1)*qt(0.975, df=fit$df)*coef(summary(fit))[2,2])*2

fit <- lm(mpg ~ wt -1, mtcars)
summary(fit)
fit <- lm(mpg ~ wt, mtcars)
summary(fit)

# Quiz 3
#1 
summary(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt -1, data=mtcars)
summary(fit)
summary(fit)$coef[1,1]-summary(fit)$coef[3,1]

#2
fit2 <- lm(mpg ~ factor(cyl) -1, data=mtcars)
summary(fit2)
# cylinders have more of an impact because slope without wt as a 
# confounding variable has a larger slope implying that cyl have a
# bigger impact

#3
fit3 <- lm(mpg ~ factor(cyl) + wt, data=mtcars)
fit3a <- lm(mpg ~ factor(cyl)*wt, data=mtcars)
summary(fit3)
summary(fit3a)
library("lmtest")
lmtest::lrtest(fit3, fit3a)

#4
fit4 <- lm(mpg ~ I(wt* 0.5) + factor(cyl), data = mtcars)
summary(fit4)
# estimated expected change per one ton increase in weight

#5 & 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y ~ x)
summary(fit5)
t<-influence.measures(fit5)
# largest hat diagonal:  0.995 and corresponding dfbeta: -134

#7 - must be strong correlation term

# Quiz 4
library(MASS)
table(shuttle$wind, shuttle$use)
auto_wind <- glm(use ~ factor(wind), family="binomial", data=shuttle)
lodds <- predict(auto_wind, c('head', 'tail'))
# 1 - answer 0.969

# 2
auto_windmag <- glm(use ~ factor(wind) + factor(magn), family="binomial", data=shuttle)

# 3

# 6
x <- (-5:5)
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87,4.97)
plot(y, x)
