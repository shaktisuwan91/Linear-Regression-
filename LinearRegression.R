## Read Consumer data
setwd("F:/0_1_Trainings/Institutes/TainingMatreial/3_Machine Learning/Regression/Datasets")
library(psych)
library(car)
Consumer = read.csv("Consumer2.csv",header=TRUE)
Consumer
## Summary Statistics
describe(Consumer$Income)
describe(Consumer$AmountCharged)

#Consumer2 = read.csv("Consumer2.csv",header=TRUE)
#Consumer2
#describe(Consumer2$ AmountCharged, na.rm = FALSE)
#summary(Consumer)
#describe(Consumer)


cor(Consumer)

with(Consumer, boxplot(Income, main="Income (1000) US$"))
with(Consumer, boxplot(HouseholdSize, main="Household Size"))
with(Consumer, boxplot(AmountCharged, main="Amount Charged US$"))

with(Consumer, plot(HouseholdSize, AmountCharged, pch=19, cex=0.6))
with(Consumer, plot(Income, AmountCharged, pch=19, cex=0.6))

## Normality Test
with(Consumer, shapiro.test(Income))
with(Consumer, qqnorm(Income, pch=19, cex=0.6))
with(Consumer, qqline(Income, col='red'))

## Simple Regressions
reg1 <- lm(AmountCharged ~ Income, data=Consumer)
reg1
summary(reg1)
anova(reg1)

reg2 <- lm(AmountCharged ~ HouseholdSize, data=Consumer)
reg2
summary(reg2)
anova(reg2)

## Multiple Regression
reg3 <- lm(AmountCharged ~ Income + HouseholdSize, data=Consumer)
reg3
summary(reg3)
anova(reg3)

fit3 <- fitted(reg3)
res3 <- residuals(reg3)

abline(reg1, col='red')
abline(reg2, col='red')

ConsumerReg <- cbind(Consumer, fit3, res3)

## Prediction of new observations
newobs <- data.frame(Income = 40, HouseholdSize = 2)
predict.lm(reg3, newdata=newobs)

newobs <- data.frame(Income = c(40,50), HouseholdSize = c(3, 4))
predict.lm(reg3, newdata=newobs)
