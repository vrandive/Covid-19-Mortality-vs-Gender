
library(lmtest)
library(sandwich)
library(Hmisc)

load(file = "~/Final_Project.RData")

data <- data.frame(COVID19_line_list_data)
describe(data)

data$death_dummy <- as.integer(data$death != 0)

reg<-lm(death_dummy~gender,data=data)
reg

summary(reg)

vcov = vcovHC(reg, type="HC1")
robust_se <- sqrt(diag(vcov))
robust_se

coeftest(reg, vcov. = vcov)

reg$coefficients[2]- 1.96*robust_se[2]

sum(data$death_dummy) / nrow(data)

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm=TRUE)
mean(alive$age, na.rm=TRUE)

t.test(dead$age, alive$age, alternative="two.sided", conf.level = 0.95)

men = subset(data, gender == "male")
women = subset(data, gender == "female")

mean(men$death_dummy, na.rm=TRUE)
mean(women$death_dummy, na.rm=TRUE)
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.95)





