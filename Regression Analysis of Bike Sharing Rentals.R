#PSTAT 126 Project
#Randy Zhu, 8554123  Brian Fernandez, 4398335
library(MASS)

cnt <- day$cnt
humidity <- day$hum #values are divided to 100
workday <- day$workingday
temperature <- day$temp #temperature = (temperature-tmin)/(tmax-tmin)
atemp <- day$atemp
windspeed <- day$windspeed

#1
plot(cnt~temperature)
fit_t <- lm(cnt~temperature)
summary(fit_t)
#According to the linear model we get cnt=1214.6+6640.7*temperature
#Since p-value for temperature is way smaller than 0.01 we are positive that temperature is a significant predictor
cnthat <- 1214.6+6640.7*temperature #predicted cnt values
res <- cnt-cnthat #residuals
plot(res~cnthat)
qqnorm(cnthat,main="Normal Q-Q plot")
qqline(cnthat)
boxcox(cnt~temperature,lambda=seq(0.5,1,length=10)) #lambda=0.78
fit_boxcox <- lm(cnt^0.78~temperature)
summary(fit_boxcox)
plot(fit_boxcox,which=c(1,2))

#2
mod.full <- lm(cnt~humidity+temperature+workday)
mod.reduced <- lm(cnt~temperature+workday)
anova(mod.reduced,mod.full)

#3
fit.all <- lm(cnt~humidity+temperature+workday)
summary(fit.all)
mod.indicator <- lm(cnt~humidity+temperature)
anova(mod.indicator,mod.full)

#4
mod0 <- lm(cnt~1)
mod.upper <- lm(cnt~humidity+temperature+workday+atemp+windspeed)
step(mod0,scope=list(lower=mod0,upper=mod.upper))
fitted <- 3774+7504*atemp-3167*humidity-4412*windspeed
finalres <- cnt-fitted
plot(finalres~fitted)
qqnorm(fitted,main="Normal Q-Q plot")
qqline(fitted)
boxcox(cnt~atemp+humidity+windspeed,lambda=seq(0.7,0.8,length=10)) #lambda=0.76
optimal <- lm(cnt^0.76~atemp+humidity+windspeed)
summary(optimal)
plot(optimal,which=c(1,2))
mod.full <- lm(cnt~humidity+temperature+workday+atemp+windspeed+atemp*humidity+atemp*windspeed+humidity*windspeed)
anova(mod.upper,mod.full)