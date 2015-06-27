sleep <- read.csv("sleepdata.csv", sep=";")

# make sleep quality column not a factor anymore
sleep[,3] <- as.character(sleep[,3])

# remove % symbol from data
sleep[,3] <- sub("%", "", sleep[,3])

# remove outlier steps data point
sleep <- sleep[-81,]

# fit linear model
steps <- sleep[,8]
quality <- as.numeric(sleep[,3])

fit <- lm(quality ~ steps)
B0 <- coef(fit)[1]  # intercept
B1 <- coef(fit)[2]  # slope

summary(fit)

# plot % sleep quality vs number of steps
plot(steps, quality, xlab="steps", ylab="% sleep quality",
     main="Effect of activity on \n sleep quality ", pch=16)
# add regression line
abline(a=B0, b=B1)

# test for correlation
cor.test(quality, steps)