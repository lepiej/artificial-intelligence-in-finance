attach(swiss)
Fertility
summary(Fertility)
hist(Fertility)
hist(Fertility, col = "red")
library(Hmisc)
describe(swiss) 
hist(Fertility, col = rainbow(20), probability = T)
hist(Catholic, col = rainbow(30), probability = T)
lines(density(Fertility), lwd=3)
mean(Fertility)
sd(Fertility)

hist(Catholic, col = rainbow(20), probability = T)
lines(density(Catholic), lwd=3)

x <- Agriculture
hist(x, col = rainbow(20), probability = T)
lines(density(x), lwd=3)
sd(x)

install.packages("e1071")
library(e1071)
kurtosis(Fertility)
skewness(Fertility)

summary(swiss)


plot(swiss)
plot(Catholic, Fertility, lwd=2)

results <- kmeans( data.frame(Catholic, Fertility), 2)
results
results$cluster
results$centers
results$tot.withinss


results <- kmeans( data.frame(Catholic, Fertility), 4)
results$tot.withinss
plot(Catholic, Fertility, lwd=2, col=results$cluster)
