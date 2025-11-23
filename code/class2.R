######
plot(swiss$Catholic, swiss$Examination)

data_do_klastrow <- data.frame(swiss$Catholic,swiss$Examination)
set.seed(123)
results_k2 <- kmeans(data_do_klastrow, 2)
results_k2$tot.withinss
results_k4 <- kmeans(data_do_klastrow, 4)
results_k4$tot.withinss
print(results_k2)

plot(swiss$Catholic, swiss$Examination,
     lwd = 2,
     col = results_k2$cluster,
     main = "Wynik K-means (k=2)",
     xlab = "% Katolikow",
     ylab = "Wyniki egzaminow")
results_3 <- kmeans(data_do_klastrow, 3)
results_3$tot.withinss
results_1 <- kmeans(data_do_klastrow, 1)
results_1$tot.withinss
results_5 <- kmeans(data_do_klastrow, 5)
results_5$tot.withinss
plot(swiss$Catholic,swiss$Examination, lwd=3, col = results$cluster)
grupy <- 1:5
Total <- c(results_1$tot.withinss, results_k2$tot.withinss, results_3$tot.withinss, results_k4$tot.withinss, results_5$tot.withinss)

plot(grupy, Total, type="b", lwd=3, col="darkblue", xlab="Liczba klastrow", ylab="Suma kwadratow wewnatrz klastrow")

twinss <- c()
for(k in 1:7) {
  results <- kmeans(data.frame(swiss$Catholic,swiss$Examination),k)
  twinss <- append(twinss, results$tot.withinss)
}
print(twinss)
grupy <- 1:7
plot(grupy, twinss, type="b", lwd=3, col="darkblue", xlab="Liczba klastrow", ylab="Suma kwadratow wewnatrz klastrow")


twinss <- c()
grupy <- 1:14
for(k in 1:14) {
  results <- kmeans(data.frame(swiss$Catholic,swiss$Examination, swiss$Fertility),k)
  twinss <- append(twinss, results$tot.withinss)
}
plot(grupy, twinss, type="b", lwd=3, col="darkblue", xlab="Liczba klastrow", ylab="Suma kwadratow wewnatrz klastrow")

results <- kmeans(data.frame(swiss$Catholic,swiss$Examination, swiss$Fertility),2)
# Plot w 3D
install.packages("car")
install.packages("rgl")
install.packages("scatterplot3d")
library(car)
library(rgl)
library(scatterplot3d)

scatterplot3d(swiss$Catholic,swiss$Examination, swiss$Fertility,
              pch=16,
              color=results$cluster,
              main="K-means z 3 zmiennymi (k=2)",
              xlab="% Katolikow",
              ylab="Wyniki egzaminow",
              zlab="Plodnosc")
scatter3d(swiss$Catholic,swiss$Examination, swiss$Fertility,
          pch=16,
          color=results$cluster,
          surface=FALSE,
          main="K-means z 3 zmiennymi (k=2)",
          xlab="% Katolikow",
          ylab="Wyniki egzaminow",
          zlab="Plodnosc")
#Can I have coloring for points based on groups
