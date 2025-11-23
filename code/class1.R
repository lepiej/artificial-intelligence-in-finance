# Load the built-in socio-economic dataset "swiss"
attach(swiss)                   

#Explore a single variable
Fertility                        # prints the vector to the console
summary(Fertility)               # min/1Q/median/mean/3Q/max — quick distribution snapshot

hist(Fertility)                  # basic histogram: shape, spread, outliers
hist(Fertility, col = "red")     # same histogram with color (purely cosmetic)

# Density overlays help see distribution shape beyond binning
hist(Fertility, col = rainbow(40), probability = TRUE)  # probability=TRUE → density scale
hist(Catholic,  col = rainbow(30), probability = TRUE)  # same for "Catholic"
lines(density(Fertility), lwd = 3)                       # smooth density curve over Fertility histogram

mean(Fertility)                  # central tendency
sd(Fertility)                    # dispersion (standard deviation)

# Repeat for Catholic with density overlay
hist(Catholic, col = rainbow(30), probability = TRUE)
lines(density(Catholic), lwd = 3)

# Another variable: Agriculture
x <- Agriculture
hist(x, col = rainbow(20), probability = TRUE)
lines(density(x), lwd = 3)
sd(x)

# ---- Shape diagnostics: skewness/kurtosis ----
install.packages("e1071")       # install once; comment out after first run
library(e1071)
kurtosis(Fertility)              # tail heaviness vs normal (0 = normal for Fisher definition; here e1071 uses type=3)
skewness(Fertility)              # asymmetry (>0 right-skewed, <0 left-skewed)

summary(swiss)                   # summary for all variables

# ---- Bivariate relationships ----
plot(swiss)                      # scatterplot matrix for all pairs — look for linear/nonlinear patterns, clusters
plot(Catholic, Fertility, lwd=2) # single scatter: how Fertility relates to Catholic

# ---- UNSUPERVISED LEARNING: k-means clustering ----
# We cluster observations using only features (no labels) → classic unsupervised learning.
# Here we use 2D space: Catholic (x) and Fertility (y).

results <- kmeans(data.frame(Catholic, Fertility), centers = 2)  # fit 2 clusters
results                         # prints clustering summary
results$cluster                 # vector of cluster assignments (1..k) per province
results$centers                 # centroid coordinates (mean Catholic, mean Fertility for each cluster)
results$tot.withinss            # total within-cluster sum of squares (compactness measure; lower is better)

# Try more clusters to compare compactness (for elbow heuristic)
results <- kmeans(data.frame(Catholic, Fertility), centers = 4)
results$tot.withinss            # compare with k=2; plot tot.withinss for k=1..10 for elbow

# Visualize clusters in feature space; color by assigned cluster
plot(Catholic, Fertility, lwd=2, col = results$cluster)
# Centroids could be added too:
# points(results$centers, pch = 8, cex = 2, lwd = 2)
