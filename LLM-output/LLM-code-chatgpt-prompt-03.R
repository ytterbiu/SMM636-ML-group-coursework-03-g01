# Load libraries
library(tidyverse)
library(factoextra)
library(cluster)

# Load data
data <- read.csv("Wholesale_customers_data.csv")

# Convert categorical variables
data$Channel <- as.factor(data$Channel)
data$Region <- as.factor(data$Region)

# Select continuous variables
spending_data <- data %>%
  select(Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen)

# Log transform to reduce skewness
spending_log <- log1p(spending_data)

# Scale data
spending_scaled <- scale(spending_log)

# -------------------------
# PCA (for dimensionality reduction)
# -------------------------
pca_result <- prcomp(spending_scaled, center = TRUE, scale. = TRUE)

# Extract first few principal components (e.g., first 3)
pca_scores <- as.data.frame(pca_result$x[, 1:3])

# -------------------------
# Determine optimal number of clusters
# -------------------------

# Elbow method
fviz_nbclust(pca_scores, kmeans, method = "wss") +
  ggtitle("Elbow Method")

# Silhouette method
fviz_nbclust(pca_scores, kmeans, method = "silhouette") +
  ggtitle("Silhouette Method")

# -------------------------
# Apply K-means clustering
# -------------------------

set.seed(123) # for reproducibility

k <- 3 # Adjust based on elbow/silhouette results

kmeans_result <- kmeans(pca_scores, centers = k, nstart = 25)

# Add cluster labels to original data
data$Cluster <- as.factor(kmeans_result$cluster)

# -------------------------
# Visualisation
# -------------------------

# Cluster plot
fviz_cluster(
  kmeans_result,
  data = pca_scores,
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
)

# -------------------------
# Cluster profiling
# -------------------------

# Average spending per cluster
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(across(Fresh:Delicassen, mean))

print(cluster_summary)

# Distribution of Channel by cluster
table(data$Cluster, data$Channel)

# Distribution of Region by cluster
table(data$Cluster, data$Region)

# New code below ====

# Compute distance matrix
dist_mat <- dist(pca_scores)

# Hierarchical clustering (Ward's method)
hc <- hclust(dist_mat, method = "ward.D2")

# Plot dendrogram
plot(hc, labels = FALSE, main = "Dendrogram")

# Cut tree into k clusters
k <- 3
hc_clusters <- cutree(hc, k = k)

# Compare with k-means
table(hc_clusters, kmeans_result$cluster)

library(mclust)

gmm_model <- Mclust(pca_scores)

# Best model and number of clusters (BIC-based)
summary(gmm_model)

# Cluster assignments
gmm_clusters <- gmm_model$classification

# Compare with k-means
table(gmm_clusters, kmeans_result$cluster)

library(dbscan)

# kNN distance plot to choose eps
kNNdistplot(pca_scores, k = 5)
abline(h = 1.5, col = "red") # adjust visually

# Run DBSCAN
db <- dbscan(pca_scores, eps = 1.5, minPts = 5)

# Cluster labels
table(db$cluster)

library(fpc)

cluster_stability <- clusterboot(
  pca_scores,
  B = 100,
  clustermethod = kmeansCBI,
  k = 3
)

cluster_stability$bootmean


library(cluster)

sil <- silhouette(kmeans_result$cluster, dist(pca_scores))

# Plot
fviz_silhouette(sil)

# Average silhouette score
mean(sil[, 3])

# K-means on original scaled data
set.seed(123)
kmeans_raw <- kmeans(spending_scaled, centers = 3, nstart = 25)

# Compare assignments
table(kmeans_raw$cluster, kmeans_result$cluster)

# Try multiple k values
k_values <- 2:6

for (k in k_values) {
  km <- kmeans(pca_scores, centers = k, nstart = 25)
  cat("k =", k, " | Tot.withinss =", km$tot.withinss, "\n")
}
