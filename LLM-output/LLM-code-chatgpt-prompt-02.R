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
