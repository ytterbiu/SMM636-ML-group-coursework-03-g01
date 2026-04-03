# Load required libraries
library(tidyverse)
library(factoextra)

# Load the dataset
data <- read.csv("Wholesale_customers_data.csv")

# Inspect structure
str(data)
summary(data)

# Convert categorical variables to factors
data$Channel <- as.factor(data$Channel)
data$Region <- as.factor(data$Region)

# Select only continuous variables for PCA
spending_data <- data %>%
  select(Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen)

# Optional: Log transformation to reduce skewness
spending_log <- log1p(spending_data) # log(1 + x) handles zeros

# Scale the data (important for PCA)
spending_scaled <- scale(spending_log)

# Perform PCA
pca_result <- prcomp(spending_scaled, center = TRUE, scale. = TRUE)

# View summary of PCA (variance explained)
summary(pca_result)

# Eigenvalues (variance explained by each component)
eig_vals <- get_eigenvalue(pca_result)
print(eig_vals)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60))

# Biplot (individuals + variables)
fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue", col.ind = "gray")

# PCA variable contributions
fviz_pca_var(
  pca_result,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red")
)

# PCA individuals colored by Channel
fviz_pca_ind(
  pca_result,
  geom = "point",
  col.ind = data$Channel,
  palette = c("#00AFBB", "#E7B800"),
  addEllipses = TRUE,
  legend.title = "Channel"
)
