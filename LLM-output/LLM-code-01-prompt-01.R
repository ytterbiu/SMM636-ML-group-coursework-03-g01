# =============================================================================
# PCA Analysis: Wholesale Customers Dataset
# Goal: Customer segmentation to inform inventory planning, pricing strategy,
#       and sales team deployment
# =============================================================================

# --- 0. Install & Load Libraries ---------------------------------------------

required_packages <- c(
  "tidyverse",
  "corrplot",
  "factoextra",
  "FactoMineR",
  "gridExtra",
  "scales",
  "ggrepel",
  "psych"
)

installed <- rownames(installed.packages())
to_install <- required_packages[!required_packages %in% installed]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(scales)
library(ggrepel)
library(psych)

# --- 1. Load Data ------------------------------------------------------------

data_path <- "Wholesale_customers_data.csv"

df_raw <- read.csv(data_path, stringsAsFactors = FALSE)

cat("=== Dataset Overview ===\n")
cat("Dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n\n")
print(head(df_raw))
cat("\n")
print(summary(df_raw))

# --- 2. Pre-processing -------------------------------------------------------

# 2.1  Recode categorical variables as factors (kept aside from PCA)
df <- df_raw %>%
  mutate(
    Channel = factor(Channel, levels = c(1, 2), labels = c("HoReCa", "Retail")),
    Region = factor(
      Region,
      levels = c(1, 2, 3),
      labels = c("Lisbon", "Oporto", "Other")
    )
  )

# 2.2  Isolate the six continuous spending variables for PCA
spend_vars <- c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)
df_spend <- df[, spend_vars]

# 2.3  Check for missing values
cat("\n=== Missing Values per Column ===\n")
print(colSums(is.na(df_spend)))

# 2.4  Check for outliers via boxplots (raw scale)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (v in spend_vars) {
  boxplot(
    df_spend[[v]],
    main = v,
    ylab = "Annual Spend (m.u.)",
    col = "#4E79A7",
    border = "#2C4770",
    outline = TRUE
  )
}
mtext(
  "Raw Spending Distributions – Outlier Inspection",
  side = 3,
  line = -2,
  outer = TRUE,
  cex = 1.1,
  font = 2
)
par(mfrow = c(1, 1))

# 2.5  Assess skewness – highly skewed data warrants log transformation
cat("\n=== Skewness of Raw Spending Variables ===\n")
skew_raw <- apply(df_spend, 2, function(x) psych::skew(x))
print(round(skew_raw, 3))

# 2.6  Log-transform to reduce right skew (common for monetary spend data)
#      Adding 1 to guard against any zero values
df_log <- log1p(df_spend)

cat("\n=== Skewness After log1p Transformation ===\n")
skew_log <- apply(df_log, 2, function(x) psych::skew(x))
print(round(skew_log, 3))

# 2.7  Visualise distributions: raw vs log-transformed
p_list <- list()
for (v in spend_vars) {
  p_raw <- ggplot(df_spend, aes_string(x = v)) +
    geom_histogram(fill = "#4E79A7", colour = "white", bins = 30) +
    labs(title = paste(v, "(raw)"), x = NULL, y = "Count") +
    theme_minimal(base_size = 9)

  p_log <- ggplot(df_log, aes_string(x = v)) +
    geom_histogram(fill = "#F28E2B", colour = "white", bins = 30) +
    labs(title = paste(v, "(log1p)"), x = NULL, y = "Count") +
    theme_minimal(base_size = 9)

  p_list <- c(p_list, list(p_raw, p_log))
}
grid.arrange(
  grobs = p_list,
  ncol = 4,
  top = "Distribution: Raw vs log1p-transformed Spending"
)

# 2.8  Correlation matrix on log-transformed data
cat("\n=== Pearson Correlation Matrix (log-transformed) ===\n")
cor_mat <- cor(df_log, method = "pearson")
print(round(cor_mat, 3))

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.75,
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  title = "Correlation Matrix – log1p Spending Variables",
  mar = c(0, 0, 2, 0)
)

# 2.9  KMO & Bartlett's test – check suitability for PCA
cat("\n=== KMO Measure of Sampling Adequacy ===\n")
kmo_result <- KMO(cor_mat)
print(kmo_result)

cat("\n=== Bartlett's Test of Sphericity ===\n")
bart_result <- cortest.bartlett(cor_mat, n = nrow(df_log))
print(bart_result)

# --- 3. Principal Component Analysis ----------------------------------------

# prcomp with scale. = TRUE standardises the log-transformed variables
# (zero mean, unit variance) – essential so no single variable dominates
pca_result <- prcomp(df_log, center = TRUE, scale. = TRUE)

cat("\n=== PCA Summary ===\n")
print(summary(pca_result))

cat("\n=== Loadings (Rotation Matrix) ===\n")
print(round(pca_result$rotation, 4))

# --- 4. Variance Explained ---------------------------------------------------

# 4.1  Eigenvalues
eig_vals <- pca_result$sdev^2
prop_var <- eig_vals / sum(eig_vals)
cum_var <- cumsum(prop_var)

variance_table <- data.frame(
  PC = paste0("PC", seq_along(eig_vals)),
  Eigenvalue = round(eig_vals, 4),
  Proportion_Var = round(prop_var, 4),
  Cumulative_Var = round(cum_var, 4)
)
cat("\n=== Variance Explained Table ===\n")
print(variance_table)

# 4.2  Scree plot with Kaiser criterion line (eigenvalue = 1)
fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim = c(0, 55),
  barfill = "#4E79A7",
  barcolor = "#2C4770",
  linecolor = "#E15759",
  ggtheme = theme_minimal()
) +
  geom_hline(
    yintercept = 100 / length(spend_vars), # Kaiser: eigenvalue = 1
    linetype = "dashed",
    colour = "red",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 5.5,
    y = 100 / length(spend_vars) + 1.5,
    label = "Kaiser criterion (eigenvalue = 1)",
    colour = "red",
    size = 3.5
  ) +
  labs(
    title = "Scree Plot – Wholesale Customers PCA",
    subtitle = "Dashed line = Kaiser criterion"
  )

# 4.3  Cumulative variance plot
ggplot(variance_table, aes(x = PC, y = Cumulative_Var * 100, group = 1)) +
  geom_line(colour = "#E15759", linewidth = 1.2) +
  geom_point(size = 3, colour = "#E15759") +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "gray40") +
  geom_hline(yintercept = 90, linetype = "dotted", colour = "gray40") +
  annotate(
    "text",
    x = 5.6,
    y = 81,
    label = "80%",
    size = 3.5,
    colour = "gray30"
  ) +
  annotate(
    "text",
    x = 5.6,
    y = 91,
    label = "90%",
    size = 3.5,
    colour = "gray30"
  ) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cumulative Variance Explained by PCs",
    subtitle = "Dashed = 80%, Dotted = 90% thresholds",
    x = "Principal Component",
    y = "Cumulative Variance (%)"
  ) +
  theme_minimal(base_size = 12)

# --- 5. Visualise Loadings ---------------------------------------------------

# 5.1  Loading heatmap for selected PCs
n_pcs <- sum(eig_vals >= 1) # Kaiser rule
loadings_df <- as.data.frame(pca_result$rotation[, 1:n_pcs]) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(-Variable, names_to = "PC", values_to = "Loading")

ggplot(loadings_df, aes(x = PC, y = Variable, fill = Loading)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Loading, 2)), size = 3.5, colour = "black") +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "PCA Loading Heatmap",
    subtitle = paste("First", n_pcs, "PCs (Kaiser criterion: eigenvalue ≥ 1)"),
    x = NULL,
    y = NULL,
    fill = "Loading"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(face = "bold"))

# 5.2  Variable contribution plot (cos²)
fviz_cos2(
  pca_result,
  choice = "var",
  axes = 1:2,
  fill = "#4E79A7",
  color = "#2C4770",
  ggtheme = theme_minimal()
) +
  labs(
    title = "Variable cos² on PC1 & PC2",
    subtitle = "Quality of representation on the first two components"
  )

# --- 6. Biplots & Score Plots ------------------------------------------------

# 6.1  Biplot – coloured by Channel
scores_df <- as.data.frame(pca_result$x) %>%
  mutate(Channel = df$Channel, Region = df$Region)

# Build arrow data for loadings overlay
arrows_df <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("Variable")
scale_factor <- 3.5 # scale arrows to fit score space

ggplot(scores_df, aes(x = PC1, y = PC2, colour = Channel, shape = Channel)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_segment(
    data = arrows_df,
    aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "black",
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_label_repel(
    data = arrows_df,
    aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Variable),
    size = 3.5,
    colour = "black",
    box.padding = 0.4,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = c("HoReCa" = "#E15759", "Retail" = "#4E79A7")) +
  labs(
    title = "PCA Biplot – PC1 vs PC2",
    subtitle = "Coloured by Channel (HoReCa vs Retail)",
    x = paste0("PC1 (", round(prop_var[1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(prop_var[2] * 100, 1), "% variance)"),
    colour = "Channel",
    shape = "Channel"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# 6.2  Biplot – coloured by Region
ggplot(scores_df, aes(x = PC1, y = PC2, colour = Region, shape = Region)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_segment(
    data = arrows_df,
    aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "black",
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_label_repel(
    data = arrows_df,
    aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Variable),
    size = 3.5,
    colour = "black",
    box.padding = 0.4,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c("Lisbon" = "#59A14F", "Oporto" = "#F28E2B", "Other" = "#B07AA1")
  ) +
  labs(
    title = "PCA Biplot – PC1 vs PC2",
    subtitle = "Coloured by Region",
    x = paste0("PC1 (", round(prop_var[1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(prop_var[2] * 100, 1), "% variance)"),
    colour = "Region",
    shape = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# 6.3  PC1 vs PC3 (if relevant)
if (n_pcs >= 3) {
  ggplot(scores_df, aes(x = PC1, y = PC3, colour = Channel, shape = Channel)) +
    geom_point(alpha = 0.7, size = 2.5) +
    scale_colour_manual(
      values = c("HoReCa" = "#E15759", "Retail" = "#4E79A7")
    ) +
    labs(
      title = "PCA Score Plot – PC1 vs PC3",
      x = paste0("PC1 (", round(prop_var[1] * 100, 1), "%)"),
      y = paste0("PC3 (", round(prop_var[3] * 100, 1), "%)")
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# --- 7. Extract PC Scores for Downstream Use ---------------------------------

# Attach PC scores back to full dataset
df_with_scores <- bind_cols(df, as.data.frame(pca_result$x))

cat("\n=== PC Scores – First 6 Rows ===\n")
print(head(df_with_scores[, c("Channel", "Region", "PC1", "PC2", "PC3")]))

# Save enriched dataset
output_path <- "wholesale_pca_scores.csv"
write.csv(df_with_scores, output_path, row.names = FALSE)
cat("\nPC scores saved to:", output_path, "\n")

# --- 8. Interpretation Summary -----------------------------------------------

cat("\n")
cat("========================================================\n")
cat("              PCA INTERPRETATION SUMMARY                \n")
cat("========================================================\n")
cat(sprintf("  Components retained (Kaiser, eigenvalue ≥ 1): %d\n", n_pcs))
cat(sprintf(
  "  Variance explained by retained PCs:           %.1f%%\n",
  cum_var[n_pcs] * 100
))
cat("\n  PC1 – 'Overall Spend Scale'\n")
cat("    Likely captures total spending power / business size.\n")
cat("    High positive loadings across most categories.\n")
cat("\n  PC2 – 'Fresh vs Packaged Goods Axis'\n")
cat("    Separates Fresh-heavy buyers (HoReCa) from\n")
cat("    Grocery/Detergent-heavy buyers (Retail).\n")
cat("\n  PC3 – 'Speciality/Frozen Dimension' (if retained)\n")
cat("    May differentiate niche buyers (Delicassen, Frozen).\n")
cat("\n  Business Implication:\n")
cat("    PC scores can directly feed K-Means / Hierarchical\n")
cat("    clustering for actionable customer segmentation.\n")
cat("========================================================\n")
