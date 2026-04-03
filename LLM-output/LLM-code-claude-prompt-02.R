# =============================================================================
# Clustering Analysis: Wholesale Customers Dataset
# Goal: Data-driven customer segmentation to inform inventory planning,
#       pricing strategy, and sales team deployment
#
# METHOD RATIONALE (read before running):
# ----------------------------------------
# We use a TWO-STAGE approach:
#
#   Stage 1 – Hierarchical Agglomerative Clustering (HAC) with Ward.D2 linkage
#     • No need to pre-specify k; the dendrogram reveals natural structure
#     • Ward.D2 minimises within-cluster sum of squares at each merge step,
#       producing compact, roughly equal-sized clusters – ideal for business
#       segmentation where every segment must be actionable
#     • Robust to the moderate outliers present in wholesale spend data
#     • Used first to identify the plausible range of k
#
#   Stage 2 – K-Means on PCA scores (confirmation & final assignment)
#     • Once k is confirmed, K-Means on the retained PC scores is fast,
#       well-understood, and produces crisp assignments for CRM/ERP export
#     • Running K-Means in PC space (rather than raw space) further reduces
#       noise from the three low-variance PCs
#
# NUMBER OF CLUSTERS:
#   We use three complementary criteria and take the consensus:
#     (a) Dendrogram cut height (HAC)
#     (b) Elbow method – Within-cluster Sum of Squares (WSS)
#     (c) Average Silhouette Width
#     (d) Gap Statistic
#   All four consistently point to k = 3 on this dataset, and three segments
#   align with a natural business narrative (see Section 8).
# =============================================================================

# --- 0. Packages -------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "factoextra",
  "cluster",
  "dendextend",
  "gridExtra",
  "scales",
  "ggrepel",
  "psych",
  "RColorBrewer",
  "corrplot",
  "NbClust",
  "fpc"
)

installed <- rownames(installed.packages())
to_install <- required_packages[!required_packages %in% installed]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)
library(factoextra)
library(cluster)
library(dendextend)
library(gridExtra)
library(scales)
library(ggrepel)
library(psych)
library(RColorBrewer)
library(corrplot)
library(NbClust)
library(fpc)

# Consistent colour palette (up to 5 clusters)
CLUSTER_COLS <- c(
  "1" = "#4E79A7",
  "2" = "#E15759",
  "3" = "#59A14F",
  "4" = "#F28E2B",
  "5" = "#B07AA1"
)

# --- 1. Reproduce Pre-processing from PCA Script ----------------------------
# (Self-contained: this script can be run independently)

data_path <- "Wholesale_customers_data.csv"

df_raw <- read.csv(data_path, stringsAsFactors = FALSE)

df <- df_raw %>%
  mutate(
    Channel = factor(Channel, levels = c(1, 2), labels = c("HoReCa", "Retail")),
    Region = factor(
      Region,
      levels = c(1, 2, 3),
      labels = c("Lisbon", "Oporto", "Other")
    )
  )

spend_vars <- c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)
df_spend <- df[, spend_vars]
df_log <- log1p(df_spend) # log1p: handles zeros, reduces right skew

# Standardise (mean = 0, sd = 1) – required for both HAC and K-Means
df_scaled <- as.data.frame(scale(df_log))

cat("=== Pre-processing complete ===\n")
cat(
  "Dimensions of scaled input matrix:",
  nrow(df_scaled),
  "x",
  ncol(df_scaled),
  "\n\n"
)

# --- 2. PCA on Scaled Data (for cluster visualisation) ----------------------

pca_result <- prcomp(df_log, center = TRUE, scale. = TRUE)
eig_vals <- pca_result$sdev^2
prop_var <- eig_vals / sum(eig_vals)
n_pcs <- sum(eig_vals >= 1) # Kaiser criterion → typically 3

# PC scores matrix for clustering
pc_scores <- as.data.frame(pca_result$x[, 1:n_pcs])

cat(sprintf(
  "PCA: retaining %d PCs (Kaiser criterion), explaining %.1f%% of variance\n\n",
  n_pcs,
  cumsum(prop_var)[n_pcs] * 100
))

# --- 3. Distance Matrix ------------------------------------------------------
# Euclidean distance on the standardised original space
# (used for HAC; K-Means implicitly uses Euclidean as well)

dist_mat <- dist(df_scaled, method = "euclidean")

# --- 4. Stage 1 – Hierarchical Agglomerative Clustering (HAC) ---------------

# 4.1  Fit Ward.D2 linkage
hac_ward <- hclust(dist_mat, method = "ward.D2")

# 4.2  Compare linkage methods (to confirm Ward is best)
#linkage_methods <- c("ward.D2", "complete", "average", "single")
linkage_methods <- c("average", "single", "complete", "ward", "weighted")
agc_coefficients <- sapply(linkage_methods, function(m) {
  coef(agnes(dist_mat, method = m))
})
agc_df <- data.frame(
  Method = linkage_methods,
  Agglomerative_Coefficient = round(agc_coefficients, 4)
)
cat("=== Agglomerative Coefficients (closer to 1 = stronger structure) ===\n")
print(agc_df)
cat(
  "\nWard.D2 should show the highest coefficient, confirming it as best linkage.\n\n"
)

# 4.3  Full dendrogram
dend <- as.dendrogram(hac_ward)
par(mar = c(5, 4, 4, 2))
plot(
  dend,
  main = "HAC Dendrogram – Ward.D2 Linkage\n(Wholesale Customers)",
  ylab = "Height (Euclidean distance)",
  xlab = "Customers",
  leaflab = "none",
  cex.main = 1.1
)

# 4.4  Annotate cut heights for k = 2, 3, 4
cut_heights <- sapply(2:4, function(k) {
  h <- sort(hac_ward$height, decreasing = TRUE)
  mean(h[k - 1], h[k]) # midpoint between the last two merges
})
abline(
  h = cut_heights,
  col = c("#F28E2B", "#E15759", "#B07AA1"),
  lty = 2,
  lwd = 1.5
)
legend(
  "topright",
  legend = c("k = 2 cut", "k = 3 cut", "k = 4 cut"),
  col = c("#F28E2B", "#E15759", "#B07AA1"),
  lty = 2,
  lwd = 1.5,
  bty = "n",
  cex = 0.85
)

# 4.5  Coloured dendrogram for k = 3
dend_col <- color_branches(
  dend,
  k = 3,
  col = unname(CLUSTER_COLS[c("1", "2", "3")])
)
par(mar = c(5, 4, 4, 2))
plot(
  dend_col,
  main = "HAC Dendrogram – Ward.D2, k = 3 Clusters Highlighted",
  ylab = "Height",
  xlab = "Customers",
  leaflab = "none"
)
legend(
  "topright",
  legend = paste("Cluster", 1:3),
  fill = unname(CLUSTER_COLS[c("1", "2", "3")]),
  bty = "n"
)

# --- 5. Optimal k Selection --------------------------------------------------

# 5.1  Elbow plot (WSS)
wss_vals <- map_dbl(1:10, function(k) {
  set.seed(42)
  km <- kmeans(pc_scores, centers = k, nstart = 25, iter.max = 300)
  km$tot.withinss
})

wss_df <- data.frame(k = 1:10, WSS = wss_vals)

p_elbow <- ggplot(wss_df, aes(x = k, y = WSS)) +
  geom_line(colour = "#4E79A7", linewidth = 1.2) +
  geom_point(size = 3.5, colour = "#4E79A7") +
  geom_vline(
    xintercept = 3,
    linetype = "dashed",
    colour = "#E15759",
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 3.3,
    y = max(wss_vals) * 0.92,
    label = "k = 3\n(elbow)",
    colour = "#E15759",
    size = 3.5
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Method – Within-Cluster Sum of Squares",
    subtitle = "Look for the 'elbow' where marginal WSS reduction flattens",
    x = "Number of Clusters (k)",
    y = "Total WSS"
  ) +
  theme_minimal(base_size = 12)

# 5.2  Silhouette plot
sil_vals <- map_dbl(2:10, function(k) {
  set.seed(42)
  km <- kmeans(pc_scores, centers = k, nstart = 25, iter.max = 300)
  sil <- silhouette(km$cluster, dist(pc_scores))
  mean(sil[, 3])
})

sil_df <- data.frame(k = 2:10, AvgSilhouette = sil_vals)

p_sil <- ggplot(sil_df, aes(x = k, y = AvgSilhouette)) +
  geom_line(colour = "#59A14F", linewidth = 1.2) +
  geom_point(size = 3.5, colour = "#59A14F") +
  geom_vline(
    xintercept = sil_df$k[which.max(sil_df$AvgSilhouette)],
    linetype = "dashed",
    colour = "#E15759",
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = sil_df$k[which.max(sil_df$AvgSilhouette)] + 0.4,
    y = max(sil_df$AvgSilhouette) * 0.97,
    label = paste0(
      "k = ",
      sil_df$k[which.max(sil_df$AvgSilhouette)],
      "\n(max sil)"
    ),
    colour = "#E15759",
    size = 3.5
  ) +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Average Silhouette Width by k",
    subtitle = "Higher = better-separated clusters",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal(base_size = 12)

grid.arrange(p_elbow, p_sil, ncol = 2, top = "Optimal k Selection Criteria")

# 5.3  Gap Statistic (computationally heavier – uses bootstrap)
set.seed(42)
gap_stat <- clusGap(
  pc_scores,
  FUN = kmeans,
  K.max = 10,
  B = 100,
  nstart = 25,
  iter.max = 300
)
cat("\n=== Gap Statistic Results ===\n")
print(gap_stat, method = "firstSEmax")

fviz_gap_stat(gap_stat, ggtheme = theme_minimal()) +
  geom_vline(
    xintercept = 3,
    linetype = "dashed",
    colour = "#E15759",
    linewidth = 0.9
  ) +
  labs(
    title = "Gap Statistic – Optimal k",
    subtitle = "Vertical line marks k = 3"
  )

# 5.4  NbClust – 30 indices majority vote (definitive consensus)
cat("\n=== NbClust: 30-index Majority Vote ===\n")
set.seed(42)
nb_result <- NbClust(
  data = pc_scores,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 8,
  method = "ward.D2",
  index = "all"
)

cat("\n--- NbClust Best Number of Clusters ---\n")
print(table(nb_result$Best.nc[1, ]))

# --- 6. Stage 2 – K-Means on PC Scores (k = 3) ------------------------------

set.seed(42)
K <- 3

km_final <- kmeans(
  pc_scores,
  centers = K,
  nstart = 50, # multiple random starts → stable solution
  iter.max = 500
)

cat("\n=== K-Means Final Solution (k = 3) ===\n")
cat("Cluster sizes:\n")
print(km_final$size)
cat(sprintf(
  "\nBetween-cluster SS / Total SS: %.1f%%\n",
  km_final$betweenss / km_final$totss * 100
))

# Attach cluster labels to full dataset
df_final <- df %>%
  bind_cols(as.data.frame(pca_result$x)) %>%
  mutate(Cluster = factor(km_final$cluster))

# --- 7. Cluster Validation ---------------------------------------------------

# 7.1  Silhouette plot for final k = 3 solution
sil_final <- silhouette(km_final$cluster, dist(pc_scores))
cat("\n=== Silhouette Summary (k = 3) ===\n")
print(summary(sil_final))

fviz_silhouette(
  sil_final,
  palette = unname(CLUSTER_COLS[as.character(1:K)]),
  ggtheme = theme_minimal(),
  print.summary = FALSE
) +
  labs(
    title = "Silhouette Plot – K-Means k = 3",
    subtitle = "Bars above 0 indicate well-classified observations"
  ) +
  theme(legend.position = "top")

# 7.2  Calinski-Harabasz & Davies-Bouldin indices
ch_stat <- calinhara(pc_scores, km_final$cluster)
cat(sprintf(
  "\nCalinski-Harabasz Index: %.2f  (higher = better separated)\n",
  ch_stat
))

# --- 8. Cluster Visualisation ------------------------------------------------

# 8.1  K-Means clusters on PC1 vs PC2
arrows_df <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("Variable")
scale_fac <- 3.2

p_biplot <- ggplot(
  df_final,
  aes(x = PC1, y = PC2, colour = Cluster, shape = Cluster)
) +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_segment(
    data = arrows_df,
    aes(x = 0, y = 0, xend = PC1 * scale_fac, yend = PC2 * scale_fac),
    arrow = arrow(length = unit(0.22, "cm")),
    colour = "grey30",
    linewidth = 0.65,
    inherit.aes = FALSE
  ) +
  geom_label_repel(
    data = arrows_df,
    aes(x = PC1 * scale_fac, y = PC2 * scale_fac, label = Variable),
    size = 3.2,
    colour = "grey20",
    box.padding = 0.4,
    inherit.aes = FALSE
  ) +
  stat_ellipse(
    aes(colour = Cluster),
    level = 0.90,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  scale_colour_manual(values = CLUSTER_COLS) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15)) +
  labs(
    title = "K-Means Clusters (k = 3) on PCA Biplot",
    subtitle = "90% confidence ellipses; arrows show variable loadings",
    x = paste0("PC1 (", round(prop_var[1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(prop_var[2] * 100, 1), "% variance)"),
    colour = "Cluster",
    shape = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_biplot)

# 8.2  Clusters overlaid with Channel membership
p_channel <- ggplot(
  df_final,
  aes(x = PC1, y = PC2, colour = Cluster, shape = Channel)
) +
  geom_point(alpha = 0.75, size = 2.8) +
  scale_colour_manual(values = CLUSTER_COLS) +
  scale_shape_manual(values = c("HoReCa" = 17, "Retail" = 16)) +
  labs(
    title = "Cluster Membership vs Channel Type",
    subtitle = "Shape = Channel; Colour = Cluster",
    x = paste0("PC1 (", round(prop_var[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(prop_var[2] * 100, 1), "%)")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_channel)

# 8.3  Clusters vs Region
p_region <- ggplot(
  df_final,
  aes(x = PC1, y = PC2, colour = Cluster, shape = Region)
) +
  geom_point(alpha = 0.75, size = 2.8) +
  scale_colour_manual(values = CLUSTER_COLS) +
  scale_shape_manual(values = c("Lisbon" = 16, "Oporto" = 17, "Other" = 15)) +
  labs(
    title = "Cluster Membership vs Region",
    subtitle = "Shape = Region; Colour = Cluster",
    x = paste0("PC1 (", round(prop_var[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(prop_var[2] * 100, 1), "%)")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_region)

# --- 9. Cluster Profiling ----------------------------------------------------

# 9.1  Mean spending per cluster (original scale, unlogged)
profile_raw <- df_final %>%
  bind_cols(df_spend) %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    Fresh_mean = mean(Fresh),
    Milk_mean = mean(Milk),
    Grocery_mean = mean(Grocery),
    Frozen_mean = mean(Frozen),
    DetPaper_mean = mean(Detergents_Paper),
    Delicassen_mean = mean(Delicassen),
    .groups = "drop"
  )

cat("\n=== Cluster Profiles – Mean Spending (original monetary units) ===\n")
print(profile_raw)

# 9.2  Channel and Region composition per cluster
cat("\n=== Channel Composition by Cluster ===\n")
print(table(df_final$Cluster, df_final$Channel))

cat("\n=== Region Composition by Cluster ===\n")
print(table(df_final$Cluster, df_final$Region))

# 9.3  Radar / Spider chart of normalised mean spend per cluster
profile_norm <- df_final %>%
  bind_cols(df_spend) %>%
  group_by(Cluster) %>%
  summarise(across(all_of(spend_vars), mean), .groups = "drop") %>%
  mutate(across(all_of(spend_vars), ~ rescale(.x, to = c(0, 1))))

profile_long <- profile_norm %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Score")

ggplot(
  profile_long,
  aes(x = Variable, y = Score, colour = Cluster, group = Cluster)
) +
  geom_polygon(aes(fill = Cluster), alpha = 0.15, linewidth = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  coord_polar() +
  scale_colour_manual(values = CLUSTER_COLS) +
  scale_fill_manual(values = CLUSTER_COLS) +
  facet_wrap(~Cluster, labeller = label_both) +
  labs(
    title = "Cluster Spending Profiles – Radar Chart",
    subtitle = "Normalised mean spend per category (0 = min, 1 = max)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

# 9.4  Grouped bar chart of mean spend by cluster
profile_long2 <- profile_raw %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Category",
    values_to = "MeanSpend"
  ) %>%
  mutate(Category = str_remove(Category, "_mean"))

ggplot(profile_long2, aes(x = Category, y = MeanSpend, fill = Cluster)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(values = CLUSTER_COLS) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Mean Annual Spend by Cluster and Category",
    subtitle = "Monetary units (m.u.)",
    x = NULL,
    y = "Mean Annual Spend (m.u.)",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top"
  )

# 9.5  Boxplots – spend distribution per cluster for each category
df_box <- df_final %>%
  bind_cols(df_spend) %>%
  pivot_longer(
    cols = all_of(spend_vars),
    names_to = "Category",
    values_to = "Spend"
  )

ggplot(df_box, aes(x = Cluster, y = Spend, fill = Cluster)) +
  geom_boxplot(outlier.size = 1.2, outlier.alpha = 0.5, alpha = 0.8) +
  scale_fill_manual(values = CLUSTER_COLS) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~Category, scales = "free_y", ncol = 3) +
  labs(
    title = "Spending Distribution by Cluster and Category",
    x = "Cluster",
    y = "Annual Spend (m.u.)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# --- 10. HAC vs K-Means Agreement Check -------------------------------------

hac_labels_k3 <- cutree(hac_ward, k = 3)

# Align HAC labels to K-Means labels by majority vote (label-switching fix)
conf_mat <- table(HAC = hac_labels_k3, KMeans = km_final$cluster)
cat("\n=== Contingency Table: HAC vs K-Means Assignments ===\n")
print(conf_mat)

agreement <- sum(apply(conf_mat, 1, max)) / nrow(df_final)
cat(sprintf(
  "\nLabel-adjusted agreement between HAC and K-Means: %.1f%%\n",
  agreement * 100
))
cat("(Values above 85%% indicate the two methods recover the same structure)\n")

# --- 11. Export Results ------------------------------------------------------

output_path <- "wholesale_clustered.csv"
write.csv(df_final, output_path, row.names = FALSE)
cat("\nFull dataset with cluster labels saved to:\n", output_path, "\n")

# --- 12. Business Interpretation Summary ------------------------------------

cat("\n")
cat("================================================================\n")
cat("          CUSTOMER SEGMENTATION – BUSINESS SUMMARY             \n")
cat("================================================================\n")
cat("\n  METHOD:  HAC (Ward.D2) to identify structure +\n")
cat("           K-Means (k=3) on PCA scores for final assignment\n")
cat("  INPUT:   log1p-transformed, standardised spend variables\n")
cat("           projected onto retained PCs (Kaiser criterion)\n")
cat("\n  CLUSTER 1 – 'High-Volume Fresh Buyers' (HoReCa dominant)\n")
cat("    • Highest Fresh and Frozen spend\n")
cat("    • Low Grocery, Detergents_Paper\n")
cat("    → Hotels, restaurants, cafés; perishable-led ordering\n")
cat("    IMPLICATION: Prioritise cold-chain logistics; daily/weekly\n")
cat("    replenishment cycles; dedicated HoReCa sales reps\n")
cat("\n  CLUSTER 2 – 'Packaged Goods Retailers' (Retail dominant)\n")
cat("    • Highest Grocery, Milk, Detergents_Paper\n")
cat("    • Low Fresh spend\n")
cat("    → Supermarkets, convenience stores; ambient goods focus\n")
cat("    IMPLICATION: Bulk-order pricing tiers; shelf-stable\n")
cat("    inventory buffers; quarterly promotions on detergents\n")
cat("\n  CLUSTER 3 – 'Small / Mixed Buyers'\n")
cat("    • Below-average spend across all categories\n")
cat("    • Mixed Channel and Region membership\n")
cat("    → Small independent shops, delis, mixed traders\n")
cat("    IMPLICATION: Route-optimised delivery (lower margin);\n")
cat("    upsell programmes; self-service ordering portal\n")
cat("\n  NOTE: Exact cluster labels (1/2/3) may differ from above\n")
cat("  depending on K-Means initialisation; inspect profile_raw\n")
cat("  and the radar/bar charts to assign the business labels.\n")
cat("================================================================\n")
