# Load convenience functions for PCA.
source("pca.R")

# Dependencies.
library(ggplot2)

# Let's start with two dataframes:
# 1. A dataframe called "dat" with rows = genes and columns = samples.
# 2. A dataframe called "meta" with covariates. It has one row for each sample.

# Typically, one of my first steps is to look at the distribution of standard deviation.
row_sd <- apply(dat, 1, sd)
plot(density(row_sd))

# It is useful look at PCA for genes with high variability.
idx_sd <- row_sd > 0.5
sum(idx_sd)

# I find that centering is necessary for expression data, but scaling is not always desired.
pca1 <- prcomp(dat[idx_sd, ], scale = FALSE, center = TRUE)

# Always check to see if the principal components explain a large amount of variation.
# Remember that PCA with fewer genes will always explain more variation than PCA with more genes.
plot_variance_explained(pca1) + theme_bw(base_size = 18)

# We'll use this to annotate the scatter plots with variance explained.
pca3_var_labs <- sprintf(
  "PC%s, %.02g%% of variance",
  variance_explained(pca1)$Component,
  100 * variance_explained(pca1)$Variance
)

# We'll use this to annotate the plot with covariates.
pca1_r <- cbind(pca1$rotation, meta)

# For exploratory analysis, we should correlate covariates with principal components.
# This will help us decide which components to plot, and how to color points.
correlate_pcs(pca1, meta)

# I find that exploratory plotting is easiest with ggplot2.
ggplot(data = pca1_r) +
  geom_point(size = 5, aes(PC1, PC2, color = Time)) +
  geom_text(size = 4, hjust = 0, vjust = 0, aes(PC1, PC2, label = Index)) +
  theme_bw(base_size = 18) +
  labs(x = pca1_var_labs[1],
       y = pca1_var_labs[2])