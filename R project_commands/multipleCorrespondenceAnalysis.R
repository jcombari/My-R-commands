## To load SPSS data
library(foreign)

## Analysis and display
library(ggplot2)
library(data.table)
library(MASS)
library(ca)
library(FactoMineR)
library(factoextra)

## Loading the file
wip <- poison

## Select the appropriate columns.
# Only the variables to be included in the following analysis.
wipMCA.df <- wip[,c(4,5,6)]
wipMCA.df <- na.omit(wipMCA.df)

## Multiple Correspondence Analysis

## Screen plot for dimensions
fviz_screeplot(survey.mca)

## Perform the MCA
## References for process as follows:
# http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining#visualize-supplementary-variables
# http://factominer.free.fr/classical-methods/multiple-correspondence-analysis.html
survey.mca <- MCA(wipMCA.df, graph = FALSE, ncp = 2)

## Plots for distribution of points by dimensions
## As per SPSS output
plot.MCA(survey.mca, invisible=c("ind","quali.sup"), axes = c(1,2))
plot.MCA(survey.mca, invisible=c("ind","quali.sup"), axes = c(1,3))
plot.MCA(survey.mca, invisible=c("ind","quali.sup"), axes = c(2,3))

plot.MCA(survey.mca, invisible=c("var","quali.sup"), axes = c(1,2))
plot.MCA(survey.mca, invisible=c("var","quali.sup"), axes = c(1,3))
plot.MCA(survey.mca, invisible=c("var","quali.sup"), axes = c(2,3))

plot.MCA(survey.mca, invisible=c("ind"))
plot.MCA(survey.mca, invisible=c("ind", "var"))

## Information on the model
# Eigenvalues Variance is the inertia measure per dimensions.
# Categorical variables (eta2) contains Discrimination Measures
summary(survey.mca, ncp = 2)
get_eig(survey.mca)

## Contribution figures for effect per row/col
# Displayed as percent
survey.mca$var$contrib
survey.mca$ind$contrib

## Discriminant Measures
# Contributions by variables to dimensions.
survey.mca$var$eta2