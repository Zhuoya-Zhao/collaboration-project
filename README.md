# Collaboration-project
We use R programming to perform statistical analysis on the dataset from the experiment that explored the effect of a new treatment on gene expression. The produced plots are saved in the figs folder and the produced tables are saved in the figs folder.

Run EDA.R from the R folder to perform exploratory data analysis (EDA), including statistics summary of gene expression, regression lines for observing the effect of concentration on the gene expression, and boxplots for observing the effect of treatment on the gene expression.

Run figure.R from the R folder to generate the plot of data points grouped by the cell type, treatment and cell name.

Run sample-size.R from the R folder to compute the sample size with known $R^2$
 of 0.1 between the predictors and the response level. The scenario is that we want a power of 90% and a significance level of 0.05 for a linear regression of gene expression with the predictors concentration, cell age, treatment (two levels), cell type (two levels), and media (two levels).

Run the IMRaD.qmd from the R folder to produce a IMRaD report of a predictive model of gene expression. The produced report, with the R code appended at the end, can be checked from the root directory.
