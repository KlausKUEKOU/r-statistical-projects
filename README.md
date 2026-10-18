# R Statistical Projects

A collection of applied statistics projects in R, from multivariate analysis to regression modelling
and Shiny applications.

| Project | Methods | Output |
|---|---|---|
| [`pca-french-cities/`](pca-french-cities) | Descriptive statistics, outlier detection (IQR), correlation, **PCA**, clustering of 20 French cities on unemployment, housing, taxation, crime and sunshine | R Markdown + Word report |
| [`fertility-rate-regression/`](fertility-rate-regression) | Bivariate analysis, PCA, hierarchical clustering of countries, **linear regression** with Box-Cox transformation and diagnostics, prediction | R Markdown |
| [`teacher-evaluation-shiny/`](teacher-evaluation-shiny) | **Shiny dashboard** for teacher evaluation surveys (shinydashboard, highcharter, MySQL backend) | Shiny app |

## Run a project

Open the `.Rmd` file in RStudio and click **Knit**. Each analysis expects its dataset in a `data/`
folder next to the `.Rmd` (datasets are not included in the repository).

The Shiny app needs a MySQL database: set the connection details in `server.R`, then run
`shiny::runApp("teacher-evaluation-shiny")`.

**Author:** Klaus KUEKOU
