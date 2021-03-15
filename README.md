<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/Ecume)](https://cran.r-project.org/package=Ecume)
[![R build status](https://github.com/HectorRDB/Ecume/workflows/R-CMD-check/badge.svg)](https://github.com/HectorRDB/Ecume/actions)
[![Codecov test coverage](https://codecov.io/gh/HectorRDB/Ecume/branch/main/graph/badge.svg)](https://codecov.io/gh/HectorRDB/Ecume?branch=main)
<!-- badges: end -->
  
# R package: Ecume

  
This package implemements (or re-implements in `R`) a variety of statistical tools used to do non-parametric two-sample (or k-sample) distribution comparisons in the univariate or multivariate case.

If you want to suggest a new test or a variant, open an [issue](https://github.com/HectorRDB/Ecume/issues).

## Installation

To install the current version of *Ecume*, run.

```
if(!requireNamespace("BiocManager", quietly = TRUE)) {
 install.packages("BiocManager") 
}
BiocManager::install("Ecume")
```

To install the development version in `R`, run 

```r
if(!requireNamespace("devtools", quietly = TRUE)) {
 install.packages("devtools") 
}
devtools::install_github("HectoRDB/Ecume")
```
## Usage 

The *Ecume* package has been developed as part of the [*condiments*](https://hectorrdb.github.io/condiments/) paper but it was spawned as a separate package since it use can be broader than scRNA-Seq data analysis. The [online vignette](https://hectorrdb.github.io/Ecume/articles/Ecume.html) is a good place to get started.
