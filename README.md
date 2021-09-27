# R-Library

[![R test, lint & check, build, release](https://github.com/imotions/R-library/actions/workflows/workflow.yml/badge.svg)](https://github.com/imotions/R-library/actions/workflows/workflow.yml)

With the iMotions R library `imotionsApi`, it is possible to edit or write notebooks that can be used to generate metrics, signals, or HTML reports for studies.

Notebooks are combine R code and markdown and can be knitted (compiled) to a document that contains text, R code and graphics output. To access study data you can use the new iMotions R library. You can run existing and custom notebooks on your own computer with RStudio.

## Getting started

1. Download and install [R](https://www.r-project.org/).
2. Download and install [RStudio](https://www.rstudio.com/products/rstudio/download/#download).
3. Open RStudio and go to Tools →  Global Options →  Code →  Saving and set "Default text encoding" to UTF-8.
4. Download and install the `imotionsApi` package and load it with the command `library(imotionsApi)`.

To see the library's inline documentation, type `?imotionsApi` in the RStudio console.

## How to write notebooks

### Connecting to an iMotions study

Make sure that iMotions is running on your computer. You can connect to iMotions from R with the command:
```r
connection <- imConnection("xxxxxxxx")
```

Please refer to the documentation of the methods in the `imotionsApi` package for further instructions and examples.


### Articles

- **Article 1: How Notebook Runs in iMotions - An Example Notebook Explained**: go through the code of an existing notebook running in iMotions platform → [See article here]((https://htmlpreview.github.io/https://github.com/imotions/R-library/tree/main/documentation/article1-example-notebook-explained.html).
- **Article 2: How to Access iMotions Data from RStudio: how to retrieve iMotions data from RStudio** → [See article here](https://htmlpreview.github.io/https://github.com/imotions/R-library/tree/main/documentation/article2-imotionsApi-from-RStudio.html).
