# R-Library

[![R test, lint & check, build, release](https://github.com/imotions/R-library/actions/workflows/workflow.yml/badge.svg)](https://github.com/imotions/R-library/actions/workflows/workflow.yml)

With the iMotions R library `imotionsApi`, it is possible to edit or write notebooks that can be used to generate metrics, signals, or HTML reports for studies.

Notebooks combine R code and markdown and can be knitted (compiled) to a document that contains text, R code and graphics output. To access study data you can use the new iMotions R library. You can run existing and custom notebooks on your own computer with RStudio.

## Getting started

Download and install the `imotionsApi` package using `devtools::install_github("imotions/R-library", subdir = 'imotionsApi')` and load it with the command `library(imotionsApi)`.
To see the library's inline documentation, type `?imotionsApi` in the RStudio console.

## How to write notebooks

### Connecting to an iMotions study

Make sure that iMotions is running on your computer. You can connect to iMotions from R with the command:
```r
connection <- imConnection("xxxxxxxx")
```

Note that this generic token only gives access to raw data collected in a study. To access data specific to an analysis in the software, right-click on the analysis and select "Get token for R API connection". Start a connection by copying this token and replacing the generic token:
```r
connection <- imConnection(token_copied)
```

Please refer to the documentation of the methods in the `imotionsApi` package for further instructions and examples.


### Articles

- **Article 1: How Notebook Runs in iMotions - An Example Notebook Explained**: go through the code of an existing notebook running in iMotions platform → [See article here](https://htmlpreview.github.io/?https://github.com/imotions/R-library/blob/main/documentation/article1-example-notebook-explained.html).
- **Article 2: How to Access iMotions Data from RStudio: how to retrieve iMotions data from RStudio** → [See article here](https://htmlpreview.github.io/?https://github.com/imotions/R-library/blob/main/documentation/article2-imotionsApi-from-RStudio.html).
