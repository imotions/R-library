# R-Library

## Background

The iMotions R library `imotionsApi`, makes it possible to create R scripts and notebooks that can be used to read and write signals, generate metrics, and HTML reports for iMotions studies.

Notebooks are combine R code and markdown and can be knitted (compiled) to a document that contains text, R code and graphics output. To access study data you can use the new iMotions R library. You can run existing and custom notebooks on your own computer with RStudio.

## Getting started

1. Download and install R.
2. Download and install RStudio.
3. Open RStudio and go to Tools →  Global Options →  Code →  Saving and set "Default text encoding" to UTF-8.
4. Download and install the `imotionsApi` package and load it with the command `library(imotionsApi)`.

## Connecting to an iMotions study

Make sure that iMotions is running on your computer. You can connect to iMotions from R with the command:
```r
connection <- imConnection("xxxxxxxx")
```

Please refer to the documentation of the methods in the `imotionsApi` package for further instructions and examples.
