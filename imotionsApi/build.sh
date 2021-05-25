Rscript R/installBuildDependencies.R
Rscript -e devtools::document()
cd target
R CMD build --no-manual --no-build-vignettes ..
R CMD check --no-manual --no-vignettes --no-examples *.tar.gz
