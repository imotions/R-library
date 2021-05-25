# Install packages needed by the R build and check commands.
# Dependencies needed by the actual project should be listed under Imports in DESCRIPTION instead.
dependencies <- c("devtools", "roxygen2", "testthat", "stubthat", "mockr", "lintr");

toInstall <- dependencies[!dependencies %in% installed.packages()];
if (length(toInstall) > 0) {
    install.packages(toInstall, repos = "http://cran.us.r-project.org");
}
