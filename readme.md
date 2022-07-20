# citeR

This R-function creates a .bib-file that includes the citations from all the packages that you have used in a project.
The input can be a file or folder. The function will use all R-files and Rmd-files and extract the

You __must__ specify
- the _input_

You __can__ specify
- the _output_ file location and name
- a vector of packagaes to _exclude_


```R
citeR <- function(input, output, exclude)
```


# Code to install packages to your system

```R
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("purr")
```



# How to install a package/R-file from GitHub


First, you need to install the devtools package. You can do this from CRAN. Invoke R and then type

```R
install.packages("devtools")
```

Load the devtools package.

```R
library(devtools)
```

Usually you just use install_github("author/package") but it's not yet a package.
Therefore, just do as follows

```R
  devtools::source_url(
    "https://raw.githubusercontent.com/sebaristoteles/citeR/main/cite.R"
  )
```