# citeR

This R-function creates a .bib-file that includes the citations from all the packages that you have used in a project.
The input can be a file or folder. The function will use all R-files and Rmd-files and extract the used packages

You __must__ specify
- the _input_

You __can__ specify
- the _output_ file location and name
- a vector of packages to _exclude_


```R
citeR <- function(input, output, exclude)
```


# Purpose

citeR is an easy way to get a bib-file with all the references of the R-packages that you used.
It is good scientific practice to cite as well tools that your analysis relies on.


# How to use the software-bib

If you have the bib-file, you can easily integrate the references by either citing them in Latex using the common citation approach 
or use nocite if you do not want to write them explicitly but want to list them.

```Latex
\nocite{<bib-key>}

# for all entries:
\nocite{*}
```



An efficient alternative in Rmarkdown is using nocites for all entries of the bib file and thereby creating the list of Software references

```R

```{r nocite-software,  echo=FALSE, results='asis'}
nocites <- read_lines(here("doc", "bib", "software.bib")) %>%
  .[grepl("@.+?\\{R(|:).*?,", .)] %>%
  gsub("@.+?\\{", "", .) %>%
  gsub(",$", "", .) %>%
  paste0("\\nocite{", ., "}")

cat(paste0(nocites, collapse = "\n"))
```



# Code to install required packages to your system

```R
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("purr")
install_package_if_missing("magrittr")
install_package_if_missing("pbapply")
install_package_if_missing("here")
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