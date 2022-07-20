library("tidyverse")
library("purrr")

'%ni%' <- Negate("%in%")

# citeR process
# 1) identify if file or folder and R-files
# 2) read all R/Rmd-files and prepare
# 3) identify packages
# 4) get citation infos
# 5) put warnings for missing citations
# 6) save all citation info in one bibtex-file


# planned extensions
## - variable input of one file/folder or list
## - how to deal with packages from github: Some might not be official and without citation
## - add dependencies of packages as an option


citeR <- function(input, 
                  output = "bibtex.bib", # change the location and file name
                  exclude = NULL, # vector of packages to be excluded
                  dependencies = F){
  
  # ---------- 1) identify if file or folder and R-files ----------------------
  
  if(grepl("\\.(R|Rmd)$", input)){
    files_to_check <- input
  }else{
    files_to_check <-
      list.files(input) %>%
      .[grepl("\\.(R|Rmd)$", .)]
  }
  
  if(length(files_to_check) == 0){
    stop("No file or folder found at the location.")
  }
  
  
  
  # ---------- 2) read all R-files and prepare --------------------------------
  
  files_read <- list()
  
  if(length(files_to_check) == 1){
    files_read[[1]] <- 
      read_file(input) %>%
      strsplit(., "\r\n") %>%
      unlist()
  }else{
    for(i in 1:length(files_to_check)){
      files_read[[i]] <- 
        read_file(file.path(input, files_to_check[i])) %>%
        strsplit(., "\r\n") %>%
        unlist()
    }
  }
  
  
  
  # ---------- 3) identify packages -------------------------------------------
  
  packages_found <- list()
  for(i in 1:length(files_read)){
    packages_found[[i]] <- 
      regmatches(files_read[[i]], 
                 gregexpr("(library|require)\\(.+?\\)", files_read[[i]])) %>%
      unlist() %>%
      gsub('library|require|\\(|\\)|\\"', "", .) %>%
      gsub("\\'", "", .)
  }
  
  packages_to_cite <-
    packages_found %>%
    unlist()
  
  # exclude packages provided as a vector by the user
  if(!is.null(exclude)){
    packages_to_cite <-
      packages_to_cite[packages_to_cite %ni% exclude]
  }
  
  
  # check if all packages are installed
  # otherwise raise error
  if(any(packages_to_cite %ni% installed.packages())){
    missing_p <- packages_to_cite[packages_to_cite %ni% installed.packages()]
    stop("The following packages need to be installed or excluded:\n",
         paste0(missing_p, collapse = "\n"))
  }
  
  # order alphabetically
  packages_to_cite <-
    packages_to_cite[packages_to_cite %>% order(.)]
  
  
  
  # ---------- 4) get citation infos ------------------------------------------
  
  citations <- list()
  for(i in 1:length(packages_to_cite)){
    citations[[i]] <- 
      citation(packages_to_cite[i]) %>%
      toBibtex() %>%
      sub("\\{,", paste0("{R:", packages_to_cite[i], ","), .)
  }
  
  # add R
  citation_R <- list()
  citation_R[[1]] <-
    citation() %>%
    toBibtex() %>%
    sub("\\{,", paste0("{R,"), .)
  
  citation_citeR <- list()
  citation_citeR <-
    citation_R[[1]] %>%
    gsub("\\{R,", "{R:citeR", .) %>%
    gsub("title = \\{.+?\\}", 
         "title = {citeR: An easy citation tool to account for all your used packages}", .) %>%
    gsub("author = \\{.+?\\}", "author = {Sebastian Geschonke", .) %>%
    gsub("year = \\{.+?\\}", "year = {2022}", .) %>%
    gsub("url = \\{.+?\\}", "url = {https://github.com/Sebaristoteles/citeR}",.) %>%
    gsub("organization.+?$", "", .) %>%
    gsub("address.+?$", "", .) %>%
    list()
  
  #citation_citeR <-
  #  citation_citeR[names(citation_citeR) %ni% c("organization", "address")] %>%
  #  list()
  
  citations_all <-
    c(citation_R, citation_citeR, citations)
  
  
  
  # ---------- 5) put warnings for missing citations --------------------------
  
  # TBD: needs an example of a package that exists but has no citation info
  
  
  
  # ---------- 6) save all citation info in one bibtex-file -------------------
  
  citation_file <-
    citations_all %>%
    unlist() %>%
    paste0(., collapse = "\n") %>%
    gsub("\n\\s+\n", "\n", .)
  
  write_file(citation_file, file = output)
  
  
}
