packages_to_load <- c("tidyverse", "purrr", "magrittr", "pbapply", "here")

invisible(
  suppressPackageStartupMessages(
    sapply(packages_to_load, 
           function(pkg) eval(parse(text = paste0("library(", pkg, ")"))))
  )
)

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
                  output = here("software.bib"), # change the location and file name
                  exclude = NULL, # vector of packages to be excluded
                  dependencies = F){
  
  # ---------- 1) identify if file or folder and R-files ----------------------
  
  if(grepl("\\.(R|Rmd)$", input)){ #  all from current folder
    files_to_check <- 
      list.files(input) %>%
      .[grepl("\\.(R|Rmd)$", .)]
  }else{ #                            all files in down the folder structure
    files_to_check <-
      list.files(input, recursive = T) %>%
      .[grepl("\\.(R|Rmd)$", .)]
  }
  
  if(length(files_to_check) == 0){
    stop("No file or folder found at the location.")
  }
  
  
  
  # ---------- 2) read all R-files and prepare --------------------------------
  
  files_read <- list()
  files_paths <- list()
  
  if(length(files_to_check) == 1){
    files_read[[1]] <- 
      read_file(input) #%>%
      #strsplit(., "\r\n") %>%
      #unlist()
  }else{
    for(i in 1:length(files_to_check)){
      files_paths[[i]] <-
        file.path(input, files_to_check[i])
      
      files_read[[i]] <- 
        read_file(files_paths[[i]]) #%>%
        #strsplit(., "\r\n") %>%
        #unlist()
    }
  }
  
  
  
  # ---------- 3) identify packages -------------------------------------------
  
  packages_R_has <- available.packages() %>% as.data.frame()
  
  
  # Function to get all potential packages
  
  f_get_potential_packages <- function(text){
    
    # get all potential packages
    lib_req <-
      regmatches(text, 
                 gregexpr("(library|require)\\(.+?\\)", text)) %>%
      unlist() %>%
      gsub('library|require|\\(|\\)|\\"', "", .) %>%
      gsub("\\'", "", .) %>%
      unique()
    
    all_enclosed_items <-
      c(regmatches(text, 
                   gregexpr("\'.+?\'", text)) %>%
          unlist(),
        regmatches(text, 
                   gregexpr('\".+?\"', text)) %>%
          unlist(),
        regmatches(text, 
                   gregexpr("\\(.+?\\)", text)) %>%
          unlist()
      ) %>%
      gsub("\'|\"|\\(|\\)", "", .) %>%
      unique()
    
    potential_packages <-
      c(lib_req, all_enclosed_items) %>%
      unique()
    
    return(potential_packages)
  }
  
  
  # Function to find exact matches for each set of texts
  f_find_exact_matches <- function(texts, patterns = packages_R_has$Package) {
    # Create a data frame to identify exact matches between patterns and the current set of texts
    map_dfr(patterns, ~ {
      tibble(
        pattern = .x,
        text = texts,
        match = texts == .x
      )
    }) %>%
      filter(match)
  }
  
  
  # get all potential packages from all files
  message(format(Sys.time(), "%d.%m.%Y %X"), 
          ": getting all potential packages")
  pot_packs <-
    pblapply(files_read, f_get_potential_packages)
  
  
  # Apply the function across all text sets with a progress bar
  message(format(Sys.time(), "%d.%m.%Y %X"), 
          ": finding matches of potential packages and existing matches")
  results_list <- pblapply(pot_packs, f_find_exact_matches)
  
  packages_found <-
    results_list %>%
    unlist() %>%
    unique()
  
  # check final results once more
  message(format(Sys.time(), "%d.%m.%Y %X"), 
          ": confirming matches")
  packages_found_confirmed <-
    f_find_exact_matches(packages_found) %>%
    filter(match %in% TRUE) %>%
    pull(pattern)
  
  packages_to_cite <-
    c(packages_found_confirmed, packages_to_load) %>%
    unique()
  
  # exclude packages provided as a vector by the user
  if(!is.null(exclude)){
    packages_to_cite <-
      packages_to_cite[packages_to_cite %ni% exclude]
  }
  
  
  # check if all packages are installed
  # otherwise raise warning
  if(any(packages_to_cite %ni% installed.packages())){
    missing_p <- packages_to_cite[packages_to_cite %ni% installed.packages()]
    warning("The following packages are not installed and might be false Positives:\n",
         paste0(missing_p, collapse = "\n"),
         "\nConsider to exclude them.")
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
    gsub("year = \\{.+?\\}", "year = {2024}", .) %>%
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
