# Load libraries
library(tidyverse)
library(bibtex)
library(here)
library(revtools)

# Set reference directory
setwd(here("data/AR6_bibFiles"))
Files <- list.files(pattern = ".bib")

# A function to extract DOIs and relevant information from each chapter. Every
# DOI is contained on one line with accompanying ID information.
extractDOIs <- function(files){
  # Create blank tibble to be appended to in the for loop.
  DOIs <- tibble()
  
  for(i in files){
    bibDF <- read_bibliography(i) %>%
      # Extract the name of the working group and chapter to be added as 
      # columns to support the long-data format.
      mutate(WG = str_extract(i, "WG[I]{1,3}(?=_)"),
             chapter = str_extract(i, "(?<=References_)[A-z0-9]*(?=\\.)")) %>%
      select(any_of(c("WG", "chapter", "year", "label", "journal", "publisher", "abstract", "doi", "url", "isbn", "issn")))
    #Save the dataframe to the local function environment
    # assign(paste0(str_extract(i, ".+(?=.bib)")), bibDF)
    
    # Append the chapter's data to the overall DOI tibble in the function 
    # environment.
    DOIs <- bind_rows(DOIs, bibDF)
    # ...And update in the global environemnt!
    DOIs <<- DOIs
  } 
}

# Run the above function
extractDOIs(Files)
# Save the tibble output as a CSV
write_csv(DOIs, file = here("data/AR6_DOIs.csv"))

# Showing that there are > 32000 reports in WGII with citations.
# filter(DOIs, WG == "WGII" & !is.na(doi))
