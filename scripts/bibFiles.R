# A script to explore and process IPCC .bib files.
#
# Load libraries
library(tidyverse)
library(bibtex)
library(here)
library(revtools)

# Set reference directory
setwd(here("data/AR6_bibFiles"))
Files <- list.files(pattern = ".bib")

# PT 1: Load and explore the files.
# This function generates a data table that tells us how many DOIs we can 
# access from the IPCC's .bib files.
explainCitations <- function(files){
  # Define an empty tibble to be filled.
  citationInfo <- tibble()
  
  for(i in files){
    bibDF <- read_bibliography(i)
    # Extract relevant parameters (working group, chapter number, number of 
    # citations and DOIs) from the file name.
    WG <- str_extract(i, "WG[I]{1,3}(?=_)")
    chapter <- str_extract(i, "(?<=References_)[A-z0-9]*(?=\\.)")
    numCitations <- nrow(bibDF)
    # Sometimes the numDOI count is 1 higher than it should be as NA counts as 
    # a unique entry. I consider this acceptable error.
    numDOI <- nrow(filter(bibDF, !is.na(doi)))
    # A count of references that do not have DOIs but do have some other sort of
    # identifier.
    numURL <- nrow(filter(bibDF, is.na(doi) & !is.na(url)))
    # Rounded percentage of number of references with DOIs
    percentDOI = round(100*(1-((numCitations - numDOI)/numCitations)))
    # Total coverage if we include url, issn, isbn.
    percentURL = round(100*(1-((numCitations - (numDOI + numURL))/numCitations)))
    # Append the extracted data to a dataframe for easy review.
    citationInfo <- bind_rows(citationInfo, tibble(WG, chapter, numCitations, numDOI, numURL, percentDOI, percentURL))
    citationInfo <<- citationInfo
  } 
}

# Run the above function.
explainCitations(Files)
# Export the citationInfo table as a CSV.
write_csv(citationInfo, file = here("data/citationInfo.csv"))
