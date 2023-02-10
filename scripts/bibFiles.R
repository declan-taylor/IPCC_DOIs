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
  citationInfo <- tibble(WG = character(), 
                            chapter = character(),
                            numCitations = numeric(),
                            numDOI = numeric())
  
  for(i in files){
    bibDF <- read_bibliography(i)
    # Extract relevant parameters (working group, chapter number, number of 
    # citations and DOIs) from the file name.
    WG <- str_extract(i, "WG[I]{1,3}(?=_)")
    chapter <- str_extract(i, "(?<=References_)[A-z0-9]*(?=\\.)")
    numCitations <- nrow(bibDF)
    numDOI <- length(unique(bibDF$doi))
    percentDOI = round(100*(1-((numCitations - numDOI)/numCitations)))
    # Append the extracted data to a dataframe for easy review.
    citationInfo <- bind_rows(citationInfo, tibble(WG, chapter, numCitations, numDOI, percentDOI))
    citationInfo <<- citationInfo
  } 
}

# Run the above function.
explainCitations(Files)
# Export the citationInfo table as a CSV.
write_csv(citationInfo, file = here("data/citationInfo.csv"))
