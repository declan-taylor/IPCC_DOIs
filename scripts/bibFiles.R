# A script to explore and process IPCC .bib files. Written by Declan D. Taylor,
# December 2022 for Dr. Pasang Y. Sherpa's research work. All .bib files 
# explored were sourced from the IPCC website on Dec 14 2022 (WGI, WGII) and 
# Jan 08 2022 (WG III).In WGIII Ch 1, 3, 17 there are no line breaks between 
# the end "}" of one citation and the leading "@" of the next. I manually added 
# these prior to further analysis. Futher, in WGIII Ch 4-16, there is a comment
# at the end of the .bib file which reads 
# "@Comment{jabref-meta: databaseType:bibtex;}", which I also had to remove as
# it was throwing an error.
#
# WGI: https://www.ipcc.ch/report/ar6/wg1/downloads/
# WGII: https://www.ipcc.ch/report/ar6/wg2/downloads/
# WGII: https://www.ipcc.ch/report/ar6/wg3/downloads/
#
# Load libraries
library(tidyverse)
library(bibtex)
library(revtools)

# Set reference directory
setwd("/Users/declantaylor/Documents/Work/Sherpa_Project/IPCC_DOIs/IPCC_AR6/bibFiles")
Files <- list.files(pattern = ".bib")

# PT 1: Load and explore the files.
# This function generates a data table that tells us how many DOIs we can 
# access from the IPCC's .bib files.
explainCitations <- function(files){
  # Set up blank tibble (data table) to be filled.
  citationInfo <- tibble(WG = character(), 
                            chapter = numeric(),
                            numCitations = numeric(),
                            numDOI = numeric(),
                            numOmitted = numeric())
  
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
    citationInfo <- rbind(citationInfo, tibble(WG, chapter, numCitations, numDOI, percentDOI))
    citationInfo <<- citationInfo
  } 
}

# Run the above function.
explainCitations(Files)
# Export the citationInfo table as a CSV.
write_csv(citationInfo, file = "citationInfo.CSV")

IPCC_DOIs <- Files %>%
  lapply(read_bibliography) %>%
  mutate(WG = str_extract(x, "WG[I]{1,3}(?=_)"),
         chapter = str_extract(x, "(?<=References_)[A-z0-9]*(?=\\.)")) %>%
  bind_rows %>%
  select(any_of(c("WG", "chapter", "label", "doi", "url", "isbn", "issn")))
  

Test <- read_bibliography(Files[1])

extractDOIs <- function(files){
  for(i in files){
    bibDF <- read_bibliography(i) %>%
      mutate(WG = str_extract(i, "WG[I]{1,3}(?=_)"),
             chapter = str_extract(i, "(?<=References_)[A-z0-9]*(?=\\.)")) %>%
      select(any_of(c("WG", "chapter", "label", "doi", "url", "isbn", "issn")))
    # Write an appropriatley named CSV for each chapter that
    write_csv(bibDF, file = paste0(str_extract(i, ".+(?=.bib)"),"_DOIs.csv"))
  } 
}

extractDOIs(Files)

