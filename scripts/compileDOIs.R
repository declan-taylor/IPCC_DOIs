extractDOIs <- function(files){
  DOIs <- tibble(WG = character(), 
                 chapter = character(),
                 numCitations = numeric(),
                 numDOI = numeric())
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