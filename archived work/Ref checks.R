library(tidyverse)

#Load the CSV
Ref <- read_csv("/Users/declantaylor/Documents/Work/Sherpa_Project/IPCC DOIs/Refs.csv", col_names = FALSE) %>%
  # Remove any rows containing NA values
  na.omit()

# Check for alphabetical order
first_letters <- as.list(toupper(str_sub(Ref$X1, 1, 1)))

alpha_order <- tibble(
  row_number = numeric(),
  alpha_order = character(),
  citation1 = character(),
  citation2 = character())

doi <- tibble(
  row_number = numeric(),
  citation1 = character(),
  doi_count = numeric())

# Check for changes in the first letter (should just be logical alpha-order 
# breaks)
for(i in 1:length(first_letters)){
  # Check for breaks in alphabetical order
  if(identical(first_letters[i], first_letters[i+1]) == FALSE){
    alpha_order <- add_row(alpha_order,
                           row_number = i,
                           alpha_order = paste0(first_letters[i], first_letters[i+1]),
                           citation1 = Ref$X1[i],
                           citation2 = Ref$X1[i+1])
  }
  # Check for abnormal occurrences of the phrase "doi"
  if(str_count(Ref$X1[i], pattern = "doi") != 1){
    doi <- add_row(doi,
                   row_number = i,
                   citation1 = Ref$X1[i],
                   doi_count = str_count(Ref$X1[i], pattern = "doi"))
  }
}

rows_to_check <- full_join(alpha_order,
                            doi,
                            by = c("row_number", "citation1")) %>%
  select(row_number,
         alpha_order,
         doi_count,
         citation1,
         citation2)

View(rows_to_check)
