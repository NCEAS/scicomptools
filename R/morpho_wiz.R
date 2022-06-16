#' @title Pre-Morpho Wrangling
#'
#' @description "Morpho" refers to dataONE's metadata software **NOT** the 2022 NCEAS working group request for proposals of the same name. See [here](https://old.dataone.org/software-tools/morpho) for the Morpho these functions are meant to deal with.
#'


# Written by: Justin Kroes (jkroes14@ucsbalum.com | orcid.org/0000-0003-3257-2321)

# Prepare session
library(magrittr)  # This script was created using Version 1.5

morpho_wiz <- function(.read_dir, .write_dir, .missing_code) {
  # Read columns in as character type. This prevents the coercion of blank cells in otherwise numeric data to NA.
  .csv_file <- read.csv(.read_dir,
                        stringsAsFactors = FALSE,
                        colClasses = "character")
  rm(.read_dir)

  # Across all columns, find unique values and sort them in ascending
  # alphabetical order.
  .csv_unique <- lapply(.csv_file, unique) %>%
    lapply(sort, na.last = FALSE)
  rm(.csv_file)

  # Place alternatve string for missing values at top of vector of unique values
  # for each .csv-file column. Note that subsequent values in the vector will be
  # "" (i.e. the empty string--a blank cell in Excel--and NA).
  SortMissingCode <- function(col, code_missing) {
    # Prevent errors about missing values using if-statement
    if (any((col %in% code_missing))) {
      # Append alternative string at beginning of vector
      col %<>%
        c(code_missing, .)
      # Find the indices of the second (i.e. original) occurence of the
      # alternative string, and remove the duplicate occurence
      idx <- grep(code_missing, col)[2]
      col %<>% extract(-idx) # only way I found to remove vector elements
    }
    return(col)
  }

  # Call SortMissingCode()
  # if (.missing_code != "") {
  .csv_unique <- lapply(.csv_unique, SortMissingCode, code_missing = .missing_code)
  # }
  rm(SortMissingCode)

  # Assign .csv-file column names to column contents in temporary environment
  e <- new.env()
  mapply(assign,
         names(.csv_unique),
         value = .csv_unique,
         MoreArgs = list(envir = e))
  .fields <- names(.csv_unique)
  rm(.csv_unique)

  # Write variables to file
  lapply(.fields,
         function(field, dir) write.table(eval(as.symbol(field),
                                               envir = e),
                                          file = file.path(dir, field),
                                          row.names = FALSE,
                                          quote = FALSE,
                                          col.names = FALSE,
                                          sep = ",",
                                          append = FALSE),
         dir = .write_dir)

  return("Go check out your shiny new files!")
}

