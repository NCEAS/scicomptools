#' @title Pre-Morpho Wrangling
#'
#' @description "Morpho" refers to dataONE's metadata software **NOT** the 2022 NCEAS working group request for proposals of the same name. See [here](https://old.dataone.org/software-tools/morpho) for the Morpho these functions are meant to deal with. Written by: Justin Kroes (jkroes14@ucsbalum.com | orcid.org/0000-0003-3257-2321)
#'
#'
#' @param file character, name of file to check
#' @param read_dir character, path to file to check
#' @param write_dir character, path to write checked file to
#' @param na_code character, how missing values are coded in the data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export
#'
morpho_wiz <- function(file = NULL, read_dir = getwd(),
                       write_dir = getwd(), na_code = "") {
  # NOTE: I (Nick) guessed at the namespace for the `extract()` function and that could be wrong

  # Squelch visible bindings note
  .csv_unique <- . <- NULL

  # Error out if file is not named
  if(is.null(file)) stop("No file provided.")

  # Read columns in as character type. This prevents the coercion of blank cells in otherwise numeric data to NA.
  csv_file <- utils::read.csv(file = file.path(read_dir, file),
                             stringsAsFactors = FALSE,
                             colClasses = "character")

  # Across all columns, find unique values and sort them in ascending
  # alphabetical order.
  csv_unique <- lapply(csv_file, unique) %>%
    lapply(sort, na.last = FALSE)

  # Place alternative string for missing values at top of vector of unique values
  # for each .csv-file column. Note that subsequent values in the vector will be
  # "" (i.e. the empty string--a blank cell in Excel--and NA).
  sort_missing <- function(col, na_code) {
    # Prevent errors about missing values using if-statement
    if (base::any((col %in% na_code))) {
      # Append alternative string at beginning of vector
      col %<>%
        c(na_code, .)
      # Find the indices of the second (i.e. original) occurrence of the
      # alternative string, and remove the duplicate occurrence
      idx <- base::grep(na_code, col)[2]
      # Only way I found to remove vector elements
      col %<>% tidyr::extract(-idx)
    }
    return(col)
  }

  # Call sort_missing()
  # if (.missing_code != "") {
  csv_unique <- lapply(.csv_unique, sort_missing, na_code = na_code)
  # }

  # Assign .csv-file column names to column contents in temporary environment
  e <- new.env()
  mapply(assign,
         names(csv_unique),
         value = csv_unique,
         MoreArgs = list(envir = e))
  fields <- names(csv_unique)

  # Write variables to file
  lapply(fields,
         function(field, dir) utils::write.table(eval(as.symbol(field),
                                               envir = e),
                                          file = file.path(dir, field),
                                          row.names = FALSE,
                                          quote = FALSE,
                                          col.names = FALSE,
                                          sep = ",",
                                          append = FALSE),
         dir = write_dir)

  return("Go check out your sleek new files!")
}
