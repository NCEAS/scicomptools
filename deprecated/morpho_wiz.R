#' @title Pre-Morpho Wrangling
#'
#' @description "Morpho" refers to dataONE's metadata software **NOT** the 2022 NCEAS working group request for proposals of the same name. See [here](https://old.dataone.org/software-tools/morpho) for the Morpho these functions are meant to deal with. Written by: Justin Kroes (jkroes14@ucsbalum.com | orcid.org/0000-0003-3257-2321)
#'
#'
#' @param file_name (character) Name of (and path to) the input CSV
#' @param na_code (character) How missing values are coded in the data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export
#'
#' @return (list) unique elements
#'
#' @examples
#' # Identify unique values per column with custom missing code appended to front of vector
#' morpho_wiz(file_name = system.file("extdata", "fake_data.csv",
#' package = "scicomptools"), na_code = "empty")
#"
morpho_wiz <- function(file_name = NULL, na_code = "") {
  # Squelch visible bindings note
  . <- NULL

  # Error out if file is not named
  if(base::is.null(file_name)) stop("No file provided")

  # Read columns in as characters. This prevents the coercion of blank cells in otherwise numeric data to NA.
  csv_file <- utils::read.csv(file = file_name,
                              stringsAsFactors = FALSE,
                              colClasses = "character")

  # Within each column
  csv_unique <- csv_file %>%
    ## Find unique values
    purrr::map(.f = base::unique) %>%
    ## And sort them alphabetically
    purrr::map(.f = base::sort, na.last = FALSE)

  # Call function for placing alternative string for missing values at top of vector of unique values
  sort_missing <- function(col, na_code) {
    # Prevent errors about missing values using if-statement
    if (base::any((col %in% na_code))) {
      # Append alternative string at beginning of vector
      col %<>% c(na_code, .)
      # Find the indices of the second (i.e. original) occurrence of the alternative string, and remove the duplicate
      idx <- base::grep(na_code, col)[2]
      # Only way I found to remove vector elements
      col %<>% R.utils::extract(-idx)
    }
    return(col) }

  # Call `sort_missing()`
  csv_with_missing <- csv_unique %>%
    purrr::map(.f = sort_missing, na_code = "empty")

  # Return this list
  return(csv_with_missing)
}
