#' @title Summarize and Export a CSV
#'
#' @description Applies `Hmisc::describe()` to a specific input CSV file and outputs the results into a new csv
#'
#' @param input (character) path to input csv file
#' @param output (character) path to output summary file
#'
#' @export
#'
#'
csv_summary <- function(input = NULL, output = NULL){
  # Error out if either input or output is null
  if(base::is.null(input) | base::is.null(output))
    stop("Input and output are both required")

  # Error out if the provided file is not a CSV
  if(!tools::file_ext(input) == "csv")
    stop("`input` must be a CSV")

  # Read csv
  df <- utils::read.csv(input, stringsAsFactors = FALSE)

  #capturing filename of csv
  filename <- base::gsub(pattern = "^.*\\/(.*)\\.csv$",
                         replacement = "\\1",
                         x = input)

  # Creating output filename for summary stats
  summary_output_name <- base::sprintf("%s%s_summary.txt", output, filename)

  # Writing summary stats to .txt in specified output directory
  R.utils::captureOutput(Hmisc::describe(df),
                         file = summary_output_name)

  # Confirmation message
  line <- base::paste(base::rep("-", 100), collapse = "")
  confirmation <- base::sprintf("\n%s\nsummary statistics outputted to %s\n%s\n", line, summary_output_name, line)
  message(confirmation)
}

#' @title Compute Categorical Frequency
#'
#' @description Computes the frequency of categorical variables in a given dataframe.
#'
#' @param df (data frame) data to summarize
#' @param threshold_fract (numeric) threshold proportion (0-1) for identifying and dropping factors that are unique for every row
#'
#' @importFrom magrittr %>%
#'
#' @return summary statistics for categorical values
#'
#' @export
#'
#' @examples
#' \dontrun{
#' categorical_frequency(faimstracs_legacy)
#' }
#'
categorical_frequency <- function(df, threshold_fract = 0.9){
  # Squelch visible bindings note
  category <- attribute <- ntotal_cat <- NULL

  # NOTE: need to be improved regarding field with NAs

  # Select the character columns
  cat_data <- base::Filter(f = is.character, x = df)

  # Compute the unique values and frequencies
  cat_freq_list <- purrr::map(.x = cat_data, .f = janitor::tabyl)

  # Create a data frame out of that
  cat_freq <- dplyr::bind_rows(cat_freq_list, .id = "attribute")
  names(cat_freq)[[2]] <- "category"

  # Add the total number of categories
  cat_freq <- cat_freq %>%
    tidyr::drop_na(category) %>%
    dplyr::group_by(attribute) %>%
    dplyr::mutate(ntotal_cat = dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Remove the one that are unique for each row (here 90%), (i.e., no categories
    dplyr::filter(ntotal_cat < nrow(df) * threshold_fract)

  return(cat_freq)
}
