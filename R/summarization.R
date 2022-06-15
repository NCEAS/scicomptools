#' Apply summary function on a scpecific input files and export the results into a csv
#'
#'
#' @param input (character) path to input csv file
#' @param output (character) path to output summary file
#'
#' @export
#'
#' @examples
#'
csv_summary <- function(input, output){
  #read csv
  df <- read.csv(input, stringsAsFactors = FALSE)

  #capturing filename of csv
  filename <- gsub("^.*\\/(.*)\\.csv$", "\\1", input)

  #creating output filename for summary stats
  summary_output_name <- sprintf("%s%s_summary.txt", output, filename)

  #writing summary stats to .txt in specified output directory
  R.utils::captureOutput(Hmisc::describe(df), file=summary_output_name)

  #confirmation message
  line <- paste(rep("-", 100), collapse = "")
  confirmation <- sprintf("\n%s\nsummary statistics outputted to %s\n%s\n", line, summary_output_name, line)
  message(confirmation)
}

#' Compute categorical frequency
#'
#' @param df A data frame containing the data to summarize
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
  # Select the character columns
  cat_data <- Filter(is.character, df)
  # Compute the unique values and frequencies
  cat_freq_list <- purrr::map(cat_data, janitor::tabyl)
  # Create a data frame out of that
  cat_freq <- dplyr::bind_rows(cat_freq_list, .id = "attribute")
  names(cat_freq)[[2]] <- "category"
  # add the total number of categories
  cat_freq <- cat_freq %>%
    tidyr::drop_na(category) %>%
    dplyr::group_by(attribute) %>%
    dplyr::mutate(ntotal_cat = n()) %>%
    dplyr::ungroup() %>%
    #remove the one that are unique for each row (here 90%), ie no categories
    # Note: need to be improved regarding field with NAs
    dplyr::filter(ntotal_cat < nrow(df)*threshold_fract)

  return(cat_freq)
}
