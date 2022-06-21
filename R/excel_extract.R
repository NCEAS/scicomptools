#' @title Read All Sheets from an Excel Workbook
#'
#' @description Retrieves all of the sheets in a given Microsoft Excel workbook and stores them as elements in a list. Note that the guts of this function were created by the developers of `readxl::read_excel()` and we merely created a wrapper function to invoke their work more easily.
#'
#' @param file_name (character) Name of (and path to) the Excel workbook
#'
#' @return (list) One tibble per sheet in the Excel workbook stored as separate elements in a list
#'
#' @export
#'
read_xl_sheets <- function(file_name = NULL) {
  # Error out if no file name is provided
  if(base::is.null(file_name)) stop("No file provided")

  # For a given Excel file
  excel_data <- file_name %>%
    # Retrieve the names of all of the sheets
    readxl::excel_sheets() %>%
    # Name each list element by the corresponding sheet names
    purrr::set_names() %>%
    # For each name, read in the sheet to that list element
    purrr::map(readxl::read_excel, path = file_name)

  # Return that list
  return(excel_data)
}

#' @title Read Formatting of All Sheets in an Excel Workbook
#'
#' @description Retrieves all sheets of a Microsoft Excel workbook and identifies the formatting of each value (including column headers and blank cells).
#'
#' @param file_name (character) Name of (and path to) the Excel workbook
#'
#' @return (data frame) One row per cell in the dataframe with a column for each type of relevant formatting and its 'address' within the original Excel workbook
#'
#' @export
#'
read_xl_format <- function(file_name = NULL){

  # Error out if no file name is provided
  if(base::is.null(file_name)) stop("No file provided")

# Otherwise, identify contents and format of all sheets
contents <- tidyxl::xlsx_cells(path = file_name)
formats <- tidyxl::xlsx_formats(path = file_name)

# Now let's pare down the contents dataframe
output <- contents %>%
  dplyr::mutate(
    # Coerce non-character cells into characters
    error_char = base::as.character(error),
    logical_char = base::as.character(logical),
    numeric_char = base::as.character(numeric),
    date_char = base::as.character(date),
    # So that we can `coalesce()` them into a single column
    cell_contents = dplyr::coalesce(error_char, logical_char, numeric_char, date_char, character),
    # Now retrieve necessary formatting information
    bold = formats$local$font$bold[.$local_format_id],
    italic = formats$local$font$italic[.$local_format_id],
    underline = formats$local$font$underline[.$local_format_id],
    font_size = formats$local$font$size[.$local_format_id],
    font_color = formats$local$font$color$rgb[.$local_format_id],
    cell_color = formats$local$fill$patternFill$bgColor$rgb[.$local_format_id]
  ) %>%
  dplyr::select(sheet, address, row, col, cell_contents, comment,
                formula, bold, italic, underline, font_size,
                font_color, cell_color)

# And return that output dataframe
return(base::as.data.frame(output))

}
