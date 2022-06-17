#' @title Read All Sheets from an Excel Workbook
#'
#' @description Retrieves all of the sheets in a given Microsfot Excel workbook and stores them as elements in a list. Note that the guts of this function were created by the developers of `readxl` and a tutorial-ized version of this function can be found here: https://readxl.tidyverse.org/articles/readxl-workflows.html (Note however that they didn't have it as a function)
#'
#' @param filename (character) Name of the Excel workbook
#'
#' @export
#'
read_full_excel <- function(filename = NULL) {
  # Error out if no filename is provided
  if(base::is.null(filename)) stop("No file provided")

  # For a given Excel file
  excel_data <- filename %>%
    # Retrieve the names of all of the sheets
    readxl::excel_sheets() %>%
    # Name each list element by the corresponding sheet names
    purrr::set_names() %>%
    # For each name, read in the sheet to that list element
    purrr::map(readxl::read_excel, path = filename)

  # Return that list
  return(excel_data)
}

#' Export csv files out of an Excel workbook including formatting and formulas
#'
#' @param filename (character)  Name of the Excel workbook
#'
#' @export
#'
output_to_csv <- function(filename) {
  my_sheets <- read_full_excel(filename)
  workbook_name <- gsub("^(.*\\/)*(.*)\\.xlsx$","\\2", filename)
  master_dir <- sprintf("./%s/", workbook_name)
  message("\n")
  for (i in c(1:length(my_sheets))) {
    df <- data.frame(my_sheets[i])
    sheet_name <- sprintf("sheet_%d", i)
    dir_name <- sprintf("%s%s/", master_dir, sheet_name)
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)

    stat_output_name <- sprintf("%ssummary_stats.txt", dir_name)
    R.utils::captureOutput(Hmisc::describe(df), file=stat_output_name)

    output_name <- sprintf("%s%s.csv", dir_name, sheet_name)
    R.utils::write.table(df,file=output_name, row.names=FALSE,col.names=FALSE, sep = ",")

    output_sheet(filename, dir_name, names(my_sheets[i]), i)

  }
  message("\n")
}

#' Wrapper function around the other functions to extract information from excel sheet into separate csv files
#'
#' @param filename (character)  Name of the Excel workbook
#' @param directory (character)  Path to the output directory
#' @param sheet (character)  Excel sheet name
#' @param n (integer)  Sheet number
#'
#' @export
#'
output_sheet <- function(filename, directory, sheet, n) {
  x <-  tidyxl::xlsx_cells(filename)

  y <-  tidyxl::xlsx_formats(filename)

  output_value(x, y, directory, sheet)
  output_bold(x, y, directory, sheet)
  output_italic(x, y, directory, sheet)
  output_underline(x, y, directory, sheet)
  output_fontcolor(x, y, directory, sheet)
  output_font(x, y, directory, sheet)
  output_bgcolor(x, y, directory, sheet)
  output_formula(x, y, directory, sheet)
  output_comment(x, y, directory, sheet)

  confirmation <- sprintf("sheet %d outputted to %s%s.csv", n, directory, sheet)
  message(confirmation)
}

#'  Extract data information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_value <- function(contents, formats, directory, sheet) {
  values <- contents[[sheet]][["character"]]

  output <- matrix(data=values, nrow=nrow(contents[[sheet]]),ncol=ncol(contents[[sheet]]), byrow = TRUE)

  #if clause for values
  if(return(values=='NA')){
    values <- contents[[sheet]][["content"]]
  } else {
    if(values!='NA'){
      values <- contents[[sheet]][["character"]]
    }
  }

  output <- data.frame(output)
  output_name <- sprintf("%s%s.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, col.names=FALSE, sep = ",")
}



#'  Extract bold formatting information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_bold <- function(contents, formats, directory, sheet) {
  bolds <- contents[[sheet]]$local_format_id %in% which(formats$local$font$bold)
  output <- matrix(data=bolds, nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]), byrow = TRUE)
  output <- data.frame(output)
  output_name <- sprintf("%s%s_bold.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE,col.names=FALSE, sep = ",")
}



#' Extract italic formatting information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_italic <- function(contents, formats, directory, sheet) {
  italics <- contents[[sheet]]$local_format_id %in% which(formats$local$font$italic)
  output <- matrix(data=italics, nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]), byrow = TRUE)
  output <- data.frame(output)
  output_name <- sprintf("%s%s_italic.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, col.names=FALSE,sep = ",")
}



#' Extract underline formatting information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_underline <- function(contents, formats, directory, sheet) {
  output <- matrix(nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]))
  for (i in c(1:length(contents[[sheet]]$address))) {
    output[contents[[sheet]]$row[i],contents[[sheet]]$col[i]] <- formats$local$font$underline[contents[[sheet]]$local_format_id[i]]
  }
  output <- data.frame(output)
  output_name <- sprintf("%s%s_underline.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE,col.names=FALSE, sep = ",")
}

#' Extract font colors information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_fontcolor <- function(contents, formats, directory, sheet) {
  output <- matrix(nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]))
  for (i in c(1:length(contents[[sheet]]$address))) {
    output[contents[[sheet]]$row[i],contents[[sheet]]$col[i]] <- formats$local$font$color$rgb[contents[[sheet]]$local_format_id[i]]
  }
  output <- data.frame(output)
  output_name <- sprintf("%s%s_fontColor.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, col.names=FALSE,sep = ",")
}

#' Extract font type information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_font <- function(contents, formats, directory, sheet) {
  output <- matrix(nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]))
  for (i in c(1:length(contents[[sheet]]$address))) {
    output[contents[[sheet]]$row[i],contents[[sheet]]$col[i]] <- formats$local$font$name[contents[[sheet]]$local_format_id[i]]
  }
  output <- data.frame(output)
  output_name <- sprintf("%s%s_font.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, col.names=FALSE,sep = ",")
}

#' Extract background colors information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_bgcolor <- function(contents, formats, directory, sheet) {
  output <- matrix(nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]))
  for (i in c(1:length(contents[[sheet]]$address))) {
    output[contents[[sheet]]$row[i],contents[[sheet]]$col[i]] <- formats$local$fill$patternFill$fgColor$rgb[contents[[sheet]]$local_format_id[i]]
  }
  output <- data.frame(output)
  output_name <- sprintf("%s%s_bgColor.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, col.names=FALSE,sep = ",")
}

#' Extract formulas information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_formula <- function(contents, formats, directory, sheet) {
  formulas <- contents[[sheet]][["formula"]]
  output <- matrix(data=formulas, nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]), byrow = TRUE)
  output <- data.frame(output)
  output_name <- sprintf("%s%s_formula.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE, sep = ",")
}

#'  Extract comments information to csv
#'
#' @param contents (data frame) values stored in the Excel sheet
#' @param formats (data frame) values stored in the Excel sheet
#' @param directory (character) path to the folder where to save the csv files
#' @param sheet (character) name of the sheet to process
#'
#' @export
#'
output_comment <- function(contents, formats, directory, sheet) {
  comments <- contents[[sheet]][["comment"]]
  output <- matrix(data=comments, nrow=max(contents[[sheet]][["row"]]),ncol=max(contents[[sheet]][["col"]]), byrow = TRUE)
  output <- data.frame(output)
  output_name <- sprintf("%s%s_comment.csv", directory, sheet)
  R.utils::write.table(output,file=output_name, row.names=FALSE,col.names=FALSE, sep = ",")
}
