# Set of tools to import csv files into R


#' Read file from Google Drive using the google id hash
#' addapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R
#'
#' @param file_id_gdrive (character) Google Drive csv file id A character
#' @param skipper (integer) Skip first lines An integer
#' @param gdrive_url (character) base URL An character
#'
#' @return data frame with the csv content A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' my_data <- read.csv.gdrive("0B7AABlvKD6WjSTY3YUVKZ1AwLWs")
#' }
#'
read.csv.gdrive <- function (file_id_gdrive, skipper = 0, gdrive_url = "https://drive.google.com/uc?export=download&id=")
{
  download.link <- paste0(gdrive_url, file_id_gdrive)
  data.table <- read.csv(file = download.link, header = T,
                         skip = skipper)
  if (colnames(data.table)[[1]]== "X..DOCTYPE.html."){ data.table <- NULL
  print('could not retrieve CSV content')}
  else {data.table}
  return(data.table)
}

#' Read file from Google Drive using the google id hash using readr::read_csv
#'
#' @param file_id_gdrive Google Drive csv file id A character
#' @param skipper Skip first lines An integer
#' @param gdrive_url base URL An character
#'
#' @return data frame with the csv content A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' my_data <- read_csv_gdrive("0B7AABlvKD6WjSTY3YUVKZ1AwLWs")
#' }
#'
read_csv_gdrive <- function (file_id_gdrive, skipper = 0, gdrive_url = "https://drive.google.com/uc?export=download&id=")
{
  download.link <- paste0(gdrive_url, file_id_gdrive)
  data.table <- read_csv(file = download.link, header = T,
                         skip = skipper)
  if (colnames(data.table)[[1]]== "X..DOCTYPE.html."){ data.table <- NULL
  print('could not retrieve CSV content')}
  else {data.table}
  return(data.table)
}
