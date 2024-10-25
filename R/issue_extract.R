#' @title Export GitHub issues as PDF Files
#'
#' @description Exports specified GitHub issues as PDF files when given the URL of a GitHub repository. This function will export the first 10 issues as a default. 
#' 
#' @param repo_url (character) URL of the GitHub repository as a character string.
#' @param start (numeric) First GitHub issue to be exported. Default is issue number 1.
#' @param end (numeric) Last GitHub issue to be exported. Default is issue number 10.
#' @param export_folder (character) Name of the folder that will be created to contain the output PDF files. Default is "exported_issues". 
#' @param cookies (character) Optional file path to the cookies to load into the Chrome session.
#'
#' @return No return value, called for side effects
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Export GitHub issues #7080 through #7089 for the public `dplyr` repository 
#' issue_extract(repo_url = "https://github.com/tidyverse/dplyr",
#'               start = 7080,
#'               end = 7089,
#'               export_folder = "dplyr_issues")
#' }
#' \dontrun{
#' # Export GitHub issues #295 through #300 for a GitHub Enterprise repository
#' # First save your credentials as cookies (see link below for more details)
#' # https://github.com/rstudio/chromote/blob/main/README.md#websites-that-require-authentication
#' 
#' # Start new Chrome session
#' b <- ChromoteSession$new()
#' # Open interactive window
#' b$view()
#'
#' # Navigate to a random Github Enterprise issue
#' # Make sure to log in to GitHub Enterprise
#' b$Page$navigate("https://github.nceas.ucsb.edu/LTER/lter-wg-scicomp/issues/278")
#' 
#' # Save credentials as cookies
#' cookies <- b$Network$getCookies()
#' saveRDS(cookies, "cookies.rds")
#'
#' # Close the browser tab/window
#' b$close()
#' 
#' # After saving cookies, you should restart R
#' # Then read in the cookies and export the necessary issues 
#' issue_extract(repo_url = "https://github.nceas.ucsb.edu/LTER/lter-wg-scicomp",
#'               start = 295,
#'               end = 300,
#'               export_folder = "scicomp_issues",
#'               cookies = "cookies.rds")
#'}

issue_extract <- function(repo_url = NULL,
                          start = 1,
                          end = 10,
                          export_folder = "exported_issues",
                          cookies = NULL) {
  
  # Error out if the ending issue number is less than the starting issue number
  if (end <= start){
    stop("Ending issue number cannot be less than the starting issue number")
  }
  
  # Create sub-folder for exported issues
  dir.create(path = file.path(export_folder), showWarnings = F)
  
  # Start new Chrome session
  b <- chromote::ChromoteSession$new()
  # Open interactive window
  b$view()
  
  if (!base::is.null(cookies)) {
    # Read in and set cookies
    given_cookies <- base::readRDS(cookies)
    b$Network$setCookies(cookies = given_cookies$cookies)
  }
  
  for (i in start:end) {
    
    # If there's an extra slash at the end of the url, remove it just in case
    repo_url <- stringr::str_replace(repo_url, "\\/$", "")
    
    # Specify the url of interest
    issue_url <- paste0(repo_url, "/issues/", i)
    
    # NOTE: see the below link on how to load pages reliably
    # https://github.com/rstudio/chromote?tab=readme-ov-file#loading-a-page-reliably
    
    # Get the promise for the loadEventFired
    p <- b$Page$loadEventFired(wait_ = FALSE)
    
    # Navigate to the app that requires a login
    b$Page$navigate(issue_url, wait_ = FALSE)
    
    # Block until p resolves
    b$wait_for(p)
    
    # Get the navigation history so we can access the metadata
    hist <- b$Page$getNavigationHistory()
    
    # If the issue does not exist, skip this iteration
    if (stringr::str_detect(hist$entries[[hist$currentIndex + 1]]$title, "Page not found")) {
      next
      # Else if the issue exists, export its pdf
    } else {
      # Create the pdf name from the webpage title
      # Remove punctuation
      title_1 <- stringr::str_replace_all(hist$entries[[hist$currentIndex + 1]]$title, "[:punct:]", "")
      # Remove symbols
      title_2 <- stringr::str_replace_all(title_1, "[:symbol:]", "")
      # Extract only the necessary substring from the full title
      title_3 <- stringr::str_extract(title_2, ".*(?=[:blank:]{2,3}(Issue|Pull))")
      # Replace spaces with underscores
      title_4 <- stringr::str_replace_all(title_3, "[:space:]{1,2}", "_")
      
      # Issue number padding
      if (i < 10) {
        num <- paste0("000", i)
      } else if (i < 100) {
        num <- paste0("00", i)
      } else if (i < 1000) {
        num <- paste0("0", i)
      } else {
        num <- paste0(i)
      }
      
      # Grab the name of the repo
      repo_name <- stringr::str_extract(repo_url, "([^\\/]+$)")
      
      # Attach the issue number to the pdf name
      pdf_name <- paste0(repo_name, "_issue_", num, "_", title_4, ".pdf")
      
      message(paste("Exporting issue", i))
      
      # Export the GitHub issue webpage as a pdf
      b$screenshot_pdf(
        filename = file.path(export_folder, pdf_name),
        display_header_footer = TRUE,
        print_background = TRUE
      )
      
    }
    
  }
  # Close the browser tab/window
  b$close()
  
}
