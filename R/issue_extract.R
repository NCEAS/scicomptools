#' @title Export GitHub issues as PDF Files
#'
#' @description Exports specified GitHub issues as PDF files when given the URL of a GitHub repository. This function will export the first 10 issues as a default. 
#' 
#' @param repo_url (character) URL of the GitHub repository as a character string.
#' @param issue_nums (numeric) Numeric vector of the issue numbers to be exported. Default is issue number 1 through 10. 
#' @param export_folder (character) Name of the folder that will be created to contain the output PDF files. Default is "exported_issues". 
#' @param cookies (character) Optional file path to the cookies to load into the Chrome session. This is only required when accessing GitHub repositories that require a login. See this link for more details: https://github.com/rstudio/chromote/blob/main/README.md#websites-that-require-authentication.
#' @param quiet (logical) Whether to silence informative messages while issues are being exported. Default is FALSE.  
#'
#' @return No return value, called for side effects
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Export GitHub issue #7000 and #7080 through #7089 for the public `dplyr` repository 
#' issue_extract(repo_url = "https://github.com/tidyverse/dplyr",
#'               issue_nums = c(7000, 7080:7089),
#'               export_folder = "dplyr_issues")
#' }
#' 

issue_extract <- function(repo_url = NULL,
                          issue_nums = 1:10,
                          export_folder = NULL,
                          cookies = NULL,
                          quiet = FALSE) {
  # Error out if no URL was provided
  if (base::is.null(repo_url)) {
    stop("URL not provided")
  }
  
  # Error out if the URL is not a GitHub URL
  if (!stringr::str_detect(repo_url, "[Gg][Ii][Tt][Hh][Uu][Bb]")) {
    stop("URL is not a valid GitHub repo URL")
  }
  
  # Error out if the vector of GitHub issue numbers is not a numeric vector
  if (!base::is.vector(issue_nums) | !base::is.numeric(issue_nums)) {
    stop("Vector of issue numbers is not a numeric vector")
  }
  
  # Error out if the vector of GitHub issue numbers contains decimal numbers
  if (base::any(stringr::str_detect(issue_nums, "\\."))) {
    stop("Vector of issue numbers cannot contain decimal numbers")
  }
  
  # If no export folder name is specified, default to "exported_issues"
  if (base::is.null(export_folder) | length(export_folder) == 0) {
    message("No export folder name specified. Will default to 'exported_issues'.")
    export_folder <- "exported_issues"
  }
  
  # Warn user if the `quiet` argument is not logical and coerce to FALSE
  if (!is.logical(quiet)){
    warning("`quiet` is not a logical, coercing to FALSE")
    quiet <- FALSE
  }
  
  # Create sub-folder for exported issues
  dir.create(path = file.path(export_folder), showWarnings = F)
  
  # Start new Chrome session
  b <- chromote::ChromoteSession$new()
  # Open interactive window
  b$view()
  
  # If a file is provided for cookies...
  if (!base::is.null(cookies)) {
    # Error out if cookies are not saved as a .rds file
    if (!stringr::str_detect(cookies, "\\.[Rr][Dd][Ss]")) {
      # Close the browser tab/window
      b$close()
      stop("Cookies need to be saved in the .rds extenstion")
      # Otherwise read in cookies if the file has the correct extension
    } else {
      given_cookies <- base::readRDS(cookies)
    }
    
    # Error out if the file does not contain the actual components making up cookies
    if (!base::isTRUE(stringr::str_detect(names(given_cookies), "cookies"))) {
      # Close the browser tab/window
      b$close()
      stop("Cookies need to contain a list element called 'cookies'")
    } else {
      # Otherwise set the cookies if the file has the correct components
      b$Network$setCookies(cookies = given_cookies$cookies)
    }
  }
  
  for (i in issue_nums) {
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
      pdf_name <-
        paste0(repo_name, "_issue_", num, "_", title_4, ".pdf")
      
      if (quiet == TRUE) {
        # Export the GitHub issue webpage as a pdf
        b$screenshot_pdf(
          filename = file.path(export_folder, pdf_name),
          display_header_footer = TRUE,
          print_background = TRUE
        )
      } else {
        message(paste("Exporting issue", i))
        
        # Export the GitHub issue webpage as a pdf
        b$screenshot_pdf(
          filename = file.path(export_folder, pdf_name),
          display_header_footer = TRUE,
          print_background = TRUE
        )
      }
    }
    
  }
  # Close the browser tab/window
  b$close()
  
}
