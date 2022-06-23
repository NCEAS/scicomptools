#' @title Check Token Status
#'
#' @description To make some direct-from-API workflows functional (e.g., Qualtrics surveys, etc.). It is necessary to quickly test whether a given R session "knows" the API token. This function returns an error if the specified token type isn't found and prints a message if one is found
#'
#' @param api (character) API the token is for (currently only supports "qualtrics")
#' @param secret (logical) Whether to include the token character string in the success message. FALSE prints the token, TRUE keeps it secret but returns a success message
#'
#' @export
#'
token_check <- function(api = "qualtrics", secret = TRUE){

  # Error out for unsupported `api`
  if(!base::tolower(api) %in% c("qualtrics"))
    stop("Token API not recognized. Must be 'qualtrics'")

  # Grab the specified token
  if(base::tolower(api) == "qualtrics"){
    token_string = base::Sys.getenv("QUALTRICS_API_KEY")
  }

  # Print message if no token found
  if(base::nchar(x = token_string) <= 1)
    base::message("No token detected. Calls to this API will not work")

  # Depending on what secret is set to, print a success message
  if(base::nchar(x = token_string) > 1 & secret == TRUE)
    base::message("API Key identified! Set `secret` to FALSE if token printing is desired")

  if(base::nchar(x = token_string) > 1 & secret == FALSE)
    base::message("API Key identified! Token string is '", token_string, "'")
}

