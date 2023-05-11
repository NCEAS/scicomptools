#' @title Check Token Status
#'
#' @description To make some direct-from-API workflows functional (e.g., Qualtrics surveys, etc.). It is necessary to quickly test whether a given R session "knows" the API token. This function returns an error if the specified token type isn't found and prints a message if one is found
#'
#' @param api (character) API the token is for (currently only supports "qualtrics" and "github")
#' @param secret (logical) Whether to include the token character string in the success message. FALSE prints the token, TRUE keeps it secret but returns a success message
#'
#' @export
#' 
#' @examples
#' # Check whether a GitHub token is attached or not
#' token_check(api = "github", secret = TRUE)
#' 
#' # Check whether a Qualtrics token is attached or not
#' token_check(api = "qualtrics", secret = TRUE)
#' 
token_check <- function(api = "qualtrics", secret = TRUE){

  # Error out for unsupported `api`
  if(!base::tolower(api) %in% c("qualtrics", "github"))
    stop("Token API not recognized. Must be 'qualtrics' or 'github'")

  # Grab the specified token
  if(base::tolower(api) == "qualtrics"){
    token_string = base::Sys.getenv("QUALTRICS_API_KEY") }

  if(base::tolower(api) == "github"){
   token_string = gitcreds::gitcreds_get()[["password"]]
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

