#' @title Define Local or Remote Working Directories
#' 
#' @description While working on the same script both in a remote server and locally on your home computer, defining file paths can be unwieldy and may even require duplicate scripts--one for each location--that require maintenance in parallel. This function allows you to define whether you are working locally or not and specify the path to use in either case.
#' 
#' @param local (logical) Whether you are working locally or on a remote server
#' @param local_path (character) File path to use if `local` is `TRUE` (defaults to `getwd()`)
#' @param remote_path (character) File path to use if `local` is `FALSE`
#' 
#' @return (character) Either the entry of `local_path` or `remote_path` depending on whether `local` is set as true or false
#' 
#' @export
#' 
#' @examples
#' # Set two working directory paths to toggle between
#' \dontrun{
#' # If you are working in your local computer, set `local` to "TRUE"
#' wd_loc(local = TRUE,
#'        local_path = getwd(),
#'        remote_path = file.path("path on server"))
#'        
#' # If you are working in a remote server, set `local` to "FALSE"
#' wd_loc(local = FALSE,
#'        local_path = getwd(),
#'        remote_path = file.path("path on server"))
#'}       
wd_loc <- function(local = TRUE, local_path = getwd(), remote_path = NULL){
  
  # If local isn't logical, coerce to TRUE
  if(!is.logical(local)){
    message("`local` must be either TRUE or FALSE. Coercing to TRUE")
    local <- TRUE}
  
  # If local is FALSE but remote_path isn't specified, error out
  if(local == FALSE & is.null(remote_path))
    stop("If working on a remote server, `remote_path` must be specified")
  
  # Pick user-specified path
  if(local == TRUE){ path <- local_path }
  if(local == FALSE){ path <- remote_path }
  
  # Return chosen path
  return(path) 
}
