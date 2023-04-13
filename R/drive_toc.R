#' @title Identify all Folders within Specified Google Drive Folder
#'
#' @description Identifies all sub-folders within a user-supplied Drive folder (typically the top-level URL). Also allows for exclusion of folders by name; useful if a "Backups" or "Archive" folder is complex and a table of contents is unwanted for that folder(s).
#'
#' @param url (drive_id) Google Drive folder link modified by `googledrive::as_id` to be a true "Drive ID" (e.g., `url = as_id("url text")`)
#' @param ignore_names (character) Vector of name(s) of folder(s) to be excluded from list of folders
#' @param quiet (logical) Whether to message which folder it is currently listing (defaults to `FALSE`). Complex folder structures will take time to fully process but the informative per-folder message provides solace that this function has not stopped working
#' 
#' 
#' @return (node / R6) Special object class used by the `data.tree` package
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' 
#' @export
#' 
drive_toc <- function(url = NULL, ignore_names = NULL, quiet = FALSE){
  # Squelch visible bindings note
  name <- id <- listed <- parent_path <- path <- NULL
  
  # Error out for missing folder URL
  if(is.null(url))
    stop("URL must be provided")
  
  # Also if URL is not wrapped with `googledrive::as_id`
  if(!methods::is(object = url, class = "drive_id"))
    stop("URL must be a Drive ID (wrap URL with `googledrive::as_id`")
  
  # Identify top-level folders
  top_conts <- googledrive::drive_ls(path = url, type = "folder", recursive = FALSE)
  
  # Duplicate to preserve that object
  contents <- top_conts %>%
    # Make columns for whether that directory has been listed and its path
    dplyr::mutate(listed = FALSE,
                  parent_path = ".")
  
  # While any folders are not identified
  while(FALSE %in% contents$listed){
    
    # Remove any folders marked to be ignored (if any are)
    if(length(x = ignore_names) != 0){
      contents <- dplyr::filter(contents, !name %in% ignore_names)
    }
    
    # Loop across these folders to identify their subfolders
    for(k in 1:nrow(contents)){
      
      # Skip if already listed
      if(contents[k,]$listed == TRUE & quiet != TRUE){ 
        message("Skipping already listed folder (folder ", k, ")") 
        
        # Otherwise...
      } else {
        
        # List out the folders within that folder
        sub_conts <- googledrive::drive_ls(path = googledrive::as_id(contents[k,]$id), 
                                           type = "folder", recursive = FALSE) %>%
          # Add the columns we added to the first `drive_ls` return
          dplyr::mutate(listed = FALSE,
                        parent_path = paste0(contents[k,]$parent_path, "/", contents[k,]$name))
        
        # Combine that output with the contents object
        contents %<>%
          # Row bind nested folders
          dplyr::bind_rows(sub_conts) %>%
          # Flip this folder's "listed" entry to TRUE
          dplyr::mutate(listed = ifelse(test = (id == contents[k,]$id),
                                        yes = TRUE,
                                        no = listed))
        
        # Message success (if `quiet` is FALSE)
        if(quiet != TRUE){ message("Subfolders identified for folder ", k) }
      } } # Close conditional & `for` loop
    
  } # Close `while` loop
  
  # Process this a little
  paths <- contents %>%
    # Complete the path by adding in each folder's name
    dplyr::mutate(path = paste0(parent_path, "/", name)) %>%
    # Strip out ONLY paths
    dplyr::pull(var = path)
  
  # Identify number of folders
  folder_num <- base::max(stringr::str_count(string = paths, pattern = "/")) + 1
  
  # Wrangle the path object as needed for `data.tree::as.Node`
  contents_df <- as.data.frame(paths) %>%
    # Make into a dataframe where each path is a row and each column is a folder
    tidyr::separate_wider_delim(cols = paths, delim = '/', too_few = "align_start",
                                names = paste0("V", 1:folder_num)) %>%
    # Also re-gain the full path string
    dplyr::mutate(pathString = paths)
  
  # Strip out folder paths
  drive_tree <- data.tree::as.Node(contents_df)
  
  # Return this
  return(drive_tree) }
