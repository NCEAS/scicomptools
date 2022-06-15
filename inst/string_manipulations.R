library(googlesheets)
library(stringr)
library(dplyr)


# Google Sheet name
file_name <- "[LTER -SOM] Participants list"
sheet_name <- "Sheet1" 

# email regex
# From https://www.shellhacks.com/regex-find-email-addresses-file-grep/
regexpression <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}\\b"


## Read the mailing list ----
# List files 
participants <- gs_title(file_name)
mailinglist <- gs_read(participants, ws = sheet_name)


## Extract the email addresses
df <- mailinglist %>%
  mutate(email = str_extract(.[[1]], regexpression))

## Upload the results
# Write the file as csv
write.csv(df,"~/Desktop/lter_som_participants.csv", row.names = FALSE)
# Upload the csv to Google Drive
gs_upload("~/Desktop/lter_som_participants.csv")

#Change image path in md files

my_string <- "in case you are obsessed by that ![adapted from https://www.atlassian.com/git/tutorials/git-merge](atlassian_branches_sketch.png) in case you want more turotieal"
  
image_string <- str_extract(my_string,"!\\[[^\\]]+\\]\\([^)]+\\)")

image_path <- str_split(image_string,"\\(")[[1]][[2]] %>% 
  str_sub(.,1,str_length(.)-1) %>% 
  str_split(.,"/")

# Build the new path
path_sub <- paste0("![](images/",image_path[[1]],")")

# Replace the path
str_replace(my_string,"!\\[[^\\]]+\\]\\([^)]+\\)", path_sub )

