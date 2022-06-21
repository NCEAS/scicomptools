#' @title Wrangle Zoom Webinar Report CSVs
#'
#' @description Zoom webinars produce report CSVs of (1) attendance and (2) post-webinar survey responses. Both of these can be produced with human-readable header content before getting to participant data; unfortunately, these headers completely destroy R's ability to comprehend the column names for the relevant bits so a function was necessary to wrangle both types of report. Additionally, this function identifies the job sector of attendees based on their email domain
#'
#' @param file_name (character) Name of (and path to) the CSV output from Zoom webinar
#' @param report_type (character) Either "attendance" or "survey" depending on which report type is in the CSV
#'
#' @importFrom magrittr %>%
#'
#' @return dataframe containing tidied (and machine-readable) webinar report. Also drops identifying information in attendance report (no such information in survey response).
#' @export
#'

zoom_webinar_fix <- function(file_name, report_type = "attendance") {
  # Squelch visible bindings note
  row_num <- `Attendee Report` <- cutoff <- `...2` <- . <- NULL
  contents <- Attended <- `Join Time` <- `join_streamlined_1` <- NULL
  `join_streamlined_2` <- Email <- webinar_day <- email_suffix <- NULL
  domain <- participant_num <- suffix_ct <- time_submit <- NULL
  `Survey Report` <- webinar_day_simp <- participation <- NULL
  heard_where <- heard_other_text <- webinar_days_count <- NULL
  email <- name_user <- webinar_useful <- feedback <- NULL

  # Error out if inappropriate `report_type` is specified
  if(!report_type %in% c('attendance', 'survey'))
    stop("'report_type' incorrectly specified. Must be one of 'attendance' or 'survey'")

  # If the report being retrieved is an attendance report do the following:
  ## Attendance reports ----
  if(report_type == "attendance") {

    # Read in the attendance data
    headcount_raw <- suppressWarnings(utils::read.csv(file = file_name))

    # If the download has the header on it, remove the header
    if (names(headcount_raw)[1] == "Attendee Report"){

      headcount_partial <- headcount_raw %>%
        # Remove all rows above the 'Attendee Details' entry (that's all bad header content)
        dplyr::mutate(row_num = 1:nrow(.),
                      cutoff = row_num[`Attendee Report` == "Attendee Details"]) %>%
        dplyr::filter(row_num > (cutoff + 1)) %>%
        # Remove commas from all date columns
        dplyr::mutate(contents = gsub(", 20", " 20", `...2`)) %>%
        # Break apart the data into the actual columns (the raw .csv is not tidily formatted)
        tidyr::separate(col = contents,
                        into = c("User Name (Original Name)", "First Name", "Last Name",
                                 "Email", "Registration Time", "Approval Status",
                                 "Join Time", "Leave Time", "Time in Session (minutes)",
                                 "Is Guest", "Country/Region Name"),
                        sep = ",", extra = 'merge', fill = 'right') %>%
        # Rename the first column
        dplyr::rename(Attended = `Attendee Report`)
    }

    # If not, just save the object as a different name
    if (names(headcount_raw)[1] != "Attendee Report"){
      headcount_partial <- headcount_raw
    }

    # Now we can continue with wrangling
    headcount_fix <- headcount_partial %>%
      # Keep only people who actually attended the webinar
      dplyr::filter(Attended == "Yes") %>%
      # Identify webinar day & make email all lowercase
      dplyr::mutate(
        `join_streamlined_1` = gsub(", ", " ", `Join Time`),
        `join_streamlined_2` = gsub('\\"', "",
                                    `join_streamlined_1`),
        webinar_day = stringr::str_sub(`join_streamlined_2`, 1, 11),
        email = tolower(Email)) %>%
      # Strip down to only needed columns
      dplyr::select(webinar_day, email) %>%
      # And drop duplicates
      base::unique() %>%
      # Count how many participants there were
      dplyr::mutate(participant_num = seq_along(email)) %>%
      # Separate the emails between prefix and suffix (i.e., around the @)
      tidyr::separate(col = email,
                      into = c('email_prefix', 'email_suffix'),
                      sep = '@') %>%
      # Then separate again to break apart the suffix
      tidyr::separate(col = email_suffix, into = c('suffix_p1', 'suffix_p2', 'suffix_p3', 'domain'), fill = 'left') %>%
      # Then re-combine the suffix parts preceding the final bit
      tidyr::unite(col = email, dplyr::contains('suffix_p'),
                   sep = '.', na.rm = T) %>%
      # Group by suffix type...
      dplyr::group_by(email, domain) %>%
      # ...and count attendees per email suffix (while retaining other columns)
      dplyr::summarise(webinar_day = webinar_day,
                       participant_num = participant_num,
                       suffix_ct = dplyr::n()) %>%
      # Ungroup and get the total participation (while retaining other columns)
      dplyr::ungroup() %>%
      dplyr::summarise(webinar_day = webinar_day,
                       email = email,
                       domain = domain,
                       suffix_ct = suffix_ct,
                       attendance = max(participant_num, na.rm = T)) %>%
      # And retain only unique rows
      base::unique()

    return(headcount_fix) }

  # If the report type is "survey" we'll need to handle the wonky header
  ## Survey reports ----
  if (report_type == "survey"){

    survey_fix <- suppressWarnings(utils::read.csv(file = file_name)) %>%
      # Remove commas from all date columns
      dplyr::mutate(contents = gsub(", 20", " 20", `...2`)) %>%
      # Break apart the data into the actual columns (the raw .csv is not tidily formatted)
      tidyr::separate(col = contents, into = c("name_user", "email_user", "time_submit", "heard_where", "heard_other_text", "webinar_useful", "feedback"), sep = ",", extra = 'merge', fill = 'right') %>%
      # Now strip down to only actual responses
      ## This incidentally removes the gross header rows so we're fine with that
      dplyr::filter(name_user == "Anonymous") %>%
      # Make all emails lowercase & find start date
      dplyr::mutate(webinar_day_simp = stringr::str_sub(time_submit, 2, 12),
                    participation = max(as.numeric(`Survey Report`), na.rm = T)) %>%
      # Keep only desired columns
      dplyr::select(webinar_day_simp, participation, webinar_useful, heard_where, heard_other_text, feedback) %>%
      # Count how responses per day
      dplyr::group_by(webinar_day_simp) %>%
      dplyr::mutate(webinar_days_count = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      # Identify maximum day (as this will certainly be the day of the webinar)
      dplyr::mutate(webinar_day = webinar_day_simp[which.max(webinar_days_count)], .before = webinar_day_simp) %>%
      # Remove now deprecated webinar timing columns
      dplyr::select(-webinar_day_simp, -webinar_days_count)

    # Finally, return the data
    return(survey_fix) }
}
