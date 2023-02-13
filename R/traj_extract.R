#' @title Extract Summary Statistics From `RRPP::trajectory.analysis` Model Fit Object
#' 
#' @description Extracts summary tables for distance, shape, and angle results identified by `RRPP::trajectory.analysis`. Returns as a flat dataframe for convenience
#' 
#' @param traj_mod (trajectory.analysis) Object returned by `RRPP::trajectory.analysis`
#' @param angle_type (character) Either "deg" or "rad" for what units to use for trajectory analysis angle metric. See `?RRPP::summary.trajectory.analysis` for more information
#' 
#' @return (data.frame) Flat table including various useful summary metrics for the three trajectory analysis metrics
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
traj_extract <- function(traj_mod = NULL, angle_type = "deg"){
  # Squelch no visible bindings note
  d <- `UCL (95%)` <- Z <- `Pr > d` <- r <- `Pr > angle` <- NULL
  metric <- angle_r <- P_Value <- NULL
  
  # Error out for missing trajectory model
  if(is.null(traj_mod) | is.null(angle_type))
    stop("All arguments must be supplied")
  
  # Error out if input is wrong class
  if(methods::is(object = traj_mod, class2 = "trajectory.analysis") != TRUE)
    stop("Trajectory model must be class 'trajectory.analysis'")
  
  # Error out if angle_type is not supported entry
  if(!angle_type %in% c("deg", "rad"))
    stop("angle_type must be one of 'rad' or 'deg'. See `?RRPP::summary.trajectory.analysis`")
  
  # Strip out summary models for distance, shape, and angle
  dist <- summary(object = traj_mod, attribute = "MD")
  shape <- summary(object = traj_mod, attribute = "SD")
  angle <- summary(object = traj_mod, attribute = "TC", angle.type = angle_type)
  
  # Wrangle the distance output
  dist_v2 <- tibble::as_tibble(as.list(dist$x$PD$obs)) %>%
    # Now bring in remaining summary values
    cbind(dist$summary.table) %>%
    # Make a column identifying which metric this is
    dplyr::mutate(metric = "distance") %>%
    # Rename some of these columns
    dplyr::rename(diff = d,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > d`)
  
  # Check for whether shape is there
  if(is.null(traj_mod$SD)){
    # Make an empty dummy df if shape isn't included in the shape output
    shape_v2 <- data.frame("diff" = NA,
                           "UCL_95perc" = NA,
                           "Z_Score" = NA,
                           "P_Value" = NA,
                           "metric" = "shape") 
    # Otherwise...
  } else {
    # Wrangle the output
    shape_v2 <- shape$summary.table %>%
      # Make a metric column
      dplyr::mutate(metric = "shape") %>%
      # Rename columns
      dplyr::rename(diff = d,
                    UCL_95perc = `UCL (95%)`,
                    Z_Score = Z,
                    P_Value = `Pr > d`) }
  
  # Wrangle angle output
  angle_v2 <- angle$summary.table %>%
    # Make a metric column
    dplyr::mutate(metric = "angle") %>%
    # Rename columns
    dplyr::rename(angle_r = r,
                  diff = angle,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > angle`)
  
  # Combine these extracted objects
  combo <- dist_v2 %>%
    # Bind distance, shape, and angle together by column name
    dplyr::bind_rows(shape_v2, angle_v2) %>%
    # Reorder columns
    dplyr::relocate(metric, .before = dplyr::everything()) %>%
    dplyr::relocate(angle_r:P_Value, .after = dplyr::everything()) %>%
    # Identify whether each metric was significant
    dplyr::mutate(significance = dplyr::case_when(
      P_Value >= 0.05 ~ paste0(metric, "-NS"),
      is.na(P_Value) ~ paste0(metric, "-NULL"),
      TRUE ~ paste0(metric, "-sig")),
      .before = dplyr::everything())
  
  # Drop the row names
  rownames(combo) <- NULL
  
  # Return that object
  return(combo) }
