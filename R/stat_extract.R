#' @title Extract Summary Statistics from Model Fit Object
#' 
#' @description Accepts model fit object and extracts core statistical information. This includes P value, test statistic, degrees of freedom, etc. Currently accepts the following model types: `nlme::lme`, `lmerTest::lmer`, or `RRPP::trajectory.analysis`
#' 
#' @param mod_fit (lme, trajectory.analysis) Model fit object of supported class
#' @param traj_angle (character) Either "deg" or "rad" for whether trajectory analysis angle information should be extracted in degrees or radians. Only required if model is trajectory analysis
#' 

stat_extract <- function(mod_fit = NULL, traj_angle = "deg"){
  # Global Checks ----
  # Squelch visible bindings note
  
  
  # Error out if the model is not provided
  if(is.null(mod_fit))
    stop("Model fit must be supplied")
  
  # Error out if model is not of any supported class
  if(
    methods::is(object = mod_fit, class2 = "lmerModLmerTest") != TRUE &
    methods::is(object = mod_fit, class2 = "lme") != TRUE &
    methods::is(object = mod_fit, class2 = "trajectory.analysis") != TRUE
    )
  stop("Model type is not supported")
  
  # Linear Mixed-Effects Model (`lmerTest::lmer`) ----
  if(methods::is(object = mod_fit, class2 = "lmerModLmerTest") == TRUE){
    
    # Extract summary from data
    summary_v1 <- base::as.data.frame(base::summary(mod_fit)$coefficients)
    
    # Wrangle to simplify columns
    stat_out <- summary_v1 %>%
      # Add terms as a column
      dplyr::mutate(Term = row.names(summary_v1),
                    .before = dplyr::everything()) %>%
      # Rename some columns
      dplyr::rename(Std_Error = `Std. Error`,
                    DF = df,
                    T_Value = `t value`,
                    P_Value = `Pr(>|t|)`)
  }
  
  # Linear Mixed-Effects Model (`nlme::lme`) ----
  if(methods::is(object = mod_fit, class2 = "lme") == TRUE){
    
    # Grab summary of the fit object
    summary_v1 <- base::summary(mod_fit)
    
    # Grab all relevant bits
    stat_out <- cbind(data.frame("Term" = rownames(summary_v1$tTable),
                                 "N_obs" = summary_v1$dims$N),
                      summary_v1$tTable) %>%
      # Rename some of those columns
      dplyr::rename(Std_Error = Std.Error,
                    T_Value = `t-value`,
                    P_Value = `p-value`)
  }
  
  # Multiple Regression on distance Matrices (MRM) ----
  
  ### Issue with `ecodist` package that needs resolving before we can continue here
  
  # Trajectory Analysis ----
  if(methods::is(object = mod_fit, class2 = "trajectory.analysis") == TRUE){
    
    # Error out for improper angle specification
    if(!traj_angle %in% c("deg", "rad"))
      stop("Angle type must be one of 'rad' or 'deg'. See `?RRPP::summary.trajectory.analysis`")
    
    # Strip out summary models for distance, shape, and angle
    dist <- summary(object = mod_fit, attribute = "MD")
    shape <- summary(object = mod_fit, attribute = "SD")
    angle <- summary(object = mod_fit, attribute = "TC", angle.type = traj_angle)
    
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
    if(is.null(mod_fit$SD)){
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
    stat_out <- dist_v2 %>%
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
    }
  
  # Final Wrangling ----
  
  # Drop row names (if any exist)
  rownames(stat_out) <- NULL
  
  # Return that object
  return(stat_out) 
}
