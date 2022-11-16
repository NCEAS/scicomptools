#' @title Extract Estimates From `nlme::lme` Model Fit Object
#' 
#' @description Extracts model terms, total sample size, estimates, standard errors, degrees of freedom, T values, and P values from a `nlme::lme` fit object
#' 
#' @param fit (lme) Fit object returned by `nlme::lme`
#' 
#' @export
#' 
nlme_extract <- function(fit = NULL){
  
  # Error out for missing fit
  if(is.null(fit))
    stop("`fit` must be specified")
  
  # Error out for inappropriate type of fit
  if(class(fit) != "lme")
    stop("`fit` must be class 'lme'")
  
  # Grab summary of the fit object
  table <- base::summary(fit)
  
  # Grab all relevant bits
  strip <- cbind(data.frame("Term" = rownames(table$tTable),
                            "N_obs" = table$dims$N),
                 table$tTable)
  
  # Drop old (flawed row names)
  rownames(strip) <- NULL
  
  # Return final table
  return(strip) }
