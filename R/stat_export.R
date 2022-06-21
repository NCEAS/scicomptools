#' @title Export Summary Statistics from a Model Fit
#'
#' @description Accepts a model fit object for one of a set of models and saves a CSV of summary statistics to the desired location. Allows user to define the number of digits for each summary statistic if desired
#'
#' @param model_obj output of `lmerTest::lmer()`, `stats::lm()`, `stats::nls()`, `stats::t.test()`
#' @param model_type character, one of "lmer", "lm", "nls" or "t-test"
#' @param output_path character, file path to save file to
#' @param output_name character, desired name of file (defaults to model type and system time)
#' @param est_dig numeric, number of digits to round estimate to
#' @param se_dig numeric, number of digits to round standard error to
#' @param df_dig numeric, number of digits to round degrees of freedom to
#' @param t_dig numeric, number of digits to round T value to
#' @param p_dig numeric, number of digits to round P value to
#'
#' @export
#'
stat_export <- function(model_obj = NULL, model_type = "lmer",
                        output_path = getwd(),
                        output_name = paste0(model_type, "_", Sys.time(), "_model.csv"),
                        est_dig = 2, se_dig = 2, df_dig = 2,
                        t_dig = 2, p_dig = 4){
  # Squelch visible bindings note
  Estimate <- `Std. Error` <- df <- `t value` <- NULL
  `Pr(>|t|)` <- term <- SE <- p <- mod_t <- NULL

  # Error out if model isn't provided
  if(base::is.null(model_obj)) stop("Model object is required")

  # If the model type is not one of the accepted four, error out
  if(!model_type %in% c("lmer", "lm", "nls", "t.test"))
    stop("Model type not supported. Please supply one of 'lmer', 'lm', 'nls', or 't.test' to `model_type` argument.")

  # Otherwise, process the supplied model type
  # lmer model ----
  if(model_type == "lmer"){

    # Extract summary from data
    lmer_smry <- base::summary(model_obj)

    # Strip out coefficients
    lmer_coef <- base::as.data.frame(lmer_smry$coefficients)

    # Calculate new columns
    lmer_new <- dplyr::mutate(.data = lmer_coef,
                              term = base::row.names(lmer_coef),
                              Estimate = base::round(Estimate, digits = est_dig),
                              SE = base::round(`Std. Error`, digits = se_dig),
                              df = base::round(df, digits = df_dig),
                              t = base::round(`t value`, digits = t_dig),
                              p = base::round(`Pr(>|t|)`, digits = p_dig))

    # Get a final version of just desired columns in the correct order
    lmer_actual <- dplyr::select(.data = lmer_new, term,
                                 Estimate, SE, df, t, p)

    # Remove rownames
    base::rownames(lmer_actual) <- NULL

    # And name the object more broadly
    results <- lmer_actual

    # Export file
    utils::write.csv(x = results, row.names = F,
              file = file.path(output_path, output_name)) }

  # Now do linear model
  # lm model ----
  if(model_type == "lm") {

    # Get summary
    lm_smry <- base::summary(model_obj)

    # Get coefficients from that
    lm_coef <- base::as.data.frame(lm_smry$coefficients)

    # Round columns as needed
    lm_new <- dplyr::mutate(.data = lm_coef,
                            term = base::row.names(lm_coef),
                            Estimate = base::round(Estimate, digits = est_dig),
                            SE = base::round(`Std. Error`, digits = se_dig),
                            df = base::round(lm_smry$df[1:nrow(lm_coef)], df_dig),
                            t = base::round(`t value`, digits = t_dig),
                            p = base::round(`Pr(>|t|)`, digits = p_dig))

    # Strip out desired columns in preferred order
    results <- dplyr::select(.data = lm_new, term,
                             Estimate, SE, df, t, p)

    # Ditch row names
    base::rownames(results) <- NULL

    # Export file
    utils::write.csv(x = results, row.names = F,
              file = file.path(output_path, output_name)) }

  # Now do non-linear least squares
  # nls model ----
  if(model_type == "nls") {

    # Get model summary
    nls_smry <- base::summary(model_obj)

    # Extract coefficients
    nls_coef <- base::as.data.frame(nls_smry$coefficients)

    # Get new columns
    nls_new <- dplyr::mutate(.data = nls_coef,
                             term = base::row.names(nls_coef),
                             Estimate = base::round(Estimate, digits = est_dig),
                             SE = base::round(`Std. Error`, digits = se_dig),
                             df = base::round(nls_smry$df[1:nrow(nls_coef)],
                                        digits = df_dig),
                             t = base::round(`t value`, digits = t_dig),
                             p = base::round(`Pr(>|t|)`, digits = p_dig))

    # Get just desired columns
    results <- dplyr::select(.data = nls_new, term,
                             Estimate, SE, df, t, p)

    # Remove row names
    rownames(results) <- NULL

    # Export file
    utils::write.csv(x = results, row.names = F,
              file = file.path(output_path, output_name)) }

  # Process t-test model
  # t.test model ----
  if(model_type == "t.test") {

    # Extract relevant bit
    results <- data.frame(
      "Estimate" = base::round(base::as.numeric(mod_t$estimate), digits = est_dig),
      "df" = base::round(mod_t$parameter, digits = df_dig),
      "t" = base::round(mod_t$statistic, digits = t_dig),
      "p" = base::round(mod_t$p.value, digits = p_dig))

    # Remove rownames
    base::rownames(results) <- NULL

    # Export file
    utils::write.csv(x = results, row.names = F,
              file = file.path(output_path, output_name)) }
  }
