# R/99_dispatcher.R
# StatLab Single JSON Entry Point Dispatcher (webR & Desktop R 4.5.2)
# Uses ONLY base R functions + StatLab R/00..05 modules (zero external/CRAN packages).

#' Dispatch a StatLab engine action and serialize the response to UTF-8 JSON
#'
#' @param action Character(1) action name: `"healthcheck"`, `"run_analysis"`, or `"general_conclusion"`
#' @param payload Named R `list` of action parameters
#' @return character(1) valid UTF-8 JSON string produced by `sl_to_json`
sl_dispatch <- function(action, payload = list()) {
  if (missing(action) || is.null(action) || length(action) == 0L || is.na(action[[1L]])) {
    stop("`action` must be a non-empty character string.")
  }
  act <- as.character(action[[1L]])
  if (is.null(payload)) {
    payload <- list()
  }
  if (!is.list(payload)) {
    stop("`payload` must be a list.")
  }

  if (identical(act, "healthcheck")) {
    res <- list(
      ok = TRUE,
      engine = "StatLab",
      r_version = paste0(R.version$major, ".", R.version$minor),
      platform = R.version$platform,
      supported_analyses = sl_box_vec(c("descriptive")),
      recommended_packages = list(
        survival = nzchar(system.file(package = "survival")),
        cluster  = nzchar(system.file(package = "cluster")),
        MASS     = nzchar(system.file(package = "MASS"))
      )
    )
    return(sl_to_json(res))
  }

  if (identical(act, "run_analysis")) {
    if (is.null(payload$data)) {
      stop("`payload$data` is required for `run_analysis`.")
    }
    analysis_type <- if (!is.null(payload$analysis_type) && length(payload$analysis_type) > 0L) {
      as.character(payload$analysis_type[[1L]])
    } else {
      "descriptive"
    }

    df <- .sl_as_dataframe(payload$data)
    opts <- if (!is.null(payload$opts) && is.list(payload$opts)) payload$opts else list()
    if (!is.null(payload$roles) && is.list(payload$roles)) {
      opts$roles <- payload$roles
    }

    res <- switch(
      analysis_type,
      descriptive = sl_analyze_descriptive(df, opts),
      stop(sprintf("Unsupported `analysis_type`: %s", analysis_type))
    )
    return(sl_to_json(res))
  }

  if (identical(act, "general_conclusion")) {
    items <- if (!is.null(payload$items)) payload$items else list()
    lang <- if (!is.null(payload$lang) && length(payload$lang) > 0L) {
      as.character(unlist(payload$lang, use.names = FALSE))
    } else {
      c("id", "en")
    }
    res <- sl_compile_general_conclusion(items = items, lang = lang)
    return(sl_to_json(res))
  }

  stop(sprintf("Unknown `sl_dispatch` action: %s", act))
}
