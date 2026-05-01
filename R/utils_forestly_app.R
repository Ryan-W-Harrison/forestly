`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

build_forestly_plot <- function(data, mapping) {
  required <- unlist(mapping[c("subject", "treatment", "term", "soc")], use.names = FALSE)
  missing_required <- required[is.na(required) | !nzchar(required)]
  if (length(missing_required) > 0) {
    stop("Subject, treatment, AE term, and system organ class mappings are required.", call. = FALSE)
  }

  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0) {
    stop("Mapped columns are missing: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  optional <- unlist(mapping[c("safety_flag", "serious", "related")], use.names = FALSE)
  optional <- optional[nzchar(optional)]
  missing_optional <- setdiff(optional, names(data))
  if (length(missing_optional) > 0) {
    stop("Optional mapped columns are missing: ", paste(missing_optional, collapse = ", "), call. = FALSE)
  }

  safety_flag <- mapped_or_default(data, mapping$safety_flag, "Y")
  serious_flag <- mapped_or_default(data, mapping$serious, "N")
  related_flag <- mapped_or_default(data, mapping$related, "UNKNOWN")

  adae <- data |>
    dplyr::transmute(
      USUBJID = as.character(.data[[mapping$subject]]),
      TRTA = as.character(.data[[mapping$treatment]]),
      AEDECOD = as.character(.data[[mapping$term]]),
      AEBODSYS = as.character(.data[[mapping$soc]]),
      SAFFL = normalize_yes_no(safety_flag, default = "Y"),
      AESER = normalize_yes_no(serious_flag, default = "N"),
      AEREL = normalize_related(related_flag),
      SITEID = "Not available",
      SEX = "Unknown",
      RACE = "Unknown",
      AGE = NA_real_,
      ASTDY = NA_real_,
      AESEV = "Unknown",
      AEACN = "Unknown",
      AEOUT = "Unknown",
      ADURN = NA_real_,
      ADURU = "Unknown"
    ) |>
    dplyr::filter(
      !is.na(.data$USUBJID),
      nzchar(.data$USUBJID),
      !is.na(.data$TRTA),
      nzchar(.data$TRTA),
      !is.na(.data$AEDECOD),
      nzchar(.data$AEDECOD),
      !is.na(.data$AEBODSYS),
      nzchar(.data$AEBODSYS)
    )

  if (nrow(adae) == 0) {
    stop("No complete AE records remain after applying the selected mappings.", call. = FALSE)
  }

  treatment_levels <- unique(adae$TRTA)
  if (length(treatment_levels) < 2) {
    stop("Forestly requires at least two treatment groups.", call. = FALSE)
  }

  adae$TRTA <- factor(adae$TRTA, levels = treatment_levels)

  adsl <- adae |>
    dplyr::distinct(
      .data$USUBJID,
      .data$TRTA,
      .data$SAFFL,
      .data$SITEID,
      .data$SEX,
      .data$RACE,
      .data$AGE
    )

  parameter_term <- build_parameter_term(mapping$parameters)

  meta <- metalite::meta_adam(population = adsl, observation = adae) |>
    metalite::define_plan(plan = metalite::plan(
      analysis = "ae_forestly",
      population = "apat",
      observation = "apat",
      parameter = parameter_term
    )) |>
    metalite::define_analysis(name = "ae_forestly", label = "Interactive forest plot") |>
    metalite::define_population(
      name = "apat",
      group = "TRTA",
      id = "USUBJID",
      subset = SAFFL == "Y",
      label = "Analysis population"
    ) |>
    metalite::define_observation(
      name = "apat",
      group = "TRTA",
      subset = SAFFL == "Y",
      label = "Adverse events"
    ) |>
    add_forestly_parameters(mapping$parameters) |>
    metalite::meta_build()

  meta |>
    prepare_ae_forestly(
      parameter = parameter_term,
      ae_listing_display = c(
        "USUBJID", "SEX", "RACE", "AGE", "ASTDY", "AESEV",
        "AESER", "AEREL", "AEACN", "AEOUT", "SITEID", "ADURN", "ADURU"
      )
    ) |>
    format_ae_forestly() |>
    ae_forestly(width = 1200)
}

build_parameter_term <- function(parameters) {
  if (length(parameters) == 0) {
    stop("Select at least one AE criteria.", call. = FALSE)
  }

  paste(parameters, collapse = ";")
}

add_forestly_parameters <- function(meta, parameters) {
  for (parameter in parameters) {
    meta <- switch(
      parameter,
      any = metalite::define_parameter(
        meta,
        name = "any",
        subset = NULL,
        label = "Any AE",
        var = "AEDECOD",
        soc = "AEBODSYS"
      ),
      serious = metalite::define_parameter(
        meta,
        name = "serious",
        subset = AESER == "Y",
        label = "Serious AE",
        var = "AEDECOD",
        soc = "AEBODSYS"
      ),
      related = metalite::define_parameter(
        meta,
        name = "related",
        subset = AEREL %in% c("PROBABLE", "POSSIBLE", "RELATED", "YES", "Y"),
        label = "Related AE",
        var = "AEDECOD",
        soc = "AEBODSYS"
      ),
      related_serious = metalite::define_parameter(
        meta,
        name = "related_serious",
        subset = AESER == "Y" & AEREL %in% c("PROBABLE", "POSSIBLE", "RELATED", "YES", "Y"),
        label = "Related serious AE",
        var = "AEDECOD",
        soc = "AEBODSYS"
      ),
      stop("Unsupported AE criteria: ", parameter, call. = FALSE)
    )
  }

  meta
}

normalize_yes_no <- function(x, default) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  value <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    value %in% c("Y", "YES", "TRUE", "T", "1") ~ "Y",
    value %in% c("N", "NO", "FALSE", "F", "0") ~ "N",
    TRUE ~ default
  )
}

normalize_related <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("UNKNOWN")
  }

  value <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    value %in% c("Y", "YES", "TRUE", "T", "1", "RELATED") ~ "RELATED",
    value %in% c("PROBABLE", "POSSIBLE") ~ value,
    value %in% c("N", "NO", "FALSE", "F", "0", "NOT RELATED", "UNRELATED") ~ "NOT RELATED",
    TRUE ~ value
  )
}

mapped_or_default <- function(data, column, default) {
  if (is.null(column) || !nzchar(column)) {
    return(rep(default, nrow(data)))
  }

  data[[column]]
}
