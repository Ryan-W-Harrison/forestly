read_uploaded_data <- function(path, name) {
  ext <- tolower(tools::file_ext(name))

  out <- switch(
    ext,
    sas7bdat = haven::read_sas(path),
    csv = readr::read_csv(path, show_col_types = FALSE),
    tsv = readr::read_tsv(path, show_col_types = FALSE),
    xlsx = readxl::read_excel(path),
    parquet = arrow::read_parquet(path),
    stop("Unsupported file type: .", ext)
  )

  tibble::as_tibble(out)
}

demo_forestly_data <- function() {
  tibble::as_tibble(forestly_adae_3grp)
}

default_column <- function(data, column, optional = FALSE) {
  matched <- names(data)[toupper(names(data)) == toupper(column)]

  if (length(matched) > 0) {
    return(matched[[1]])
  }

  if (optional) {
    return("")
  }

  names(data)[[1]]
}
