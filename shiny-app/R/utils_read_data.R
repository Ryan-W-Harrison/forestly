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
