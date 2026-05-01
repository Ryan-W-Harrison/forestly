source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(source_files, source))

run_app()
