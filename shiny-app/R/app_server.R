app_server <- function(input, output, session) {
  uploaded_data <- mod_upload_server("upload")
  mapping <- mod_mapping_server("mapping", uploaded_data)
  mod_preview_server("preview", uploaded_data)
  mod_plot_server("plot", uploaded_data, mapping)
}
