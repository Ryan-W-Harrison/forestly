app_ui <- function(request = NULL) {
  bslib::page_sidebar(
    title = "forestly Shiny App",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    sidebar = bslib::sidebar(
      width = 360,
      mod_upload_ui("upload"),
      mod_mapping_ui("mapping")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Dataset preview"),
        mod_preview_ui("preview")
      ),
      bslib::card(
        bslib::card_header("Forest plot"),
        mod_plot_ui("plot")
      )
    )
  )
}
