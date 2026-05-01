#' Run the forestly Shiny app
#'
#' @param ... Options passed to the golem app.
#'
#' @return A Shiny application object.
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui(), server = app_server),
    golem_opts = list(...),
    maintenance = FALSE
  )
}
