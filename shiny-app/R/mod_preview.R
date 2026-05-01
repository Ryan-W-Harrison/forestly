mod_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::tableOutput(ns("table"))
  )
}

mod_preview_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      df <- data()
      shiny::validate(shiny::need(!is.null(df), "Upload a dataset to preview it."))

      htmltools::tags$p(
        class = "text-muted",
        sprintf("%s rows, %s columns", nrow(df), ncol(df))
      )
    })

    output$table <- shiny::renderTable({
      df <- data()
      shiny::validate(shiny::need(!is.null(df), ""))
      utils::head(df, 10)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  })
}
