mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("build"), "Generate plot", class = "btn-primary"),
    shiny::br(),
    shiny::br(),
    shiny::uiOutput(ns("forest_plot"))
  )
}

mod_plot_server <- function(id, data, mapping) {
  shiny::moduleServer(id, function(input, output, session) {
    plot_object <- shiny::eventReactive(input$build, {
      df <- data()
      map <- mapping()

      shiny::validate(
        shiny::need(!is.null(df), "Upload a dataset first."),
        shiny::need(length(map$parameters) > 0, "Select at least one AE criteria.")
      )

      tryCatch(
        build_forestly_plot(df, map),
        error = function(err) {
          shiny::showNotification(
            paste("Unable to generate plot:", conditionMessage(err)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
    })

    output$forest_plot <- shiny::renderUI({
      shiny::validate(shiny::need(input$build > 0, "Map variables, then generate a plot."))
      plot_object()
    })
  })
}
