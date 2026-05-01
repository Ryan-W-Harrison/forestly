mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fileInput(
      ns("file"),
      "Upload data",
      accept = c(".sas7bdat", ".csv", ".tsv", ".xlsx", ".parquet")
    ),
    shiny::helpText("Supported formats: sas7bdat, csv, tsv, xlsx, parquet.")
  )
}

mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      shiny::req(input$file)

      tryCatch(
        read_uploaded_data(input$file$datapath, input$file$name),
        error = function(err) {
          shiny::showNotification(
            paste("Unable to read file:", conditionMessage(err)),
            type = "error",
            duration = 8
          )
          NULL
        }
      )
    })
  })
}
