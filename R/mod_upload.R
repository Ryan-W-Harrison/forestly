mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(
      ns("demo"),
      "Load demo data",
      class = "btn-outline-primary"
    ),
    shiny::br(),
    shiny::br(),
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
    data_store <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$demo, {
      data_store(demo_forestly_data())
      shiny::showNotification(
        "Loaded forestly_adae_3grp demo data.",
        type = "message",
        duration = 4
      )
    })

    shiny::observeEvent(input$file, {
      tryCatch(
        data_store(read_uploaded_data(input$file$datapath, input$file$name)),
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

    shiny::reactive({
      data_store()
    })
  })
}
