mod_mapping_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("mapping_controls")),
    shiny::hr(),
    shiny::checkboxGroupInput(
      ns("parameters"),
      "AE criteria",
      choices = c(
        "Any AE" = "any",
        "Serious AE" = "serious",
        "Related AE" = "related",
        "Related serious AE" = "related_serious"
      ),
      selected = "any"
    )
  )
}

mod_mapping_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mapping_controls <- shiny::renderUI({
      df <- data()
      shiny::validate(shiny::need(
        !is.null(df),
        "Upload a dataset before mapping variables."
      ))

      choices <- stats::setNames(names(df), names(df))
      optional_choices <- c("Not available" = "", choices)

      shiny::tagList(
        shiny::selectInput(
          session$ns("subject"),
          "Subject ID",
          choices = choices,
          selected = default_column(df, "USUBJID")
        ),
        shiny::selectInput(
          session$ns("treatment"),
          "Treatment group",
          choices = choices,
          selected = default_column(df, "TRTA")
        ),
        shiny::selectInput(
          session$ns("term"),
          "AE term",
          choices = choices,
          selected = default_column(df, "AEDECOD")
        ),
        shiny::selectInput(
          session$ns("soc"),
          "System organ class",
          choices = choices,
          selected = default_column(df, "AEBODSYS")
        ),
        shiny::selectInput(
          session$ns("safety_flag"),
          "Safety flag",
          choices = optional_choices,
          selected = default_column(df, "SAFFL", optional = TRUE)
        ),
        shiny::selectInput(
          session$ns("serious"),
          "Serious AE flag",
          choices = optional_choices,
          selected = default_column(df, "AESER", optional = TRUE)
        ),
        shiny::selectInput(
          session$ns("related"),
          "Relatedness flag",
          choices = optional_choices,
          selected = default_column(df, "AEREL", optional = TRUE)
        )
      )
    })

    shiny::reactive({
      list(
        subject = input$subject,
        treatment = input$treatment,
        term = input$term,
        soc = input$soc,
        safety_flag = input$safety_flag,
        serious = input$serious,
        related = input$related,
        parameters = input$parameters %||% character()
      )
    })
  })
}
