# Plot shiny dist binomial para q de comprimento 1 (lower.tail = T)
.shinyplotleembinomial <- function(q,
                                   size,
                                   prob,
                                   rounding,
                                   main = "Binomial Distribution",
                                   minimo = 0,
                                   maximo = size) {

  # Rename object
  p_success <- prob

  # Path to logo inside package
  logo_path <- system.file(
    "etc",
    package = "leem"
  )

  # Make folder available to shiny
  shiny::addResourcePath(
    "leemlogo",
    logo_path
  )

  # =========================
  # User Interface
  # =========================

  ui <- shiny::fluidPage(

    shiny::titlePanel(
      gettext(
        "leem package: Binomial Distribution",
        domain = "R-leem"
      )
    ),

    shiny::sidebarLayout(

      # =========================
      # Sidebar panel
      # =========================

      shiny::sidebarPanel(

        shiny::h4(
          gettext(
            "Input(s):",
            domain = "R-leem"
          )
        ),

        # q slider
        shiny::sliderInput(
          inputId = "q",

          label = gettext(
            "Quantile",
            domain = "R-leem"
          ),

          min = minimo,
          max = maximo,
          value = q,
          step = 1
        ),

        # size slider
        shiny::sliderInput(
          inputId = "size",

          label = gettext(
            "Size",
            domain = "R-leem"
          ),

          min = 1,
          max = size + 30,
          value = size,
          step = 1
        ),

        # probability slider
        shiny::sliderInput(
          inputId = "prob",

          label = gettext(
            "Success",
            domain = "R-leem"
          ),

          min = 0.01,
          max = 0.99,
          value = p_success,
          step = 0.01
        ),

        # Small spacing
        shiny::br(),

        # =========================
        # Package logo
        # =========================

        shiny::div(

          style = "
            text-align:center;
            margin-top:5px;
          ",

          shiny::tags$img(
            src = "leemlogo/logo.png",

            style = "
              width:80%;
              max-width:180px;
            "
          )
        )
      ),

      # =========================
      # Main panel
      # =========================

      shiny::mainPanel(

        shiny::plotOutput(
          "binomPlot",
          height = "600px"
        )
      )
    )
  )

  # =========================
  # Server
  # =========================

  server <- function(input, output, session) {

    # Properly close application
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    # Update q slider when size changes
    shiny::observeEvent(input$size, {

      # Current size value
      n <- input$size

      # Current q value
      q_current <- input$q

      # Prevent q > size
      if (q_current > n) {
        q_current <- n
      }

      # Update q slider
      shiny::updateSliderInput(
        session = session,
        inputId = "q",
        max = n,
        value = q_current
      )

    })

    # Render plot
    output$binomPlot <- shiny::renderPlot({

      quantil <- input$q
      n <- input$size
      p <- input$prob

      plotpbinomiallttplot(
        quantil,
        n,
        p,
        rounding,
        main
      )
    })
  }

  # =========================
  # Return shiny app
  # =========================

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
