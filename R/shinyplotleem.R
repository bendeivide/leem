#######################################################################
# Name: .shinyplot + leem + type + dist
## lt - lower.tail; ltt - lower.tail == TRUE; ltn - lower.tail == NULL;
##   ra - Region A; rb - Region B
## dist: "normal", "binomial", ...
######################################################################


# Low-level Shiny GUI function for the leem package.
# Generates the Normal Distribution interface associated with
# P(X <= q) (lower.tail = TRUE) using gui = "shiny"
# ===> This is the interface reference! <===
.shinyplotleemlttnormal <- function(q, mu, sigma, rounding,
                                    minimo, maximo, dec,
                                    long.segment, col,
                                    col2, lty, main,
                                    text.size, cex.main,
                                    cex.axis, cex.lab,
                                    vert.orien.main) {

  # Path to package resources directory
  logo_path <- system.file("etc", package = "leem")

  # Make package resources available to the Shiny app
  shiny::addResourcePath("leemlogo", logo_path)

  # =========================
  # Central plotting function
  # =========================
  make_plot <- function(q, mu, sigma, text.size, titorient, lsegment, comma) {

    # Generate the normal distribution plot
    plotpnormallttplot(
      q, mu, sigma,
      rounding,
      dec = comma,
      long.segment = lsegment,
      col = col,
      col2 = col2,
      lty = lty,
      main = main,
      text.size = text.size,
      cex.main = text.size,
      cex.axis = cex.axis,
      cex.lab = cex.lab,
      vert.orien.main = titorient
    )
  }

  # =========================
  # User Interface
  # =========================
  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel(
      gettext("Normal Distribution", domain = "R-leem")
    ),

    # Sidebar and main panel layout
    shiny::sidebarLayout(

      # =========================
      # Sidebar panel
      # =========================
      shiny::sidebarPanel(

        # Package logo
        shiny::div(

          style = "display:flex;justify-content:center;margin-top:5px;",

          shiny::tags$img(
            src = "leemlogo/logo.png",
            style = "width:80%;max-width:180px;height:auto;"
          )
        ),

        # Section title for inputs
        shiny::h4(
          gettext(
            "Input(s):",
            domain = "R-leem"
          )
        ),

        # Quantile slider
        shiny::sliderInput(
          inputId = "quantil",

          label = gettext(
            "Quantile",
            domain = "R-leem"
          ),

          min = minimo,
          max = maximo,
          value = q,
          step = 0.1
        ),

        # Mean slider
        shiny::sliderInput(
          inputId = "mu",

          label = gettext(
            "Mean",
            domain = "R-leem"
          ),

          min = minimo,
          max = maximo,
          value = mu,
          step = 0.1
        ),

        # Standard deviation slider
        shiny::sliderInput(
          inputId = "sigma",

          label = gettext(
            "Standard Deviation",
            domain = "R-leem"
          ),

          min = sigma,
          max = sigma * 1.8,
          value = sigma,
          step = 0.01
        ),

        # Text size slider
        shiny::sliderInput(
          inputId = "text.size",

          label = gettext(
            "Text Size",
            domain = "R-leem"
          ),

          min = 0.8,
          max = 3,
          value = text.size,
          step = 0.01
        ),

        # Vertical title orientation option
        shiny::checkboxInput(
          "titorient",

          gettext(
            "Vertical Title Orientation",
            domain = "R-leem"
          ),

          TRUE
        ),

        # Long segment option
        shiny::checkboxInput(
          "lsegment",

          gettext(
            "Long segment",
            domain = "R-leem"
          ),

          FALSE
        ),

        # Decimal separator option
        shiny::checkboxInput(
          "comma",

          gettext(
            "Comma",
            domain = "R-leem"
          ),

          FALSE
        ),

        # Horizontal separator
        shiny::hr(),

        # Export section title
        shiny::h4("Export"),

        # Download buttons
        shiny::downloadButton("dl_png", "Download PNG"),
        shiny::downloadButton("dl_pdf", "Download PDF"),
        shiny::downloadButton("dl_svg", "Download SVG")

        #shiny::hr(),

        # shiny::tags$img(
        #   src = "leemlogo/logo.png",
        #   style = "width:80%; max-width:180px;"
        # )
      ),

      # =========================
      # Main panel
      # =========================
      shiny::mainPanel(

        # Plot output area
        shiny::plotOutput("normPlot", height = "600px")
      )
    )
  )

  # =========================
  # Server
  # =========================
  server <- function(input, output, session) {

    # Properly stop the application when the session ends
    session$onSessionEnded(function() shiny::stopApp())

    # -------------------------
    # Render plot
    # -------------------------
    output$normPlot <- shiny::renderPlot({

      # Generate interactive plot using current inputs
      plotpnormallttplot(
        q = input$quantil,
        mu = input$mu,
        sigma = input$sigma,
        rounding = rounding,
        dec = if (input$comma) "," else ".",
        long.segment = input$lsegment,
        col = col,
        col2 = col2,
        lty = lty,
        main = main,
        text.size = input$text.size,
        cex.main = input$text.size,
        cex.axis = cex.axis,
        cex.lab = cex.lab,
        vert.orien.main = input$titorient
      )

    })

    # =========================
    # PNG export
    # =========================
    output$dl_png <- shiny::downloadHandler(

      # Output file name
      filename = function() {
        paste0("normal_plot_", Sys.Date(), ".png")
      },

      # PNG generation
      content = function(file) {

        # Open PNG graphics device
        png(file, width = 2000, height = 1400, res = 300)

        # Generate plot
        make_plot(
          input$quantil,
          input$mu,
          input$sigma,
          input$text.size,
          input$titorient,
          input$lsegment,
          if (input$comma) "," else "."
        )

        # Close graphics device
        dev.off()
      }
    )

    # =========================
    # PDF export
    # =========================
    output$dl_pdf <- shiny::downloadHandler(

      # Output file name
      filename = function() {
        paste0("normal_plot_", Sys.Date(), ".pdf")
      },

      # PDF generation
      content = function(file) {

        # Open PDF graphics device
        pdf(file, width = 8, height = 6)

        # Generate plot
        make_plot(
          input$quantil,
          input$mu,
          input$sigma,
          input$text.size,
          input$titorient,
          input$lsegment,
          if (input$comma) "," else "."
        )

        # Close graphics device
        dev.off()
      }
    )

    # =========================
    # SVG export
    # =========================
    output$dl_svg <- shiny::downloadHandler(

      # Output file name
      filename = function() {
        paste0("normal_plot_", Sys.Date(), ".svg")
      },

      # SVG generation
      content = function(file) {

        # Open SVG graphics device
        svg(file, width = 8, height = 6)

        # Generate plot
        make_plot(
          input$quantil,
          input$mu,
          input$sigma,
          input$text.size,
          input$titorient,
          input$lsegment,
          if (input$comma) "," else "."
        )

        # Close graphics device
        dev.off()
      }
    )

  }

  # Launch Shiny application
  shiny::shinyApp(ui, server)
}




.shinyplotleembinomial <- function(q,
                                   size,
                                   prob,
                                   rounding,
                                   main,
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
