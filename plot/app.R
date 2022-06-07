#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(gettext("t-Student distribution", domain = "R-leem")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("q1",
                        "Lower limit:",
                        min = -6,
                        max = q[2],
                        value = q[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
    plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
        q[1] <- q1
        q[2] <- q2
        plotcurve(q, df)
    }
    output$distPlot <- renderPlot({
        plotcurveaux(q1 = input$q1, q2=q[2], df=nu)
    })
}

# Run the application
shinyApp(ui = ui, server = server)


binner <- function(var) {
    require(shiny)
    shinyApp(
        ui = fluidPage(
            sidebarLayout(
                sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
                mainPanel(plotOutput("hist"))
            )
        ),
        server = function(input, output) {
            output$hist <- renderPlot(
                hist(var, breaks = input$n,
                     col = "skyblue", border = "white")
            )
        }
    )
}
