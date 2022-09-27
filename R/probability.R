# Probability
#' @import manipulate
#' @import tkRplotR
#  @import shiny
#' @export
p <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regionb)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
          plotcurve <- function(q, nu, ...) {
            x <- seq(q[1], q[2], by=0.01)
            y <- seq(-6, 6, by=0.01)
            fx <- dt(x, df = nu)
            fy <- dt(y, df = nu)
            curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T",
                  ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
            polygon(c(y, rev(y)),
                    c(fy, rep(0, length(fy))),
                    col="gray90")
            polygon(c(x, rev(x)),
                    c(fx, rep(0, length(fx))),
                    col="red")
            abline(v=0, lty=2)
            qq <- round(q, digits=2)
            qqaux <- qq
            Pr <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
            Pr <- gsub("\\.", ",", Pr)
            qq <- gsub("\\.", ",", qq)
            axis(side=1, at=qqaux, labels=qqaux,
                 col="red", font = 2)
            abline(v = qqaux, lty=2, col = "red")
            if (attr(q, "region") == "region2") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< T < "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region4") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= T <= "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region7") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= T < "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region8") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< T <= "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
          }
          if (gui == "plot" ) {
            # Probability
            nu <- argaddit$df
            prob <- round(pt(q = q[2], df = nu) - pt(q = q[1], df = nu), rounding)
            # Plot
            plotcurve(q, nu)
          }
          if (gui == "rstudio") {
            nu <- argaddit$df
            plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
              q[1] <- q1
              q[2] <- q2
              plotcurve(q, df)
            }
            manipulate::manipulate(plotcurveaux(q1, q2, df),
                                   q1 = manipulate::slider(-6, q[2], q[1]),
                                   q2 = manipulate::slider(q[2], 6, q[2]),
                                   df = manipulate::slider(1, 200, nu))
            prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
          }
          if (gui == "tcltk") {
            # Environment of package
            envleem <- new.env(parent = base::emptyenv())
            leemget <- function(x) {
              get(x, envir= envleem, inherits=FALSE )
            }
            leemset <- function(x, value) {
              assign(x, value, envir= envleem)
            }
            globalvariables <- function(x, value) {
              assign(x, value, envir= .GlobalEnv)
            }
            # Desabilitar warnings global
            #options(warn = - 1)
            # war <- options(warn = - 1)
            # on.exit(options(war))

            nu <- argaddit$df
            plotcurveaux <- function(q1 = q[1], q2 = q[2], nu = nu, ...) {
              q[1] <- q1
              q[2] <- q2
              plotcurve(q, nu)
            }
            tk_q1 <- leemset("tk_q1", tclVar(q[1]))
            tk_q2 <- leemset("tk_q2", tclVar(q[2]))
            tk_df <- leemset("tk_df", tclVar(nu))
            sapply(c("tk_q1", "tk_q2", "tk_df"),
                   function(x) globalvariables(x, leemget(x)))

            # q1 <- NULL
            # q2 <- NULL
            # nu <- NULL
            ##
            # Disabled GUI (Type I)
            oldmode <- tclServiceMode(FALSE)
            # Logo
            tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))

            # Plot
            tkplot <- tktoplevel()

            #Icon main toplevel window
            tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")

            # Title
            tkwm.title(tkplot,
                       gettext("leem package: t Distribution", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                         height = 500, fun = function(...) {
              q1 <- as.numeric(tclvalue(tk_q1))
              q2 <- as.numeric(tclvalue(tk_q2))
              nu <- as.numeric(tclvalue(tk_df))
              plotcurveaux(q1 = q1, q2 = q2, nu = nu)
            })
            s02 <- tcltk::tkscale(
              tkplot,
              from = q[2],
              to = 6,
              label = 'q2',
              variable = tk_q2,
              showvalue = TRUE,
              resolution = 1,
              repeatdelay = 200,
              repeatinterval = 100,
              orient = "hor"
            )
            s01 <- tcltk::tkscale(
              tkplot,
              from = -6,
              to = q[2],
              label = 'q1',
              variable = tk_q1,
              showvalue = TRUE,
              resolution = 1,
              repeatdelay = 200,
              repeatinterval = 100,
              orient = "hor"
            )
            s03 <- tkscale(
              tkplot,
              from = 1,
              to = 200,
              label = 'df',
              variable = tk_df,
              showvalue = TRUE,
              resolution = 1,
              repeatdelay = 200,
              repeatinterval = 100,
              orient = "hor"
            )
            tkpack(s01, s02, s03,
                   side = "top",
                   expand = TRUE,
                   before = tkplot$env$canvas,
                   fill = "both")
            # Activate GUI
            finish <- tclServiceMode(oldmode)
            tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
              response <- tk_messageBox(
                title = gettext("Tell me something:", domain = "R-leem"),
                message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
                icon = "question",
                type = "yesno"
              )
              if (response == "yes") {
                if (exists("tk_q1", envir = .GlobalEnv)) {
                  rm("tk_q1", "tk_q2", "tk_df", envir = .GlobalEnv)
                }
                tkdestroy(tkplot)
              }
            })


            prob <- round(pt(q[2], df = nu,
                             lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)

          }
          if (gui == "shiny") {
            # # Environment of package
            # envleem <- new.env(parent = base::emptyenv())
            # assign("shinyaux", NULL, envir = envleem)
            # assign("tk_q2", NULL, envir = envleem)
            # assign("tk_df", NULL, envir = envleem)
            # nu <- argaddit$df
            # prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
            # plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
            #   q[1] <- q1
            #   q[2] <- q2
            #   plotcurve(q, df)
            # }
            # shinyaux <<- function(...) {
            #   require(shiny)
            #   # Define UI for application that draws a histogram
            #   ui <- fluidPage(
            #
            #     # Application title
            #     titlePanel(gettext("t-Student distribution", domain = "R-leem")),
            #
            #     # Sidebar with a slider input for number of bins
            #     sidebarLayout(
            #       sidebarPanel(
            #         sliderInput("q1",
            #                     "Lower limit:",
            #                     min = -6,
            #                     max = q[2],
            #                     value = q[1])
            #       ),
            #
            #       # Show a plot of the generated distribution
            #       mainPanel(
            #         plotOutput("distPlot")
            #       )
            #     )
            #   )
            #
            #   # Define server logic required to draw a histogram
            #   server <- function(input, output) {
            #     prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
            #     plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
            #       q[1] <- q1
            #       q[2] <- q2
            #       plotcurve(q, df)
            #     }
            #     output$distPlot <- renderPlot({
            #       plotcurveaux(q1 = input$q1, q2=q[2], df=nu)
            #     })
            #   }
            #
            #   # Run the application
            #   shinyApp(ui = ui, server = server)
            # }
            # shinyaux()
          }

      }
    }
    if (any(attr(q, "region") == regiona)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
        plotcurve <- function(q, nu) {
          x <- seq(-6, q[1], by=0.01)
          z <- seq(q[2], 6, by=0.01)
          y <- seq(-6, 6, by=0.01)
          fx <- dt(x, df = nu)
          fz <- dt(z, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T",
                ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(z, rev(z)),
                  c(fz, rep(0, length(fz))),
                  col="red")
          abline(v=0, lty=2)
          qq <- round(q, digits=2)
          Pr <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qq, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1)
          axis(side=1, at=as.character(c(-6, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], 6)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qq, lty=2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >"~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X >="~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X > "~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region6") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >= "~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
        }
        if (gui == "plot" ) {
          # Probability
          nu <- argaddit$df
          prob <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
          # Plot
          plotcurve(q, nu)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, df)
          }
          manipulate::manipulate(plotcurveaux(q1, q2, df),
                                 q1 = manipulate::slider(-6, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 6, q[2]),
                                 df = manipulate::slider(1, 200, nu))
          prob <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
        }
      }
    }

  } else {
    if (dist == "t-student") {
      if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
      if (lower.tail) {
        plotcurve <- function(q, nu) {
          x <- seq(-6, q, by=0.01)
          y <- seq(q, 6, by=0.01)
          fx <- dt(x, df = nu)
          fy <- dt(y, df = nu)

          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
                xlab="T", ylim = c(0, 1.2 * max(fx)), panel.first = grid(col = "gray"))

          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=0, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pt(qq, df = nu, lower.tail = T), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1)
          axis(side=1, at=as.character(c(-6, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(T<=q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
        }
        if (gui == "plot" ) {
          # Probability
          nu <- argaddit$df
          prob <- pt(q = q, df = nu)
          # Plot
          plotcurve(q, nu)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          manipulate::manipulate(plotcurve(qaux, nuaux),
                                 qaux = manipulate::slider(-6, 6, q),
                                 nuaux = manipulate::slider(1, 200, nu))
          prob <- pt(q = q, df = nu)
        }
        if (gui == "tcltk") {
          # Environment of package
          envleem <- new.env(parent = base::emptyenv())
          leemget <- function(x) {
            get(x, envir= envleem, inherits=FALSE )
          }
          leemset <- function(x, value) {
            assign(x, value, envir= envleem)
          }
          globalvariables <- function(x, value) {
            assign(x, value, envir= .GlobalEnv)
          }
          # Desabilitar warnings global
          #options(warn = - 1)
          # war <- options(warn = - 1)
          # on.exit(options(war))

          nu <- argaddit$df
          # plotcurveaux <- function(q1 = q[1], q2 = q[2], nu = nu, ...) {
          #   q[1] <- q1
          #   q[2] <- q2
          #   plotcurve(q, nu)
          # }
          tk_q1 <- leemset("tk_q1", tclVar(q[1]))
          tk_df <- leemset("tk_df", tclVar(nu))
          sapply(c("tk_q1", "tk_df"),
                 function(x) globalvariables(x, leemget(x)))

          # q1 <- NULL
          # q2 <- NULL
          # nu <- NULL
          ##
          # Disabled GUI (Type I)
          oldmode <- tclServiceMode(FALSE)
          # Logo
          tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))

          # Plot
          tkplot <- tktoplevel()

          #Icon main toplevel window
          tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")

          # Title
          tkwm.title(tkplot,
                     gettext("leem package: t Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q <- as.numeric(tclvalue(tk_q1))
                                        nu <- as.numeric(tclvalue(tk_df))
                                        plotcurve(q = q, nu = nu)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = -6,
            to = q,
            label = 'q',
            variable = tk_q1,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tkscale(
            tkplot,
            from = 1,
            to = 200,
            label = 'df',
            variable = tk_df,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          tkpack(s01, s03,
                 side = "top",
                 expand = TRUE,
                 before = tkplot$env$canvas,
                 fill = "both")
          # Activate GUI
          finish <- tclServiceMode(oldmode)
          tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
            response <- tk_messageBox(
              title = gettext("Tell me something:", domain = "R-leem"),
              message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
              icon = "question",
              type = "yesno"
            )
            if (response == "yes") {
              if (exists("tk_q1", envir = .GlobalEnv)) {
                rm("tk_q1", "tk_df", envir = .GlobalEnv)
              }
              tkdestroy(tkplot)
            }
          })


          prob <- pt(q = q, df = nu)

        }
      } else {
        plotcurve <- function(q, nu) {
          x <- seq(q, 6, by=0.01)
          y <- seq(-6, q, by=0.01)
          fx <- dt(x, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
                xlab="T", ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))

          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=0, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pt(qq, df = nu, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1)
          axis(side=1, at=as.character(c(qqaux, 6)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(T~`>`~q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
        }
        if (gui == "plot") {
          # Probability
          nu <- argaddit$df
          prob <- pt(q = q, df = nu, lower.tail = FALSE)
          # Plot
          plotcurve(q, nu)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          prob <- pt(q = q, df = nu)
          manipulate::manipulate(plotcurve(qaux, nuaux),
                                 qaux = manipulate::slider(-6, 6, q),
                                 nuaux = manipulate::slider(1, 200, nu))
        }
        if (gui == "tcltk") {
          # Environment of package
          envleem <- new.env(parent = base::emptyenv())
          leemget <- function(x) {
            get(x, envir= envleem, inherits=FALSE )
          }
          leemset <- function(x, value) {
            assign(x, value, envir= envleem)
          }
          globalvariables <- function(x, value) {
            assign(x, value, envir= .GlobalEnv)
          }
          # Desabilitar warnings global
          #options(warn = - 1)
          # war <- options(warn = - 1)
          # on.exit(options(war))

          nu <- argaddit$df
          # plotcurveaux <- function(q1 = q[1], q2 = q[2], nu = nu, ...) {
          #   q[1] <- q1
          #   q[2] <- q2
          #   plotcurve(q, nu)
          # }
          tk_q1 <- leemset("tk_q1", tclVar(q[1]))
          tk_df <- leemset("tk_df", tclVar(nu))
          sapply(c("tk_q1", "tk_df"),
                 function(x) globalvariables(x, leemget(x)))

          # q1 <- NULL
          # q2 <- NULL
          # nu <- NULL
          ##
          # Disabled GUI (Type I)
          oldmode <- tclServiceMode(FALSE)
          # Logo
          tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))

          # Plot
          tkplot <- tktoplevel()

          #Icon main toplevel window
          tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")

          # Title
          tkwm.title(tkplot,
                     gettext("leem package: t Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q <- as.numeric(tclvalue(tk_q1))
                                        nu <- as.numeric(tclvalue(tk_df))
                                        plotcurve(q = q, nu = nu)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = -6,
            to = q,
            label = 'q',
            variable = tk_q1,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tkscale(
            tkplot,
            from = 1,
            to = 200,
            label = 'df',
            variable = tk_df,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          tkpack(s01, s03,
                 side = "top",
                 expand = TRUE,
                 before = tkplot$env$canvas,
                 fill = "both")
          # Activate GUI
          finish <- tclServiceMode(oldmode)
          tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
            response <- tk_messageBox(
              title = gettext("Tell me something:", domain = "R-leem"),
              message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
              icon = "question",
              type = "yesno"
            )
            if (response == "yes") {
              if (exists("tk_q1", envir = .GlobalEnv)) {
                rm("tk_q1", "tk_df", envir = .GlobalEnv)
              }
              tkdestroy(tkplot)
            }
          })


          prob <- pt(q = q, df = nu, lower.tail = FALSE)

        }

      }
    }
    if (dist == "gumbel"){
      if (!any(names(argaddit) == "location")) stop("Insira o argumento 'location'!", call. = FALSE)
      if (!any(names(argaddit) == "scale")) stop("Insira o argumento 'scale'!", call. = FALSE)
      if (argaddit$scale <= 0 ) stop("o argumento 'scale' deve ser maior que zero!", call. = FALSE)
      if (lower.tail) {
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
          aux <- seq(q, 10, by=0.01)
          y <- seq(-5, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col="gray90")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="red")
          abline(v = location, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pgumbel(qq, location, scale), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G<=q)==Pr, list(q=qq, Pr=Pr)))
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          manipulate::manipulate(plotcurve(q, location, scale),
                                 q = manipulate::slider(-5, 10, q),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = q, location, scale)
        }
      } else {
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
          aux <- seq(q, 10, by=0.01)
          y <- seq(-5, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, repBEN(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v = location, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pgumbel(qq, location, scale, lower.tail = FALSE), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G>q)==Pr, list(q=qq, Pr=Pr)))
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          manipulate::manipulate(plotcurve(q, location, scale),
                                 q = manipulate::slider(-5, 10, q),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
        }
      }
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}








