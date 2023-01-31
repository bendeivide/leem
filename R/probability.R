#' Cumulative distribution function
#'
#' \code{P} Cumulative distribution function for multiple distributions
#'
#' @details The argument that can have length 2, when we use the functions that give us the probability regions, given by: \code{\%<X<\%}, \code{\%<=X<\%}, \code{\%<X<=\%}, \code{\%<=X<=\%}, \code{\%>X>\%}, \code{\%>X=>\%}, \code{\%>X=>\%} and \code{\%>=X=>\%}.
#' The additional arguments represent the parameters of the distributions, that is:
#'
#' - \code{dist = "t-student"}: \code{nu} argument (\eqn{\nu}) represents the degrees of freedom parameter. The PDF is
#' \deqn{
#' \frac{\Gamma\left\( \frac{\nu + 1}{2} \right\)}{\sqrt{\nu \pi}}\left\(1 + \frac{x^2}{\nu}\right\)
#' }
#'
#' @param q quantile. The \code{q} argument can have length 1 or 2. See Details.
#' @param dist distribution to use. The default is \code{'t-student'}. Options: \code{'normal'}, \code{'t-student'}, \code{'gumbel'}, \code{'binomial'}, \code{'poisson'}, and ....
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}. This argument is valid only if \code{q} has length 1.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param porcentage logical; if \code{FALSE} (default), the result in decimal. Otherwise, probability is given in percentage.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{'rstudio'} or \code{'tcltk'}.
#' @param ... additional arguments according to the chosen distribution.
#'
#' @return \code{P} returns the probability and its graphical representation. The result can be given as a percentage or not.
#'
#' @examples
#' # Loading package
#' library(leem)
#' \dontrun{
#' P(q = 2, dist = "t-student", df = 10)
#' P(q = 2, dist = "t-student", df = 10, gui = 'rstudio')
#' P(q = 2, dist = "t-student", df = 10, gui = 'tcltk')
#' P(-1 %<X<% 1, dist = "t-student", df = 10)
#' }
#' @import manipulate
#' @import tkRplotR
#  @import shiny
#' @export
P <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  # Region of q
  # if (dist == "t-student") {
  #   if (q > 6 | q < -6) stop("Define the 'q' argument between -6 and 6", call. = FALSE, domain = "R-leem")
  # }
    # Arguments in '...'
  argaddit <- list(...)
  # Formal arguments
  argdef <- formals(p)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regionb)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline("Insert the value of degree of freedom (df): ")
          argaddit$df <- as.numeric(df)
        }
          plotcurve <- function(q, nu, ...) {
            x <- seq(q[1], q[2], by=0.01)
            y <- seq(-6, 6, by=0.01)
            fx <- dt(x, df = nu)
            fy <- dt(y, df = nu)
            curve(dt(x, df = nu), -6, 6, ylab = expression(f[X](x)), xlab="X",
                  ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray"),
                  main = gettext("Student's t-distribution", domain = "R-leem"))
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
                 col="red", font = 2, col.axis = "red")
            abline(v = qqaux, lty=2, col = "red")
            if (attr(q, "region") == "region2") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< x < "~t2~";"~nu==df)==Pr~"\n\n",
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
            }
            if (attr(q, "region") == "region4") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= X <= "~t2~";"~nu==df)==Pr~"\n\n",
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
            }
            if (attr(q, "region") == "region7") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= T < "~t2~";"~nu==df)==Pr~"\n\n",
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
            }
            if (attr(q, "region") == "region8") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< T <= "~t2~";"~nu==df)==Pr~"\n\n",
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
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
      if (dist == "gumbel") {
        if (!any(names(argaddit) == "location")) {
          location <- readline("Insert the value of 'location' argument: ")
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline("Insert the value of 'scale' argument: ")
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        plotcurve <- function(q, location, scale){
          # variação de x
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          curve(dgumbel(x, location, scale),xvq, xvq1,ylim = c(0,1.5*(dgumbel(1, location, scale))),xlim = c(xvq,xvq1), ylab = expression(f[G](g)), xlab = "G",panel.first = grid(col = "gray"))
          aux <- seq(xvq,xvq1, by = 0.01)
          y <- seq(q[1],q[2], by = 0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col = "gray90")

          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="red")
          abline(v = location, lty = 2)
          locaux <- location
          scaux <- scale
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round( pgumbel(max(q[1],q[2]), location, scale) - pgumbel(min(q[1],q[2]),
                                                                          location, scale) , digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = as.character(c(xvq1,xvq)),labels = FALSE, lwd.ticks = 0 , col = "black", font = 2)
          axis(side=1, at=qqaux, labels = qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~"< G < "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region4") {
            legend("topleft", bty = "n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~"<= G <= "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region7") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~"<= G < "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region8") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~"< G <= "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = max(q[1],q[2]), location, scale) - pgumbel(q = min(q[1],q[2]), location, scale)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          # variação de x
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }

          plotcurveaux <- function(q1 = q[1], q2 = q[2], location,scale) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, location, scale)
          }
          manipulate::manipulate(plotcurveaux(q1 , q2 , location, scale),
                                 q1 = manipulate::slider(xvq, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], xvq1, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = max(q[1],q[2]), location, scale) - pgumbel(q = min(q[1],q[2]), location, scale)
        }
      }
      if (dist == "normal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline("Insert the value of 'mean' argument: ")
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline("Insert the value of 'sd' argument: ")
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        plotcurve <- function(q, mu, sigma) {

          x <- seq(q[1], q[2], by = 0.01)
          y <- seq(mu - 4 * sigma, mu + 4 * sigma, by = 0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)

          curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
                ylab = expression(f[X](x)), xlab = "X",
                ylim = c(0, 1.2 * max(fx,fy)),
                panel.first = grid(col="gray90"))

          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")

          abline(v=argaddit$mean, lty=2)
          qq <- round(q, digits=2)
          qqaux <- qq
          Pr <- round(pnorm(q[2], mean = mu,sd = sigma, lower.tail = T) - pnorm(q[1], mean = mu, sd=sigma, lower.tail = T), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")

          if (attr(q, "region") == "region2") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"< X < "~t2)==Pr~"\n\n,"~ mu == media ~ sigma == varen,
                                       list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
          }

          if (attr(q, "region") == "region4") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"<= X <= "~t2)==Pr~"\n\n,"~mu == media ~ sigma == varen,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, media = mu, varen = sigma)))
          }

          if (attr(q, "region") == "region7") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"<= X < "~t2)==Pr~"\n\n,"~mu == media ~ sigma == varen,
                                     list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen = sigma)))
          }

          if ( attr(q, "region") == "region8") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1~"< X <= "~t2)==Pr~"\n\n,"~mu == media ~ sigma == varen,
                                        list(t1=qq[1], t2=qq[2], Pr=Pr, media = mu,varen=sigma)))
          }
        }
        if (gui == "plot") {
          # Probability
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q[2], mean = mu, sd=sigma) - pnorm(q = q[1], mean = mu, sd=sigma)
          # Plot
          plotcurve(q, mu, sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }

      }
    }
    if (any(attr(q, "region") == regiona)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline("Insert the value of degree of freedom (df): ")
          argaddit$df <- as.numeric(df)
        }
        plotcurve <- function(q, nu) {
          x <- seq(-6, q[1], by=0.01)
          z <- seq(q[2], 6, by=0.01)
          y <- seq(-6, 6, by=0.01)
          fx <- dt(x, df = nu)
          fz <- dt(z, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[X](x)), xlab="X",
                ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray"),
                main = gettext("Student's t-distribution", domain = "R-leem"))
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
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(-6, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], 6)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qq, lty=2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >"~t2~";"~nu==df)==Pr~"\n\n",
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X >="~t2~";"~nu==df)==Pr~"\n\n",
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X > "~t2~";"~nu==df)==Pr~"\n\n",
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
          }
          if (attr(q, "region") == "region6") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >= "~t2~";"~nu==df)==Pr~"\n\n",
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, df = nu)))
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
      if (dist == "gumbel") {
        if (!any(names(argaddit) == "location")) {
          location <- readline("Insert the value of 'location' argument: ")
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline("Insert the value of 'scale' argument: ")
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
        plotcurve <- function(q, location, scale){
          # variação de x
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }

          curve(dgumbel(x, location, scale),xvq, xvq1,ylim = c(0,1.5*(dgumbel(1, location, scale))),xlim = c(xvq,xvq1), ylab = expression(f[G](g)), xlab = "G",panel.first = grid(col = "gray"))
          aux <- seq(xvq,xvq1, by = 0.01)
          y <- seq(q[1],q[2],by = 0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col = "red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col = "gray90")
          abline(v = location, lty = 2)
          locaux <- location
          scaux <- scale
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgumbel(q[1], location, scale) + pgumbel(q[2], location, scale), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = as.character(c(xvq1,xvq)),labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
          axis(side = 1, at = qqaux, labels = qqaux,
               col ="black", font = 2)
          abline(v = qqaux, lty = 2, col = "red")

          if (attr(q, "region") == "region1") {
            legend("topleft", bty ="n", fill ="red",cex=0.8,
                   legend =substitute(P(t1~"> G > "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                      list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~">= G >= "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend = substitute(P(t1~">= G > "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                       list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region6") {
            legend("topleft", bty="n", fill = "red",cex=0.8,
                   legend=substitute(P(t1~"> G >= "~t2)==Pr~"\n\n"~scale==scaux~location==locaux,
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q[1], location, scale) + pgumbel(q = q[2], location, scale)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          # variação de x
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          plotcurveaux <- function(q1 = q[1], q2 = q[2], location,scale) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, location, scale)
          }
          manipulate::manipulate(plotcurveaux(q1 , q2 , location, scale),
                                 q1 = manipulate::slider(xvq, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], xvq1, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = q[1], location, scale) + pgumbel(q = q[2], location, scale)
        }
      }
      if (dist == "normal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline("Insert the value of 'mean' argument: ")
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline("Insert the value of 'sd' argument: ")
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
        plotcurve <- function(q, mu, sigma) {


          x <- seq(mu - 4 * sigma, q[1], by = 0.01)
          z <- seq(q[2], mu + 4*sigma, by = 0.01)
          y <-seq(mu - 4*sigma, mu + 4*sigma, by = 0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fz <- dnorm(z,mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)

          curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma,
                ylim = c(0, 1.2 * max(fx,fy,fz)),xlab="X",
                ylab = expression(f[X](x)),
                panel.first = grid(col="gray"))

          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")


          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")

          polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
                  col="red" )


          abline(v=argaddit$mean, lty=2)

          qq <- round(q, digits=2)
          qqaux <- qq
          Pr <- round(pnorm(q[1], mean = mu,sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd=sigma, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)

          axis(side=1, at=qq, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1)
          axis(side=1, at=as.character(c(mu - 4*sigma, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], mu + 4*sigma)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)


          abline(v = qqaux, lty=2, col = "red")

          if (attr(q, "region") == "region1") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"> X > "~t2)==Pr~"\n\n,"~mu == media ~ sigma == varen,
                                       list(t1=qq[1],t2=qq[2], Pr = Pr, media = mu, varen = sigma)))
          }

          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~">= X >= "~t2)==Pr~"\n\n,"~mu == media ~ sigma == varen,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, media = mu, varen=sigma)))
          }

          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X > "~t2)==Pr~"\n\n,"~ mu == media  ~ sigma == varen,
                                     list(t1=qq[1],t2=qq[2], Pr=Pr, media = mu, varen = sigma)))
          }

          if ( attr(q, "region") == "region6") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1~"> X >= "~t2)==Pr~"\n\n,"~ mu == media ~ sigma == varen,
                                        list(t1=qq[1], t2=qq[2], Pr=Pr, media = mu,varen = sigma)))
          }
        }

        if (gui == "plot") {
          # Probability
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)

          # Plot
          plotcurve(q, mu, sigma)
        }

        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }
      }
    }
  } else {
    if (dist == "t-student") {
      if (!any(names(argaddit) == "df")) {
        df <- readline("Insert the value of degree of freedom (df): ")
        argaddit$df <- as.numeric(df)
      }
      if (lower.tail) {
        # Desabilitar warnings global
        #options(warn = - 1)
        war <- options(warn = - 1)
        on.exit(options(war))

        plotcurve <- function(q, nu) {
          x <- seq(-6, q, by=0.01)
          y <- seq(q, 6, by=0.01)
          fx <- dt(x, df = nu)
          fy <- dt(y, df = nu)

          curve(dt(x, df = nu), -6, 6, ylab = expression(f[X](X)),
                xlab="X", ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray"),
                main = gettext("Student's t-distribution", domain = "R-leem"))

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
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(-6, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bg = "white", bty="n", fill="red",
                 legend=substitute(P(X<=q~";"~nu==df)==Pr~"\n\n", list(q=qq, Pr=Pr, df = nu)))
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
          manipulate::manipulate(plotcurve(q, nu),
                                 q = manipulate::slider(-6, 6, q),
                                 nu = manipulate::slider(1, 200, nu))
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
        #options(warn = - 1)
        war <- options(warn = - 1)
        on.exit(options(war))

        plotcurve <- function(q, nu) {
          x <- seq(q, 6, by=0.01)
          y <- seq(-6, q, by=0.01)
          fx <- dt(x, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[X](x)),
                xlab="X", ylim = c(0, 1.2 * max(c(fx,fy))), panel.first = grid(col = "gray"),
                main = gettext("Student's t-distribution", domain = "R-leem"))

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
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(qqaux, 6)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`>`~q~";"~nu==df)==Pr~"\n\n", list(q=qq, Pr=Pr, df = nu)))
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
          manipulate::manipulate(plotcurve(q, nu),
                                 q = manipulate::slider(-6, 6, q),
                                 nu = manipulate::slider(1, 200, nu))
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
          war <- options(warn = - 1)
          on.exit(options(war))

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
    if (dist == "gumbel") {
      if (!any(names(argaddit) == "location")) {
        location <- readline("Insert the value of 'location' argument: ")
        argaddit$location <- as.numeric(location)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline("Insert the value of 'scale' argument: ")
        argaddit$scale <- as.numeric(scale)
      }
      if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        # variação de x
        xvq <- 5*q
        xvq1 <- -5*q
        if ( q >= 0) {
          xvq <- 5*q
          xvq1 <- -5*q
        }
        if ( q == 0 ) { xvq <- 5*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -5*(1 + q) }
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale),xvq1, xvq,ylim = c(0,1.5*(dgumbel(1, location, scale))),xlim = c(xvq1,xvq), ylab = expression(f[G](g)), xlab = "G",panel.first = grid(col = "gray"))
          aux <- seq(q, xvq, by=0.01)
          y <- seq(xvq1, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col="gray90")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="red")
          abline(v = location, lty=2)
          locaux <- location
          scaux <- scale
          qq <- round(q, digits=2)
          qqaux <- qq
          Pr <- round(pgumbel(qq, location, scale), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = as.character(c(xvq1,xvq)),labels = FALSE, lwd.ticks = 0 , col = "black", font = 2)
          axis(side=1, at=as.character(c(xvq1,qqaux)), labels=TRUE, col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",cex=0.8,
                 legend=substitute(P(G<=q)==Pr~"\n\n"~scale==scaux~location==locaux,
                                   list(q=qq,scaux=scaux,locaux=locaux, Pr = Pr)))
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
        # variação de x
        xvq <- 5*q
        xvq1 <- -5*q
        if ( q >= 0) {
          xvq <- 5*q
          xvq1 <- -5*q
        }
        if ( q == 0 ) { xvq <- 5*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -5*(1 + q) }
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale),xvq1, xvq,ylim = c(0,1.5*(dgumbel(1, location, scale))),xlim = c(xvq1,xvq), ylab = expression(f[G](g)), xlab = "G",panel.first = grid(col = "gray"))
          aux <- seq(q, xvq, by=0.01)
          y <- seq(xvq1, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v = location, lty=2)
          locaux <- location
          scaux <- scale
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pgumbel(qq, location, scale, lower.tail = FALSE), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = as.character(c(xvq1,xvq)),labels = FALSE, lwd.ticks = 0 , col = "black", font = 2)
          axis(side=1, at=as.character(c(qqaux,xvq)), labels=TRUE, col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",cex=0.8,
                 legend=substitute(P(G>=q)==Pr~"\n\n"~scale==scaux~location==locaux,
                                   list(q=qq,scaux=scaux,locaux=locaux, Pr = Pr)))
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
    if (dist == "normal") {
      if (!any(names(argaddit) == "mean")) {
        mean <- readline("Insert the value of 'mean' argument: ")
        argaddit$mean <- as.numeric(mean)
      }
      if (!any(names(argaddit) == "sd")) {
        sd <- readline("Insert the value of 'sd' argument: ")
        argaddit$sd <- as.numeric(sd)
      }
      if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

      if (lower.tail) {
        plotcurve <- function(q, mu, sigma) {
          x <- seq(mu - 4 * sigma, q, by = 0.01)
          y <- seq(q, mu + 4 * sigma, by = 0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)

          curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
                ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
                panel.first = grid(col = "gray"),
                main = "Normal distribution")

          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray")
          abline(v=argaddit$mean, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`<`~q~";"~mu == media~","~sigma == varen)==Pr, list(q = qq, Pr = Pr, media = mu, varen = sigma)))
        }
        if (gui == "plot" ) {
          # Probability
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q, mean = mu, sd = sigma)
          # Plot
          plotcurve(q, mu,sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }
      } else{
        plotcurve <- function(q, mu, sigma) {
          x <- seq(q, mu + 4 * sigma, by=0.01)
          y <- seq(mu - 4 * sigma, q, by=0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)

          curve(dnorm(x, mean = mu, sd=sigma), mu - 4 * sigma, mu + 4 * sigma,
                ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
                panel.first = grid(col = "gray"),
                main = "Normal distribution")

          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=argaddit$mean, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pnorm(qq, mean = mu, sd=sigma, lower.tail = FALSE), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`>`~q)==Pr~"\n\n"~mu == media~~sigma == varen, list(q=qq, Pr=Pr, media = mu, varen = sigma)))
        }
        if (gui == "plot") {
          # Probability
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q, mean = mu, sd=sigma)
          # Plot
          plotcurve(q, mu, sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }

      }
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}








