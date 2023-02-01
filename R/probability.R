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
  argdef <- formals(P)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regionb)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }
        plotcurve <- function(q, nu, ...) {
          x <- seq(q[1], q[2], by=0.01)
          y <- seq(-6, 6, by=0.01)
          fx <- dt(x, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[X](x)), xlab="X",
                ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: T-Student.", domain = "R-leem"))
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
          nu <- argaddit$df
          prob <- round(pt(q = q[2], df = nu) - pt(q = q[1], df = nu), rounding)
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
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        plotcurve <- function(q, location, scale){
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          curve(dgumbel(x, location, scale),xvq, xvq1,
                ylim = c(0,1.5*(dgumbel(1, location, scale))),
                xlim = c(xvq,xvq1), ylab = expression(f[G](g)),
                xlab = "G",panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Gumbel.", domain = "R-leem"))
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
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = max(q[1],q[2]), location, scale) - pgumbel(q = min(q[1],q[2]), location, scale)
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
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
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) {
          stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
        }
        plotcurve <- function(q, mu, sigma) {
          x <- seq(q[1], q[2], by = 0.01)
          y <- seq(mu - 4 * sigma, mu + 4 * sigma, by = 0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)
          curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
                ylab = expression(f[X](x)), xlab = "X",
                ylim = c(0, 1.2 * max(fx,fy)),
                panel.first = grid(col="gray90"),
                main = gettext("Probability Function: Normal.", domain = "R-leem"))
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
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q[2], mean = mu, sd=sigma) - pnorm(q = q[1], mean = mu, sd=sigma)
          plotcurve(q, mu, sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        plotcurve <- function(q, lambda) {
          rmin <- lambda - 4 * sqrt(lambda)
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- ceiling(lambda + 4 * sqrt(lambda))
          x <- rmin:rmax # rever
          x1 <- rmin:q[1]
          x2 <- (q[1] + 1):rmax # rever (dois elementos só pega o primeiro)
          x3 <- c(1, 2)
          if (attr(q, "region") == "region2") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if(attr(q, "region") == "region4"){
            x3 <- (q[1]):(q[2])
          } else if(attr(q, "region") == "region7") {
            x3 <- (q[1]):(q[2]-1)
          } else {
            x3 <- (q[1]+1):(q[2])
          }
          probx <- dpois(x, lambda = lambda)
          probx1 <- dpois(x1, lambda = lambda)
          probx2 <- dpois(x2, lambda = lambda)
          probx3 <- dpois(x3, lambda = lambda)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = gettext("Probability Function: Poisson.", domain = "R-leem"))
          lines(x1, probx1, type = "h", lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x3, probx3, type = "h", panel.first = grid(), lwd = 2, col = "red")
          points(x3, probx3, lwd = 2, col = "red", pch = 19)
          abline(v = lambda, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(ppois(q = q[1], lambda = lambda) - ppois(q=q[2], lambda=lambda),digits = rounding)*-1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X < " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X <= " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X < " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X <= " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
        }
        if (gui == "plot") {
          # Probability
          lambda <- argaddit$lambda
          prob <- (ppois(q = q[1], lambda = lambda) - ppois(q = q[2], lambda=lambda))*-1
          # Plot
          plotcurve(q, lambda)
        }
        if (gui == "rstudio") {
          lambda <- argaddit$lambda
          plotcurveaux <- function(q1 = q[1], q2 = q[2],lambda) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,lambda)
          }
          prob <- (ppois(q = q[1], lambda = lambda) - ppois(q = q[2],lambda=lambda))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, lambda),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 lambda = manipulate::slider(lambda, lambda + 200, lambda))
        }
      }
      if (dist == "beta") {
        if (!any(names(argaddit) == "alpha")) {
          alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
          argaddit$alpha <- as.numeric(alpha)
        }
        if (!any(names(argaddit) == "beta")) {
          beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
          argaddit$beta <- as.numeric(beta)
        }
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta
        plotcurve <- function(q, shape1, shape2) {
          curve(dbeta(x, shape1, shape2), 0, 1 ,
                ylab = expression(f[X](x)), xlab = "X",
                panel.first = grid(col="gray90"), main = expression("Probability Function: Beta.", domain = "R-leem"))
          x <- seq(q[1],q[2], by = 0.01)
          y <- seq(0, 1, by = 0.01)
          fx <- dbeta(x, shape1, shape2)
          fy <- dbeta(y, shape1, shape2)
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          abline(v=argaddit$alpha, lty=2)
          qq <- round(q, digits=2)
          qqaux <- qq
          Pr <- round(pbeta(q[2], shape1,shape2, lower.tail = T) - pbeta(q[1], shape1, shape2, lower.tail = T), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          abline(v = qqaux, lty=2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"< X < "~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region4") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"<= X <= "~t2~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region7") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"<= X < "~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                     list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if ( attr(q, "region") == "region8") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1~"< X <= "~t2~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                        list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
        }
        if (gui == "plot") {
          shape1 <- argaddit$alpha
          shape2 <- argaddit$beta
          prob <- pbeta(q = q[2], shape1, shape2) - pbeta(q = q[1], shape1, shape2)
          plotcurve(q, shape1, shape2)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(q[1], q[2], q),
                                 muaux = manipulate::slider(shape1, shape1 + 200, shape1))
        }
      }
      if (dist == "exp") {
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument: ",  domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        plotcurve <- function(q, rate) {
          rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          x <- seq(q[1], q[2], by = 0.01)
          y <- seq(0, rmax, by = 0.01)
          probx <- dexp(x, rate = rate)
          proby <- dexp(y, rate = rate)
          curve(dexp(x, rate), 0, rmax,
                ylab = expression(p[x](q)),
                xlab = "x", ylim = c(0, 1.2 * max(c(probx, proby))),
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Exponential.", domain = "R-leem"))
          polygon(c(y, rev(y)),
                  c(proby, rep(0,length(proby))),
                  col = "gray90")
          polygon(c(x, rev(x)),
                  c(probx, rep(0,length(probx))),
                  col = "red")
          abline(v = 1 / rate, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pexp(qq[2], rate = rate, lower.tail = T) -
                        pexp(qq[1], rate = rate, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          rate2 <- gsub("\\.", ",", rate)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X <" ~ t2 ~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2], q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X <= " ~ t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X < " ~ t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X <= " ~ t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate = rate))*-1
          plotcurve(q, rate)
        }
        if (gui == "rstudio") {
          rate <- argaddit$rate
          plotcurveaux <- function(q1 = q[1], q2 = q[2],rate) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, rate)
          }
          prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate = rate))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, rate),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 lambda = manipulate::slider(rate, rate + 200, rate))
        }
      }
      if (dist == "binomial") {
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        size <- argaddit$size
        sucesso <- argaddit$prob
        plotcurve <- function(q, size, prob) {
          rmin <- size * prob - 4 * sqrt(size * prob * (1 - prob))
          if (rmin < 0 || rmin>q[1]) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q[1]
          x2 <- q[2]:size
          x3 <- c(1, 2)
          if (attr(q, "region") == "region2") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if (attr(q, "region") == "region4") {
            x3 <- (q[1]):(q[2])
          } else if (attr(q, "region") == "region7") {
            x3 <- (q[1]):(q[2] - 1)
          } else {
            x3 <- (q[1] + 1):(q[2])
          }
          probx <- dbinom(x, size = size, prob = prob)
          probx1 <- dbinom(x1, size = size, prob = prob)
          probx2 <- dbinom(x2, size = size, prob = prob)
          probx3 <- dbinom(x3, size = size, prob = prob)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",main="Probability Function: Binomial.")
          lines(x1, probx1, type = "h", lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x3, probx3, lwd = 2, col = "red", pch = 19)
          abline(v = size * prob, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pbinom(q[1], size = size, prob = prob, lower.tail = T) - pbinom(q[2], size = size, prob = prob, lower.tail = T), rounding) * -1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "< X < " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "<= X <= " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "<= X < " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "< X <= " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
        }
        if (gui == "plot") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          plotcurve(q, size, prob = sucesso)
        }
        if (gui == "rstudio") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          manipulate::manipulate(plotcurve(q, size, prob),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(size, size + 30, size),
                                 prob = manipulate::slider(sucesso, 1, sucesso)
          )
        }
      }
      if (dist == "hyper") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        if (!any(names(argaddit) == "k")) {
          k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
          argaddit$k <- as.numeric(k)
        }
        size <- argaddit$m
        samples <- argaddit$n
        sucess <- argaddit$k
        plotcurve <- function(q, size, samples, sucess) {
          rmin <- 0
          if (rmin < 0 || rmin > q[1]) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q[1]
          x2 <- q[2]:size
          x3 <- c(1, 2)
          if (attr(q, "region") == "region2") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if (attr(q, "region") == "region4") {
            x3 <- (q[1]):(q[2])
          } else if (attr(q, "region") == "region7") {
            x3 <- (q[1]):(q[2] - 1)
          } else {
            x3 <- (q[1] + 1):(q[2])
          }
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          probx3 <- dhyper(x3, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Probability Function: Hypergeometric.")
          lines(x1, probx1, type = "h", lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x3, probx3, lwd = 2, col = "red", pch = 19)
          abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)),dhyper(x = x, m = size, n = samples, k = sucess))-1, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q[1], m = size, n = samples, k = sucess) - phyper(q[2], m = size, n = samples, k = sucess), rounding) * -1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "< X < " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "<= X <= " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "<= X < " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "< X <= " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
        }
        if (gui == "plot") {
          result <- phyper(q = q, m = size, n = samples, k = sucess)
          prob <- result[2]
          plotcurve(q, size, samples, sucess)
        }
      }
      if (dist == "nbinom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        if (argaddit$prob > 1 ) {
          stop("The 'prob' argument must be lower then zero!", call. = FALSE, domain = "R-leem")
        }
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
              xvq <- 2*q[2]
              xvq1 <- 2*q[1]
              if ( q[1] >= 0) {
                xvq <- 2*q[2]
                xvq1 <- -2*q[1]
              }
              if ( q[1] == 0 ) { xvq <- 10 }
              if ( q[2] == 0 ) { xvq1 <- -10 }
              plotcurve <- function(q, s, p){
                x <- xvq1:xvq
                fx <- dnbinom(x,s,p)
                xlim <- c(xvq1, xvq)
                ylim <- c(min(fx), max(fx) + (max(fx)/2))
                plot.new()
                plot.window(xlim, ylim)
                axis(1)
                axis(2)
                title(ylab = expression(f[X](x)), xlab = "X", main = gettext("Probability Function: Negative Binomial.", domain = "R-leem"))
                if (attr(q, "region") == "region2") {
                  x2 <- (q[1] + 1):(q[2] - 1)
                } else if(attr(q, "region") == "region4"){
                  x2 <- (q[1]):(q[2])
                } else if(attr(q, "region") == "region7") {
                  x2 <- (q[1]):(q[2] - 1 )
                } else {
                  x2 <- (q[1] + 1 ):(q[2])
                }
                x1 <- xvq1:xvq
                fx1 <- dnbinom(x1,s,p)
                lines(x1, fx1, type = "h", lwd = 2)
                points(x1, fx1, lwd = 2, pch = 19)
                fx2 <- dnbinom(x2,s,p)
                lines(x2, fx2, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
                points(x2, fx2, lwd = 2, col = "red", pch = 19)
                title(ylab = expression(f[X](x)), xlab = "X")
                qq <- round(q, digits = rounding)
                qqaux <- round(q, digits = rounding)
                Pr <- round(pnbinom(qq[2],s,p) - pnbinom(qq[1],s,p), rounding)
                Pr <- gsub("\\.", ",", Pr)
                qq <- gsub("\\.", ",", qq)
                axis(side = 1, at = c(q[1],q[2]),labels = c(q[1],q[2]),col = "red", font = 2, col.axis = "red")
                abline(v = q[1], lty = 2, col = "red")
                abline(v = q[2], lty = 2, col = "red")
                if (attr(q, "region") == "region2") {
                  legend("topleft",
                         bty = "n", fill = "red",
                         legend = substitute(
                           P(t1 ~ "< X < " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                           list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                         )
                  )
                }
                if (attr(q, "region") == "region4") {
                  legend("topleft",
                         bty = "n", fill = "red",
                         legend = substitute(
                           P(t1 ~ "<= X <= " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                           list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                         )
                  )
                }
                if (attr(q, "region") == "region7") {
                  legend("topleft",
                         bty = "n", fill = "red",
                         legend = substitute(
                           P(t1 ~ "<= X < " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                           list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                         )
                  )
                }
                if (attr(q, "region") == "region8") {
                  legend("topleft",
                         bty = "n", fill = "red",
                         legend = substitute(
                           P(t1 ~ "< X <= " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                           list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                         )
                  )
                }
              }
              if (gui == "plot" ) {
                pro <- argaddit$prob
                size <- argaddit$size
                prob <- pnbinom(q = q[2] , size = size , prob = pro) - pnbinom(q = q[1] , size = size , prob = pro)
                plotcurve(q,size,pro)
              }
              if (gui == "rstudio") {
                xvq <- 2*q
                xvq1 <- -2*q
                if ( q >= 0) {
                  xvq <- 2*q
                  xvq1 <- -2*q
                }
                if ( q == 0 ) { xvq <- 10*( 1 + q) }
                if ( q == 0 ) { xvq1 <- -10*(1 + q) }
                pro <- argaddit$prob
                size <- argaddit$size
                manipulate::manipulate(plotcurve(q, pro, size),
                                       q = manipulate::slider(xvq1,xvq, q),
                                       pro = manipulate::slider(0.1,1, pro ),
                                       size = manipulate::slider(1,xvq, size))
                prob <- pnbinom(q,pro,size,lower.tail = TRUE)
              }
            }
      if (dist == "geometric") {
        if (!any(names(argaddit) == "probability")) {
          probability <- readline(gettext("Insert the value of 'probability' argument: ", domain = "R-leem"))
          argaddit$probability <- as.numeric(probability)
        }
        plotcurve <- function(q, probability) {
          rmin <- -5*q[1]
          rmax <-  5*q[2]
          if (rmin < 0) rmin <- 0
          x <- rmin:rmax # rever
          x1 <- rmin:q[1]
          x2 <- q[2]:rmax # rever (dois elementos só pega o primeiro)
          x3 <- c(1, 2)
          if (attr(q, "region") == "region2") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if(attr(q, "region") == "region4"){
            x3 <- (q[1]):(q[2])
          } else if(attr(q, "region") == "region7") {
            x3 <- (q[1]):(q[2]-1)
          } else {
            x3 <- (q[1]+1):(q[2])
          }
          probx <- dgeom(x, p = probability)
          probx1 <- dgeom(x1, p = probability)
          probx2 <- dgeom(x2, p = probability)
          probx3 <- dgeom(x3, p = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X", main = gettext("Probability Function: Geometric.", domain = "R-leem"))
          lines(x1, probx1, type = "h", lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x3, probx3, lwd = 2, col = "red", pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(q = q[1], p = probability) - pgeom(q = q[2], p = probability),digits = rounding)*- 1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X < " ~ t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X <= " ~ t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "<= X < " ~ t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "< X <= " ~ t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
        }

        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- (pgeom(q[1], p = probability) - pgeom(q[2], p = probability))*-1
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          plotcurveaux <- function(q1 = q[1], q2 = q[2],p = probability) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,probability)
          }
          prob <- (pgeom(q = q[1], p = probability) - pgeom(q = q[2], p = probability))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, p = probability),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 probability = manipulate::slider(p, p + 200, p))
        }
      }
      }
    if (any(attr(q, "region") == regiona)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
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
                ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: T-Student.", domain = "R-leem"))
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
          nu <- argaddit$df
          prob <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
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
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
        plotcurve <- function(q, location, scale){
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          curve(dgumbel(x, location, scale),xvq, xvq1,ylim = c(0,1.5*(dgumbel(1, location, scale)))
                ,xlim = c(xvq,xvq1),
                ylab = expression(f[G](g)),
                xlab = "G",panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Gumbel.", domain = "R-leem"))
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
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q[1], location, scale) + pgumbel(q = q[2], location, scale)
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
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
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
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
                panel.first = grid(col="gray90"),
                main = gettext("Probability Function: Normal.", domain = "R-leem"))
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
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)
          plotcurve(q, mu, sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        plotcurve <- function(q, lambda) {
          rmin <- lambda - 4 * sqrt(lambda)
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- ceiling(lambda + 4 * sqrt(lambda))
          x <- rmin:rmax
          x1 <- rmin:q[1]
          x2 <- q[2]:rmax
          x3 <- c(1, 2)
          if (attr(q, "region") == "region1") {
            x3 <- q[1]:q[2]
          } else if(attr(q, "region") == "region3"){
            x3 <- (q[1]+1):(q[2]-1)
          } else if(attr(q, "region") == "region5") {
            x3 <- (q[1]+1):(q[2])
          } else {
            x3 <- (q[1]):(q[2]-1)
          }
          probx <- dpois(x, lambda = lambda)
          probx1 <- dpois(x1, lambda = lambda)
          probx2 <- dpois(x2, lambda = lambda)
          probx3 <- dpois(x3, lambda = lambda)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = gettext("Probability Function: Poisson.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(), lwd = 2,col="red")
          points(x1, probx1, lwd = 2, pch = 19,col="red")
          lines(x2, probx2, type = "h", lwd = 2,col="red")
          points(x2, probx2, lwd = 2, pch = 19,col="red")
          lines(x3, probx3, type = "h", panel.first = grid(), lwd = 2)
          points(x3, probx3, lwd = 2, pch = 19)
          abline(v = lambda, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(ppois(q = q[1], lambda = lambda, lower.tail = T) - ppois(q=q[2], lambda=lambda, lower.tail = F),digits = rounding)*-1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at =x1, labels =x1 ,
            col = "red", font = 2
          )
          axis(
            side = 1, at =x2, labels =x2 ,
            col = "red", font = 2
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X > " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X >= " ~ t2 )== Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X > " ~ t2) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(q[1] ~ "> X >= " ~ q[2]) == Pr ~ "\n\n" ~ lambda == lambd,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, lambd = lambda)
                   )
            )
          }
        }

        if (gui == "plot") {
          lambda <- argaddit$lambda
          prob <- (ppois(q = q[1], lambda = lambda, lower.tail = t)) - (ppois(q = q[2],lambda = lambda, lower.tail = F))*-1
          plotcurve(q, lambda)
        }
        if (gui == "rstudio") {
          lambda <- argaddit$lambda
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          plotcurveaux <- function(q1 = q[1], q2 = q[2],lambda) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,lambda)
          }
          prob <- (ppois(q = q[1], lambda = lambda) - ppois(q = q[2],lambda=lambda))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, lambda),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 lambda = manipulate::slider(lambda, lambda + 200, lambda))
        }
      }
      if (dist == "beta") {
        if (!any(names(argaddit) == "alpha")) {
          alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
          argaddit$alpha <- as.numeric(alpha)
        }
        if (!any(names(argaddit) == "beta")) {
          beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
          argaddit$beta <- as.numeric(beta)
        }
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta
        plotcurve <- function(q, shape1, shape2) {
          curve(dbeta(x, shape1, shape2), 0, 1,
                xlab="X",
                ylab = expression(f[X](x)),
                panel.first = grid(col="gray90"), main = gettext("Probability Function: Beta.", domain = "R-leem"))
          x <- seq(0, q[1], by = 0.01)
          z <- seq(q[2],1, by = 0.01)
          y <-seq(q[1], q[2], by = 0.01)
          fx <- dbeta(x, shape1, shape2)
          fz <- dbeta(z, shape1, shape2)
          fy <- dbeta(y, shape1, shape2)
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
                  col="red" )
          abline(v=argaddit$alpha, lty=2)
          qq <- round(q, digits=2)
          qqaux <- qq
          Pr <- round(pbeta(q[1], shape1, shape2, lower.tail = T) + pbeta(q[2], shape1, shape2, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(0, qqaux[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qqaux[2], 1)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~"> X > "~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1~">= X >= "~ t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X > "~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                     list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if ( attr(q, "region") == "region6") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1~"> X >= "~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                        list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
        }
        if (gui == "plot") {
          shape1 <- argaddit$alpha
          shape2 <- argaddit$beta
          prob <- pbeta(q[1], shape1, shape2, lower.tail = T) + pbeta(q[2], shape1, shape2, lower.tail = F)
          plotcurve(q, shape1, shape2)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(0, 1, q),
                                 muaux = manipulate::slider(shape1, shape1 + 200, shape1))
        }
      }
      if (dist == "exp") {
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument: ",  domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        plotcurve <- function(q, rate) {
          rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          x1 <- seq(0, q[1], by = 0.01)
          x2 <- seq(q[2], rmax, by = 0.01)
          y <- seq(0, rmax, by = 0.01)
          probx1 <- dexp(x1, rate = rate)
          probx2 <- dexp(x2, rate = rate)
          proby <- dexp(y, rate = rate)
          curve(dexp(x, rate), 0, rmax,
                ylab = expression(p[x](q)),
                xlab = "x", ylim = c(0, 1.2 * max(c(probx1, probx2, proby))),
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Exponential.", domain = "R-leem"))
          polygon(c(y, rev(y)),
                  c(proby, rep(0,length(proby))),
                  col = "gray90")
          polygon(c(x1, rev(x1)),
                  c(probx1, rep(0,length(probx1))),
                  col = "red")
          polygon(c(x2, rev(x2)),
                  c(probx2, rep(0,length(probx2))),
                  col = "red")
          abline(v = 1 / rate, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round((pexp(q = q[1], rate = rate) - pexp(q = q[2],rate=rate))*-1, rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          rate2 <- gsub("\\.", ",", rate)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X > " ~ t2) == Pr ~ "\n\n" ~ rate == rat,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X >= " ~ t2 )== Pr ~ "\n\n" ~ rate == rat,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X > " ~ t2) == Pr ~ "\n\n" ~ rate == rat,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(q[1] ~ "> X >= " ~ q[2]) == Pr ~ "\n\n" ~ rate == rat,
                     list(t1 = qq[1],t2 = qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate=rate))*-1
          plotcurve(q, rate)
        }
        if (gui == "rstudio") {
          rate <- argaddit$rate
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          plotcurveaux <- function(q1 = q[1], q2 = q[2],rate) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,rate)
          }
          prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate=rate))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, rate),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 lambda = manipulate::slider(rate, rate + 200, rate))
        }
      }
      if (dist == "binomial") {
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        size <- argaddit$size
        sucesso <- argaddit$prob
        plotcurve <- function(q, size, prob) {
          rmin <- size * prob - 4 * sqrt(size * prob * (1 - prob))
          if (rmin < 0 || rmin>q[1]) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q[1]
          x2 <- (q[1] + 1):size
          x3 <- c(1, 2)
          if (attr(q, "region") == "region1") {
            x3 <- q[1]:q[2]
          } else if (attr(q, "region") == "region3") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if (attr(q, "region") == "region5") {
            x3 <- (q[1] + 1):(q[2])
          } else {
            x3 <- (q[1]):(q[2] - 1)
          }
          probx <- dbinom(x, size = size, prob = prob)
          probx1 <- dbinom(x1, size = size, prob = prob)
          probx2 <- dbinom(x2, size = size, prob = prob)
          probx3 <- dbinom(x3, size = size, prob = prob)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",main="Probability Function: Binomial.")
          lines(x1, probx1, type = "h", panel.first = grid(col="gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", panel.first = grid(col="gray90"), lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, col = "red", pch = 19)
          lines(x3, probx3, type = "h", lwd = 2)
          points(x3, probx3, lwd = 2, pch = 19)
          abline(v = size * prob, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pbinom(q[1], size = size, prob = prob, lower.tail = T) - pbinom(q[2], size = size, prob = prob, lower.tail = T), rounding) * -1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(rmin,qqaux[1]), labels = c("",qqaux[1]),
            col = "red", font = 2, col.axis = "red"
          )
          axis(
            side = 1, at = c(qqaux[2],size), labels = c(qqaux[2],""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "> X > " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob, list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ ">= X >= " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ ">= X > " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "> X >= " ~ t2) == Pr ~ "\n\n" ~ size == n ~ p == prob,
                                       list(t1 = qq[1], t2 = qq[2], q = qq, Pr = Pr, n = size, prob = prob))
            )
          }
        }
        if (gui == "plot") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          plotcurve(q, size, prob = sucesso)
        }
        if (gui == "rstudio") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          manipulate::manipulate(plotcurve(q, size, prob),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(size, size + 30, size),
                                 prob = manipulate::slider(sucesso, 1, sucesso)
          )
        }
      }
      if (dist == "hyper") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        if (!any(names(argaddit) == "k")) {
          k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
          argaddit$k <- as.numeric(k)
        }
        size <- argaddit$m
        samples <- argaddit$n
        sucess <- argaddit$k
        plotcurve <- function(q, size, samples, sucess) {
          rmin <- 0
          if (rmin < 0 || rmin > q[1]) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q[1]
          x2 <- (q[1] + 1):size
          x3 <- c(1, 2)
          if (attr(q, "region") == "region1") {
            x3 <- q[1]:q[2]
          } else if (attr(q, "region") == "region3") {
            x3 <- (q[1] + 1):(q[2] - 1)
          } else if (attr(q, "region") == "region5") {
            x3 <- (q[1] + 1):(q[2])
          } else {
            x3 <- (q[1]):(q[2] - 1)
          }
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          probx3 <- dhyper(x3, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Probability Function: Hypergeometric.")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, col = "red", pch = 19)
          lines(x3, probx3, type = "h", lwd = 2)
          points(x3, probx3, lwd = 2, pch = 19)
          abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)),
                           dhyper(x = x, m = size, n = samples, k = sucess))-1, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q[1], m = size, n = samples, k = sucess) - phyper(q[2], m = size, n = samples, k = sucess), rounding) * -1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(rmin, qqaux[1]), labels = c("", qqaux[1]),
            col = "red", font = 2, col.axis = "red"
          )
          axis(
            side = 1, at = c(qqaux[2], size), labels = c(qqaux[2], ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "> X > " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ ">= X >= " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ ">= X > " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess, list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1 ~ "> X >= " ~ t2) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess, list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
        }
        if (gui == "plot") {
          result <- phyper(q = q, m = size, n = samples, k = sucess)
          prob <- result[2]#2 results?
          plotcurve(q, size, samples, sucess)
        }
      }
      if (dist == "nbinom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        if (argaddit$prob > 1 ) {
          stop("The 'prob' argument must be lower then zero!", call. = FALSE, domain = "R-leem")
        }
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        xvq <- 2*q[2]
        xvq1 <- 2*q[1]
        if ( q[1] >= 0) {
          xvq <- 2*q[2]
          xvq1 <- -2*q[1]
        }
        if ( q[1] == 0 ) { xvq <- 10 }
        if ( q[2] == 0 ) { xvq1 <- -10 }
        plotcurve <- function(q, s, p){
          x <- xvq1:xvq
          fx <- dnbinom(x,s,p)
          xlim <- c(xvq1, xvq)
          ylim <- c(min(fx), max(fx) + (max(fx)/2))
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Probability Function: Negative Binomial.", domain = "R-leem"))
          if (attr(q, "region") == "region1") {
            x2 <- (q[1]-1):(q[2]+1)
          } else if(attr(q, "region") == "region3"){
            x2 <- (q[1]+1):(q[2]-1)
          } else if(attr(q, "region") == "region5") {
            x2 <- (q[1]+1):q[2]
          } else {
            x2 <- q[1]:(q[2]-1)
          }
          x1 <- xvq1:xvq
          fx1 <- dnbinom(x1,s,p)
          lines(x1, fx1, type = "h", lwd = 2,col = "red", panel.first = grid(col = "gray90"))
          points(x1, fx1, lwd = 2, pch = 19, col = "red")
          fx2 <- dnbinom(x2,s,p)
          lines(x2, fx2, type = "h", lwd = 2)
          points(x2, fx2, lwd = 2, pch = 19)
          qq <- round(q, digits = rounding)
          qqaux <- round(q, digits = rounding)
          Pr <- round(pnbinom(qq[2],s,p) - pnbinom(qq[1],s,p), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(q[1],q[2]),labels = c(q[1],q[2]),col = "red", font = 2, col.axis = "red")
          abline(v = q[1], lty = 2, col = "red")
          abline(v = q[2], lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X > " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X >= " ~ t2 )== Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X > " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X >= " ~ t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
        }
        if (gui == "plot" ) {
          pro <- argaddit$prob
          size <- argaddit$size
          prob <- pnbinom(q = q[2] , size = size , prob = pro) - pnbinom(q = q[1] , size = size , prob = pro)
          plotcurve(q,size,pro)
        }
        if (gui == "rstudio") {
          xvq <- 2*q
          xvq1 <- -2*q
          if ( q >= 0) {
            xvq <- 2*q
            xvq1 <- -2*q
          }
          if ( q == 0 ) { xvq <- 10*( 1 + q) }
          if ( q == 0 ) { xvq1 <- -10*(1 + q) }
          pro <- argaddit$prob
          size <- argaddit$size
          manipulate::manipulate(plotcurve(q, pro, size),
                                 q = manipulate::slider(xvq1,xvq, q),
                                 pro = manipulate::slider(0.1,1, pro ),
                                 size = manipulate::slider(1,xvq, size))
          prob <- pnbinom(q,pro,size,lower.tail = TRUE)
        }
      }
      if (dist == "geometric") {
        if (!any(names(argaddit) == "probability")) {
          probability <- readline(gettext("Insert the value of 'probability' argument: ", domain = "R-leem"))
          argaddit$probability <- as.numeric(probability)
        }
        plotcurve <- function(q, probability) {
          rmin <- -5*q[1]
          rmax <- +5*q[2]
          if (rmin < 0) rmin <- 0
          x <- rmin:rmax
          x1 <- rmin:q[1]
          x2 <- q[2]:rmax
          x3 <- c(1, 2)
          if (attr(q, "region") == "region1") {
            x3 <- q[1]:q[2]
          } else if(attr(q, "region") == "region3"){
            x3 <- (q[1]+1):(q[2]-1)
          } else if(attr(q, "region") == "region5") {
            x3 <- (q[1]+1):(q[2])
          } else {
            x3 <- (q[1]):(q[2]-1)
          }
          probx <- dgeom(x, p = probability)
          probx1 <- dgeom(x1, p = probability)
          probx2 <- dgeom(x2, p = probability)
          probx3 <- dgeom(x3, p = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X", main = gettext("Probability Function: Geometric.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2,col="red")
          points(x1, probx1, lwd = 2, pch = 19,col="red")
          lines(x2, probx2, type = "h", lwd = 2,col="red")
          points(x2, probx2, lwd = 2, pch = 19,col="red")
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x3, probx3, lwd = 2, pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(q = q[1], p = probability) - pgeom(q = q[2], p = probability),digits = rounding)*-1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at =x1, labels = x1 ,
            col = "red", font = 2
          )
          axis(
            side = 1, at =x2, labels =x2 ,
            col = "red", font = 2
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X > " ~ t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X >= " ~ t2 ~ " ;" ~ p == probability )== Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ ">= X > " ~ t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1 ~ "> X >= " ~ t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- (pgeom(q = q[1], p = probability) - pgeom(q = q[2], p = probability))*-1
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          xvq <- 5*q[1]
          xvq1 <- 5*q[2]
          if ( q[1] >= 0) {
            xvq <- -5*q[1]
            xvq1 <- 5*q[2]
          }
          if ( q[1] == 0 ) { xvq <- -5*( 1 + q[1]) }
          if ( q[2] == 0 ) { xvq1 <- 5*(1 + q[2]) }
          plotcurveaux <- function(q1 = q[1], q2 = q[2], probability) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,probability)
          }
          prob <- (pgeom(q = q[1], p = probability) - pgeom(q = q[2],p=probability))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, probability),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 p = manipulate::slider(probability, probability + 200, probability))
        }
      }
      }}
    else {
      if (dist == "t-student") {
      if (!any(names(argaddit) == "df")) {
        df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
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
                xlab="X", ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: T-Student.", domain = "R-leem"))
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
          nu <- argaddit$df
          prob <- pt(q = q, df = nu)
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
                xlab="X", ylim = c(0, 1.2 * max(c(fx,fy))), panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: T-Student.", domain = "R-leem"))

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
        location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
        argaddit$location <- as.numeric(location)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        xvq <- 5*q
        xvq1 <- -5*q
        if ( q >= 0) {
          xvq <- 5*q
          xvq1 <- -5*q
        }
        if ( q == 0 ) { xvq <- 5*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -5*(1 + q) }
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale),xvq1, xvq,
                ylim = c(0,1.5*(dgumbel(1, location, scale))),
                xlim = c(xvq1,xvq), ylab = expression(f[G](g)),
                xlab = "G",panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Gumbel.", domain = "R-leem"))
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
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale)
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
        xvq <- 5*q
        xvq1 <- -5*q
        if ( q >= 0) {
          xvq <- 5*q
          xvq1 <- -5*q
        }
        if ( q == 0 ) { xvq <- 5*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -5*(1 + q) }
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale),xvq1, xvq,
                ylim = c(0,1.5*(dgumbel(1, location, scale))),
                xlim = c(xvq1,xvq),
                ylab = expression(f[G](g)),
                xlab = "G",panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Gumbel.", domain = "R-leem"))
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
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
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
        mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
        argaddit$mean <- as.numeric(mean)
      }
      if (!any(names(argaddit) == "sd")) {
        sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
        argaddit$sd <- as.numeric(sd)
      }
      if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        plotcurve <- function(q, mu, sigma) {
          x <- seq(mu - 4 * sigma, q, by = 0.01)
          y <- seq(q, mu + 4 * sigma, by = 0.01)
          fx <- dnorm(x, mean = mu, sd = sigma)
          fy <- dnorm(y, mean = mu, sd = sigma)
          curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
                ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Normal.", domain = "R-leem"))
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
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
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q, mean = mu, sd = sigma)
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
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Normal.", domain = "R-leem"))
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
          mu <- argaddit$mean
          sigma <- argaddit$sd
          prob <- pnorm(q = q, mean = mu, sd=sigma)
          plotcurve(q, mu, sigma)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q),
                                 muaux = manipulate::slider(mu, mu + 200, mu))
        }
      }
    }
      if (dist == "poisson") {
      if (!any(names(argaddit) == "lambda")) {
        lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
        argaddit$lambda <- as.numeric(lambda)
      }
      if (lower.tail) {
        plotcurve <- function(q, lambda) {
          rmin <- lambda - 4 * sqrt(lambda)
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- ceiling(lambda + 4 * sqrt(lambda))
          x <- rmin:rmax
          x1 <- rmin:q
          x2 <- (q + 1):rmax
          probx <- dpois(x, lambda = lambda)
          probx1 <- dpois(x1, lambda = lambda)
          probx2 <- dpois(x2, lambda = lambda)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",
                main = gettext("Probability Function: Poisson.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          abline(v = lambda, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(ppois(qq, lambda = lambda, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(rmin,qqaux),labels=c(rmin,""),col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n" ~ lambda == lambd, list(q = qq, Pr = Pr, lambd = lambda)))
        }
        if (gui == "plot") {
          lambda <- argaddit$lambda
          prob <- ppois(q = q, lambda = lambda)
          plotcurve(q, lambda)
        }
        if (gui == "rstudio") {
          lambda <- argaddit$lambda
          prob <- ppois(q = q, lambda = lambda)
          manipulate::manipulate(plotcurve(q, lambda),
                                 q = manipulate::slider(0, q + 30, q),
                                 lambda = manipulate::slider(lambda, lambda + 200, lambda)
          )
        }
      } else {
        plotcurve <- function(q, lambda) {
          rmin <- lambda - 4 * sqrt(lambda)
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- ceiling(lambda + 4 * sqrt(lambda))
          x <- rmin:rmax
          x1 <- rmin:q
          x2 <- q[1]:rmax
          probx <- dpois(x, lambda = lambda)
          probx1 <- dpois(x1, lambda = lambda)
          probx2 <- dpois(x2, lambda = lambda)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",
                main = gettext("Probability Function: Poisson.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, col = "red", pch = 19)
          abline(v = lambda, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(ppois(qq, lambda = lambda, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(qqaux,rmax) ,labels = c(qqaux, ""), col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n" ~ lambda == lambd, list(q = qq, Pr = Pr, lambd = lambda))
          )
        }
        if (gui == "plot") {
          lambda <- argaddit$lambda
          prob <- ppois(q = q, lambda = lambda, lower.tail = F)
          plotcurve(q, lambda = lambda)
        }
        if (gui == "rstudio") {
          lambda <- argaddit$lambda
          prob <- ppois(q = q, lambda = lambda, lower.tail = F)
          manipulate::manipulate(plotcurve(q, lambda),
                                 q = manipulate::slider(q, q + 30, q),
                                 lambda = manipulate::slider(lambda, lambda + 200, lambda)
          )
        }
      }
    }
      if (dist == "beta") {
      if (!any(names(argaddit) == "alpha")) {
        alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
        argaddit$alpha <- as.numeric(alpha)
      }
      if (!any(names(argaddit) == "beta")) {
        beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
        argaddit$beta <- as.numeric(beta)
      }
      shape1 <- argaddit$alpha
      shape2 <- argaddit$beta
      if (lower.tail) {
        plotcurve <- function(q, shape1, shape2) {
          x <- seq(0, q[1], by = 0.01)
          y <- seq(q[1], 1, by = 0.01)
          fx <- dbeta(x, shape1, shape2)
          fy <- dbeta(y, shape1, shape2)
          curve(dbeta(x, shape1, shape2), 0,1 ,
                ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)),xlab = "X",panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Beta.", domain = "R-leem"))
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=argaddit$alpha, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pbeta(qq,  shape1, shape2, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`<`~q ~ ";" ~ alpha == alpha1 ~ ";" ~ beta == beta1) == Pr~"\n\n",
                                   list(q = qq, Pr = Pr, alpha1 = shape1, beta1= shape2)))
        }
        if (gui == "plot" ) {
          shape1 <- argaddit$alpha
          shape2 <- argaddit$beta
          prob <- pbeta(q ,shape1,  shape2)
          plotcurve(q, shape1 ,shape2)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(shape1 - 4 * shape2, shape1 + 4 * shape2, q),
                                 muaux = manipulate::slider(shape1, shape1 + 200, shape1))
        }
      }
      else{
        plotcurve <- function(q, shape1 , shape2 ) {
          x <- seq(q[1], 1, by=0.01)
          y <- seq(0, q[1], by=0.01)
          fx <- dbeta(x, shape1, shape2)
          fy <- dbeta(y, shape1, shape2)
          curve(dbeta(x, shape1, shape2), 1, 0,
                ylim = c(0, 1.2*max(fx,fy))  ,ylab = expression(f[X](x)), xlab="X",
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Beta.", domain = "R-leem"))
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=argaddit$alpha, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pbeta(qq, shape1, shape1, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`>`~q ~ ";" ~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                   list(q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
        }
        if (gui == "plot") {
          shape1 <- argaddit$alpha
          shape2 <- argaddit$beta
          prob <- pbeta(q, shape1, shape2)
          plotcurve(q, shape1, shape2)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(qaux, muaux),
                                 qaux = manipulate::slider(0, 1, q),
                                 muaux = manipulate::slider(shape1, shape1 + 200, shape1))
        }
      }
    }
      if (dist == "exp") {
      if (!any(names(argaddit) == "rate")) {
        rate <- readline(gettext("Insert the value of 'rate' argument: ",  domain = "R-leem"))
        argaddit$rate <- as.numeric(rate)
      }
      if (lower.tail) {
        plotcurve <- function(q, rate) {
          rmin <- 0
          rmax <- q + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          x <- rmin:rmax
          x1 <- seq(rmin, q, by = 0.01)
          x2 <- seq(q, rmax, by = 0.01)
          probx <- dexp(x, rate = rate)
          probx1 <- dexp(x1, rate = rate)
          probx2 <- dexp(x2, rate = rate)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(f[X](x)), xlab = "X")
          curve(dexp(x, rate), rmin, rmax,
                ylab = expression(p[x](q)),
                xlab = "x", ylim = ylim,
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Exponential.", domain = "R-leem"))
          polygon(c(x2, rev(x2)),
                  c(probx2, rep(0,length(probx2))),
                  col = "gray90")
          polygon(c(x1, rev(x1)),
                  c(probx1, rep(0,length(probx1))),
                  col = "red")
          abline(v = 1 / rate, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pexp(qq, rate = rate, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          rate2 <- gsub("\\.", ",", rate)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n;" ~ lambda == rat, list(q = qq, Pr = Pr, rat = rate2))
                 )
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate)
          plotcurve(q, rate)
        }
        if (gui == "rstudio") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate)
          manipulate::manipulate(plotcurve(q, rate),
                                 q = manipulate::slider(0, q + 30, q),
                                 rate = manipulate::slider(rate, rate + 200, rate)
          )
        }
      } else {
        plotcurve <- function(q, rate) {
          rmin <- 0
          rmax <- q + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          x <- rmin:rmax
          x1 <- seq(rmin, q, by = 0.01)
          x2 <- seq(q, rmax, by = 0.01)
          probx <- dexp(x, rate = rate)
          probx1 <- dexp(x1, rate = rate)
          probx2 <- dexp(x2, rate = rate)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X")
          curve(dexp(x, rate), rmin, rmax,
                ylab = expression(p[x](q)),
                xlab = "x", ylim = ylim,
                panel.first = grid(col = "gray90"),
                main = gettext("Probability Function: Exponential.", domain = "R-leem"))
          polygon(c(x2, rev(x2)),
                  c(probx2, rep(0,length(probx2))),
                  col = "red")
          polygon(c(x1, rev(x1)),
                  c(probx1, rep(0,length(probx1))),
                  col = "gray90")
          abline(v = rate, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pexp(qq, rate = rate, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = qqaux, labels = qqaux,
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X > q ~";"~~ lambda == rat) == Pr, list(q = qq, Pr = Pr, rat = rate))
          )
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate)
          plotcurve(q, rate = rate)
        }
        if (gui == "rstudio") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate)
          manipulate::manipulate(plotcurve(q, rate),
                                 q = manipulate::slider(q, q + 30, q),
                                 lambda = manipulate::slider(rate, rate + 200, rate)
          )
        }
      }
    }
      if (dist == "binomial") {
      if (!any(names(argaddit) == "size")) {
        size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
        argaddit$size <- as.numeric(size)
      }
      if (!any(names(argaddit) == "prob")) {
        prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
        argaddit$prob <- as.numeric(prob)
      }
      size <- argaddit$size
      sucesso <- argaddit$prob
      if (lower.tail) {
        plotcurve <- function(q, size, prob) {
          rmin <- 0
          x <- rmin:size
          x1 <- rmin:q
          x2 <- (q + 1):size
          probx <- dbinom(x, size = size, prob = prob)
          probx1 <- dbinom(x1, size = size, prob = prob)
          probx2 <- dbinom(x2, size = size, prob = prob)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",main="Probability Function: Binomial.")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          abline(v = size * prob, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pbinom(q, size = size, prob = prob, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(rmin, qqaux), labels = c(rmin, ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n" ~ size == n ~ p == prob, list(q = qq, Pr = Pr, n = size, prob = prob))
          )
        }
        if (gui == "plot") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          plotcurve(q, size, prob = sucesso)
        }
        if (gui == "rstudio") {
          prob <- pbinom(q = q, size = size, prob = sucesso)
          manipulate::manipulate(plotcurve(q, size, prob),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(size, size + 30, size),
                                 prob = manipulate::slider(sucesso, 1, sucesso)
          )
        }
      } else {
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        plotcurve <- function(q, size, prob) {
          rmin <- size * prob - 4 * sqrt(size * prob * (1 - prob))
          if (rmin < 0 || rmin>q) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q
          x2 <- (q + 1):size
          probx <- dbinom(x, size = size, prob = prob)
          probx1 <- dbinom(x1, size = size, prob = prob)
          probx2 <- dbinom(x2, size = size, prob = prob)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X",main="Probability Function: Binomial.")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, pch = 19, col = "red")
          abline(v = size * prob, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pbinom(q, size = size, prob = prob, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(qqaux, size), labels = c(qqaux, ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X > q) == Pr ~ "\n\n" ~ size == n ~ p == prob, list(q = qq, Pr = Pr, n = size, prob = prob))
          )
        }
        if (gui == "plot") {
          prob <- pbinom(q = q, size = size, prob = sucesso, lower.tail = FALSE)
          # Plot
          plotcurve(q, size, prob = sucesso)
        }
        if (gui == "rstudio") {
          prob <- pbinom(q = q, size = size, prob = sucesso, lower.tail = FALSE)
          manipulate::manipulate(plotcurve(q, size, prob),
                                 q = manipulate::slider(q, size, q),
                                 size = manipulate::slider(size, size + 30, size),
                                 prob = manipulate::slider(sucesso, 1, sucesso)
          )
        }
      }
    }
      if (dist == "hyper") {
      if (!any(names(argaddit) == "m")) {
        m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
        argaddit$m <- as.numeric(m)
      }
      if (!any(names(argaddit) == "n")) {
        n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
        argaddit$n <- as.numeric(n)
      }
      if (!any(names(argaddit) == "k")) {
        k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
        argaddit$k <- as.numeric(k)
      }
      size <- argaddit$m
      samples <- argaddit$n
      sucess <- argaddit$k
      if (lower.tail) {
        plotcurve <- function(q, size, samples, sucess) {
          rmin <- 0
          x <- rmin:size
          x1 <- rmin:q
          x2 <- (q + 1):size
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Probability Function: Hypergeometric.")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)),dhyper(x = x, m = size, n = samples, k = sucess))-1, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q, m = size, n = samples, k = sucess), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(rmin, qqaux), labels = c(rmin, ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess,
                                     list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
          )
        }
        if (gui == "plot") {
          prob <- phyper(q = q, m = size, n = samples, k = sucess)
          plotcurve(q, size, samples, sucess)
        }
      } else {
        plotcurve <- function(q, size, samples, sucess) {
          rmin <- 0
          if (rmin < 0 || rmin > q) rmin <- 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q
          x2 <- (q + 1):size
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Probability Function:Hypergeometric.")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, pch = 19, col = "red")
          abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)),dhyper(x = x, m = size, n = samples, k = sucess))-1, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q, m = size, n = samples, k = sucess, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(qqaux, size), labels = c(qqaux, ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X > q) == Pr ~ "\n\n" ~ m == size ~ n == samples ~ k == sucess, list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
          )
        }
        if (gui == "plot") {
          prob <- phyper(q = q, m = size, n = samples, k = sucess)
          plotcurve(q, size, samples, sucess)
        }
      }
    }
      if (dist == "nbinom") {
      if (!any(names(argaddit) == "prob")) {
        prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
        argaddit$prob <- as.numeric(prob)
      }
      if (argaddit$prob > 1 ) {
        stop("The 'prob' argument must be lower then zero!", call. = FALSE, domain = "R-leem")
      }
      if (!any(names(argaddit) == "size")) {
        size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
        argaddit$size <- as.numeric(size)
      }
      if (lower.tail) {
        xvq <- 2*q
        xvq1 <- -2*q
        if ( q >= 0) {
          xvq <- 2*q
          xvq1 <- -2*q
        }
        if ( q == 0 ) { xvq <- 10*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -10*(1 + q) }
        plotcurve <- function(q, s, p){
          x <- xvq1:xvq
          fx <- dnbinom(x,s,p)
          xlim <- c(xvq1, xvq)
          ylim <- c(min(fx), max(fx) + (max(fx)/2))
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Probability Function: Negative Binomial.", domain = "R-leem"))
          x1 <- xvq1:q
          fx1 <- dnbinom(x1,s,p)
          lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
          points(x1, fx1, lwd = 2, col = "red", pch = 19)
          x2 <- (q+1):xvq
          fx2 <- dnbinom(x2,s,p)
          lines(x2, fx2, type = "h", lwd = 2)
          points(x2, fx2, lwd = 2, pch = 19)
          abline(v = s, lty = 2)
          qq <- round(q, digits = rounding)
          qqaux <- round(q, digits = rounding)
          Pr <- round(pnbinom(qq,s,p,lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q) == Pr ~ "\n\n" ~ size == si ~ prob == po, list(q = qq, Pr = Pr, si = s, po = p))
          )
        }
        if (gui == "plot" ) {
          pro <- argaddit$prob
          size <- argaddit$size
          prob <- pnbinom(q = q , size = size , prob = pro ,lower.tail = T)
          plotcurve(q,size,pro)
        }
        if (gui == "rstudio") {
          xvq <- 2*q
          xvq1 <- -2*q
          if ( q >= 0) {
            xvq <- 2*q
            xvq1 <- -2*q
          }
          if ( q == 0 ) { xvq <- 10*( 1 + q) }
          if ( q == 0 ) { xvq1 <- -10*(1 + q) }
          pro <- argaddit$prob
          size <- argaddit$size
          manipulate::manipulate(plotcurve(q, pro, size),
                                 q = manipulate::slider(xvq1,xvq, q),
                                 pro = manipulate::slider(0.1,1, pro ),
                                 size = manipulate::slider(1,xvq, size))
          prob <- pnbinom(q,pro,size,lower.tail = TRUE)
        }
      }  else {
        xvq <- 2*q
        xvq1 <- -2*q
        if ( q >= 0) {
          xvq <- 2*q
          xvq1 <- -2*q
        }
        if ( q == 0 ) { xvq <- 10*( 1 + q) }
        if ( q == 0 ) { xvq1 <- -10*(1 + q) }
        plotcurve <- function(q, s, p){
          x <- xvq1:xvq
          fx <- dnbinom(x,s,p)
          xlim <- c(xvq1, xvq)
          ylim <- c(min(fx), max(fx) + (max(fx)/2))
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Probability Function: Negative Binomial.", domain = "R-leem"))
          x1 <- xvq1:q
          fx1 <- dnbinom(x1,s,p)
          lines(x1, fx1, type = "h", lwd = 2, )
          points(x1, fx1, lwd = 2, pch = 10)
          x2 <- (q):xvq
          fx2 <- dnbinom(x2,s,p)
          lines(x2, fx2, type = "h", lwd = 2,panel.first = grid(col = "gray90"),col = "red")
          points(x2, fx2, lwd = 2, pch = 10,col = "red")
          abline(v = s, lty = 2)
          qq <- round(q, digits = rounding)
          qqaux <- round(q, digits = rounding)
          Pr <- round(pnbinom(qq,s,p,lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(qqaux,xvq),labels = c(qqaux,""),col = "red", font = 2, col.axis = "red")
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X > q) == Pr ~ "\n\n" ~ size == si ~ prob == po, list(q = qq, Pr = Pr, si = s, po = p))
          )
        }
        if (gui == "plot" ) {
          pro <- argaddit$prob
          size <- argaddit$size
          prob <- pnbinom(q = q , size = size , prob = pro ,lower.tail = F)
          plotcurve(q,size,pro)
        }
        if (gui == "rstudio") {
          xvq <- 2*q
          xvq1 <- -2*q
          if ( q >= 0) {
            xvq <- 2*q
            xvq1 <- -2*q
          }
          if ( q == 0 ) { xvq <- 10*( 1 + q) }
          if ( q == 0 ) { xvq1 <- -10*(1 + q) }
          pro <- argaddit$prob
          size <- argaddit$size
          manipulate::manipulate(plotcurve(q, pro, size),
                                 q = manipulate::slider(xvq1,xvq, q),
                                 pro = manipulate::slider(0.1,1, pro ),
                                 size = manipulate::slider(1,xvq, size))
          prob <- pnbinom(q,pro,size,lower.tail = F)
        }
      }
    }
      if (dist == "geometric") {
      if (!any(names(argaddit) == "probability")) {
        probability <- readline(gettext("Insert the value of 'probability' argument: ", domain = "R-leem"))
        argaddit$probability <- as.numeric(probability)
      }
      if (lower.tail) {
        plotcurve <- function(q, probability) {
          rmin <- -5*q
          rmax <- 5*q
          if (rmin < 0) rmin <- 0
          x <- rmin:rmax
          x1 <- rmin:q
          x2 <- (q + 1):rmax
          probx <-  dgeom(x, p = probability)
          probx1 <- dgeom(x1, p = probability)
          probx2 <- dgeom(x2, p = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X",main = gettext("Probability Function: Geometric.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(qq, p = probability, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(rmin,qqaux),labels=c(rmin,qqaux),col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q ~ " ;" ~ p == probability) == Pr ~ "\n\n" , list(q = qq, Pr = Pr, probability = probability))
          )
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, p = probability)
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, p= probability)
          manipulate::manipulate(plotcurve(q, probability),
                                 q = manipulate::slider(0, q + 30, q),
                                 p = manipulate::slider(probability, probability + 200, probability)
          )
        }
      } else {
        plotcurve <- function(q, probability) {
          rmin <- -5*q
          rmax <-  5*q
          if (rmin < 0) rmin <- 0
          x <- rmin:rmax
          x1 <- rmin:q
          x2 <- q:rmax
          probx <- dgeom(x, p = probability)
          probx1 <- dgeom(x1, p = probability)
          probx2 <- dgeom(x2, p = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = gettext("Probability Function: Geometric.", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, col = "red", pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(qq, p = probability, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(qqaux,rmax) ,labels = c(qqaux, ""), col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X <= q ~ " ;" ~ p == probability) == Pr ~ "\n\n" , list(q = qq, Pr = Pr, probability = probability))
          )
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, p = probability)
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, p = probability)
          manipulate::manipulate(plotcurve(q, lambda),
                                 q = manipulate::slider(q, q + 30, q),
                                 p = manipulate::slider(probability, probability + 200, probability)
          )
        }
      }
    }
   }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}


