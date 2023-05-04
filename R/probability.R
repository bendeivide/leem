#' Cumulative distribution function
#'
#' \code{P} Compute the cumulative distribution function for multiple distributions
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
#' @param dist distribution to use. The default is \code{'normal'}. Options: \code{'normal'}, \code{'t-student'}, \code{'gumbel'}, \code{'binomial'}, \code{'poisson'}, and ....
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}. This argument is valid only if \code{q} has length 1.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param porcentage logical; if \code{FALSE} (default), the result in decimal. Otherwise, probability is given in percentage.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{'none'}, \code{'rstudio'} or \code{'tcltk'}.
#' @param ... additional arguments according to the chosen distribution.
#'
#' @return \code{P} returns the probability and its graphical representation. The result can be given as a percentage or not.
#'
#' @examples
#' # Loading package
#' library(leem)
#' # Example 1 - t-Student distribution
#' \dontrun{
#' P(q = 2, dist = "t-student", df = 10)
#' P(q = 2, dist = "t-student", df = 10, gui = 'rstudio')
#' P(q = 2, dist = "t-student", df = 10, gui = 'tcltk')
#' P(-1 %<X<% 1, dist = "t-student", df = 10)
#' }
#' # Example 2 - Normal distribution
#' P(-2,  dist = "normal", mean = 3, sd = 2, main = expression(f(x) == (1 / sqrt(n * sigma^2)) * exp(-1/2 * (x - mu)^2/sigma^2)))
#' @import manipulate
#' @import tkRplotR
#  @import shiny
#' @export
P <- function(q, dist = "normal", lower.tail = TRUE,
              rounding = 5, porcentage = FALSE, gui = "plot", main = NULL, ...) {
  # defensive programming
  ## dist
  ## gui
  ## q

  # Arguments in '...'
  argaddit <- list(...)
  # Formal arguments
  argdef <- formals(P)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regiona)) {
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

        mu <- argaddit$mean
        sigma <- argaddit$sd

        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd

        if (gui == "plot") {
          plotpnormalarplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpnormalarrstudio(q1, q2, mean, sd, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))
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
          # on.exit(options(war))

          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotcurveaux <- function(q1 = q[1], q2 = q[2], mu = mu,  sigma = sigma, ...) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, mu, sigma)
          }
          tk_q1 <- leemset("tk_q1", tclVar(q[1]))
          tk_q2 <- leemset("tk_q2", tclVar(q[2]))
          tk_mean <- leemset("tk_mean", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_q1", "tk_q2", "tk_mean", "tk_sigma"),
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
                     gettext("leem package: Normal Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))
          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q1 <- as.numeric(tclvalue(tk_q1))
                                        q2 <- as.numeric(tclvalue(tk_q2))
                                        mu <- as.numeric(tclvalue(tk_mean))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotpnormalartcltk(q1 = q1, q2 = q2, mu = mu, sigma = sigma, rounding, main, q)
                                      })
          s02 <- tcltk::tkscale(
            tkplot,
            from = q[2],
            to = maximo,
            label = 'q2',
            variable = tk_q2,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s01 <- tcltk::tkscale(
            tkplot,
            from = minimo,
            to = q[2],
            label = 'q1',
            variable = tk_q1,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tkscale(
            tkplot,
            from = mu,
            to = mu + 2 * sigma,
            label = 'mean',
            variable = tk_mean,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s04 <- tkscale(
            tkplot,
            from = sigma,
            to = 1.8 * sigma,
            label = 'standard deviation',
            variable = tk_sigma,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          tkpack(s01, s02, s03, s04,
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
          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
          # prob <- round(pt(q[2], df = nu,
          #                  lower.tail = T) + pt(q[1], df = nu, lower.tail = F), digits=rounding)
        }
        # Calculates the desired probability
        prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) +
          pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)
      }
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }

        #Auxiliary Arguments
        llower <- if(abs(q[1]) > 6) abs(q[1] + 2) else 6
        lupper <- if(abs(q[2]) > 6) abs(q[2] + 2) else 6


        if (gui == "plot" ) {
          nu <- argaddit$df
          plotptstudentarplot(q, nu, rounding, main)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          manipulate::manipulate(plotptstudentarrstudio(q1, q2, df, rounding, main, q),
                                 q1 = manipulate::slider(-llower, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], lupper, q[2]),
                                 df = manipulate::slider(1, nu + 100, nu))
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
                     gettext("leem package: T Distribution", domain = "R-leem"))

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
            to = lupper,
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
            from = -llower,
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
            to = nu + 100,
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
          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Calculates the desired probability
        prob <- pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F)
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        while (argaddit$lambda <= 0) {
          lambda <- readline(gettext("Please, Insert the value of 'lambda' greater then 0: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }

        lambda <- argaddit$lambda
        rmax <- ceiling(q[2] + 4 * sqrt(lambda))
        rmin <- ceiling(lambda - 4 * sqrt(lambda))
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)

        if (gui == "plot") {
          plotppoissonarplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonarrstudio(q1, q2, lambda, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 lambda = manipulate::slider(1, lambda+30, lambda))
        }
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- (ppois(q = q[1], lambda = lambda, lower.tail = T)) +
          (ppois(q = q[2] - 1,lambda = lambda, lower.tail = F))
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
        prob <- argaddit$prob

        if (gui == "plot") {
          plotpbinomialarplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialarrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], q[2]+size, q[2]),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        prob <- round(pbinom(q[1], size = size, prob = prob, lower.tail = T)
                      + pbinom(q[2], size = size, prob = prob, lower.tail = F), digits = rounding)
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
                main = gettext("Distribution Function: Gumbel", domain = "R-leem"))
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
          axis(side=1, at=qq, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(xvq, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], xvq1)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft", bty ="n", fill ="red",
                   legend =substitute(P(t1>~G>~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                      list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1>=~G>=~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill = "red",
                   legend = substitute(P(t1>=~G>~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                       list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region6") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1>~G >=~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
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
                panel.first = grid(col="gray90"), main = gettext("Distribution Function: Beta", domain = "R-leem"))
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
                   legend = substitute(P(t1>~X>~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1>=~X>=~ t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1>=~X>~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                     list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if ( attr(q, "region") == "region6") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1>~X>=~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
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
          rate <- readline(gettext("Insert the value of 'rate' argument: ",
                                   domain = "R-leem"))
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
                xlab = "x", ylim = c(0, 1.2 * max(c(c(probx1,probx2), proby))),
                panel.first = grid(col = "gray90"),
                main = gettext("Distribution Function: Exponential", domain = "R-leem"))
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
          Pr <- round(pexp(qq[1], rate = rate, lower.tail = T) +
                        pexp(qq[2], rate = rate, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          rate2 <- gsub("\\.", ",", rate)
          axis(
            side = 1, at = c(qqaux[1],0), labels = c(qqaux[1],""),
            col = "red", font = 2, col.axis = "red"
          )
          axis(
            side = 1, at = c(qqaux[2],rmax+1), labels = c(qqaux[2],""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>~X>~ t2~ ";" ~ lambda == rat) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>=~ t2~ ";" ~ lambda == rat) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>~ t2~ ";" ~ lambda == rat) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>~X>=~ t2~ ";" ~ lambda == rat) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- round(pexp(q[1], rate = rate, lower.tail = T) +
                          pexp(q[2], rate = rate, lower.tail = F), rounding)
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
          prob <- round(pexp(q[1], rate = rate, lower.tail = T) +
                          pexp(q[2], rate = rate, lower.tail = F), rounding)
          manipulate::manipulate(plotcurveaux(q1,q2, rate),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 lambda = manipulate::slider(rate, rate + 200, rate))
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
          title(ylab = expression(p[X](x)), xlab = "X", main = "Distribution Function: Hypergeometric")
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
          Pr <- round(phyper(q[1], m = size, n = samples, k = sucess) - phyper(q[2], m = size, n = samples, k = sucess, lower.tail = FALSE), rounding) * -1
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
                   legend = substitute(P(t1>~X>~t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n" ,
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1>=~X>=~t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n",
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1>=~X>~ t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n", list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1>~X>=~t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n", list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
        }
        if (gui == "plot") {
          prob <- round(phyper(q[1], m = size, n = samples, k = sucess) - phyper(q[2], m = size, n = samples, k = sucess, lower.tail = FALSE), digits = rounding) * -1
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
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Distribution Function: Negative Binomial", domain = "R-leem"))
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
                     P(t1>~X>~t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>=~t2 )== Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>~t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>~X>=~t2) == Pr ~ "\n\n" ~ size == si ~ prob == pro,
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
      }##########INCORRECT
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
          probx <- dgeom(x, prob = probability)
          probx1 <- dgeom(x1, prob = probability)
          probx2 <- dgeom(x2, prob = probability)
          probx3 <- dgeom(x3, prob = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X", main = gettext("Distribution Function: Geometric", domain = "R-leem"))
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2,col="red")
          points(x1, probx1, lwd = 2, pch = 19,col="red")
          lines(x2, probx2, type = "h", lwd = 2,col="red")
          points(x2, probx2, lwd = 2, pch = 19,col="red")
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x3, probx3, lwd = 2, pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(q = q[1], prob = probability) - pgeom(q = q[2], prob = probability, lower.tail = F),digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qq, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(rmin, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 1, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], rmax)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>~X>~t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region3") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>=~t2 ~ " ;" ~ p == probability )== Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region5") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>=~X>~t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region6") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1>~X>=~t2 ~ " ;" ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- round(pgeom(q = q[1], prob = probability) - pgeom(q = q[2], prob = probability, lower.tail = F),digits = rounding)
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
          prob <- round(pgeom(q = q[1], prob = probability) - pgeom(q = q[2], prob = probability, lower.tail = F),digits = rounding)
          manipulate::manipulate(plotcurveaux(q1,q2, probability),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 p = manipulate::slider(probability, probability + 200, probability))
        }
      }
    }
    if (any(attr(q, "region") == regionb)) {
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
        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd

        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotpnormalbrplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotpnormalbrrstudio(q1, q2, mean, sd, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))
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
          # on.exit(options(war))

          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotcurveaux <- function(q1 = q[1], q2 = q[2], mu = mu,  sigma = sigma, ...) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, mu, sigma)
          }
          tk_q1 <- leemset("tk_q1", tclVar(q[1]))
          tk_q2 <- leemset("tk_q2", tclVar(q[2]))
          tk_mean <- leemset("tk_mean", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_q1", "tk_q2", "tk_mean", "tk_sigma"),
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
                     gettext("leem package: Normal Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))
          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q1 <- as.numeric(tclvalue(tk_q1))
                                        q2 <- as.numeric(tclvalue(tk_q2))
                                        mu <- as.numeric(tclvalue(tk_mean))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotpnormalbrtcltk(q1 = q1, q2 = q2, mu = mu, sigma = sigma, rounding, main, q)
                                      })
          s02 <- tcltk::tkscale(
            tkplot,
            from = q[2],
            to = maximo,
            label = 'q2',
            variable = tk_q2,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s01 <- tcltk::tkscale(
            tkplot,
            from = minimo,
            to = q[2],
            label = 'q1',
            variable = tk_q1,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tkscale(
            tkplot,
            from = mu,
            to = mu + 2 * sigma,
            label = 'mean',
            variable = tk_mean,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s04 <- tkscale(
            tkplot,
            from = sigma,
            to = 1.8 * sigma,
            label = 'standard deviation',
            variable = tk_sigma,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          tkpack(s01, s02, s03, s04,
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
          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
          # prob <- round(pt(q[2], df = nu,
          #                  lower.tail = T) + pt(q[1], df = nu, lower.tail = F), digits=rounding)
        }
        # Compute the desired probability
        prob <- pnorm(q = q[2], mean = mu, sd=sigma) - pnorm(q = q[1], mean = mu, sd=sigma)
      }
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }
        # Auxiliar objects
        nu <- argaddit$df
        llower <- if(abs(q[1]) > 6) abs(q[1] + 2) else 6
        lupper <- if(abs(q[2]) > 6) abs(q[2] + 2) else 6
        # Function
        if (gui == "plot" ) {
          nu <- argaddit$df
          plotptstudentbrplot(q, nu, rounding, main)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          manipulate::manipulate(plotptstudentbrrstudio(q1, q2, df, rounding, main, q),
                                 q1 = manipulate::slider(-llower, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], lupper, q[2]),
                                 df = manipulate::slider(1, nu + 100, nu))
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
                     gettext("leem package: T Distribution", domain = "R-leem"))

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
            to = lupper,
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
            from = -llower,
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
            to = nu + 100,
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
          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
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
        # Compute the desired probability
        prob <- pt(q[2], df = nu) - pt(q[1], df = nu)
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        while (argaddit$lambda <= 0) {
          lambda <- readline(gettext("Please, Insert the value of 'lambda' greater then 0: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }

        lambda <- argaddit$lambda
        rmax <- ceiling(q[2] + 4 * sqrt(lambda))
        rmin <- ceiling(lambda - 4 * sqrt(lambda))
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)

        if (gui == "plot") {
          plotppoissonbrplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonbrrstudio(q1, q2, lambda, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 lambda = manipulate::slider(1, lambda+30, lambda))
        }
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(ppois(q = q[2], lambda = lambda) - ppois(q = q[1],lambda = lambda), digits = rounding)
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
        prob <- argaddit$prob

        if (gui == "plot") {
          plotpbinomialbrplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialbrrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], q[2]+size, q[2]),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        prob <- round(pbinom(q[2], size, prob, lower.tail = T) - pbinom(q[1], size, prob, lower.tail = T), rounding)
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
                main = gettext("Distribution Function: Gumbel", domain = "R-leem"))
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
          #Pr <- gsub("\\.", ",", Pr)
          #qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1<~G<~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region4") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1<=~ G<=~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region7") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1<=~G<~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
                                     list(t1=qq[1],t2=qq[2],scaux=scaux,locaux=locaux, Pr = Pr)))
          }
          if (attr(q, "region") == "region8") {
            legend("topleft", bty="n", fill = "red",
                   legend=substitute(P(t1<~G<=~t2 ~";"~ scale==scaux ~";"~ location==locaux)==Pr~"\n\n",
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
                panel.first = grid(col="gray90"), main = expression("Distribution Function: Beta", domain = "R-leem"))
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
                   legend = substitute(P(t1<~X<~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region4") {
            legend("topleft", bty="n", fill="red",
                   legend = substitute(P(t1<=~X<=~t2~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                       list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if (attr(q, "region") == "region7") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1<=~X<~t2 ~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                     list(t1 = qq[1], t2 = qq[2], q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
          }
          if ( attr(q, "region") == "region8") {
            legend( "topleft", bty="n", fill="red",
                    legend = substitute(P(t1<~X<=~t2~";"~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
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
          rate <- readline(gettext("Insert the value of 'rate' argument: ",
                                   domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        plotcurve <- function(q, rate) {
          rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          x <- seq(q[1], q[2], by = 0.01)
          y <- seq(0, rmax, by = 0.01)
          probx <- dexp(x, rate = rate)
          proby <- dexp(y, rate = rate)

          # Curve
          curve(dexp(x, rate), 0, rmax,
                ylab = expression(p[x](q)),
                xlab = "x", ylim = c(0, 1.2 * max(c(probx, proby))),
                panel.first = grid(col = "gray90"),
                main = gettext("Distribution Function: Exponential", domain = "R-leem"))


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
                     P(t1<~X<~ t2 ~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2], q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<=~t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<~ t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<~X<=~t2~ ";"~lambda == rat) == Pr,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, rat = rate)
                   )
            )
          }
        }

        if (gui == "plot") {
          # Probability
          rate <- argaddit$rate
          prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate = rate))*-1
          # Plot
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
          title(ylab = expression(p[X](x)), xlab = "X", main = "Distribution Function: Hypergeometric")
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
                   legend = substitute(P(t1<~X<~t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n",
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1<=~X<=~ t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n",
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1<=~X<~ t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n",
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(t1<~X<=~t2 ~ ";" ~ m == size ~ ";" ~ n == samples ~";"~ k == sucess) == Pr ~ "\n\n",
                                       list(t1 = qq[1], t2 = qq[2], Pr = Pr, size = size, samples = samples, sucess = sucess))
            )
          }
        }
        if (gui == "plot") {
          prob <- round(phyper(q[1], m = size, n = samples, k = sucess) - phyper(q[2], m = size, n = samples, k = sucess), rounding) * -1
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
          title(ylab = expression(f[X](x)), xlab = "X", main = gettext("Distribution Function: Negative Binomial", domain = "R-leem"))
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
                     P(t1<~X<~ t2~ ";" ~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<=~ t2~ ";" ~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<~t2~ ";" ~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n",
                     list(t1=qq[1],t2=qq[2],si = s, Pr = Pr, pro = p)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<~X<=~t2~ ";" ~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n",
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
      }###########INCORREC
      if (dist == "geometric") {
        if (!any(names(argaddit) == "probability")) {
          probability <- readline(gettext("Insert the value of 'probability' argument: ", domain = "R-leem"))
          argaddit$probability <- as.numeric(probability)
        }
        plotcurve <- function(q, probability) {
          rmin <- -5*q[1]
          rmax <-  5*q[2]
          if (rmin < 0) rmin <- 0
          x <- rmin:rmax
          x1 <- rmin:q[1]
          x2 <- q[2]:rmax
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
          probx <- dgeom(x, prob = probability)
          probx1 <- dgeom(x1, prob = probability)
          probx2 <- dgeom(x2, prob = probability)
          probx3 <- dgeom(x3, prob = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X", main = gettext("Distribution Function: Geometric", domain = "R-leem"))
          lines(x1, probx1, type = "h", lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x3, probx3, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x3, probx3, lwd = 2, col = "red", pch = 19)
          abline(v = q, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(q = q[1], prob = probability) - pgeom(q = q[2], prob = probability),digits = rounding)*- 1
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          abline(v = qqaux, lty = 2, col = "red")
          if (attr(q, "region") == "region2") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<~X<~t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region4") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<=~t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region7") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<=~X<~t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
          if (attr(q, "region") == "region8") {
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(
                     P(t1<~X<=~t2 ~ "; " ~ p == probability) == Pr ~ "\n\n" ,
                     list(t1=qq[1],t2=qq[2],q = qq, Pr = Pr, probability = probability)
                   )
            )
          }
        }

        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- (pgeom(q[1], prob = probability) - pgeom(q[2], prob = probability))*-1
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          plotcurveaux <- function(q1 = q[1], q2 = q[2],p = probability) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q,probability)
          }
          prob <- (pgeom(q = q[1], prob = probability) - pgeom(q = q[2], prob = probability))*-1
          manipulate::manipulate(plotcurveaux(q1,q2, p = probability),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 260, q[2]),
                                 probability = manipulate::slider(p, p + 200, p))
        }
      }
    }
  }
  else {
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
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        if (gui == "plot" ) {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotpnormallttplot(q, mu,sigma, rounding, main)
        }
        if (gui == "rstudio") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotpnormallttplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

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

          mu <- argaddit$mean
          sigma <- argaddit$sd
          tk_q <- leemset("tk_q", tclVar(q))
          tk_mu <- leemset("tk_mu", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_q", "tk_mu", "tk_sigma"),
                 function(x) globalvariables(x, leemget(x)))


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
                     gettext("leem package: Normal Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q <- as.numeric(tclvalue(tk_q))
                                        mu <- as.numeric(tclvalue(tk_mu))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotpnormallttplot(q = q, mu = mu, sigma = sigma, rounding, main)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = minimo,
            to = maximo,
            label = 'q',
            variable = tk_q,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s02 <- tcltk::tkscale(
            tkplot,
            from = mu,
            to = mu + 2 * sigma,
            label = 'mean',
            variable = tk_mu,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tcltk::tkscale(
            tkplot,
            from = sigma,
            to = sigma * 1.8,
            label = 'standard deviation',
            variable = tk_sigma,
            showvalue = TRUE,
            resolution = 0.01,
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
                rm("tk_q1", "tk_df", envir = .GlobalEnv)
              }
              tkdestroy(tkplot)
            }
            # Desabilitar warnings global
            #options(warn = - 1)
            #war <- options(warn = - 1)
            on.exit(options(war))
          })
        }
        # Compute the desired probability
        prob <- pnorm(q = q, mean = mu, sd = sigma)

      }
      else {
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        # Plot function

        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotpnormalltfplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotpnormalltfplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))

        }

        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

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

          mu <- argaddit$mean
          sigma <- argaddit$sd
          # plotcurveaux <- function(q1 = q[1], q2 = q[2], nu = nu, ...) {
          #   q[1] <- q1
          #   q[2] <- q2
          #   plotcurve(q, nu)
          # }
          tk_q <- leemset("tk_q", tclVar(q))
          tk_mu <- leemset("tk_mu", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_q", "tk_mu", "tk_sigma"),
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
                     gettext("leem package: Normal Distribution", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        q <- as.numeric(tclvalue(tk_q))
                                        mu <- as.numeric(tclvalue(tk_mu))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotpnormallftplot(q = q, mu = mu, sigma = sigma, rounding, main)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = minimo,
            to = maximo,
            label = 'q',
            variable = tk_q,
            showvalue = TRUE,
            resolution = 0.01,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s02 <- tcltk::tkscale(
            tkplot,
            from = mu,
            to = mu + 2 * sigma,
            label = 'mean',
            variable = tk_mu,
            showvalue = TRUE,
            resolution = 1,
            repeatdelay = 200,
            repeatinterval = 100,
            orient = "hor"
          )
          s03 <- tcltk::tkscale(
            tkplot,
            from = sigma,
            to = sigma * 1.8,
            label = 'standard deviation',
            variable = tk_sigma,
            showvalue = TRUE,
            resolution = 0.01,
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
                rm("tk_q1", "tk_df", envir = .GlobalEnv)
              }
              tkdestroy(tkplot)
            }
            # Desabilitar warnings global
            #options(warn = - 1)
            #war <- options(warn = - 1)
            on.exit(options(war))
          })


          prob <- pnorm(q = q, mean = mu, sd = sigma)

        }
        # Compute the desired probability
        prob <- pnorm(q = q, mean = mu, sd=sigma, lower.tail = F)
      }
    }
    if (dist == "t-student") {
      if (!any(names(argaddit) == "df")) {
        df <- readline(gettext("Insert the value of degree of freedom (df): ", domain = "R-leem"))
        argaddit$df <- as.numeric(df)
      }
      lim <- if(abs(q) > 6) abs(q + 2) else 6
      if (lower.tail) {
        if (gui == "plot" ) {
          nu <- argaddit$df
          plotptstudentlttplot(q, nu, rounding, main)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          manipulate::manipulate(plotptstudentlttplot(q, nu, rounding, main),
                                 q = manipulate::slider(-lim, lim, q),
                                 nu = manipulate::slider(1, nu+100, nu))
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
                     gettext("leem package: T Distribution", domain = "R-leem"))

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
            # Desabilitar warnings global
            #options(warn = - 1)
            #war <- options(warn = - 1)
            on.exit(options(war))
          })
        }
        # Calculates the desired probability
        prob <- pt(q = q, df = nu)
      } else {
        if (gui == "plot") {
          nu <- argaddit$df
          plotptstudentltfplot(q, nu, rounding, main)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          manipulate::manipulate(plotptstudentltfplot(q, nu, rounding, main),
                                 q = manipulate::slider(-lim, lim, q),
                                 nu = manipulate::slider(1, nu+100, nu))
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
            # options(warn = - 1)
            # war <- options(warn = - 1)
            on.exit(options(war))
          })
        }
        # Calculates the desired probability
        prob <- pt(q = q, df = nu, lower.tail = FALSE)

      }
    }
    if (dist == "poisson") {
      if (!any(names(argaddit) == "lambda")) {
        lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
        argaddit$lambda <- as.numeric(lambda)
      }
      lambda <- argaddit$lambda
      rmin <- ceiling(lambda - 4 * sqrt(lambda))
      if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
      rmax <- ceiling(lambda + 4 * sqrt(lambda))
      if (lower.tail) {
        if (gui == "plot") {
          plotppoissonlttplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonlttplot(q, lambda, rounding, main),
                                 q = manipulate::slider(0, lambda+30, q),
                                 lambda = manipulate::slider(1, lambda + 30, lambda)
          )
        }
        prob <- ppois(q = q, lambda = lambda)
      }
      else {
        if (gui == "plot") {
          plotppoissonltfplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonltfplot(q, lambda, rounding, main),
                                 q = manipulate::slider(0, lambda+30, q),
                                 lambda = manipulate::slider(1, lambda + 30, lambda)
          )
        }
        prob <- ppois(q = q, lambda = lambda, lower.tail = FALSE)
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
      prob <- argaddit$prob

      if (lower.tail) {
        if (gui == "plot") {
          plotpbinomiallttplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomiallttplot(q, size, prob, rounding, main),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        prob <- pbinom(q, size, prob)
      } else {
        if (gui == "plot") {
          plotpbinomialltfplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialltfplot(q, size, prob, rounding, main),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        prob <- pbinom(q, size, prob, lower.tail = FALSE)
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
                main = gettext("Distribution Function: Gumbel", domain = "R-leem"))
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
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(xvq1, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G<= ~ q ~ ";" ~ scale==scaux ~ ";" ~ location ==locaux)==Pr~"\n\n",
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
                main = gettext("Distribution Function: Gumbel", domain = "R-leem"))
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
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(xvq, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G> ~ q ~ ";" ~ scale==scaux ~ ";" ~ location ==locaux)==Pr~"\n\n",
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
    if (dist == "beta") {
      if (q > 1) stop("The 'q' argument must be lower then 1.", call. = FALSE, domain = "R-leem")
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
                main = gettext("Distribution Function: Beta", domain = "R-leem"))
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=argaddit$alpha, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pbeta(qq,  shape1, shape2), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2, col.axis = "red")
          axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X<= ~ q ~ ";" ~ alpha == alpha1 ~ ";" ~ beta == beta1) == Pr~"\n\n",
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
                main = gettext("Distribution Function: Beta.", domain = "R-leem"))
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
          axis(side = 1, at = as.character(c(qqaux, 1)), labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X>~q ~ ";" ~ alpha == alpha1 ~ ";" ~ beta == beta1 )==Pr~"\n\n",
                                   list(q=qq, Pr=Pr, alpha1 = shape1, beta1 = shape2)))
        }
        if (gui == "plot") {
          shape1 <- argaddit$alpha
          shape2 <- argaddit$beta
          prob <- pbeta(q, shape1, shape2, lower.tail = F)
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
        rate <- readline("Insert the value of 'rate' argument: ")
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
                main = gettext("Distribution Function: Exponential.", domain = "R-leem"))
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
                 legend = substitute(P(X<= ~ q ~";"~~ lambda == rat) == Pr, list(q = qq, Pr = Pr, rat = rate))
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
          rmax <- (q + ceiling(1 / rate + 7 * sqrt(1 / rate^2)))
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
                main = gettext("Distribution Function: Exponential.", domain = "R-leem"))
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
                 legend = substitute(P(X> ~ q ~";"~~ lambda == rat) == Pr, list(q = qq, Pr = Pr, rat = rate))
          )
        }
        if (gui == "plot") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate, lower.tail = F)
          plotcurve(q, rate = rate)
        }
        if (gui == "rstudio") {
          rate <- argaddit$rate
          prob <- pexp(q = q, rate = rate, lower.tail = F)
          manipulate::manipulate(plotcurve(q, rate),
                                 q = manipulate::slider(q, q + 30, q),
                                 lambda = manipulate::slider(rate, rate + 200, rate)
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
          x2 <- q:size
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Distribution Function: Hypergeometric")
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          abline(v = samples, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q, m = size, n = samples, k = sucess), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(qqaux, rmin), labels = c(qqaux, ""),
            col = "red", font = 2, col.axis = "red"
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X<= ~ q ~ ";" ~ m == size ~ ";" ~  n == samples ~ ";" ~ k == sucess) == Pr ~ "\n\n",
                                     list(q = q, t1 = qq[1], t2 = qq[2],
                                          Pr = Pr, size = size, samples = samples, sucess = sucess))
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
          x2 <- q+1:size
          probx <- dhyper(x, m = size, n = samples, k = sucess)
          probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
          probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
          xlim <- c(rmin, size)
          ylim <- c(min(probx), max(probx) + 0.1)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = "Distribution Function:Hypergeometric")
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, pch = 19, col = "red")
          abline(v = samples, lty = 2)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(phyper(q, m = size, n = samples, k = sucess, lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(
            side = 1, at = c(qqaux, size),
            col = "red", col.axis = "red",labels =
          )
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X> ~ q ~ ";" ~ m == size ~ ";" ~ n == samples ~ ";" ~ k == sucess) == Pr ~ "\n\n" ,
                                     list(t1 = qq[1], t2 = qq[2],
                                          Pr = Pr, size = size, samples = samples, sucess = sucess, q = q))
          )
        }
        if (gui == "plot") {
          prob <- phyper(q = q, m = size, n = samples, k = sucess, lower.tail = F)
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

        # Desabilitar warnings global
        #options(warn = - 1)
        war <- options(warn = - 1)
        on.exit(options(war))
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
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Distribution Function: Negative Binomial", domain = "R-leem"))
          x1 <- q:xvq1
          fx1 <- dnbinom(x1,s,p)
          x2 <- q:xvq
          fx2 <- dnbinom(x2,s,p)
          lines(x2, fx2, type = "h", lwd = 2)
          points(x2, fx2, lwd = 2, pch = 19)
          lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
          points(x1, fx1, lwd = 2, col = "red", pch = 19)
          abline(v = s, lty = 2)
          qq <- round(q, digits = rounding)
          qqaux <- round(q, digits = rounding)
          Pr <- round(pnbinom(qq,s,p,lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(xvq1, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X<=~ q~ ";" ~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n", list(q = qq, Pr = Pr, si = s, po = p))
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
          title(ylab = expression(f[X](x)), xlab = "X",main = gettext("Distribution Function: Negative Binomial", domain = "R-leem"))
          x1 <- xvq1:q
          fx1 <- dnbinom(x1,s,p)
          lines(x1, fx1, type = "h", lwd = 2, )
          points(x1, fx1, lwd = 2, pch = 19)
          if (q < 0){
            x2 <- (q - 1):xvq
          }
          if (q > 0){
            x2 <- (q+1):xvq
          }
          fx2 <- dnbinom(x2,s,p)
          lines(x2, fx2, type = "h", lwd = 2,panel.first = grid(col = "gray90"),col = "red")
          points(x2, fx2, lwd = 2, pch = 19,col = "red")
          abline(v = s, lty = 2)
          qq <- round(q, digits = rounding)
          qqaux <- round(q, digits = rounding)
          Pr <- round(pnbinom(qq,s,p,lower.tail = F), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          if (q > 0){
            axis(side = 1, at = c(qqaux,xvq),col = "red", lwd.ticks = 0)
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(X>~ q~ ";"~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n", list(q = qq, Pr = Pr, si = s, po = p))
            )
          }
          if (q < 0){
            axis(side = 1, at = c(qqaux,xvq),col = "red", lwd.ticks = 0)
            legend("topleft",
                   bty = "n", fill = "red",
                   legend = substitute(P(X<~ q~ ";"~ size == si ~ ";" ~ prob == po) == Pr ~ "\n\n", list(q = qq, Pr = Pr, si = s, po = p))
            )
          }
          axis(side = 1, at = qqaux,col = "red", font = 2, col.axis = "red")
          abline(v = qqaux, lty = 2, col = "red")
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
    }#############INCORRECT
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
          x2 <- q:rmax
          probx <-  dgeom(x, prob = probability)
          probx1 <- dgeom(x1, prob = probability)
          probx2 <- dgeom(x2, prob = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) + 0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(P[X](x)), xlab = "X",main = gettext("Distribution Function: Geometric", domain = "R-leem"))
          lines(x2, probx2, type = "h", lwd = 2)
          points(x2, probx2, lwd = 2, pch = 19)
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
          points(x1, probx1, lwd = 2, col = "red", pch = 19)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(qq, prob = probability, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1, col.axis = "red")
          axis(side=1, at=as.character(c(0, qqaux)), tick = TRUE, lwd = 1,
               col="red", font = 2, labels = FALSE)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X<= ~ q ~ " ;" ~ prob == probability) == Pr ~ "\n\n" , list(q = qq, Pr = Pr, probability = probability))
          )
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, prob = probability)
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, prob = probability)
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
          probx <- dgeom(x, prob = probability)
          probx1 <- dgeom(x1, prob = probability)
          probx2 <- dgeom(x2, prob = probability)
          xlim <- c(rmin, rmax)
          ylim <- c(min(probx), max(probx) +0.2)
          plot.new()
          plot.window(xlim, ylim)
          axis(1)
          axis(2)
          title(ylab = expression(p[X](x)), xlab = "X", main = gettext("Distribution Function: Geometric", domain = "R-leem"))
          lines(x2, probx2, type = "h", lwd = 2, col = "red")
          points(x2, probx2, lwd = 2, col = "red", pch = 19)
          lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
          points(x1, probx1, lwd = 2, pch = 19)
          qq <- round(q, digits = 2)
          qqaux <- round(q, digits = 2)
          Pr <- round(pgeom(qq, prob = probability, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side = 1, at = c(qqaux,rmax),col = "red",labels = FALSE,lwd.ticks = 0)
          axis(side = 1, at = qqaux, col = "red",col.axis="red", font = 2)
          abline(v = qqaux, lty = 2, col = "red")
          legend("topleft",
                 bty = "n", fill = "red",
                 legend = substitute(P(X>~ q ~ " ;" ~ prob == probability) == Pr ~ "\n\n" , list(q = qq, Pr = Pr, probability = probability))
          )
        }
        if (gui == "plot") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, prob = probability, lower.tail= FALSE)
          plotcurve(q, probability)
        }
        if (gui == "rstudio") {
          probability <- argaddit$probability
          prob <- pgeom(q = q, prob = probability, lower.tail = FALSE)
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
