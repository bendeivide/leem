#' Quantile distribution function.
#'
#' \code{Q} Quantile function for multiple distributions.
#'
#' @param p probability. The \code{p} argument need have length 1 and value lower then 1.
#' @param dist distribution to use. The default is \code{'normal'}. Options: \code{'normal'}, \code{'t-student'}, \code{'gumbel'}, \code{'binomial'}, \code{'poisson'}, and ....
#' @param lower.tail logical; if \code{TRUE} (default), the quantile function is computed; otherwise, the complement of the quantile function (survival function) will be computed. The \code{lower.tail} argument will only be valid for two-\code{sided = FALSE}.
#' @param two.sided logical. if \code{TRUE} (default), the calculation of the quantile function and survival will be presented; otherwise the \code{Q()} function will be based according to the \code{lower.tail} argument.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{"plot"} and \code{"rstudio"} and \code{"tcltk"}.
#' @param mfrow numerical vector. Considering the arguments \code{two.sided = TRUE} and \code{type = "both"}, the default will be to present two graphs (based on CDF and PDF) horizontally for the quantile function, that is, \code{mfrow = c(1, 2)} (default).
#' @param type character argument. The default is \code{"both"}; the output will display two plots (based on CDF and PDF) to present the result of Q(). The other options are: \code{"cdf"} and \code{"pdf"}.
#' @param ... additional parameters according to the chosen distribution.
#' @details The expression of quantile function is given by:
#' \deqn{
#' Q(p)=\inf {x\in \mathbb{R}: p \le F(x)},
#' }
#' where \code{p} is the first argument of \code{Q()} and \code{x} its return value;
#' @return \code{Q} returns the quantile and its graphical representation for a given distribution. The output is a vector.
#'
#' @examples
#' # Attaching package
#' library(leem)
#' \dontrun{
#' Q(p = 0.8, dist = "normal", mean = 200, sd=30)
#' }
#' @import manipulate
#' @import tkRplotR
#  @import shiny
#' @export
Q <- function(p, dist = "normal", lower.tail = TRUE, two.sided = FALSE, rounding = 2, gui = "plot", mfrow = c(1, 2), type = "both", ...) {
  if (p>1) stop("The 'p' argument are very large, please insert a value correct for probabilities!", call. = FALSE)
  argaddit <- list(...)
  argdef <- formals(Q)
  if (dist == "normal") {
    if (!any(names(argaddit) == "mean")) {
      mean <- readline(gettext("Insert the 'mean' argument: ", domain = "R-leem"))
      argaddit$mean <- as.numeric(mean)
    }
    if (!any(names(argaddit) == "sd")) {
      sd <- readline(gettext("Insert the 'sd' argument: ", domain = "R-leem"))
      argaddit$sd <- as.numeric(sd)
    }
    if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotqnormaltsboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqnormaltsboth(p, mean, sd, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
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
          tk_p <- leemset("tk_p", tclVar(p))
          tk_mu <- leemset("tk_mu", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                     gettext("leem package: Quantile function", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
                                      height = 500, fun = function(...) {
                                        p <- as.numeric(tclvalue(tk_p))
                                        mu <- as.numeric(tclvalue(tk_mu))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotqnormaltsboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = 0.01,
            to = 0.99,
            label = 'p',
            variable = tk_p,
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
      }
      if (type == "cdf") {
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotqnormaltscdf(p, mu, sigma, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqnormaltscdf(p, mean, sd, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
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
          tk_p <- leemset("tk_p", tclVar(p))
          tk_mu <- leemset("tk_mu", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                     gettext("leem package: Quantile function", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        p <- as.numeric(tclvalue(tk_p))
                                        mu <- as.numeric(tclvalue(tk_mu))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotqnormaltscdf(p = p, mu = mu, sigma = sigma, rounding)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = 0.01,
            to = 0.99,
            label = 'p',
            variable = tk_p,
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
      }
      if (type == "pdf") {
        if (gui == "plot") {
          mu <- argaddit$mean
          sigma <- argaddit$sd
          plotqnormaltspdf(p, mu,sigma, rounding)
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqnormaltspdf(p, mean, sd, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
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
          tk_p <- leemset("tk_p", tclVar(p))
          tk_mu <- leemset("tk_mu", tclVar(mu))
          tk_sigma <- leemset("tk_sigma", tclVar(sigma))
          sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                     gettext("leem package: Quantile function", domain = "R-leem"))

          tkpack(tklabel(tkplot, text = "Parameters"))

          tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                      height = 500, fun = function(...) {
                                        p <- as.numeric(tclvalue(tk_p))
                                        mu <- as.numeric(tclvalue(tk_mu))
                                        sigma <- as.numeric(tclvalue(tk_sigma))
                                        plotqnormaltspdf(p = p, mu = mu, sigma = sigma, rounding)
                                      })
          s01 <- tcltk::tkscale(
            tkplot,
            from = 0.01,
            to = 0.99,
            label = 'p',
            variable = tk_p,
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
      }
      point <- qnorm(c(p/2, 1 - p/2), mean = mu, sd = sigma)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            plotqnormalttboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqnormalttboth(p, mean, sd, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
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
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormalttboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        if (type == "cdf") {
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            plotqnormalltcdf(p, mu, sigma, rounding)

          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqnormalltcdf(p, mean, sd, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
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
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormalltcdf(p = p, mu = mu, sigma = sigma, rounding)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        if (type == "pdf") {
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd

            plotqnormallttpdf(p, mu,sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqnormallttpdf(p, mean, sd, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
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
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormallttpdf(p = p, mu = mu, sigma = sigma, rounding)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        point <- qnorm(p, mean = mu, sd = sigma)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            cex.main <- 0.8
            plotqnormaltfboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            cex.main <- 0.8
            manipulate::manipulate(plotqnormaltfboth(p, mean, sd, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
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
            cex.main <- 0.8
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormaltfboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            plotqnormalltfsf(p, mu, sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqnormalltfsf(p, mean, sd, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
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
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormalltfcdf(p = p, mu = mu, sigma = sigma, rounding)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        if (type == "pdf") {
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            plotqnormalltfpdf(p, mu,sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqnormalltfpdf(p, mean, sd, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
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
            tk_p <- leemset("tk_p", tclVar(p))
            tk_mu <- leemset("tk_mu", tclVar(mu))
            tk_sigma <- leemset("tk_sigma", tclVar(sigma))
            sapply(c("tk_p", "tk_mu", "tk_sigma"),
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
                       gettext("leem package: Quantile function", domain = "R-leem"))

            tkpack(tklabel(tkplot, text = "Parameters"))

            tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                        height = 500, fun = function(...) {
                                          p <- as.numeric(tclvalue(tk_p))
                                          mu <- as.numeric(tclvalue(tk_mu))
                                          sigma <- as.numeric(tclvalue(tk_sigma))
                                          plotqnormalltfpdf(p = p, mu = mu, sigma = sigma, rounding)
                                        })
            s01 <- tcltk::tkscale(
              tkplot,
              from = 0.01,
              to = 0.99,
              label = 'p',
              variable = tk_p,
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
        }
        point <- qnorm(p, mean = mu, sd = sigma, lower.tail = FALSE)
      }
    }
  }
  if (dist == "t-student") {
    if (!any(names(argaddit) == "df")) {
      df <- readline(gettext("Insert the 'df' argument: ", domain = "R-leem"))
      argaddit$df <- as.numeric(df)
      df <- argaddit$df
    }
    if (argaddit$df <= 0 ) stop("The 'df' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqtstudenttsboth(p, df, rounding, mfrow, cex.main = cex.main)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqtstudenttsboth(p, df, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 4 * df, df)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqtstudenttscdf(p, df, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqtstudenttscdf(p, df, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 4 * df, df)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqtstudenttspdf(p, df, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqtstudenttspdf(p, df, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 4 * df, df)
          )
        }
      }
      point <- qt(c(p/2, 1 - p/2), df = df)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqtstudentlttboth(p, df, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentlttboth(p, df, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 4  * df, df)
            )
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqtstudentlttcdf(p, df, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentlttcdf(p, df, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   mean = manipulate::slider(df, df + 4 * df, df)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqtstudentlttpdf(p, df, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentlttpdf(p, df, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   mean = manipulate::slider(df, df + 4* df, df)
            )
          }
        }
        point <- qt(p, df = df)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqtstudentltfboth(p, df, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentltfboth(p, df, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 4  * df, df)
            )
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqtstudentltfsf(p, df, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentltfsf(p, df, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   mean = manipulate::slider(df, df + 4 * df, df)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqtstudentltfpdf(p, df, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqtstudentltfpdf(p, df, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   mean = manipulate::slider(df, df + 4* df, df)
            )
          }
        }
        point <- qt(p, df = df, lower.tail = FALSE)
      }
    }
  }
  if (dist == "poisson") {
    if (!any(names(argaddit) == "lambda")) {
      lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
      argaddit$lambda <- as.numeric(lambda)
    }
    lambda <- argaddit$lambda
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqpoissontsboth(p, lambda, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqpoissontsboth(p, lambda, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqpoissontscdf(p, lambda, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqpoissontscdf(p, lambda, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }
      if (type == "pdf" || type  == "pf") {
        if(type == "pdf"){
            warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                 call. = FALSE,
                 domain = "R-leem")
        }
        if (gui == "plot") {
          plotqpoissontspdf(p, lambda, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqpoissontspdf(p, lambda, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }

      point <- qpois(c(p/2, 1 - p/2), lambda)
    } else{
      if(lower.tail == TRUE){
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqpoissonlttboth(p, lambda, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqpoissonlttboth(p, lambda, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            lambda <- argaddit$lambda
            plotqpoissonlttcdf(p, lambda, rounding)
          }
          if (gui == "rstudio") {
            lambda <- argaddit$lambda
            manipulate::manipulate(plotqpoissonlttcdf(p, lambda, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 200, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }

        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqpoissonlttpdf(p, lambda, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqpoissonlttpdf(p, lambda, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }



        point <- qpois(p = p, lambda = argaddit$lambda)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqpoissonltfboth(p, lambda, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqpoissonltfboth(p, lambda, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            plotqpoissonlttsf(p, lambda, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqpoissonlttsf(p, lambda, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 200, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }

        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqpoissonltfpdf(p, lambda, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqpoissonltfpdf(p, lambda, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   lambda = manipulate::slider(lambda, lambda + 4 * lambda, lambda)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }
        point <- qpois(p = p, lambda = argaddit$lambda, lower.tail=FALSE)
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
    prob <- argaddit$prob
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqbinomialtsboth(p, size, prob, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqbinomialtsboth(p, size, prob, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + size, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqbinomialtscdf(p, size, prob, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqbinomialtscdf(p, size, prob, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + size, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqbinomialtspdf(p, size, prob, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqbinomialtspdf(p, size, prob, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + size, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }

      point <- qbinom(c(p/2, 1 - p/2), size, prob)
    } else{
      if(lower.tail == TRUE){
        # Only type == "cdf"
        type <- "cdf"
        if (type == "cdf") {
          if (gui == "plot") {
            plotqbinomiallttcdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomiallttcdf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + sqrt(size), size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        point <- qbinom(p, size, prob)
      } else {
        # Only type == "cdf"
        type <- "cdf"
        if (type == "cdf") {
          if (gui == "plot") {
            plotqbinomiallttsf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomiallttsf(`1-p`, size, prob, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + sqrt(size), size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        point <- qbinom(p, size, prob, lower.tail = FALSE)
      }
    }

  }
  if (dist == "chisq") {
    if (!any(names(argaddit) == "df")) {
      df <- readline(gettext("Insert the 'df' argument: ", domain = "R-leem"))
      argaddit$df <- as.numeric(df)
    }
    if (!any(names(argaddit) == "ncp")) {
      ncp <- 0
      argaddit$ncp <- as.numeric(ncp)
    }
    if (argaddit$ncp < 0 ) stop("The 'ncp' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
    df <- argaddit$df
    ncp <- argaddit$ncp
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {

          plotqchisqtsboth(p, df, ncp, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqchisqtsboth(p, df, ncp, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 5 * df, df),
                                 ncp = manipulate::slider(ncp, df + 5 * df, ncp)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqchisqtscdf(p, df, ncp, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqchisqtscdf(p, df, ncp, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 5 * df, df),
                                 ncp = manipulate::slider(ncp, df + 5 * df, ncp)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqchisqtspdf(p, df, ncp, rounding)
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqchisqtspdf(p, df, ncp, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df = manipulate::slider(df, df + 5 * df, df),
                                 ncp = manipulate::slider(ncp, df + 5 * df, ncp)
          )
        }}
      point <- qchisq(c(p/2, 1 - p/2), df = df, ncp = ncp)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqchisqlttboth(p, df, ncp, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqchisqlttboth(p, df, ncp, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqchisqlttcdf(p, df, ncp, rounding)

          }
          if (gui == "rstudio") {
            # Plot
            manipulate::manipulate(plotqchisqlttcdf(p, df, ncp, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {

            plotqchisqlttpdf(p, df, ncp, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            manipulate::manipulate(plotqchisqlttpdf(p, df, ncp, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        point <- qchisq(p, df = df, ncp = ncp)
      }
      else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqchisqltfboth(p, df, ncp, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            cex.main <- 0.8
            manipulate::manipulate(plotqchisqltfboth(p, df, ncp, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqchisqltfsf(p, df, ncp, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            manipulate::manipulate(plotqchisqltfsf(p, df, ncp, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqchisqltfpdf(p, df, ncp, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            manipulate::manipulate(plotqchisqltfpdf(p, df, ncp, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df = manipulate::slider(df, df + 5 * df, df),
                                   ncp = manipulate::slider(ncp, df + 5 * df, ncp)
            )
          }
        }
        point <- qchisq(p, df = df, ncp = ncp, lower.tail = FALSE)
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
    if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                   call. = FALSE, domain = "R-leem")
    loca <- argaddit$location
    sca <- argaddit$scale
    if (lower.tail) {
      xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
      xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
        xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
        xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
      }
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
        xvq <- 10*abs(loca)
        xvq1 <- -10*abs(loca)
      }
      plotcurve <- function(p, location,scale) {
        q <- qgumbel(p,loca,sca,lower.tail = TRUE)
        curve(pgumbel(x,location,scale,lower.tail = TRUE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
              ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
              main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
        x <- seq(xvq1, q[1], by = 0.01)
        y <- seq(q[1], xvq,by = 0.01)
        fx <- pgumbel(x, location,scale)
        fy <- pgumbel(y, location, scale)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "gray90"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "red"
        )
        abline(v = location, lty = 2,col = "black")
        pp <- round(p, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(q, digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", pp)
        axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
        axis(side = 1, at = as.character(c(qqaux,xvq1)),labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.8,
               legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ "<=" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, media = location, varen = scale)))
      }
      if (gui == "plot") {
        point <- qgumbel(p,loca,sca,lower.tail = TRUE)
        plotcurve(p, loca, sca)
      }
      if (gui == "rstudio") {
        loca <- argaddit$location
        sca <- argaddit$scale
        manipulate::manipulate(plotcurve(p, loca, sca),
                               p = manipulate::slider(0.01,0.9, p),
                               loca = manipulate::slider(xvq1,xvq, loca ),
                               sca = manipulate::slider(1,xvq, sca))
        point <- qgumbel(p,loca,sca,lower.tail = TRUE)
      }
    }
    else {
      xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
      xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
        xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
        xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
      }
      if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
        xvq <- 10*abs(loca)
        xvq1 <- -10*abs(loca)
      }
      plotcurve <- function(p,location, scale) {
        q <- qgumbel(p,location,scale,lower.tail = FALSE)
        curve(pgumbel(x,location,scale,lower.tail = FALSE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
              ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
              main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
        x <- seq(xvq1, q, by = 0.01)
        y <- seq(q, xvq,by = 0.01)
        fx <- pgumbel(x, location, scale,lower.tail = FALSE)
        fy <- pgumbel(y, location, scale,lower.tail = FALSE)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "red"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "gray90"
        )
        abline(v = location, lty = 2, col="black")
        pp <- round(p, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(q, digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", pp)
        axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
        axis(side = 1, at = as.character(c(qqaux,xvq)),labels = c(qqaux,""),
             lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.8,
               legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ ">" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, media = location, varen = scale)))
      }
      if (gui == "plot") {
        point <- qgumbel(p,loca, sca,lower.tail = FALSE)
        plotcurve(p, loca,sca)
      }
      if (gui == "rstudio") {
        loca <- argaddit$location
        sca <- argaddit$scale
        manipulate::manipulate(plotcurve(p, loca, sca),
                               p = manipulate::slider(0.01,0.9, p),
                               loca = manipulate::slider(xvq1,xvq, loca ),
                               sca = manipulate::slider(1,xvq, sca))
        point <- qgumbel(p,loca,sca,lower.tail = FALSE)
      }
    }
  }
  if (dist == "beta") {
    if (!any(names(argaddit) == "alpha")){
      alpha <- readline(expression("Insert the 'alpha' argument: ",domain = "R-leem"))
      argaddit$alpha <- as.numeric(alpha)
    }
    if (!any(names(argaddit) == "beta")){
      beta <- readline(expression("Insert the 'beta' argument: ",domain = "R-leem"))
      argaddit$beta <- as.numeric(beta)
    }
    shape1 <- argaddit$alpha
    shape2 <- argaddit$beta
    if (lower.tail) {
      plotcurve <- function(p, shape1, shape2) {
        x <- qbeta(p, shape1, shape2)
        curve(pbeta(x, shape1, shape2), 0, 1, ylab = expression(F[X](x)),
              xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
        x <- seq(0, x[1], by = 0.01)
        y <- seq(x[1], 1, by = 0.01)
        fx <- pbeta(x, shape1, shape2)
        fy <- pbeta(y, shape1, shape2)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "gray90"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "red"
        )
        abline(v = shape1, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
        Pr <- round(qbeta(qq,  shape1 , shape2 , lower.tail = T), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 1 , col = "red", font = 2)
        abline(v = max(x), lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",
               legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1   ~ "; "~ beta == beta1) ~ "<=" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
        )
      }
      if (gui == "plot") {

        point <- qbeta(p, shape1 ,  shape2)
        plotcurve(p, shape1 , shape2)
      }
    }
    else {
      plotcurve <- function(p, shape1, shape2) {
        x<-qbeta(p, shape1, shape2)
        curve(pbeta(x, shape1, shape2 ),0, 1, ylab = expression(F[X](x)),
              xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
        x <- seq(0 , x[1], by = 0.01)
        y <- seq(x[1], 1, by = 0.01)
        fx <- pbeta(x, shape1, shape2)
        fy <- pbeta(y, shape1, shape2)
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col = "red"
        )
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col = "gray90"
        )
        abline(v = argaddit$alpha, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
        Pr <- round(qbeta(qq, shape1, shape2, lower.tail = T), digits = rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = max(x), labels = max(x), col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(max(x), 1)),labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
        abline(v = max(x), lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",
               legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1 ~ "; "~ beta == beta1) ~">" ~ Pr ~ "\n\n",
                                   list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
        )
      }
      if (gui == "plot") {
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta
        point <- qbeta(p, shape1, shape2)
        plotcurve(p, shape1, shape2)
      }

    }
  }
  if (dist == "nbinomial") {
    size <- argaddit$size
    prob <- argaddit$prob
    # Seguranças da distribuição nbinom .
    if (!any(names(argaddit) == "prob")) stop("Insira o argumento 'size'!", call. = FALSE)
    if (!any(names(argaddit) == "size")) stop("Insira o argumento 'prob'!", call. = FALSE)
    if (prob == 0) stop("Scale tem que ser difrente de 0!", call. = FALSE)
    # Cauda verdadeira.
    if (lower.tail) {
      # variação de valores para o eixo x;
      xvq <- 2*size
      xvq1 <- 2*size
      if ( size >= 0) {
        xvq <- 2*size
        xvq1 <- -2*size
      }
      if ( size == 0 ) {
        xvq <- 10
        xvq1 <- -10
      }
      # função plot.
      plotcurve <- function(p, s, pro){
        # Criando  o quantili.
        q <- qnbinom(p,s,pro,lower.tail = TRUE)
        x <- xvq1:xvq
        fx <- pnbinom(x,s,pro)
        xlim <- c(xvq1, xvq)
        ylim <- c(min(fx),max(fx)+(max(fx)/3))
        # plot
        plot.new()
        plot.window(xlim, ylim)
        # Eixos
        axis(1)
        axis(2)
        # Título e labels
        title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
        # prlote das linhas
        x1 <- xvq1:q
        fx1 <- pnbinom(x1,s,p)
        lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, fx1, lwd = 2, col = "red", pch = 10)
        x2 <- (q+1):xvq
        fx2 <- pnbinom(x2,s,p)
        lines(x2, fx2, type = "h", lwd = 2)
        points(x2, fx2, lwd = 2, pch = 10)
        qq <- round(q, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(p, rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.9,
               legend = substitute(Q(P ~ '<' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
      }
      # Configurando plotagem padrão.
      if (gui == "plot") {
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = TRUE)
        # Plotagem.
        plotcurve(p,size,prob)
      }
      if (gui == "rstudio") {
        #plotagem no rstudio
        manipulate::manipulate(plotcurve(p,size,prob),
                               p = manipulate::slider(0.01,0.9, p),
                               size = manipulate::slider(xvq1,xvq, size ),
                               prob = manipulate::slider(0.1,1,prob))
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = TRUE)
      }
    }
    # Caso Cauda Falsa.
    else {
      # variação de valores para o eixo x;
      xvq <- 2*size
      xvq1 <- 2*size
      if ( size >= 0) {
        xvq <- 2*size
        xvq1 <- -2*size
      }
      if ( size == 0 ) {
        xvq <- 10
        xvq1 <- -10
      }
      # função plot.
      plotcurve <- function(p, s, pro){
        # Criando  o quantili.
        q <- qnbinom(p,s,pro,lower.tail = FALSE)
        x <- xvq1:xvq
        fx <- pnbinom(x,s,pro, lower.tail = FALSE)
        xlim <- c(xvq1, xvq)
        ylim <- c(min(fx),max(fx)+(max(fx)/3))
        # plot
        plot.new()
        plot.window(xlim, ylim)
        # Eixos
        axis(1)
        axis(2)
        # Título e labels
        title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
        # prlote das linhas
        x1 <- xvq1:q
        fx1 <- pnbinom(x1,s,p,lower.tail = FALSE)
        lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, fx1, lwd = 2, col = "red", pch = 10)
        x2 <- (q+1):xvq
        fx2 <- pnbinom(x2,s,p,lower.tail = FALSE)
        lines(x2, fx2, type = "h", lwd = 2)
        points(x2, fx2, lwd = 2, pch = 10)
        qq <- round(q, digits = rounding)
        qqaux <- round(q, digits = rounding)
        Pr <- round(p, rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft", bty = "n", fill = "red",cex=0.9,
               legend = substitute(Q(P ~ '>=' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
      }
      # Configurando plotagem padrão.
      if (gui == "plot") {
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = FALSE)
        # Plotagem.
        plotcurve(p,size,prob)
      }
      if (gui == "rstudio") {
        #plotagem no rstudio
        manipulate::manipulate(plotcurve(p,size,prob),
                               p = manipulate::slider(0.01,0.9, p),
                               size = manipulate::slider(xvq1,xvq, size ),
                               prob = manipulate::slider(0.1,1,prob))
        #quantili
        point <- qnbinom(p,size,prob,lower.tail = FALSE)
      }
    }
  }############ INCORRECT
  if (dist == "geometric") {
    if (!any(names(argaddit) == "prob")){
      prob <- readline(expression("Insert the 'prob' argument: ",domain = "R-leem"))
      argaddit$prob <- as.numeric(prob)
    }
    if (lower.tail){
      plotcurve <- function(p, prob) {
        rmin <- -10*p
        rmax <- 10*p
        if (rmin < 0) rmin <- 0
        rmax <- ceiling(10*p)
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- (p + 1):rmax
        pointx <- pgeom(x, prob = prob)
        pointx1 <- pgeom(x1, prob = prob)
        pointx2 <- pgeom(x2, prob = prob)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
        lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, pointx1, lwd = 2, col = "red", pch = 19)
        lines(x2, pointx2, type = "h", lwd = 2)
        points(x2, pointx2, lwd = 2, pch = 19)
        abline(v = p, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <-round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmin)), lwd.ticks = 1,labels = F, col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~"<=" ~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
        )
      }
      if (gui == "plot") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        plotcurve(point, prob)
      }
      if (gui == "rstudio") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        manipulate::manipulate(plotcurve(point, prob),
                               p = manipulate::slider(0, p + 30, p),
                               prob = manipulate::slider(prob, prob + 200, prob)
        )
      }
    } else {
      plotcurve <- function(p, prob) {
        rmin <- -10*p
        rmax <- 10*p
        if (rmin < 0) rmin <- 0
        rmax <- ceiling(10*p)
        x <- rmin:rmax
        x1 <- rmin:p
        x2 <- (p + 1):rmax
        pointx <- pgeom(x, prob = prob)
        pointx1 <- pgeom(x1, prob = prob)
        pointx2 <- pgeom(x2, prob = prob)
        xlim <- c(rmin, rmax)
        ylim <- c(min(pointx), max(pointx) + 0.2)
        plot.new()
        plot.window(xlim, ylim)
        axis(1)
        axis(2)
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
        lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2)
        points(x1, pointx1, lwd = 2, pch = 19)
        lines(x2, pointx2, type = "h", lwd = 2, col = "red")
        points(x2, pointx2, lwd = 2, col = "red", pch = 19)
        abline(v = prob, lty = 2)
        qq <- round(p, digits = 2)
        qqaux <- round(p, digits = 2)
        Pr <- qqaux
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
        axis(side = 1, at = as.character(c(qqaux,rmax)),lwd.ticks = 0 ,labels = F, col = "red", font = 2)
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~">"~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
        )
      }
      if (gui == "plot") {
        prob <- argaddit$prob
        point <- qgeom(p = p, prob = prob)
        plotcurve(point, prob = prob)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        point <- qpois(p = p, lambda = lambda)
        manipulate::manipulate(plotcurve(point, lambda),
                               p = manipulate::slider(p, p + 30, p),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda)
        )
      }
    }
}
  if (dist == "hyper") {
    if (!any(names(argaddit) == "m")){
      m <- readline(expression("Insert the 'm' argument: ",domain = "R-leem"))
      argaddit$m <- as.numeric(m)
    }
    if (!any(names(argaddit) == "n")){
      n <- readline(expression("Insert the 'n' argument: ",domain = "R-leem"))
      argaddit$n <- as.numeric(n)
    }
    if (!any(names(argaddit) == "k")){
      k <- readline(expression("Insert the 'k' argument: ",domain = "R-leem"))
      argaddit$k <- as.numeric(k)
    }
    size <- argaddit$m
    samples <- argaddit$n
    sucess <- argaddit$k
    if (lower.tail) {
      plotcurve <- function(p, size, samples, sucess) {
        q <- qhyper(p, m = size, n = samples, k = sucess)
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
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
        points(x1, probx1, lwd = 2, col = "red", pch = 19)
        lines(x2, probx2, type = "h", lwd = 2)
        points(x2, probx2, lwd = 2, pch = 19)
        abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qhyper(p, m = size, n = samples, k = sucess), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(rmin, qqaux), labels = c("", qqaux),
          col = "red", font = 2, col.axis = "red"
        )
        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~"<="~ Pr ~ "\n\n",
                                   list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
        )
      }
      if (gui == "plot") {
        point <- qhyper(p, m = size, n = samples, k = sucess)
        plotcurve(p, size, samples, sucess)
      }
    }
    else {
      plotcurve <- function(p, size, samples, sucess) {
        q <- qhyper(p, m = size, n = samples, k = sucess)
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
        title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
        lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
        points(x1, probx1, lwd = 2, pch = 19)
        lines(x2, probx2, type = "h", lwd = 2, col = "red")
        points(x2, probx2, lwd = 2, pch = 19, col = "red")
        abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
        qq <- round(q, digits = 2)
        qqaux <- round(q, digits = 2)
        Pr <- round(qhyper(p, m = size, n = samples, k = sucess, lower.tail = F), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(
          side = 1, at = c(qqaux, size), labels = FALSE,
          col = "red", font = 2, col.axis = "red", lwd.ticks= 0
        )
        axis(
          side = 1, at = qqaux, labels = qqaux,
          col = "red", font = 2, col.axis = "red"
        )

        abline(v = qqaux, lty = 2, col = "red")
        legend("topleft",
               bty = "n", fill = "red",
               legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~">"~ Pr ~ "\n\n",
                                   list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
        )
      }
      if (gui == "plot") {
        point <- qhyper(p, m = size, n = samples, k = sucess, lower.tail = F)
        plotcurve(p, size, samples, sucess)
      }
    }
  }
  point <- round(point, rounding)
  return(point)
}





# Q <- function(p, dist = "normal", lower.tail = TRUE, two.sided = FALSE, rounding = 2, gui = "plot", mfrow = c(1, 2), type = "both", ...) {
#   if (p>1) stop("The 'p' argument are very large, please insert a value correct for probabilities!", call. = FALSE)
#   argaddit <- list(...)
#   argdef <- formals(Q)
#   if (dist == "normal") {
#     if (!any(names(argaddit) == "mean")) {
#       mean <- readline(gettext("Insert the 'mean' argument: ", domain = "R-leem"))
#       argaddit$mean <- as.numeric(mean)
#     }
#     if (!any(names(argaddit) == "sd")) {
#       sd <- readline(gettext("Insert the 'sd' argument: ", domain = "R-leem"))
#       argaddit$sd <- as.numeric(sd)
#     }
#     if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
#     if (two.sided) {
#       if (type == "both") {
#         if (gui == "plot") {
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           plotqnormaltsboth(p, mu, sigma, rounding, mfrow) # aux_quantile.R
#         }
#         if (gui == "rstudio") {
#           # Plot
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           manipulate::manipulate(plotqnormaltsboth(p, mean, sd, rounding, mfrow), # aux_quantile.R
#                                  p = manipulate::slider(0.01, 0.99, p),
#                                  mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                  sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#           )
#         }
#         if (gui == "tcltk") {
#           # Desabilitar warnings global
#           #options(warn = - 1)
#           war <- options(warn = - 1)
#           #on.exit(options(war))
#
#           # Environment of package
#           envleem <- new.env(parent = base::emptyenv())
#           leemget <- function(x) {
#             get(x, envir= envleem, inherits=FALSE )
#           }
#           leemset <- function(x, value) {
#             assign(x, value, envir= envleem)
#           }
#           globalvariables <- function(x, value) {
#             assign(x, value, envir= .GlobalEnv)
#           }
#
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           tk_p <- leemset("tk_p", tclVar(p))
#           tk_mu <- leemset("tk_mu", tclVar(mu))
#           tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#           sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                  function(x) globalvariables(x, leemget(x)))
#
#           # q1 <- NULL
#           # q2 <- NULL
#           # nu <- NULL
#           ##
#           # Disabled GUI (Type I)
#           oldmode <- tclServiceMode(FALSE)
#           # Logo
#           tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#           # Plot
#           tkplot <- tktoplevel()
#
#           #Icon main toplevel window
#           tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#           # Title
#           tkwm.title(tkplot,
#                      gettext("leem package: Quantile function", domain = "R-leem"))
#
#           tkpack(tklabel(tkplot, text = "Parameters"))
#
#           tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
#                                       height = 500, fun = function(...) {
#                                         p <- as.numeric(tclvalue(tk_p))
#                                         mu <- as.numeric(tclvalue(tk_mu))
#                                         sigma <- as.numeric(tclvalue(tk_sigma))
#                                         plotqnormaltsboth(p = p, mu = mu, sigma = sigma, rounding, mfrow)
#                                       })
#           s01 <- tcltk::tkscale(
#             tkplot,
#             from = 0.01,
#             to = 0.99,
#             label = 'p',
#             variable = tk_p,
#             showvalue = TRUE,
#             resolution = 0.01,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s02 <- tcltk::tkscale(
#             tkplot,
#             from = mu,
#             to = mu + 2 * sigma,
#             label = 'mean',
#             variable = tk_mu,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s03 <- tcltk::tkscale(
#             tkplot,
#             from = sigma,
#             to = sigma * 1.8,
#             label = 'standard deviation',
#             variable = tk_sigma,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           tkpack(s01, s02, s03,
#                  side = "top",
#                  expand = TRUE,
#                  before = tkplot$env$canvas,
#                  fill = "both")
#           # Activate GUI
#           finish <- tclServiceMode(oldmode)
#           tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#             response <- tk_messageBox(
#               title = gettext("Tell me something:", domain = "R-leem"),
#               message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#               icon = "question",
#               type = "yesno"
#             )
#             if (response == "yes") {
#               if (exists("tk_q1", envir = .GlobalEnv)) {
#                 rm("tk_q1", "tk_df", envir = .GlobalEnv)
#               }
#               tkdestroy(tkplot)
#             }
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             #war <- options(warn = - 1)
#             on.exit(options(war))
#           })
#         }
#       }
#       if (type == "cdf") {
#         if (gui == "plot") {
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           plotqnormaltscdf(p, mu, sigma, rounding)
#
#         }
#         if (gui == "rstudio") {
#           # Plot
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           manipulate::manipulate(plotqnormaltscdf(p, mean, sd, rounding),
#                                  p = manipulate::slider(0.01, 0.99, p),
#                                  mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                  sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#           )
#         }
#         if (gui == "tcltk") {
#           # Desabilitar warnings global
#           #options(warn = - 1)
#           war <- options(warn = - 1)
#           #on.exit(options(war))
#
#           # Environment of package
#           envleem <- new.env(parent = base::emptyenv())
#           leemget <- function(x) {
#             get(x, envir= envleem, inherits=FALSE )
#           }
#           leemset <- function(x, value) {
#             assign(x, value, envir= envleem)
#           }
#           globalvariables <- function(x, value) {
#             assign(x, value, envir= .GlobalEnv)
#           }
#
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           tk_p <- leemset("tk_p", tclVar(p))
#           tk_mu <- leemset("tk_mu", tclVar(mu))
#           tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#           sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                  function(x) globalvariables(x, leemget(x)))
#
#           # q1 <- NULL
#           # q2 <- NULL
#           # nu <- NULL
#           ##
#           # Disabled GUI (Type I)
#           oldmode <- tclServiceMode(FALSE)
#           # Logo
#           tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#           # Plot
#           tkplot <- tktoplevel()
#
#           #Icon main toplevel window
#           tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#           # Title
#           tkwm.title(tkplot,
#                      gettext("leem package: Quantile function", domain = "R-leem"))
#
#           tkpack(tklabel(tkplot, text = "Parameters"))
#
#           tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                       height = 500, fun = function(...) {
#                                         p <- as.numeric(tclvalue(tk_p))
#                                         mu <- as.numeric(tclvalue(tk_mu))
#                                         sigma <- as.numeric(tclvalue(tk_sigma))
#                                         plotqnormaltscdf(p = p, mu = mu, sigma = sigma, rounding)
#                                       })
#           s01 <- tcltk::tkscale(
#             tkplot,
#             from = 0.01,
#             to = 0.99,
#             label = 'p',
#             variable = tk_p,
#             showvalue = TRUE,
#             resolution = 0.01,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s02 <- tcltk::tkscale(
#             tkplot,
#             from = mu,
#             to = mu + 2 * sigma,
#             label = 'mean',
#             variable = tk_mu,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s03 <- tcltk::tkscale(
#             tkplot,
#             from = sigma,
#             to = sigma * 1.8,
#             label = 'standard deviation',
#             variable = tk_sigma,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           tkpack(s01, s02, s03,
#                  side = "top",
#                  expand = TRUE,
#                  before = tkplot$env$canvas,
#                  fill = "both")
#           # Activate GUI
#           finish <- tclServiceMode(oldmode)
#           tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#             response <- tk_messageBox(
#               title = gettext("Tell me something:", domain = "R-leem"),
#               message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#               icon = "question",
#               type = "yesno"
#             )
#             if (response == "yes") {
#               if (exists("tk_q1", envir = .GlobalEnv)) {
#                 rm("tk_q1", "tk_df", envir = .GlobalEnv)
#               }
#               tkdestroy(tkplot)
#             }
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             #war <- options(warn = - 1)
#             on.exit(options(war))
#           })
#         }
#       }
#       if (type == "pdf") {
#         if (gui == "plot") {
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           plotqnormaltspdf(p, mu,sigma, rounding)
#         }
#         if (gui == "rstudio") {
#           # Plot
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           manipulate::manipulate(plotqnormaltspdf(p, mean, sd, rounding),
#                                  p = manipulate::slider(0.01, 0.99, p),
#                                  mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                  sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#           )
#         }
#         if (gui == "tcltk") {
#           # Desabilitar warnings global
#           #options(warn = - 1)
#           war <- options(warn = - 1)
#           #on.exit(options(war))
#
#           # Environment of package
#           envleem <- new.env(parent = base::emptyenv())
#           leemget <- function(x) {
#             get(x, envir= envleem, inherits=FALSE )
#           }
#           leemset <- function(x, value) {
#             assign(x, value, envir= envleem)
#           }
#           globalvariables <- function(x, value) {
#             assign(x, value, envir= .GlobalEnv)
#           }
#
#           mu <- argaddit$mean
#           sigma <- argaddit$sd
#           tk_p <- leemset("tk_p", tclVar(p))
#           tk_mu <- leemset("tk_mu", tclVar(mu))
#           tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#           sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                  function(x) globalvariables(x, leemget(x)))
#
#           # q1 <- NULL
#           # q2 <- NULL
#           # nu <- NULL
#           ##
#           # Disabled GUI (Type I)
#           oldmode <- tclServiceMode(FALSE)
#           # Logo
#           tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#           # Plot
#           tkplot <- tktoplevel()
#
#           #Icon main toplevel window
#           tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#           # Title
#           tkwm.title(tkplot,
#                      gettext("leem package: Quantile function", domain = "R-leem"))
#
#           tkpack(tklabel(tkplot, text = "Parameters"))
#
#           tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                       height = 500, fun = function(...) {
#                                         p <- as.numeric(tclvalue(tk_p))
#                                         mu <- as.numeric(tclvalue(tk_mu))
#                                         sigma <- as.numeric(tclvalue(tk_sigma))
#                                         plotqnormaltspdf(p = p, mu = mu, sigma = sigma, rounding)
#                                       })
#           s01 <- tcltk::tkscale(
#             tkplot,
#             from = 0.01,
#             to = 0.99,
#             label = 'p',
#             variable = tk_p,
#             showvalue = TRUE,
#             resolution = 0.01,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s02 <- tcltk::tkscale(
#             tkplot,
#             from = mu,
#             to = mu + 2 * sigma,
#             label = 'mean',
#             variable = tk_mu,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           s03 <- tcltk::tkscale(
#             tkplot,
#             from = sigma,
#             to = sigma * 1.8,
#             label = 'standard deviation',
#             variable = tk_sigma,
#             showvalue = TRUE,
#             resolution = 1,
#             repeatdelay = 200,
#             repeatinterval = 100,
#             orient = "hor"
#           )
#           tkpack(s01, s02, s03,
#                  side = "top",
#                  expand = TRUE,
#                  before = tkplot$env$canvas,
#                  fill = "both")
#           # Activate GUI
#           finish <- tclServiceMode(oldmode)
#           tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#             response <- tk_messageBox(
#               title = gettext("Tell me something:", domain = "R-leem"),
#               message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#               icon = "question",
#               type = "yesno"
#             )
#             if (response == "yes") {
#               if (exists("tk_q1", envir = .GlobalEnv)) {
#                 rm("tk_q1", "tk_df", envir = .GlobalEnv)
#               }
#               tkdestroy(tkplot)
#             }
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             #war <- options(warn = - 1)
#             on.exit(options(war))
#           })
#         }
#       }
#       point <- qnorm(c(p/2, 1 - p/2), mean = mu, sd = sigma)
#     } else{
#       if (lower.tail) {
#         if (type == "both") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             plotqnormalttboth(p, mu, sigma, rounding, mfrow)
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormalttboth(p, mean, sd, rounding, mfrow),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormalttboth(p = p, mu = mu, sigma = sigma, rounding, mfrow)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         if (type == "cdf") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             plotqnormalltcdf(p, mu, sigma, rounding)
#
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormalltcdf(p, mean, sd, rounding),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormalltcdf(p = p, mu = mu, sigma = sigma, rounding)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         if (type == "pdf") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#
#             plotqnormallttpdf(p, mu,sigma, rounding)
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormallttpdf(p, mean, sd, rounding),
#                                    p = manipulate::slider(0.001, 0.999, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormallttpdf(p = p, mu = mu, sigma = sigma, rounding)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         point <- qnorm(p, mean = mu, sd = sigma)
#       } else {
#         if (type == "both") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             plotqnormaltfboth(p, mu, sigma, rounding, mfrow)
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormaltfboth(p, mean, sd, rounding, mfrow),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 700,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormaltfboth(p = p, mu = mu, sigma = sigma, rounding, mfrow)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         if (type == "cdf") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             plotqnormalltcdf(p, mu, sigma, rounding)
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormalltcdf(p, mean, sd, rounding),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormalltfcdf(p = p, mu = mu, sigma = sigma, rounding)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         if (type == "pdf") {
#           if (gui == "plot") {
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             plotqnormalltfpdf(p, mu,sigma, rounding)
#           }
#           if (gui == "rstudio") {
#             # Plot
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             manipulate::manipulate(plotqnormalltfpdf(p, mean, sd, rounding),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    mean = manipulate::slider(mu, mu + 2 * sigma, mu),
#                                    sd = manipulate::slider(sigma, sigma * 1.8, sigma)
#             )
#           }
#           if (gui == "tcltk") {
#             # Desabilitar warnings global
#             #options(warn = - 1)
#             war <- options(warn = - 1)
#             #on.exit(options(war))
#
#             # Environment of package
#             envleem <- new.env(parent = base::emptyenv())
#             leemget <- function(x) {
#               get(x, envir= envleem, inherits=FALSE )
#             }
#             leemset <- function(x, value) {
#               assign(x, value, envir= envleem)
#             }
#             globalvariables <- function(x, value) {
#               assign(x, value, envir= .GlobalEnv)
#             }
#
#             mu <- argaddit$mean
#             sigma <- argaddit$sd
#             tk_p <- leemset("tk_p", tclVar(p))
#             tk_mu <- leemset("tk_mu", tclVar(mu))
#             tk_sigma <- leemset("tk_sigma", tclVar(sigma))
#             sapply(c("tk_p", "tk_mu", "tk_sigma"),
#                    function(x) globalvariables(x, leemget(x)))
#
#             # q1 <- NULL
#             # q2 <- NULL
#             # nu <- NULL
#             ##
#             # Disabled GUI (Type I)
#             oldmode <- tclServiceMode(FALSE)
#             # Logo
#             tkimage.create("photo", "::image::iconleem", file = system.file("etc", "leem-icon.png", package = "leem"))
#
#             # Plot
#             tkplot <- tktoplevel()
#
#             #Icon main toplevel window
#             tcl("wm", "iconphoto", tkplot, "-default", "::image::iconleem")
#
#             # Title
#             tkwm.title(tkplot,
#                        gettext("leem package: Quantile function", domain = "R-leem"))
#
#             tkpack(tklabel(tkplot, text = "Parameters"))
#
#             tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
#                                         height = 500, fun = function(...) {
#                                           p <- as.numeric(tclvalue(tk_p))
#                                           mu <- as.numeric(tclvalue(tk_mu))
#                                           sigma <- as.numeric(tclvalue(tk_sigma))
#                                           plotqnormalltfpdf(p = p, mu = mu, sigma = sigma, rounding)
#                                         })
#             s01 <- tcltk::tkscale(
#               tkplot,
#               from = 0.01,
#               to = 0.99,
#               label = 'p',
#               variable = tk_p,
#               showvalue = TRUE,
#               resolution = 0.01,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s02 <- tcltk::tkscale(
#               tkplot,
#               from = mu,
#               to = mu + 2 * sigma,
#               label = 'mean',
#               variable = tk_mu,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             s03 <- tcltk::tkscale(
#               tkplot,
#               from = sigma,
#               to = sigma * 1.8,
#               label = 'standard deviation',
#               variable = tk_sigma,
#               showvalue = TRUE,
#               resolution = 1,
#               repeatdelay = 200,
#               repeatinterval = 100,
#               orient = "hor"
#             )
#             tkpack(s01, s02, s03,
#                    side = "top",
#                    expand = TRUE,
#                    before = tkplot$env$canvas,
#                    fill = "both")
#             # Activate GUI
#             finish <- tclServiceMode(oldmode)
#             tkwm.protocol(tkplot, "WM_DELETE_WINDOW", function() {
#               response <- tk_messageBox(
#                 title = gettext("Tell me something:", domain = "R-leem"),
#                 message = gettext("Do you want to use the GUI for the package?", domain = "R-leem"),
#                 icon = "question",
#                 type = "yesno"
#               )
#               if (response == "yes") {
#                 if (exists("tk_q1", envir = .GlobalEnv)) {
#                   rm("tk_q1", "tk_df", envir = .GlobalEnv)
#                 }
#                 tkdestroy(tkplot)
#               }
#               # Desabilitar warnings global
#               #options(warn = - 1)
#               #war <- options(warn = - 1)
#               on.exit(options(war))
#             })
#           }
#         }
#         point <- qnorm(p, mean = mu, sd = sigma, lower.tail = FALSE)
#       }
#     }
#   }
#   if (dist == "poisson") {
#     if (!any(names(argaddit) == "lambda")) {
#       lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
#       argaddit$lambda <- as.numeric(lambda)
#     }
#     if (two.sided) {
#       stop("Em desenvolvimento!")
#     } else{
#       if(lower.tail == TRUE){
#         # Only type == "cdf"
#         type <- "cdf"
#         if (type == "cdf") {
#           if (gui == "plot") {
#             lambda <- argaddit$lambda
#             plotqpoissonlttcdf(p, lambda, rounding)
#           }
#           if (gui == "rstudio") {
#             lambda <- argaddit$lambda
#             manipulate::manipulate(plotqpoissonlttcdf(p, lambda, rounding),
#                                    p = manipulate::slider(0.01, 0.99, p),
#                                    lambda = manipulate::slider(lambda, lambda + 200, lambda)
#             )
#           }
#           if (gui == "tcltk") {
#             stop("Em desenvolvimento!")
#           }
#         }
#         point <- qpois(p = p, lambda = argaddit$lambda)
#         } else {
#           # Only type == "cdf"
#           type <- "cdf"
#           if (type == "cdf") {
#             if (gui == "plot") {
#               lambda <- argaddit$lambda
#               plotqpoissonlttsf(p, lambda, rounding)
#             }
#             if (gui == "rstudio") {
#               lambda <- argaddit$lambda
#               manipulate::manipulate(plotqpoissonlttsf(`1-p`, lambda, rounding),
#                                      `1-p` = manipulate::slider(0.01, 0.99, p),
#                                      lambda = manipulate::slider(lambda, lambda + 200, lambda)
#               )
#             }
#             if (gui == "tcltk") {
#               stop("Em desenvolvimento!")
#             }
#           }
#           point <- qpois(p = p, lambda = argaddit$lambda, lower.tail = FALSE)
#         }
#     }
#
#   }
#   if (dist == "gumbel") {
#     if (!any(names(argaddit) == "location")) {
#       location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
#       argaddit$location <- as.numeric(location)
#     }
#     if (!any(names(argaddit) == "scale")) {
#       scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
#       argaddit$scale <- as.numeric(scale)
#     }
#     if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
#                                    call. = FALSE, domain = "R-leem")
#     loca <- argaddit$location
#     sca <- argaddit$scale
#     if (lower.tail) {
#       xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
#       xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
#       if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
#         xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
#         xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
#       }
#       if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
#         xvq <- 10*abs(loca)
#         xvq1 <- -10*abs(loca)
#       }
#       plotcurve <- function(p, location,scale) {
#         q <- qgumbel(p,loca,sca,lower.tail = TRUE)
#         curve(pgumbel(x,location,scale,lower.tail = TRUE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
#               ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
#               main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
#         x <- seq(xvq1, q[1], by = 0.01)
#         y <- seq(q[1], xvq,by = 0.01)
#         fx <- pgumbel(x, location,scale)
#         fy <- pgumbel(y, location, scale)
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col = "gray90"
#         )
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col = "red"
#         )
#         abline(v = location, lty = 2,col = "black")
#         pp <- round(p, digits = rounding)
#         qqaux <- round(q, digits = rounding)
#         Pr <- round(q, digits = rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", pp)
#         axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
#         axis(side = 1, at = as.character(c(qqaux,xvq1)),labels = c(qqaux,""),
#              lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",cex=0.8,
#                legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ "<=" ~ Pr ~ "\n\n",
#                                    list(p = qq, Pr = Pr, media = location, varen = scale)))
#       }
#       if (gui == "plot") {
#         point <- qgumbel(p,loca,sca,lower.tail = TRUE)
#         plotcurve(p, loca, sca)
#       }
#       if (gui == "rstudio") {
#         loca <- argaddit$location
#         sca <- argaddit$scale
#         manipulate::manipulate(plotcurve(p, loca, sca),
#                                p = manipulate::slider(0.01,0.9, p),
#                                loca = manipulate::slider(xvq1,xvq, loca ),
#                                sca = manipulate::slider(1,xvq, sca))
#         point <- qgumbel(p,loca,sca,lower.tail = TRUE)
#       }
#     }
#     else {
#       xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) -loca
#       xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) +loca
#       if ( qgumbel(p,loca,sca,lower.tail = TRUE) >= 0) {
#         xvq <- 2*qgumbel(p,loca,sca,lower.tail = TRUE) - loca
#         xvq1 <- -2*qgumbel(p,loca,sca,lower.tail = TRUE) + loca
#       }
#       if ( qgumbel(p,loca,sca,lower.tail = TRUE) == 0 ) {
#         xvq <- 10*abs(loca)
#         xvq1 <- -10*abs(loca)
#       }
#       plotcurve <- function(p,location, scale) {
#         q <- qgumbel(p,location,scale,lower.tail = FALSE)
#         curve(pgumbel(x,location,scale,lower.tail = FALSE), xvq1,xvq,ylim=c(0,1.2),xlim=c(xvq1,xvq),
#               ylab = expression(F[X](x)), xlab = "X",panel.first = grid(col = "gray90"),
#               main = gettext("Quantitative Function: Gumbel.", domain = "R-leem"))
#         x <- seq(xvq1, q, by = 0.01)
#         y <- seq(q, xvq,by = 0.01)
#         fx <- pgumbel(x, location, scale,lower.tail = FALSE)
#         fy <- pgumbel(y, location, scale,lower.tail = FALSE)
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col = "red"
#         )
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col = "gray90"
#         )
#         abline(v = location, lty = 2, col="black")
#         pp <- round(p, digits = rounding)
#         qqaux <- round(q, digits = rounding)
#         Pr <- round(q, digits = rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", pp)
#         axis(side = 1, at = qqaux, labels = qqaux, col = "black", font = 2)
#         axis(side = 1, at = as.character(c(qqaux,xvq)),labels = c(qqaux,""),
#              lwd.ticks = 0 ,col.axis = "red", col = "red", font = 2)
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",cex=0.8,
#                legend = substitute(Q("P=" ~ p ~"; " ~ location == media ~ "; "~ scale == varen )~ ">" ~ Pr ~ "\n\n",
#                                    list(p = qq, Pr = Pr, media = location, varen = scale)))
#       }
#       if (gui == "plot") {
#         point <- qgumbel(p,loca, sca,lower.tail = FALSE)
#         plotcurve(p, loca,sca)
#       }
#       if (gui == "rstudio") {
#         loca <- argaddit$location
#         sca <- argaddit$scale
#         manipulate::manipulate(plotcurve(p, loca, sca),
#                                p = manipulate::slider(0.01,0.9, p),
#                                loca = manipulate::slider(xvq1,xvq, loca ),
#                                sca = manipulate::slider(1,xvq, sca))
#         point <- qgumbel(p,loca,sca,lower.tail = FALSE)
#       }
#     }
#   }######ADD FUNÇÃO Q.
#   if (dist == "beta") {
#     if (!any(names(argaddit) == "alpha")){
#       alpha <- readline(expression("Insert the 'alpha' argument: ",domain = "R-leem"))
#       argaddit$alpha <- as.numeric(alpha)
#     }
#     if (!any(names(argaddit) == "beta")){
#       beta <- readline(expression("Insert the 'beta' argument: ",domain = "R-leem"))
#       argaddit$beta <- as.numeric(beta)
#     }
#     shape1 <- argaddit$alpha
#     shape2 <- argaddit$beta
#     if (lower.tail) {
#       plotcurve <- function(p, shape1, shape2) {
#         x <- qbeta(p, shape1, shape2)
#         curve(pbeta(x, shape1, shape2), 0, 1, ylab = expression(F[X](x)),
#               xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
#         x <- seq(0, x[1], by = 0.01)
#         y <- seq(x[1], 1, by = 0.01)
#         fx <- pbeta(x, shape1, shape2)
#         fy <- pbeta(y, shape1, shape2)
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col = "gray90"
#         )
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col = "red"
#         )
#         abline(v = shape1, lty = 2)
#         qq <- round(p, digits = 2)
#         qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
#         Pr <- round(qbeta(qq,  shape1 , shape2 , lower.tail = T), digits = rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
#         axis(side = 1, at = as.character(c(qqaux, 0)), labels = FALSE, lwd.ticks = 1 , col = "red", font = 2)
#         abline(v = max(x), lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",
#                legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1   ~ "; "~ beta == beta1) ~ "<=" ~ Pr ~ "\n\n",
#                                    list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
#         )
#       }
#       if (gui == "plot") {
#
#         point <- qbeta(p, shape1 ,  shape2)
#         plotcurve(p, shape1 , shape2)
#       }
#     }
#     else {
#       plotcurve <- function(p, shape1, shape2) {
#         x<-qbeta(p, shape1, shape2)
#         curve(pbeta(x, shape1, shape2 ),0, 1, ylab = expression(F[X](x)),
#               xlab = "X",panel.first = grid(col = "gray"), main = gettext("Quantitative Function: Beta.", domain = "R-leem"))
#         x <- seq(0 , x[1], by = 0.01)
#         y <- seq(x[1], 1, by = 0.01)
#         fx <- pbeta(x, shape1, shape2)
#         fy <- pbeta(y, shape1, shape2)
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col = "red"
#         )
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col = "gray90"
#         )
#         abline(v = argaddit$alpha, lty = 2)
#         qq <- round(p, digits = 2)
#         qqaux <- round(qbeta(p, shape1, shape2), digits = 2)
#         Pr <- round(qbeta(qq, shape1, shape2, lower.tail = T), digits = rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = max(x), labels = max(x), col = "red", font = 2, col.axis ="red")
#         axis(side = 1, at = as.character(c(max(x), 1)),labels = FALSE, lwd.ticks = 0 , col = "red", font = 2)
#         abline(v = max(x), lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",
#                legend = substitute(Q("P="~ p ~"; " ~ alpha == alpha1 ~ "; "~ beta == beta1) ~">" ~ Pr ~ "\n\n",
#                                    list(p = qq, Pr = Pr, alpha1 = shape1, beta1 = shape2))
#         )
#       }
#       if (gui == "plot") {
#         shape1 <- argaddit$alpha
#         shape2 <- argaddit$beta
#         point <- qbeta(p, shape1, shape2)
#         plotcurve(p, shape1, shape2)
#       }
#
#     }
#   }
#   if (dist == "nbinom") {
#     size <- argaddit$size
#     prob <- argaddit$prob
#     # Seguranças da distribuição nbinom .
#     if (!any(names(argaddit) == "prob")) stop("Insira o argumento 'size'!", call. = FALSE)
#     if (!any(names(argaddit) == "size")) stop("Insira o argumento 'prob'!", call. = FALSE)
#     if (prob == 0) stop("Scale tem que ser difrente de 0!", call. = FALSE)
#     # Cauda verdadeira.
#     if (lower.tail) {
#       # variação de valores para o eixo x;
#       xvq <- 2*size
#       xvq1 <- 2*size
#       if ( size >= 0) {
#         xvq <- 2*size
#         xvq1 <- -2*size
#       }
#       if ( size == 0 ) {
#         xvq <- 10
#         xvq1 <- -10
#       }
#       # função plot.
#       plotcurve <- function(p, s, pro){
#         # Criando  o quantili.
#         q <- qnbinom(p,s,pro,lower.tail = TRUE)
#         x <- xvq1:xvq
#         fx <- pnbinom(x,s,pro)
#         xlim <- c(xvq1, xvq)
#         ylim <- c(min(fx),max(fx)+(max(fx)/3))
#         # plot
#         plot.new()
#         plot.window(xlim, ylim)
#         # Eixos
#         axis(1)
#         axis(2)
#         # Título e labels
#         title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
#         # prlote das linhas
#         x1 <- xvq1:q
#         fx1 <- pnbinom(x1,s,p)
#         lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
#         points(x1, fx1, lwd = 2, col = "red", pch = 10)
#         x2 <- (q+1):xvq
#         fx2 <- pnbinom(x2,s,p)
#         lines(x2, fx2, type = "h", lwd = 2)
#         points(x2, fx2, lwd = 2, pch = 10)
#         qq <- round(q, digits = rounding)
#         qqaux <- round(q, digits = rounding)
#         Pr <- round(p, rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",cex=0.9,
#                legend = substitute(Q(P ~ '<' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
#       }
#       # Configurando plotagem padrão.
#       if (gui == "plot") {
#         #quantili
#         point <- qnbinom(p,size,prob,lower.tail = TRUE)
#         # Plotagem.
#         plotcurve(p,size,prob)
#       }
#       if (gui == "rstudio") {
#         #plotagem no rstudio
#         manipulate::manipulate(plotcurve(p,size,prob),
#                                p = manipulate::slider(0.01,0.9, p),
#                                size = manipulate::slider(xvq1,xvq, size ),
#                                prob = manipulate::slider(0.1,1,prob))
#         #quantili
#         point <- qnbinom(p,size,prob,lower.tail = TRUE)
#       }
#     }
#     # Caso Cauda Falsa.
#     else {
#       # variação de valores para o eixo x;
#       xvq <- 2*size
#       xvq1 <- 2*size
#       if ( size >= 0) {
#         xvq <- 2*size
#         xvq1 <- -2*size
#       }
#       if ( size == 0 ) {
#         xvq <- 10
#         xvq1 <- -10
#       }
#       # função plot.
#       plotcurve <- function(p, s, pro){
#         # Criando  o quantili.
#         q <- qnbinom(p,s,pro,lower.tail = FALSE)
#         x <- xvq1:xvq
#         fx <- pnbinom(x,s,pro, lower.tail = FALSE)
#         xlim <- c(xvq1, xvq)
#         ylim <- c(min(fx),max(fx)+(max(fx)/3))
#         # plot
#         plot.new()
#         plot.window(xlim, ylim)
#         # Eixos
#         axis(1)
#         axis(2)
#         # Título e labels
#         title(ylab = expression(F[X](x)), xlab = "X", main = "Quantile Function" )
#         # prlote das linhas
#         x1 <- xvq1:q
#         fx1 <- pnbinom(x1,s,p,lower.tail = FALSE)
#         lines(x1, fx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
#         points(x1, fx1, lwd = 2, col = "red", pch = 10)
#         x2 <- (q+1):xvq
#         fx2 <- pnbinom(x2,s,p,lower.tail = FALSE)
#         lines(x2, fx2, type = "h", lwd = 2)
#         points(x2, fx2, lwd = 2, pch = 10)
#         qq <- round(q, digits = rounding)
#         qqaux <- round(q, digits = rounding)
#         Pr <- round(p, rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = c(xvq1,qqaux),labels = c("",qqaux),col = "red", font = 2, col.axis= "red")
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft", bty = "n", fill = "red",cex=0.9,
#                legend = substitute(Q(P ~ '>=' ~ Pr ~"; " ~ size == si ~ "; "~ prob == pb ) == p ~ "\n\n", list(p = qq, Pr = Pr, si = s, pb = pro)))
#       }
#       # Configurando plotagem padrão.
#       if (gui == "plot") {
#         #quantili
#         point <- qnbinom(p,size,prob,lower.tail = FALSE)
#         # Plotagem.
#         plotcurve(p,size,prob)
#       }
#       if (gui == "rstudio") {
#         #plotagem no rstudio
#         manipulate::manipulate(plotcurve(p,size,prob),
#                                p = manipulate::slider(0.01,0.9, p),
#                                size = manipulate::slider(xvq1,xvq, size ),
#                                prob = manipulate::slider(0.1,1,prob))
#         #quantili
#         point <- qnbinom(p,size,prob,lower.tail = FALSE)
#       }
#     }
#   }############ INCORRECT
#   if (dist == "geometric") {
#     if (!any(names(argaddit) == "prob")){
#       prob <- readline(expression("Insert the 'prob' argument: ",domain = "R-leem"))
#       argaddit$prob <- as.numeric(prob)
#     }
#     if (lower.tail){
#       plotcurve <- function(p, prob) {
#         rmin <- -10*p
#         rmax <- 10*p
#         if (rmin < 0) rmin <- 0
#         rmax <- ceiling(10*p)
#         x <- rmin:rmax
#         x1 <- rmin:p
#         x2 <- (p + 1):rmax
#         pointx <- pgeom(x, prob = prob)
#         pointx1 <- pgeom(x1, prob = prob)
#         pointx2 <- pgeom(x2, prob = prob)
#         xlim <- c(rmin, rmax)
#         ylim <- c(min(pointx), max(pointx) + 0.2)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
#         lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
#         points(x1, pointx1, lwd = 2, col = "red", pch = 19)
#         lines(x2, pointx2, type = "h", lwd = 2)
#         points(x2, pointx2, lwd = 2, pch = 19)
#         abline(v = p, lty = 2)
#         qq <- round(p, digits = 2)
#         qqaux <-round(p, digits = 2)
#         Pr <- qqaux
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
#         axis(side = 1, at = as.character(c(qqaux,rmin)), lwd.ticks = 1,labels = F, col = "red", font = 2)
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~"<=" ~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
#         )
#       }
#       if (gui == "plot") {
#         prob <- argaddit$prob
#         point <- qgeom(p = p, prob = prob)
#         plotcurve(point, prob)
#       }
#       if (gui == "rstudio") {
#         prob <- argaddit$prob
#         point <- qgeom(p = p, prob = prob)
#         manipulate::manipulate(plotcurve(point, prob),
#                                p = manipulate::slider(0, p + 30, p),
#                                prob = manipulate::slider(prob, prob + 200, prob)
#         )
#       }
#     } else {
#       plotcurve <- function(p, prob) {
#         rmin <- -10*p
#         rmax <- 10*p
#         if (rmin < 0) rmin <- 0
#         rmax <- ceiling(10*p)
#         x <- rmin:rmax
#         x1 <- rmin:p
#         x2 <- (p + 1):rmax
#         pointx <- pgeom(x, prob = prob)
#         pointx1 <- pgeom(x1, prob = prob)
#         pointx2 <- pgeom(x2, prob = prob)
#         xlim <- c(rmin, rmax)
#         ylim <- c(min(pointx), max(pointx) + 0.2)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Geometric.", domain = "R-leem"))
#         lines(x1, pointx1, type = "h", panel.first = grid(), lwd = 2)
#         points(x1, pointx1, lwd = 2, pch = 19)
#         lines(x2, pointx2, type = "h", lwd = 2, col = "red")
#         points(x2, pointx2, lwd = 2, col = "red", pch = 19)
#         abline(v = prob, lty = 2)
#         qq <- round(p, digits = 2)
#         qqaux <- round(p, digits = 2)
#         Pr <- qqaux
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side = 1, at = qqaux, labels = qqaux, col = "red", font = 2, col.axis ="red")
#         axis(side = 1, at = as.character(c(qqaux,rmax)),lwd.ticks = 0 ,labels = F, col = "red", font = 2)
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend =  substitute(Q("P=" ~ Pr ~"; " ~ probability == prob ) ~">"~ p ~ "\n\n", list(p = qq, Pr = Pr, prob = prob))
#         )
#       }
#       if (gui == "plot") {
#         prob <- argaddit$prob
#         point <- qgeom(p = p, prob = prob)
#         plotcurve(point, prob = prob)
#       }
#       if (gui == "rstudio") {
#         lambda <- argaddit$lambda
#         point <- qpois(p = p, lambda = lambda)
#         manipulate::manipulate(plotcurve(point, lambda),
#                                p = manipulate::slider(p, p + 30, p),
#                                lambda = manipulate::slider(lambda, lambda + 200, lambda)
#         )
#       }
#     }
#   }
#   if (dist == "t-student"){
#     nu <- argaddit$df
#     if (!any(names(argaddit) == "df")) {
#       df <- readline("Insert the value of degree of freedom (df): ")
#       argaddit$df <- as.numeric(df)
#     }
#     if (lower.tail) {
#       plotcurve <- function(p, nu) {
#         x <- qt(p,nu)
#         x <- seq(-6, x[1], by=0.01)
#         y <- seq(x[1], 6, by=0.01)
#         fx <- pt(x, df = nu)
#         fy <- pt(y, df = nu)
#         curve(pt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
#               xlab="T", ylim = c(0, 1.2 * max(c(fx, fy))), panel.first = grid(col = "gray"),
#               main = gettext("Quantitative Function: T-Student.", domain = "R-leem"))
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col="gray90")
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col="red")
#         abline(v=0, lty=2)
#         qq <- round(p, digits=2)
#         qqaux <-round(qt(p,nu), digits=4)
#         Pr <- round(qt(p, df = nu, lower.tail = T), digits=4)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(side=1, at=qqaux, , labels = qqaux, tick = TRUE, lwd = 0,
#              col="red", font = 2, lwd.ticks = 1, col.axis="red")
#         axis(side=1, at=as.character(c(-6, qqaux)), tick = TRUE, lwd = 1,
#              col="red", font = 2, lwd.ticks = 0, labels = FALSE)
#         abline(v = qqaux, lty=2, col = "red")
#         legend("topleft", bty="n", fill="red",
#                legend=substitute(Q("P=" ~ p ~";"~ df == nu)~"<="~Pr~"\n\n", list(p = qq, Pr = Pr, nu = nu)))
#       }
#       if (gui == "plot" ) {
#         nu <- argaddit$df
#         point <- qt(p, df = nu)
#         plotcurve(p, nu)
#       }
#       if (gui == "rstudio") {
#         nu <- argaddit$df
#         manipulate::manipulate(plotcurve(qaux, nuaux),
#                                qaux = manipulate::slider(-6, 6, p),
#                                nuaux = manipulate::slider(1, 200, nu))
#         point <- qt(p, df = nu)
#       }
#     }
#     else {
#       #options(warn = - 1)
#       war <- options(warn = - 1)
#       on.exit(options(war))
#       plotcurve <- function(p, nu) {
#         x <- qt(p,nu)
#         x <- seq(x[1], 6, by=0.01)
#         y <- seq(-6, x[1], by=0.01)
#         fx <- pt(x, df = nu)
#         fy <- pt(y, df = nu)
#         curve(pt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
#               xlab="T", ylim = c(0, 1.2 * max(c(fx,fy))), panel.first = grid(col = "gray"),
#               main = gettext("Quantitative Function: T-Student.", domain = "R-leem"))
#         polygon(c(x, rev(x)),
#                 c(fx, rep(0, length(fx))),
#                 col="red")
#         polygon(c(y, rev(y)),
#                 c(fy, rep(0, length(fy))),
#                 col="gray90")
#         abline(v=0, lty=2)
#         qq <- round(p, digits=2)
#         qqaux <-round(qt(p,nu, lower.tail = F), digits= 4) *-1
#         qq <- gsub("\\.", ",", qq)
#         axis(side=1, at=qqaux, labels = qqaux, col.axis = "red", tick = TRUE, lwd = 0,
#              col="red", font = 2, lwd.ticks = 1)
#         axis(side=1, at=as.character(c(qqaux, 6)), tick = TRUE, lwd = 1,
#              col="red", font = 2, lwd.ticks = 0, labels = FALSE)
#         abline(v = qqaux, lty=2, col = "red")
#         legend("topleft", bty="n", fill="red",
#                legend=substitute(Q("P=" ~ p ~";"~ df == nu)~">"~Pr~"\n\n", list(p = qq, Pr = qqaux, nu = nu)))
#       }
#       if (gui == "plot") {
#         nu <- argaddit$df
#         point <- qt(p, df = nu, lower.tail = F) *-1
#         plotcurve(p, nu)
#       }
#       if (gui == "rstudio") {
#         nu <- argaddit$df
#         manipulate::manipulate(plotcurve(p, df),
#                                p = manipulate::slider(-6, 6, p),
#                                df = manipulate::slider(1, 200, nu))
#         point <- qt(p, nu)
#       }
#     }
#   }
#   if (dist == "binomial") {
#     if (!any(names(argaddit) == "size")){
#       size <- readline(expression("Insert the 'size' argument: ",domain = "R-leem"))
#       argaddit$size <- as.numeric(size)
#     }
#     if (!any(names(argaddit) == "prob")){
#     prob <- readline(expression("Insert the 'prob' argument: ",domain = "R-leem"))
#     argaddit$prob <- as.numeric(prob)
#   }
#     size <- argaddit$size
#     sucesso <- argaddit$prob
#     if (lower.tail) {
#       plotcurve <- function(p, size, prob) {
#         q <- qbinom(p, size, prob)
#         rmin <- 0
#         if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
#         x <- rmin:size
#         x1 <- rmin:q
#         x2 <- (q + 1):size
#         probx <- dbinom(x, size = size, prob = prob)
#         probx1 <- dbinom(x1, size = size, prob = prob)
#         probx2 <- dbinom(x2, size = size, prob = prob)
#         xlim <- c(rmin, size)
#         ylim <- c(min(probx), max(probx) + 0.1)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X",
#               main = gettext("Quantitative Function: Binomial.", domain = "R-leem"))
#         lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
#         points(x1, probx1, lwd = 2, col = "red", pch = 19)
#         lines(x2, probx2, type = "h", lwd = 2)
#         points(x2, probx2, lwd = 2, pch = 19)
#         abline(v = size * prob, lty = 2)
#         qq <- round(q, digits = 2)
#         qqaux <- round(q, digits = 2)
#         Pr <- round(qbinom(p, size = size, prob = prob, lower.tail = T), rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(
#           side = 1, at = c(rmin, qqaux), labels = c("", qqaux),
#           col = "red", font = 2, col.axis = "red"
#         )
#         axis(
#           side = 1, at = qqaux, labels = TRUE, lwd.ticks = 1,
#           col = "red", font = 2, col.axis= "red"
#         )
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend = substitute(Q("P=" ~ py ~"; " ~ size == n~"; " ~ p == prob)~"<="~Pr ~ "\n\n" ,
#                                    list(py = p, Pr = Pr, n = size, prob = prob))
#         )
#       }
#       if (gui == "plot") {
#         point <- qbinom(p, size = size, prob = sucesso)
#         plotcurve(p, size, prob = sucesso)
#       }
#     }
#     else {
#       plotcurve <- function(q, size, prob) {
#         q <- qbinom(p, size, prob, lower.tail = F)
#         rmin <- size * prob - 4 * sqrt(size * prob * (1 - prob))
#         if (rmin < 0 || rmin>q) rmin <- 0 else rmin <- round(rmin)
#         x <- rmin:size
#         x1 <- rmin:q
#         x2 <- (q + 1):size
#         probx <- dbinom(x, size = size, prob = prob)
#         probx1 <- dbinom(x1, size = size, prob = prob)
#         probx2 <- dbinom(x2, size = size, prob = prob)
#         xlim <- c(rmin, size)
#         ylim <- c(min(probx), max(probx) + 0.1)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X",
#               main = gettext("Quantitative Function: Binomial.", domain = "R-leem"))
#         lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
#         points(x1, probx1, lwd = 2, pch = 19)
#         lines(x2, probx2, type = "h", lwd = 2, col = "red")
#         points(x2, probx2, lwd = 2, pch = 19, col = "red")
#         abline(v = size * prob, lty = 2)
#         qq <- round(q, digits = 2)
#         qqaux <- round(q, digits = 2)
#         Pr <- round(qbinom(p, size = size, prob = prob, lower.tail = F), rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(
#           side = 1, at = c(qqaux, size), labels = c(qqaux, ""), lwd.ticks = 0,
#           col = "red", font = 2, col.axis= "red"
#         )
#         axis(
#           side = 1, at = qqaux, labels = TRUE, lwd.ticks = 1,
#           col = "red", font = 2, col.axis= "red"
#         )
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend = substitute(Q("P=" ~ py ~"; " ~ size == n~"; " ~ p == prob)~">"~Pr ~ "\n\n" ,
#                                    list(py = p, Pr = Pr, n = size, prob = prob))
#         )
#       }
#       if (gui == "plot") {
#         point <- qbinom(p, size = size, prob = sucesso, lower.tail = F)
#         plotcurve(p, size, prob = sucesso)
#       }
#     }
#   }
#   if (dist == "hyper") {
#     if (!any(names(argaddit) == "m")){
#       m <- readline(expression("Insert the 'm' argument: ",domain = "R-leem"))
#       argaddit$m <- as.numeric(m)
#     }
#     if (!any(names(argaddit) == "n")){
#       n <- readline(expression("Insert the 'n' argument: ",domain = "R-leem"))
#       argaddit$n <- as.numeric(n)
#     }
#     if (!any(names(argaddit) == "k")){
#       k <- readline(expression("Insert the 'k' argument: ",domain = "R-leem"))
#       argaddit$k <- as.numeric(k)
#     }
#     size <- argaddit$m
#     samples <- argaddit$n
#     sucess <- argaddit$k
#     if (lower.tail) {
#       plotcurve <- function(p, size, samples, sucess) {
#         q <- qhyper(p, m = size, n = samples, k = sucess)
#         rmin <- 0
#         x <- rmin:size
#         x1 <- rmin:q
#         x2 <- (q + 1):size
#         probx <- dhyper(x, m = size, n = samples, k = sucess)
#         probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
#         probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
#         xlim <- c(rmin, size)
#         ylim <- c(min(probx), max(probx) + 0.1)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
#         lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2, col = "red")
#         points(x1, probx1, lwd = 2, col = "red", pch = 19)
#         lines(x2, probx2, type = "h", lwd = 2)
#         points(x2, probx2, lwd = 2, pch = 19)
#         abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
#         qq <- round(q, digits = 2)
#         qqaux <- round(q, digits = 2)
#         Pr <- round(qhyper(p, m = size, n = samples, k = sucess), rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(
#           side = 1, at = c(rmin, qqaux), labels = c("", qqaux),
#           col = "red", font = 2, col.axis = "red"
#         )
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~"<="~ Pr ~ "\n\n",
#                                    list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
#         )
#       }
#       if (gui == "plot") {
#         point <- qhyper(p, m = size, n = samples, k = sucess)
#         plotcurve(p, size, samples, sucess)
#       }
#     }
#     else {
#       plotcurve <- function(p, size, samples, sucess) {
#         q <- qhyper(p, m = size, n = samples, k = sucess)
#         rmin <- 0
#         if (rmin < 0 || rmin > q) rmin <- 0 else rmin <- round(rmin)
#         x <- rmin:size
#         x1 <- rmin:q
#         x2 <- (q + 1):size
#         probx <- dhyper(x, m = size, n = samples, k = sucess)
#         probx1 <- dhyper(x1, m = size, n = samples, k = sucess)
#         probx2 <- dhyper(x2, m = size, n = samples, k = sucess)
#         xlim <- c(rmin, size)
#         ylim <- c(min(probx), max(probx) + 0.1)
#         plot.new()
#         plot.window(xlim, ylim)
#         axis(1)
#         axis(2)
#         title(ylab = expression(F[X](x)), xlab = "X", main = gettext("Quantitative Function: Hypergeometric.", domain = "R-leem"))
#         lines(x1, probx1, type = "h", panel.first = grid(col = "gray90"), lwd = 2)
#         points(x1, probx1, lwd = 2, pch = 19)
#         lines(x2, probx2, type = "h", lwd = 2, col = "red")
#         points(x2, probx2, lwd = 2, pch = 19, col = "red")
#         abline(v = match(max(dhyper(x = x, m = size, n = samples, k = sucess)), dhyper(x = x, m = size, n = samples, k = sucess)) - 1, lty = 2)
#         qq <- round(q, digits = 2)
#         qqaux <- round(q, digits = 2)
#         Pr <- round(qhyper(p, m = size, n = samples, k = sucess, lower.tail = F), rounding)
#         Pr <- gsub("\\.", ",", Pr)
#         qq <- gsub("\\.", ",", qq)
#         axis(
#           side = 1, at = c(qqaux, size), labels = FALSE,
#           col = "red", font = 2, col.axis = "red", lwd.ticks= 0
#         )
#         axis(
#           side = 1, at = qqaux, labels = qqaux,
#           col = "red", font = 2, col.axis = "red"
#         )
#
#         abline(v = qqaux, lty = 2, col = "red")
#         legend("topleft",
#                bty = "n", fill = "red",
#                legend = substitute(Q("P=" ~py ~ "; " ~ m == size ~ "; " ~ n == samples ~ "; " ~ k == sucess)~">"~ Pr ~ "\n\n",
#                                    list(py = p, Pr = Pr, size = size, samples = samples, sucess = sucess))
#         )
#       }
#       if (gui == "plot") {
#         point <- qhyper(p, m = size, n = samples, k = sucess, lower.tail = F)
#         plotcurve(p, size, samples, sucess)
#       }
#     }
#   }
#   point <- round(point, rounding)
#   return(point)
# }
