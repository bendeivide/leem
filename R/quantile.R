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
  if ( dist == "gumbel") {
    if (!any(names(argaddit) == "location")) {
      location <- readline(gettext("Insert the value of 'location' argument: ", domain = "R-leem"))
      argaddit$location <- as.numeric(location)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }

    if (argaddit$scale <= 0) stop("The 'scale' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (argaddit$location <= 0) stop("The 'location' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")

    location <- argaddit$location
    scale <- argaddit$scale
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqgumbeltsboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          manipulate::manipulate(plotqgumbeltsboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqgumbeltscdf(p, location, scale, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqgumbeltscdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqgumbeltspdf(p, location, scale, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqgumbeltspdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      point <- qgumbel(c(p/2, 1 - p/2), location, scale)
    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqgumbellttboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqgumbellttboth(p, location, scale, rounding, mfrow),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqgumbellttcdf(p, location, scale, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgumbellttcdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgumbellttpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgumbellttpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qgumbel(p, location, scale)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqgumbelltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqgumbelltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqgumbelltfsf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgumbelltfsf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgumbelltfpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotgumbelltfpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qgumbel(p, location, scale, lower.tail = FALSE)
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
                                 p = manipulate::slider(0.00, 0.99, p),
                                 df = manipulate::slider(1, 100, df),
                                 ncp = manipulate::slider(0, 100, ncp))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqchisqtscdf(p, df, ncp, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqchisqtscdf(p, df, ncp, rounding),
                                 p = manipulate::slider(0.00, 0.99, p),
                                 df = manipulate::slider(1, 100, df),
                                 ncp = manipulate::slider(0, 100, ncp)
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
                                 p = manipulate::slider(0.00, 0.99, p),
                                 df = manipulate::slider(1, 100, df),
                                 ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
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
                                   p = manipulate::slider(0.00, 0.99, p),
                                   df = manipulate::slider(1, 100, df),
                                   ncp = manipulate::slider(0, 100, ncp)
            )
          }
        }
        point <- qchisq(p, df = df, ncp = ncp, lower.tail = FALSE)
      }
    }
  }
  if (dist == "f") {
    if (!any(names(argaddit) == "df1")) {
      df1 <- readline(gettext("Insert the value of 'df1' argument: ", domain = "R-leem"))
      argaddit$df1 <- as.numeric(df1)
    }
    if (!any(names(argaddit) == "df2")) {
      df2 <- readline(gettext("Insert the value of 'df2' argument: ", domain = "R-leem"))
      argaddit$df2 <- as.numeric(df2)
    }

    if (argaddit$df1 <= 0) stop("The df1 arguments must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (argaddit$df2 <= 0) stop("The df2 arguments must be greater then zero!", call. = FALSE, domain = "R-leem")

    df1 <- argaddit$df1
    df2 <- argaddit$df2
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqftsboth(p, df1, df2, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          manipulate::manipulate(plotqftsboth(p, df1, df2, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df1 = manipulate::slider(1, 100, df1),
                                 df2 = manipulate::slider(1, 100, df2))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqftscdf(p, df1, df2, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqftscdf(p, df1, df2, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df1 = manipulate::slider(1, 100, df1),
                                 df2 = manipulate::slider(1, 100, df2))
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqftspdf(p, df1, df2, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqftspdf(p, df1, df2, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 df1 = manipulate::slider(1, 100, df1),
                                 df2 = manipulate::slider(1, 100, df2))
        }
      }
      point <- qf(c(p/2, 1 - p/2), df1, df2)
    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqflttboth(p, df1, df2, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqflttboth(p, df1, df2, rounding, mfrow),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqfltcdf(p, df1, df2, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqfltcdf(p, df1, df2, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqflttpdf(p, df1, df2, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqflttpdf(p, df1, df2, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        point <- qf(p, df1, df2)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqfltfboth(p, df1, df2, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqfltfboth(p, df1, df2, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqfltfsf(p, df1, df2, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqfltfsf(p, df1, df2, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqfltfpdf(p, df1, df2, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotfltfpdf(p, df1, df2, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   df1 = manipulate::slider(1, 100, df1),
                                   df2 = manipulate::slider(1, 100, df2))
          }
        }
        point <- qf(p, df1, df2, lower.tail = FALSE)
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
  if (dist == "beta") {
    if (!any(names(argaddit) == "alpha")) {
      alpha <- readline(gettext("Insert the 'alpha' argument: ", domain = "R-leem"))
      argaddit$alpha <- as.numeric(alpha)
    }
    if (!any(names(argaddit) == "beta")) {
      beta <- readline(gettext("Insert the 'beta' argument: ", domain = "R-leem"))
      argaddit$beta <- as.numeric(beta)
    }
    if (argaddit$beta <= 0 ) stop("The 'beta' argument alphast be greater then zero!", call. = FALSE, domain = "R-leem")
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          plotqbetatsboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          manipulate::manipulate(plotqbetatsboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                 beta = manipulate::slider(beta, beta * 1.8, beta)
          )
        }      }
      if (type == "cdf") {
        if (gui == "plot") {
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          plotqbetatscdf(p, alpha, beta, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          manipulate::manipulate(plotqbetatscdf(p, alpha, beta, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                 beta = manipulate::slider(beta, beta * 1.8, beta)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          plotqbetatspdf(p, alpha,beta, rounding)
        }
        if (gui == "rstudio") {
          # Plot
          alpha <- argaddit$alpha
          beta <- argaddit$beta
          manipulate::manipulate(plotqbetatspdf(p, alpha, beta, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                 beta = manipulate::slider(beta, beta * 1.8, beta)
          )
        }
      }
      point <- qbeta(c(p/2, 1 - p/2), alpha, beta)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            plotqbetalttboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            Q(0.8, alpha = 0, beta = 1)
            manipulate::manipulate(plotqbetalttboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            plotqbetalttcdf(p, alpha, beta, rounding)

          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            manipulate::manipulate(plotqbetalttcdf(p, alpha, beta, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta

            plotqbetalttpdf(p, alpha,beta, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            manipulate::manipulate(plotqbetalttpdf(p, alpha, beta, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        point <- qbeta(p, alpha, beta)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            cex.main <- 0.8
            plotqbetaltfboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            cex.main <- 0.8
            manipulate::manipulate(plotqbetaltfboth(p, alpha, beta, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            plotqbetaltfsf(p, alpha, beta, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            manipulate::manipulate(plotqbetaltfsf(p, alpha, beta, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            plotqbetaltfpdf(p, alpha,beta, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            alpha <- argaddit$alpha
            beta <- argaddit$beta
            manipulate::manipulate(plotqbetaltfpdf(p, alpha, beta, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   alpha = manipulate::slider(alpha, alpha + 2 * beta, alpha),
                                   beta = manipulate::slider(beta, beta * 1.8, beta)
            )
          }
        }
        point <- qbeta(p, alpha, beta, lower.tail = FALSE)
      }
    }
  }
  if (dist == "exp") {
    if (!any(names(argaddit) == "rate")) {
      rate <- readline(gettext("Insert the 'rate' argument: ", domain = "R-leem"))
      argaddit$rate <- as.numeric(rate)
      rate <- argaddit$rate
    }
    rate <- argaddit$rate
    if (argaddit$rate <= 0 ) stop("The 'rate' argument must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqexptsboth(p, rate, rounding, mfrow, cex.main = cex.main)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqexptsboth(p, rate, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 rate = manipulate::slider(rate, rate + 4 * rate, rate)
          )
        }
      }
      if (type == "crate") {
        if (gui == "plot") {
          plotqexptscrate(p, rate, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqexptscrate(p, rate, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 rate = manipulate::slider(rate, rate + 4 * rate, rate)
          )
        }
      }
      if (type == "prate") {
        if (gui == "plot") {
          plotqexptsprate(p, rate, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqexptsprate(p, rate, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 rate = manipulate::slider(rate, rate + 4 * rate, rate)
          )
        }
      }
      point <- qexp(c(p/2, 1 - p/2), rate = rate)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqexplttboth(p, rate, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexplttboth(p, rate, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   rate = manipulate::slider(rate, rate + 4  * rate, rate)
            )
          }
        }
        if (type == "crate") {
          if (gui == "plot") {
            plotqexplttcrate(p, rate, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexplttcrate(p, rate, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   mean = manipulate::slider(rate, rate + 4 * rate, rate)
            )
          }
        }
        if (type == "prate") {
          if (gui == "plot") {
            plotqexplttprate(p, rate, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexplttprate(p, rate, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   mean = manipulate::slider(rate, rate + 4* rate, rate)
            )
          }
        }
        point <- qexp(p, rate = rate)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqexpltfboth(p, rate, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexpltfboth(p, rate, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   rate = manipulate::slider(rate, rate + 4  * rate, rate)
            )
          }
        }
        if (type == "crate") {
          if (gui == "plot") {
            plotqexpltfsf(p, rate, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexpltfsf(p, rate, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   mean = manipulate::slider(rate, rate + 4 * rate, rate)
            )
          }
        }
        if (type == "prate") {
          if (gui == "plot") {
            plotqexpltfprate(p, rate, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqexpltfprate(p, rate, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   mean = manipulate::slider(rate, rate + 4* rate, rate)
            )
          }
        }
        point <- qexp(p, rate = rate, lower.tail = FALSE)
      }
    }
  }
  if (dist == "gamma") {
    if (!any(names(argaddit) == "shape")) {
      shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
      argaddit$shape <- as.numeric(shape)
    }
    if (!any(names(argaddit) == "rate")) {
      rate <- readline(gettext("Insert the value of 'rate' argument (press enter to skip): ", domain = "R-leem"))
      argaddit$rate <- as.numeric(rate)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument (press enter to skip): ", domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }

    shape <- argaddit$shape
    rate <- argaddit$rate
    scale <- argaddit$scale
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqgammatsboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          if(is.na(rate)){
            manipulate::manipulate(plotqgammatsboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
          if(is.na(scale)){
            manipulate::manipulate(plotqgammatsboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   rate = manipulate::slider(0, 100, rate))
          }

        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqgammatscdf(p, shape, rate, scale, rounding)

        }
        if (gui == "rstudio") {
          if(is.na(rate)){
            manipulate::manipulate(plotqgammatscdf(p, shape, rate, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
          if(is.na(scale)){
            manipulate::manipulate(plotqgammatscdf(p, shape, rate, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   rate = manipulate::slider(0, 100, rate))
          }

        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqgammatspdf(p, shape, rate, scale, rounding)
        }
        if (gui == "rstudio") {
          if(is.na(rate)){
            manipulate::manipulate(plotqgammatspdf(p, shape, rate, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
          if(is.na(scale)){
            manipulate::manipulate(plotqgammatspdf(p, shape, rate, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   rate = manipulate::slider(0, 100, rate))
          }

        }
      }
      if(is.na(rate)){
        point <- qgamma(c(p/2, 1 - p/2), shape, scale = scale)
      }
      if(is.na(scale)){
        point <- qgamma(c(p/2, 1 - p/2), shape, rate = rate)
      }

    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqgammalttboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            if(is.na(rate)){
              manipulate::manipulate(plotqgammalttboth(p, shape, rate, scale, rounding, mfrow),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotqgammalttboth(p, shape, rate, scale, rounding, mfrow),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqgammalttcdf(p, shape, rate, scale, rounding)

          }
          if (gui == "rstudio") {
            if(is.na(rate)){
              manipulate::manipulate(plotqgammalttcdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotqgammalttcdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }

          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgammalttpdf(p, shape, rate, scale, rounding)
          }
          if (gui == "rstudio") {
            if(is.na(rate)){
              manipulate::manipulate(plotqgammalttpdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotqgammalttpdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }

          }
        }
        if(is.na(rate)){
          point <- qgamma(p, shape, scale = scale)
        }
        if(is.na(scale)){
          point <- qgamma(p, shape, rate = rate)
        }
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqgammaltfboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            if(is.na(rate)){
              manipulate::manipulate(plotqgammaltfboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotqgammaltfboth(p, shape, rate, scale, rounding, mfrow, cex.main = cex.main),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }

          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqgammaltfsf(p, shape, rate, scale, rounding)
          }
          if (gui == "rstudio") {
            if(is.na(rate)){
              manipulate::manipulate(plotqgammaltfsf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotqgammaltfsf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }

          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgammaltfpdf(p, shape, rate, scale, rounding)
          }
          if (gui == "rstudio") {
            if(is.na(rate)){
              manipulate::manipulate(plotgammaltfpdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     scale = manipulate::slider(0, 100, scale))
            }
            if(is.na(scale)){
              manipulate::manipulate(plotgammaltfpdf(p, shape, rate, scale, rounding),
                                     p = manipulate::slider(0.01, 0.99, p),
                                     shape = manipulate::slider(0, 100, shape),
                                     rate = manipulate::slider(0, 100, rate))
            }
          }
        }
        if(is.na(rate)){
          point <- qgamma(p, shape, scale = scale, lower.tail = FALSE)

        }
        if(is.na(scale)){
          point <- qgamma(p, shape, rate = rate, lower.tail = FALSE)

        }
      }
    }
  }
  if (dist == "cauchy") {
    if (!any(names(argaddit) == "location")) {
      location <- readline(gettext("Insert the value of 'location' argument: ", domain = "R-leem"))
      argaddit$location <- as.numeric(location)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }

    if (argaddit$scale <= 0) stop("The 'scale' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (argaddit$location <= 0) stop("The 'location' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")

    location <- argaddit$location
    scale <- argaddit$scale
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqcauchytsboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          manipulate::manipulate(plotqcauchytsboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqcauchytscdf(p, location, scale, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqcauchytscdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqcauchytspdf(p, location, scale, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqcauchytspdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      point <- qcauchy(c(p/2, 1 - p/2), location, scale)
    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqcauchylttboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqcauchylttboth(p, location, scale, rounding, mfrow),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqcauchylttcdf(p, location, scale, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqcauchylttcdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqcauchylttpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqcauchylttpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qcauchy(p, location, scale)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqcauchyltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqcauchyltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqcauchyltfsf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqcauchyltfsf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqcauchyltfpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotcauchyltfpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qcauchy(p, location, scale, lower.tail = FALSE)
      }
    }
  }
  if (dist == "logis") {
    if (!any(names(argaddit) == "location")) {
      location <- readline(gettext("Insert the value of 'location' argument: ", domain = "R-leem"))
      argaddit$location <- as.numeric(location)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }

    if (argaddit$scale <= 0) stop("The 'scale' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (argaddit$location <= 0) stop("The 'location' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")

    location <- argaddit$location
    scale <- argaddit$scale
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqlogistsboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          manipulate::manipulate(plotqlogistsboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqlogistscdf(p, location, scale, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqlogistscdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqlogistspdf(p, location, scale, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqlogistspdf(p, location, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 location = manipulate::slider(0, 100, location),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      point <- qlogis(c(p/2, 1 - p/2), location, scale)
    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqlogislttboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqlogislttboth(p, location, scale, rounding, mfrow),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqlogislttcdf(p, location, scale, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqlogislttcdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqlogislttpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqlogislttpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qlogis(p, location, scale)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqlogisltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqlogisltfboth(p, location, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqlogisltfsf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqlogisltfsf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqlogisltfpdf(p, location, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotlogisltfpdf(p, location, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   location = manipulate::slider(0, 100, location),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qlogis(p, location, scale, lower.tail = FALSE)
      }
    }
  }
  if (dist == "lnormal") {
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
          plotqlnormaltsboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqlnormaltsboth(p, mean, sd, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
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
                                        plotqlnormaltsboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
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
          plotqlnormaltscdf(p, mu, sigma, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqlnormaltscdf(p, mean, sd, rounding),
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
                                        plotqlnormaltscdf(p = p, mu = mu, sigma = sigma, rounding)
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
          plotqlnormaltspdf(p, mu,sigma, rounding)
        }
        if (gui == "rstudio") {
          # Plot
          mu <- argaddit$mean
          sigma <- argaddit$sd
          manipulate::manipulate(plotqlnormaltspdf(p, mean, sd, rounding),
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
                                        plotqlnormaltspdf(p = p, mu = mu, sigma = sigma, rounding)
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
      point <- qlnorm(c(p/2, 1 - p/2), mean = mu, sd = sigma)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            plotqlnormalttboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqlnormalttboth(p, mean, sd, rounding, mfrow, cex.main = cex.main),
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
                                          plotqlnormalttboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
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
            plotqlnormalltcdf(p, mu, sigma, rounding)

          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqlnormalltcdf(p, mean, sd, rounding),
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
                                          plotqlnormalltcdf(p = p, mu = mu, sigma = sigma, rounding)
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

            plotqlnormallttpdf(p, mu,sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqlnormallttpdf(p, mean, sd, rounding),
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
                                          plotqlnormallttpdf(p = p, mu = mu, sigma = sigma, rounding)
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
        point <- qlnorm(p, meanlog = mu, sdlog = sigma)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            mu <- argaddit$mean
            sigma <- argaddit$sd
            cex.main <- 0.8
            plotqlnormaltfboth(p, mu, sigma, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            cex.main <- 0.8
            manipulate::manipulate(plotqlnormaltfboth(p, mean, sd, rounding, mfrow, cex.main = cex.main),
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
                                          plotqlnormaltfboth(p = p, mu = mu, sigma = sigma, rounding, mfrow, cex.main = cex.main)
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
            plotqlnormalltfsf(p, mu, sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqlnormalltfsf(p, mean, sd, rounding),
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
                                          plotqlnormalltfcdf(p = p, mu = mu, sigma = sigma, rounding)
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
            plotqlnormalltfpdf(p, mu,sigma, rounding)
          }
          if (gui == "rstudio") {
            # Plot
            mu <- argaddit$mean
            sigma <- argaddit$sd
            manipulate::manipulate(plotqlnormalltfpdf(p, mean, sd, rounding),
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
        point <- qlnorm(p, meanlog = mu, sdlog = sigma, lower.tail = FALSE)
      }
    }
  }
  if (dist == "tukey") {
    stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
  }
  if (dist == "weibull") {
    if (!any(names(argaddit) == "shape")) {
      shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
      argaddit$shape <- as.numeric(shape)
    }
    if (!any(names(argaddit) == "scale")) {
      scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
      argaddit$scale <- as.numeric(scale)
    }

    if (argaddit$scale <= 0) stop("The 'scale' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")
    if (argaddit$shape <= 0) stop("The 'shape' arguments must be greater then zero!", call. = FALSE, domain = "R-leem")

    shape <- argaddit$shape
    scale <- argaddit$scale
    # Auxiliar variables

    if (two.sided) {
      if (type == "both") {
        if (gui == "plot") {
          cex.main <- 0.7
          plotqweibulltsboth(p, shape, scale, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          cex.main <- 0.7
          manipulate::manipulate(plotqweibulltsboth(p, shape, scale, rounding, mfrow, cex.main = cex.main),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 shape = manipulate::slider(0, 100, shape),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqweibulltscdf(p, shape, scale, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqweibulltscdf(p, shape, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 shape = manipulate::slider(0, 100, shape),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqweibulltspdf(p, shape, scale, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqweibulltspdf(p, shape, scale, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 shape = manipulate::slider(0, 100, shape),
                                 scale = manipulate::slider(0, 100, scale))
        }
      }
      point <- qweibull(c(p/2, 1 - p/2), shape, scale)
    } else{
      if (lower.tail) {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqweibulllttboth(p, shape, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, mean = 0, sd = 1)
            manipulate::manipulate(plotqweibulllttboth(p, shape, scale, rounding, mfrow),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqweibulllttcdf(p, shape, scale, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqweibulllttcdf(p, shape, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqweibulllttpdf(p, shape, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqweibulllttpdf(p, shape, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qweibull(p, shape, scale)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqweibullltfboth(p, shape, scale, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqweibullltfboth(p, shape, scale, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqweibullltfsf(p, shape, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqweibullltfsf(p, shape, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqweibullltfpdf(p, shape, scale, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotweibullltfpdf(p, shape, scale, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   shape = manipulate::slider(0, 100, shape),
                                   scale = manipulate::slider(0, 100, scale))
          }
        }
        point <- qweibull(p, shape, scale, lower.tail = FALSE)
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
      if (type == "pdf" || type  == "pf") {
        if(type == "pdf"){
          warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                  call. = FALSE,
                  domain = "R-leem")
        }
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
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqbinomiallttpdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomiallttpdf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + sqrt(size), size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotqbinomiallttboth(p, size, prob, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomiallttboth(p, size, prob, rounding, mfrow, cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + sqrt(size), size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        point <- qbinom(p, size, prob)
      } else {
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
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'Probability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'Probability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqbinomiallttpdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomialltfpdf(`1-p`, size, prob, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + sqrt(size), size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotqbinomialltfboth(p, size, prob, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqbinomialltfboth(`1-p`, size, prob, rounding, mfrow, cex.main),
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
  if (dist == "nbinom") {
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
          plotqnbinomtsboth(p, size, prob, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqnbinomtsboth(p, size, prob, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + 4 * size, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqnbinomtscdf(p, size, prob, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqnbinomtscdf(p, size, prob, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + 4 * size, size),
                                 prob = manipulate::slider(0, 1, prob)
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
          plotqnbinomtspdf(p, size, prob, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqnbinomtspdf(p, size, prob, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 size = manipulate::slider(size, size + 4 * size, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }

      point <- qnbinom(c(p/2, 1 - p/2), size, prob)
    } else{
      if(lower.tail == TRUE){
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqnbinomlttboth(p, size, prob, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqnbinomlttboth(p, size, prob, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 4 * size, size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            size <- argaddit$size
            plotqnbinomlttcdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            size <- argaddit$size
            manipulate::manipulate(plotqnbinomlttcdf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 200, size),
                                   prob = manipulate::slider(0, 1, prob)
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
            plotqnbinomlttpdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqnbinomlttpdf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 4 * size, size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }



        point <- qnbinom(p = p, size, prob)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqnbinomltfboth(p, size, prob, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqnbinomltfboth(p, size, prob, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 4 * size, size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            plotqnbinomlttsf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqnbinomlttsf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 200, size),
                                   prob = manipulate::slider(0, 1, prob)
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
            plotqnbinomltfpdf(p, size, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqnbinomltfpdf(p, size, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   size = manipulate::slider(size, size + 4 * size, size),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }
        point <- qnbinom(p = p, size, prob, lower.tail=FALSE)
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
    m <- argaddit$m
    n <- argaddit$n
    k <- argaddit$k
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqhypertsboth(p, m, n, k, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqhypertsboth(p, m, n, k, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(1, k+100, m),
                                 n = manipulate::slider(1, k+100, n),
                                 k = manipulate::slider(1, k+100, k)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqhypertscdf(p, m, n, k, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqhypertscdf(p, m, n, k, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(1, k+100, m),
                                 n = manipulate::slider(1, k+100, n),
                                 k = manipulate::slider(1, k+100, k)
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
          plotqhypertspdf(p, m, n, k, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqhypertspdf(p, m, n, k, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(1, k+100, m),
                                 n = manipulate::slider(1, k+100, n),
                                 k = manipulate::slider(1, k+100, k)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }

      point <- qhyper(c(p/2, 1 - p/2), m, n, k,)
    } else{
      if(lower.tail == TRUE){
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqhyperlttboth(p, m, n, k, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperlttboth(p, m, n, k, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            plotqhyperlttcdf(p, m, n, k, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperlttcdf(p, m, n, k, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
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
            plotqhyperlttpdf(p, m, n, k, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperlttpdf(p, m, n, k, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }



        point <- qhyper(p = p, m, n, k)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqhyperltfboth(p, m, n, k, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperltfboth(p, m, n, k, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            plotqhyperlttsf(p, m, n, k, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperlttsf(p, m, n, k, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
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
            plotqhyperltfpdf(p, m, n, k, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqhyperltfpdf(p, m, n, k, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(1, k+100, m),
                                   n = manipulate::slider(1, k+100, n),
                                   k = manipulate::slider(1, k+100, k)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }
        point <- qhyper(p = p, m, n, k, lower.tail=FALSE)
      }
    }
  }
  if (dist == "geom") {
    if (!any(names(argaddit) == "prob")) {
      prob <- readline(gettext("Insert the 'prob' argument: ", domain = "R-leem"))
      argaddit$prob <- as.numeric(prob)
    }
    prob <- argaddit$prob
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {


          plotqgeomtsboth(p, prob, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqgeomtsboth(p, prob, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqgeomtscdf(p, prob, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqgeomtscdf(p, prob, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }
      if (type == "pdf") {
        if (gui == "plot") {
          plotqgeomtspdf(p, prob, rounding)
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqgeomtspdf(p, prob, sd, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
      }
      point <- qgeom(c(p/2, 1 - p/2), prob)
    } else{
      if (lower.tail) {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqgeomlttboth(p, prob, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            Q(0.8, prob = 0, sd = 1)
            manipulate::manipulate(plotqgeomttboth(p, prob, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        if (type == "cdf") {
          if (gui == "plot") {
            plotqgeomlttcdf(p, prob, rounding)

          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgeomlttcdf(p, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgeomlttpdf(p, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgeomlttpdf(p, prob, rounding),
                                   p = manipulate::slider(0.001, 0.999, p),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        point <- qgeom(p, prob)
      } else {
        if (type == "both") {
          if (gui == "plot") {
            cex.main <- 0.8
            plotqgeomltfboth(p, prob, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            cex.main <- 0.8
            manipulate::manipulate(plotqgeomltfboth(p, prob, rounding, mfrow, cex.main = cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   prob = manipulate::slider(prob, prob + 2 * sigma, prob)
            )
          }
        }
        if (type == "cdf") {
          warning("The plot shown is based on the survival function", call. = FALSE, domain = "R-leem")
          if (gui == "plot") {
            plotqgeomltfsf(p, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgeomltfsf(p, prob, sd, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        if (type == "pdf") {
          if (gui == "plot") {
            plotqgeomltfpdf(p, prob, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqgeomltfpdf(p, prob, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   prob = manipulate::slider(0, 1, prob)
            )
          }
        }
        point <- qgeom(p, prob, lower.tail = FALSE)
      }
    }
  }
  if (dist == "unif") {
    if (!any(names(argaddit) == "min")) {
      min <- readline(gettext("Insert the value of 'min' argument: ", domain = "R-leem"))
      argaddit$min <- as.numeric(min)
    }
    if (!any(names(argaddit) == "max")) {
      max <- readline(gettext("Insert the value of 'max' argument: ", domain = "R-leem"))
      argaddit$max <- as.numeric(max)
    }
    min <- argaddit$min
    max <- argaddit$max
    if (two.sided) {
      if (type == "both") {
        # Plot min title
        cex.main <- 0.7
        if (gui == "plot") {
          plotquniftsboth(p, min, max, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotquniftsboth(p, min, max, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 min = manipulate::slider(min, max + max, min),
                                 max = manipulate::slider(max, max + max, max)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotquniftscdf(p, min, max, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotquniftscdf(p, min, max, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 min = manipulate::slider(min, max + max, min),
                                 max = manipulate::slider(max, max + max, max)
          )
        }
      }
      if (type == "pdf" || type  == "pf") {
        if(type == "pdf"){
          warning("The type of 'maxability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'maxability Function' (pf). See more in help.",
                  call. = FALSE,
                  domain = "R-leem")
        }
        if (gui == "plot") {
          plotquniftspdf(p, min, max, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotquniftspdf(p, min, max, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 min = manipulate::slider(min, max + max, min),
                                 max = manipulate::slider(max, max + max, max)
          )
        }
      }

      point <- qunif(c(p/2, 1 - p/2), min, max)
    } else{
      if(lower.tail == TRUE){
        if (type == "cdf") {
          if (gui == "plot") {
            plotquniflttcdf(p, min, max, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotquniflttcdf(p, min, max, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, max + max, min),
                                   max = manipulate::slider(max, max + max, max)
            )
          }
        }
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'maxability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'maxability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotquniflttpdf(p, min, max, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotquniflttpdf(p, min, max, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, max + max, min),
                                   max = manipulate::slider(max, max + max, max)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotquniflttboth(p, min, max, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotquniflttboth(p, min, max, rounding, mfrow, cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, min + sqrt(min), min),
                                   max = manipulate::slider(max, max + sqrt(max), max)
            )
          }
        }
        point <- qunif(p, min, max)
      } else {
        if (type == "cdf") {
          if (gui == "plot") {
            plotquniflttsf(p, min, max, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotquniflttsf(`1-p`, min, max, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, min + sqrt(min), min),
                                   max = manipulate::slider(max, max + sqrt(max), max)
            )
          }
        }
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'maxability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'maxability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotquniflttpdf(p, min, max, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqunifltfpdf(`1-p`, min, max, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, max + max, min),
                                   max = manipulate::slider(max, max + max, max)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotqunifltfboth(p, min, max, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqunifltfboth(`1-p`, min, max, rounding, mfrow, cex.main),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   min = manipulate::slider(min, max + max, min),
                                   max = manipulate::slider(max, max + max, max)
            )
          }
        }
        point <- qunif(p, min, max, lower.tail = FALSE)
      }
    }

  }
  if (dist == "wilcox") {
    if (!any(names(argaddit) == "m")) {
      m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
      argaddit$m <- as.numeric(m)
    }
    if (!any(names(argaddit) == "n")) {
      n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
      argaddit$n <- as.numeric(n)
    }
    m <- argaddit$m
    n <- argaddit$n
    if (two.sided) {
      if (type == "both") {
        # Plot m title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqwilcoxtsboth(p, m, n, rounding, mfrow, cex.main = cex.main) # aux_quantile.R
        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqwilcoxtsboth(p, m, n, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(m, n + n, m),
                                 n = manipulate::slider(n, n + sqrt(n), n)
          )
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqwilcoxtscdf(p, m, n, rounding)

        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqwilcoxtscdf(p, m, n, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(m, n + n, m),
                                 n = manipulate::slider(n, n + sqrt(n), n)
          )
        }
      }
      if (type == "pdf" || type  == "pf") {
        if(type == "pdf"){
          warning("The type of 'nability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'nability Function' (pf). See more in help.",
                  call. = FALSE,
                  domain = "R-leem")
        }
        if (gui == "plot") {
          plotqwilcoxtspdf(p, m, n, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqwilcoxtspdf(p, m, n, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 m = manipulate::slider(m, n + n, m),
                                 n = manipulate::slider(n, n + sqrt(n), n)
          )
        }
      }

      point <- qwilcox(c(p/2, 1 - p/2), m, n)
    } else{
      if(lower.tail == TRUE){
        if (type == "cdf") {
          if (gui == "plot") {
            plotqwilcoxlttcdf(p, m, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxlttcdf(p, m, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'nability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'nability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqwilcoxlttpdf(p, m, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxlttpdf(p, m, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotqwilcoxlttboth(p, m, n, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxlttboth(p, m, n, rounding, mfrow, cex.main),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        point <- qwilcox(p, m, n)
      } else {
        if (type == "cdf") {
          if (gui == "plot") {
            plotqwilcoxlttsf(p, m, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxlttsf(`1-p`, m, n, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        if (type == "pdf" || type  == "pf") {
          if(type == "pdf"){
            warning("The type of 'nability Density Function' (pdf) doesen't exist for this distribution, are you seeing the 'nability Function' (pf). See more in help.",
                    call. = FALSE,
                    domain = "R-leem")
          }
          if (gui == "plot") {
            plotqwilcoxlttpdf(p, m, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxltfpdf(`1-p`, m, n, rounding),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        if (type == "both") {
          if (gui == "plot") {
            plotqwilcoxltfboth(p, m, n, rounding,mfrow, cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqwilcoxltfboth(`1-p`, m, n, rounding, mfrow, cex.main),
                                   `1-p` = manipulate::slider(0.01, 0.99, p),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   m = manipulate::slider(m, m + sqrt(m), m),
                                   n = manipulate::slider(n, n + sqrt(n), n)
            )
          }
        }
        point <- qwilcox(p, m, n, lower.tail = FALSE)
      }
    }

  }
  if (dist == "signrank") {
    if (!any(names(argaddit) == "n")) {
      n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
      argaddit$n <- as.numeric(n)
    }
    n <- argaddit$n
    if (two.sided) {
      if (type == "both") {
        # Plot size title
        cex.main <- 0.7
        if (gui == "plot") {
          plotqsignranktsboth(p, n, rounding, mfrow, cex.main = cex.main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqsignranktsboth(p, n, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                 p = manipulate::slider(0.01, 0.99, p),
                                 n = manipulate::slider(n, n + 4 * n, n)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }
      if (type == "cdf") {
        if (gui == "plot") {
          plotqsignranktscdf(p, n, rounding)

        }
        if (gui == "rstudio") {
          # Plot
          manipulate::manipulate(plotqsignranktscdf(p, n, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 n = manipulate::slider(n, n + 4 * n, n)
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
          plotqsignranktspdf(p, n, rounding)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotqsignranktspdf(p, n, rounding),
                                 p = manipulate::slider(0.01, 0.99, p),
                                 n = manipulate::slider(n, n + 4 * n, n)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento!")
        }
      }

      point <- qsignrank(c(p/2, 1 - p/2), n)
    } else{
      if(lower.tail == TRUE){
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqsignranklttboth(p, n, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqsignranklttboth(p, n, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 4 * n, n)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            n <- argaddit$n
            plotqsignranklttcdf(p, n, rounding)
          }
          if (gui == "rstudio") {
            n <- argaddit$n
            manipulate::manipulate(plotqsignranklttcdf(p, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 200, n)
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
            plotqsignranklttpdf(p, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqsignranklttpdf(p, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 4 * n, n)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }



        point <- qsignrank(p = p, n = argaddit$n)
      } else {
        if (type == "both") {
          cex.main <- 0.7
          if (gui == "plot") {
            plotqsignrankltfboth(p, n, rounding, mfrow, cex.main = cex.main)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqsignrankltfboth(p, n, rounding, mfrow, cex.main = cex.main), # aux_quantile.R
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 4 * n, n)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }


        if (type == "cdf") {
          if (gui == "plot") {
            plotqsignranklttsf(p, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqsignranklttsf(p, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 200, n)
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
            plotqsignrankltfpdf(p, n, rounding)
          }
          if (gui == "rstudio") {
            manipulate::manipulate(plotqsignrankltfpdf(p, n, rounding),
                                   p = manipulate::slider(0.01, 0.99, p),
                                   n = manipulate::slider(n, n + 4 * n, n)
            )
          }
          if (gui == "tcltk") {
            stop("Em desenvolvimento!")
          }
        }
        point <- qsignrank(p = p, n = argaddit$n, lower.tail=FALSE)
      }
    }
  }
  point <- round(point, rounding)
  return(point)
}

