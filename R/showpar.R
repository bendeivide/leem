#' Interpretation of location and scale parameters
#'
#' \code{showpar} Function that exemplifies the interpretation of location and scale parameters
#'
#' @details The result of the \code{showpar()} call will interactively present a plot of the normal distribution showing the behavior of the location and scale parameters via RStudio. For \code{showpar(gui = "tcltk")} the result will be displayed in a tcltk interface.
#'
#' @param gui character argument. The options are: \code{"rstudio"} (default) and \code{"tcltk"}.

#' @return \code{showpar} returns an interactive plot.
#'
#' @examples
#' # Loading package
#' library(leem)
#' \dontrun{
#' showpar()
#' }
#'
#' @export
showpar <- function(gui = "rstudio") {
  # Parameters
  media <- sample(-50:50, 1)
  stdvar <- sample(5:10, 1)

  # Plot
  plotcurve <- function(mu, sigma = 5) {
    x <- seq(mu - 4 * sigma, mu + 4 * sigma, by = 0.01)
    fx <- dnorm(x, mean = mu, sd = sigma)


    curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
          xlim = c(-70, 70), ylim = c(0, 0.1), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray"),
          main = gettext("Normal distribution", domain = "R-leem"))

    polygon(c(x, rev(x)),
            c(fx, rep(0, length(fx))),
            col="red")
    abline(v=mu, lty=2)
    legend("topleft", bty="n", fill="red",
           legend=substitute(mu == media~","~sigma == vari, list(media = mu, vari = sigma)))
  }

  if (gui == "tcltk") {
    # Environment of package
    envleem <- new.env(parent = base::emptyenv())
    # Global variables
    leemget <- function(x) {
      get(x, envir= envleem, inherits=FALSE )
    }
    leemset <- function(x, value) {
      assign(x, value, envir= envleem)
    }
    globalvariables <- function(x, value) {
      assign(x, value, envir= .GlobalEnv)
    }

    # Mean variable
    tk_mu <- leemset("tk_mu", tclVar(media))
    tk_var <- leemset("tk_var", tclVar(stdvar))
    sapply(c("tk_mu", "tk_var"),
           function(x) globalvariables(x, leemget(x)))

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

    # Omit warnings
    oldw <- getOption("warn")
    options(warn = -1)


    # Plot
    tkplot <- tkRplotR::tkRplot(W = tkplot, width = 500,
                                height = 500, fun = function(...) {
                                  media <- as.numeric(tclvalue(tk_mu))
                                  vari <- as.numeric(tclvalue(tk_var))
                                  plotcurve(mu = media, sigma = vari)
                                })
    # Omit warnigns
    options(warn = oldw)

    # Scale
    s01 <- tcltk::tkscale(
      tkplot,
      from = -50,
      to = 50,
      label = 'mean',
      variable = tk_mu,
      showvalue = TRUE,
      resolution = 1,
      repeatdelay = 200,
      repeatinterval = 100,
      orient = "hor"
    )
    s02 <- tcltk::tkscale(
      tkplot,
      from = 5,
      to = 10,
      label = 'Standard Deviation',
      variable = tk_var,
      showvalue = TRUE,
      resolution = 1,
      repeatdelay = 200,
      repeatinterval = 100,
      orient = "hor"
    )
    tkpack(s01, s02,
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
        if (exists("tk_mu", envir = .GlobalEnv)) {
          rm("tk_mu", envir = .GlobalEnv)
        }
        tkdestroy(tkplot)
      }
    })
  }
  if (gui == "rstudio") {
    manipulate::manipulate(plotcurve(Mean, `Standard Deviation`),
                           Mean = manipulate::slider(-50, 50, media),
                           `Standard Deviation` = manipulate::slider(5, 10, stdvar))
  }
}

