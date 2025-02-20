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
    .plotcurve(gui)
  }
  if (gui == "rstudio") {
    # Parameters
    media <- sample(-50:50, 1)
    stdvar <- sample(5:10, 1)

    manipulate::manipulate(plotcurve(Mean, `Standard Deviation`),
                           Mean = manipulate::slider(-50, 50, media),
                           `Standard Deviation` = manipulate::slider(5, 10, stdvar))
  }
}

