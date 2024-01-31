#' Undertanding a box plot
#'
#' Detailing of a box plot, showing the main information contained in this type of graph
#'
#' @param horizontal Logical argument indicating if the boxplots should be horizontal; default \code{FALSE} means vertical boxes.
#' @param col Character vector. Default \code{col = rgb(0, 175, 239, maxColorValue = 255)}.
#'
#' @examples
#' library(leem)
#' # Example 1
#' showboxplot()
#'
#' @export
showboxplot <- function(horizontal = FALSE,
                        col = rgb(0, 175, 239, maxColorValue = 255)) {
  set.seed(10)
  x <- c(rnorm(30, 100, 2), 105)
  boxplot(x, col = col,
          horizontal = horizontal, lwd = 2, axes = FALSE)
  # points
  npoints <- grDevices::boxplot.stats(x)
  iqr <- npoints$stats[4] - npoints$stats[2]
  q1 <- npoints$stats[1]
  q2 <- npoints$stats[2]
  q3 <- npoints$stats[3]
  q4 <- npoints$stats[4]
  q5 <- npoints$stats[5]

  if (horizontal) {
    # IQR
    arrows(q3, 1.3, q4, 1.3, angle = 90, length = 0.10, lwd = 2, col = "red")
    arrows(q3, 1.3, q2, 1.3, angle = 90, length = 0.10, lwd = 2, col = "red")
    text(q3, 1.35, bquote(bold(IQR == q[3] - q[1])), lwd = 2, col = "red")
    iqrtext <- gettext("(Interquartile Range)", domain = "R-leem")
    text(q3, 1.4, iqrtext, col = "red")
    # Q2
    text(98, 1.3, bquote(bold(q[1])), col = "red")
    arrows(98, 1.27 , q2, 1.21, length = 0.1, lwd = 2, col = "red")
    text(97.7, 1.33, adj = 1,
         gettext("(25th percentile)", domain = "R-leem"), col = "red")
    # Q4
    text(101.2, 1.3, bquote(bold(q[3])), col = "red")
    arrows(101.1, 1.27, q4, 1.21, length = 0.1, lwd = 2, col = "red")
    text(101.5, 1.33, adj = 0,
         gettext("(75th percentile)", domain = "R-leem"), col = "red")
    # Median
    medtext <- gettext("Median", domain = "R-leem")
    text(q3, 0.63, substitute(bold(q[2] == medtext), list(medtext = medtext)), col = "red")
    arrows(q3, 0.7, q3, 0.78, length = 0.1, lwd = 2, col = "red")
    text(q3, 0.68, gettext("(50th percentile)", domain = "R-leem"), col = "red", lwd = 2)
    # Q1
    lwl <- gettext("Lower whisker limit", domain = "R-leem")
    text(q1, 0.8, substitute(bold(lwl), list(lwl = lwl)), col = "red",
         adj = 0.1)
    arrows(q1, 0.83, q1, 0.89, length = 0.1, lwd = 2, col = "red")
    text(96.8, 0.74, bquote(min("("*x*")", x >= q[1] - coef %*% IQR)), col = "red")
    # Q5
    uwl <- gettext("Upper whisker limit", domain = "R-leem")
    text(q5, 0.8, substitute(bold(uwl), list(uwl = uwl)), col = "red")
    arrows(q5, 0.83, q5, 0.89, length = 0.1, lwd = 2, col = "red")
    text(102.5, 0.74, bquote(max("("*x*")", x <= q[3] + coef %*% IQR)), col = "red")
    # Outlier
    outtext <- gettext("Outlier", domain = "R-leem")
    text(103.4, 1.2, substitute(italic(Outlier), list(Outlier = outtext)), col = "red")
    arrows(104, 1.15, 104.8, 1.03, length = 0.1, lwd = 2, col = "red")
  } else {
    # IQR
    arrows(1.3, q3, 1.3, q4, angle = 90, length = 0.10, lwd = 2, col = "red")
    arrows(1.3, q3, 1.3, q2, angle = 90, length = 0.10, lwd = 2, col = "red")
    text(1.41, q3, bquote(bold(IQR == q[3] - q[1])), lwd = 2, col = "red")
    text(1.43, 100.5, gettext("(Interquartile \n Range)", domain = "R-leem"), col = "red")
    # Q2
    text(1.3, 98, bquote(bold(q[1])), col = "red")
    arrows(1.27, 98, 1.21, q2, length = 0.1, lwd = 2, col = "red")
    text(1.3, 97.5, gettext("(25th percentile)", domain = "R-leem"), col = "red")
    # Q4
    text(1.3, 101.2, bquote(bold(q[3])), col = "red")
    arrows(1.28, 101, 1.21, q4, length = 0.1, lwd = 2, col = "red")
    text(1.3, 101.7, gettext("(75th percentile)", domain = "R-leem"), col = "red")
    # Median
    medtext <- gettext("Median", domain = "R-leem")
    text(0.6, q3, substitute(bold(q[2] == medtext), list(medtext = medtext)), col = "red")
    arrows(0.72, q3, 0.78, q3, length = 0.1, lwd = 2, col = "red")
    text(0.62, 100, gettext("(50th percentile)", domain = "R-leem"), col = "red", lwd = 2)
    # Q1
    lwl <- gettext("Lower whisker limit", domain = "R-leem")
    text(0.8, q1, substitute(bold(lwl), list(lwl = lwl)),
         col = "red", adj = 1)
    arrows(0.83, q1, 0.89, q1, length = 0.1, lwd = 2, col = "red")
    text(0.65, 96.5, bquote(min("("*x*")", x >= q[1] - coef %*% IQR)), col = "red")
    # Q5
    uwl <- gettext("Upper whisker limit", domain = "R-leem")
    text(0.8, q5, substitute(bold(uwl), list(uwl = uwl)),
         col = "red", adj = 1)
    arrows(0.83, q5, 0.89, q5, length = 0.1, lwd = 2, col = "red")
    text(0.65, 102.9, bquote(max("("*x*")", x <= q[3] + coef %*% IQR)), col = "red")
    # Outlier
    outtext <- gettext("Outlier", domain = "R-leem")
    text(1.2, 103.9, substitute(italic(Outlier), list(Outlier = outtext)), col = "red")
    arrows(1.2, 104.2, 1.02, 104.9, length = 0.1, lwd = 2, col = "red")
  }


}
