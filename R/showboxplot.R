#' @export
showboxplot <- function(horizontal = FALSE,
                        col = rgb(0, 175, 239, maxColorValue = 255)) {
  set.seed(10)
  x <- c(rnorm(30, 100, 2), 105)
  boxplot(x, main = main, col = col,
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
    text(q3, 1.35, bquote(bold(IQR == q[4] - q[2])), lwd = 2, col = "red")
    text(q3, 1.385, "(Interquartile Range)", col = "red")
    # Q2
    text(1.3, 98, bquote(bold(q[2])), col = "red")
    arrows(1.28, 98, 1.21, q2, length = 0.1, lwd = 2, col = "red")
    text(1.3, 97.7, "(25th percentile)", col = "red")
    # Q4
    text(1.3, 101.2, bquote(bold(q[4])), col = "red")
    arrows(1.29, 101.1, 1.21, q4, length = 0.1, lwd = 2, col = "red")
    text(1.3, 101.5, "(75th percentile)", col = "red")
    # Median
    text(0.62, q3, bquote(bold(q[3] == "Median")), col = "red")
    arrows(0.7, q3, 0.78, q3, length = 0.1, lwd = 2, col = "red")
    text(0.62, 100, "(50th percentile)", col = "red", lwd = 2)
    # Q1
    text(0.8, q1, bquote(bold(q[1])), col = "red")
    arrows(0.83, q1, 0.89, q1, length = 0.1, lwd = 2, col = "red")
    text(0.8, 96, bquote(min*"("*x <= Q[1] - coef %*% IQR*")"), col = "red")
    # Q5
    text(0.8, q5, bquote(bold(q[5])), col = "red")
    arrows(0.83, q5, 0.89, q5, length = 0.1, lwd = 2, col = "red")
    text(0.8, 102.5, bquote(max*"("*x <= Q[1] - coef %*% IQR*")"), col = "red")
    # Outlier
    text(0.85, 104, bquote(italic(Outlier)), col = "red")
    arrows(0.9, 104, 0.98, 104.8, length = 0.1, lwd = 2, col = "red")
  } else {
    # IQR
    arrows(1.3, q3, 1.3, q4, angle = 90, length = 0.10, lwd = 2, col = "red")
    arrows(1.3, q3, 1.3, q2, angle = 90, length = 0.10, lwd = 2, col = "red")
    text(1.41, q3, bquote(bold(IQR == q[4] - q[2])), lwd = 2, col = "red")
    text(1.41, 99.8, "(Interquartile Range)", col = "red")
    # Q2
    text(1.3, 98, bquote(bold(q[2])), col = "red")
    arrows(1.28, 98, 1.21, q2, length = 0.1, lwd = 2, col = "red")
    text(1.3, 97.7, "(25th percentile)", col = "red")
    # Q4
    text(1.3, 101.2, bquote(bold(q[4])), col = "red")
    arrows(1.29, 101.1, 1.21, q4, length = 0.1, lwd = 2, col = "red")
    text(1.3, 101.5, "(75th percentile)", col = "red")
    # Median
    text(0.62, q3, bquote(bold(q[3] == "Median")), col = "red")
    arrows(0.7, q3, 0.78, q3, length = 0.1, lwd = 2, col = "red")
    text(0.62, 100, "(50th percentile)", col = "red", lwd = 2)
    # Q1
    text(0.8, q1, bquote(bold(q[1])), col = "red")
    arrows(0.83, q1, 0.89, q1, length = 0.1, lwd = 2, col = "red")
    text(0.8, 96, bquote(min*"("*x <= Q[1] - coef %*% IQR*")"), col = "red")
    # Q5
    text(0.8, q5, bquote(bold(q[5])), col = "red")
    arrows(0.83, q5, 0.89, q5, length = 0.1, lwd = 2, col = "red")
    text(0.8, 102.5, bquote(max*"("*x <= Q[1] - coef %*% IQR*")"), col = "red")
    # Outlier
    text(0.85, 104, bquote(italic(Outlier)), col = "red")
    arrows(0.9, 104, 0.98, 104.8, length = 0.1, lwd = 2, col = "red")
  }


}
