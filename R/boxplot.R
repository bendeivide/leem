#' Box plot
#'
#' Produce box-and-whisker plot(s) of leem class object and computes the necessary values for the development of the plot.
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param type character argument. Default is \code{rawdata}. If \code{type = "classes"}, the function returns a boxplot plot for each set of data grouped of classes of \code{x} object.
#' @param details Logical argument. Default is \code{FALSE}, otherwise, in addition to the plot, the measurements necessary for the development of the plot will be displayed on the console.
#' @param horizontal Logical argument indicating if the boxplots should be horizontal; default \code{FALSE} means vertical boxes.
#' @param coef this determines how far the plot whiskers extend out from the box. If \code{coef} is positive, the whiskers extend to the most extreme data point which is no more than \code{coef} times the interquartile range from the box. A value of zero causes the whiskers to extend to the data extremes.
#' @param main Title name. Defaults is \code{NULL}.
#' @param xlab a label for the \code{x} axis. Defaults is \code{NULL}.
#' @param ylab a label for the \code{y} axis. Defaults is \code{NULL}.
#' @param col Character vector. Default \code{col = rgb(0, 175, 239, maxColorValue = 255)}.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' library(leem)
#' # Example 1
#' x <- rnorm(30, 100, 2) |>
#'   new_leem(variable = 2) |>
#'   tabfreq()
#' boxplot(x, details = TRUE)
#' # Example 2
#' boxplot(x, type = "classes")
#'
#' @importFrom graphics boxplot
#' @export
boxplot.leem <- function(x,
                         type = "rawdata",
                         details = FALSE,
                         horizontal = FALSE,
                         coef = 1.5,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         col = rgb(0, 175, 239, maxColorValue = 255),
                         ...) {
  # defensive programming
  #if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
  if (is(x, "leem") & attr(x, "output") == "newleem") x <- tabfreq(x, ...)
  if(is.null(main)) main <- gettext("Box Plots", domain = "R-leem")

  if (attr(x, "variable") == "discrete") {
    # Terminar!
  }
  if (attr(x, "variable") == "continuous") {
    if (type == "rawdata") {
      if (horizontal) {
        boxplot(x$statistics$raw_data, axes = FALSE, main = main, col = col,
                horizontal = TRUE, range = coef, xlab = xlab, ylab = ylab, ...)
        axis(1)
      } else{
        boxplot(x$statistics$raw_data, axes = FALSE, main = main, col = col,
                range = coef, xlab = xlab, ylab = ylab, ...)
        axis(2)
      }
      # Details
      if (details) {
        if (horizontal) {
          abline(v = mean(x, grouped = FALSE), lwd = 2, lty = 2, col = "red")
          points(mean(x, grouped = FALSE), 1, pch = 4, lwd = 2, col = "red")
          legend("topleft", legend = c("Mean"), pch = c(4),
                 col = c("red"), pt.lwd = c(2, 2), bty = "n",
                 lty=c(2))
        } else {
          abline(h = mean(x, grouped = FALSE), lwd = 2, lty = 2, col = "red")
          points(1, mean(x, grouped = FALSE), pch = 4, lwd = 2, col = "red")
          legend("topleft", legend = c("Mean"), pch = c(4),
                 col = c("red"), pt.lwd = c(2, 2), bty = "n",
                 lty=c(2))
        }
        resume <- grDevices::boxplot.stats(x$statistics$raw_data, coef = coef, ...)
        resume$iqr <- resume$stats[4] - resume$stats[2]
        aux <- sort(x$statistics$raw_data)
        resume$min <- aux[aux >= (resume$stats[1] - coef * resume$iqr)][1]
        aux2 <- sort(x$statistics$raw_data, decreasing = TRUE)
        resume$max <- aux2[aux2 <= (resume$stats[1] + coef * resume$iqr)][1]
        resume$mean <- mean(x, grouped = FALSE)
        return(resume)
      }
    }
    if (type == "classes") {
      # n classes
      nclass <- x$statistics$nclasses
      # n sample
      nsample <- x$statistics$nsample
      # Limits
      #li <- x$statistics$lower_lim
      ls <- x$statistics$upper_lim
      # data
      aux2 <- sort(x$statistics$raw_data)
      #
      aux <- data.frame(classes = rep(paste("C", nclass, sep = ""), nsample), data = aux2)
      for (i in (nclass - 1):1) {
         aux$classes[which(aux2 <= ls[i])] <- paste("C", i, sep = "")
      }

      if (horizontal) {
        # PLot
        boxplot(aux$data ~ aux$classes, axes = FALSE, main = main, col = col,
                xlab = xlab, ylab = ylab, horizontal = TRUE, ...)
        # Axis
        axis(1)
        if (is.null(xlab)) {
          title(ylab = "Classes")
          axis(2, at = 1:nclass, labels = paste("C", 1:nclass, sep = ""), las = 2)
        } else {
          axis(2, at = 1:nclass, labels = paste("C", 1:nclass, sep = ""), las = 2)
        }
      } else {
        # PLot
        boxplot(aux$data ~ aux$classes, axes = FALSE, main = main, col = col,
                xlab = xlab, ylab = ylab, ...)

        # Axis
        axis(2)
        if (is.null(xlab)) {
          title(xlab = "Classes")
          axis(1, at = 1:nclass, labels = paste("C", 1:nclass, sep = ""))
        } else {
          axis(1, at = 1:nclass, labels = paste("C", 1:nclass, sep = ""))
        }
      }


      # Details
      if (details) {
        if (horizontal) {
          points(x$table$PM, 1:nclass, pch = 4, lwd = 2, col = "red")
          abline(v = mean(x, grouped = FALSE), lwd = 2, lty = 2, col = "yellow3")
          legend("topleft", legend = c("Midpoint", "Mean"), pch = c(4, NA),
                 col = c("red", "yellow3"), pt.lwd = c(2, 2), bty = "n",
                 lty=c(NA, 2))
        } else {
          points(1:nclass, x$table$PM, pch = 4, lwd = 2, col = "red")
          abline(h = mean(x, grouped = FALSE), lwd = 2, lty = 2, col = "yellow3")
          legend("topleft", legend = c("Midpoint", "Mean"), pch = c(4, NA),
                 col = c("red", "yellow3"), pt.lwd = c(2, 2), bty = "n",
                 lty=c(NA, 2))
        }
      }
    }
    # Does not make sense!!!
    # if (type == "midpoint") {
    #   midpoint <- rep(x$table$PM, x$table$Fi)
    #   if (horizontal) {
    #     boxplot(midpoint, axes = FALSE, main = main, col = col,
    #             horizontal = TRUE, range = coef, ...)
    #     axis(1)
    #   } else{
    #     boxplot(midpoint, axes = FALSE, main = main, col = col, range = coef, ...)
    #     axis(2)
    #   }
    #   # Details
    #   if (details) {
    #     if (horizontal) {
    #       abline(v = mean(x), lwd = 2, lty = 2, col = "red")
    #       points(mean(x), 1, pch = 4, lwd = 2, col = "red")
    #       legend("topleft", legend = c("Mean"), pch = c(4),
    #              col = c("red"), pt.lwd = c(2, 2), bty = "n",
    #              lty=c(2))
    #     } else {
    #       abline(h = mean(x), lwd = 2, lty = 2, col = "red")
    #       points(1, mean(x), pch = 4, lwd = 2, col = "red")
    #       legend("topleft", legend = c("Mean"), pch = c(4),
    #              col = c("red"), pt.lwd = c(2, 2), bty = "n",
    #              lty=c(2))
    #     }
    #     resume <- grDevices::boxplot.stats(x$statistics$raw_data, coef = coef, ...)
    #     resume$iqr <- resume$stats[4] - resume$stats[2]
    #     aux <- sort(x$statistics$raw_data)
    #     resume$min <- aux[aux >= (resume$stats[1] - coef * resume$iqr)][1]
    #     aux2 <- sort(x$statistics$raw_data, decreasing = TRUE)
    #     resume$max <- aux2[aux2 <= (resume$stats[1] + coef * resume$iqr)][1]
    #     resume$mean <- mean(x, grouped = FALSE)
    #     return(resume)
    #   }
    # }
  }
}
