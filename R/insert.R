#' Insert measures of position in plot
#'
#' Generic function that allows inserting measures of position in plots
#'
#' @param x R object (list) of class leem. Use \code{new_leem()} function.
#' @param type Type of measure of position. The default is \code{type = "mean"}. Other options: \code{"median"}, \code{"mode"} or \code{"all"}.
#' @param lcol Vertical line color type. The default is \code{lpcol = "black"}. This argument must be the same length as the \code{type} argument.
#' @param tcol Text color type. The default is \code{tcol = lcol}.
#' @param acol Arrow color type. The default is \code{acol = lcol}.
#' @param parrow Text and arrow height. The default is \code{parrow = 0.5}. This argument must be the same length as the \code{type} argument.
#' @param larrow Text and arrow length. The default is \code{larrow = 0.6}.
#' @param ptext Distance between lines of text. The default is \code{ptext = 0.06}.
#' @param side Side to insert the text. The default is \code{side = "right"}. This argument must be the same length as the \code{type} argument.
#' @param lwd numeric argument. The vertical line width. The default is  \code{lwd = 2}.
#' @param lwdarrow numeric argument. The arrow width. The default is  \code{lwdarrow = lwd}.
#' @return The result of \code{tabfreq()} is a list. This list has two elements: \code{table} and \code{statistics}. The first is the data frequency table, and the second represents some useful statistics for methods of leem class.
#' @examples
#' # Example 1
#' library(leem)
#' set.seed(10)
#' rnorm(36, 100, 50) |>
#'  new_leem(variable = "continuous") |>
#'  tabfreq() |>
#'  hist() |>
#'  insert(
#'   lcol = "black",
#'   tcol = "purple",
#'   acol = "brown",
#'   parrow = 0.6,
#'   larrow = 0.6,
#'   ptext = 0.4,
#'   side = "left",
#'   lwd = 2,
#'   lwdarrow = 4
#'  )
#' @usage
#' insert(dados, ...)
#'
#' ## Leem S3 method:
#' insert(x, type = "black", lcol, tcol = lcol, acol = lcol, parrow = 0.5,
#'        larrow = 0.2, ptext = 0.6, side = "right", lwd = 2, lwdarrow = lwd)
#'
#' ## Default S3 method:
#' insert(x)
#' @export
insert <- function(x, ...) {
  UseMethod("insert")
}

#' @export
insert.leem <- function(x, type = "mean",
                        lty = 1,
                        lcol = "black",
                        tcol = lcol,
                        acol = lcol,
                        parrow = 0.5,
                        larrow = 0.6,
                        ptext = 0.06,
                        side = "right",
                        lwd = 2,
                        lwdarrow = lwd,
                        ...) {
  # Defensive programming
  if (length(parrow) != length(type)) {
    parrow <- c(0.2, 0.5, 0.9)
    parrow <- parrow[1:length(type)]
  }
  if (type == "all") {
    if (length(lcol) < 3) {
      lcol <- c(lcol[1], lcol[1], lcol[1])
      lcol <- lcol[1:length(type)]
    }
  } else {
    if (length(lcol) != length(type)) {
      lcol <- c(lcol[1], lcol[1], lcol[1])
      lcol <- lcol[1:length(type)]
    }
  }


  if (length(side) != length(type)) {
    side <- c("left", "right", "left")
    side <- side[1:length(type)]
  }
  if (type == "all") {
    # Defensive programming
    if (length(parrow) < 3) {
      parrow <- c(0.2, 0.5, 0.9)
    }
    if (length(lcol) < 3) {
      lcol <- c(lcol[1], lcol[1], lcol[1])
      acol <- lcol
    }
    if (length(side) < 3) {
      side <- c("left", "right", "left")
    }
    # Mean
    abline(v = mean(x),
           lty = lty, lwd = lwd, col = lcol[1])
    # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
    if (side[1] == "right") {
      x0 <- mean(x)  + (par("usr")[2] - par("usr")[1])/2 * larrow
      y0 <- par("usr")[4] * parrow[1]
      arrows(x0 = x0, y0 = y0,
             x1 = mean(x), y1 = y0,
             length = 0.1, col = acol[1], lwd = lwdarrow)

      text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
           labels = gettext("Mean", domain = "R-leem"), col = tcol[1])

      # ?plotmath
      text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
           labels = bquote(bar(X) ==.(format(mean(x, rounding = 4), digits = 4))), col = tcol[1])
    }
    if (side[1] == "left") {
      x0 <- mean(x) - (par("usr")[2] - par("usr")[1])/2 * larrow
      y0 <- par("usr")[4] * parrow[1]
      arrows(x0 = x0, y0 = y0,
             x1 = mean(x), y1 = y0,
             length = 0.1, col = acol[1], lwd = lwdarrow)

      text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
           labels = gettext("Mean", domain = "R-leem"), col = tcol[1])

      # ?plotmath
      text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
           labels = bquote(bar(X) ==.(format(mean(x, rounding = 4), digits = 4))), col = tcol[1])
    }
    # Median
    abline(v = median(x),
           lty = lty, lwd = lwd, col = lcol[2])
    # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
    if (side[2] == "right") {
      x0 <- median(x)  + (par("usr")[2] - par("usr")[1])/2 * larrow
      y0 <- par("usr")[4] * parrow[2]
      arrows(x0 = x0, y0 = y0,
             x1 = median(x), y1 = y0,
             length = 0.1, col = acol[2], lwd = lwdarrow)

      text(x = x0, y =  ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
           labels = gettext("Median", domain = "R-leem"), col = tcol[2])

      # ?plotmath
      text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
           labels = bquote(md(X) ==.(format(median(x, rounding = 4), digits = 4))), col = tcol[2])
    }
    if (side[2] == "left") {
      x0 <- median(x) - (par("usr")[2] - par("usr")[1]) / 2 * larrow
      y0 <- par("usr")[4] * parrow[2]
      arrows(x0 = x0, y0 = y0,
             x1 = median(x), y1 = y0,
             length = 0.1, col = acol[2], lwd = lwdarrow)

      text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
           labels = gettext("Median", domain = "R-leem"), col = tcol[2])

      # ?plotmath
      text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
           labels = bquote(md(X) ==.(format(median(x, rounding = 4), digits = 4))), col = tcol[2])
    }
    # Mode
    if (is.numeric(mfreq(x))) {
      abline(v = mfreq(x),
             lty = lty, lwd = lwd, col = lcol[3])
      # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
      if (side[3] == "right") {
        x0 <- mfreq(x) + (par("usr")[2] - par("usr")[1]) / 2 * larrow
        y0 <- par("usr")[4] * parrow[3]
        arrows(x0 = x0, y0 = y0,
               x1 = mfreq(x), y1 = par("usr")[4] * parrow[3],
               length = 0.1, col = acol[3], lwd = lwdarrow)

        text(x = x0, y =  ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
             labels = gettext("Mode", domain = "R-leem"), col = tcol[3])

        # ?plotmath
        text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
             labels = bquote(mo(X) ==.(format(mfreq(x, rounding = 4), digits = 4))), col = tcol[3])
      }
      if (side[3] == "left") {
        x0 <- mfreq(x) - (par("usr")[2] - par("usr")[1]) / 2 * larrow
        y0 <- par("usr")[4] * parrow[3]
        arrows(x0 = x0, y0 = y0,
               x1 = mfreq(x), y1 = par("usr")[4] * parrow[3],
               length = 0.1, col = acol[3], lwd = lwdarrow)

        text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
             labels = gettext("Mode", domain = "R-leem"), col = tcol[3])

        # ?plotmath
        text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
             labels = bquote(md(X) ==.(format(mfreq(x, rounding = 4), digits = 4))), col = tcol[3])
      }
    } else {
      cat("\nThe data set has no mode!", domain = "R-leem")
    }
  } else {
    # Counter
    i  <- 1
    for(j in type) {
      # Defensive programming
      if (length(parrow) != length(type)) {
        stop("The parrow argument must be the same length as the type argument.",
             call. = FALSE, domain = "R-leem")
      }
      if (length(lcol) != length(type)) {
        stop("The lcol argument must be the same length as the type argument.",
             call. = FALSE, domain = "R-leem")
      }
      if (length(side) != length(type)) {
        stop("The side argument must be the same length as the type argument.",
             call. = FALSE, domain = "R-leem")
      }
      # Measures of position
      if (j == "mean") {
        abline(v = mean(x),
               lty = lty, lwd = lwd, col = lcol[i])
        if (side[i] == "right") {
          x0 <- mean(x) + (par("usr")[2] - par("usr")[1]) / 2 * larrow
          y0 <- par("usr")[4] * parrow[i]
          arrows(x0 = x0, y0 = y0,
                 x1 = mean(x), y1 = par("usr")[4] * parrow[i],
                 length = 0.1, col = acol[i], lwd = lwdarrow)

          text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
               labels = gettext("Mean", domain = "R-leem"), col = tcol[i])

          # ?plotmath
          text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
               labels = bquote(bar(X) ==.(format(mean(x, rounding = 4), digits = 4))), col = tcol[i])
        }
        if (side[i] == "left") {
          x0 <- mean(x) - (par("usr")[2] - par("usr")[1]) / 2 * larrow
          y0 <- par("usr")[4] * parrow[i]
          arrows(x0 = x0, y0 = y0,
                 x1 = mean(x), y1 = y0,
                 length = 0.1, col = acol[i], lwd = lwdarrow)

          text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
               labels = gettext("Mean", domain = "R-leem"), col = tcol[i])

          # ?plotmath
          text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
               labels = bquote(bar(X) ==.(format(mean(x, rounding = 4), digits = 4))), col = tcol[i])
        }
      }
      if (j == "median") {
        abline(v = median(x),
               lty = lty, lwd = lwd, col = lcol[i])
        # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
        if (side[i] == "right") {
          x0 <- median(x) + (par("usr")[2] - par("usr")[1]) / 2 * larrow
          y0 <- par("usr")[4] * parrow[i]
          arrows(x0 = x0, y0 = y0,
                 x1 = median(x), y1 = par("usr")[4] * parrow[i],
                 length = 0.1, col = acol[i], lwd = lwdarrow)

          text(x = x0, y =  ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
               labels = gettext("Median", domain = "R-leem"), col = tcol[i])

          # ?plotmath
          text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
               labels = bquote(md(X) ==.(format(median(x, rounding = 4), digits = 4))), col = tcol[i])
        }
        if (side[i] == "left") {
          x0 <- median(x) - (par("usr")[2] - par("usr")[1]) / 2 * larrow
          y0 <- par("usr")[4] * parrow[i]
          arrows(x0 = x0, y0 = y0,
                 x1 = median(x), y1 = y0,
                 length = 0.1, col = acol[i], lwd = lwdarrow)

          text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
               labels = gettext("Median", domain = "R-leem"), col = tcol[i])

          # ?plotmath
          text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
               labels = bquote(md(X) ==.(format(median(x, rounding = 4), digits = 4))), col = tcol[i])
        }
      }
      if (j == "mode") {
        if (is.numeric(mfreq(x))) {
          abline(v = mfreq(x),
                 lty = lty, lwd = lwd, col = lcol[i])
          # par("usr")[i] => [i] -> c(x1, x2, y1, y2)
          if (side[i] == "right") {
            x0 <- mfreq(x) + (par("usr")[2] - par("usr")[1]) / 2 * larrow
            y0 <- par("usr")[4] * parrow[i]
            arrows(x0 = x0, y0 = y0,
                   x1 = mfreq(x), y1 = y0,
                   length = 0.1, col = acol[i], lwd = lwdarrow)

            text(x = x0, y =  ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
                 labels = gettext("Mode", domain = "R-leem"), col = tcol[i])

            # ?plotmath
            text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
                 labels = bquote(mo(X) ==.(format(mfreq(x, rounding = 4), digits = 4))), col = tcol[i])
          }
          if (side[i] == "left") {
            x0 <- mfreq(x) - (par("usr")[2] - par("usr")[1]) / 2 * larrow
            y0 <- par("usr")[4] * parrow[i]
            arrows(x0 = x0, y0 = y0,
                   x1 = mfreq(x), y1 = y0,
                   length = 0.1, col = acol[i], lwd = lwdarrow)

            text(x = x0, y = ptext * (par("usr")[4] - par("usr")[3]) / 2 + y0,
                 labels = gettext("Mode", domain = "R-leem"), col = tcol[i])

            # ?plotmath
            text(x = x0, y = y0 - ptext * (par("usr")[4] - par("usr")[3]) / 2,
                 labels = bquote(md(X) ==.(format(mfreq(x, rounding = 4), digits = 4))), col = tcol[i])
          }
        } else {
          cat("\nThe data set has no mode!", domain = "R-leem")
        }
      }
      # Update counter
      i <- i + 1
    }
  }





  invisible(x)
}
