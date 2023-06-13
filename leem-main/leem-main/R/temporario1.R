#' Constructor of object of leem class
#'
#' @param x R object (vector as data structure).
#' @param variable Type of data. If \code{discrete} (default), the data are categorical (numeric or not). If continuous, the data are numeric.
#' @return The \code{variable} argument also allows using \code{variable = 1} for categorical variable and \code{variable = 2} for continuous variable.
#' @examples
#' # Example 1
#' library(leem)
#' x <- rbinom(36, 10, 0.6)
#' new_leem(x, variable = 1)
#'
#' # Example 2 (Pipe operator)
#' rnorm(36, 100, 4) |> new_leem(variable = 2)
#'
#' @export
new_leem <- function(x = vector(), variable = "discrete") {
  stopifnot("The x argument should be vector!" = is.vector(x))
  if (variable == 1) variable <- "discrete"
  if (variable == 2) variable <- "continuous"
  if(!any(variable == c("discrete", "continuous"))) stop("The variable argument must be 'discrete' or 'continuous'.")
  structure(x, class = "leem", variable = variable, output = "newleem")
}


#' # Grafico de hastes ou bastao
#' #' @export
#' stickplot <- function(x,
#'                       bg = TRUE,
#'                       main = NULL,
#'                       xlab = NULL,
#'                       ylab = NULL,
#'                       panel.first = grid(col = "white"),
#'                       bgcol = "gray",
#'                       bgborder = NA,
#'                       lcol = "black",
#'                       pcol = lcol,
#'                       pty = 19,
#'                       pwd = 3,
#'                       lty = 1,
#'                       lwd = 2,
#'                       ...) {
#'   if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!")
#'   if (attr(x, "variable") == "continuous") stop("The function only applies to discrete variables.", call. = FALSE, domain = "R-leem")
#'   if (class(x) == "leem" & is.null(attr(x, "output"))) x <- tabfreq(x)
#'   if (attr(x, "variable") == "discrete") {
#'     numchar <- is.numeric(x$table$Groups)
#'     if (numchar) {
#'       xmin <- x$table$Groups[1]
#'       xmax <- max(x$table$Groups)
#'       xvar <- x$table$Groups
#'       yvar <- x$table$Fi
#'
#'
#'
#'       # Limiares
#'       xlim <- c(xmin - 0.5, xmax + 0.5)
#'       ylim <- c(0, 1.2 * max(yvar))
#'
#'       # Area de plotagem
#'       plot.new()
#'       plot.window(xlim, ylim)
#'
#'       # Labels
#'       if (is.null(main)) {
#'         main <- gettext("Stick plot", domain = "R-leem")
#'       }
#'       if (is.null(xlab)) {
#'         xlab <- gettext("Groups", domain = "R-leem")
#'       }
#'       if (is.null(ylab)) {
#'         ylab <- gettext("Frequency", domain = "R-leem")
#'       }
#'
#'       title(main = main, xlab = xlab, ylab = ylab)
#'
#'       if(bg) {
#'         rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
#'                bgcol, border = bgborder)
#'       }
#'
#'       # Eixos
#'       axis(1, at = xvar)
#'       axis(2)
#'
#'       # Grid
#'       panel.first
#'
#'       # Inserindo hastes
#'       lines(x$table$Groups, x$table$Fi, type = "h",
#'             lty = lty, lwd = lwd, col = lcol)
#'       points(x$table$Groups, x$table$Fi, pch  = pty, lwd = pwd,
#'              col = pcol)
#'     } else {
#'       stop("Em desenvolvimento!")
#'     }
#'   }
#'   invisible(x)
#' }


# Grafico de barras
# rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
#   plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n")
#   text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# }
# rotate_x(mtcars, 'mpg', row.names(mtcars), 45)










