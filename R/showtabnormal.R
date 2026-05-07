#' showtabnormal
#'
#' Displays how to locate values in the standard normal (Z) table.
#' Supports scalar or vector input. For multiple values, one plot
#' is shown per value.
#'
#' @param z Numeric value or vector of Z values.
#' @param ask Logical; if TRUE, prompts before showing next plot.
#'
#' @examples
#' # Single value
#' showtabnormal(1.25)
#'
#' # Multiple values (interactive mode)
#' \dontrun{
#' showtabnormal(c(0.23, 1.45, 2.01))
#' }
#'
#' # Multiple values without interaction (runs all plots)
#' showtabnormal(c(0.23, 1.45), ask = FALSE)
#'
#' @export
showtabnormal <- function(z, ask = interactive()) {

  if (!is.numeric(z)) {
    stop(gettext("Argument 'z' must be numeric."))
  }

  z <- z[!is.na(z)]

  if (length(z) == 0) {
    stop(gettext("No valid values provided."))
  }

  # Positive value
  z_orig <- z
  z <- abs(z)

  for (i in seq_along(z)) {
    if (z_orig[i] != z[i]) {
      message(gettextf("Showing positive value (symmetry) Z = %.4f (%d of %d)", z_orig[i], i, length(z)))
    } else {
      message(gettextf("Showing Z = %.4f (%d of %d)", z_orig[i], i, length(z)))
    }


    .showtabnormal_one(z[i])

    if (ask && i < length(z)) {
      choice <- menu(
        choices = c(
          gettext("Next"),
          gettext("Stop")
        ),
        title = gettext("Show next Z value?")
      )

      if (choice != 1) {
        message(gettext("Interrupted by user."))
        break
      }
    }
  }

  invisible(NULL)
}
