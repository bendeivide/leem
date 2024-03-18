#' @import utils write.table
#' @import writexl write_xlsx
#' @import xtable xtable
#' @export
exptable <- function(x, name = NULL, type = "latex", dir = getwd()){
  if (type != "latex" & is.null(name)) stop("Enter the file name using the 'name' argument!", domain = "R-leem")
  if (type == "txt") {
    x1 <- x$table
    if (attr(x, "variable") == "continuous") {
      k <- length(x1[,1])
      for(i in 1:k) {
        x1[i,1] <- paste0("[", x$statistics$lower_lim[i], ", ", x$statistics$upper_lim[i], ")", sep = "")
      }
    }
    utils::write.table(x1, name, sep = "\t", row.names = FALSE, quote = FALSE, eol = "\r\n")
  }
  if (type == "csv") {
    x1 <- x$table
    if (attr(x, "variable") == "continuous") {
      k <- length(x1[,1])
      for(i in 1:k) {
        x1[i,1] <- paste0("[", x$statistics$lower_lim[i], ", ", x$statistics$upper_lim[i], ")", sep = "")
      }
    }
    utils::write.csv2(x1, name, row.names = FALSE, quote = FALSE, eol = "\r\n")
  }
  if (type == "xlsx") {
    x1 <- x$table
    if (attr(x, "variable") == "continuous") {
      k <- length(x1[,1])
      for(i in 1:k) {
        x1[i,1] <- paste0("[", x$statistics$lower_lim[i], ", ", x$statistics$upper_lim[i], ")", sep = "")
      }
    }
    writexl::write_xlsx(x1, name)
  }
  if (type == "latex") {
    x1 <- x$table
    if (attr(x, "variable") == "continuous") {
      k <- length(x1[,1])
      for(i in 1:k) {
        x1[i,1] <- paste0("[", x$statistics$lower_lim[i], ", ", x$statistics$upper_lim[i], ")", sep = "")
      }
    }
    xtable::xtable(x1)
  }
}
