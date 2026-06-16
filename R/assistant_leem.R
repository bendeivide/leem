#' Interactive Assistant for leem Package with Language Selection
#'
#' This function starts by asking the user to select their preferred language.
#' Default language is English. The assistant responds to keywords and provides
#' helpful information about leem package functions.
#'
#' @param interface Character string indicating the interface used by the
#'   assistant. Possible values are `"tcltk"` for a graphical Tcl/Tk interface
#'   and `"console"` for a text-based interaction in the R console.
#'
#' @return NULL (interactive function called for side effects)
#'
#' @examples
#' \dontrun{
#' # Start the interactive assistant (will ask for language)
#' assistant_leem()
#' }
#' @import tcltk
#' @export
assistant_leem <- function(interface = c("tcltk", "console")) {
  interface <- match.arg(interface)

  # Create isolated environment
  env <- new.env()

  # Define file paths BEFORE checking existence
  if (interface == "tcltk") {
    impl_file <- system.file("extdata", "assistente_tcltk_canvas_e_tktext.R", package = "leem")
  } else {
    impl_file <- system.file("extdata", "assistente_leem_console.R", package = "leem")
  }

  # Check if file exists
  if (file.exists(impl_file)) {
    # Source file
    sys.source(impl_file, envir = env)

    # Execute appropriate function
    if (interface == "tcltk") {
      env$.assistente_tcltk_leem()
    } else {
      env$.assistente_leem()
    }
  } else {
    # Fallback message
    cat("Interactive assistant file not found.\n")
    cat("Please reinstall the leem package.\n")
    cat("Missing file:", impl_file, "\n")
  }
}
