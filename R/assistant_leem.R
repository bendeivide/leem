#' Interactive Assistant for leem Package with Language Selection
#'
#' This function starts by asking the user to select their preferred language.
#' Default language is English. The assistant responds to keywords and provides
#' helpful information about leem package functions.
#'
#' @return NULL (interactive function called for side effects)
#'
#' @examples
#' \dontrun{
#' # Start the interactive assistant (will ask for language)
#' assistant_leem()
#' }
#' @export
assistant_leem <- function(interface = c("tcltk", "console")) {
  interface <- match.arg(interface)

  # Create isolated environment
  env <- new.env()



  if (file.exists(impl_file)) {
    if (interface == "tcltk") {
      # Path to the implementation file
      impl_file <- system.file("extdata", "assistente_tcltk_canvas_e_tktext.R", package = "leem")
      # Source file
      sys.source(impl_file, envir = env)
      # Execute via tcltk
      env$.assistente_tcltk_leem()
    } else {
      # Path to the implementation file
      impl_file <- system.file("extdata", "assistente_leem_console.R", package = "leem")
      # Source file
      sys.source(impl_file, envir = env)
      # Execute via console
      env$.assistente_leem()
    }
  } else {
    # Fallback message
    cat("Interactive assistant file not found.\n")
    cat("Please reinstall the leem package.\n")
  }
}
