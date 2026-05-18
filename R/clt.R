#' Central Limit Theorem Simulation
#'
#' Simulates the Central Limit Theorem (CLT) using repeated random
#' sampling from different probability distributions.
#'
#' The function was designed for educational purposes, allowing users
#' to visualize how the distribution of sample means approaches the
#' Normal distribution as the sample size increases.
#'
#' @param n Integer indicating the sample size.
#'
#' @param nsim Integer indicating the number of simulations.
#'
#' @param dist Character string specifying the population distribution.
#' Possible values are:
#' \itemize{
#'   \item \code{"normal"}
#'   \item \code{"uniform"}
#'   \item \code{"exponential"}
#'   \item \code{"binomial"}
#'   \item \code{"poisson"}
#' }
#'
#' @param mean Numeric value indicating the mean of the Normal
#' distribution.
#'
#' @param sd Numeric value indicating the standard deviation of the
#' Normal distribution.
#'
#' @param lambda Numeric value indicating the rate parameter of the
#' Exponential distribution.
#'
#' @param prob Numeric value indicating the probability of success
#' for the Binomial distribution.
#'
#' @param size Integer indicating the number of trials for the
#' Binomial distribution.
#'
#' @param lambda.pois Numeric value indicating the parameter of the
#' Poisson distribution.
#'
#' @param col Character string specifying the histogram color.
#'
#' @param border Character string specifying the histogram border color.
#'
#' @param breaks Number of histogram classes.
#'
#' @param seed Optional random seed for reproducibility.
#'
#' @param main Optional character string specifying the graph title.
#'
#' @param dec Character string specifying the decimal separator.
#' Possible values are \code{"."} or \code{","}.
#'
#' @details
#' The function generates repeated random samples from a specified
#' probability distribution and computes their sample means.
#'
#' According to the Central Limit Theorem, the distribution of the
#' sample means approaches a Normal distribution as the sample size
#' increases, regardless of the original population distribution.
#'
#' The histogram of the simulated sample means is displayed together
#' with the corresponding Normal approximation.
#'
#' @return
#' Returns invisibly a list containing:
#' \itemize{
#'   \item \code{sample.means}: simulated sample means.
#'   \item \code{mean}: empirical mean.
#'   \item \code{sd}: empirical standard deviation.
#' }
#'
#' @author
#' Ben Dêivide
#'
#' @examples
#' # Exponential population
#' TCL(
#'   n = 30,
#'   nsim = 5000,
#'   dist = "exponential"
#' )
#'
#' # Binomial population
#' TCL(
#'   n = 20,
#'   nsim = 3000,
#'   dist = "binomial",
#'   size = 10,
#'   prob = 0.3
#' )
#' @export
TCL <- function(n = 30,
                nsim = 1000,
                dist = c(
                  "normal",
                  "uniform",
                  "exponential",
                  "binomial",
                  "poisson"
                ),
                mean = 0,
                sd = 1,
                lambda = 1,
                prob = 0.5,
                size = 10,
                lambda.pois = 5,
                col = "lightblue",
                border = "white",
                breaks = "FD",
                seed = NULL,
                main = NULL,
                dec = getOption("OutDec")) {

  # Match distribution argument
  dist <- match.arg(dist)

  # Preserve current decimal separator
  old.dec <- getOption("OutDec")

  # Restore option on exit
  on.exit(options(OutDec = old.dec))

  # Set decimal separator
  options(OutDec = dec)

  # Optional seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate sample means
  sample.means <- replicate(nsim, {

    x <- switch(

      dist,

      normal = rnorm(
        n,
        mean = mean,
        sd = sd
      ),

      uniform = runif(n),

      exponential = rexp(
        n,
        rate = lambda
      ),

      binomial = rbinom(
        n,
        size = size,
        prob = prob
      ),

      poisson = rpois(
        n,
        lambda = lambda.pois
      )

    )

    mean(x)

  })

  # Dynamic title
  if (is.null(main)) {

    main <- gettext(
      "Central Limit Theorem",
      domain = "R-leem"
    )

  }

  # Distribution label
  dist.label <- switch(

    dist,

    normal = gettext(
      "Normal distribution",
      domain = "R-leem"
    ),

    uniform = gettext(
      "Uniform distribution",
      domain = "R-leem"
    ),

    exponential = gettext(
      "Exponential distribution",
      domain = "R-leem"
    ),

    binomial = gettext(
      "Binomial distribution",
      domain = "R-leem"
    ),

    poisson = gettext(
      "Poisson distribution",
      domain = "R-leem"
    )

  )

  # Histogram
  hist(
    sample.means,
    probability = TRUE,
    col = col,
    border = border,
    breaks = breaks,
    main = main,
    xlab = gettext(
      "Sample means",
      domain = "R-leem"
    ),
    ylab = gettext(
      "Density",
      domain = "R-leem"
    ),
    cex.main = 1.4,
    cex.lab = 1.2,
    cex.axis = 1.1
  )

  # Normal approximation
  curve(
    dnorm(
      x,
      mean = mean(sample.means),
      sd = sd(sample.means)
    ),
    add = TRUE,
    lwd = 3,
    col = "red"
  )

  # Legend
  legend(
    "topleft",
    title = dist.label,
    title.font = 2,
    x.intersp = 0.4,
    seg.len = 0.5,
    legend = c(

      paste0(

        gettext(
          "Simulated means",
          domain = "R-leem"
        )
      ),

      gettext(
        "Normal approximation",
        domain = "R-leem"
      )
    ),

    fill = c(col, NA),

    border = c(border, NA),

    lty = c(NA, 1),

    lwd = c(NA, 3),

    col = c(NA, "red"),

    bty = "n",

    cex = 1
  )

  ###################
  # Shapiro-Wilk test
  ###################
  # Shapiro-Wilk test
  shapiro.result <- shapiro.test(sample.means)

  # Extract statistics
  W <- round(shapiro.result$statistic, 4)

  p.value <- round(shapiro.result$p.value, 4)


  legend(

    "topright",

    inset = c(0.02, 0),

    title = gettext(
      "Shapiro-Wilk test",
      domain = "R-leem"
    ),

    title.font = 2,

    legend = c(

      paste0(
        "W = ",
        format(W, decimal.mark = dec)
      ),

      paste0(
        "p-value = ",
        format(p.value, decimal.mark = dec)
      )

    ),

    bty = "o",
    bg = "lightblue",
    cex = 0.9,

    text.col = "#404040"

  )

  # Return object
  invisible(

    list(

      sample.means = sample.means,

      mean = mean(sample.means),

      sd = sd(sample.means)

    )

  )

}
