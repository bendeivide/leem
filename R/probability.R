#' Probability Computation and Graphical Visualization
#'
#' Computes probabilities associated with several probability distributions
#' and optionally produces interactive graphical visualizations using
#' base graphics, Tcl/Tk, RStudio manipulate, or Shiny interfaces.
#'
#' The function was designed for educational purposes, combining analytical
#' computation with dynamic graphical interpretation of probability models.
#'
#' @param q quantile. The \code{q} argument can have length 1 or 2. See Details.
#'
#' @param dist distribution to use. The default is \code{'normal'}. Options: \code{'normal'}, \code{'t-student'}, \code{'chisq'}, \code{'f'}, ...
#'
#' @param lower.tail Logical value indicating the probability region:
#'   \itemize{
#'     \item \code{TRUE}: computes lower tail probability.
#'     \item \code{FALSE}: computes upper tail probability.
#'     \item \code{NULL}: computes the probability density/mass function.
#'   }
#'
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#'
#' @param porcentage If \code{TRUE}, probabilities are multiplied by 100. If \code{lower.tail = NULL}, this argument is ignored.

#'
#' @param gui Character string specifying the graphical interface used.
#'   Possible values are:
#'   \itemize{
#'     \item \code{"none"}: analytical computation only.
#'     \item \code{"plot"}: static base R graphics.
#'     \item \code{"tcltk"}: interactive Tcl/Tk graphical interface.
#'     \item \code{"rstudio"}: interactive manipulate interface in RStudio.
#'     \item \code{"shiny"}: interactive Shiny application.
#'   }
#'
#' @param main defalt is \code{NULL}; it represents title of plot.
#'
#' @param browser.shiny Logical indicating whether the Shiny application
#'   should be opened in a browser. The default behavior follows:
#'   \code{getOption("shiny.launch.browser", interactive())}.
#'
#' @param plot.type Character string specifying the graphical representation:
#'   \itemize{
#'     \item \code{"pdf"}: probability density function.
#'     \item \code{"cdf"}: cumulative distribution function.
#'     \item \code{"pmf"}: probability mass function.
#'   }
#' @param dec Character string specifying the decimal separator used
#'   in graphical annotations. Possible values are:
#'   \code{"."} or \code{","}.
#'
#' @param long.segment Logical. If \code{TRUE}, auxiliary graphical
#'   segments are drawn with extended length.
#'
#' @param col Character string specifying the primary color used
#'   in the graphical visualization.
#'
#' @param col2 Character string specifying the secondary color used
#'   in the graphical visualization.
#'
#' @param lty Integer or character specifying the line type used
#'   in auxiliary graphical elements.
#'
#' @param ... additional arguments according to the chosen distribution.
#'
#' @details The argument that can have length 2, when we use the functions that give us the probability regions, given by: \code{\%<X<\%}, \code{\%<=X<\%}, \code{\%<X<=\%}, \code{\%<=X<=\%}, \code{\%>X>\%}, \code{\%>X=>\%}, \code{\%>X=>\%} and \code{\%>=X=>\%}.
#' The additional arguments represent the parameters of the distributions, that is:
#' - If \code{dist = "normal"} (Default); the additional arguments are: \code{mean} (\eqn{\mu}) and \code{sd} (\eqn{\sigma}). The PDF is given by:
#' \deqn{\displaystyle{f_X(x; \mu, \sigma) = \frac {1}{\sqrt {2\pi \sigma ^{2}}}}e^{-{\frac {(x-\mu )^{2}}{2\sigma ^{2}}}}, \quad x \in \mathbb{R},~ \mu \in \mathbb{R},~\sigma^2 > 0;}
#'
#' - If \code{dist = "t-student"}; the additional argument is: \code{df} (\eqn{\nu}). The PDF is given by:
#' \deqn{\displaystyle{f_X(x; \nu) = \frac {\Gamma \left({\frac {\ \nu +1\ }{2}}\right)}{{\sqrt {\pi \ \nu \ }}\ \Gamma \left({\frac {\nu }{\ 2\ }}\right)}}\left(\ 1+{\frac {~x^{2}\ }{\nu }}\ \right)^{-{\frac {\ \nu +1\ }{2}}}, \quad x \in \mathbb{R},~\nu > 1;}
#'
#' - If \code{dist = "chisq"}; the additional argument is: \code{df} (\eqn{\nu}). The PDF is given by:
#' \deqn{\displaystyle{f_X(x; \nu) = \frac {1}{2^{k/2}\Gamma (k/2)}}\;x^{k/2-1}e^{-x/2}, \quad x > 0,~\nu > 0;}
#'
#' - If \code{dist = "f"}; the additional argument is: \code{df1} (\eqn{\nu_1}) and \code{df2} (\eqn{\nu_2}). The PDF is given by:
#' \deqn{f_X(x; \nu_1, \nu_2) = {\displaystyle {\frac {\sqrt {\frac {(\nu_{1}x)^{\nu_{1}}\nu_{2}^{\nu_{2}}}{(\nu_{1}x+\nu_{2})^{\nu_{1}+\nu_{2}}}}}{x\,\mathrm {B} \!\left({\frac {\nu_{1}}{2}},{\frac {\nu_{2}}{2}}\right),}}\!}, \quad x > 0,~\nu_1,\nu_2 > 0;}
#' where, \eqn{x > 0}, \eqn{\nu_1,~\nu_2} > 0, and \eqn{B} is the beta function.
#'
#' - If \code{dist = "binomial"}; the additional arguments are: \code{size} (\eqn{n}) and \code{prob} (\eqn{p}). The PMF is given by:
#' \deqn{\displaystyle{P(X = x) = \binom{n}{x} p^x (1-p)^{n-x}}, \quad x = 0,1,\dots,n,~0 < p < 1;}
#'
#' - If \code{dist = "poisson"}; the additional argument is: \code{lambda} (\eqn{\lambda}). The PMF is given by:
#' \deqn{\displaystyle{P(X = x) = \frac{\lambda^x e^{-\lambda}}{x!}}, \quad x = 0,1,2,\dots,~\lambda > 0;}
#'
#' - If \code{dist = "geom"}; the additional argument is: \code{prob} (\eqn{p}). The PMF is given by:
#' \deqn{\displaystyle{P(X = x) = (1-p)^x p}, \quad x = 0,1,2,\dots,~0 < p < 1;}
#'
#' - If \code{dist = "nbinom"}; the additional arguments are: \code{size} (\eqn{r}) and \code{prob} (\eqn{p}). The PMF is given by:
#' \deqn{\displaystyle{P(X = x) = \binom{x+r-1}{x} (1-p)^x p^r}, \quad x = 0,1,2,\dots,~r > 0,~0 < p < 1;}
#'
#' - If \code{dist = "hyper"}; the additional arguments are: \code{m} (\eqn{M}), \code{n} (\eqn{N-M}) and \code{k} (\eqn{n}). The PMF is given by:
#' \deqn{\displaystyle{P(X = x) = \frac{\binom{M}{x}\binom{N-M}{n-x}}{\binom{N}{n}}}, \quad \max(0, n-(N-M)) \le x \le \min(n, M);}#'
#' The \code{ncp} parameter (\eqn{\lambda \in \mathbb{R}}) represents the noncentrality parameter. The PDFs presented graphically do not take this parameter into account. However, to reinforce the importance of this parameter
#' in the three distributions (Student's t-distribution, F-distribution and Chi-squared distribution), especially when studying hypothesis testing, we present their distributions taking into account the \code{ncp} parameter, as follows:
#' - The PDF for the noncentral t-distribution with \eqn{\nu > 0} degrees of freedom and noncentrality parameter \eqn{\lambda} is based on the \link[stats]{pt} function. If \eqn{Z}
#' is a standard normal random variable, and \eqn{V} is a chi-squared distribution random variable with \eqn{\nu} degrees of freedom that is independent of \eqn{Z}, then \eqn{T = (Z + \lambda) / \sqrt{V / \nu}} is
#' a noncentral t-distributed random variable with \eqn{\nu} degrees of freedom and noncentrality parameter \eqn{\lambda}. If \eqn{\lambda = 0}, the PDF reduces to the probability density function of the Student's t-distribution. However, it is worth noting that the parameter \eqn{\lambda \in \mathbb{R}}.
#'
#' @return \code{P} returns the probability and its graphical representation. The result can be given as a percentage or not.
#'
#' @examples
#' # Loading package
#' library(leem)
#' # Example 1 - Student's t distribution
#' \dontrun{
#' P(q = 2, dist = "t-student", df = 10)
#' P(q = 2, dist = "t-student", df = 10, gui = 'rstudio')
#' P(q = 2, dist = "t-student", df = 10, gui = 'tcltk')
#' P(-1 %<X<% 1, dist = "t-student", df = 10)
#' }
#' # Example 2 - Normal distribution
#' P(-2,  dist = "normal", mean = 3, sd = 2,
#'   main = expression(f(x) == (1 / sqrt(n * sigma^2)) *
#'   exp(-1/2 * (x - mu)^2/sigma^2)))
#' @import manipulate
#' @import tkRplotR
#' @importFrom "stats" "dbeta" "dbinom" "dcauchy" "dchisq" "dexp" "df" "dgamma" "dgeom" "dhyper" "dlnorm" "dlogis" "dnbinom" "dnorm" "dpois" "dsignrank" "dt" "dunif" "dweibull" "dwilcox" "pbeta" "pbinom" "pcauchy" "pchisq" "pexp" "pf" "pgamma" "pgeom" "phyper" "plnorm" "plogis" "pnbinom" "pnorm" "ppois" "psignrank" "pt" "ptukey" "punif" "pweibull" "pwilcox" "qbeta" "qbinom" "qcauchy" "qchisq" "qexp" "qf" "qgamma" "qgeom" "qhyper" "qlnorm" "qlogis" "qnbinom" "qnorm" "qpois" "qsignrank" "qt" "qunif" "qweibull" "qwilcox" "rnorm" "sd" "sigma" "var"
#' @importFrom shiny fluidPage
#' @export
P <- function(q, dist = "normal", lower.tail = TRUE,
              rounding = 5, porcentage = FALSE,
              gui = c("none", "plot", "tcltk", "rstudio", "shiny"), main = NULL,
              browser.shiny = getOption("shiny.launch.browser", interactive()),
              plot.type = c("pdf", "cdf", "pmf"),
              dec = c(".", ","), long.segment = FALSE, col = "#8EC5E5",
              col2 = "#38A8E8", lty = 2,
              text.size = par("cex"), cex.main = par("cex.main"),
              cex.axis = par("cex.axis"), cex.lab = par("cex.lab"),
              vert.orien.main = TRUE, ...) {

  #################################################
  # Argument matching
  #################################################

  # Validate and standardize the graphical
  # interface argument provided by the user.
  #
  # match.arg() ensures that only one of the
  # predefined interface options is accepted.
  gui <- match.arg(gui)

  # Validate the type of probability-related
  # plot requested by the user.
  #
  # Supported plot types include:
  #
  # - pdf -> probability density function
  # - cdf -> cumulative distribution function
  # - pmf -> probability mass function
  plot.type <- match.arg(plot.type)

  # Validate the decimal separator used in
  # numerical labels and graphical outputs.
  #
  # Supported formats:
  #
  # - "." -> decimal point
  # - "," -> decimal comma
  dec <- match.arg(dec)

  # Arguments in '...'
  # grab the list of the object in '...', if is some there
  argaddit <- list(...)

  # Formal arguments
  argdef <- formals(P)

  ###############################################
  # First processing stage of function P():
  # the function initially checks the length of q.
  #
  # If q has length equal to 1, the argument dist
  # is evaluated. After validating the selected
  # distribution, the function checks the
  # lower.tail argument, and finally evaluates
  # the gui argument to determine how the result
  # stored in 'prob' will be returned.
  # Resume
  # ======
  # | P()
  # |--> q = 1
  #   |-> dist
  #   | - "normal", ...
  #     |-> lower.tail
  #     | - TRUE or FALSE or NULL
  #       |-> gui
  #       | - "plot", "rstudio", "tcltk", "shiny"
  #         |-> return prob
  #
  # If q has length greater than 1, the function
  # evaluates the probability region associated
  # with operators such as %>X>%, %<X<%, and their
  # variants through the 'region' attribute.
  #
  # After identifying the corresponding region,
  # the function evaluates the selected
  # distribution through the dist argument and,
  # subsequently, the gui argument to determine
  # the output interface.
  #
  # Resume
  # ======
  # | P()
  # |--> q > 1
  #   |-> region
  #     | - A or B
  #     |-> dist
  #     | - "normal", ...
  #       |-> gui
  #       | - "plot", "rstudio", "tcltk", "shiny"
  #         |-> return prob
  #
  # Finally, function P() returns the value stored
  # in 'prob'.
  ################################################

  #########################
  # Check the q argument
  #########################

  # If q argument is greater than 1
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {

    # r1: %>X>%; r3 = %>=X>=%; r5: %>=X>%; r6: %>X>=%
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%

    # r2: %<X<%; r4: %<=X<=%; r7: %<=X<%; r8: %<=x<%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%

    # Region A
    if (any(attr(q, "region") == regiona)) {
      
      # Verifying if the distrubution is a Normal shape 
      if (dist == "normal") {
        
        #Region A Call
        prob <- normal_distrubution(q,
          argaddit,
          rounding,
          main,
          gui,
          lower.tail,
          dec,
          col,
          col2,
          long.segment,
          lty,
          text.size,
          cex.main,
          cex.axis,
          cex.lab,
          vert.orien.main,
          porcentage,
          browser.shiny,
          region = "Region A"
        )

      }
    }

    # Region B
    if (any(attr(q, "region") == regionb)) {
      if (dist == "normal") {
        
        # Region B Call
        prob <- normal_distrubution(q,
          argaddit,
          rounding,
          main,
          gui,
          lower.tail,
          dec,
          col,
          col2,
          long.segment,
          lty,
          text.size,
          cex.main,
          cex.axis,
          cex.lab,
          vert.orien.main,
          porcentage,
          browser.shiny,
          region = "Region B"
        )
      }
    }
  } else { # If the q argument length is 1

    #########################
    # Check the dist argument
    #########################

    # If the dist argument is set to "normal"
    if (dist == "normal") {

      prob <- normal_distrubution(q,
        argaddit,
        rounding,
        main,
        gui,
        lower.tail,
        dec,
        col,
        col2,
        long.segment,
        lty,text.size,
        cex.main,
        cex.axis,
        cex.lab,
        vert.orien.main,
        porcentage,
        browser.shiny
      )

    }
  }

  #################################################
  # Final formatting of the returned value
  #################################################

  # Round the computed value according to the
  # number of decimal places specified by the
  # user through the 'rounding' argument.
  #
  # This step standardizes the numerical output
  # before returning the final result.
  prob <- round(prob, rounding)

  #################################################
  # Percentage conversion
  #################################################

  # Check whether the argument 'lower.tail'
  # is NULL.
  #
  # If lower.tail is NULL, the returned value
  # corresponds to the density function output
  # produced by dnorm(), and therefore no
  # percentage conversion is applied.
  #
  # Otherwise, convert the value to percentage
  # format by multiplying it by 100.
  #
  # This structure was adopted to maintain
  # compatibility between probability outputs
  # and density outputs within the same function.
  if (is.null(lower.tail)) {

    prob

  } else {

    prob <- prob * 100
  }

  #################################################
  # Return final result
  #################################################

  # Return the formatted value to the user.
  return(prob)
}





normal_distrubution <- function(q, argaddit, rounding, main, gui, lower.tail, dec, col, col2, long.segment, lty,text.size, cex.main,
              cex.axis, cex.lab, vert.orien.main, porcentage, browser.shiny, region = NULL) {

  #################################################
  # Interactive input for distribution parameters
  #################################################

  # Check whether the user supplied the 'mean'
  # argument inside the additional arguments list.
  #
  # If the argument is missing, request the value
  # interactively through the console using
  # readline().
  #
  # The entered value is converted to numeric
  # format and stored back into 'argaddit' to
  # preserve a consistent structure for subsequent
  # computations inside the package workflow.
  if (!any(names(argaddit) == "mean")) {

    mean <- readline(
      paste0(
        gettext(
          "Enter the value of 'mean' argument:",
          domain = "R-leem"
        ),
        " "
      )
    )

    argaddit$mean <- as.numeric(mean)
  }

  # Check whether the user supplied the 'sd'
  # (standard deviation) argument.
  #
  # If the argument is not available, request
  # the value interactively from the user.
  #
  # The value is converted to numeric format
  # before being stored in 'argaddit'.
  if (!any(names(argaddit) == "sd")) {

    sd <- readline(
      paste0(
        gettext(
          "Enter the value of 'sd' argument:",
          domain = "R-leem"
        ),
        " "
      )
    )

    argaddit$sd <- as.numeric(sd)
  }

  #################################################
  # Validation of the standard deviation parameter
  #################################################

  # Ensure that the standard deviation parameter
  # is strictly greater than zero.
  #
  # The Normal distribution requires a positive
  # standard deviation. Therefore, keep requesting
  # a new value until the user provides a valid
  # numeric input.
  #
  # gettext() is used to support package
  # internationalization and translation files.
  while (argaddit$sd <= 0) {

    arg1 <- gettext(
      "Please, Insert the value of 'sd' greater then 0:",
      domain = "R-leem"
    )

    sd <- readline(paste0(arg1, " "))

    argaddit$sd <- as.numeric(sd)
  }


  mu <- argaddit$mean
  sigma <- argaddit$sd

  if (length(q) > 1){
    # Auxiliar variables
    minimo <- if (q[1] <= argaddit$mean - 3 * argaddit$sd) q[1] - 3 * argaddit$sd else argaddit$mean - 3 * argaddit$sd
    maximo <- if (q[2] > argaddit$mean + 3 * argaddit$sd) q[2] + 3 * argaddit$sd else argaddit$mean + 3 * argaddit$sd

  } else {
    minimo <- if (q <=  argaddit$mean - 3 * argaddit$sd) q - 3 * argaddit$sd else argaddit$mean - 3 * argaddit$sd
    maximo <- if (q > argaddit$mean + 3 * argaddit$sd) q + 3 * argaddit$sd else argaddit$mean + 3 * argaddit$sd
  
  }

  #########################################
  # Verifying regions parameter
  #########################################
  if (!is.null(region)) {

    #########################################
    # Starting call the gui
    # where Lower tail is none and q > 1
    #########################################
  
    # Region A
    if (region == "Region A") {
      if (gui == "plot") {
        #################################################
        # Base R plotting interface
        #################################################

        # Call the internal plotting function
        # responsible for generating the static
        # visualization of the Normal density curve
        # using base R graphics.
        
        # This interface produces a traditional plot
        # highlighting the density curve associated
        # with the Normal distribution.
        
        # ./aux_probability.R
        plotpnormalraplot(q, mu, sigma, rounding,
                           dec, long.segment, col,
                           col2, lty, main,
                           text.size, cex.main,
                           cex.axis, cex.lab,
                           vert.orien.main)
      }

    if (gui == "rstudio") {
      #################################################
      # RStudio graphical interface
      #################################################

      # Call the internal plotting function
      # responsible for generating the interactive
      # Normal distribution visualization in the
      # RStudio environment.
      #
      # This interface was designed to provide an
      # interactive graphical experience directly
      # within RStudio.
      #
      # ./aux_probability.R
      plotpnormalrarstudio(q[1], q[2], q, mu, sigma, rounding,
                           minimo, maximo, dec,
                           long.segment, col,
                           col2, lty, main,
                           text.size, cex.main,
                           cex.axis, cex.lab,
                           vert.orien.main)
    }

    if (gui == "tcltk") {
      # Desabilitar warnings global
      # options(warn = - 1)
      war <- options(warn = - 1)

     .tkplotleemnormal3(q[1], q[2], mu, sigma, rounding, main, minimo, maximo, q)

      # Desabilitar warnings global
      #options(warn = - 1)
      #war <- options(warn = - 1)
      on.exit(options(war))
    }

    # Calculates the desired probability
    prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) +
      pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)

    return(prob)
  }

  # If is region B
  if (region == "Region B"){
    # Auxiliar variables
    # minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
    # maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
    # mu <- argaddit$mean
    # sigma <- argaddit$sd

    if (gui == "plot") {
      plotpnormalbrplot(q, mu, sigma, rounding, main)
    }

    if (gui == "rstudio") {
      manipulate::manipulate(plotpnormalbrrstudio(q1, q2, mean, sd, rounding, main, q),
                             q1 = manipulate::slider(minimo, q[2], q[1]),
                             q2 = manipulate::slider(q[1], maximo, q[2]),
                             mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                             sd = manipulate::slider(sigma, sigma * 1.8, sigma))
    }
    
    if (gui == "tcltk") {
      # Desabilitar warnings global
      #options(warn = - 1)
      war <- options(warn = - 1)

      .tkplotleemnormal4(q[1], q[2], mu, sigma, rounding, main, minimo, maximo, q)

      # Desabilitar warnings global
      on.exit(options(war))
    }

    prob <- pnorm(q = q[2], mean = mu, sd=sigma) - 
    pnorm(q = q[1], mean = mu, sd=sigma)
  }
  }#########################################
  # Starting call the gui
  # where Lower tail is none and q > 1
  #########################################
  
  # Region A
  if (region == "Region A") {
    if (gui == "plot") {
      #################################################
      # Base R plotting interface
      #################################################

       # Call the internal plotting function
      # responsible for generating the static
      # visualization of the Normal density curve
      # using base R graphics.
      #
      # This interface produces a traditional plot
      # highlighting the density curve associated
      # with the Normal distribution.
      #
      # ./aux_probability.R
      plotpnormalraplot(q, mu, sigma, rounding,
                         dec, long.segment, col,
                         col2, lty, main,
                         text.size, cex.main,
                         cex.axis, cex.lab,
                         vert.orien.main)
    }

    if (gui == "rstudio") {
      #################################################
      # RStudio graphical interface
      #################################################

      # Call the internal plotting function
      # responsible for generating the interactive
      # Normal distribution visualization in the
      # RStudio environment.
      #
      # This interface was designed to provide an
      # interactive graphical experience directly
      # within RStudio.
      #
      # ./aux_probability.R
      plotpnormalrarstudio(q[1], q[2], q, mu, sigma, rounding,
                           minimo, maximo, dec,
                           long.segment, col,
                           col2, lty, main,
                           text.size, cex.main,
                           cex.axis, cex.lab,
                           vert.orien.main)
    }

    if (gui == "tcltk") {
      # Desabilitar warnings global
      # options(warn = - 1)
      war <- options(warn = - 1)

     .tkplotleemnormal3(q[1], q[2], mu, sigma, rounding, main, minimo, maximo, q)

      # Desabilitar warnings global
      #options(warn = - 1)
      #war <- options(warn = - 1)
      on.exit(options(war))
    }

    # Calculates the desired probability
    prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) +
      pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)

    return(prob)
  }

  # If is region B
  if (region == "Region B"){
    # Auxiliar variables
    # minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
    # maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
    # mu <- argaddit$mean
    # sigma <- argaddit$sd

    if (gui == "plot") {
      plotpnormalbrplot(q, mu, sigma, rounding, main)
    }

    if (gui == "rstudio") {
      manipulate::manipulate(plotpnormalbrrstudio(q1, q2, mean, sd, rounding, main, q),
                             q1 = manipulate::slider(minimo, q[2], q[1]),
                             q2 = manipulate::slider(q[1], maximo, q[2]),
                             mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                             sd = manipulate::slider(sigma, sigma * 1.8, sigma))
    }
    
    if (gui == "tcltk") {
      # Desabilitar warnings global
      #options(warn = - 1)
      war <- options(warn = - 1)

      .tkplotleemnormal4(q[1], q[2], mu, sigma, rounding, main, minimo, maximo, q)

      # Desabilitar warnings global
      on.exit(options(war))
    }

    prob <- pnorm(q = q[2], mean = mu, sd=sigma) - 
    pnorm(q = q[1], mean = mu, sd=sigma)
  
  } else {

    #########################################
    # Verifying Lower Tail parameter
    #########################################

    # If the lower.tail argument is set to "TRUE"
    if(isTRUE(lower.tail)) {

      #################################################
      # Base R plotting interface
      #################################################

      # Call the internal plotting function
      # responsible for generating the static
      # visualization of the Normal probability curve
      # using base R graphics.
      #
      # This interface produces a traditional plot
      # highlighting the density curve associated
      # with the Normal distribution.
      #
      # ./aux_probability.R
      if (gui == "plot") {
        plotpnormallttplot(q, mu, sigma, rounding,
                           dec, long.segment, col,
                           col2, lty, main,
                           text.size, cex.main,
                           cex.axis, cex.lab,
                           vert.orien.main
                          )
      }

      if (gui == "rstudio") {
        #################################################
        # RStudio graphical interface
        #################################################

        # Call the internal plotting function
        # responsible for generating the interactive
        # Normal distribution visualization in the
        # RStudio environment.
        #
        # This interface was designed to provide an
        # interactive graphical experience directly
        # within RStudio.
        #
        # ./aux_probability.R
        plotpnormallttrstudio(
          q, mu, sigma, rounding,
          minimo, maximo, dec,
          long.segment, col,
          col2, lty, main,
          text.size, cex.main,
          cex.axis, cex.lab,
          vert.orien.main
        )
      }

      if (gui == "tcltk") {
        #################################################
        # Tcl/Tk graphical interface
        #################################################

        # Call the internal Tcl/Tk plotting function
        # responsible for generating the interactive
        # Normal distribution visualization.
        #
        # This graphical interface allows the user to
        # explore the Normal density curve interactively
        # using Tcl/Tk components.
        #
        # ./aux_probability.R
        plotpnormalltttcltk(
          q, mu, sigma, rounding,
          minimo, maximo, dec,
          long.segment, col,
          col2, lty, main,
          text.size, cex.main,
          cex.axis, cex.lab,
          vert.orien.main
        )
      }

      if (gui == "shiny" ) {
        #################################################
        # Shiny graphical interface
        #################################################

        # Call the internal Shiny plotting function
        # responsible for generating the interactive
        # Normal distribution visualization.
        #
        # The returned object contains all information
        # required for the S3 method print.leem() to
        # launch the Shiny application automatically.
        #
        # ./aux_probability.R
        return(
          plotpnormallttshiny(
            q, mu, sigma, rounding, porcentage,
            minimo, maximo, dec,
            long.segment, col,
            col2, lty, main,
            browser.shiny,
            text.size, cex.main,
            cex.axis, cex.lab,
            vert.orien.main
          )
        )
      }

      # Compute the desired probability
      prob <- pnorm(q = q, mean = mu, sd = sigma)
      return(prob)
    }

    if(isFALSE(lower.tail)) {
      if (gui == "plot") {
        plotpnormalltfplot(q, mu, sigma, rounding, dec,
                               long.segment, col,
                               col2, lty, main)
      }

      if (gui == "rstudio") {
        manipulate::manipulate(plotpnormalltfplot(q, mean, sd, rounding, main),
                               q = manipulate::slider(q, mu + 4 * sigma, q),
                               mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                               sd = manipulate::slider(sigma, sigma * 1.8, sigma))
      }

      if (gui == "tcltk") {
        #################################################
        # Tcl/Tk graphical interface
        #################################################

        # Call the internal Tcl/Tk plotting function
        # responsible for generating the interactive
        # Normal distribution visualization.
        #
        # This graphical interface allows the user to
        # explore the Normal area interactively
        # using Tcl/Tk components.
        #
        # ./aux_probability.R

        #plotdnormalltntcltk(
        #  q, mu, sigma, rounding,
        #  minimo, maximo, dec,
        #  long.segment, col,
        #  col2, lty, main,
        #  text.size, cex.main,
        #  cex.axis, cex.lab,
        #  vert.orien.main
        #)



        # Desabilitar warnings global
        #options(warn = - 1)
        war <- options(warn = - 1)
        #on.exit(options(war))

        .tkplotleemnormal2(q, mu, sigma, rounding, main, minimo, maximo)

        # Desabilitar warnings global
        #options(warn = - 1)
        #war <- options(warn = - 1)
        on.exit(options(war))


      }

      # Compute the desired probability
      prob <- pnorm(q = q, mean = mu, sd=sigma, lower.tail = F)

      return(prob)
    }

    # If the lower.tail argument is set to "NULL"
    if(is.null(lower.tail)) {
      #################################################
      # Base R plotting interface
      #################################################

      # Call the internal plotting function
      # responsible for generating the static
      # visualization of the Normal density curve
      # using base R graphics.
      #
      # This interface produces a traditional plot
      # highlighting the density curve associated
      # with the Normal distribution.
      #
      # ./aux_probability.R
      if (gui == "plot") {
        plotdnormalltnplot(
          q, mu, sigma, rounding, dec,
          long.segment, col,
          col2, lty, main,
          text.size, cex.main,
          cex.axis, cex.lab,
          vert.orien.main
        )
      }

      # If the gui argument is set to "rstudio"
      if (gui == "rstudio") {
        #################################################
        # RStudio graphical interface
        #################################################

        # Call the internal plotting function
        # responsible for generating the interactive
        # Normal distribution visualization in the
        # RStudio environment.
        #
        # This interface was designed to provide an
        # interactive graphical experience directly
        # within RStudio.
        #
        # ./aux_probability.R
        plotdnormalltnrstudio(
          q, mu, sigma, rounding,
          minimo, maximo, dec,
          long.segment, col,
          col2, lty, main,
          text.size, cex.main,
          cex.axis, cex.lab,
          vert.orien.main
        )
      }

      if (gui == "tcltk") {
        #################################################
        # Tcl/Tk graphical interface
        #################################################

        # Call the internal Tcl/Tk plotting function
        # responsible for generating the interactive
        # Normal distribution visualization.
        #
        # This graphical interface allows the user to
        # explore the Normal density curve interactively
        # using Tcl/Tk components.
        #
        # ./aux_probability.R
        plotdnormalltntcltk(
          q, mu, sigma, rounding,
          minimo, maximo, dec,
          long.segment, col,
          col2, lty, main,
          text.size, cex.main,
          cex.axis, cex.lab,
          vert.orien.main
        )
      }

      # If the gui argument is set to "shiny"
      if (gui == "shiny") {
        #################################################
        # Shiny graphical interface
        #################################################

        # If the user selected the Shiny graphical
        # interface, display a warning message informing
        # that the returned value corresponds to the
        # height of the Normal density curve at x = q
        # and not to a probability value.
        #
        # This distinction is important because dnorm()
        # computes the value of the probability density
        # function (PDF), whereas probabilities for the
        # Normal distribution are obtained with pnorm().
        message(
          gettext(
            "Note: the returned value is the height of the Normal density curve at x = q and not a probability.",
            domain = "R-leem"
          )
        )

        # Call the internal Shiny plotting function
        # responsible for generating the interactive
        # Normal distribution visualization.
        #
        # The returned object contains all information
        # required for the S3 method print.leem() to
        # launch the Shiny application automatically.
        #
        # ./aux_probability.R
        return(
          plotdnormalltnshiny(
            q, mu, sigma, rounding, porcentage,
            minimo, maximo, dec,
            long.segment, col,
            col2, lty, main,
            browser.shiny,
            text.size, cex.main,
            cex.axis, cex.lab,
            vert.orien.main
          )
        )

        #################################################
        # Probability density function
        #################################################

        # Compute the value of the Normal probability
        # density function (PDF) at x = q.
        #
        # The function dnorm() does NOT return a
        # probability. Instead, it returns the height
        # of the density curve associated with the
        # Normal distribution.
        prob <- dnorm(
          x = q,
          mean = mu,
          sd = sigma
        )

        #################################################
        # Informative user message
        #################################################

        # Display a warning message clarifying the
        # interpretation of the returned value.
        #
        # This message was intentionally added because
        # users frequently confuse:
        #
        # - dnorm()  -> density function values
        # - pnorm()  -> cumulative probabilities
        #
        # The message helps prevent the incorrect
        # interpretation of the density value as a
        # probability.
        #
        # gettext() is used to support package
        # internationalization and translation files.
        message(
          gettext(
            "Note: the returned value is the height of the Normal density curve at x = q and not a probability.",
            domain = "R-leem"
          )
        )
      }

      # Compute the probability density function
      prob <- dnorm(x = q, mean = mu, sd=sigma)
      return(prob)
    }
  }
}