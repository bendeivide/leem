#' Cumulative distribution function
#'
#' \code{P} Compute the cumulative distribution function for multiple distributions
#'
#' @param q quantile. The \code{q} argument can have length 1 or 2. See Details.
#' @param dist distribution to use. The default is \code{'normal'}. Options: \code{'normal'}, \code{'t-student'}, \code{'chisq'}, \code{'f'}, ...
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}. This argument is valid only if \code{q} has length 1.
#' @param rounding numerical; it represents the number of decimals for calculating the probability.
#' @param porcentage logical; if \code{FALSE} (default), the result in decimal. Otherwise, probability is given in percentage.
#' @param gui default is \code{'plot'}; it graphically displays the result of the probability. Others options are: \code{'none'}, \code{'rstudio'} or \code{'tcltk'}.
#' @param main defalt is \code{NULL}; it represents title of plot.
#' @param ... additional arguments according to the chosen distribution.
#'
#' @details The argument that can have length 2, when we use the functions that give us the probability regions, given by: \code{\%<X<\%}, \code{\%<=X<\%}, \code{\%<X<=\%}, \code{\%<=X<=\%}, \code{\%>X>\%}, \code{\%>X=>\%}, \code{\%>X=>\%} and \code{\%>=X=>\%}.
#' The additional arguments represent the parameters of the distributions, that is:
#' - If \code{dist = "normal"} (Default); the additional arguments are: \code{mean} (\eqn{\mu}) and \code{sd} (\eqn{\sigma}). The PDF is given by:
#' \deqn{\displaystyle{f_X(x; \mu, \sigma) = \frac {1}{\sqrt {2\pi \sigma ^{2}}}}e^{-{\frac {(x-\mu )^{2}}{2\sigma ^{2}}}}, \quad x \in \mathbb{R},~ \mu \in \mathbb{R},~\sigma^2 > 0;}
#' - If \code{dist = "t-student"}; the additional argument is: \code{df} (\eqn{\nu}). The PDF is given by:
#' \deqn{\displaystyle{f_X(x; \nu) = \frac {\Gamma \left({\frac {\ \nu +1\ }{2}}\right)}{{\sqrt {\pi \ \nu \ }}\ \Gamma \left({\frac {\nu }{\ 2\ }}\right)}}\left(\ 1+{\frac {~x^{2}\ }{\nu }}\ \right)^{-{\frac {\ \nu +1\ }{2}}}, \quad x \in \mathbb{R},~\nu > 1;}
#' - If \code{dist = "chisq"}; the additional argument is: \code{df} (\eqn{\nu}). The PDF is given by:
#'  \deqn{\displaystyle{f_X(x; \nu) = \frac {1}{2^{k/2}\Gamma (k/2)}}\;x^{k/2-1}e^{-x/2}, \quad x > 0,~\nu > 0;}
#' - If \code{dist = "f"}; the additional argument is: \code{df1} (\eqn{\nu_1}) and \code{df2} (\eqn{\nu_2}). The PDF is given by:
#'  \deqn{f_X(x; \nu_1, \nu_2) = {\displaystyle {\frac {\sqrt {\frac {(\nu_{1}x)^{\nu_{1}}\nu_{2}^{\nu_{2}}}{(\nu_{1}x+\nu_{2})^{\nu_{1}+\nu_{2}}}}}{x\,\mathrm {B} \!\left({\frac {\nu_{1}}{2}},{\frac {\nu_{2}}{2}}\right),}}\!}, \quad x > 0,~\nu_1,\nu_2 > 0;}
#'  where, \eqn{x > 0}, \eqn{\nu_1,~\nu_2} > 0, and \eqn{B} is the beta function.
#'
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
#  @import shiny
#' @importFrom "stats" "dbeta" "dbinom" "dcauchy" "dchisq" "dexp" "df" "dgamma" "dgeom" "dhyper" "dlnorm" "dlogis" "dnbinom" "dnorm" "dpois" "dsignrank" "dt" "dunif" "dweibull" "dwilcox" "pbeta" "pbinom" "pcauchy" "pchisq" "pexp" "pf" "pgamma" "pgeom" "phyper" "plnorm" "plogis" "pnbinom" "pnorm" "ppois" "psignrank" "pt" "ptukey" "punif" "pweibull" "pwilcox" "qbeta" "qbinom" "qcauchy" "qchisq" "qexp" "qf" "qgamma" "qgeom" "qhyper" "qlnorm" "qlogis" "qnbinom" "qnorm" "qpois" "qsignrank" "qt" "qunif" "qweibull" "qwilcox" "rnorm" "sd" "sigma" "var"
#' @export
P <- function(q, dist = "normal", lower.tail = TRUE,
              rounding = 5, porcentage = FALSE, gui = "plot", main = NULL, ...) {
  # defensive programming
  ## dist
  ## gui
  ## q

  # Arguments in '...'
  argaddit <- list(...)
  # Formal arguments
  argdef <- formals(P)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regiona)) {
      if (dist == "normal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        mu <- argaddit$mean
        sigma <- argaddit$sd

        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd

        if (gui == "plot") {
          plotpnormalarplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpnormalarrstudio(q1, q2, mean, sd, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[1], maximo, q[2]),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)

          .tkplotleemnormal3(q[1], q[2], mu, sigma, rounding, main, minimo, maximo, q)

          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
          # prob <- round(pt(q[2], df = nu,
          #                  lower.tail = T) + pt(q[1], df = nu, lower.tail = F), digits=rounding)
        }
        # Calculates the desired probability
        prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) +
          pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)
      }
      if (dist == "t-student") {
        if (!any(names(argaddit) == "ncp")) {
          argaddit$ncp <- 0
        }
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Enter the value of 'df' argument:", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }

        nu <- argaddit$df
        ncp <- argaddit$ncp

        #Auxiliary Arguments
        # Auxiliar function
        llower <- if (nu <= 2) q[1] - 6 else q[1]  - 4 * .erro_padrao_t_nc(nu, ncp)
        lupper <- if (nu <= 2) q[2] + 6 else q[2] + 4 * .erro_padrao_t_nc(nu, ncp)
        #x <- seq(-llower, q[1], by=0.01)


        if (gui == "plot" ) {
          plotptstudentarplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotptstudentarrstudio(q1, q2, df, ncp, rounding, main, q),
                                 q1 = manipulate::slider(llower, q[2], q[1]),
                                 q2 = manipulate::slider(q[1], lupper, q[2]),
                                 df = manipulate::slider(1, nu + 100, nu),
                                 ncp = manipulate::slider(ncp - 5, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          # on.exit(options(war))

          .tkplotleemtstudent3(q[1], q[2], nu, ncp, rounding, main, llower, lupper, q)

          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Calculates the desired probability
        prob <- pt(q[1], df = nu, ncp, lower.tail = T) + pt(q[2], df = nu, ncp, lower.tail = F)
      }
      if (dist == "chisq") {
        if (!any(names(argaddit) == "ncp")) {
          ncp <- 0
          argaddit$ncp <- as.numeric(ncp)
        }
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of 'df' argument: ", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }

        if (argaddit$ncp < 0 ) stop("The 'ncp' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        ncp <- argaddit$ncp
        nu <- argaddit$df
        sig4n <- nu + ncp - 4 * sqrt(2 * (nu + 2 * ncp)) # mu - 4 * sigma
        sig4p <- nu + ncp + 5 * sqrt(2 * (nu + 2 * ncp)) # mu + 5 * sigma
        minimo <- if (sig4n < 0 | sig4n > q[1]) 0 else sig4n
        maximo <- if (sig4p < q[2]) q[2] + sig4p else sig4p

        if (gui == "plot") {
          plotpchisqarplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpchisqarrstudio(q1, q2, nu, ncp, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[1], maximo, q[2]),
                                 nu = manipulate::slider(1, nu * 10, nu),
                                 ncp = manipulate::slider(0, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          # on.exit(options(war))

          .tkplotleemchisq3(q[1], q[2], nu, ncp, rounding, main, minimo, maximo, q)

          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        prob <- pchisq(q[1], df = nu, ncp = ncp, lower.tail = T) +
          pchisq(q[2], df = nu, ncp = ncp, lower.tail = F)
      }
      if (dist == "f") {
        if (!any(names(argaddit) == "df1")) {
          df1 <- readline(gettext("Insert the value of 'df1' argument: ", domain = "R-leem"))
          argaddit$df1 <- as.numeric(df1)
        }
        if (!any(names(argaddit) == "df2")) {
          df2 <- readline(gettext("Insert the value of 'df2' argument: ", domain = "R-leem"))
          argaddit$df2 <- as.numeric(df2)
        }

        if (argaddit$df1 <= 0) stop("The df1 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")
        if (argaddit$df2 <= 0) stop("The df2 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")

        df1 <- argaddit$df1
        df2 <- argaddit$df2
        # Auxiliar variables

        if (gui == "plot") {
          plotpfarplot(q, df1, df2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpfarrstudio(q1, q2, df1, df2, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[1], 20, q[2]),
                                 df1 = manipulate::slider(1, df1  * 10 , df1),
                                 df2 = manipulate::slider(1, df2 * 10, df2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # Calculates the desired probability
        prob <- pf(q[1], df1, df2, lower.tail = T) +
          pf(q[2], df1, df2, lower.tail = F)
      }
      if (dist == "gumbel") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
        maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location

        if (gui == "plot" ) {
          plotpgumbelarplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgumbelarrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pgumbel(q[1], location, scale, lower.tail = T) +
          pgumbel(q[2], location, scale, lower.tail = F)
        }
      if (dist == "beta") {
        if (!any(names(argaddit) == "alpha")) {
          alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
          argaddit$alpha <- as.numeric(alpha)
        }
        if (!any(names(argaddit) == "beta")) {
          beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
          argaddit$beta <- as.numeric(beta)
        }
        if (q[1]<0 || q[2]>1 || q[1]>1 || q[2]<0){
          stop("The quantile argument must be on the interval 0 and 1!",  call. = FALSE, domain = "R-leem")
        }
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta

        if (gui == "plot") {
          plotpbetaarplot(q, shape1, shape2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbetaarrstudio(q1, q2, shape1, shape2, rounding, main, q),
                                 q1 = manipulate::slider(q[1], 1, q[1]),
                                 q2 = manipulate::slider(q[2], 1, q[2]),
                                 shape1 = manipulate::slider(shape1, shape1 + 5 * shape1, shape1),
                                 shape2 = manipulate::slider(shape2, shape2 + 5 * shape2, shape2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbeta(q[1], shape1, shape2, lower.tail = T) + pbeta(q[2], shape1, shape2, lower.tail = F)
      }
      if (dist == "exp") {
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument: ",
                                   domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        rate <- argaddit$rate

        if (gui == "plot") {
        plotpexparplot(q, rate, rounding, main = NULL)
        }
        if (gui == "rstudio") {
          rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          manipulate::manipulate(plotpexparrstudio(q1, q2, rate, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 rate = manipulate::slider(rate, rate + 5 * rate, rate))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(pexp(q[1], rate = rate, lower.tail = T) +
                        pexp(q[2], rate = rate, lower.tail = F), rounding)
      }
      if (dist == "gamma") {
        if (!any(names(argaddit) == "shape")) {
          shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
          argaddit$shape <- as.numeric(shape)
        }
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument (to skip press enter): ", domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument (to skip press enter): ", domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }

        shape <- argaddit$shape
        rate <- argaddit$rate
        scale <- argaddit$scale

        if (gui == "plot") {
          plotpgammaarplot(q, shape, rate, scale, rounding, main)
        }

        if (gui == "rstudio") {
          if (is.na(rate)){
          auxarg <- scale
          minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
          maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
          manipulate::manipulate(plotpgammaarrstudio(q1, q2, shape, rate, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 shape = manipulate::slider(shape, shape + 4 * shape, shape),
                                 scale = manipulate::slider(scale, scale + 4 * scale, scale))
          }
        if (is.na(scale)){
          auxarg <- rate
          minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
          maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
          manipulate::manipulate(plotpgammaarrstudio(q1, q2, shape, rate, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 shape = manipulate::slider(shape, shape + 4 * shape, shape),
                                 rate = manipulate::slider(rate, rate + 4 * rate, rate))
        }
      }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if(is.na(scale)){
          prob <- round(pgamma(q = q[1], shape, rate = rate) -
                          pgamma(q = q[2], shape, rate = rate, lower.tail = F),digits = rounding)
        }
        if (is.na(rate)){
          prob <- round(pgamma(q = q[1], shape, scale = scale) -
                          pgamma(q = q[2], shape, scale = scale, lower.tail = F),digits = rounding)
        }
      }
      if (dist == "cauchy") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
        maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location

        if (gui == "plot" ) {
          plotpcauchyarplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpcauchyarrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pcauchy(q[1], location, scale, lower.tail = T) +
          pcauchy(q[2], location, scale, lower.tail = F)
      }
      if (dist == "logis") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
        maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location

        if (gui == "plot" ) {
          plotplogisarplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplogisarrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plogis(q[1], location, scale, lower.tail = T) +
          plogis(q[2], location, scale, lower.tail = F)
      }
      if (dist == "lnormal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        mu <- argaddit$mean
        sigma <- argaddit$sd

        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd

        if (gui == "plot") {
          plotplnormalarplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplnormalarrstudio(q1, q2, mean, sd, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # Calculates the desired probability
        prob <- plnorm(q[1], meanlog = mu, sdlog = sigma, lower.tail = T) +
          plnorm(q[2], meanlog = mu, sdlog = sigma, lower.tail = F)
      }
      if (dist == "tukey") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      if (dist == "weibull") {
        if (!any(names(argaddit) == "shape")) {
          shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
          argaddit$shape <- as.numeric(shape)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        shape <- argaddit$shape
        scale <- argaddit$scale

        if (gui == "plot") {
          plotpweibullarplot(q, shape, scale, rounding, main)
        }
        if (gui == "rstudio") {
          minimo <- if (q[1] <= shape - 4 * shape) q[1] - 4 * shape else 0
          maximo <- if (q[2] > shape + 4 * shape) q[2] + 4 * shape else shape + 4 * shape
          manipulate::manipulate(plotpweibullarrstudio(q1, q2, shape, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 shape = manipulate::slider(shape, shape + 5 * sqrt(shape), shape),
                                 scale = manipulate::slider(scale, scale + 5 * sqrt(scale), scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pweibull(q[1], shape, scale, lower.tail = T) +
          pweibull(q[2], shape, scale, lower.tail = F)
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        while (argaddit$lambda <= 0) {
          lambda <- readline(gettext("Please, Insert the value of 'lambda' greater then 0: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }

        lambda <- argaddit$lambda
        rmax <- ceiling(q[2] + 4 * sqrt(lambda))
        rmin <- ceiling(lambda - 4 * sqrt(lambda))
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)

        if (gui == "plot") {
          plotppoissonarplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonarrstudio(q1, q2, lambda, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 lambda = manipulate::slider(1, lambda+30, lambda))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- (ppois(q = q[1], lambda = lambda, lower.tail = T)) +
          (ppois(q = q[2],lambda = lambda, lower.tail = F))
      }
      if (dist == "binomial") {
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        size <- argaddit$size
        prob <- argaddit$prob

        if (gui == "plot") {
          plotpbinomialarplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialarrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], q[2]+size, q[2]),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(pbinom(q = q[1], size = size, prob = prob) + pbinom(q = q[2] - 1, size = size, prob = prob, lower.tail = FALSE),
                      digits = rounding)
      }
      if (dist == "nbinom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        if (argaddit$prob > 1 || argaddit$prob < 0) {
          stop("The 'prob' argument must be between zero and one!", call. = FALSE, domain = "R-leem")
        }
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (argaddit$size < 0) {
          stop("The 'size' argument must be higther then zero!", call. = FALSE, domain = "R-leem")
        }
        prob <- argaddit$prob
        size <- argaddit$size
        if (gui == "plot" ) {
          plotpnbinomarplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < size) trunc(q[1] - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > size) ceiling(q[2] + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
          manipulate::manipulate(plotpnbinomarrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 size = manipulate::slider(1, size + 4 * size, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- (pnbinom(q = q[1], size, prob, lower.tail = T)) +
          (pnbinom(q = q[2], size, prob, lower.tail = F))
      }
      if (dist == "hyper") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        if (!any(names(argaddit) == "k")) {
          k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
          argaddit$k <- as.numeric(k)
        }
        m <- argaddit$m
        n <- argaddit$n
        k <- argaddit$k
        if (gui == "plot") {
          plotphyperarplot(q, m, n, k, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < k) trunc(q[1] - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > k) ceiling(q[1] + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
          manipulate::manipulate(plotphyperarrstudio(q1, q2, m, n, k, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n),
                                 k = manipulate::slider(k, k + 5 * k, k))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <- round((phyper(q = q[1], m, n, k, lower.tail = T)) + (phyper(q = q[2], m, n, k, lower.tail = F)),
                      digits = rounding)
      }
      if (dist == "geom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        prob <- argaddit$prob
        if (gui == "plot") {
          plotpgeomarplot(q, prob, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < 10*prob) trunc(q[1] - 4 * sqrt(10*prob)) else trunc(prob - 4 * sqrt(10*prob))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > 10*prob) ceiling(q[2] + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
          manipulate::manipulate(plotpgeomarrstudio(q1, q2, prob, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(pgeom(q = q[1], prob) - pgeom(q = q[2], prob, lower.tail = F),digits = rounding)
      }
      if (dist == "unif") {
        if (!any(names(argaddit) == "min")) {
          min <- readline(gettext("Insert the value of 'min' argument: ", domain = "R-leem"))
          argaddit$min <- as.numeric(min)
        }
        if (!any(names(argaddit) == "max")) {
          max <- readline(gettext("Insert the value of 'max' argument: ", domain = "R-leem"))
          argaddit$max <- as.numeric(max)
        }
        min <- argaddit$min
        max <- argaddit$max

        if (gui == "plot") {
          plotpunifarplot(q, min, max, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < min) trunc(q[1] - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > max) ceiling(q[2] + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
          manipulate::manipulate(plotpunifarrstudio(q1, q2, min, max, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 min = manipulate::slider(0, min + 5 * min, min),
                                 max = manipulate::slider(min, max + 5 * min, max))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }

        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(punif(q = q[1], min, max) + punif(q = q[2] - 1, min, max, lower.tail = FALSE),
                      digits = rounding)
      }
      if (dist == "wilcox") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        m <- argaddit$m
        n <- argaddit$n
        if (gui == "plot") {
          plotpwilcoxarplot(q, m, n, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < m+n) trunc(q[1] - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > m+n) ceiling(q[1] + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
          manipulate::manipulate(plotpwilcoxarrstudio(q1, q2, m, n, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <- round((pwilcox(q = q[1], m, n, lower.tail = T)) + (pwilcox(q = q[2], m, n, lower.tail = F)),
                      digits = rounding)
      }
      if (dist == "signrank") {
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        n <- argaddit$n
        if (gui == "plot") {
          plotpswilcoxarplot(q, n, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < n) trunc(q[1] - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > n) ceiling(q[1] + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
          manipulate::manipulate(plotpswilcoxarrstudio(q1, q2, n, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <- round((psignrank(q = q[1], n, lower.tail = T)) + (psignrank(q = q[2], n, lower.tail = F)),
                      digits = rounding)
      }

    }
    if (any(attr(q, "region") == regionb)) {
      if (dist == "normal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) {
          stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
        }
        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        mu <- argaddit$mean
        sigma <- argaddit$sd
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
        # Compute the desired probability
        prob <- pnorm(q = q[2], mean = mu, sd=sigma) - pnorm(q = q[1], mean = mu, sd=sigma)
      }
      if (dist == "t-student") {
        if (!any(names(argaddit) == "ncp")) {
          argaddit$ncp <- 0
        }
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Enter the value of 'df' argument:", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }
        nu <- argaddit$df
        ncp <- argaddit$ncp

        #Auxiliary Arguments
        llower <- if (nu <= 2) q[1] - 8 - 2 * abs(ncp) else q[1]  - 4 * .erro_padrao_t_nc(nu, ncp)
        lupper <- if (nu <= 2) q[2] + 8 + 2 * abs(ncp) else q[2] + 4 * .erro_padrao_t_nc(nu, ncp)
        # Function
        if (gui == "plot" ) {
          plotptstudentbrplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotptstudentbrrstudio(q1, q2, df, ncp, rounding, main, q),
                                 q1 = manipulate::slider(llower, q2, q[1]),
                                 q2 = manipulate::slider(q1, lupper, q[2]),
                                 df = manipulate::slider(1, nu + 100, nu),
                                 ncp = manipulate::slider(ncp - 5, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          # on.exit(options(war))

          .tkplotleemtstudent4(q[1], q[2], nu, ncp, rounding, main, llower, lupper, q)

          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # if (gui == "shiny") {
        #   # # Environment of package
        #   # envleem <- new.env(parent = base::emptyenv())
        #   # assign("shinyaux", NULL, envir = envleem)
        #   # assign("tk_q2", NULL, envir = envleem)
        #   # assign("tk_df", NULL, envir = envleem)
        #   # nu <- argaddit$df
        #   # prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
        #   # plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
        #   #   q[1] <- q1
        #   #   q[2] <- q2
        #   #   plotcurve(q, df)
        #   # }
        #   # shinyaux <<- function(...) {
        #   #   require(shiny)
        #   #   # Define UI for application that draws a histogram
        #   #   ui <- fluidPage(
        #   #
        #   #     # Application title
        #   #     titlePanel(gettext("t-Student distribution", domain = "R-leem")),
        #   #
        #   #     # Sidebar with a slider input for number of bins
        #   #     sidebarLayout(
        #   #       sidebarPanel(
        #   #         sliderInput("q1",
        #   #                     "Lower limit:",
        #   #                     min = -6,
        #   #                     max = q[2],
        #   #                     value = q[1])
        #   #       ),
        #   #
        #   #       # Show a plot of the generated distribution
        #   #       mainPanel(
        #   #         plotOutput("distPlot")
        #   #       )
        #   #     )
        #   #   )
        #   #
        #   #   # Define server logic required to draw a histogram
        #   #   server <- function(input, output) {
        #   #     prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
        #   #     plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
        #   #       q[1] <- q1
        #   #       q[2] <- q2
        #   #       plotcurve(q, df)
        #   #     }
        #   #     output$distPlot <- renderPlot({
        #   #       plotcurveaux(q1 = input$q1, q2=q[2], df=nu)
        #   #     })
        #   #   }
        #   #
        #   #   # Run the application
        #   #   shinyApp(ui = ui, server = server)
        #   # }
        #   # shinyaux()
        # }
        # Compute the desired probability
        prob <- pt(q[2], df = nu) - pt(q[1], df = nu)
      }
      if (dist == "chisq") {
        if (!any(names(argaddit) == "ncp")) {
          ncp <-0
          argaddit$ncp <- as.numeric(ncp)
        }
        if (!any(names(argaddit) == "df")) {
          df <- readline(gettext("Insert the value of 'df' argument: ", domain = "R-leem"))
          argaddit$df <- as.numeric(df)
        }
        ncp <- argaddit$ncp
        nu <- argaddit$df
        sig4n <- nu + ncp - 4 * sqrt(2 * (nu + 2 * ncp)) # mu - 4 * sigma
        sig4p <- nu + ncp + 5 * sqrt(2 * (nu + 2 * ncp)) # mu + 5 * sigma
        minimo <- if (sig4n < 0 | sig4n > q[1]) 0 else sig4n
        maximo <- if (sig4p < q[2]) q[2] + sig4p else sig4p

        if (argaddit$ncp < 0 ) stop("The 'ncp' argument must be greater than zero!", call. = FALSE, domain = "R-leem")

        if (gui == "plot") {
          plotpchisqbrplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpchisqbrrstudio(q1, q2, nu, ncp, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[1], maximo, q[2]),
                                 df = manipulate::slider(1, nu * 10, nu),
                                 ncp = manipulate::slider(0, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          # on.exit(options(war))

          .tkplotleemchisq4(q[1], q[2], nu, ncp, rounding, main, minimo, maximo, q)

          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        prob <- pchisq(q = q[2], df = nu, ncp= ncp) - pchisq(q = q[1], df = nu, ncp = ncp)
      }
      if (dist == "f") {
        if (!any(names(argaddit) == "df1")) {
          df1 <- readline(gettext("Insert the value of 'df1' argument: ", domain = "R-leem"))
          argaddit$df1 <- as.numeric(df1)
        }
        if (!any(names(argaddit) == "df2")) {
          df2 <- readline(gettext("Insert the value of 'df2' argument: ", domain = "R-leem"))
          argaddit$df2 <- as.numeric(df2)
        }

        if (argaddit$df1 <= 0) stop("The df1 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")
        if (argaddit$df2 <= 0) stop("The df2 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")

        df1 <- argaddit$df1
        df2 <- argaddit$df2
        # Auxiliar variables
        minimo <- if (q[1] >= df1 - 4 * df2) q[1] - 4 * df2 else 0
        maximo <- if (q[2] > df1 + 4 * df2) q[2] + 4 * df2 else df1 + 4 * df2

        if (gui == "plot") {
          plotpfbrplot(q, df1, df2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpfbrrstudio(q1, q2, df1, df2, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 20, q[2]),
                                 df1 = manipulate::slider(1, df1  * 2 , df1),
                                 df2 = manipulate::slider(1, df2 * 2, df2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # Calculates the desired probability
        prob <- pf(q = q[2], df1, df2) - pf(q = q[1], df1, df2)

      }
      if (dist == "gumbel") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
        maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location

        if (gui == "plot" ) {
          plotpgumbelbrplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgumbelbrrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pgumbel(q = max(q[1],q[2]), location, scale) - pgumbel(q = min(q[1],q[2]), location, scale)
      }
      if (dist == "beta") {
        if (!any(names(argaddit) == "alpha")) {
          alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
          argaddit$alpha <- as.numeric(alpha)
        }
        if (!any(names(argaddit) == "beta")) {
          beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
          argaddit$beta <- as.numeric(beta)
        }
        if (q[1]<0 || q[2]>1 || q[1]>1 || q[2]<0){
          stop("The quantile argument must be on the interval 0 and 1!",  call. = FALSE, domain = "R-leem")
        }
        shape1 <- argaddit$alpha
        shape2 <- argaddit$beta
        if (gui == "plot") {
          plotpbetabrplot(q, shape1, shape2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbetabrrstudio(q1, q2, shape1, shape2, rounding, main, q),
                                 q1 = manipulate::slider(q[1], 1, q[1]),
                                 q2 = manipulate::slider(q[2], 1, q[2]),
                                 shape1 = manipulate::slider(shape1, shape1 + 5 * shape1, shape1),
                                 shape2 = manipulate::slider(shape2, shape2 + 5 * shape2, shape2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbeta(q = q[2], shape1, shape2) - pbeta(q = q[1], shape1, shape2)
      }
      if (dist == "exp") {
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument: ",
                                   domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        rate <- argaddit$rate

        if (gui == "plot") {
          plotpexpbrplot(q, rate, rounding, main)
        }
        if (gui == "rstudio") {
          rmax <- q[2] + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          manipulate::manipulate(plotpexpbrrstudio(q1, q2, rate, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 rate = manipulate::slider(rate, rate + 5 * rate, rate))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- (pexp(q = q[1], rate = rate) - pexp(q = q[2],rate = rate))*-1
      }
      if (dist == "gamma") {
        if (!any(names(argaddit) == "shape")) {
          shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
          argaddit$shape <- as.numeric(shape)
        }
        if (!any(names(argaddit) == "rate")) {
          rate <- readline(gettext("Insert the value of 'rate' argument (to skip press enter): ", domain = "R-leem"))
          argaddit$rate <- as.numeric(rate)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument (to skip press enter): ", domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        shape <- argaddit$shape
        rate <- argaddit$rate
        scale <- argaddit$scale
        if (gui == "plot") {
          plotpgammabrplot(q, shape, rate, scale, rounding, main)
        }
        if (gui == "rstudio") {
          if (is.na(rate)){
            auxarg <- scale
            minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
            maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammabrrstudio(q1, q2, shape, rate, scale, rounding, main, q),
                                   q1 = manipulate::slider(minimo, q[2], q[1]),
                                   q2 = manipulate::slider(q[2], maximo, q[2]),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   scale = manipulate::slider(scale, scale + 4 * sqrt(scale), scale))
          }
          if (is.na(scale)){
            auxarg <- rate
            minimo <- if (q[1] <= auxarg - 4 * sqrt(auxarg)) q[1] - 4 * sqrt(auxarg) else 0
            maximo <- if (q[2] > auxarg + 4 * sqrt(auxarg)) q[2] + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammabrrstudio(q1, q2, shape, rate, scale, rounding, main, q),
                                   q1 = manipulate::slider(minimo, q[2], q[1]),
                                   q2 = manipulate::slider(q[2], maximo, q[2]),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   rate = manipulate::slider(rate, rate + 4 * sqrt(rate), rate))
          }
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if(is.na(scale)){
          prob <- round(pgamma(q = q[2], shape, rate = rate) - pgamma(q = q[1], shape, rate = rate),rounding)
        }
        if (is.na(rate)){
          prob <- round(pgamma(q = q[2], shape, scale = scale) - pgamma(q = q[1], shape, scale = scale),rounding)
        }
      }
      if (dist == "cauchy") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * scale) q[1] - 10 * scale else scale - 10 * scale
        maximo <- if (q[2] > scale + 10 * scale) q[2] + 10 * scale else scale + 10 * scale

        if (gui == "plot" ) {
          plotpcauchybrplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpcauchybrrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pcauchy(q = max(q[1],q[2]), location, scale) - pcauchy(q = min(q[1],q[2]), location, scale)
      }
      if (dist == "logis") {
        if (!any(names(argaddit) == "location")) {
          location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
          argaddit$location <- as.numeric(location)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!",
                                       call. = FALSE, domain = "R-leem")
        location <- argaddit$location
        scale <- argaddit$scale

        minimo <- if (q[1] <=  scale - 10 * location) q[1] - 10 * location else scale - 10 * location
        maximo <- if (q[2] > scale + 10 * location) q[2] + 10 * location else scale + 10 * location

        if (gui == "plot" ) {
          plotplogisbrplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplogisbrrstudio(q1 , q2 , location, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plogis(q = max(q[1],q[2]), location, scale) - plogis(q = min(q[1],q[2]), location, scale)
      }
      if (dist == "lnormal") {
        if (!any(names(argaddit) == "mean")) {
          mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
          argaddit$mean <- as.numeric(mean)
        }
        if (!any(names(argaddit) == "sd")) {
          sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
          argaddit$sd <- as.numeric(sd)
        }
        if (argaddit$sd <= 0 ) {
          stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
        }
        # Auxiliar variables
        minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        mu <- argaddit$mean
        sigma <- argaddit$sd
        if (gui == "plot") {
          plotplnormalbrplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplnormalbrrstudio(q1, q2, mean, sd, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plnorm(q = q[2], meanlog = mu, sdlog=sigma) - plnorm(q = q[1], meanlog = mu, sdlog=sigma)
      }
      if (dist == "tukey") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      if (dist == "weibull") {
        if (!any(names(argaddit) == "shape")) {
          shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
          argaddit$shape <- as.numeric(shape)
        }
        if (!any(names(argaddit) == "scale")) {
          scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
          argaddit$scale <- as.numeric(scale)
        }
        shape <- argaddit$shape
        scale <- argaddit$scale

        if (gui == "plot") {
          plotpweibullbrplot(q, shape, scale, rounding, main)
        }
        if (gui == "rstudio") {
          minimo <- if (q[1] <= shape - 4 * shape) q[1] - 4 * shape else 0
          maximo <- if (q[2] > shape + 4 * shape) q[2] + 4 * shape else shape + 4 * shape
          manipulate::manipulate(plotpweibullbrrstudio(q1, q2, shape, scale, rounding, main, q),
                                 q1 = manipulate::slider(minimo, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], maximo, q[2]),
                                 shape = manipulate::slider(shape, shape + 5 * sqrt(shape), shape),
                                 scale = manipulate::slider(scale, scale + 5 * sqrt(scale), scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pweibull(q = q[2], shape, scale) - pweibull(q = q[1], shape, scale)
      }
      if (dist == "poisson") {
        if (!any(names(argaddit) == "lambda")) {
          lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }
        while (argaddit$lambda <= 0) {
          lambda <- readline(gettext("Please, Insert the value of 'lambda' greater then 0: ", domain = "R-leem"))
          argaddit$lambda <- as.numeric(lambda)
        }

        lambda <- argaddit$lambda
        rmax <- ceiling(q[2] + 4 * sqrt(lambda))
        rmin <- ceiling(lambda - 4 * sqrt(lambda))
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)

        if (gui == "plot") {
          plotppoissonbrplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonbrrstudio(q1, q2, lambda, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 lambda = manipulate::slider(1, lambda+30, lambda))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(ppois(q = q[2], lambda = lambda) - ppois(q = q[1],lambda = lambda), digits = rounding)
      }
      if (dist == "binomial") {
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }

        size <- argaddit$size
        prob <- argaddit$prob

        if (gui == "plot") {
          plotpbinomialbrplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialbrrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(0, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], q[2]+size, q[2]),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # readjusting the range
        ## ab-region
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(pbinom(q = q[2], size, prob) - pbinom(q = q[1],size, prob), digits = rounding)
      }
      if (dist == "nbinom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        if (argaddit$prob > 1 || argaddit$prob < 0) {
          stop("The 'prob' argument must be between zero and one!", call. = FALSE, domain = "R-leem")
        }
        if (!any(names(argaddit) == "size")) {
          size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
          argaddit$size <- as.numeric(size)
        }
        if (argaddit$size < 0) {
          stop("The 'size' argument must be higther then zero!", call. = FALSE, domain = "R-leem")
        }
        prob <- argaddit$prob
        size <- argaddit$size
        if (gui == "plot" ) {
          plotpnbinombrplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < size) trunc(q[1] - 4 * sqrt(size)) else trunc(size - 4 * sqrt(size))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > size) ceiling(q[2] + 4 * sqrt(size)) else ceiling(size + 4 * sqrt(size))
          manipulate::manipulate(plotpnbinombrrstudio(q1, q2, size, prob, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 size = manipulate::slider(1, size + 4 * size, size),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <-  round(pnbinom(q = q[2], size, prob) - pnbinom(q = q[1], size, prob),
                       digits = rounding)
      }
      if (dist == "hyper") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        if (!any(names(argaddit) == "k")) {
          k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
          argaddit$k <- as.numeric(k)
        }
        m <- argaddit$m
        n <- argaddit$n
        k <- argaddit$k
        if (gui == "plot") {
          plotphyperbrplot(q, m, n, k, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < k) trunc(q[1] - 4 * sqrt(k)) else trunc(k - 4 * sqrt(k))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > k) ceiling(q[1] + 4 * sqrt(k)) else ceiling(k + 4 * sqrt(k))
          manipulate::manipulate(plotphyperbrrstudio(q1, q2, m, n, k, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n),
                                 k = manipulate::slider(k, k + 5 * k, k))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <-  round(phyper(q = q[2], m, n, k) - phyper(q = q[1], m, n, k), digits = rounding)
        }
      if (dist == "geom") {
        if (!any(names(argaddit) == "prob")) {
          prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
          argaddit$prob <- as.numeric(prob)
        }
        prob <- argaddit$prob
        if (gui == "plot") {
          plotpgeombrplot(q, prob, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < 10*prob) trunc(q[1] - 4 * sqrt(10*prob)) else trunc(prob - 4 * sqrt(10*prob))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > 10*prob) ceiling(q[2] + 4 * sqrt(10*prob)) else ceiling(10*prob + 4 * sqrt(10*prob))
          manipulate::manipulate(plotpgeombrrstudio(q1, q2, prob, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <- round(pgeom(q[2], prob) - pgeom(q[1], prob), rounding)
      }
      if (dist == "unif") {
        if (!any(names(argaddit) == "min")) {
          min <- readline(gettext("Insert the value of 'min' argument: ", domain = "R-leem"))
          argaddit$min <- as.numeric(min)
        }
        if (!any(names(argaddit) == "max")) {
          max <- readline(gettext("Insert the value of 'max' argument: ", domain = "R-leem"))
          argaddit$max <- as.numeric(max)
        }

        min <- argaddit$min
        max <- argaddit$max

        if (gui == "plot") {
          plotpunifbrplot(q, min, max, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < min) trunc(q[1] - 4 * sqrt(min)) else trunc(min - 4 * sqrt(min))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > max) ceiling(q[2] + 4 * sqrt(max)) else ceiling(max + 4 * sqrt(max))
          manipulate::manipulate(plotpunifbrrstudio(q1, q2, min, max, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 min = manipulate::slider(0, min + 5 * min, min),
                                 max = manipulate::slider(min, max + 5 * min, max))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        # readjusting the range
        ## ab-region
        # readjusting the range
        ## ab-region
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }
        prob <- round(punif(q = q[2], min, max) - punif(q = q[1], min, max), digits = rounding)
      }
      if (dist == "wilcox") {
        if (!any(names(argaddit) == "m")) {
          m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
          argaddit$m <- as.numeric(m)
        }
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        m <- argaddit$m
        n <- argaddit$n
        if (gui == "plot") {
          plotpwilcoxbrplot(q, m, n, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < m+n) trunc(q[1] - 4 * sqrt(m+n)) else trunc(m+n - 4 * sqrt(m+n))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > m+n) ceiling(q[1] + 4 * sqrt(m+n)) else ceiling(m+n + 4 * sqrt(m+n))
          manipulate::manipulate(plotpwilcoxbrrstudio(q1, q2, m, n, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <-  round(pwilcox(q = q[2], m, n) - pwilcox(q = q[1], m, n), digits = rounding)
      }
      if (dist == "signrank") {
        if (!any(names(argaddit) == "n")) {
          n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
          argaddit$n <- as.numeric(n)
        }
        n <- argaddit$n
        if (gui == "plot") {
          plotpswilcoxbrplot(q, n, rounding, main)
        }
        if (gui == "rstudio") {
          rmin <- if (q[1] < n) trunc(q[1] - 4 * sqrt(n)) else trunc(n - 4 * sqrt(n))
          if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
          rmax <- if (q[2] > n) ceiling(q[1] + 4 * sqrt(n)) else ceiling(n + 4 * sqrt(n))
          manipulate::manipulate(plotpswilcoxbrrstudio(q1, q2, n, rounding, main, q),
                                 q1 = manipulate::slider(rmin, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], rmax, q[2]),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if (is.double(q)) {
          if (attr(q, "region") == "region5") {
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region1") {
            q[1] <- q[1] - 1
            q[2] <- q[2] + 1
          }
          if (attr(q, "region") == "region6") {
            q[1] <- q[1] - 1
          }
          ## b-region
          if (attr(q, "region") == "region7") {
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region2") {
            q[1] <- q[1] + 1
            q[2] <- q[2] - 1
          }
          if (attr(q, "region") == "region8") {
            q[1] <- q[1] + 1
          }
          if (q[1] >= q[2]) {
            saida <- paste0("\nThis was equivalent to: \n", "- Lower limit: ", q[1], "\n", "- Upper limit: ", q[2], "\n\n")
            cat(saida)
            stop("Lower limit must be less than upper limit", call. = FALSE, domain = "R-leem")
          }
        }

        prob <-  round(psignrank(q = q[2], n) - psignrank(q = q[1], n), digits = rounding)
      }
    }}
  else {
    if (dist == "normal") {
      if (!any(names(argaddit) == "mean")) {
        mean <- readline(paste0(gettext("Enter the value of 'mean' argument:", domain = "R-leem"), " "))
        argaddit$mean <- as.numeric(mean)
      }
      if (!any(names(argaddit) == "sd")) {
        sd <- readline(paste0(gettext("Enter the value of 'sd' argument:", domain = "R-leem"), " "))
        argaddit$sd <- as.numeric(sd)
      }
      if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        mu <- argaddit$mean
        sigma <- argaddit$sd
        if (gui == "plot" ) {
          plotpnormallttplot(q, mu,sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpnormallttplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist normal com q de comp 1 (~/tkplotleem.R)
          .tkplotleemnormal(q, mu, sigma, rounding, main, minimo, maximo)


          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Compute the desired probability
        prob <- pnorm(q = q, mean = mu, sd = sigma)

      }
      else {
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        # Plot function
        mu <- argaddit$mean
        sigma <- argaddit$sd
        if (gui == "plot") {
          plotpnormalltfplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpnormalltfplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))

        }

        if (gui == "tcltk") {
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
      }
    }
    if (dist == "t-student") {
      if (!any(names(argaddit) == "ncp")) {
        argaddit$ncp <- 0
      }
      if (!any(names(argaddit) == "df")) {
        df <- readline(paste0(gettext("Enter the value of 'df' argument:", domain = "R-leem"), " "))
        argaddit$df <- as.numeric(df)
      }
      nu <- argaddit$df
      ncp <- argaddit$ncp
      lim <- if (abs(q) > 6) abs(q) + 2 + abs(ncp) * 3  else 6 + abs(ncp) * 3
      lim <- if(nu < 3) lim + nu * 20 else lim
      if (lower.tail) {
        if (gui == "plot" ) {
          plotptstudentlttplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotptstudentlttplot(q, nu, ncp, rounding, main),
                                 q = manipulate::slider(-lim, lim, q),
                                 nu = manipulate::slider(1, nu * 10, nu),
                                 ncp = manipulate::slider(ncp - 5 , ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist t-student com q de comp 1 (~/tkplotleem.R)
          .tkplotleemtstudent(q, nu, ncp, rounding, main, -lim, lim)


          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Calculates the desired probability
        prob <- pt(q = q, df = nu, ncp = ncp)
      } else {
        if (gui == "plot") {
          plotptstudentltfplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotptstudentltfplot(q, nu, ncp, rounding, main),
                                 q = manipulate::slider(-lim, lim, q),
                                 nu = manipulate::slider(1, nu * 10, nu),
                                 ncp = manipulate::slider(ncp - 5, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          on.exit(options(war))

          # Plot tk da dist t-student com q de comp 1 (~/tkplotleem.R)
          .tkplotleemtstudent2(q, nu, ncp, rounding, main, -lim, lim)

          # options(warn = - 1)
          # war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Calculates the desired probability
        prob <- pt(q = q, df = nu, ncp = ncp, lower.tail = FALSE)
        }

    }
    if (dist == "chisq") {
      if (q < 0 ) stop("The 'q' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (!any(names(argaddit) == "ncp")) {
        argaddit$ncp <- 0
      }
      if (!any(names(argaddit) == "df")) {
        df <- readline(gettext("Enter the value of 'df' argument:", domain = "R-leem"))
        argaddit$df <- as.numeric(df)
      }
      ncp <- argaddit$ncp
      nu <- argaddit$df

      if (argaddit$ncp < 0 ) stop("The 'ncp' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        # Auxiliar variables
        if (gui == "plot" ) {
          plotpchisqlttplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpchisqlttplot(q, nu, ncp, rounding, main),
                                 q = manipulate::slider(0, ncp + 4 * nu, q),
                                 nu = manipulate::slider(1, nu * 10, nu),
                                 ncp = manipulate::slider(0, ncp + 5, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist Qui-quadrado com q de comp 1 (~/tkplotleem.R)
          .tkplotleemchisq(q, nu, ncp, rounding, main, 0, ncp + 4 * nu)



          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))



        }
        prob <- pchisq(q = q, df = nu, ncp = ncp)
      }
      else {
        if (gui == "plot" ) {
          plotpchisqltfplot(q, nu, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpchisqltfplot(q, nu, ncp, rounding, main),
                                 q = manipulate::slider(0, ncp + 4 * nu, q),
                                 df = manipulate::slider(1, ncp + 2 * nu, nu),
                                 ncp = manipulate::slider(0, ncp + 2 * nu, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist Qui-quadrado com q de comp 1 (~/tkplotleem.R)
          .tkplotleemchisq02(q, nu, ncp, rounding, main, 0, ncp + 4 * nu)



          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        prob <- pchisq(q = q, df = nu, ncp = ncp, lower.tail = FALSE)
      }
    }
    if (dist == "f") {
      if (q < 0 ) stop("The 'q' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (ncp < 0 ) stop("The 'q' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (any(names(argaddit) == "ncp")) {
        if (argaddit$ncp < 0) stop("the 'ncp' argument must be a non-negative value!", call. = FALSE, domain = "R-leem")
      } else {
        argaddit$ncp <- 0
      }
      if (!any(names(argaddit) == "df1")) {
        df1 <- readline(gettext("Insert the value of 'df1' argument: ", domain = "R-leem"))
        argaddit$df1 <- as.numeric(df1)
      }
      if (!any(names(argaddit) == "df2")) {
        df2 <- readline(gettext("Insert the value of 'df2' argument: ", domain = "R-leem"))
        argaddit$df2 <- as.numeric(df2)
      }

      if (argaddit$df1 <= 0) stop("The df1 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (argaddit$df2 <= 0) stop("The df2 arguments must be greater than zero!", call. = FALSE, domain = "R-leem")

      df1 <- argaddit$df1
      df2 <- argaddit$df2
      ncp <- argaddit$ncp

      if (lower.tail) {
        if (gui == "plot") {
          plotpflttplot(q, df1, df2, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpflttplot(q, df1, df2, ncp, rounding, main),
                                 q = manipulate::slider(0, q + 10, q),
                                 df1 = manipulate::slider(1, df1  + 2 * df1, df1),
                                 df2 = manipulate::slider(1, df2 + 2 * df2, df2),
                                 ncp = manipulate::slider(0, ncp + 10, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist F com q de comp 1 (~/tkplotleem.R)
          .tkplotleemf(q, df1, df2, ncp, rounding, main)



          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Compute the desired probability
        prob <- pf(q, df1, df2, ncp)

      }
      else {
        if (gui == "plot") {
          plotpfltfplot(q, df1, df2, ncp, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpfltfplot(q, df1, df2, ncp, rounding, main),
                                 q = manipulate::slider(0, 10, q),
                                 df1 = manipulate::slider(1, df1  + 2 * df1, df1),
                                 df2 = manipulate::slider(1, df2 + 2 * df2, df2),
                                 ncp = manipulate::slider(0, ncp + 10, ncp))
        }
        if (gui == "tcltk") {
          # Desabilitar warnings global
          #options(warn = - 1)
          war <- options(warn = - 1)
          #on.exit(options(war))

          # Plot tk da dist F com q de comp 1 (~/tkplotleem.R)
          .tkplotleemf02(q, df1, df2, ncp, rounding, main)



          # Desabilitar warnings global
          #options(warn = - 1)
          #war <- options(warn = - 1)
          on.exit(options(war))
        }
        # Compute the desired probability
        prob <- pf(q, df1, df2, lower.tail = F)
      }
      }
    if (dist == "gumbel") {
      if (!any(names(argaddit) == "location")) {
        location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
        argaddit$location <- as.numeric(location)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      location <- argaddit$location
      scale <- argaddit$scale

      minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
      maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location

      if (lower.tail) {
        if (gui == "plot" ) {
          plotpgumbellttplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgumbellttplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))

        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pgumbel(q = q, location, scale)
      } else {
        if (gui == "plot" ) {
          plotpgumbelltfplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgumbelltfplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))

        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
      }
    }
    if (dist == "beta") {
      if (q<0 || q>1){
        stop("The quantile argument must be on the interval 0 and 1!",  call. = FALSE, domain = "R-leem")
      }
      if (!any(names(argaddit) == "alpha")) {
        alpha <- readline(gettext("Insert the value of 'alpha' argument: ",  domain = "R-leem"))
        argaddit$alpha <- as.numeric(alpha)
      }
      if (!any(names(argaddit) == "beta")) {
        beta <- readline(gettext("Insert the value of 'beta' argument: ",  domain = "R-leem"))
        argaddit$beta <- as.numeric(beta)
      }
      shape1 <- argaddit$alpha
      shape2 <- argaddit$beta
      if (lower.tail) {
        if (gui == "plot" ) {
          plotpbetalttplot(q, shape1 ,shape2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbetalttplot(q, shape1, shape2, rounding, main),
                                 q = manipulate::slider(q,  q + 4 * q, q),
                                 shape1 = manipulate::slider(shape1, shape1 + 5 * shape1, shape1),
                                 shape2 = manipulate::slider(shape2, shape2 + 5 * shape2, shape2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbeta(q ,shape1,  shape2)
      }
      else{
        if (gui == "plot") {
          plotpbetaltfplot(q, shape1, shape2, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbetalttplot(q, shape1, shape2, rounding, main),
                                 q = manipulate::slider(q,  q + 4 * q, q),
                                 shape1 = manipulate::slider(shape1, shape1 + 5 * shape1, shape1),
                                 shape2 = manipulate::slider(shape2, shape2 + 5 * shape2, shape2))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbeta(q, shape1, shape2, lower.tail = F)
      }
    }
    if (dist == "exp") {
      if (!any(names(argaddit) == "rate")) {
        rate <- readline("Insert the value of 'rate' argument: ")
        argaddit$rate <- as.numeric(rate)
      }
      rate <- argaddit$rate
      if (lower.tail) {
        if (gui == "plot") {
          plotpexplttplot(q, rate, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpexplttplot(q, rate, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 rate = manipulate::slider(rate, rate + 5 * rate, rate))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pexp(q = q, rate = rate)
      } else {
        if (gui == "plot") {
          plotpexpltfplot(q, rate, rounding, main)
        }
        if (gui == "rstudio") {
          rmax <- q + ceiling(1 / rate + 7 * sqrt(1 / rate^2))
          manipulate::manipulate(plotpexplttplot(q, rate, rounding, main),
                                 q = manipulate::slider(0, rmax, q),
                                 rate = manipulate::slider(rate, rate + 5 * rate, rate))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pexp(q = q, rate = rate, lower.tail = F)

      }
    }
    if (dist == "gamma") {
      if (!any(names(argaddit) == "shape")) {
        shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
        argaddit$shape <- as.numeric(shape)
      }
      if (!any(names(argaddit) == "rate")) {
        rate <- readline(gettext("Insert the value of 'rate' argument (to skip press enter): ", domain = "R-leem"))
        argaddit$rate <- as.numeric(rate)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument (to skip press enter): ", domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      shape <- argaddit$shape
      rate <- argaddit$rate
      scale <- argaddit$scale
      if (lower.tail) {
        # Auxiliar variables
        if (gui == "plot" ) {
          plotpgammalttplot(q, shape, rate, scale, rounding, main)
        }
        if (gui == "rstudio") {
          if (is.na(rate)){
            auxarg <- scale
            minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
            maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammalttplot(q, shape, rate, scale, rounding, main),
                                   q = manipulate::slider(minimo, maximo, q),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   scale = manipulate::slider(scale, scale + 4 * sqrt(scale), scale))
          }
          if (is.na(scale)){
            auxarg <- rate
            minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
            maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammalttplot(q, shape, rate, scale, rounding, main),
                                   q1 = manipulate::slider(minimo, maximo, q),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   rate = manipulate::slider(rate, rate + 4 * sqrt(rate), rate))
          }
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if(is.na(scale)){
          prob <- pgamma(q = q, shape, rate = rate)        }
        if (is.na(rate)){
          prob <- pgamma(q = q, shape, scale = scale)        }
      }
      else {
        if (gui == "plot") {
          plotpgammaltfplot(q, shape, rate, scale, rounding, main)
        }
        if (gui == "rstudio") {
          if (is.na(rate)){
            auxarg <- scale
            minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
            maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammaltfplot(q, shape, rate, scale, rounding, main),
                                   q = manipulate::slider(minimo, maximo, q),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   scale = manipulate::slider(scale, scale + 4 * sqrt(scale), scale))
          }
          if (is.na(scale)){
            auxarg <- rate
            minimo <- if (q <= auxarg - 4 * sqrt(auxarg)) q - 4 * sqrt(auxarg) else 0
            maximo <- if (q > auxarg + 4 * sqrt(auxarg)) q + 4 * sqrt(auxarg) else 4 * sqrt(auxarg)
            manipulate::manipulate(plotpgammaltfplot(q, shape, rate, scale, rounding, main),
                                   q1 = manipulate::slider(minimo, maximo, q),
                                   shape = manipulate::slider(shape, shape + 4 * sqrt(shape), shape),
                                   rate = manipulate::slider(rate, rate + 4 * sqrt(rate), rate))
          }
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        if(is.na(scale)){
          prob <- pgamma(q = q, shape, rate = rate, lower.tail = FALSE)        }
        if (is.na(rate)){
          prob <- pgamma(q = q, shape, scale = scale, lower.tail = FALSE)        }
      }
    }
    if (dist == "cauchy") {
      if (!any(names(argaddit) == "location")) {
        location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
        argaddit$location <- as.numeric(location)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      location <- argaddit$location
      scale <- argaddit$scale

      minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
      maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location

      if (lower.tail) {
        if (gui == "plot" ) {
          plotpcauchylttplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpcauchylttplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))

        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pcauchy(q = q, location, scale)
      } else {
        if (gui == "plot" ) {
          plotpcauchyltfplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpcauchyltfplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))

        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pcauchy(q = q, location, scale, lower.tail = FALSE)
      }
    }
    if (dist == "logis") {
      if (!any(names(argaddit) == "location")) {
        location <- readline(gettext("Insert the value of 'location' argument: ",  domain = "R-leem"))
        argaddit$location <- as.numeric(location)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument: ",  domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      if (argaddit$scale <= 0 ) stop("The 'scale' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      location <- argaddit$location
      scale <- argaddit$scale

      minimo <- if (q <=  scale - 10 * location) q - 10 * location else scale - 10 * location
      maximo <- if (q > scale + 10 * location) q + 10 * location else scale + 10 * location

      if (lower.tail) {
        if (gui == "plot" ) {
          plotplogislttplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplogislttplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 5 * scale, scale))

        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plogis(q = q, location, scale)
      } else {
        if (gui == "plot" ) {
          plotplogisltfplot(q, location, scale, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplogisltfplot(q, location, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 location = manipulate::slider(minimo, maximo, location ),
                                 scale = manipulate::slider(1, scale + 5 * scale, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plogis(q = q, location, scale, lower.tail = FALSE)
      }
    }
    if (dist == "lnormal") {
      if (!any(names(argaddit) == "mean")) {
        mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
        argaddit$mean <- as.numeric(mean)
      }
      if (!any(names(argaddit) == "sd")) {
        sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
        argaddit$sd <- as.numeric(sd)
      }
      if (argaddit$sd <= 0 ) stop("The 'sd' argument must be greater than zero!", call. = FALSE, domain = "R-leem")
      if (lower.tail) {
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        mu <- argaddit$mean
        sigma <- argaddit$sd
        if (gui == "plot" ) {
          plotplnormallttplot(q, mu,sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotlpnormallttplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plnorm(q = q, meanlog = mu, sdlog = sigma)

      }
      else {
        # Auxiliar variables
        minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
        maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
        # Plot function
        mu <- argaddit$mean
        sigma <- argaddit$sd
        if (gui == "plot") {
          plotplnormalltfplot(q, mu, sigma, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotplnormalltfplot(q, mean, sd, rounding, main),
                                 q = manipulate::slider(q, mu + 4 * sigma, q),
                                 mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                                 sd = manipulate::slider(sigma, sigma * 1.8, sigma))

        }

        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- plnorm(q = q, meanlog = mu, sdlog =sigma, lower.tail = F)
      }
    }
    if (dist == "tukey") {
      stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
    }
    if (dist == "weibull") {
      if (!any(names(argaddit) == "shape")) {
        shape <- readline(gettext("Insert the value of 'shape' argument: ", domain = "R-leem"))
        argaddit$shape <- as.numeric(shape)
      }
      if (!any(names(argaddit) == "scale")) {
        scale <- readline(gettext("Insert the value of 'scale' argument: ", domain = "R-leem"))
        argaddit$scale <- as.numeric(scale)
      }
      shape <- argaddit$shape
      scale <- argaddit$scale

      if (lower.tail) {
        if (gui == "plot" ) {
          plotpweibulllttplot(q, shape, scale, rounding, main)
        }
        if (gui == "rstudio") {
          minimo <- if (q <= shape - 4 * shape) q - 4 * shape else 0
          maximo <- if (q > shape + 4 * shape) q + 4 * shape else shape + 4 * shape
          manipulate::manipulate(plotpweibulllttplot(q, shape, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 shape = manipulate::slider(shape, shape + 5 * shape, shape),
                                 scale = manipulate::slider(scale, scale + 5 * scale, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pweibull(q = q, shape, scale)

      }
      else {
        if (gui == "plot") {
          plotpweibullltfplot(q, shape, scale, rounding, main)
        }
        if (gui == "rstudio") {
          minimo <- if (q <= shape - 4 * shape) q - 4 * shape else 0
          maximo <- if (q > shape + 4 * shape) q + 4 * shape else shape + 4 * shape
          manipulate::manipulate(plotpweibullltfplot(q, shape, scale, rounding, main),
                                 q = manipulate::slider(minimo, maximo, q),
                                 shape = manipulate::slider(shape, shape + 5 * shape, shape),
                                 scale = manipulate::slider(scale, scale + 5 * scale, scale))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pweibull(q = q, shape, scale, lower.tail = F)
      }
    }
    if (dist == "poisson") {
      if (!any(names(argaddit) == "lambda")) {
        lambda <- readline(gettext("Insert the value of 'lambda' argument: ", domain = "R-leem"))
        argaddit$lambda <- as.numeric(lambda)
      }
      lambda <- argaddit$lambda
      rmin <- ceiling(lambda - 4 * sqrt(lambda))
      if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
      rmax <- ceiling(lambda + 4 * sqrt(lambda))
      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotppoissonlttplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonlttplot(q, lambda, rounding, main),
                                 q = manipulate::slider(0, lambda+30, q),
                                 lambda = manipulate::slider(1, lambda + 30, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- ppois(q = q, lambda = lambda)
      }
      if (isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotppoissonltfplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonltfplot(q, lambda, rounding, main),
                                 q = manipulate::slider(0, lambda+30, q),
                                 lambda = manipulate::slider(1, lambda + 30, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- ppois(q = q, lambda = lambda, lower.tail = FALSE)
      }
      if(is.null(lower.tail)){
        if (gui == "plot") {
          plotppoissonltnplot(q, lambda, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotppoissonltnplot(q, lambda, rounding, main),
                                 q = manipulate::slider(0, lambda+30, q),
                                 lambda = manipulate::slider(1, lambda + 30, lambda)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- dpois(q, lambda=lambda)
      }
    }
    if (dist == "binomial") {
      if (!any(names(argaddit) == "size")) {
        size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
        argaddit$size <- as.numeric(size)
      }
      if (!any(names(argaddit) == "prob")) {
        prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
        argaddit$prob <- as.numeric(prob)
      }
      size <- argaddit$size
      prob <- argaddit$prob

      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotpbinomiallttplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomiallttplot(q, size, prob, rounding, main),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbinom(q, size, prob)
      }
      if(isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotpbinomialltfplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialltfplot(q, size, prob, rounding, main),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- pbinom(q, size, prob, lower.tail = FALSE)
      }
      if(is.null(lower.tail)){
        if (gui == "plot") {
          plotpbinomialltnplot(q, size, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpbinomialltnplot(q, size, prob, rounding, main),
                                 q = manipulate::slider(0, size, q),
                                 size = manipulate::slider(1, size+30, size),
                                 prob = manipulate::slider(0, 1, prob)
          )
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- dbinom(q, size, prob)
      }
    }
    if (dist == "nbinom") {
    if (!any(names(argaddit) == "prob")) {
      prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
      argaddit$prob <- as.numeric(prob)
    }
    if (argaddit$prob > 1 || argaddit$prob < 0) {
      stop("The 'prob' argument must be between zero and one!", call. = FALSE, domain = "R-leem")
    }
    if (!any(names(argaddit) == "size")) {
      size <- readline(gettext("Insert the value of 'size' argument: ", domain = "R-leem"))
      argaddit$size <- as.numeric(size)
    }
    if (argaddit$size < 0) {
      stop("The 'size' argument must be higther then zero!", call. = FALSE, domain = "R-leem")
    }
    prob <- argaddit$prob
    size <- argaddit$size
    if (isTRUE(lower.tail)) {
      if (gui == "plot" ) {
        plotpnbinomiallttplot(q, size, prob, rounding, main)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotpnbinomiallttplot(q, size, prob, rounding, main),
                               q = manipulate::slider(q, q + 4 * q, q),
                               size = manipulate::slider(size, size + 5 * size, size),
                               prob = manipulate::slider(0, 1, prob))
      }
      if (gui == "tcltk") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      prob <- pnbinom(q, size, prob)
    }
    if (isFALSE(lower.tail)) {
      if (gui == "plot" ) {
        plotpnbinomialltfplot(q, size, prob, rounding, main)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotpnbinomialltfplot(q, size, prob, rounding, main),
                               q = manipulate::slider(q, q + 4 * q, q),
                               size = manipulate::slider(size, size + 5 * size, size),
                               prob = manipulate::slider(0, 1, prob))
      }
      if (gui == "tcltk") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      prob <- pnbinom(q, size, prob, lower.tail = FALSE)
    }
    if (is.null(lower.tail)) {
      if (gui == "plot" ) {
        plotpnbinomialltnplot(q, size, prob, rounding, main)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotpnbinomialltnplot(q, size, prob, rounding, main),
                               q = manipulate::slider(q, q + 4 * q, q),
                               size = manipulate::slider(size, size + 5 * size, size),
                               prob = manipulate::slider(0, 1, prob))
      }
      if (gui == "tcltk") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      prob <- dnbinom(q, size, prob)
    }
  }
    if (dist == "hyper") {
      if (!any(names(argaddit) == "m")) {
        m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
        argaddit$m <- as.numeric(m)
      }
      if (!any(names(argaddit) == "n")) {
        n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
        argaddit$n <- as.numeric(n)
      }
      if (!any(names(argaddit) == "k")) {
        k <- readline(gettext("Insert the value of 'k' argument: ", domain = "R-leem"))
        argaddit$k <- as.numeric(k)
      }
      m <- argaddit$m
      n <- argaddit$n
      k <- argaddit$k
      if (isTRUE(lower.tail)) {
      if (gui == "plot") {
        plotphyperlttplot(q, m, n, k, rounding, main)
      }
        if (gui == "rstudio") {
          manipulate::manipulate(plotphyperlttplot(q, m, n, k, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n),
                                 k = manipulate::slider(k, k + 5 * k, k))
        }
        if (gui == "tcltk") {
        stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
      }
      prob <- round(phyper(q, m, n, k), digits = rounding)
      }
      if (isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotphyperltfplot(q, m, n, k, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotphyperltfplot(q, m, n, k, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n),
                                 k = manipulate::slider(k, k + 5 * k, k))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(phyper(q, m, n, k, lower.tail = FALSE), digits = rounding)
      }
      if (is.null(lower.tail)) {
        if (gui == "plot") {
          plotphyperltnplot(q, m, n, k, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotphyperltnplot(q, m, n, k, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n),
                                 k = manipulate::slider(k, k + 5 * k, k))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(dhyper(q, m, n, k), digits = rounding)
      }

    }
    if (dist == "geom") {
      if (!any(names(argaddit) == "prob")) {
        prob <- readline(gettext("Insert the value of 'prob' argument: ", domain = "R-leem"))
        argaddit$prob <- as.numeric(prob)
      }
      prob <- argaddit$prob
      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotpgeomlttplot(q, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgeomlttplot(q, prob, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(pgeom(q, prob), rounding)
      }
      if (isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotpgeomltfplot(q, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgeomltfplot(q, prob, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(pgeom(q, prob, lower.tail = FALSE), rounding)
      }
      if(is.null(lower.tail)){
        if (gui == "plot") {
          plotpgeomltnplot(q, prob, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpgeomltnplot(q, prob, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 prob = manipulate::slider(0, 1, prob))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(dgeom(q, prob), rounding)
      }
    }
    if (dist == "unif") {
      if (!any(names(argaddit) == "min")) {
        min <- readline(gettext("Insert the value of 'min' argument: ", domain = "R-leem"))
        argaddit$min <- as.numeric(min)
      }
      if (!any(names(argaddit) == "max")) {
        max <- readline(gettext("Insert the value of 'max' argument: ", domain = "R-leem"))
        argaddit$max <- as.numeric(max)
      }
      min <- argaddit$min
      max <- argaddit$max

      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotpuniflttplot(q, min, max, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpuniflttplot(q, min, max, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 min = manipulate::slider(0, min + 5 * min, min),
                                 max = manipulate::slider(min, max + 5 * max, max))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- punif(q, min, max)
      }
      if(isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotpunifltfplot(q, min, max, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpunifltfplot(q, min, max, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 min = manipulate::slider(0, min + 5 * min, min),
                                 max = manipulate::slider(min, max + 5 * max, max))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- punif(q, min, max, lower.tail = FALSE)
      }
      if(is.null(lower.tail)){
        if (gui == "plot") {
          plotpunifltnplot(q, min, max, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpunifltnplot(q, min, max, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 min = manipulate::slider(0, min + 5 * min, min),
                                 max = manipulate::slider(min, max + 5 * max, max))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- dunif(q, min, max)
      }
    }
    if (dist == "wilcox") {
      if (!any(names(argaddit) == "m")) {
        m <- readline(gettext("Insert the value of 'm' argument: ", domain = "R-leem"))
        argaddit$m <- as.numeric(m)
      }
      if (!any(names(argaddit) == "n")) {
        n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
        argaddit$n <- as.numeric(n)
      }
      m <- argaddit$m
      n <- argaddit$n
      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotpwilcoxlttplot(q, m, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpwilcoxlttplot(q, m, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(pwilcox(q, m, n), digits = rounding)
      }
      if (isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotpwilcoxltfplot(q, m, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpwilcoxltfplot(q, m, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(pwilcox(q, m, n, lower.tail = FALSE), digits = rounding)
      }
      if (is.null(lower.tail)) {
        if (gui == "plot") {
          plotpwilcoxltnplot(q, m, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpwilcoxltnplot(q, m, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 m = manipulate::slider(m, m + 5 * m, m),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(dwilcox(q, m, n), digits = rounding)
      }

    }
    if (dist == "signrank") {
      if (!any(names(argaddit) == "n")) {
        n <- readline(gettext("Insert the value of 'n' argument: ", domain = "R-leem"))
        argaddit$n <- as.numeric(n)
      }
      n <- argaddit$n
      if (isTRUE(lower.tail)) {
        if (gui == "plot") {
          plotpswilcoxlttplot(q, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpswilcoxlttplot(q, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(psignrank(q, n), digits = rounding)
      }
      if (isFALSE(lower.tail)) {
        if (gui == "plot") {
          plotpswilcoxltfplot(q, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpswilcoxltfplot(q, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(psignrank(q, n, lower.tail = FALSE), digits = rounding)
      }
      if (is.null(lower.tail)) {
        if (gui == "plot") {
          plotpswilcoxltnplot(q, n, rounding, main)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotpswilcoxltnplot(q, n, rounding, main),
                                 q = manipulate::slider(q, q + 4 * q, q),
                                 n = manipulate::slider(n, n + 5 * n, n))
        }
        if (gui == "tcltk") {
          stop("Em desenvolvimento...", call. = FALSE, domain = "R-leem")
        }
        prob <- round(dsignrank(q, n), digits = rounding)
      }

    }

  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}
