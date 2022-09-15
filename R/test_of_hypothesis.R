#' Test of hypothesis
#'
#' Performs hypothesis testing for various parameters of one or more populations
#'
#' @param x R object. See in details.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param test character value. The options are: \code{"ttest"}, \code{"ztest"}, \code{"ptest"}, \code{"chitest"}, \code{"ftest"}, \code{"anova"}, \code{"friedman"}, \code{"kruskal"}, \code{"mann whitney"}.
#' @param h0 numeric value. The hypothesized parameter.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param alpha significance level of the test
#' @param exact a logical indicating whether you want to use the exact test or not. Default is \code{exact=TRUE}.
#' @param correct a logical indicating whether Yates' continuity correction should be applied where possible. This argument must be used when \code{exact = FALSE}.
#' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' @param plot a logical indicating whether you want a graph indicating the regions of rejection or not of the null hypothesis, as well as the test decision.
#'
#' @import crayon
#' @export
th <- function(x, y = NULL, test = "ztest", h0, alternative = c("two.sided",
               "less", "greater"), alpha = 0.05, exact = TRUE,
               correct = FALSE, paired = FALSE, plot = FALSE, ...) {
  alternative <- match.arg(alternative)
  argaddit <- list(...)
  if (missing(x)) {
    xfile <- file.choose(new = TRUE)
    x <- read.table(xfile, h = TRUE)
  }
  # Z test:
  if (test == "ztest") {
    if (!any(names(argaddit) == "sd")) {
      sdev <- readline("Insert the value of population standard deviation? ")
      sdev <- as.numeric(sdev)
    } else sdev <- argaddit$sd
    if (missing(h0)) {
      h0 <- readline("Insert the value of null hypothesis? ")
      h0 <- as.numeric(h0)
    }
    if (alternative == "two.sided") {
      if (is.null(y)) {
        title <- paste(gettext("  One Sample z-test (Two-sided test) \n", domain = "R-leem"))
        nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
                         sep = "")
        althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
                        sep = "")
        signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
                           sep = "")
        n <- length(x)
        ztest <- round((mean(x) - h0) / (sdev /sqrt(n)), 2)
        ztab <- round(qnorm(1 - alpha), 2)
        ztab <- round(c(-ztab, ztab), 2)
        pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
        if (abs(ztest) >= abs(ztab[2])) {
          decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
                                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
                                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
          decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
                            pvalue, " < alpha = ", alpha,
                            gettext(" then reject H0!", domain = "R-leem"), sep = "")
          conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
                              round(alpha * 100),
                              gettext("% probability", domain = "R-leem"), sep = "")
        } else {
          decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
                            abs(ztest), "| < |ztab = ", abs(ztab[2]),
                            gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
          decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
                            pvalue, " > alpha = ", alpha,
                            gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
          conlcusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
                              round(alpha * 100),
                              gettext("% probability", domain = "R-leem"), sep = "")
        }
        results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
                        alternative = alternative, title = title, nullhyp = nullhyp,
                        althyp = althyp, signlevel = signlevel, decision = decision,
                        decision2 = decision2, conclusion = conclusion)
      }
      if (plot == TRUE) {

      }
    }

    if (any(alternative == c("less", "l", "L"))) {
      print("less")
    }

    if (any(alternative == c("greater", "g", "G"))) {
      print("greater")
    }
  }
  attr(results, "output") <- "htest"
  class(results) <- "leem"
  results
}




