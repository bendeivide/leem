#' #' Test of hypothesis
#' #'
#' #' Performs hypothesis testing for various parameters of one or more populations
#' #'
#' #' @param x R object. See in details.
#' #' @param y an optional (non-empty) numeric vector of data values.
#' #' @param test character value. The options are: \code{"ttest"}, \code{"ztest"}, \code{"ptest"}, \code{"chitest"}, \code{"ftest"}, \code{"anova"}, \code{"friedman"}, \code{"kruskal"}, \code{"mann whitney"}.
#' #' @param h0 numeric value. The hypothesized parameter.
#' #' @param prop a logical indicating whether you want to use the proportion test of not. Default is \code{prop=FALSE}.
#' #' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' #' @param alpha significance level of the test
#' #' @param exact a logical indicating whether you want to use the exact test or not. Default is \code{exact=TRUE}.
#' #' @param correct a logical indicating whether Yates' continuity correction should be applied where possible. This argument must be used when \code{exact = FALSE}.
#' #' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' #' @param paired a logical indicating whether you want a paired t-test. Valid only for \code{test="ttest"}.
#' #' @param plot a logical indicating whether you want a graph indicating the regions of rejection or not of the null hypothesis, as well as the test decision.
#' #'
#' #' @import crayon
#' #' @export
#' th <- function(x, y = NULL, test = "ztest", h0, prop = FALSE,
#'                delta = 0, p, pa,
#'                alternative = c("two.sided","L",
#'                                                                 "less", "greater","G"),
#'                alpha = 0.05, exact = TRUE,
#'                correct = FALSE, paired = FALSE, plot = FALSE, ...) {
#'
#'   alternative <- match.arg(alternative)
#'   argaddit <- list(...)
#'
#'   if (missing(x)) {
#'     xfile <- file.choose(new = TRUE)
#'     x <- read.table(xfile, header = TRUE)
#'   }
#'   if (any(test ==  c("ztest", "z", "Z", "normal"))) {
#'     if(prop == TRUE){
#'         if (!any(names(argaddit) == "pa")) {
#'           pa <- readline("Insert the value of population sample part? ")
#'           pa <- as.numeric(pa)
#'         } else pa <- argaddit$pa
#'         if (missing(p)) {
#'           p <- readline("Insert the value of  proportion? ")
#'           p <- as.numeric(p)
#'         }else p <- argaddit$p
#'     }else{
#'       if (is.null(y)) {
#'       if (!any(names(argaddit) == "sd")) {
#'         sdev <- readline("Insert the value of population standard deviation? ")
#'         sdev <- as.numeric(sdev)
#'       } else sdev <- argaddit$sd
#'       if (missing(h0)) {
#'         h0 <- readline("Insert the value of null hypothesis? ")
#'         h0 <- as.numeric(h0)
#'       }
#'       }else{
#'         if (!any(names(argaddit) == "sd")) {
#'           sdev <- c(as.numeric(readline("Insert the value of population 1 standard deviation: ")),
#'                     as.numeric(readline("Insert the value of population 2 standard deviation: ")))
#'         }else if(length(argaddit$sd) < 1){
#'           sdev <- c(as.numeric(readline("Insert the value of population 1 standard deviation: ")),
#'                     as.numeric(readline("Insert the value of population 2 standard deviation: ")))
#'         }else sdev <- argaddit$sd
#'         if(sdev[1] > length(x) || sdev[2] > length(y)){
#'           stop("Error: Incorrect standard deviantion argument for 2 populations.", call. = FALSE, domain = "R-leem")
#'         }
#'         if ((length(x) > length(y) && sdev[1] <= sdev[2])||((length(x) < length(y) && sdev[1] >= sdev[2]))){
#'             stop("Error: Incorrect standard deviantion argument for 2 populations.", call. = FALSE, domain = "R-leem")
#'           }
#'       }
#'     }
#'     if (any(alternative == c("two.sided", "t", "T"))) {
#'       if (is.null(y)) {
#'         if(prop == TRUE){
#'           title <- paste(gettext("  One Sample z-test proportion (Two-sided test) \n", domain = "R-leem"))
#'           nullhyp <- paste(gettext("  H0: p= ", domain = "R-leem"), round(p, 2),
#'                            sep = "")
#'           althyp <- paste(gettext("  H1: p != ", domain = "R-leem"), round(p, 2),
#'                           sep = "")
#'           signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                              sep = "")
#'           n <- length(x)
#'           pa <- pa/n
#'           ztest <- round( (( pa - p ) / ( sqrt(  (p * ( 1-p) ) / n  )  )) , 2)
#'           ztab <- round(qnorm(1 - (alpha)/2), 2)
#'           ztab <- round(c(-ztab, ztab), 2)
#'           pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'           h0 <- p
#'         }else{
#'         title <- paste(gettext("  One Sample z-test (Two-sided test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ztest <- round((mean(x) - h0) / (sdev /sqrt(n)), 2)
#'         ztab <- round(qnorm(1 - (alpha)/2), 2)
#'         ztab <- round(c(-ztab, ztab), 2)
#'         pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'         }
#'         if (abs(ztest) >= abs(ztab[2])) {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'           title <- paste(gettext("  two Sample z-test (Two-sided test) \n", domain = "R-leem"))
#'           nullhyp <- paste(gettext("  H0: mu1 = mu2 ", domain = "R-leem"),sep ="")
#'           althyp <- paste(gettext("  H1: mu1 != mu2", domain = "R-leem"),sep ="")
#'           signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha           ,2),sep = "")
#'           n <- length(x)
#'           n1 <- length(y)
#'           ztest <- round(((mean(x) - mean(y))/sqrt(((sdev[1]^2)/n)-((sdev[2]^2)/n1))), 2)
#'           ztab <- round(qnorm(1 - (alpha)/2), 2)
#'           ztab <- round(c(-ztab, ztab), 2)
#'           pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'           if (abs(ztest) >= abs(ztab[2])) {
#'             decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                               abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                               gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'             decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                                pvalue, " < alpha = ", alpha,
#'                                gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'             decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                   abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'             conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                                 round(alpha * 100),
#'                                 gettext("% probability", domain = "R-leem"), sep = "")
#'             conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'           } else {
#'             decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                               abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                               gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'             decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                                pvalue, " > alpha = ", alpha,
#'                                gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'             decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                   abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'             conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                                 round(alpha * 100),
#'                                 gettext("% probability", domain = "R-leem"), sep = "")
#'             conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'           }
#'           results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                           alternative = alternative, title = title, nullhyp = nullhyp,
#'                           althyp = althyp, signlevel = signlevel, decision = decision,
#'                           decision2 = decision2, conclusion = conclusion)
#'         }
#'       if (plot == TRUE) {
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 * 1, ztab[1], by = 0.01)
#'         z <- seq(ztab[2],  4, by = 0.01)
#'         y <- seq( - 4, + 4, by = 0.01)
#'         fx <- dnorm(x, mean = 0, sd = 1)
#'         fz <- dnorm(z,mean = 0, sd = 1)
#'         fy <- dnorm(y, mean = 0, sd = 1)
#'         curve(dnorm(x, mean = 0, sd = 1),  - 4 , + 4 ,
#'               ylim = c(0.0, 1.2 * max(fx,fy,fz)),xlab = "X",
#'               ylab = expression(f[X](x)),
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#99ccff")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#cc0000")
#'         polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
#'                 col="#cc0000" )
#'         abline(v = ztab[1], lty=2, col = "#cc0000")
#'         abline(v = ztab[2], lty=2, col = "#cc0000")
#'         abline(v = ztest, lty=2, col = "blue")
#'         text(-2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ztab[1] ,0.14, -2.3, 0.27)
#'         text(2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ztab[2] ,0.14,2.3, 0.27)
#'         text(-1.6, 0.4, expression(bold("zTest")))
#'         arrows(ztest ,0.28, -1.4, 0.37)
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'         axis(side = 1, at = c(ztab[1],ztab[2]), font = 2, labels = FALSE,
#'              col.axis = "#cc0000", col.ticks = "#cc0000", col = "#559ee8")
#'         axis(side=1, at=as.character(c( - 4, ztab[1])), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'         axis(side=1, at=as.character(c(ztab[2], + 4)), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'         axis(side = 1, at = c(ztab[1],ztab[2]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'         axis(side = 1, at = ztest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ztest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'
#'         if(prop == TRUE){
#'           title("Test of proportion: Z-Test.")
#'         }else{
#'           title("Test of hypothesis: Z-Test.")
#'         }
#'         text(0,0.1,expression(bold("ACCEPT H0")), font = 2)
#'         text(ztab[1] - 1 , 0.1, "RRH0", font = 2)
#'         text(ztab[2] + 1 ,0.1,"RRH0", font = 2)
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'       }
#'     }
#'     if (any(alternative == c("less", "l", "L"))) {
#'       if (is.null(y)) {
#'         if(prop == TRUE){
#'           title <- paste(gettext("  One Sample z-test proportion (Less test) \n", domain = "R-leem"))
#'           nullhyp <- paste(gettext("  H0: p >= ", domain = "R-leem"), round(p, 2),
#'                            sep = "")
#'           althyp <- paste(gettext("  H1: p < ", domain = "R-leem"), round(p, 2),
#'                           sep = "")
#'           signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                              sep = "")
#'           n <- length(x)
#'           pa <- pa/n
#'           ztest <- round( (( pa - p ) / ( sqrt(  (p * ( 1-p) ) / n  )  )) , 2)
#'           ztab <- round(qnorm(1 - (alpha)), 2)
#'           ztab <- round(c(-ztab, ztab), 2)
#'           pvalue <- 2 * pnorm(abs(ztest), lower.tail = T)
#'           h0 <- p
#'         }else{
#'         title <- paste(gettext("  One Sample z-test (Less test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ztest <- round((mean(x) - h0) / (sdev /sqrt(n)), 2)
#'         ztab <- round(qnorm(1 - alpha), 2)
#'         ztab <- round(c(-ztab, ztab), 2)
#'         pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'         }
#'         if (abs(ztest) >= abs(ztab[2])) {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'
#'         } else {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         title <- paste(gettext("  two Sample z-test (Less-sided test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu1 >= mu2 ", domain = "R-leem"),sep ="")
#'         althyp <- paste(gettext("  H1: mu1 < mu2", domain = "R-leem"),sep ="")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha           ,2),sep = "")
#'         n <- length(x)
#'         n1 <- length(y)
#'         ztest <- round((mean(x) - mean(y)) /sqrt((sdev[1]^2/n)-(sdev[2]^2 /n1)), 2)
#'         ztab <- round(qnorm(1 - (alpha)), 2)
#'         ztab <- round(c(-ztab, ztab), 2)
#'         pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'         if (abs(ztest) >= abs(ztab[2])) {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         }
#'         results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       if (plot == TRUE) {
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 , ztab[1], by = 0.01)
#'         y <- seq(ztab[1],+ 4, by = 0.01)
#'         fx <- dnorm(x, mean = 0, sd = 1)
#'         fy <- dnorm(y, mean = 0, sd = 1)
#'         curve(dnorm(x, mean = 0, sd = 1), - 4 , + 4  ,
#'               ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#99ccff")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#cc0000")
#'         abline(v = ztab[1], lty=2, col = "red")
#'         text(-2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ztab[1] ,0.14, -2.3, 0.28)
#'         abline(v = ztest, lty=2, col = "blue")
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'
#'
#'         axis(side = 1, at = c(ztab[1], 4), font = 2, labels = FALSE, lwd.ticks = 0,
#'              col.axis = "#cc0000", col.ticks = "#cc0000", col = "#559ee8")
#'
#'         axis(side=1, at=as.character(c( -4, ztab[1])), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'
#'         axis(side = 1, at = c("",ztab[1]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'
#'         axis(side=1, at=as.character(c( "", ztab[1])), tick = TRUE, labels = FALSE,
#'              lwd.ticks = 1, col="#cc0000")
#'
#'         axis(side = 1, at = ztest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ztest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'
#'
#'         if(prop == TRUE){
#'           title("Test of proportion: Z-Test.")
#'         }else{
#'           title("Test of hypothesis: Z-Test.")
#'         }
#'         text(0, 0.1, expression(bold("ACCEPT H0")))
#'         text(-2.5, 0.1, expression(bold("RRH0")))
#'         arrows(ztest, 0.27, -1.3, 0.37)
#'         text(-1.3, 0.39, expression(bold("ztest")))
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'               }
#'     }
#'     if (any(alternative == c("greater", "g", "G"))) {
#'       if (is.null(y)) {
#'         if(prop == TRUE){
#'           title <- paste(gettext("  One Sample z-test proportion (Greater test) \n", domain = "R-leem"))
#'           nullhyp <- paste(gettext("  H0: p <= ", domain = "R-leem"), round(p, 2),
#'                            sep = "")
#'           althyp <- paste(gettext("  H1: p > ", domain = "R-leem"), round(p, 2),
#'                           sep = "")
#'           signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                              sep = "")
#'           n <- length(x)
#'           pa <- pa/n
#'           ztest <- round( (( pa - p ) / ( sqrt(  (p * ( 1-p) ) / n  )  )) , 2)
#'           ztab <- round(qnorm(1 - (alpha)), 2)
#'           ztab <- round(c(-ztab, ztab), 2)
#'           pvalue <- 2 * pnorm(abs(ztest), lower.tail = F)
#'           h0 <- p
#'         }else{
#'         title <- paste(gettext("  One Sample z-test (Greater test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ztest <- round((mean(x) - h0) / (sdev /sqrt(n)), 2)
#'         ztab <- round(qnorm(1 - alpha), 2)
#'         ztab <- round(c(-ztab, ztab), 2)
#'         pvalue <- 2 * pnorm(abs(ztest), lower.tail = FALSE)
#'         }
#'         if (abs(ztest) >= abs(ztab[2])) {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         title <- paste(gettext("  two Sample z-test (Greater-sided test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu1 <= mu2 ", domain = "R-leem"),sep ="")
#'         althyp <- paste(gettext("  H1: mu1 > mu2", domain = "R-leem"),sep ="")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha           ,2),sep = "")
#'         n <- length(x)
#'         n1 <- length(y)
#'         ztest <- round((mean(x) - mean(y)) /sqrt((sdev[1]^2/n)-(sdev[2]^2 /n1)), 2)
#'         ztab <- round(qnorm(1 - (alpha)), 2)
#'         ztab <- round(c(-ztab, ztab), 2)
#'         pvalue <- 2 * pnorm(abs(ztest), lower.tail = TRUE)
#'
#'         if (abs(ztest) >= abs(ztab[2])) {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| > |ztab = ", abs(ztab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| > |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                             abs(ztest), "| < |ztab = ", abs(ztab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ztest = ", domain = "R-leem"),
#'                                 abs(ztest), "| < |ztab = ", abs(ztab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the Z Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         }
#'         results <- list(ztest = ztest, ztab = ztab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'
#'       }
#'       if(plot == TRUE){
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 , ztab[2], by = 0.01)
#'         y <- seq(ztab[2],+ 4, by = 0.01)
#'         fx <- dnorm(x, mean = 0, sd = 1)
#'         fy <- dnorm(y, mean = 0, sd = 1)
#'         curve(dnorm(x, mean = 0, sd = 1), - 4 , + 4  ,
#'               ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#cc0000")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#99ccff")
#'         abline(v = ztab[2], lty=2, col = "red")
#'         text(2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ztab[2] ,0.14,2.3, 0.28)
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'         axis(side = 1, at = c(-4, ztab[2]), font = 2, labels = FALSE, lwd.ticks = 0,
#'              col.axis = "#cc0000", col.ticks = "#cc0000", col = "#559ee8")
#'
#'         axis(side=1, at=as.character(c( ztab[2], 4)), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'
#'         axis(side = 1, at = c("",ztab[2]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'
#'         axis(side=1, at=as.character(c( "", ztab[2])), tick = TRUE, labels = FALSE,
#'              lwd.ticks = 1, col="#cc0000")
#'         axis(side = 1, at = ztest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ztest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'
#'
#'         if(prop == TRUE){
#'           title("Test of proportion: Z-Test.")
#'         }else{
#'           title("Test of hypothesis: Z-Test.")
#'         }
#'         text(0, 0.1, expression(bold("ACCEPT H0")))
#'         text(2.5, 0.1, expression(bold("RRH0")))
#'         arrows(ztest, 0.27, -1.8, 0.38)
#'         abline(v = ztest, lty=2, col = "blue")
#'         text(-2, 0.4, expression(bold("ztest")))
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'       }
#'     }
#'   }
#'   if (any(test ==  c("ttest", "t", "T", "tstudent"))) {
#'     df <- length(x) - 1
#'     nu <- df
#'     nu <- as.numeric(nu)
#'     sdev <- df
#'     if (missing(h0)) {
#'       h0 <- readline("Insert the value of null hypothesis: ")
#'       h0 <- as.numeric(h0)
#'     }
#'     if (any(alternative == c("two.sided", "t", "T"))) {
#'       if (is.null(y)) {
#'         title <- paste(gettext("  One Sample t-test (Two-sided test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ttest <- round((mean(x) - h0) / (sdev /sqrt(n)), 2)
#'         if (alternative == "two.sided"){
#'           ttab <- round(qt(1 - (alpha)/2, df = nu), 2)
#'         } else {
#'           ttab <- round(qt(1 - alpha, df = nu), 2)
#'         }
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df = nu, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]), '|')
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]),"|")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject the H0. ", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         n1 <- length(x)
#'         n2 <- length(y)
#'         if(n1 == n2){
#'           df <- n1+n2-2
#'           sp <- ((n1-1)*var(x) + (n2-1)*var(y))/df
#'         }else{
#'           df <- ((sd(x)^2/n1)^2 + 2*(sd(x)^2/n1)*(sd(y)^2/n2) + (sd(y)^2/n2)^2)/((((sd(x)^2/n1)^2)/(n1-1))+(((sd(y)^2/n2)^2)/(n2-1)))
#'           sp <- sqrt(((sd(x)^2)/n1)+((sd(y)^2)/n2))
#'         }
#'         title <- paste(gettext("  Two Sample t-test (Two-sided test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu1 = mu2", domain = "R-leem"), sep = "")
#'         althyp <- paste(gettext("  H1: mu1 != mu2", domain = "R-leem"), sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2), sep = "")
#'         ttest <- round((mean(x) - mean(y)) / sqrt(sp*(1/n1+1/n2)), 2)
#'         ttab <- round(qt(1 - (alpha)/2, df), 2)
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "" )
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext(" H0 was rejected", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]),"|")
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("   No evidence to reject the H0 ", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       if (plot == TRUE) {
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 * 1, ttab[1], by = 0.01)
#'         z <- seq(ttab[2],  4, by = 0.01)
#'         y <- seq( - 4, + 4, by = 0.01)
#'         fx <- dt(x, df = nu)
#'         fz <- dt(z,df = nu)
#'         fy <- dt(y, df = nu)
#'         curve(dt(x, df = nu),  - 4, + 4,
#'               ylim = c(0.0, 1.2 * max(fx,fy,fz)),xlab = "X",
#'               ylab = expression(f[X](x)),
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#99ccff")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#cc0000")
#'         polygon(c(z,rev(z)), c(fz,rep(0,length(fz))),
#'                 col="#cc0000")
#'         abline(v = ttab[1], lty=2, col = "red")
#'         abline(v = ttab[2], lty=2, col = "red")
#'         abline(v = ttest, lty=2, col = "blue")
#'         text(-2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ttab[1] ,0.14, -2.3, 0.27)
#'         text(2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ttab[2] ,0.14, 2.3, 0.27)
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'         axis(side = 1, at = c(ttab[1],ttab[2]), font = 2, labels = FALSE,
#'              col.axis = "#cc0000", col.ticks = "#cc0000", col = "#559ee8")
#'         axis(side=1, at=as.character(c( - 4, ttab[1])), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'         axis(side=1, at=as.character(c(ttab[2], + 4)), tick = TRUE, lwd = 1,
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'         axis(side = 1, at = c(ttab[1],ttab[2]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'         axis(side = 1, at = ttest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ttest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'         title("Test of hypothesis: T-Test.")
#'         text(0,0.1, expression(bold("ACCEPT H0" )))
#'         text(ttab[1] - 1 , 0.1, expression(bold("RRH0")))
#'         text(ttab[2] + 1 ,0.1, expression(bold("RRH0")))
#'         arrows(ttest, 0.27, -1.5, 0.33)
#'         text(-1.7, 0.35, expression(bold("ttest")))
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'       }
#'     }
#'     if (any(alternative == c("less", "l", "L"))) {
#'       if (is.null(y)) {
#'         title <- paste(gettext("  One Sample t-test (Less test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ttest <- round((mean(x) - h0) / (nu /sqrt(n)), 2)
#'         ttab <- round(dt(df = nu, 1 - alpha), 2)
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df = nu, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                                 "|")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = |", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]), "|")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         n1 <- length(x)
#'         n2 <- length(y)
#'         df <- n1+n2-2
#'         sp <- ((n1-1)*var(x) + (n2-1)*var(y))/df
#'         title <- paste(gettext("  Two Sample t-test (Less test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu1 >= mu2", domain = "R-leem"),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu1 < mu2", domain = "R-leem"),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         ttest <- round((mean(x) - mean(y)) / sqrt(sp*(1/n1+1/n2)), 2)
#'         ttab <- round(dt(df, 1 - alpha), 2)
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext(" H0 was rejected", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("   No evidence to reject the H0 ", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       if (plot == TRUE) {
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 , ttab[1], by = 0.01)
#'         y <- seq(ttab[1],+ 4, by = 0.01)
#'         fx <- dt(x, df = nu)
#'         fy <- dt(y, df = nu)
#'         curve(dt(x, df = nu), - 4 , + 4  ,
#'               ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#99ccff")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#cc0000")
#'         abline(v = ttab[1], lty=2, col = "red")
#'         abline(v = ttest, lty=2, col = "blue")
#'         text(-2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ttab[1] ,0.14, -2.1, 0.28)
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'
#'
#'         axis(side = 1, at = c(ttab[1], 4), font = 2, labels = FALSE, lwd.ticks = 0,
#'              col.axis = "#cc0000", col.ticks = "#cc0000", col = "#559ee8")
#'         axis(side=1, at=as.character(c( -4, ttab[1])),
#'              col="#cc0000", font = 2, lwd.ticks = 0, labels = FALSE)
#'         axis(side=1, at=as.character(c( "", ttab[1])), tick = TRUE, labels = FALSE,
#'              lwd.ticks = 1, col="#cc0000")
#'         axis(side = 1, at = c("",ttab[1]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'         axis(side = 1, at = ttest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ttest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'
#'
#'         title("Test of hypothesis: T-Test.")
#'         text(0, 0.1, expression(bold("ACCEPT H0")))
#'         text(-2.5, 0.1, expression(bold("RRH0")))
#'         arrows(ttest , 0.27, -2.1, 0.38)
#'         text(-2.3, 0.4, expression(bold("ttest")))
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'       }
#'     }
#'     if (any(alternative == c("greater", "g", "G"))) {
#'       if (is.null(y)) {
#'         title <- paste(gettext("  One Sample t-test (Greater test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu = ", domain = "R-leem"), round(h0, 2),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu != ", domain = "R-leem"), round(h0, 2),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         n <- length(x)
#'         ttest <- round((mean(x) - h0) / (nu /sqrt(n)), 2)
#'         ttab <- round(qt(df = nu,1 - alpha), 2)
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df = nu, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]),"|")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]),'|')
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject the H0.", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         n1 <- length(x)
#'         n2 <- length(y)
#'         df <- n1+n2-2
#'         sp <- ((n1-1)*var(x) + (n2-1)*var(y))/df
#'         title <- paste(gettext("  Two Sample z-test (Greater test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: mu1 <= mu2", domain = "R-leem"),
#'                          sep = "")
#'         althyp <- paste(gettext("  H1: mu1 > mu2", domain = "R-leem"),
#'                         sep = "")
#'         signlevel <- paste(gettext("  alpha = ", domain = "R-leem"), round(alpha, 2),
#'                            sep = "")
#'         ttest <- round((mean(x) - mean(y)) / sqrt(sp*(1/n1+1/n2)), 2)
#'         ttab <- round(dt(df, 1 - alpha), 2)
#'         ttab <- round(c(-ttab, ttab), 2)
#'         pvalue <- 2 * pt(df, abs(ttest), lower.tail = FALSE)
#'         if (abs(ttest) >= abs(ttab[2])) {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| > |ttab = ", abs(ttab[2]),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| > |ttab = ", abs(ttab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the t Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext(" H0 was rejected", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                             abs(ttest), "| < |ttab = ", abs(ttab[2]),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As |ttest = ", domain = "R-leem"),
#'                                 abs(ttest), "| < |ttab = ", abs(ttab[2]), '|')
#'           conclusion <- paste(gettext("   We observed by the t Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("   No evidence to reject the H0 ", domain = "R-leem"))
#'         }
#'         results <- list(ttest = ttest, ttab = ttab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       if(plot == TRUE){
#'         par(mar = c(10,4,4,4))
#'         x <- seq(- 4 , ttab[2], by = 0.01)
#'         y <- seq(ttab[2],+ 4, by = 0.01)
#'         fx <- dt(x, df = nu)
#'         fy <- dt(y, df = nu)
#'         curve(dt(x, df = nu), - 4 , + 4  ,
#'               ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
#'               panel.first = grid(col="gray90"), lwd = 5)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#cc0000")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#559ee8")
#'         abline(v = ttab[2], lty=2, col = "red")
#'         abline(v = ttest, lty=2, col = "blue")
#'         text(2.5, 0.3, expression(bold("Critical point")))
#'         arrows(ttab[2] ,0.14,2.3, 0.27)
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'
#'
#'
#'         axis(side = 1, at = c(-4, ttab[2]), font = 2, labels = FALSE,
#'              lwd.ticks = 0, col = "#559ee8")
#'
#'         axis(side=1, at=as.character(c( ttab[2], 4)), tick = TRUE, labels = FALSE,
#'              lwd.ticks = 0,col="#cc0000")
#'
#'         axis(side=1, at=as.character(c( "", ttab[2])), tick = TRUE, labels = FALSE,
#'              lwd.ticks = 1, col="#cc0000")
#'
#'         axis(side = 1, at = c("",ttab[2]), lwd = 0,
#'              col = "#cc0000", font = 2, tick = FALSE, col.axis="#cc0000", pos = aux2)
#'
#'
#'         axis(side = 1, at = ttest, lwd = 0,
#'              col = "blue", font = 2, tick = TRUE, col.axis="blue", pos = aux2)
#'         axis(side = 1, at = ttest, tick = TRUE, lwd = 1,
#'              col="blue", lwd.ticks = 1, labels = FALSE)
#'
#'
#'
#'         title("Test of hypothesis: T-Test.")
#'         text(0, 0.1, expression(bold("ACCEPT H0")))
#'         text(2.5, 0.1, expression(bold("RRH0")))
#'         arrows(ttest ,0.27, -2, 0.37)
#'         text(-2, 0.4, expression(bold("ttest")))
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = nullhyp  , side = 1, line = 4, adj = 0)
#'         mtext(text = althyp , side = 1, line = 5, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'       }
#'     }
#'   }
#'   if (any(test ==  c("chisqtest", "chisq", "CHI", "chisquared"))) {
#'     if (any(alternative == c("less", "l", "L"))) {
#'       if (is.null(y)) {
#'         aux_x <- x
#'         sd <- sd(x)
#'         df <- length(x)-1
#'         if (missing(h0)) {
#'             h0 <- readline("Insert the value of null hypothesis: ")
#'             h0 <- as.numeric(h0)
#'           }
#'         if (!any(names(argaddit) == "s")) {
#'           s <- readline("Insert the value of population variance: ")
#'           s <- as.numeric(s)
#'         } else s <- argaddit$s
#'         title <- paste(gettext("  One Sample Chi-Squared Test (Unilateral Test) \n", domain = "R-leem"))
#'
#'
#'         nullhyp <- paste(gettext("  H0: sigma^2 = ", domain = "R-leem"), round(h0, 2), sep = "")
#'         althyp <- paste(gettext("  H1: sigma^2 != ", domain = "R-leem"), round(h0, 2), sep = "")
#'         signlevel <- paste(gettext("  Alpha = ", domain = "R-leem"), round(alpha, 2), sep = "")
#'
#'
#'         chitest <- round((df*s)/h0, 2)
#'         chitab <- round(qchisq((1-alpha), df, lower.tail = FALSE), 2)
#'         pvalue <- 2 * pchisq(abs(chitest), df, lower.tail = FALSE)
#'         if (abs(chitest) <= abs(chitab)) {
#'           decision <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                             abs(chitest), " | < | CP = ", abs(chitab),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < Alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                                 abs(chitest), " | < | CP = ", abs(chitab), '|')
#'           conclusion <- paste(gettext("   We observed by the Chi-Squared Test that hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'
#'         } else {
#'           decision <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                             abs(chitest), " | > | CP = ", abs(chitab),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > Alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                                 abs(chitest), " | > | CP = ", abs(chitab), '|')
#'           conclusion <- paste(gettext("   We observed by the Chi-Squared Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         results <- list(chitest = chitest, chitab = chitab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'       }
#'       else{
#'         stop("Em desenvolvimento", .call = FALSE, domain = "R-leem")
#'       }
#'         if (plot == TRUE) {
#'         par(mar = c(11,4,4,4))
#'         minimo <- if (chitest <= (-4) * df) chitest - 4 * df else 0
#'         maximo <- if (chitest > 4 * df) chitest + 4 * df else 4 * df
#'         x <- seq(minimo, chitab, by = 0.01)
#'         y <- seq(chitab, maximo, by = 0.01)
#'         fx <- dchisq(x, df)
#'         fy <- dchisq(y, df)
#'         main <- substitute(atop(bold("One Sample Chi-Squared Test (Unilateral Test)"),chi[c]^2 == frac((n-1)*s^2,sigma^2)), list(h0 = h0))
#'         curve(dchisq(x, df), minimo, maximo,
#'               ylim = c(0, 1.2*max(fx,fy)),  ylab = expression(f[X](x)), xlab="",
#'               panel.first = grid(col="gray90"), main = main)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="#99ccff")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="gray")
#'         abline(v = chitab, lty=2, col = "#880000")
#'
#'
#'         #text(chitab, 0.8*max(fx,fy), expression(bold("CRITICAL POINT")), col = "#880000")
#'         #text(chitest, 0.8*max(fx,fy), expression(bold("CHISQTEST")), col = "blue")
#'         legend(chitest, 0.8*max(fx, fy), bg = "#010199", cex=0.8, box.col = "#010199",
#'            legend = expression(bold("STATISTICAL TEST(ST)    ")), text.col = "white")
#'         legend(chitab, 0.9*max(fx, fy), bg = "#880000", cex=0.8, box.col = "#880000",
#'            legend = expression(bold("CRITICAL POINT(CP)    ")), text.col = "white")
#'
#'
#'         abline(v = chitest, lty=2, col = "#010199")
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'         axis(side = 1, at = as.character(c(0, chitab)),
#'              col = "#888888", col.axis = "#888888", labels = FALSE)
#'         axis(side = 1, at = as.character(c(chitab, auxmaximo*10)),
#'              col = "#559ee8", col.axis = "#559ee8", labels = FALSE)
#'         axis(side = 1, at = chitab, tick = TRUE,
#'              col.ticks = "#880000", lwd.ticks = 1, labels = FALSE)
#'         axis(side = 1, at = chitab, tick = FALSE,
#'              font = 2, col.axis="#880000", pos = aux2)
#'         axis(side = 1, at = chitest, tick = FALSE,
#'              font = 2, col.axis="#010199", pos = aux2)
#'         axis(side = 1, at = chitest, tick = TRUE,
#'              col.ticks = "#010199", lwd.ticks = 1, labels = FALSE)
#'
#'
#'         legend("topleft", cex = 0.9, box.col = "black", bg = "#e0e0e0",
#'            legend = c("REJECT H0", "ACCEPT H0"), fill = c("gray", "#559ee8"))
#'
#'
#'
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = substitute(~~H[0]:~sigma^2 >= h0, list(h0 = h0))  , side = 1, line = 4.3, adj = 0)
#'         mtext(text =substitute(~~H[1]:~sigma^2 < h0, list(h0 = h0)), side = 1, line = 5.3, adj = 0)
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4.3, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'               }
#'
#'     }
#'     if (any(alternative == c("greater", "g", "G"))) {
#'       if (is.null(y)) {
#'         aux_x <- x
#'         sd <- sd(x)
#'         df <- length(x)-1
#'         if (missing(h0)) {
#'             h0 <- readline("Insert the value of null hypothesis: ")
#'             h0 <- as.numeric(h0)
#'           }
#'         if (!any(names(argaddit) == "s")) {
#'           s <- readline("Insert the value of population variance: ")
#'           s <- as.numeric(s)
#'         } else s <- argaddit$s
#'         title <- paste(gettext("  One Sample Chi-Squared Test(Unilateral Test) \n", domain = "R-leem"))
#'         nullhyp <- paste(gettext("  H0: sgima^2 = ", domain = "R-leem"), round(h0, 2), sep = "")
#'         althyp <- paste(gettext("  H1: sigma^2 != ", domain = "R-leem"), round(h0, 2), sep = "")
#'         signlevel <- paste(gettext("  Alpha = ", domain = "R-leem"), round(alpha, 2), sep = "")
#'
#'         chitest <- round((df*s)/h0, 2)
#'         chitab <- round(qchisq(1 - alpha, df), 2)
#'         pvalue <- 2 * pchisq(abs(chitest),  df, lower.tail = TRUE)
#'         if (abs(chitest) >= abs(chitab)) {
#'           decision <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                             abs(chitest), " | > | CP = ", abs(chitab),
#'                             gettext("| then reject H0!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " < Alpha = ", alpha,
#'                              gettext(" then reject H0!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                                 abs(chitest), " | > | CP = ", abs(chitab), '|')
#'           conclusion <- paste(gettext("   We observed by the Chi-Squared Test that the hypothesis H0 was rejected, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("H0 was rejected.", domain = "R-leem"))
#'         } else {
#'           decision <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                             abs(chitest), " | < | CP = ", abs(chitab),
#'                             gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decision2 <- paste(gettext("   As p-value = ", domain = "R-leem"),
#'                              pvalue, " > Alpha = ", alpha,
#'                              gettext("| then H0 is not rejected!", domain = "R-leem"), sep = "")
#'           decisionplot <- paste(gettext("   As | ST = ", domain = "R-leem"),
#'                                 abs(chitest), " | < | CP = ", abs(chitab), '|')
#'           conclusion <- paste(gettext("   We observed by the Chi-Squared Test that there is no evidence to reject the H0 hypothesis, at the significance level of ", domain = "R-leem"),
#'                               round(alpha * 100),
#'                               gettext("% probability", domain = "R-leem"), sep = "")
#'           conclusionplot <- paste(gettext("No evidence to reject H0.", domain = "R-leem"))
#'         }
#'         } else{
#'         stop("Em desenvolvimento", .call = FALSE, domain = "R-leem")
#'       }
#'         results <- list(chitest = chitest, chitab = chitab, pvalue = pvalue, test = test,
#'                         alternative = alternative, title = title, nullhyp = nullhyp,
#'                         althyp = althyp, signlevel = signlevel, decision = decision,
#'                         decision2 = decision2, conclusion = conclusion)
#'  if (plot == TRUE) {
#'         par(mar = c(11,4,4,4))
#'         main <- substitute(atop(bold("One Sample Chi-Squared Test (Greater Test)"),chi[c]^2 == frac((n-1)*s^2,sigma^2)), list(h0 = h0))
#'         minimo <- if (chitest <= (-4) * df) chitest - 4 * df else 0
#'         maximo <- if (chitest > 4 * df) chitest + 4 * df else 4 * df
#'         x <- seq(minimo, chitab, by = 0.01)
#'         y <- seq(chitab, maximo, by = 0.01)
#'         fx <- dchisq(x, df)
#'         fy <- dchisq(y, df)
#'         curve(dchisq(x, df), minimo, maximo,
#'               ylim = c(0, 1.2*max(fx,fy)),  ylab = expression(f[X](x)), xlab="X",
#'               panel.first = grid(col="gray90"), main = main)
#'         polygon(c(y, rev(y)),
#'                 c(fy, rep(0, length(fy))),
#'                 col="gray")
#'         polygon(c(x, rev(x)),
#'                 c(fx, rep(0, length(fx))),
#'                 col="#99ccff")
#'         abline(v = chitab, lty=2, col = "#880000")
#'
#'
#'         #text(chitab, 0.8*max(fx,fy), expression(bold("CRITICAL POINT")), col = "#880000")
#'         #text(chitest, 0.8*max(fx,fy), expression(bold("CHISQTEST")), col = "blue")
#'         legend(chitest, 0.8*max(fx, fy), bg = "#010199", cex=0.8, box.col = "#010199",
#'            legend = expression(bold("STATISTICAL TEST(ST)    ")), text.col = "white")
#'         legend(chitab, 0.9*max(fx, fy), bg = "#880000", cex=0.8, box.col = "#880000",
#'            legend = expression(bold("CRITICAL POINT(CP)    ")), text.col = "white")
#'
#'
#'         abline(v = chitest, lty=2, col = "#010199")
#'         aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20
#'         axis(side = 1, at = as.character(c(0, chitab)),
#'              col = "#888888", col.axis = "#888888", labels = FALSE)
#'         axis(side = 1, at = as.character(c(chitab, auxmaximo*10)),
#'              col = "#559ee8", col.axis = "#559ee8", labels = FALSE)
#'         axis(side = 1, at = chitab, tick = TRUE,
#'              col.ticks = "#880000", lwd.ticks = 1, labels = FALSE)
#'         axis(side = 1, at = chitab, tick = FALSE,
#'              font = 2, col.axis="#880000", pos = aux2)
#'         axis(side = 1, at = chitest, tick = FALSE,
#'              font = 2, col.axis="#010199", pos = aux2)
#'         axis(side = 1, at = chitest, tick = TRUE,
#'              col.ticks = "#010199", lwd.ticks = 1, labels = FALSE)
#'
#'
#'         legend("topleft", cex = 0.9, box.col = "black", bg = "#e0e0e0",
#'            legend = c("REJECT H0", "ACCEPT H0"), fill = c("gray", "#559ee8"))
#'
#'
#'
#'         mtext("Step 1: Hypothesis", side = 1, line = 3, adj = 0, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 3, adj = 0, col = "#0099ff")
#'         mtext(text = substitute(~~H[0]:~sigma^2 <= h0, list(h0 = h0))  , side = 1, line = 4.3, adj = 0)
#'         mtext(text =substitute(~~H[1]:~sigma^2 > h0, list(h0 = h0)), side = 1, line = 5.3, adj = 0)
#'
#'         mtext("Step 2: Significante level", side = 1, line = 6, adj = 0, col = "#0099ff", font = 2)
#'         mtext("_____________________", side = 1, line = 6, adj = 0, col = "#0099ff")
#'         mtext(text = signlevel  , side = 1, line = 7, adj = 0)
#'
#'         mtext("Step 3: Rule of decision", side = 1, line = 3, adj = 1, col = "#0099ff", font = 2)
#'         mtext("____________________", side = 1, line = 3, adj = 1, col = "#0099ff")
#'         mtext(decisionplot, side = 1, line = 4, adj = 1)
#'
#'         mtext("Step 4: Conclusion", side = 1, line = 6, adj = 1, col = "#0099ff", font = 2)
#'         mtext("________________", side = 1, line = 6, adj = 1, col = "#0099ff")
#'         mtext(conclusionplot, side = 1, line = 7, adj = 1)
#'               }
#'     }
#'   }
#'   if (any(test ==  c("ftest", "f", "F"))){
#'
#'     stop("Em desenvolvimento", .call = FALSE, domain = "R-leem")
#'   }
#'   attr(results, "output") <- "htest"
#'   class(results) <- "leem"
#'   results
#' }
#'
#'
#'
#'
