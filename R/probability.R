# Probability
#' @importFrom manipulate manipulate slider
#' @export
p <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "t-student") {
    if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
    if (lower.tail) {
      plotcurve <- function(q, nu) {
        curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(-6, q, by=0.01)
        y <- seq(q, 6, by=0.01)
        fx <- dt(x, df = nu)
        fy <- dt(y, df = nu)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pt(qq, df = nu, lower.tail = T), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T<=q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
      }
      if (gui == "plot" ) {
        # Probability
        nu <- argaddit$df
        prob <- pt(q = q, df = nu)
        # Plot
        plotcurve(q, nu)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, nuaux),
                               qaux = manipulate::slider(-6, 6, q),
                               nuaux = manipulate::slider(nu, nu + 200, nu))
      }
    } else {
      plotcurve <- function(q, nu) {
        curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(q, 6, by=0.01)
        y <- seq(-6, q, by=0.01)
        fx <- dt(x, df = nu)
        fy <- dt(y, df = nu)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pt(qq, df = nu, lower.tail = F), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T~`>`~q)==Pr~"\n\n"~gl==nu, list(q=qq, Pr=Pr, nu = nu)))
      }
      if (gui == "plot") {
        # Probability
        nu <- argaddit$df
        prob <- pt(q = q, df = nu)
        # Plot
        plotcurve(q, nu)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, nuaux),
                               qaux = manipulate::slider(-6, 6, q),
                               nuaux = manipulate::slider(nu, nu + 200, nu))
      }

    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}



