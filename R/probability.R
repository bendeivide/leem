# Probability
#' @importFrom manipulate manipulate slider
#' @export
p <- function(q, dist = "t-student", lower.tail = TRUE,
              rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if ( length(q) > 1 & !is.null(attr(q, "class"))) {
    regiona <- c("region1", "region3", "region5", "region6") # %>X>%
    regionb <- c("region2", "region4", "region7", "region8") # %<X<%
    if (any(attr(q, "region") == regionb)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
          plotcurve <- function(q, nu) {
            x <- seq(q[1], q[2], by=0.01)
            y <- seq(-6, 6, by=0.01)
            fx <- dt(x, df = nu)
            fy <- dt(y, df = nu)
            curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T",
                  ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
            polygon(c(y, rev(y)),
                    c(fy, rep(0, length(fy))),
                    col="gray90")
            polygon(c(x, rev(x)),
                    c(fx, rep(0, length(fx))),
                    col="red")
            abline(v=0, lty=2)
            qq <- round(q, digits=2)
            Pr <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
            Pr <- gsub("\\.", ",", Pr)
            qq <- gsub("\\.", ",", qq)
            axis(side=1, at=qq, labels=qq,
                 col="red", font = 2)
            abline(v = qq, lty=2, col = "red")
            if (attr(q, "region") == "region2") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< X < "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region4") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= X <= "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region7") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"<= X < "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
            if (attr(q, "region") == "region8") {
              legend("topleft", bty="n", fill="red",
                     legend=substitute(P(t1~"< X <= "~t2)==Pr~"\n\n"~gl==nu,
                                       list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
            }
          }
          if (gui == "plot" ) {
            # Probability
            nu <- argaddit$df
            prob <- round(pt(q = q[2], df = nu) - pt(q = q[1], df = nu), rounding)
            # Plot
            plotcurve(q, nu)
          }
          if (gui == "rstudio") {
            nu <- argaddit$df
            plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
              q[1] <- q1
              q[2] <- q2
              plotcurve(q, df)
            }
            manipulate::manipulate(plotcurveaux(q1, q2, df),
                                   q1 = manipulate::slider(-6, q[2], q[1]),
                                   q2 = manipulate::slider(q[2], 6, q[2]),
                                   df = manipulate::slider(1, 200, nu))
            prob <- round(pt(q[2], df = nu, lower.tail = T) - pt(q[1], df = nu, lower.tail = T), digits=rounding)
          }
      }
    }
    if (any(attr(q, "region") == regiona)) {
      if (dist == "t-student") {
        if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
        plotcurve <- function(q, nu) {
          x <- seq(-6, q[1], by=0.01)
          z <- seq(q[2], 6, by=0.01)
          y <- seq(-6, 6, by=0.01)
          fx <- dt(x, df = nu)
          fz <- dt(z, df = nu)
          fy <- dt(y, df = nu)
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)), xlab="T",
                ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(z, rev(z)),
                  c(fz, rep(0, length(fz))),
                  col="red")
          abline(v=0, lty=2)
          qq <- round(q, digits=2)
          Pr <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qq, tick = TRUE, lwd = 0,
               col="red", font = 2, lwd.ticks = 1)
          axis(side=1, at=as.character(c(-6, qq[1])), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          axis(side=1, at=as.character(c(qq[2], 6)), tick = TRUE, lwd = 1,
               col="red", font = 2, lwd.ticks = 0, labels = FALSE)
          abline(v = qq, lty=2, col = "red")
          if (attr(q, "region") == "region1") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >"~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region3") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X >="~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region5") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~">= X > "~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
          if (attr(q, "region") == "region6") {
            legend("topleft", bty="n", fill="red",
                   legend=substitute(P(t1~"> X >= "~t2)==Pr~"\n\n"~gl==nu,
                                     list(t1=qq[1], t2=qq[2], Pr=Pr, nu = nu)))
          }
        }
        if (gui == "plot" ) {
          # Probability
          nu <- argaddit$df
          prob <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
          # Plot
          plotcurve(q, nu)
        }
        if (gui == "rstudio") {
          nu <- argaddit$df
          plotcurveaux <- function(q1 = q[1], q2 = q[2], df, ...) {
            q[1] <- q1
            q[2] <- q2
            plotcurve(q, df)
          }
          manipulate::manipulate(plotcurveaux(q1, q2, df),
                                 q1 = manipulate::slider(-6, q[2], q[1]),
                                 q2 = manipulate::slider(q[2], 6, q[2]),
                                 df = manipulate::slider(1, 200, nu))
          prob <- round(pt(q[1], df = nu, lower.tail = T) + pt(q[2], df = nu, lower.tail = F), digits=rounding)
        }
      }
    }

  } else {
    if (dist == "t-student") {
      if (!any(names(argaddit) == "df")) stop("Insira o argumento 'df'!", call. = FALSE)
      if (lower.tail) {
        plotcurve <- function(q, nu) {
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
                xlab="T", ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
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
          nu <- argaddit$df
          manipulate::manipulate(plotcurve(qaux, nuaux),
                                 qaux = manipulate::slider(-6, 6, q),
                                 nuaux = manipulate::slider(1, 200, nu))
          prob <- pt(q = q, df = nu)
        }
      } else {
        plotcurve <- function(q, nu) {
          curve(dt(x, df = nu), -6, 6, ylab = expression(f[T](t)),
                xlab="T", ylim = c(0, 1.2 * max(fy)), panel.first = grid(col = "gray"))
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
          nu <- argaddit$df
          prob <- pt(q = q, df = nu)
          manipulate::manipulate(plotcurve(qaux, nuaux),
                                 qaux = manipulate::slider(-6, 6, q),
                                 nuaux = manipulate::slider(1, 200, nu))
        }

      }
    }
    if (dist == "gumbel"){
      if (!any(names(argaddit) == "location")) stop("Insira o argumento 'location'!", call. = FALSE)
      if (!any(names(argaddit) == "scale")) stop("Insira o argumento 'scale'!", call. = FALSE)
      if (argaddit$scale <= 0 ) stop("o argumento 'scale' deve ser maior que zero!", call. = FALSE)
      if (lower.tail) {
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
          aux <- seq(q, 10, by=0.01)
          y <- seq(-5, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col="gray90")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="red")
          abline(v = location, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pgumbel(qq, location, scale), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G<=q)==Pr, list(q=qq, Pr=Pr)))
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          manipulate::manipulate(plotcurve(q, location, scale),
                                 q = manipulate::slider(-5, 10, q),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = q, location, scale)
        }
      } else {
        plotcurve <- function(q, location, scale){
          curve(dgumbel(x, location, scale), -5, 10, ylab = expression(f[G](g)), xlab="G")
          aux <- seq(q, 10, by=0.01)
          y <- seq(-5, q, by=0.01)
          fx <- dgumbel(aux, location, scale)
          fy <- dgumbel(y, location, scale)
          polygon(c(aux, rev(aux)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v = location, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pgumbel(qq, location, scale, lower.tail = FALSE), digits = rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(G>q)==Pr, list(q=qq, Pr=Pr)))
        }
        if (gui == "plot" ) {
          # Probability
          location <- argaddit$location
          scale <- argaddit$scale
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
          # Plot
          plotcurve(q, location, scale)
        }
        if (gui == "rstudio") {
          location <- argaddit$location
          scale <- argaddit$scale
          manipulate::manipulate(plotcurve(q, location, scale),
                                 q = manipulate::slider(-5, 10, q),
                                 location = manipulate::slider(location - 10, location + 10, location ),
                                 scale = manipulate::slider(1, scale + 100, scale))
          prob <- pgumbel(q = q, location, scale, lower.tail = FALSE)
        }
      }
    }
  }

  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}







