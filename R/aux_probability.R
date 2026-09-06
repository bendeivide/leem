#########################################################################
# "These functions only work correctly if executed through function P()."
#########################################################################
## (name: plot+p+dist+aux+gui)
# OBS.: aux : ltt - lower.tail == TRUE; ltf - lower.tail == FALSE;
#       ltn - low er.tail == NULL; ra - Region A; rb - Regio B
#       dist: "normal", "binomial", ...
#       gui: "plot", "rstudio", ...
#########################################################################


plot_top_text <- function(q, main, vert.orien.main, mu, sigma, q_text) {
  if (is.null(main)) {
  
    # Localized title text
    titulo <- gettext("Normal Distribution", domain = "R-leem")

    # Region A -------------------------------------------------------------
    if (vert.orien.main) { # *** NEW LINE ***
      # P(a > X > b)
      if (attr(q, "region") == "region1") {
        main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a >= X >= b)
      if (attr(q, "region") == "region3") {
        main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a >= X > b)
      if (attr(q, "region") == "region5") {
        main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a > X >= b)
      if (attr(q, "region") == "region6") {
        main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity)), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
    } else {
      # P(a > X > b)
      if (attr(q, "region") == "region1") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a >= X >= b)
      if (attr(q, "region") == "region3") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a >= X > b)
      if (attr(q, "region") == "region5") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X <= t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X > t2)== integral(f[X](x)*"dx", t2, infinity), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a > X >= b)
      if (attr(q, "region") == "region6") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(X < t1)== integral(f[X](x)*"dx", -infinity, t1)*","~~P(X >= t2)== integral(f[X](x)*"dx", t2, infinity), list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
    }
    # ------------------------------------------------------------------------------------------------

    # Region B -------------------------------------------------------------
    if (vert.orien.main) {
      # P(a < X < b)
      if (attr(q, "region") == "region2") {
        main <- substitute(atop(bold(titulo),
                                f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a <= X <= b)
      if (attr(q, "region") == "region4") {
        main <- substitute(atop(bold(titulo), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a <= X < b)
      if (attr(q, "region") == "region7") {
        main <- substitute(atop(bold(titulo),
                                f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2)),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a < X <= b)
      if (attr(q, "region") == "region8") {
        main <- substitute(atop(bold(titulo), f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2)),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
    } else {
      # P(a < X < b)
      if (attr(q, "region") == "region2") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<~t2)== integral(f[X](x)*"dx", t1, t2),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a <= X <= b)
      if (attr(q, "region") == "region4") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<=~t2)== integral(f[X](x)*"dx", t1, t2),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      ## P(a <= X < b)
      if (attr(q, "region") == "region7") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<=~X<~t2)== integral(f[X](x)*"dx", t1, t2),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
      # P(a < X <= b)
      if (attr(q, "region") == "region8") {
        main <- substitute(
          bold(titulo)~~"|"~~f[X](x) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~P(t1<~X<=~t2)== integral(f[X](x)*"dx", t1, t2),
                           list(t1 = q_text[1], t2 = q_text[2], x = "x", titulo = titulo))
      }
    }
    # ------------------------------------------------------------------------------------------------
  }


  return(main)
}

create_normal_curve_and_axis <- function (mu, sigma, density_terms_list, main, cex.main, cex.axis, dec, minimo, maximo) {
  # this function can create a Normal Distribution curve
  # allows de use of certain parameters ------------------------------------------------------------
  curve(
      dnorm(x, mean = mu, sd = sigma),
      minimo,
      maximo,
      ylim = c(0, 1.2 * max(density_terms_list$fx, density_terms_list$fy, density_terms_list$fz)),
      xlab = "X",
      ylab = expression(f[X](X)), # Draw a beautiful notation.
      panel.first = grid(col = "gray90"),
      main = main,
      xaxt = "n", # Standard X-axis = none
      yaxt = "n", # Standard Y-axis = none
      cex.main = cex.main # Text proportion related to the curve
    )
  # ------------------------------------------------------------------------------------------------

  # Generate pretty x-axis values
  axis_x <- pretty(minimo:maximo) # *** NEW LINE ***

  # *** NEW BLOCK ***
  # Draw x-axis
  axis(
    side = 1,
    at = axis_x
  )

  # Generate pretty values for the y-axis
  ## dec of P()
  axis_y <- pretty(dnorm(minimo:maximo, mu, sigma)) # *** NEW LINE ***

  # *** NEW BLOCK ***
  # Format y-axis labels with comma decimal separator
  if (dec == ",") {
    # Y-axis
    axis(
      side = 2,
      at = axis_y,
      cex.axis = cex.axis,
      labels = format(
        axis_y,
        decimal.mark = ",",
        nsmall = 2
      )
    )
  } else {
    # Default y-axis labels
    # Y-axis
    axis(
      side = 2,
      at = axis_y,
      cex.axis = cex.axis
    )
  }
}

create_polygon <- function(density_terms_list, sequence_terms_list, col){
  # 'polygon' is a function that can create a form over the Normal curve
  # previosly created by 'curve' function. 'polygon' needs coordinates over
  # the curve line (y1, fy1),(yn, fyn), and its return way ((yn, 0),(y1, 0).
  # 
  # arguments: (((y1, fy1),(yn, fyn)), ((yn, 0),(y1, 0)), fullfill_color)
  
  # Background
  polygon(
          c(sequence_terms_list$y, rev(sequence_terms_list$y)),
          c(density_terms_list$fy, rep(0, length(density_terms_list$fy))),
          col = "gray90"
        )

  # Shade the cumulative probability region
  # col1 of P(X < q[1])
  polygon(
          c(sequence_terms_list$x, rev(sequence_terms_list$x)),
          c(density_terms_list$fx, rep(0, length(density_terms_list$fx))),
          col = col
        )
  
  # col1 of P(X > q[2])
  polygon(
          c(sequence_terms_list$z,rev(sequence_terms_list$z)),
          c(density_terms_list$fz,rep(0,length(density_terms_list$fz))),
          col = col
        )
}

create_plot_details <- function(lty, q_rounded_value,
                       col2, text.size, minimo,
                       maximo, q_density_value, long.segment,
                       text_list
                      ) {
  # Highlight q on the x-axis 
  ## X-axis
  # Ploting the values of q on x axis and hightlighting then by bold and colorful fonts
  mtext(text_list$q_text, side = 1, at = q_rounded_value, line = 2, col = col2, font = 2, cex = text.size) # *** NEW LINE ***
  
  #aux2 <- par("usr")[3]-(par("usr")[4] - par("usr")[3])/20 # getting coordinates to plot
  
  # Ploting the values of q on x axis and hightlighting then by bold and colorful fonts 
  #axis(side=1, at=qq, lwd = 0, col="red", font = 2, tick = FALSE, col.axis = "red", pos = aux2) 
  
  # Creating a line under the fulfilled area of the graphic ++++++++++++++++++++++++++
  axis(side = 1, at = as.character(c(minimo, q_rounded_value[1])), tick = TRUE, lwd = 1,
       col = col2, font = 2, lwd.ticks = 0, labels = FALSE)
  
  axis(side=1, at=as.character(c(q_rounded_value[2], maximo)), tick = TRUE, lwd = 1,
       col = col2, font = 2, lwd.ticks = 0, labels = FALSE)
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Creating a tiny line under the values of q +++++++++++++++++++++++++++++++++++++++
  axis(side = 1, at = as.character(q_rounded_value[1]), tick = TRUE, lwd = 1,
       col = col2, font = 2, lwd.ticks = 1, labels = FALSE)
    
  axis(side = 1, at = as.character(q_rounded_value[2]), tick = TRUE, lwd = 1,
       col = col2, font = 2, lwd.ticks = 1, labels = FALSE)
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ------------------------------------------------------------------------------------------------


  # Creating details on the Y axis -----------------------------------------------------------------
  # Highlight density value at q on the y-axis
  mtext(text_list$q_density_text, side = 2, at = q_density_value, line = 2, col = col2, font = 2, cex = text.size)

  # Draw tick mark at density value
  axis(side = 2, at = q_density_value, labels = FALSE,
       col = col2, font = 2, col.axis = col2, tick = TRUE, lwd.ticks = 1)
  # ------------------------------------------------------------------------------------------------


  # Long segment and type --------------------------------------------------------------------------
  # Create a vertical line at the q point. Is a red and dashed line
  #abline(v = qqaux, lty=2, col = "red")

  # Draw full or partial guide lines
  # col2 and lty of P()
  if (isTRUE(long.segment)) {
    abline(v = q_rounded_value, col = col2, lty = lty)
    abline(h = q_density_value, col = col2, lty = lty)
  } else {
    segments(q_rounded_value, 0, q_rounded_value, q_density_value, col = col2, lty = lty)
    segments(par("usr")[1], q_density_value, q_rounded_value, q_density_value, col = col2, lty = lty)
  }
  # ------------------------------------------------------------------------------------------------

  # Add point at (q, density)
  # Point inserted
  points(q_rounded_value, q_density_value, pch = 19)
}

#write_legend <- function(q, col, text.size, q_text, prob_text, minimo, mu_text, sigma_text, fx, fy) {
write_legend <- function(q, col, minimo, density_terms_list, text.size, text_list) {
  # Creating a gray rectangle above the plot to hightlight the text over it
  rect(par("usr")[1], 1.03 * max(density_terms_list$fx, density_terms_list$fy), par("usr")[2], par("usr")[4], col = "gray")
  
  ## Localized parameter label
  # gettext returns the text. "Parameters:" is the original text, domain determines
  # the language base to search the translated word. 
  parametros <- gettext("Parameters:", domain = "R-leem")

  # Region A ------------------------------------------------------------
  ## P(q[1] > X > q[2])
  if (attr(q, "region") == "region1") {
    # Display cumulative probability legend
    
    # 'legend' function creates a dinamic legend if matematic notation:
    # - topleft: Location of the legend;
    # - bty (Box Type): "n" removes the bordes of the retangle around the legend;
    # - fill: creates a red square with the legend;
    # - cex: the relative size of the legend;
    # - legend: the matematic notation.
    # the fucntion returns the legend coordinates
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(X<t1)+P(X>t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    
    # Display parameter legend
    # legaux$text$y is a coordinate
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }

  if (attr(q, "region") == "region3") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(X<=t1)+P(X>=t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text[])))

    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }

  if (attr(q, "region") == "region5") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(X<=t1)+P(X>t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  if ( attr(q, "region") == "region6") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(X<t1)+P(X>=t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  # -----------------------------------------------------------------------------------------------

  # Region B ------------------------------------------------------------
  ## P(a < X < b)
  if (attr(q, "region") == "region2") {
    # Display cumulative probability legend
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(t1<~X<~t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    # Display parameter legend
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  # P(a <= X <= b)
  if (attr(q, "region") == "region4") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(t1<=~X<=~t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    legend(minimo, legaux$text$y, bty="n", bg = "white", cex = text.size,
           legend = substitute("Parameters:"~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  ## P(a <= X < b)
  if (attr(q, "region") == "region7") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(t1<=~X<~t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  # P(a < X <= b)
  if (attr(q, "region") == "region8") {
    legaux <- legend("topleft", bty = "n", fill = col,cex = text.size,
                     legend = substitute(P(t1<~X<=~t2)==Pr,
                                         list(t1=text_list$q_text[1],t2=text_list$q_text[2], Pr = text_list$prob_text)))
    parametros <- gettext("Parameters:", domain = "R-leem")
    legend(minimo, legaux$text$y, bty="n", bg = "white",  cex = text.size,
           legend = substitute(parametros~mu == media ~ "," ~ sigma == varen,
                               list(media = text_list$mu_text, varen = text_list$sigma_text, parametros = parametros)))
  }
  # -----------------------------------------------------------------------------------------------
}

# Auxiliar functions of P()
# Observations:
#    - `%>X>%`() internal function
# Continuous Distributions
## A-region (name: plot+p+name_distribution+ar+gui)
# OBS.: ar - A-region; gui: "plot", "rstudio", "tcltk"

# Normal distribution
## Plot
# q: Quantil
# mu: Mean (mi)
# sigma: Satandard Desviation
# rounding: 
# main:  
plot_p_normal_plot <- function(q, mu, sigma, rounding, dec = c(".", ","),
                               long.segment = FALSE, col = "#8EC5E5",
                               col2 = "#38A8E8", lty = 2, main = NULL,
                               text.size = 1, cex.main = 1.2,
                               cex.axis = 1, cex.lab = 1,
                               vert.orien.main = TRUE, maximo, minimo,
                               q1 = NULL, q2 = NULL, region) {
  
  if (!is.null(q1)){
    q[1] <- q1
    q[2] <- q2
  }

  
  # Define the minimum x-axis limit
  #minimo <- if (q[1] <= mu - 4 * sigma) q[1] - 4 * sigma else mu - 4 * sigma
  # Define the maximum x-axis limit
  #maximo <- if (q[2] > mu + 4 * sigma) q[2] + 4 * sigma else mu + 4 * sigma
  
  
  # Creating a list that gather all the sequences terms
  sequence_terms_list <- list()

  # Creating a sequence of terms.
  if (region == "region A") {
    # from the min. til the q
    # or from q to max. By 0.01 steps
    # Sequence of x values from minimum to q[1]
    sequence_terms_list$x <- seq(minimo, q[1], by = 0.01)
    # Sequence of x values from q[2] to maximum
    sequence_terms_list$z <- seq(q[2], maximo, by = 0.01)
    # Sequence of values from minimum to maximum
    sequence_terms_list$y <-seq(minimo, maximo, by = 0.01)
    #------------------------------

  } else if (region == "region B") {
    # Sequence of x values from q[1] to q[2], by 0.01 steps
    sequence_terms_list$x <- seq(q[1], q[2], by = 0.01)
    # Sequence of values from minimum to maximum
    sequence_terms_list$y <- seq(minimo, maximo, by = 0.01)
  }
  
  # Creating a list that gather all the density terms
  density_terms_list <- list()

  # Geting the density of the point of the 
  # normal distribution
  if (region == "region A") {
    # Density values for the left region
    density_terms_list$fx <- dnorm(sequence_terms_list$x, mean = mu, sd = sigma)
    # Density values for the right region
    density_terms_list$fz <- dnorm(sequence_terms_list$z,mean = mu, sd = sigma)
    # Density values for the background region
    density_terms_list$fy <- dnorm(sequence_terms_list$y, mean = mu, sd = sigma)
    #------------------------------
  } else if (region == "region B") {
    # Density values for region from q[1] to q[2]
    density_terms_list$fx <- dnorm(sequence_terms_list$x, mean = mu, sd = sigma)
    # Density values for the background region
    density_terms_list$fy <- dnorm(sequence_terms_list$y, mean = mu, sd = sigma)
    #------------------------------
  }


  # Density value at q
  #pdf <- dnorm(q, mu, sigma)
  q_density_value <- dnorm(q, mu, sigma)
  
  # Rounded value of q for display
  #qq <- round(q, digits=2)
  q_rounded_value <- round(q, digits=2)
  
  #qqaux <- qq #???

  if (region == "region A") {
    # Probability P(q[1] > X > q[2])
    probability_value <- round(pnorm(q[1], mean = mu,sd = sigma, lower.tail = T) + pnorm(q[2], mean = mu, sd=sigma, lower.tail = F), digits=rounding)

  } else if (region == "region B") {
    # Probability P(q[1] < X < q[2])
    probability_value <- round(pnorm(q[2], mean = mu,sd = sigma) - pnorm(q[1], mean = mu, sd=sigma), digits=rounding)
  }


  # Creating a list that gather all the text variables
  text_list <- list()

  # *** NEW BLOCK ***
  # Replace decimal separator if comma format is requested
  if (dec == ",") {
    text_list$prob_text <- gsub("\\.", ",", probability_value)
    text_list$q_text <- gsub("\\.", ",", q_rounded_value)
    text_list$mu_text <- gsub("\\.", ",", mu)
    text_list$sigma_text <- gsub("\\.", ",", sigma)
    text_list$q_density_text <- gsub("\\.", ",", round(q_density_value, 3))

  } else {
    # Keep default decimal separator
    text_list$prob_text <- probability_value
    text_list$q_text <- q_rounded_value
    text_list$q_density_text <- round(q_density_value, 3)
    text_list$mu_text <- mu
    text_list$sigma_text <- sigma
  }

  # *** NEW LINE ***
  # Localized title text
  
  # Ploting the formula (depending on the region) on
  # the top of the figure.--------------------------------------------------------------------------
  main <- plot_top_text(q, main, vert.orien.main, mu, sigma, text_list$q_text)
  # ------------------------------------------------------------------------------------------------

  # Creating a Normal Distribution curve ------------------------------------------------------------ 
  create_normal_curve_and_axis(mu, sigma, density_terms_list, main, cex.main, cex.axis, dec, minimo, maximo)
  #-------------------------------------------------------------------------------------------------
  
  # Creating a polygon -----------------------------------------------------------------------------
  create_polygon(density_terms_list, sequence_terms_list, col)
  # ------------------------------------------------------------------------------------------------

  # Creating details on the X axis -----------------------------------------------------------------
  create_plot_details(lty, q_rounded_value,
                       col2, text.size, minimo,
                       maximo, q_density_value, 
                       long.segment, text_list
                      )
  
  # Legends ----------------------------------------------------------------------------------------
  write_legend(q, col, minimo, density_terms_list, text.size, text_list)  
}

## RStudio
plot_p_normal_rstudio <- function(q, mu, sigma, rounding,
                                 minimo, maximo, dec,
                                 long.segment, col,
                                 col2, lty, main,
                                 text.size, cex.main,
                                 cex.axis, cex.lab,
                                 vert.orien.main, region) {

  q1 <- q[1]
  q2 <- q[2]
                                  
  # Ensure the interval is at least 0.02 (2 * step)
  if (maximo - q2 < 0.02) {
    maximo <- q2 + 0.02
  }
  if (q1 - minimo < 0.02) {
    minimo <- q1 - 0.02
  }

  # Create an interactive Normal distribution plot using the
  # 'manipulate' package available in RStudio.
  #
  # The interface allows the user to dynamically modify
  # distribution parameters and graphical settings through
  # sliders and checkboxes.
  manipulate::manipulate(

    # Main plotting function responsible for drawing the
    # Normal distribution graph.

    # Arguments:
    # q1 and q2          -> Quantile or x-value to be highlighted.
    # mu                 -> Mean of the Normal distribution.
    # sigma              -> Standard deviation of the Normal distribution.
    # rounding           -> Number of decimal places used in labels/results.
    # minimo             -> Minimum x-axis value for plotting.
    # maximo             -> Maximum x-axis value for plotting.
    # dec                -> Decimal separator style.
    # long.segment       -> Logical value controlling segment extension.
    # col                -> Main fill or polygon color.
    # col2               -> Secondary color used in plot elements.
    # lty                -> Line type specification.
    # main               -> Main title of the plot.
    # text.size          -> Size of additional text annotations.
    # cex.main           -> Expansion factor for the main title.
    # cex.axis           -> Expansion factor for axis labels.
    # cex.lab            -> Expansion factor for axis titles.
    # vert.orien.main    -> Logical value controlling vertical title orientation.

    plot_p_normal_plot(q, mu, sigma, rounding,
                      # Define the decimal separator according to the
                      # checkbox state selected by the user.
                      dec = if (isTRUE(decimals)) "," else ".",
                      long.segment, col,
                      col2, lty, main,
                      # Text size used in annotations and graphical elements.
                      text.size,
                      # Main title size follows the same value chosen
                      # for the general text size.
                      cex.main = text.size,
                      cex.axis, cex.lab,
                      vert.orien.main, maximo, minimo, q1, q2, region),
    #################################################
    # Interactive controls
    #################################################

    # Slider used to control the x-value (quantile)
    # highlighted in the Normal distribution plot.
    q1 = manipulate::slider(
      minimo, q1, q1,
      step = 0.01,
      label = gettext(
        "Quantile 1",
        domain = "R-leem"
      )
    ),

    # Slider used to control the x-value (quantile)
    # highlighted in the Normal distribution plot.
    q2 = manipulate::slider(
      q2, maximo, q2,
      step = 0.01,
      label = gettext(
        "Quantile 2",
        domain = "R-leem"
      )
    ),

    # Slider controlling the mean of the
    # Normal distribution.
    mu = manipulate::slider(
      minimo, maximo, mu,
      step = 0.01,
      label = gettext(
        "Mean",
        domain = "R-leem"
      )
    ),

    # Slider controlling the standard deviation.
    #
    # The upper limit is defined as 1.8 times the
    # initial standard deviation value.
    sigma = manipulate::slider(
      sigma, sigma * 1.8, sigma,
      step = 0.01,
      label = gettext(
        "Standard Deviation",
        domain = "R-leem"
      )
    ),

    # Slider controlling the size of texts displayed
    # in the graph, including labels and annotations.
    text.size = manipulate::slider(
      0.8, 3, text.size,
      step = 0.01,
      label = gettext(
        "Text Size",
        domain = "R-leem"
      )
    ),

    # Checkbox that enables or disables vertical
    # orientation for the main plot title.
    vert.orien.main = checkbox(
      vert.orien.main,
      gettext(
        "Vertical Title Orientation",
        domain = "R-leem"
      )
    ),

    # Checkbox controlling whether the highlighted
    # segment should be extended.
    long.segment = checkbox(
      long.segment,
      gettext(
        "Long segment",
        domain = "R-leem"
      )
    ),

    # Checkbox used to select the decimal separator.
    #
    # TRUE  -> comma (,)
    # FALSE -> period (.)
    decimals = checkbox(
      if (dec == ",") TRUE else FALSE,
      gettext(
        "Comma",
        domain = "R-leem"
      )
    )
  )
}
















create_containers_and_panels <- function() {
  
  # Creating a list of containers and panels
  # to be returned by this function and used by
  # tk plot p normal function
  windows_list <- list()

  # Main Window
  windows_list$base <- tktoplevel(padx = 10, pady = 10)

  tkwm.geometry(windows_list$base, "600x700")

  # Title
  tkwm.title(windows_list$base, gettext("leem package: Normal Distribution", domain = "R-leem"))


  # =========================
  # Main panels
  # =========================

  # All
  
  # Creating a main window splited in two parts with one
  # movel bar separeting the divisions
  windows_list$main_container <- ttkpanedwindow(windows_list$base, orient = "horizontal")

  # ploting the main window and turn it in a responsive interface
  # 'expand' argument allow the window to be Resizable
  # 'fill' argument allow the window to resize both horizontally and vertically
  tkpack(windows_list$main_container, expand = TRUE, fill = "both")

  # Left container (control area)
  # Building the left container within the main_container, splited by horizontal
  # bars
  windows_list$left_container <- ttkpanedwindow(windows_list$main_container, orient = "vertical")

  # Right container (plot area)
  windows_list$right_container <- ttkpanedwindow(windows_list$main_container, orient = "vertical")

  # Adding the left_container into the main_container
  tkadd(windows_list$main_container, windows_list$left_container)

  # Adding the right_container (next to the left) into the main_container
  tkadd(windows_list$main_container, windows_list$right_container)


  # =========================
  # Input frame
  # =========================
  
  # Creating the painel that gather all the slisers and checkbox
  windows_list$left_panel <- ttklabelframe(
    windows_list$left_container,
    text = gettext(
      "Input(s):",
      domain = "R-leem"
    )
  )

  # Adding left_panel into the left_container, and anchoring it
  # at northwest position of the left_container 
  tkpack(
    windows_list$left_panel,
    side = "top",
    fill = "x",
    anchor = "nw",
    pady = 5
  )

  return(windows_list)
}

create_logo <- function(left_container) {
  
  # =========================
  # Logo frame
  # =========================
  logo_frame <- tkframe(left_container)

  # Puting the logo_frame on the top of the container
  # using 100% of the horizontal space available
  # with 5 pixel of external padding on the top and
  # the botton of the frame 
  tkpack(
    logo_frame,
    side = "top",
    fill = "x",
    pady = 5
  )
 

  # =========================
  # Logo
  # =========================

  # Getting the logo path on the PC disk
  logo_path <- system.file(
    "etc",
    "logo.png",
    package = "leem"
  )

  # Create original image to be used by tcl/tk
  logo_img <- tcl(
    "image",
    "create",
    "photo",
    "-file",
    logo_path
  )

  # Create resized image object
  # memory allocation for dynamic use
  logo_small <- tclVar()

  tcl(
    "image",
    "create",
    "photo",
    logo_small
  )

  # Resize image using subsample
  tcl(
    logo_small,
    "copy",
    logo_img,
    "-subsample",
    5
  )

  # Create label containing the image
  logo_label <- tklabel(
    logo_frame,
    image = logo_small
  )

  # Pack logo
  tkpack(
    logo_label,
    side = "top",
    fill = "x",
    pady = 5
  )
}

create_sliders_and_checkboxes <- function(left_container, left_panel, var_list) {
  
  # Creating a control object list
  # to gather all the checkboxes and sliders
  ctrl_obj_list <- list()
  
  # =========================
  # Sliders
  # =========================

  ctrl_obj_list$q1_slider <- tkscale(left_panel,
                       from = var_list$minimo,
                       to = tclvalue(var_list$q2_var),
                       orient = "horizontal",
                       variable = var_list$q1_var,
                       resolution = 0.1,
                       label = gettext("Quantile 1", domain="R-leem"),
                       showvalue = TRUE)

  ctrl_obj_list$q2_slider <- tkscale(left_panel,
                       from = tclvalue(var_list$q1_var),
                       to = var_list$maximo,
                       orient = "horizontal",
                       variable = var_list$q2_var,
                       resolution = 0.1,
                       label = gettext("Quantile 2", domain="R-leem"),
                       showvalue = TRUE)

  ctrl_obj_list$mu_slider <- tkscale(left_panel,
                       from = var_list$minimo,
                       to = var_list$maximo,
                       resolution = 0.1,
                       orient = "horizontal",
                       variable = var_list$media_var,
                       label = gettext("Mean", domain = "R-leem"))

  ctrl_obj_list$sigma_slider <- tkscale(left_panel,
                         from = 0.1,
                         to = 5,
                         resolution = 0.1,
                         orient = "horizontal",
                         variable = var_list$sd_var,
                         label = gettext("Standard Deviation", domain = "R-leem"))

  ctrl_obj_list$text.size_slider <- tkscale(left_panel,
                           from = 0.8,
                           to = 3,
                           resolution = 0.1,
                           orient = "horizontal",
                           variable = var_list$text.size_var,
                           label = gettext("Text Size", domain = "R-leem"))


  # =========================
  # Checkboxes
  # =========================

  ctrl_obj_list$orientation_checkbox <- tkcheckbutton(
    left_panel,
    text = gettext("Vertical Title Orientation", domain = "R-leem"),
    variable = var_list$title_var
  )

  ctrl_obj_list$long_segment_checkbox <- tkcheckbutton(
    left_panel,
    text = gettext("Long Segment", domain = "R-leem"),
    variable = var_list$segment_var
  )

  ctrl_obj_list$comma_checkbox <- tkcheckbutton(
    left_panel,
    text = gettext("Comma", domain = "R-leem"),
    variable = var_list$comma_var
  )

  # =========================
  # Pack widgets
  # =========================

  # Adding the sliders
  tkpack(ctrl_obj_list$q1_slider, fill = "x")
  tkpack(ctrl_obj_list$q2_slider, fill = "x")
  tkpack(ctrl_obj_list$mu_slider, fill = "x")
  tkpack(ctrl_obj_list$sigma_slider, fill = "x")
  tkpack(ctrl_obj_list$text.size_slider, fill = "x")

  # Adding the checkboxes
  tkpack(ctrl_obj_list$orientation_checkbox, anchor = "w")
  tkpack(ctrl_obj_list$long_segment_checkbox, anchor = "w")
  tkpack(ctrl_obj_list$comma_checkbox, anchor = "w")

  return(ctrl_obj_list)
}

plot_p_normal_tcltk <- function(q1, q2, q, mu, sigma, rounding,
                                minimo, maximo, dec,
                                long.segment, col,
                                col2, lty, main,
                                text.size, cex.main,
                                cex.axis, cex.lab,
                                vert.orien.main, region) {

  # Temporarily suppress warning messages during the execution
  # of the graphical interface function. This avoids displaying
  # unnecessary warnings to the user while the plot is being generated.
  #
  # The current warning option is stored in 'war' so it can be
  # restored later when the function finishes.
  warning_message <- options(warn = -1)

  # Ensure that the original warning configuration is restored
  # after the function execution, even if an error occurs.
  on.exit(options(warning_message))

  # Call the low-level plotting function responsible for generating
  # the Normal distribution graphical interface using Tcl/Tk.
  #
  # Arguments:
  # q                  -> Quantile or x-value to be highlighted.
  # mu                 -> Mean of the Normal distribution.
  # sigma              -> Standard deviation of the Normal distribution.
  # rounding           -> Number of decimal places used in labels/results.
  # minimo             -> Minimum x-axis value for plotting.
  # maximo             -> Maximum x-axis value for plotting.
  # dec                -> Decimal separator style.
  # long.segment       -> Logical value controlling segment extension.
  # col                -> Main fill or polygon color.
  # col2               -> Secondary color used in plot elements.
  # lty                -> Line type specification.
  # main               -> Main title of the plot.
  # text.size          -> Size of additional text annotations.
  # cex.main           -> Expansion factor for the main title.
  # cex.axis           -> Expansion factor for axis labels.
  # cex.lab            -> Expansion factor for axis titles.
  # vert.orien.main    -> Logical value controlling vertical title orientation.

  #./tkplotleem.R
  # .tkplotleemranormal(q1, q2, q, mu, sigma, rounding,
  #                     minimo, maximo, dec,
  #                     long.segment, col,
  #                     col2, lty, main,
  #                     text.size, cex.main,
  #                     cex.axis, cex.lab,
  #                     vert.orien.main)

  
  # Creating a list of variables
  var_list <- list()

  var_list$maximo <- maximo
  var_list$minimo <- minimo

  # Ensure the interval is at least 0.02 (2 * step)
  if (maximo - q2 < 0.02) {
    var_list$maximo <- q2 + 0.02
  }
  if (q1 - minimo < 0.02) {
    var_list$minimo <- q1 - 0.02
  }

  
  # Variables for sliders
  var_list$q1_var <- tclVar(q1)
  var_list$q2_var <- tclVar(q2)
  var_list$media_var <- tclVar(mu)
  var_list$sd_var <- tclVar(sigma)
  var_list$comma_var <- if (dec == ",") tclVar(TRUE) else tclVar(FALSE)
  var_list$segment_var <- tclVar(long.segment)
  var_list$title_var <- tclVar(vert.orien.main)
  var_list$text.size_var <- tclVar(text.size)


  # Disabled GUI (Type I)
  # This line make the tcl/tk stop update the
  # window until all the draw is completed
  oldmode <- tclServiceMode(FALSE)

  # clear all
  .Tcl("option clear")

  # Create panels
  windows_list <- create_containers_and_panels()

  # Create logos
  create_logo(windows_list$left_container)

  # =========================
  # Plot area
  # =========================
  
  # Create a canvas object inside the right container, 
  # focussed on drawning graphics and geometries
  canvas <- tkcanvas(windows_list$right_container)

  # Adding the canvas 
  tkpack(
    canvas,
    fill = "both",
    expand = TRUE
  )

  # Creating a list to gather all the control
  # objects
  ctrl_obj_list <- list()
  
  # create sliders and checkboxes
  ctrl_obj_list <- create_sliders_and_checkboxes(
                      windows_list$left_container,
                      windows_list$left_panel,
                      var_list
                    )

  # =========================
  # Generic export function
  # =========================

  save_plot <- function(type = c("png", "pdf", "svg")) {

    # Match output type
    # This function verify if the argument is 
    # a valid argument. if not, the program stops
    type <- match.arg(type)

    # File extension
    ext <- switch(
      type,
      png = ".png",
      pdf = ".pdf",
      svg = ".svg"
    )

    # File filter of saving process
    file_filter <- switch(
      type,
      png = "{{PNG files} {.png}} {{All files} *}",
      pdf = "{{PDF files} {.pdf}} {{All files} *}",
      svg = "{{SVG files} {.svg}} {{All files} *}"
    )

    # Open save dialog
    file <- tclvalue(
      tkgetSaveFile(
        defaultextension = ext,
        filetypes = file_filter,
        initialfile = paste0(
          "normal_plot_",
          Sys.Date(),
          ext
        )
      )
    )

    # If cancelled
    if (file == "") {
      return(NULL)
    }

    # =========================
    # Open graphics device
    # =========================

    if (type == "png") {
      png(
        filename = file,
        width = 2000,
        height = 1400,
        res = 300
      )

    } else if (type == "pdf") {
      pdf(
        file = file,
        width = 8,
        height = 6
      )

    } else if (type == "svg") {
      svg(
        filename = file,
        width = 8,
        height = 6
      )
    }

    # =========================
    # Generate plot
    # =========================

    plot_p_normal_plot(
      q = q,
      mu = as.numeric(tclvalue(var_list$media_var)),
      sigma = as.numeric(tclvalue(var_list$sd_var)),
      rounding = rounding,
      dec = if (tclvalue(var_list$comma_var) == "0") "." else ",",
      long.segment = as.logical(as.numeric(tclvalue(var_list$segment_var))),
      col = col,
      col2 = col2,
      lty = lty,
      main = main,
      text.size = as.numeric(tclvalue(var_list$text.size_var)),
      cex.main = as.numeric(tclvalue(var_list$text.size_var)),
      cex.axis = cex.axis,
      cex.lab = cex.lab,
      vert.orien.main = as.logical(as.numeric(tclvalue(var_list$title_var))),
      maximo = maximo,
      minimo = minimo,
      q1 = as.numeric(tclvalue(var_list$q1_var)),
      q2 = as.numeric(tclvalue(var_list$q2_var)),
      region = region
    )

    # Close graphics device
    dev.off()

    # Success message
    tkmessageBox(
      title = gettext(
        "Export",
        domain = "R-leem"
      ),
      message = gettext(
        "Image successfully exported",
        domain = "R-leem"
      ),
      icon = "info"
    )
  }

  # =========================
  # Export buttons
  # =========================

  png_button <- tkbutton(
    windows_list$left_panel,
    text = gettext("Export PNG", domain = "R-leem"),
    command = function() save_plot("png")
  )

  pdf_button <- tkbutton(
    windows_list$left_panel,
    text = gettext("Export PDF", domain = "R-leem"),
    command = function() save_plot("pdf")
  )

  svg_button <- tkbutton(
    windows_list$left_panel,
    text = gettext("Export SVG", domain = "R-leem"),
    command = function() save_plot("svg")
  )
  
  # Adding the buttons
  tkpack(png_button, fill = "x", pady = 2)
  tkpack(pdf_button, fill = "x", pady = 2)
  tkpack(svg_button, fill = "x", pady = 2)


  # Draw plot
  drawGraph <- function() {
    # Controling the warning messages
    old_warning <- getOption("warn") 
    options(warn = -1) # Set the messages of warning to not appear during the code execution  

    # Canvas dimentions
    height <- as.numeric(tclvalue(tkwinfo("height", windows_list$right_container)))
    width <- as.numeric(tclvalue(tkwinfo("width", windows_list$right_container)))

    # Getting the sliders values
    quantil1 <- as.numeric(tclvalue(var_list$q1_var))
    quantil2 <- as.numeric(tclvalue(var_list$q2_var))
    media <- as.numeric(tclvalue(var_list$media_var))
    desvpad <- as.numeric(tclvalue(var_list$sd_var))
    segment <- as.logical(as.numeric(tclvalue(var_list$segment_var)))
    textsize <- as.numeric(tclvalue(var_list$text.size_var))
    orienttext <- as.logical(as.numeric(tclvalue(var_list$title_var)))

    # Creating a temporary file to save
    # pattern = "leem.": Set a prefix term to help to identifying the file 
    temporary_file <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Create the graphic image
    png(filename = temporary_file, width = width, height = height, units = "px")
    
    try(# Generate the normal distribution plot
      plot_p_normal_plot(
        q = q,
        mu = as.numeric(tclvalue(var_list$media_var)),
        sigma = as.numeric(tclvalue(var_list$sd_var)),
        rounding = rounding,
        dec = if (tclvalue(var_list$comma_var) == "0") "." else ",",
        long.segment = as.logical(as.numeric(tclvalue(var_list$segment_var))),
        col = col,
        col2 = col2,
        lty = lty,
        main = main,
        text.size = as.numeric(tclvalue(var_list$text.size_var)),
        cex.main = as.numeric(tclvalue(var_list$text.size_var)),
        cex.axis = cex.axis,
        cex.lab = cex.lab,
        vert.orien.main = as.logical(as.numeric(tclvalue(var_list$title_var))),
        maximo = var_list$maximo,
        minimo = var_list$minimo,
        q1 = as.numeric(tclvalue(var_list$q1_var)),
        q2 = as.numeric(tclvalue(var_list$q2_var)),
        region = region
        ),
      silent = TRUE
    )

    dev.off() # Close the process

    # Create a image on Tk
    # This function take the temporary file and put it on
    # tcl/tk memory
    tkimage.create("photo", "::image::canvas_draw_image", file = temporary_file)

    # Ceaning up the canvas before draw
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::canvas_draw_image")

    options(warn = old_warning) # Restoring the standard configuration of warning
  }

  # Updating plot
  # The functions bellow allow the canvas to be recursive
  # when the some parammeter, like mu_slider, changes its value
  # the command: function(...) drawGraph() runs to update the canvas

  #tkconfigure(q1_slider, command = function(...) drawGraph())
  #tkconfigure(q2_slider, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$mu_slider, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$sigma_slider, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$text.size_slider, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$orientation_checkbox, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$long_segment_checkbox, command = function(...) drawGraph())
  tkconfigure(ctrl_obj_list$comma_checkbox, command = function(...) drawGraph())

  # Update slider_q1 to have the current value of q2 as its maximum
  tkconfigure(ctrl_obj_list$q1_slider, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(var_list$q2_var))
    tkconfigure(ctrl_obj_list$q1_slider, to = novo_q2)
    drawGraph()
  })

  # Update slider_q2 to have the current value of q1 as its minimum
  tkconfigure(ctrl_obj_list$q2_slider, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(var_list$q1_var))
    tkconfigure(ctrl_obj_list$q2_slider, from = novo_q1)
    drawGraph()
  })

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }

  # Update the graphic when the the window is resized
  tkbind(windows_list$base, '<Configure>', onResize)

  drawGraph() # First call of the draw function

  # Activate GUI
  finish <- tclServiceMode(oldmode)

  # Window of end
  tkwm.protocol(windows_list$base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(windows_list$base)
    }
  })
}














##################################################################
# lower.tail = TRUE
## (name: plot+p+dist+aux+gui)
# OBS.: aux : ltt - lower.tail == TRUE; ltf - lower.tail == FALSE;
#       ltn - low er.tail == NULL; ra - Region A; rb - Regio B
#       dist: "normal", "binomial", ...
#       gui: "plot", "rstudio", ...
##################################################################

##########################
# Continuous Distributions
##########################

# Normal distribution
#####################

# Low-level function to plot the Normal distribution highlighting P(X <= q)
# ===> This is the interface reference! <===
plotpnormallttplot <- function(q, mu, sigma, rounding, dec = c(".", ","),
                               long.segment = FALSE, col = "#8EC5E5",
                               col2 = "#38A8E8", lty = 2, main = NULL,
                               text.size = 1, cex.main = 1.2,
                               cex.axis = 1, cex.lab = 1,
                               vert.orien.main = TRUE) {

  # Define the minimum x-axis limit
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma

  # Define the maximum x-axis limit
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma

  # Sequence of x values from minimum to q
  x <- seq(minimo, q, by = 0.01)

  # Sequence of x values from q to maximum
  y <- seq(q, maximo, by = 0.01)

  # Density values for the left region
  fx <- dnorm(x, mean = mu, sd = sigma)

  # Density values for the right region
  fy <- dnorm(y, mean = mu, sd = sigma)

  # Density value at q
  pdf <- dnorm(q, mu, sigma)

  # Insert vertical line over the mean

  # Rounded value of q for display
  qq <- round(q, digits=2)

  # Auxiliary rounded value of q
  qqaux <-round(q, digits=2)

  # Cumulative probability P(X <= q)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE),
              digits=rounding)

  # Decimals in plot

  # Generate pretty values for the y-axis
  ## dec of P()
  w <- pretty(dnorm(minimo:maximo, mu, sigma))

  # Replace decimal separator if comma format is requested
  if (dec == ",") {
    Pr_text <- gsub("\\.", ",", Pr)
    qq_text <- gsub("\\.", ",", qq)
    mu_text <- gsub("\\.", ",", mu)
    sigma_text <- gsub("\\.", ",", sigma)
    pdf_text <- gsub("\\.", ",", round(pdf, 3))
  } else {

    # Keep default decimal separator
    Pr_text <- Pr
    qq_text <- qq
    pdf_text <- round(pdf, 3)
    mu_text <- mu
    sigma_text <- sigma
  }

  # Create default title if none is provided
  if (is.null(main)) {

    # Localized title text
    titulo <- gettext("Normal Distribution", domain = "R-leem")

    # Vertical orientation of the mathematical title
    if (vert.orien.main) {
      main <- substitute(atop(bold(titulo), f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1)), list(t1 = qq_text, x = "x", titulo = titulo))
    } else {

      # Horizontal orientation of the mathematical title
      main <- substitute(
        bold(titulo)~~"|"~~f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~Fx(t1)== integral(f[X](x)*"dx", -infinity, t1), list(t1 = qq_text, x = "x", titulo = titulo)
      )
    }

  }

  # Draw the Normal density curve
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main, xaxt = "n", yaxt = "n",
        cex.main = cex.main)

  # X-axis

  # Generate pretty x-axis values
  z <- pretty(minimo:maximo)

  # Draw x-axis
  axis(
    side = 1,
    at = z
  )

  # Format y-axis labels with comma decimal separator
  if (dec == ",") {

    # Y-axis
    axis(
      side = 2,
      at = w,
      cex.axis = cex.axis,
      labels = format(
        w,
        decimal.mark = ",",
        nsmall = 2
      )
    )
  } else {

    # Default y-axis labels
    # Y-axis
    axis(
      side = 2,
      at = w,
      cex.axis = cex.axis
    )
  }

  # Area  P(X<= q)

  # Shade the cumulative probability region
  ## col1 of P()
  polygon(
          c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col = col
        )

  # Shade the remaining area in gray
  # Background
  polygon(
          c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col="gray90"
        )



  # Insert red q point

  # Highlight q on the x-axis
  ## col2 of P()
  #X-axis
  mtext(qq_text, side = 1, at = qq, line = 2, col = col2, font = 2, cex = text.size)

  # Draw tick mark at q
  axis(side=1, at=qqaux, labels=FALSE,
       col=col2, font = 2, col.axis = col2, tick = TRUE,lwd.ticks = 1)

  # Insert red horizontal and vertical line (X-axis)
  axis(side=1, at=as.character(c(minimo, qqaux)), tick = TRUE, lwd = 1,
       col=col2, font = 2, lwd.ticks = 0, labels = FALSE)

  # Y-axis

  # Highlight density value at q on the y-axis
  mtext(pdf_text, side = 2, at = pdf, line = 2, col = col2, font = 2, cex = text.size)

  # Draw tick mark at density value
  axis(side=2, at=pdf, labels=FALSE,
       col=col2, font = 2, col.axis = col2, tick = TRUE,lwd.ticks = 1)

  # Long segment and type

  # Draw full or partial guide lines
  ## col2 and lty of P()
  if (isTRUE(long.segment)) {
    abline(v = qqaux, col = col2, lty = lty)
    abline(h = pdf, col = col2, lty = lty)
  } else {
    segments(qqaux, 0, qqaux, pdf, col = col2, lty = lty)
    segments(par("usr")[1], pdf, qqaux, pdf, col = col2, lty = lty)
  }

  # Add point at (q, density)
  # Point inserted
  points(qqaux, pdf, pch = 19)

  # Draw legend background rectangle
  # Rectangle topleft (Legends)
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")

  # Legends

  # Display cumulative probability legend
  legaux <- legend("topleft", bty="n", fill=col, cex=text.size,
                   legend = substitute(Fx(t1)==P(X<=t1)*"="~Pr,
                                       list(t1 = qq_text, Pr = Pr_text)))

  # Localized parameter label
  paramet <- gettext("Parameters:", domain = "R-leem")

  # Display parameter legend
  legend(minimo, legaux$text$y, bty="n", bg = "white", cex=text.size,
         legend = substitute(paramet~mu == media ~ "," ~ sigma == varen,
                             list(media = mu_text, varen = sigma_text, paramet = paramet)))
}

# RSTUDIO: Low-level function to plot the Normal distribution highlighting P(X <= q)
plotpnormallttrstudio <- function(q, mu, sigma, rounding,
                                  minimo, maximo, dec,
                                  long.segment, col,
                                  col2, lty, main,
                                  text.size, cex.main,
                                  cex.axis, cex.lab,
                                  vert.orien.main) {

  # Create an interactive Normal distribution plot using the
  # 'manipulate' package available in RStudio.
  #
  # The interface allows the user to dynamically modify
  # distribution parameters and graphical settings through
  # sliders and checkboxes.
  manipulate::manipulate(

    # Main plotting function responsible for drawing the
    # Normal distribution graph.

    # Arguments:
    # q                  -> Quantile or x-value to be highlighted.
    # mu                 -> Mean of the Normal distribution.
    # sigma              -> Standard deviation of the Normal distribution.
    # rounding           -> Number of decimal places used in labels/results.
    # minimo             -> Minimum x-axis value for plotting.
    # maximo             -> Maximum x-axis value for plotting.
    # dec                -> Decimal separator style.
    # long.segment       -> Logical value controlling segment extension.
    # col                -> Main fill or polygon color.
    # col2               -> Secondary color used in plot elements.
    # lty                -> Line type specification.
    # main               -> Main title of the plot.
    # text.size          -> Size of additional text annotations.
    # cex.main           -> Expansion factor for the main title.
    # cex.axis           -> Expansion factor for axis labels.
    # cex.lab            -> Expansion factor for axis titles.
    # vert.orien.main    -> Logical value controlling vertical title orientation.

    #./aux_probability.R
    plotpnormallttplot(
      q, mu, sigma, rounding,

      # Define the decimal separator according to the
      # checkbox state selected by the user.
      dec = if (isTRUE(decimals)) "," else ".",

      long.segment, col,
      col2, lty, main,

      # Text size used in annotations and graphical elements.
      text.size,

      # Main title size follows the same value chosen
      # for the general text size.
      cex.main = text.size,

      cex.axis, cex.lab,
      vert.orien.main
    ),

    #################################################
    # Interactive controls
    #################################################

    # Slider used to control the x-value (quantile)
    # highlighted in the Normal distribution plot.
    q = manipulate::slider(
      minimo, maximo, q,
      step = 0.01,
      label = gettext(
        "Quantile",
        domain = "R-leem"
      )
    ),

    # Slider controlling the mean of the
    # Normal distribution.
    mu = manipulate::slider(
      minimo, maximo, mu,
      step = 0.01,
      label = gettext(
        "Mean",
        domain = "R-leem"
      )
    ),

    # Slider controlling the standard deviation.
    #
    # The upper limit is defined as 1.8 times the
    # initial standard deviation value.
    sigma = manipulate::slider(
      sigma, sigma * 1.8, sigma,
      step = 0.01,
      label = gettext(
        "Standard Deviation",
        domain = "R-leem"
      )
    ),

    # Slider controlling the size of texts displayed
    # in the graph, including labels and annotations.
    text.size = manipulate::slider(
      0.8, 3, text.size,
      step = 0.01,
      label = gettext(
        "Text Size",
        domain = "R-leem"
      )
    ),

    # Checkbox that enables or disables vertical
    # orientation for the main plot title.
    vert.orien.main = checkbox(
      vert.orien.main,
      gettext(
        "Vertical Title Orientation",
        domain = "R-leem"
      )
    ),

    # Checkbox controlling whether the highlighted
    # segment should be extended.
    long.segment = checkbox(
      long.segment,
      gettext(
        "Long segment",
        domain = "R-leem"
      )
    ),

    # Checkbox used to select the decimal separator.
    #
    # TRUE  -> comma (,)
    # FALSE -> period (.)
    decimals = checkbox(
      if (dec == ",") TRUE else FALSE,
      gettext(
        "Comma",
        domain = "R-leem"
      )
    )
  )
}

##################################################################
# lower.tail = FALSE
## (name: plot+d+dist+aux+gui)
# OBS.: aux : ltt - lower.tail == TRUE; ltf - lower.tail == FALSE;
#       ltn - low er.tail == NULL; ra - Region A; rb - Regio B
#       dist: "normal", "binomial", ...
#       gui: "plot", "rstudio", ...
##################################################################

##########################
# Continuous Distributions
##########################

# Normal distribution
#####################

# PLOT: Low-level function to plot the Normal distribution highlighting P(X > q)
# ===> This is the interface reference! <===
plotpnormalltfplot <- function(q, mu, sigma, rounding, dec = c(".", ","),
                               long.segment = FALSE, col = "#8EC5E5",
                               col2 = "#38A8E8", lty = 2, main = NULL,
                               text.size = 1, cex.main = 1.2,
                               cex.axis = 1, cex.lab = 1,
                               vert.orien.main = TRUE) {
  # Define the minimum x-axis limit
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma

  # Define the maximum x-axis limit
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma

  # Sequence of x values from minimum to q
  x <- seq(minimo, q, by = 0.01)

  # Sequence of x values from q to maximum
  y <- seq(q, maximo, by = 0.01)

  # Density values for the left region
  fx <- dnorm(x, mean = mu, sd = sigma)

  # Density values for the right region
  fy <- dnorm(y, mean = mu, sd = sigma)

  # Density value at q
  pdf <- dnorm(q, mu, sigma)

  # Insert vertical line over the mean

  # Rounded value of q for display
  qq <- round(q, digits=2)

  # Auxiliary rounded value of q
  qqaux <-round(q, digits=2)

  # Cumulative probability P(X <= q)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = TRUE),
              digits=rounding)

  # Decimals in plot

  # Generate pretty values for the y-axis
  ## dec of P()
  w <- pretty(dnorm(minimo:maximo, mu, sigma))

  # Replace decimal separator if comma format is requested
  if (dec == ",") {
    Pr_text <- gsub("\\.", ",", Pr)
    qq_text <- gsub("\\.", ",", qq)
    mu_text <- gsub("\\.", ",", mu)
    sigma_text <- gsub("\\.", ",", sigma)
    pdf_text <- gsub("\\.", ",", round(pdf, 3))
  } else {

    # Keep default decimal separator
    Pr_text <- Pr
    qq_text <- qq
    pdf_text <- round(pdf, 3)
    mu_text <- mu
    sigma_text <- sigma
  }

  if (is.null(main)) {
    titulo <- gettext("Normal Distribution", domain = "R-leem")
    main = substitute(atop(bold(titulo),
                           f[X](x*";"~mu*","~sigma) == frac(1, symbol(sigma)*root(2*symbol(pi)))*~e^-frac(1,2)(frac(x-symbol(mu),sigma))^2*","~~S[X](t)~"="~1 - F[X](t)~"="*1 - integral(f[X](x)*"dx", -infinity, t)~"="*P(X > t) == integral(f[X](x)*"dx", t, infinity)),
                      list(t = q, titulo = titulo))
  }
  curve(dnorm(x, mean = mu, sd = sigma), minimo, maximo,
        ylim = c(0, 1.2*max(fx,fy)), ylab = expression(f[X](x)), xlab="X",
        panel.first = grid(col = "gray90"),
        main = main, xaxt = "n", yaxt = "n",
        cex = 0.8)
  polygon(c(x, rev(x)),
          c(fx, rep(0, length(fx))),
          col="gray90")
  polygon(c(y, rev(y)),
          c(fy, rep(0, length(fy))),
          col=col)

  # Insert vertical line over the mean
  qq <- round(q, digits=2)
  qqaux <-round(q, digits=2)
  Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = FALSE), digits=rounding)

  # Decimals in plot
  ## dec of P()
  w <- pretty(dnorm(minimo:maximo, mu, sigma))
  if (dec == ",") {
    Pr_text <- gsub("\\.", ",", Pr)
    qq_text <- gsub("\\.", ",", qq)
    pdf_text <- gsub("\\.", ",", round(pdf, 3))
    # Y-axis
    axis(
      side = 2,
      at = w,
      labels = format(
        w,
        decimal.mark = ",",
        nsmall = 2
      )
    )
  } else {
    Pr_text <- Pr
    qq_text <- qq
    pdf_text <- round(pdf, 3)
    # Y-axis
    axis(
      side = 2,
      at = w
    )
  }

  # Insert red q point
  #X-axis
  mtext(qq_text, side = 1, at = qqaux, line = 2, col = col2, font = 2)
  axis(side=1, at=as.character(qqaux), tick = TRUE, lwd = 1,
       col=col2, font = 2, lwd.ticks = 1, labels = FALSE)

  # Y-axis
  mtext(pdf_text, side = 2, at = pdf, line = 2, col = col2, font = 2)
  axis(side=2, at=pdf, labels=FALSE,
       col=col2, font = 2, col.axis = col2, tick = TRUE,lwd.ticks = 1)

  # Long segment and type
  ## col2 and lty of P()
  if (isTRUE(long.segment)) {
    abline(v = qqaux, col = col2, lty = lty)
    abline(h = pdf, col = col2, lty = lty)
  } else {
    segments(qqaux, 0, qqaux, pdf, col = col2, lty = lty)
    segments(par("usr")[1], pdf, qqaux, pdf, col = col2, lty = lty)
  }
  # Point inserted
  points(qqaux, pdf, pch = 19)

  # Rectangle topleft (Legends)
  rect(par("usr")[1], 1.03 * max(fx,fy), par("usr")[2], par("usr")[4], col = "gray")

  # Legends
  legaux <- legend("topleft", bty="n", fill=col,cex=0.8,
                   legend = substitute(S[X](q)~"="~1-F[X](q)~"="~P(X > q) == Pr,
                                       list(q = qq_text, Pr = Pr_text)))
  parametro <- gettext("Parameters:", domain = "R-leem")
  legend(minimo, legaux$text$y, bty="n", bg = "white",cex=0.8,
         legend = substitute(parametro~mu ==  mean ~ "," ~ sigma == varen,
                             list(mean = mu, varen = sigma,
                                  parametro = parametro)))
}



############################################################################
## lower.tail == NULL (name: plot+d+dist+aux+gui)
# OBS.: aux : ltt - lower.tail == TRUE; ltf - lower.tail == FALSE;
#       ltn - low er.tail == NULL; ra - Region A; rb - Regio B
#       dist: "normal", "binomial", ...
#       gui: "plot", "rstudio", ...
############################################################################

##########################
# Continuous Distributions
##########################

# Normal distribution
#####################

# PLOT: Low-level function to plot the Normal distribution highlighting f_X(X = q)
plotdnormalltnplot <- function(q, mu, sigma, rounding, dec = c(".", ","),
                               long.segment = FALSE, col = "#8EC5E5",
                               col2 = "#38A8E8", lty = 2, main = NULL,
                               text.size = 1, cex.main = 1.2,
                               cex.axis = 1, cex.lab = 1,
                               vert.orien.main = TRUE) {

  # Arguments:
  # q                  -> Quantile or x-value to be highlighted.
  # mu                 -> Mean of the Normal distribution.
  # sigma              -> Standard deviation of the Normal distribution.
  # rounding           -> Number of decimal places used in labels/results.
  # minimo             -> Minimum x-axis value for plotting.
  # maximo             -> Maximum x-axis value for plotting.
  # dec                -> Decimal separator style.
  # long.segment       -> Logical value controlling segment extension.
  # col                -> Main fill or polygon color.
  # col2               -> Secondary color used in plot elements.
  # lty                -> Line type specification.
  # main               -> Main title of the plot.
  # text.size          -> Size of additional text annotations.
  # cex.main           -> Expansion factor for the main title.
  # cex.axis           -> Expansion factor for axis labels.
  # cex.lab            -> Expansion factor for axis titles.
  # vert.orien.main    -> Logical value controlling vertical title orientation.


  ###############################################################
  # Definition of the plotting interval
  ###############################################################

  # Define the minimum value of the x-axis.
  # If q is far to the left of the distribution center,
  # expand the graph to include q.
  minimo <- if (q <=  mu - 4 * sigma) q - 4 * sigma else mu - 4 * sigma

  # Define the maximum value of the x-axis.
  # If q is far to the right of the distribution center,
  # expand the graph to include q.
  maximo <- if (q > mu + 4 * sigma) q + 4 * sigma else mu + 4 * sigma

  ###############################################################
  # Generation of points for the Normal density curve
  ###############################################################

  # Create a sequence of x values used to draw the density curve
  y <- seq(minimo, maximo, by = 0.01)

  # Compute the Normal density values for each point in y
  fy <- dnorm(y, mean = mu, sd = sigma)

  # Compute the density value at the specific point q
  pdf <- dnorm(q, mu, sigma)

  ###############################################################
  # Rounded values used for display in the graph
  ###############################################################

  # Rounded value of q for display purposes
  qq <- round(q, digits = 2)

  # Auxiliary rounded value of q used in axes and segments
  qqaux <- round(q, digits = 2)

  ###############################################################
  # Cumulative probability associated with q
  ###############################################################

  # Compute P(X <= q) using the Normal cumulative distribution
  # function and round the result according to the user input
  Pr <- round(
    pnorm(
      qq,
      mean = mu,
      sd = sigma,
      lower.tail = TRUE
    ),
    digits = rounding
  )

  ###############################################################
  # Generation of pretty values for the y-axis
  ###############################################################

  # Generate aesthetically pleasant y-axis tick values
  w <- pretty(dnorm(minimo:maximo, mu, sigma))

  ###############################################################
  # Formatting decimal separator
  ###############################################################

  # If the user requests comma decimal notation,
  # replace "." by "," in all displayed numeric values
  if (dec == ",") {

    Pr_text <- gsub("\\.", ",", Pr)

    qq_text <- gsub("\\.", ",", qq)

    mu_text <- gsub("\\.", ",", mu)

    sigma_text <- gsub("\\.", ",", sigma)

    pdf_text <- gsub("\\.", ",", round(pdf, 3))

  } else {

    # Keep default decimal notation using points
    Pr_text <- Pr

    qq_text <- qq

    pdf_text <- round(pdf, 3)

    mu_text <- mu

    sigma_text <- sigma
  }

  ###############################################################
  # Main title generation
  ###############################################################

  # If the user did not provide a custom title,
  # automatically create one
  if (is.null(main)) {

    # Localized title obtained from translation files
    titulo <- gettext("Normal Distribution", domain = "R-leem")

    #############################################################
    # Vertical mathematical title
    #############################################################

    if (vert.orien.main) {

      # Create a two-line title using mathematical notation
      main <- substitute(
        atop(
          bold(titulo),
          f[X](x * ";" ~ mu * "," ~ sigma) ==
            frac(
              1,
              symbol(sigma) * root(2 * symbol(pi))
            ) *
            ~e^-frac(
              1,
              2
            )(
              frac(
                x - symbol(mu),
                sigma
              )
            )^2
        ),
        list(
          t1 = qq_text,
          x = "x",
          titulo = titulo
        )
      )

    } else {

      ###########################################################
      # Horizontal mathematical title
      ###########################################################

      main <- substitute(
        bold(titulo) ~~ "|" ~~
          f[X](x * ";" ~ mu * "," ~ sigma) ==
          frac(
            1,
            symbol(sigma) * root(2 * symbol(pi))
          ) *
          ~e^-frac(
            1,
            2
          )(
            frac(
              x - symbol(mu),
              sigma
            )
          )^2,
        list(
          t1 = qq_text,
          x = "x",
          titulo = titulo
        )
      )
    }
  }

  ###############################################################
  # Draw the Normal density curve
  ###############################################################

  curve(
    dnorm(x, mean = mu, sd = sigma),
    minimo,
    maximo,

    # Limits of the y-axis
    ylim = c(0, 1.2 * max(fy)),

    # Axis labels
    ylab = expression(f[X](x)),
    xlab = "X",

    # Background grid
    panel.first = grid(col = "gray90"),

    # Main title
    main = main,

    # Disable automatic axes
    xaxt = "n",
    yaxt = "n",

    # title scaling
    cex.main = cex.main
  )

  ###############################################################
  # X-axis creation
  ###############################################################

  # Generate aesthetically pleasant x-axis tick values
  z <- pretty(minimo:maximo)

  # Draw x-axis
  axis(
    side = 1,
    at = z
  )

  ###############################################################
  # Draw shaded polygon under the density curve
  ###############################################################

  polygon(
    c(y, rev(y)),
    c(fy, rep(0, length(fy))),
    col = "gray90"
  )

  ###############################################################
  # Y-axis formatting
  ###############################################################

  # If comma decimal separator is requested
  if (dec == ",") {

    axis(
      side = 2,
      at = w,
      cex.axis = cex.axis,

      # Replace decimal separator in labels
      labels = format(
        w,
        decimal.mark = ",",
        nsmall = 2
      )
    )

  } else {

    # Default y-axis
    axis(
      side = 2,
      at = w,
      cex.axis = cex.axis
    )
  }

  ###############################################################
  # Redraw x-axis
  ###############################################################

  # Generate pretty x-axis values
  z <- pretty(minimo:maximo)

  # Draw x-axis
  axis(
    side = 1,
    at = z
  )

  ###############################################################
  # Highlight the value q on the x-axis
  ###############################################################

  # Add text label corresponding to q
  mtext(
    qq_text,
    side = 1,
    at = qq,
    line = 2,
    col = col2,
    font = 2,
    cex = text.size
  )

  ###############################################################
  # Tick mark at q
  ###############################################################

  axis(
    side = 1,
    at = qqaux,
    labels = FALSE,
    col = col2,
    font = 2,
    col.axis = col2,
    tick = TRUE,
    lwd.ticks = 1
  )

  ###############################################################
  # Additional auxiliary x-axis marks
  ###############################################################

  axis(
    side = 1,
    at = as.character(c(minimo, qqaux)),
    tick = TRUE,
    lwd = 1,
    col = col2,
    font = 2,
    lwd.ticks = 0,
    labels = FALSE
  )

  ###############################################################
  # Highlight density value on the y-axis
  ###############################################################

  # Add density value label
  mtext(
    pdf_text,
    side = 2,
    at = pdf,
    line = 2,
    col = col2,
    font = 2,
    cex = text.size
  )

  ###############################################################
  # Tick mark at the density value
  ###############################################################

  axis(
    side = 2,
    at = pdf,
    labels = FALSE,
    col = col2,
    font = 2,
    col.axis = col2,
    tick = TRUE,
    lwd.ticks = 1
  )

  ###############################################################
  # Guide segments or complete lines
  ###############################################################

  # If long.segment = TRUE, draw full reference lines
  if (isTRUE(long.segment)) {

    # Vertical guide line
    abline(v = qqaux, col = col2, lty = lty)

    # Horizontal guide line
    abline(h = pdf, col = col2, lty = lty)

  } else {

    # Draw only the segment from the x-axis to the point
    segments(
      qqaux,
      0,
      qqaux,
      pdf,
      col = col2,
      lty = lty
    )

    # Draw only the segment from the y-axis to the point
    segments(
      par("usr")[1],
      pdf,
      qqaux,
      pdf,
      col = col2,
      lty = lty
    )
  }

  ###############################################################
  # Highlight the point (q, f(q))
  ###############################################################

  points(q, pdf, pch = 19)

  ###############################################################
  # Draw background rectangle for legends
  ###############################################################

  rect(
    par("usr")[1],
    1.03 * max(fy),
    par("usr")[2],
    par("usr")[4],
    col = "gray"
  )

  ###############################################################
  # Legend displaying the density value
  ###############################################################

  legaux <- legend(
    "topleft",
    bty = "n",
    pt.cex = 1.2,
    pch = 19,
    cex = text.size,

    # Mathematical legend
    legend = substitute(
      f[X](t1) == pdf,
      list(
        t1 = qq_text,
        pdf = pdf_text
      )
    )
  )

  ###############################################################
  # Parameter legend
  ###############################################################

  # Localized label for parameters
  paramet <- gettext("Parameters:", domain = "R-leem")

  # Display parameters mu and sigma
  legend(
    par("usr")[1],
    legaux$text$y,
    bty = "n",
    bg = "white",
    cex = text.size,

    legend = substitute(
      paramet ~ mu == media ~ "," ~ sigma == varen,
      list(
        media = mu_text,
        varen = sigma_text,
        paramet = paramet
      )
    )
  )
}

# RSTUDIO: Low-level function to plot the Normal distribution highlighting f_X(X = q)
plotdnormalltnrstudio <- function(q, mu, sigma, rounding,
                                  minimo, maximo, dec,
                                  long.segment, col,
                                  col2, lty, main,
                                  text.size, cex.main,
                                  cex.axis, cex.lab,
                                  vert.orien.main) {

  # Create an interactive Normal distribution plot using the
  # 'manipulate' package available in RStudio.
  #
  # The interface allows the user to dynamically modify
  # distribution parameters and graphical settings through
  # sliders and checkboxes.
  manipulate::manipulate(

    # Main plotting function responsible for drawing the
    # Normal distribution graph.

    # Arguments:
    # q                  -> Quantile or x-value to be highlighted.
    # mu                 -> Mean of the Normal distribution.
    # sigma              -> Standard deviation of the Normal distribution.
    # rounding           -> Number of decimal places used in labels/results.
    # minimo             -> Minimum x-axis value for plotting.
    # maximo             -> Maximum x-axis value for plotting.
    # dec                -> Decimal separator style.
    # long.segment       -> Logical value controlling segment extension.
    # col                -> Main fill or polygon color.
    # col2               -> Secondary color used in plot elements.
    # lty                -> Line type specification.
    # main               -> Main title of the plot.
    # text.size          -> Size of additional text annotations.
    # cex.main           -> Expansion factor for the main title.
    # cex.axis           -> Expansion factor for axis labels.
    # cex.lab            -> Expansion factor for axis titles.
    # vert.orien.main    -> Logical value controlling vertical title orientation.

    #./aux_probability.R
    plotdnormalltnplot(
      q, mu, sigma, rounding,

      # Define the decimal separator according to the
      # checkbox state selected by the user.
      dec = if (isTRUE(decimals)) "," else ".",

      long.segment, col,
      col2, lty, main,

      # Text size used in annotations and graphical elements.
      text.size,

      # Main title size follows the same value chosen
      # for the general text size.
      cex.main = text.size,

      cex.axis, cex.lab,
      vert.orien.main
    ),

    #################################################
    # Interactive controls
    #################################################

    # Slider used to control the x-value (quantile)
    # highlighted in the Normal distribution plot.
    q = manipulate::slider(
      minimo, maximo, q,
      step = 0.01,
      label = "X"
    ),

    # Slider controlling the mean of the
    # Normal distribution.
    mu = manipulate::slider(
      minimo, maximo, mu,
      step = 0.01,
      label = gettext(
        "Mean",
        domain = "R-leem"
      )
    ),

    # Slider controlling the standard deviation.
    #
    # The upper limit is defined as 1.8 times the
    # initial standard deviation value.
    sigma = manipulate::slider(
      sigma, sigma * 1.8, sigma,
      step = 0.01,
      label = gettext(
        "Standard Deviation",
        domain = "R-leem"
      )
    ),

    # Slider controlling the size of texts displayed
    # in the graph, including labels and annotations.
    text.size = manipulate::slider(
      0.8, 3, text.size,
      step = 0.01,
      label = gettext(
        "Text Size",
        domain = "R-leem"
      )
    ),

    # Checkbox that enables or disables vertical
    # orientation for the main plot title.
    vert.orien.main = checkbox(
      vert.orien.main,
      gettext(
        "Vertical Title Orientation",
        domain = "R-leem"
      )
    ),

    # Checkbox controlling whether the highlighted
    # segment should be extended.
    long.segment = checkbox(
      long.segment,
      gettext(
        "Long segment",
        domain = "R-leem"
      )
    ),

    # Checkbox used to select the decimal separator.
    #
    # TRUE  -> comma (,)
    # FALSE -> period (.)
    decimals = checkbox(
      if (dec == ",") TRUE else FALSE,
      gettext(
        "Comma",
        domain = "R-leem"
      )
    )
  )
}

# TCLTK: Low-level function to plot the Normal distribution highlighting f_X(X = q)
plotdnormalltntcltk <- function(q, mu, sigma, rounding,
                                minimo, maximo, dec,
                                long.segment, col,
                                col2, lty, main,
                                text.size, cex.main,
                                cex.axis, cex.lab,
                                vert.orien.main) {

  # Temporarily suppress warning messages during the execution
  # of the graphical interface function. This avoids displaying
  # unnecessary warnings to the user while the plot is being generated.
  #
  # The current warning option is stored in 'war' so it can be
  # restored later when the function finishes.
  war <- options(warn = -1)

  # Ensure that the original warning configuration is restored
  # after the function execution, even if an error occurs.
  on.exit(options(war))

  # Call the low-level plotting function responsible for generating
  # the Normal distribution graphical interface using Tcl/Tk.
  #
  # Arguments:
  # q                  -> Quantile or x-value to be highlighted.
  # mu                 -> Mean of the Normal distribution.
  # sigma              -> Standard deviation of the Normal distribution.
  # rounding           -> Number of decimal places used in labels/results.
  # minimo             -> Minimum x-axis value for plotting.
  # maximo             -> Maximum x-axis value for plotting.
  # dec                -> Decimal separator style.
  # long.segment       -> Logical value controlling segment extension.
  # col                -> Main fill or polygon color.
  # col2               -> Secondary color used in plot elements.
  # lty                -> Line type specification.
  # main               -> Main title of the plot.
  # text.size          -> Size of additional text annotations.
  # cex.main           -> Expansion factor for the main title.
  # cex.axis           -> Expansion factor for axis labels.
  # cex.lab            -> Expansion factor for axis titles.
  # vert.orien.main    -> Logical value controlling vertical title orientation.

  #./tkplotleem.R
  .tkplotleemltnnormal(q, mu, sigma, rounding,
                       minimo, maximo, dec,
                       long.segment, col,
                       col2, lty, main,
                       text.size, cex.main,
                       cex.axis, cex.lab,
                       vert.orien.main)
}

# SHINY: Low-level function to plot the Normal distribution highlighting f_X(X = q)

plotdnormalltnshiny <- function(q, mu, sigma, rounding, porcentage,
                                minimo, maximo, dec,
                                long.segment, col,
                                col2, lty, main,
                                browser.shiny = getOption("shiny.launch.browser", interactive()),
                                text.size, cex.main,
                                cex.axis, cex.lab,
                                vert.orien.main) {

  # Arguments:
  ############
  # q                  -> Quantile or x-value to be highlighted.
  # mu                 -> Mean of the Normal distribution.
  # sigma              -> Standard deviation of the Normal distribution.
  # rounding           -> Number of decimal places used in labels/results.
  # porcentage         -> Convert the probability to percentage format.
  # minimo             -> Minimum x-axis value for plotting.
  # maximo             -> Maximum x-axis value for plotting.
  # dec                -> Decimal separator style.
  # long.segment       -> Logical value controlling segment extension.
  # col                -> Main fill or polygon color.
  # col2               -> Secondary color used in plot elements.
  # lty                -> Line type specification.
  # main               -> Main title of the plot.
  # browser.shiny      -> Indicates whether the Shiny app should be launched in the browser.
  # text.size          -> Size of additional text annotations.
  # cex.main           -> Expansion factor for the main title.
  # cex.axis           -> Expansion factor for axis labels.
  # cex.lab            -> Expansion factor for axis titles.
  # vert.orien.main    -> Logical value controlling vertical title orientation.

  #################################################
  # Probability calculation
  #################################################

  # Compute the cumulative probability associated
  # with the Normal distribution.
  #
  # The function pnorm() returns:
  # P(X <= q)
  #
  # where:
  # q     -> quantile or cutoff value
  # mu    -> mean of the Normal distribution
  # sigma -> standard deviation
  dens <- dnorm(
    x = q,
    mean = mu,
    sd = sigma
  )

  # Convert the probability to percentage format
  # if the user requested percentage output.
  #
  # Example:
  # 0.95 -> 95
  # if (porcentage == TRUE) {
  #   probability <- probability * 100
  # }

  #################################################
  # Create output object
  #################################################

  # Store all results in a list object.
  #
  # Elements:
  #
  # probability   -> Calculated cumulative probability.
  #
  # process_shiny -> Object generated by the internal
  #                  Shiny plotting function responsible
  #                  for building the interactive plot.
  #
  # browser.shiny -> Indicates whether the Shiny app
  #                  should be launched in the browser.
  listres <- list(
    probability = dens,
    #./shinyplotleem.R
    process_shiny = .shinyplotleemltnnormal(
      q, mu, sigma, rounding,
      minimo, maximo, dec,
      long.segment, col,
      col2, lty, main,
      text.size, cex.main,
      cex.axis, cex.lab,
      vert.orien.main
    ),

    browser.shiny = browser.shiny
  )

  #################################################
  # Object metadata
  #################################################

  # Add an attribute identifying the type of output
  # produced by this function.
  attr(listres, "output") <- "pshiny"

  # Assign the S3 class used internally by the package.
  class(listres) <- "leem"

  #################################################
  # Return final object
  #################################################

  return(listres)
}

# Discrete Distributions





