#########################################################################
# "These functions only work correctly if executed through function P()."
#########################################################################

# Normal distribution
normal_distrubution <- function(q, argaddit, rounding, main, gui, lower.tail, dec, col, col2, long.segment, lty) {

  # Verifying is there's any arg named "mean"
  # if not, ask for one
  if (!any(names(argaddit) == "mean")) {
    # Geting the mean arg from the user
    mean <- readline(gettext("Insert the value of 'mean' argument: ", domain = "R-leem"))
    # Puting it in a object named "mean" (creating the mean object at the same time)
    argaddit$mean <- as.numeric(mean)
  }

  # Verifying is there's any arg named "sd"
  # if not, ask for one
  if (!any(names(argaddit) == "sd")) {
    sd <- readline(gettext("Insert the value of 'sd' argument: ", domain = "R-leem"))
    argaddit$sd <- as.numeric(sd)
  }

  # Verifying if the sd is equal to 0
  # if is, change the value
  while (argaddit$sd <= 0) {
    arg1 <- gettext("Please, Insert the value of 'sd' greater then 0:", domain = "R-leem")
    sd <- readline(paste0(arg1, " "))
    argaddit$sd <- as.numeric(sd)
  }

  mu <- argaddit$mean
  sigma <- argaddit$sd

  if (length(q) > 1){
    # Auxiliar variables
    minimo <- if (q[1] <= argaddit$mean - 4 * argaddit$sd) q[1] - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
    maximo <- if (q[2] > argaddit$mean + 4 * argaddit$sd) q[2] + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
  } else {
    minimo <- if (q <=  argaddit$mean - 4 * argaddit$sd) q - 4 * argaddit$sd else argaddit$mean - 4 * argaddit$sd
    maximo <- if (q > argaddit$mean + 4 * argaddit$sd) q + 4 * argaddit$sd else argaddit$mean + 4 * argaddit$sd
  }

  #########################################
  # Starting call the gui
  #########################################

  # Verifying if Lower Tail is true
  if(isTRUE(lower.tail)) {
    if (gui == "plot" ) {
      plotpnormallttplot(q, mu, sigma, rounding, dec, long.segment, col, col2, lty, main)
    }
    if (gui == "rstudio") {
      manipulate::manipulate(plotpnormallttplot(q, mean, sd, rounding, dec, long.segment, col, col2, lty, main),
                             q = manipulate::slider(q, mu + 4 * sigma, q),
                             mean = manipulate::slider(mu, mu + 2 * sigma, mu),
                             sd = manipulate::slider(sigma, sigma * 1.8, sigma)
      )
    }

    if (gui == "tcltk") {
      # Desabilitar warnings global
      #options(warn = - ufs1)
      war <- options(warn = - 1)
      #on.exit(options(war))

      # Plot tk da dist normal com q de comp 1 (~/tkplotleem.R)
      .tkplotleemnormal(q, mu, sigma, rounding, minimo, maximo, dec, long.segment, col,
                        col2, lty, main)


      # Desabilitar warnings global
      #options(warn = - 1)
      #war <- options(warn = - 1)
      on.exit(options(war))
    }
    # Compute the desired probability
    prob <- pnorm(q = q, mean = mu, sd = sigma)
    return(prob)
  }

  if(isFALSE(lower.tail)) {
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

    return(prob)
  }

  if(is.null(lower.tail)) {
    if (gui == "plot") {
      plotpnormalltnplot(q, mu, sigma, rounding, dec,
                         long.segment, col,
                         lty, main)
    }

    if (gui == "rstudio") {
      manipulate::manipulate(plotpnormalltnplot(q, mean, sd, rounding, dec,
                                                long.segment, col,
                                                lty, main),
                             q = manipulate::slider(mu - 4 * sigma, mu + 4 * sigma, q, step = 0.01),
                             mean = manipulate::slider(-abs(q), mu + 3 * sigma, mu, step = 0.01),
                             sd = manipulate::slider(sigma, sigma * 1.8, sigma, step = 0.01)
      )
    }

    if (gui == "tcltk") {
      # Desabilitar warnings global
      #options(warn = - 1)
      war <- options(warn = - 1)
      #on.exit(options(war))

      .tkplotleemltnnormal(q, mu, sigma, rounding, minimo, maximo, dec,
                           long.segment, col,
                           lty, main)


      # Desabilitar warnings global
      #options(warn = - 1)
      #war <- options(warn = - 1)
      on.exit(options(war))
    }

    # Compute the probability density function
    prob <- dnorm(x = q, mean = mu, sd=sigma)
    return(prob)
  }

  #########################################
  # Starting call the gui
  # where Lower tail is none and q > 1
  #########################################


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
  }

  # Calculates the desired probability
  prob <- pnorm(q[1], mean = mu, sd = sigma, lower.tail = T) +
    pnorm(q[2], mean = mu, sd = sigma, lower.tail = F)

  return(prob)
}
