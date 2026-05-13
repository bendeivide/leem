.tkTCL <- function(n = 30,
                   nsim = 1000,
                   dist = "normal",
                   mean = 0,
                   sd = 1,
                   lambda = 1,
                   prob = 0.5,
                   size = 10,
                   lambda.pois = 5,
                   col = "lightblue",
                   border = "white",
                   breaks = "FD",
                   dec = getOption("OutDec"),
                   main = NULL) {

  ## testing function...
  # n = 30;
  # nsim = 1000;
  # dist = "normal";
  # mean = 0;
  # sd = 1;
  # lambda = 1;
  # prob = 0.5;
  # size = 10;
  # lambda.pois = 5;
  # col = "lightblue";
  # border = "white";
  # breaks = "FD";
  # dec = getOption("OutDec");
  # main = NULL

  # Main window
  base <- tcltk::tktoplevel()

  # Window title
  tcltk::tkwm.title(
    base,
    gettext(
      "Central Limit Theorem",
      domain = "R-leem"
    )
  )

  # Window geometry
  tcltk::tkwm.geometry(
    base,
    "600x700"
  )

  # Set window icon
  icon.path <- system.file(
    "etc",
    "leem-icon.png",
    package = "leem"
  )

  if (file.exists(icon.path)) {

    icon.img <- tcltk::tkimage.create(
      "photo",
      file = icon.path
    )

    tcltk::tcl(
      "wm",
      "iconphoto",
      base,
      "-default",
      icon.img
    )

  }

  # Tcl variables
  n.var <- tcltk::tclVar(n)

  nsim.var <- tcltk::tclVar(nsim)

  mean.var <- tcltk::tclVar(mean)

  sd.var <- tcltk::tclVar(sd)

  lambda.var <- tcltk::tclVar(lambda)

  prob.var <- tcltk::tclVar(prob)

  size.var <- tcltk::tclVar(size)

  pois.var <- tcltk::tclVar(lambda.pois)

  dist.var <- tcltk::tclVar(dist)

  # Frames
  plot.frame <- tcltk::tkframe(base)

  controls.frame <- tcltk::tkframe(base)

  # Plot function
  # plot.fun <- function() {
  #
  #   TCL(
  #
  #     n = as.numeric(
  #       tcltk::tclvalue(n.var)
  #     ),
  #
  #     nsim = as.numeric(
  #       tcltk::tclvalue(nsim.var)
  #     ),
  #
  #     dist = tcltk::tclvalue(dist.var),
  #
  #     mean = as.numeric(
  #       tcltk::tclvalue(mean.var)
  #     ),
  #
  #     sd = as.numeric(
  #       tcltk::tclvalue(sd.var)
  #     ),
  #
  #     lambda = as.numeric(
  #       tcltk::tclvalue(lambda.var)
  #     ),
  #
  #     prob = as.numeric(
  #       tcltk::tclvalue(prob.var)
  #     ),
  #
  #     size = as.numeric(
  #       tcltk::tclvalue(size.var)
  #     ),
  #
  #     lambda.pois = as.numeric(
  #       tcltk::tclvalue(pois.var)
  #     ),
  #
  #     col = col,
  #
  #     border = border,
  #
  #     breaks = breaks,
  #
  #     dec = dec,
  #
  #     main = main
  #
  #   )
  #
  # }
  #
  # # Plot widget
  # img <- tkrplot::tkrplot(
  #
  #   plot.frame,
  #
  #   fun = plot.fun,
  #
  #   hscale = 1.5,
  #
  #   vscale = 1.5
  #
  # )
  #
  # # Update plot
  # update.plot <- function(...) {
  #
  #   tkrplot::tkrreplot(img)
  #
  # }

  # Distribution label
  dist.label <- tcltk::tklabel(

    controls.frame,

    text = gettext(
      "Distribution",
      domain = "R-leem"
    )

  )

  # Distribution menu button
  dist.menu.button <- tcltk::tkmenubutton(

    controls.frame,

    text = gettext(
      "Choose distribution",
      domain = "R-leem"
    ),

    relief = "raised"

  )

  # Distribution menu
  dist.menu <- tcltk::tkmenu(

    dist.menu.button,

    tearoff = FALSE

  )

  # Attach menu
  tcltk::tkconfigure(

    dist.menu.button,

    menu = dist.menu

  )

  # Available distributions
  distributions <- c(

    "normal",

    "uniform",

    "exponential",

    "binomial",

    "poisson"

  )

  # Add menu items
  for (d in distributions) {

    tcltk::tkadd(

      dist.menu,

      "command",

      label = d,

      command = function(dd = d) {

        tcltk::tclvalue(dist.var) <- dd

        #update.plot()

      }

    )

  }

  # Sample size slider
  n.slider <- tcltk::tkscale(

    controls.frame,

    from = 2,

    to = 300,

    variable = n.var,

    orient = "horizontal",

    resolution = 1,

    label = gettext(
      "Sample size",
      domain = "R-leem"
    )

    # command = update.plot

  )

  # Number of simulations slider
  nsim.slider <- tcltk::tkscale(

    controls.frame,

    from = 100,

    to = 10000,

    variable = nsim.var,

    orient = "horizontal",

    resolution = 100,

    label = gettext(
      "Number of simulations",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Mean slider
  mean.slider <- tcltk::tkscale(

    controls.frame,

    from = -10,

    to = 10,

    variable = mean.var,

    orient = "horizontal",

    resolution = 0.1,

    label = gettext(
      "Mean",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Standard deviation slider
  sd.slider <- tcltk::tkscale(

    controls.frame,

    from = 0.1,

    to = 10,

    variable = sd.var,

    orient = "horizontal",

    resolution = 0.1,

    label = gettext(
      "Standard deviation",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Exponential lambda slider
  lambda.slider <- tcltk::tkscale(

    controls.frame,

    from = 0.1,

    to = 10,

    variable = lambda.var,

    orient = "horizontal",

    resolution = 0.1,

    label = gettext(
      "Lambda",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Binomial probability slider
  prob.slider <- tcltk::tkscale(

    controls.frame,

    from = 0.01,

    to = 0.99,

    variable = prob.var,

    orient = "horizontal",

    resolution = 0.01,

    label = gettext(
      "Probability",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Binomial size slider
  size.slider <- tcltk::tkscale(

    controls.frame,

    from = 1,

    to = 100,

    variable = size.var,

    orient = "horizontal",

    resolution = 1,

    label = gettext(
      "Binomial size",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Poisson lambda slider
  pois.slider <- tcltk::tkscale(

    controls.frame,

    from = 0.1,

    to = 30,

    variable = pois.var,

    orient = "horizontal",

    resolution = 0.1,

    label = gettext(
      "Poisson lambda",
      domain = "R-leem"
    )

    #command = update.plot

  )

  # Close protocol
  tcltk::tkwm.protocol(

    base,

    "WM_DELETE_WINDOW",

    function() {

      response <- tcltk::tk_messageBox(

        title = gettext(
          "Warning",
          domain = "R-leem"
        ),

        message = gettext(
          "Do you want to close?",
          domain = "R-leem"
        ),

        icon = "question",

        type = "yesno"

      )

      if (response == "yes") {

        tcltk::tkdestroy(base)

      }

    }

  )

  # Pack plot frame
  tcltk::tkpack(

    plot.frame,

    side = "top",

    fill = "both",

    expand = TRUE

  )

  # Pack plot
  # tcltk::tkpack(
  #
  #   img,
  #
  #   fill = "both",
  #
  #   expand = TRUE
  #
  # )

  # Pack controls frame
  tcltk::tkpack(

    controls.frame,

    side = "bottom",

    fill = "x"

  )

  # Pack distribution widgets
  tcltk::tkpack(

    dist.label,

    dist.menu.button,

    anchor = "w",

    padx = 10,

    pady = 4

  )

  # Pack sliders
  slidefun <- function(...) {
    tcltk::tkpack(n.slider,
                  nsim.slider,
                  ...,
                  fill = "x", padx = 10, pady = 2)
  }

  if (dist == "normal") slidefun(mean.slider, sd.slider)
  if (dist == "expotential") slidefun(lambda.slider)
  if (dist == "binomial") slidefun(prob.slider, size.slider)
  if (dist == "poisson") slidefun(pois.slider)
  if (dist == "uniform") slidefun()



  invisible(base)

}
