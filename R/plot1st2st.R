.plot1st2st <- function(...) {

  # Style TFrame
  .Tcl("ttk::style configure leem.TFrame -relief solid")
  # Style PanedWindow
  .Tcl("ttk::style configure leem.TPanedwindow -relief solid")

  # Disabled GUI
  oldmode <- tclServiceMode(FALSE)

  # Window function
  func <- tktoplevel(width = 700, height = 700)

  # Not propagate
  tkpack.propagate(func, FALSE)

  # Title
  tkwm.title(func,
             gettext("1st and 2nd degree functions", domain = "R-leem"))

  ############
  # Overall group
  ############
  #group.all <- NULL
  group.all <- ttkpanedwindow(func, orient = "vertical", width = 700, height = 700)
  tkpack(group.all, expand = TRUE, fill = "both")

  ##############
  # Child groups
  ##############
  # Top-level window
  group1 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 250)
  tkadd(group.all, group1, weight = 1)
  ##
  group2 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 450)
  tkadd(group.all, group2, weight = 2)
  ##
  group3 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 50)
  tkadd(group.all, group3, weight = 1)

  #######################
  # Child of Child groups
  #######################
  #Q1
  child1.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3), width = 300)
  tkadd(group1, child1.group1)

  #Q2
  child2.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group1, child2.group1)
  #Q3
  child1.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group2, child1.group2)
  #Q4
  #child2.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
  #                          padding = c(3,3,3,3))
  #tkadd(group2, child2.group2,weight = 3)

  ## Child 1
  tkpack(frameaux <- ttkframe(child1.group1),
                       expand = TRUE, anchor = "n", side = "top")

  tkpack(coef <- ttklabelframe(frameaux,
                               text = gettext("Coefficients",
                                              domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  tkpack(limaxes <- ttklabelframe(frameaux,
                               text = gettext("Limit of axes",
                                              domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  ### Limit axes
  tkpack(xaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("xlimits",
                                domain = "R-leem"),
                 parent = xaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_xlim <- tclVar(gettext("8", domain = "R-leem"))
  ##
  tkpack(entry_xaxe <- tkentry(parent = xaxe,
                                textvariable = txt_xlim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  tkpack(yaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("ylimits",
                                domain = "R-leem"),
                 parent = yaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_ylim <- tclVar(gettext("4", domain = "R-leem"))
  ##
  tkpack(entry_yaxe <- tkentry(parent = yaxe,
                                textvariable = txt_ylim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  ### Coefficients
  # Coef-a
  tkpack(coefa <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("a",
                                domain = "R-leem"),
                 parent = coefa),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_vara <- tclVar(gettext("1", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefa,
                                textvariable = txt_vara),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-b
  tkpack(coefb <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("b",
                                domain = "R-leem"),
                 parent = coefb),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varb <- tclVar(gettext("-2", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefb,
                                textvariable = txt_varb),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-c
  tkpack(coefc <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("c",
                                domain = "R-leem"),
                 parent = coefc),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varc <- tclVar(gettext("-3", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefc,
                                textvariable = txt_varc),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")



  ## Child 2
  tkpack(adit1 <- ttklabelframe(child1.group1,
                                text = gettext("1st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st11 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue11 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st11,
                                 text = gettext("Interpretation of intercept (c)", domain = "R-leem"),
                                 variable = aditvalue11,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st12 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue12 <- tclVar("FALSE")
  tkpack(adit12 <- tkcheckbutton(parent = st12,
                                 text = gettext("Interpretation of the angular coefficient (b)", domain = "R-leem"),
                                 variable = aditvalue12,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")

  ## Child 3
  tkpack(adit2 <- ttklabelframe(child1.group1,
                                text = gettext("2st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st21 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue21 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st21,
                                 text = gettext("Root(s)", domain = "R-leem"),
                                 variable = aditvalue21,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st22 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue22 <- tclVar("FALSE")
  tkpack(adit22 <- tkcheckbutton(parent = st22,
                                 text = gettext("Maximum or minimum", domain = "R-leem"),
                                 variable = aditvalue22,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")


  ## Child 4
  tkpack(results <- ttklabelframe(child2.group1,
                                  text = gettext("Results",
                                                 domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")
  # Frame
  tkpack(fres <- tkframe(results), anchor = "n",
         expand = FALSE, fill = "x")
  # Label
  tkpack(res <- tklabel(fres, text = gettext("The results...", domain = "R-leem")), anchor = "n",
         expand = FALSE, fill = "x")

  ## Child 5
  tkpack(plot1 <- ttklabelframe(child1.group2,
                                text = gettext("Plot",
                                               domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")


  # Calculate

  calculate_button <- ttkbutton(text = gettext("Calculate",
                                               domain = "R-leem"),
                                parent = group3, width = 200)

  tkadd(group3, calculate_button)

  tkplot <- function(parent, fun, hscale=1, vscale=1, ...) {
    # tkrplot::.make.tkindex()
    image <- paste("Rplot", tkrplot::.make.tkindex(), sep = "")
    # tkrplot::.my.tkdev()
    tkrplot::.my.tkdev(hscale, vscale)
    try(fun())
    tcltk::.Tcl(paste("image create Rplot", image))
    lab <- tcltk::tktext(parent, height = 50)
    tcl(lab, "image", "create", "5.0", "-image", image, "-align", "center")
    tkbind(lab,"<Destroy>", function() .Tcl(paste("image delete", image)))
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
  }

  # Scrollbars
  .addScrollbars <- function(parent, widget,type=c("both", "x", "y")) {
    if(any(type %in% c("both","x"))) {
      xscr <- ttkscrollbar(parent, orient = "horizontal",
                           command = function(...) tkxview(widget, ...))
      tkconfigure(widget,
                  xscrollcommand = function(...) tkset(xscr,...))
    }

    if(any(type %in% c("both","y"))) {
      yscr <- ttkscrollbar(parent, orient = "vertical",
                           command = function(...) tkyview(widget, ...))
      tkconfigure(widget,
                  yscrollcommand = function(...) tkset(yscr,...))
    }

    ## place in grid
    tkgrid(widget, row=0, column=0, sticky="news")
    if(any(type %in% c("both", "x"))) {
      tkgrid(xscr, row=1, column=0, sticky="ew")
      tkgrid.columnconfigure(parent, 0, weight=1)
    }
    if(any(type %in% c("both", "y"))) {
      tkgrid(yscr,row=0,column=1, sticky="ns")
      tkgrid.rowconfigure(parent, 0, weight=1)
    }


  }
  fator <- 1
  plotleem <- TRUE

  # Scale plot
  f <- function(...) {
    fatorup <- as.numeric(tclvalue("fator"))
    if (fator != fatorup) {
      fator <<- fatorup
      tkrplot::tkrreplot(plotleem, hscale = fator, vscale = fator)
    }
  }



  # Auxiliar function
  faux <- function(a, b, d, xlimits = c(-eval(parse(text = tclvalue(txt_xlim))), eval(parse(text = tclvalue(txt_xlim)))),
                   ylimits = c(-eval(parse(text = tclvalue(txt_ylim))), eval(parse(text = tclvalue(txt_xlim))))) {
    x1 <- round((-b + sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    x2 <- round((-b - sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    par(mfrow=c(1,1), family = "serif")
    aux <- curve(a * x^2 + b*x + d, -8, 8, xlim = xlimits, ylim = ylimits, axes = T, xlab = "x", ylab = "f(x)", lwd = 3)
    # Rectangle
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
           "grey", border = NA)
    # Additional
    lines(aux, lwd = 3)
    #points(aux, lwd = 3, col = "red")
    # Grid
    grid(20)
    # Axes
    abline(h = 0, v = 0, col = "blue")
    if (!is.nan(x1) & x1 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x1, 0, col = "red", pch = 16)
        text(x1 + diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x1)
      }
    }
    if (!is.nan(x2) & x2 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x2, 0, col = "red", pch = 16)
        text(x2 - diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x2)
      }
    }
    if (eval(parse(text = tclvalue(aditvalue22)))) {
      # Maximum or minimum point
      fdev <- eval(substitute(expression(a * x^2 + b * x + d), list(a = a, b = b, d = d)))
      dx <- D(D(fdev, "x"), "x")
      if (eval(dx) > 0) {
        point <- gettext("Minimum point", domain = "R-leem")
      } else {
        point <- gettext("Maximun point", domain = "R-leem")
      }
      ## Critic points
      xcrit <- -b / (2 * a)
      ycrit <- -(b^2 - 4 * a * d) / (4 * a)
      points(xcrit, ycrit, col = "green", pch = 19)
      abline(h = ycrit, col = "green")
      text(0 , ycrit,
           labels = ycrit)
      abline(v = xcrit, col = "green")
      text(xcrit , 0,
           labels = xcrit)
    }
    #Update plots
    sapply(as.character(tkwinfo("children", fres)),
           function(W) tcl("destroy", W))

    typefun <- if (a == 0) paste(gettext("(1st degree function)", domain = "R-leem")) else paste(gettext("(2st degree function)", domain = "R-leem"))
    tfun <- paste(a, "x^2 + ", b, "x + ", d, " ", typefun, sep = "")
    coefang <- if (a == 0) paste(gettext("Angular coefficient: ", domain = "R-leem"), b) else ""
    coeflin <- if (a == 0) paste(gettext("Intercept: ", domain = "R-leem"), d) else ""
    int2 <- if (eval(parse(text = tclvalue(aditvalue12)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), b, ": ", gettext("For each increase of one unit of x, \n an increase/decrease will occur from ", domain = "R-leem"), b,
            gettext(" to y.", domain = "R-leem"), sep = "")
    }
    int1 <- if (eval(parse(text = tclvalue(aditvalue11)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), d, ": ", gettext("When x = 0, y will assume ", domain = "R-leem"), d,
            gettext(". Otherwise, the intercection point \n between the line and the y-axis is (0, ", domain = "R-leem"), d, ").", sep = "")
    }
    pmaxmin <- if (a != 0 & eval(parse(text = tclvalue(aditvalue22)))) paste(gettext("Maximum/Minimum point: ", domain = "R-leem"), "(", xcrit, ", ", ycrit, ")", sep = "") else ""
    roots <- if (a != 0 & eval(parse(text = tclvalue(aditvalue21)))) paste(gettext("Root(s):", domain = "R-leem"), x1, ",", x2) else ""
    texts <- paste(gettext("Results... \n", domain = "R-leem"),
                   gettext("Function: ", domain = "R-leem"), tfun, "\n",
                   pmaxmin, "\n",
                   roots, "\n",
                   coeflin, "\n",
                   coefang, "\n\n",
                   int1, "\n\n",
                   int2
                   )

    # Label
    tkpack(res <- tklabel(fres, text = texts), side = "left",
           expand = FALSE, fill = "x")

  }

  # Plot and scale together
  plotaux1 <- function(...) {
    #Update plots
    sapply(as.character(tkwinfo("children", plot1)),
           function(W) tcl("destroy", W))
    # Scale plot
    tkpack(splot <- ttkframe(plot1), side = "top", anchor = "n", fill = "x", expand = TRUE)

    # Frame
    tkpack(fplot <- ttkframe(plot1), side = "top", fill = "x", expand = TRUE)



    plotleem <<- tkplot(parent = fplot,
                       fun = function(...){
                         faux(
                           a = eval(parse(text = tclvalue(txt_vara))),
                           b = eval(parse(text = tclvalue(txt_varb))),
                           d = eval(parse(text = tclvalue(txt_varc)))
                         )
                       },
                       hscale = fator, vscale = fator)

    # Scale of plot
    s <- tkscale(splot, command = f, from = 1, to = 3.00, variable = "fator",
                 showvalue = TRUE, resolution = 0.05, orient = "horiz", borderwidth = 0)
    # Drawing the widget
    tkpack(s, fill = "both", side = "top", anchor = "n", ipady = 5)
    #tkpack.configure(s, fill = "both", side = "top", ipady = 5)
    # Plot and scrollbar
    tkpack(plotleem,  fill = "both", expand = TRUE)
    .addScrollbars(fplot, plotleem)
  }

  tkbind(calculate_button, "<ButtonRelease>", function(...) plotaux1())

  # Activate GUI
  finish <- tclServiceMode(oldmode)
}
##################
##################
##################
##################
##################
##################
##################
##################
##################
.mposition<- function(...) {

  # Style TFrame
  .Tcl("ttk::style configure leem.TFrame -relief solid")
  # Style PanedWindow
  .Tcl("ttk::style configure leem.TPanedwindow -relief solid")

  # Disabled GUI
  oldmode <- tclServiceMode(FALSE)

  # Window function
  func <- tktoplevel(width = 700, height = 700)

  # Not propagate
  tkpack.propagate(func, FALSE)

  # Title
  tkwm.title(func,
             gettext("Measures of position", domain = "R-leem"))

  ############
  # Overall group
  ############
  #group.all <- NULL
  group.all <- ttkpanedwindow(func, orient = "vertical", width = 700, height = 700)
  tkpack(group.all, expand = TRUE, fill = "both")

  ##############
  # Child groups
  ##############
  # Top-level window
  group1 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 250)
  tkadd(group.all, group1, weight = 1)
  ##
  group2 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 450)
  tkadd(group.all, group2, weight = 2)
  ##
  group3 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 50)
  tkadd(group.all, group3, weight = 1)

  #######################
  # Child of Child groups
  #######################
  #Q1
  child1.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3), width = 300)
  tkadd(group1, child1.group1)

  #Q2
  child2.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group1, child2.group1)
  #Q3
  child1.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group2, child1.group2)
  #Q4
  #child2.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
  #                          padding = c(3,3,3,3))
  #tkadd(group2, child2.group2,weight = 3)

  ## Child 1
  tkpack(frameaux <- ttkframe(child1.group1),
         expand = TRUE, anchor = "n", side = "top")

  tkpack(coef <- ttklabelframe(frameaux,
                               text = gettext("Coefficients",
                                              domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  tkpack(limaxes <- ttklabelframe(frameaux,
                                  text = gettext("Limit of axes",
                                                 domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  ### Limit axes
  tkpack(xaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("xlimits",
                                domain = "R-leem"),
                 parent = xaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_xlim <- tclVar(gettext("8", domain = "R-leem"))
  ##
  tkpack(entry_xaxe <- tkentry(parent = xaxe,
                               textvariable = txt_xlim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  tkpack(yaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("ylimits",
                                domain = "R-leem"),
                 parent = yaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_ylim <- tclVar(gettext("4", domain = "R-leem"))
  ##
  tkpack(entry_yaxe <- tkentry(parent = yaxe,
                               textvariable = txt_ylim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  ### Coefficients
  # Coef-a
  tkpack(coefa <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("a",
                                domain = "R-leem"),
                 parent = coefa),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_vara <- tclVar(gettext("1", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefa,
                                textvariable = txt_vara),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-b
  tkpack(coefb <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("b",
                                domain = "R-leem"),
                 parent = coefb),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varb <- tclVar(gettext("-2", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefb,
                                textvariable = txt_varb),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-c
  tkpack(coefc <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("c",
                                domain = "R-leem"),
                 parent = coefc),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varc <- tclVar(gettext("-3", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefc,
                                textvariable = txt_varc),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")



  ## Child 2
  tkpack(adit1 <- ttklabelframe(child1.group1,
                                text = gettext("1st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st11 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue11 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st11,
                                 text = gettext("Interpretation of intercept (c)", domain = "R-leem"),
                                 variable = aditvalue11,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st12 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue12 <- tclVar("FALSE")
  tkpack(adit12 <- tkcheckbutton(parent = st12,
                                 text = gettext("Interpretation of the angular coefficient (b)", domain = "R-leem"),
                                 variable = aditvalue12,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")

  ## Child 3
  tkpack(adit2 <- ttklabelframe(child1.group1,
                                text = gettext("2st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st21 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue21 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st21,
                                 text = gettext("Root(s)", domain = "R-leem"),
                                 variable = aditvalue21,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st22 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue22 <- tclVar("FALSE")
  tkpack(adit22 <- tkcheckbutton(parent = st22,
                                 text = gettext("Maximum or minimum", domain = "R-leem"),
                                 variable = aditvalue22,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")


  ## Child 4
  tkpack(results <- ttklabelframe(child2.group1,
                                  text = gettext("Results",
                                                 domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")
  # Frame
  tkpack(fres <- tkframe(results), anchor = "n",
         expand = FALSE, fill = "x")
  # Label
  tkpack(res <- tklabel(fres, text = gettext("The results...", domain = "R-leem")), anchor = "n",
         expand = FALSE, fill = "x")

  ## Child 5
  tkpack(plot1 <- ttklabelframe(child1.group2,
                                text = gettext("Plot",
                                               domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")


  # Calculate

  calculate_button <- ttkbutton(text = gettext("Calculate",
                                               domain = "R-leem"),
                                parent = group3, width = 200)

  tkadd(group3, calculate_button)

  tkplot <- function(parent, fun, hscale=1, vscale=1, ...) {
    # tkrplot::.make.tkindex()
    image <- paste("Rplot", tkrplot::.make.tkindex(), sep = "")
    # tkrplot::.my.tkdev()
    tkrplot::.my.tkdev(hscale, vscale)
    try(fun())
    tcltk::.Tcl(paste("image create Rplot", image))
    lab <- tcltk::tktext(parent, height = 50)
    tcl(lab, "image", "create", "5.0", "-image", image, "-align", "center")
    tkbind(lab,"<Destroy>", function() .Tcl(paste("image delete", image)))
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
  }

  # Scrollbars
  .addScrollbars <- function(parent, widget,type=c("both", "x", "y")) {
    if(any(type %in% c("both","x"))) {
      xscr <- ttkscrollbar(parent, orient = "horizontal",
                           command = function(...) tkxview(widget, ...))
      tkconfigure(widget,
                  xscrollcommand = function(...) tkset(xscr,...))
    }

    if(any(type %in% c("both","y"))) {
      yscr <- ttkscrollbar(parent, orient = "vertical",
                           command = function(...) tkyview(widget, ...))
      tkconfigure(widget,
                  yscrollcommand = function(...) tkset(yscr,...))
    }

    ## place in grid
    tkgrid(widget, row=0, column=0, sticky="news")
    if(any(type %in% c("both", "x"))) {
      tkgrid(xscr, row=1, column=0, sticky="ew")
      tkgrid.columnconfigure(parent, 0, weight=1)
    }
    if(any(type %in% c("both", "y"))) {
      tkgrid(yscr,row=0,column=1, sticky="ns")
      tkgrid.rowconfigure(parent, 0, weight=1)
    }


  }
  fator <- 1
  plotleem <- TRUE

  # Scale plot
  f <- function(...) {
    fatorup <- as.numeric(tclvalue("fator"))
    if (fator != fatorup) {
      fator <<- fatorup
      tkrplot::tkrreplot(plotleem, hscale = fator, vscale = fator)
    }
  }



  # Auxiliar function
  faux <- function(a, b, d, xlimits = c(-eval(parse(text = tclvalue(txt_xlim))), eval(parse(text = tclvalue(txt_xlim)))),
                   ylimits = c(-eval(parse(text = tclvalue(txt_ylim))), eval(parse(text = tclvalue(txt_xlim))))) {
    x1 <- round((-b + sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    x2 <- round((-b - sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    par(mfrow=c(1,1), family = "serif")
    aux <- curve(a * x^2 + b*x + d, -8, 8, xlim = xlimits, ylim = ylimits, axes = T, xlab = "x", ylab = "f(x)", lwd = 3)
    # Rectangle
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
           "grey", border = NA)
    # Additional
    lines(aux, lwd = 3)
    #points(aux, lwd = 3, col = "red")
    # Grid
    grid(20)
    # Axes
    abline(h = 0, v = 0, col = "blue")
    if (!is.nan(x1) & x1 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x1, 0, col = "red", pch = 16)
        text(x1 + diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x1)
      }
    }
    if (!is.nan(x2) & x2 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x2, 0, col = "red", pch = 16)
        text(x2 - diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x2)
      }
    }
    if (eval(parse(text = tclvalue(aditvalue22)))) {
      # Maximum or minimum point
      fdev <- eval(substitute(expression(a * x^2 + b * x + d), list(a = a, b = b, d = d)))
      dx <- D(D(fdev, "x"), "x")
      if (eval(dx) > 0) {
        point <- gettext("Minimum point", domain = "R-leem")
      } else {
        point <- gettext("Maximun point", domain = "R-leem")
      }
      ## Critic points
      xcrit <- -b / (2 * a)
      ycrit <- -(b^2 - 4 * a * d) / (4 * a)
      points(xcrit, ycrit, col = "green", pch = 19)
      abline(h = ycrit, col = "green")
      text(0 , ycrit,
           labels = ycrit)
      abline(v = xcrit, col = "green")
      text(xcrit , 0,
           labels = xcrit)
    }
    #Update plots
    sapply(as.character(tkwinfo("children", fres)),
           function(W) tcl("destroy", W))

    typefun <- if (a == 0) paste(gettext("(1st degree function)", domain = "R-leem")) else paste(gettext("(2st degree function)", domain = "R-leem"))
    tfun <- paste(a, "x^2 + ", b, "x + ", d, " ", typefun, sep = "")
    coefang <- if (a == 0) paste(gettext("Angular coefficient: ", domain = "R-leem"), b) else ""
    coeflin <- if (a == 0) paste(gettext("Intercept: ", domain = "R-leem"), d) else ""
    int2 <- if (eval(parse(text = tclvalue(aditvalue12)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), b, ": ", gettext("For each increase of one unit of x, \n an increase/decrease will occur from ", domain = "R-leem"), b,
            gettext(" to y.", domain = "R-leem"), sep = "")
    }
    int1 <- if (eval(parse(text = tclvalue(aditvalue11)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), d, ": ", gettext("When x = 0, y will assume ", domain = "R-leem"), d,
            gettext(". Otherwise, the intercection point \n between the line and the y-axis is (0, ", domain = "R-leem"), d, ").", sep = "")
    }
    pmaxmin <- if (a != 0 & eval(parse(text = tclvalue(aditvalue22)))) paste(gettext("Maximum/Minimum point: ", domain = "R-leem"), "(", xcrit, ", ", ycrit, ")", sep = "") else ""
    roots <- if (a != 0 & eval(parse(text = tclvalue(aditvalue21)))) paste(gettext("Root(s):", domain = "R-leem"), x1, ",", x2) else ""
    texts <- paste(gettext("Results... \n", domain = "R-leem"),
                   gettext("Function: ", domain = "R-leem"), tfun, "\n",
                   pmaxmin, "\n",
                   roots, "\n",
                   coeflin, "\n",
                   coefang, "\n\n",
                   int1, "\n\n",
                   int2
    )

    # Label
    tkpack(res <- tklabel(fres, text = texts), side = "left",
           expand = FALSE, fill = "x")

  }

  # Plot and scale together
  plotaux1 <- function(...) {
    #Update plots
    sapply(as.character(tkwinfo("children", plot1)),
           function(W) tcl("destroy", W))
    # Scale plot
    tkpack(splot <- ttkframe(plot1), side = "top", anchor = "n", fill = "x", expand = TRUE)

    # Frame
    tkpack(fplot <- ttkframe(plot1), side = "top", fill = "x", expand = TRUE)



    plotleem <<- tkplot(parent = fplot,
                        fun = function(...){
                          faux(
                            a = eval(parse(text = tclvalue(txt_vara))),
                            b = eval(parse(text = tclvalue(txt_varb))),
                            d = eval(parse(text = tclvalue(txt_varc)))
                          )
                        },
                        hscale = fator, vscale = fator)

    # Scale of plot
    s <- tkscale(splot, command = f, from = 1, to = 3.00, variable = "fator",
                 showvalue = TRUE, resolution = 0.05, orient = "horiz", borderwidth = 0)
    # Drawing the widget
    tkpack(s, fill = "both", side = "top", anchor = "n", ipady = 5)
    #tkpack.configure(s, fill = "both", side = "top", ipady = 5)
    # Plot and scrollbar
    tkpack(plotleem,  fill = "both", expand = TRUE)
    .addScrollbars(fplot, plotleem)
  }

  tkbind(calculate_button, "<ButtonRelease>", function(...) plotaux1())

  # Activate GUI
  finish <- tclServiceMode(oldmode)
}
###########################
###########################
###########################
###########################
###########################
###########################
.mdispersion <- function(...) {

  # Style TFrame
  .Tcl("ttk::style configure leem.TFrame -relief solid")
  # Style PanedWindow
  .Tcl("ttk::style configure leem.TPanedwindow -relief solid")

  # Disabled GUI
  oldmode <- tclServiceMode(FALSE)

  # Window function
  func <- tktoplevel(width = 700, height = 700)

  # Not propagate
  tkpack.propagate(func, FALSE)

  # Title
  tkwm.title(func,
             gettext("Measures of position", domain = "R-leem"))

  ############
  # Overall group
  ############
  #group.all <- NULL
  group.all <- ttkpanedwindow(func, orient = "vertical", width = 700, height = 700)
  tkpack(group.all, expand = TRUE, fill = "both")

  ##############
  # Child groups
  ##############
  # Top-level window
  group1 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 250)
  tkadd(group.all, group1, weight = 1)
  ##
  group2 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 450)
  tkadd(group.all, group2, weight = 2)
  ##
  group3 <- ttkpanedwindow(group.all, orient = "horizontal", style = "leem.TPanedwindow", height = 50)
  tkadd(group.all, group3, weight = 1)

  #######################
  # Child of Child groups
  #######################
  #Q1
  child1.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3), width = 300)
  tkadd(group1, child1.group1)

  #Q2
  child2.group1 <- ttkframe(parent = group1, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group1, child2.group1)
  #Q3
  child1.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
                            padding = c(3,3,3,3))
  tkadd(group2, child1.group2)
  #Q4
  #child2.group2 <- ttkframe(parent = group2, style = "leem.TFrame",
  #                          padding = c(3,3,3,3))
  #tkadd(group2, child2.group2,weight = 3)

  ## Child 1
  tkpack(frameaux <- ttkframe(child1.group1),
         expand = TRUE, anchor = "n", side = "top")

  tkpack(coef <- ttklabelframe(frameaux,
                               text = gettext("Coefficients",
                                              domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  tkpack(limaxes <- ttklabelframe(frameaux,
                                  text = gettext("Limit of axes",
                                                 domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "left")

  ### Limit axes
  tkpack(xaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("xlimits",
                                domain = "R-leem"),
                 parent = xaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_xlim <- tclVar(gettext("8", domain = "R-leem"))
  ##
  tkpack(entry_xaxe <- tkentry(parent = xaxe,
                               textvariable = txt_xlim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  tkpack(yaxe <- tkframe(limaxes), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("ylimits",
                                domain = "R-leem"),
                 parent = yaxe),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_ylim <- tclVar(gettext("4", domain = "R-leem"))
  ##
  tkpack(entry_yaxe <- tkentry(parent = yaxe,
                               textvariable = txt_ylim),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")

  ### Coefficients
  # Coef-a
  tkpack(coefa <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("a",
                                domain = "R-leem"),
                 parent = coefa),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_vara <- tclVar(gettext("1", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefa,
                                textvariable = txt_vara),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-b
  tkpack(coefb <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("b",
                                domain = "R-leem"),
                 parent = coefb),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varb <- tclVar(gettext("-2", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefb,
                                textvariable = txt_varb),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")
  # Coef-c
  tkpack(coefc <- tkframe(coef), anchor = "n",
         expand = FALSE, fill = "x")
  tkpack(tklabel(text = gettext("c",
                                domain = "R-leem"),
                 parent = coefc),
         anchor = "center", padx = 2, pady = 1,
         expand = TRUE, fill = "x", side = "left")
  # Entry
  txt_varc <- tclVar(gettext("-3", domain = "R-leem"))
  ##
  tkpack(entry_coefa <- tkentry(parent = coefc,
                                textvariable = txt_varc),
         anchor = "nw", padx = "1m", ipadx = "2m", side = "left",
         expand = TRUE, fill = "x")



  ## Child 2
  tkpack(adit1 <- ttklabelframe(child1.group1,
                                text = gettext("1st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st11 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue11 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st11,
                                 text = gettext("Interpretation of intercept (c)", domain = "R-leem"),
                                 variable = aditvalue11,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st12 <- tkframe(adit1), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue12 <- tclVar("FALSE")
  tkpack(adit12 <- tkcheckbutton(parent = st12,
                                 text = gettext("Interpretation of the angular coefficient (b)", domain = "R-leem"),
                                 variable = aditvalue12,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")

  ## Child 3
  tkpack(adit2 <- ttklabelframe(child1.group1,
                                text = gettext("2st degree function Additional",
                                               domain = "R-leem")),
         fill = "x", expand = TRUE, anchor = "n", side = "top")
  # Checkbox
  tkpack(st21 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue21 <- tclVar("FALSE")
  tkpack(adit11 <- tkcheckbutton(parent = st21,
                                 text = gettext("Root(s)", domain = "R-leem"),
                                 variable = aditvalue21,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")
  # Checkbox
  tkpack(st22 <- tkframe(adit2), anchor = "n",
         expand = FALSE, fill = "x")
  aditvalue22 <- tclVar("FALSE")
  tkpack(adit22 <- tkcheckbutton(parent = st22,
                                 text = gettext("Maximum or minimum", domain = "R-leem"),
                                 variable = aditvalue22,
                                 onvalue = "TRUE",
                                 offvalue = "FALSE"),
         anchor = "nw", padx = "1m", side = "left")


  ## Child 4
  tkpack(results <- ttklabelframe(child2.group1,
                                  text = gettext("Results",
                                                 domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")
  # Frame
  tkpack(fres <- tkframe(results), anchor = "n",
         expand = FALSE, fill = "x")
  # Label
  tkpack(res <- tklabel(fres, text = gettext("The results...", domain = "R-leem")), anchor = "n",
         expand = FALSE, fill = "x")

  ## Child 5
  tkpack(plot1 <- ttklabelframe(child1.group2,
                                text = gettext("Plot",
                                               domain = "R-leem")),
         fill = "both", expand = TRUE, anchor = "n")


  # Calculate

  calculate_button <- ttkbutton(text = gettext("Calculate",
                                               domain = "R-leem"),
                                parent = group3, width = 200)

  tkadd(group3, calculate_button)

  tkplot <- function(parent, fun, hscale=1, vscale=1, ...) {
    # tkrplot::.make.tkindex()
    image <- paste("Rplot", tkrplot::.make.tkindex(), sep = "")
    # tkrplot::.my.tkdev()
    tkrplot::.my.tkdev(hscale, vscale)
    try(fun())
    tcltk::.Tcl(paste("image create Rplot", image))
    lab <- tcltk::tktext(parent, height = 50)
    tcl(lab, "image", "create", "5.0", "-image", image, "-align", "center")
    tkbind(lab,"<Destroy>", function() .Tcl(paste("image delete", image)))
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
  }

  # Scrollbars
  .addScrollbars <- function(parent, widget,type=c("both", "x", "y")) {
    if(any(type %in% c("both","x"))) {
      xscr <- ttkscrollbar(parent, orient = "horizontal",
                           command = function(...) tkxview(widget, ...))
      tkconfigure(widget,
                  xscrollcommand = function(...) tkset(xscr,...))
    }

    if(any(type %in% c("both","y"))) {
      yscr <- ttkscrollbar(parent, orient = "vertical",
                           command = function(...) tkyview(widget, ...))
      tkconfigure(widget,
                  yscrollcommand = function(...) tkset(yscr,...))
    }

    ## place in grid
    tkgrid(widget, row=0, column=0, sticky="news")
    if(any(type %in% c("both", "x"))) {
      tkgrid(xscr, row=1, column=0, sticky="ew")
      tkgrid.columnconfigure(parent, 0, weight=1)
    }
    if(any(type %in% c("both", "y"))) {
      tkgrid(yscr,row=0,column=1, sticky="ns")
      tkgrid.rowconfigure(parent, 0, weight=1)
    }


  }
  fator <- 1
  plotleem <- TRUE

  # Scale plot
  f <- function(...) {
    fatorup <- as.numeric(tclvalue("fator"))
    if (fator != fatorup) {
      fator <<- fatorup
      tkrplot::tkrreplot(plotleem, hscale = fator, vscale = fator)
    }
  }



  # Auxiliar function
  faux <- function(a, b, d, xlimits = c(-eval(parse(text = tclvalue(txt_xlim))), eval(parse(text = tclvalue(txt_xlim)))),
                   ylimits = c(-eval(parse(text = tclvalue(txt_ylim))), eval(parse(text = tclvalue(txt_xlim))))) {
    x1 <- round((-b + sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    x2 <- round((-b - sqrt(b^2 - 4 * a * d)) / 2 *a, 2)
    par(mfrow=c(1,1), family = "serif")
    aux <- curve(a * x^2 + b*x + d, -8, 8, xlim = xlimits, ylim = ylimits, axes = T, xlab = "x", ylab = "f(x)", lwd = 3)
    # Rectangle
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
           "grey", border = NA)
    # Additional
    lines(aux, lwd = 3)
    #points(aux, lwd = 3, col = "red")
    # Grid
    grid(20)
    # Axes
    abline(h = 0, v = 0, col = "blue")
    if (!is.nan(x1) & x1 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x1, 0, col = "red", pch = 16)
        text(x1 + diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x1)
      }
    }
    if (!is.nan(x2) & x2 != 0) {
      if (eval(parse(text = tclvalue(aditvalue21)))) {
        points(x2, 0, col = "red", pch = 16)
        text(x2 - diff(xlimits) * 0.02, diff(ylimits) * 0.02, labels = x2)
      }
    }
    if (eval(parse(text = tclvalue(aditvalue22)))) {
      # Maximum or minimum point
      fdev <- eval(substitute(expression(a * x^2 + b * x + d), list(a = a, b = b, d = d)))
      dx <- D(D(fdev, "x"), "x")
      if (eval(dx) > 0) {
        point <- gettext("Minimum point", domain = "R-leem")
      } else {
        point <- gettext("Maximun point", domain = "R-leem")
      }
      ## Critic points
      xcrit <- -b / (2 * a)
      ycrit <- -(b^2 - 4 * a * d) / (4 * a)
      points(xcrit, ycrit, col = "green", pch = 19)
      abline(h = ycrit, col = "green")
      text(0 , ycrit,
           labels = ycrit)
      abline(v = xcrit, col = "green")
      text(xcrit , 0,
           labels = xcrit)
    }
    #Update plots
    sapply(as.character(tkwinfo("children", fres)),
           function(W) tcl("destroy", W))

    typefun <- if (a == 0) paste(gettext("(1st degree function)", domain = "R-leem")) else paste(gettext("(2st degree function)", domain = "R-leem"))
    tfun <- paste(a, "x^2 + ", b, "x + ", d, " ", typefun, sep = "")
    coefang <- if (a == 0) paste(gettext("Angular coefficient: ", domain = "R-leem"), b) else ""
    coeflin <- if (a == 0) paste(gettext("Intercept: ", domain = "R-leem"), d) else ""
    int2 <- if (eval(parse(text = tclvalue(aditvalue12)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), b, ": ", gettext("For each increase of one unit of x, \n an increase/decrease will occur from ", domain = "R-leem"), b,
            gettext(" to y.", domain = "R-leem"), sep = "")
    }
    int1 <- if (eval(parse(text = tclvalue(aditvalue11)))) {
      paste(gettext("Interpretation of ", domain = "R-leem"), d, ": ", gettext("When x = 0, y will assume ", domain = "R-leem"), d,
            gettext(". Otherwise, the intercection point \n between the line and the y-axis is (0, ", domain = "R-leem"), d, ").", sep = "")
    }
    pmaxmin <- if (a != 0 & eval(parse(text = tclvalue(aditvalue22)))) paste(gettext("Maximum/Minimum point: ", domain = "R-leem"), "(", xcrit, ", ", ycrit, ")", sep = "") else ""
    roots <- if (a != 0 & eval(parse(text = tclvalue(aditvalue21)))) paste(gettext("Root(s):", domain = "R-leem"), x1, ",", x2) else ""
    texts <- paste(gettext("Results... \n", domain = "R-leem"),
                   gettext("Function: ", domain = "R-leem"), tfun, "\n",
                   pmaxmin, "\n",
                   roots, "\n",
                   coeflin, "\n",
                   coefang, "\n\n",
                   int1, "\n\n",
                   int2
    )

    # Label
    tkpack(res <- tklabel(fres, text = texts), side = "left",
           expand = FALSE, fill = "x")

  }

  # Plot and scale together
  plotaux1 <- function(...) {
    #Update plots
    sapply(as.character(tkwinfo("children", plot1)),
           function(W) tcl("destroy", W))
    # Scale plot
    tkpack(splot <- ttkframe(plot1), side = "top", anchor = "n", fill = "x", expand = TRUE)

    # Frame
    tkpack(fplot <- ttkframe(plot1), side = "top", fill = "x", expand = TRUE)



    plotleem <<- tkplot(parent = fplot,
                        fun = function(...){
                          faux(
                            a = eval(parse(text = tclvalue(txt_vara))),
                            b = eval(parse(text = tclvalue(txt_varb))),
                            d = eval(parse(text = tclvalue(txt_varc)))
                          )
                        },
                        hscale = fator, vscale = fator)

    # Scale of plot
    s <- tkscale(splot, command = f, from = 1, to = 3.00, variable = "fator",
                 showvalue = TRUE, resolution = 0.05, orient = "horiz", borderwidth = 0)
    # Drawing the widget
    tkpack(s, fill = "both", side = "top", anchor = "n", ipady = 5)
    #tkpack.configure(s, fill = "both", side = "top", ipady = 5)
    # Plot and scrollbar
    tkpack(plotleem,  fill = "both", expand = TRUE)
    .addScrollbars(fplot, plotleem)
  }

  tkbind(calculate_button, "<ButtonRelease>", function(...) plotaux1())

  # Activate GUI
  finish <- tclServiceMode(oldmode)
}
##################################
##################################
##################################
##################################
##################################

.demolocsca <- function(...) {
  showpar(gui = "tcltk")
}







