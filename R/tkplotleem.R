###################################################################
## (name: .tkplotleem+aux+dist)
# OBS.: aux : ltt - lower.tail == TRUE; ltf - lower.tail == FALSE;
#       ltn - low er.tail == NULL; ra - Region A; rb - Regio B
#       dist: "normal", "binomial", ...
###################################################################



# Low-level Tcl/Tk GUI function for the leem package.
# Generates the Normal Distribution interface associated with
# P(X <= q) (lower.tail = TRUE) using gui = "tcltk"
# ===> This is the interface reference! <===
.tkplotleemlttnormal <- function(
    q, mu, sigma, rounding,
    minimo, maximo, dec,
    long.segment, col,
    col2, lty, main,
    text.size, cex.main,
    cex.axis, cex.lab,
    vert.orien.main) {

  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)

  # # Geral configurations
  #.Tcl("option add *Background #e1eff7")

  # clear all
  .Tcl("option clear")

  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))


  # Variables for sliders
  q_var <- tclVar(q)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)
  comma_var <- if (dec == ",") tclVar(TRUE) else tclVar(FALSE)
  segment_var <- tclVar(long.segment)
  title_var <- tclVar(vert.orien.main)
  text.size_var <- tclVar(text.size)




  # =========================
  # Main panels
  # =========================

  # All
  geral <- ttkpanedwindow(base, orient = "horizontal")
  tkpack(geral, expand = TRUE, fill = "both")

  # Left container
  left_container <- ttkpanedwindow(geral, orient = "vertical")

  # Right panel (plot area)
  right_panel <- ttkpanedwindow(geral, orient = "vertical")

  # Pack left container
  # tkpack(
  #   left_container,
  #   side = "left",
  #   fill = "y",
  #   anchor = "n",
  #   padx = 10,
  #   pady = 10
  # )
  tkadd(geral, left_container)


  # Pack right panel
  # tkpack(
  #   right_panel,
  #   side = "right",
  #   fill = "both",
  #   expand = TRUE
  # )
  tkadd(geral, right_panel)

  # =========================
  # Logo frame
  # =========================

  logo_frame <- tkframe(left_container)

  tkpack(
    logo_frame,
    side = "top",
    fill = "x",
    pady = 5
  )

  # =========================
  # Input frame
  # =========================

  left_panel <- ttklabelframe(
    left_container,

    text = gettext(
      "Input(s):",
      domain = "R-leem"
    )
  )

  tkpack(
    left_panel,
    side = "top",
    fill = "x",
    anchor = "nw",
    pady = 5
  )

  # =========================
  # Logo
  # =========================

  # Path to logo inside package
  logo_path <- system.file(
    "etc",
    "logo.png",
    package = "leem"
  )

  # Create original image
  logo_img <- tcl(
    "image",
    "create",
    "photo",
    "-file",
    logo_path
  )

  # Create resized image object
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

  # =========================
  # Plot area
  # =========================

  canvas <- tkcanvas(right_panel)

  tkpack(
    canvas,
    fill = "both",
    expand = TRUE
  )

  # =========================
  # Sliders
  # =========================

  q_slider <- tkscale(
    left_panel,
    from = minimo,
    to = maximo,
    resolution = 0.1,
    orient = "horizontal",
    variable = q_var,

    label = gettext(
      "Quantile",
      domain = "R-leem"
    )
  )



  mu_slider <- tkscale(
    left_panel,
    from = minimo,
    to = maximo,
    resolution = 0.1,
    orient = "horizontal",
    variable = media_var,

    label = gettext(
      "Mean",
      domain = "R-leem"
    )
  )



  sigma_slider <- tkscale(
    left_panel,
    from = 0.1,
    to = 5,
    resolution = 0.1,
    orient = "horizontal",
    variable = sd_var,

    label = gettext(
      "Standard Deviation",
      domain = "R-leem"
    )
  )

  text.size_slider <- tkscale(
    left_panel,
    from = 0.8,
    to = 3,
    resolution = 0.1,
    orient = "horizontal",
    variable = text.size_var,

    label = gettext(
      "Text Size",
      domain = "R-leem"
    )
  )

  # =========================
  # Checkboxes
  # =========================

  cb1 <- tkcheckbutton(
    left_panel,

    text = gettext(
      "Vertical Title Orientation",
      domain = "R-leem"
    ),

    variable = title_var
  )

  cb2 <- tkcheckbutton(
    left_panel,

    text = gettext(
      "Long Segment",
      domain = "R-leem"
    ),

    variable = segment_var
  )

  cb3 <- tkcheckbutton(
    left_panel,

    text = gettext(
      "Comma",
      domain = "R-leem"
    ),

    variable = comma_var
  )



  # =========================
  # Generic export function
  # =========================

  save_plot <- function(type = c("png", "pdf", "svg")) {

    # Match output type
    type <- match.arg(type)

    # File extension
    ext <- switch(
      type,
      png = ".png",
      pdf = ".pdf",
      svg = ".svg"
    )

    # File filter
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

    plotpnormallttplot(

      q = as.numeric(tclvalue(q_var)),

      mu = as.numeric(tclvalue(media_var)),

      sigma = as.numeric(tclvalue(sd_var)),

      rounding = rounding,

      dec = if (tclvalue(comma_var) == "0") "." else ",",

      long.segment = as.logical(
        as.numeric(tclvalue(segment_var))
      ),

      col = col,

      col2 = col2,

      lty = lty,

      main = main,

      text.size = as.numeric(
        tclvalue(text.size_var)
      ),

      cex.main = as.numeric(
        tclvalue(text.size_var)
      ),

      cex.axis = cex.axis,

      cex.lab = cex.lab,

      vert.orien.main = as.logical(
        as.numeric(tclvalue(title_var))
      )
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

  # =========================
  # Export buttons
  # =========================

  png_button <- tkbutton(

    left_panel,

    text = gettext(
      "Export PNG",
      domain = "R-leem"
    ),

    command = function() save_plot("png")
  )

  pdf_button <- tkbutton(

    left_panel,

    text = gettext(
      "Export PDF",
      domain = "R-leem"
    ),

    command = function() save_plot("pdf")
  )

  svg_button <- tkbutton(

    left_panel,

    text = gettext(
      "Export SVG",
      domain = "R-leem"
    ),

    command = function() save_plot("svg")
  )

  # =========================
  # Pack widgets
  # =========================

  tkpack(q_slider, fill = "x")
  tkpack(mu_slider, fill = "x")
  tkpack(sigma_slider, fill = "x")
  tkpack(text.size_slider, fill = "x")

  tkpack(cb1, anchor = "w")
  tkpack(cb2, anchor = "w")
  tkpack(cb3, anchor = "w")

  tkpack(
    png_button,
    fill = "x",
    pady = 2
  )

  tkpack(
    pdf_button,
    fill = "x",
    pady = 2
  )

  tkpack(
    svg_button,
    fill = "x",
    pady = 2
  )

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", right_panel)))
    width <- as.numeric(tclvalue(tkwinfo("width", right_panel)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))
    segment <- as.logical(as.numeric(tclvalue(segment_var)))
    textsize <- as.numeric(tclvalue(text.size_var))
    orienttext <- as.logical(as.numeric(tclvalue(title_var)))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(# Generate the normal distribution plot
      plotpnormallttplot(
        q = quantil, mu = media, sigma = desvpad,
        rounding = rounding,
        dec = if (tclvalue(comma_var) == "0") "." else ",",
        long.segment = segment,
        col = col,
        col2 = col2,
        lty = lty,
        main = main,
        text.size = textsize,
        cex.main = textsize,
        cex.axis = cex.axis,
        cex.lab = cex.lab,
        vert.orien.main = orienttext
      ), silent = TRUE)


    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(q_slider, command = function(...) drawGraph())
  tkconfigure(mu_slider, command = function(...) drawGraph())
  tkconfigure(sigma_slider, command = function(...) drawGraph())
  tkconfigure(text.size_slider, command = function(...) drawGraph())
  tkconfigure(cb1, command = function(...) drawGraph())
  tkconfigure(cb2, command = function(...) drawGraph())
  tkconfigure(cb3, command = function(...) drawGraph())


  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)

  # Window of end
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })


}


.tkplotleemnormal <- function(q, mu, sigma, rounding, minimo,
                              maximo, dec, long.segment, col,
                              col2, lty, main) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)

  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  icon_path <- system.file(
    "etc",
    "leem-icon.png",
    package = "leem"
  )

  iconleem <- tkimage.create(
    "photo",
    file = icon_path
  )

  # Keep reference alive
  attr(base, "iconleem") <- iconleem

  # Set icon
  tcl(
    "wm",
    "iconphoto",
    base,
    iconleem
  )

  # Dimension
  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                          from = minimo,
                            to = maximo,
                          orient = "horizontal",
                          variable = q_var,
                          resolution = 0.1,
                          label = gettext("Quantile", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para media
  slider_media <- tkscale(slider_frame,
                           from = mu, to = mu + 2 * sigma,
                           orient = "horizontal",
                           variable = media_var,
                           resolution = 0.1,
                           label = gettext("Mean", domain="R-leem"),
                           showvalue = TRUE)
  tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

  # Slider para desvio
  slider_sd <- tkscale(slider_frame,
                           from = sigma, to = sigma * 1.8,
                           orient = "horizontal",
                           variable = sd_var,
                           resolution = 0.1,
                           label = gettext("Standard Deviation", domain="R-leem"),
                           showvalue = TRUE)
  tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpnormallttplot(quantil, media, desvpad, rounding, dec, long.segment, col,
                           col2, lty, main), silent = TRUE)

    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_media, command = function(...) drawGraph())
  tkconfigure(slider_sd, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist normal para q de comprimento 1 (lower.tail = F)
.tkplotleemnormal2 <- function(q, mu, sigma, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para media
  slider_media <- tkscale(slider_frame,
                          from = mu, to = mu + 2 * sigma,
                          orient = "horizontal",
                          variable = media_var,
                          resolution = 0.1,
                          label = gettext("Mean", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

  # Slider para desvio
  slider_sd <- tkscale(slider_frame,
                       from = sigma, to = sigma * 1.8,
                       orient = "horizontal",
                       variable = sd_var,
                       resolution = 0.1,
                       label = gettext("Standard Deviation", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpnormalltfplot(quantil, media, desvpad, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_media, command = function(...) drawGraph())
  tkconfigure(slider_sd, command = function(...) drawGraph())




  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist normal para q de comprimento 1 (lower.tail = F)
.tkplotleemltnnormal <- function(q, mu, sigma, rounding, minimo, maximo, dec,
                                 long.segment, col,
                                 lty, main) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para media
  slider_media <- tkscale(slider_frame,
                          from = -abs(q), to = mu + 2 * sigma,
                          orient = "horizontal",
                          variable = media_var,
                          resolution = 0.1,
                          label = gettext("Mean", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

  # Slider para desvio
  slider_sd <- tkscale(slider_frame,
                       from = sigma, to = sigma * 1.8,
                       orient = "horizontal",
                       variable = sd_var,
                       resolution = 0.1,
                       label = gettext("Standard Deviation", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpnormalltnplot(quantil, media, desvpad, rounding, dec,
                           long.segment, col,
                           lty, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_media, command = function(...) drawGraph())
  tkconfigure(slider_sd, command = function(...) drawGraph())




  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist normal para q > 1, regiona
.tkplotleemnormal3 <- function(q1, q2, mu, sigma, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                      from = minimo,
                      to = tclvalue(q2_var),
                      orient = "horizontal",
                      variable = q1_var,
                      resolution = 0.1,
                      label = gettext("q1", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q1
  slider_q2 <- tkscale(slider_frame,
                       from = tclvalue(q1_var),
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para media
  slider_media <- tkscale(slider_frame,
                          from = mu, to = mu + 2 * sigma,
                          orient = "horizontal",
                          variable = media_var,
                          resolution = 0.1,
                          label = gettext("Mean", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

  # Slider para desvio
  slider_sd <- tkscale(slider_frame,
                       from = sigma, to = sigma * 1.8,
                       orient = "horizontal",
                       variable = sd_var,
                       resolution = 0.1,
                       label = gettext("Standard Deviation", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpnormalartcltk(quantil01, quantil02, media, desvpad, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  #tkconfigure(slider_q1, command = function(...) drawGraph())
  #tkconfigure(slider_q2, command = function(...) drawGraph())
  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q2, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q1, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_media, command = function(...) drawGraph())
  tkconfigure(slider_sd, command = function(...) drawGraph())




  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist normal para q > 1, regionb
.tkplotleemnormal4 <- function(q1, q2, mu, sigma, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Normal Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  media_var <- tclVar(mu)
  sd_var <- tclVar(sigma)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = tclvalue(q2_var),
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q1
  slider_q2 <- tkscale(slider_frame,
                       from = tclvalue(q1_var),
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para media
  slider_media <- tkscale(slider_frame,
                          from = mu, to = mu + 2 * sigma,
                          orient = "horizontal",
                          variable = media_var,
                          resolution = 0.1,
                          label = gettext("Mean", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

  # Slider para desvio
  slider_sd <- tkscale(slider_frame,
                       from = sigma, to = sigma * 1.8,
                       orient = "horizontal",
                       variable = sd_var,
                       resolution = 0.1,
                       label = gettext("Standard Deviation", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    media <- as.numeric(tclvalue(media_var))
    desvpad <- as.numeric(tclvalue(sd_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpnormalbrtcltk(quantil01, quantil02, media, desvpad, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  #tkconfigure(slider_q1, command = function(...) drawGraph())
  #tkconfigure(slider_q2, command = function(...) drawGraph())
  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q2, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q1, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_media, command = function(...) drawGraph())
  tkconfigure(slider_sd, command = function(...) drawGraph())




  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}


# Plot tk dist t-student para q de comprimento 1 (lower.tail = T)
.tkplotleemtstudent <- function(q, df, ncp = 0, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Student's t Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                          from = 1, to = 10 * df,
                          orient = "horizontal",
                          variable = df_var,
                          resolution = 0.1,
                          label = gettext("Degree of freedom", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = ncp - 5, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

    # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentlttplot(quantil, glib, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

    # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist t-student para q de comprimento 1 (lower.tail = F)
.tkplotleemtstudent2 <- function(q, df, ncp = 0, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Student's t Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = ncp - 5, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))



    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentltfplot(quantil, glib, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist t-student para q > 1, regiona
.tkplotleemtstudent3 <- function(q1, q2, df, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Student's t Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q1_var,
                      resolution = 0.1,
                      label = gettext("q1", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q2_var,
                      resolution = 0.1,
                      label = gettext("q2", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = ncp - 5, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)



  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    df <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentartcltk(quantil01, quantil02, df, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o slider_df para ter como minimo o valor atual de q1
  tkconfigure(slider_df, command = function(...) drawGraph())
  # Atualiza o slider_ncp
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist t-student para q > 1, regionb
.tkplotleemtstudent4 <- function(q1, q2, df, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Student's t Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = ncp - 5, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)



  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    df <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentbrtcltk(quantil01, quantil02, df, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o slider_df para ter como minimo o valor atual de q1
  tkconfigure(slider_df, command = function(...) drawGraph())
  # Atualiza o slider_ncp
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}


# Plot tk dist chisq para q de comprimento 1 (lower.tail = T)
.tkplotleemchisq <- function(q, df, ncp = 0, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Chi-Squared Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                       from = 0, to = ncp + 2 * df,
                       orient = "horizontal",
                       variable = ncp_var,
                       resolution = 0.1,
                       label = gettext("Noncentrality parameter", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpchisqlttplot(quantil, glib, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist chisq para q de comprimento 1 (lower.tail = F)
.tkplotleemchisq02 <- function(q, df, ncp = 0, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "800x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Chi-Squared Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = maximo,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 2 * df,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpchisqltfplot(quantil, glib, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist chisq para q > 1, regiona
.tkplotleemchisq3 <- function(q1, q2, df, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "800x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Chi-Squared Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    df <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpchisqartcltk(quantil01, quantil02, df, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o slider_df para ter como minimo o valor atual de q1
  tkconfigure(slider_df, command = function(...) drawGraph())
  # Atualiza o slider_ncp para ter como minimo o valor atual de ncp
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist chisq para q > 1, regionb
.tkplotleemchisq4 <- function(q1, q2, df, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "800x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Chi-Squared Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 5,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    df <- as.numeric(tclvalue(df_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpchisqbrtcltk(quantil01, quantil02, df, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })
  # Atualiza o slider_df para ter como minimo o valor atual de q1
  tkconfigure(slider_df, command = function(...) drawGraph())
  # Atualiza o slider_ncp para ter como minimo o valor atual de ncp
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}

# Plot tk dist F para q de comprimento 1 (lower.tail = T)
.tkplotleemf <- function(q, df1, df2, ncp = 0, rounding, main) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: F Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df1_var <- tclVar(df1)
  df2_var <- tclVar(df2)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = 0,
                      to = q + 10,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df1 <- tkscale(slider_frame,
                       from = 1, to = df1  + 2 * df1,
                       orient = "horizontal",
                       variable = df1_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom (numerator)", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df2 <- tkscale(slider_frame,
                       from = 1, to = df2  + 2 * df2,
                       orient = "horizontal",
                       variable = df2_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom (denominator)", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 10,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib1 <- as.numeric(tclvalue(df1_var))
    glib2 <- as.numeric(tclvalue(df2_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpflttplot(quantil, glib1, glib2, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df1, command = function(...) drawGraph())
  tkconfigure(slider_df2, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist f para q de comprimento 1 (lower.tail = F)
.tkplotleemf02 <- function(q, df1, df2, ncp = 0, rounding, main) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: F Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df1_var <- tclVar(df1)
  df2_var <- tclVar(df2)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = 0,
                      to = q + 10,
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 0.1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df1 <- tkscale(slider_frame,
                        from = 1, to = df1  + 2 * df1,
                        orient = "horizontal",
                        variable = df1_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (numerator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df2 <- tkscale(slider_frame,
                        from = 1, to = df2  + 2 * df2,
                        orient = "horizontal",
                        variable = df2_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (denominator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 10,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    glib1 <- as.numeric(tclvalue(df1_var))
    glib2 <- as.numeric(tclvalue(df2_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpfltfplot(quantil, glib1, glib2, pnc, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }

  # Atualiza o grafico quando qualquer slider eh movido
  tkconfigure(slider_q, command = function(...) drawGraph())
  tkconfigure(slider_df1, command = function(...) drawGraph())
  tkconfigure(slider_df2, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist f para q > 1, regiona
.tkplotleemf03 <- function(q1, q2, df1, df2, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: F Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df1_var <- tclVar(df1)
  df2_var <- tclVar(df2)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df1 <- tkscale(slider_frame,
                        from = 1, to = df1  + 2 * df1,
                        orient = "horizontal",
                        variable = df1_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (numerator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df2 <- tkscale(slider_frame,
                        from = 1, to = df2  + 2 * df2,
                        orient = "horizontal",
                        variable = df2_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (denominator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 10,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    glib1 <- as.numeric(tclvalue(df1_var))
    glib2 <- as.numeric(tclvalue(df2_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpfartcltk(quantil01, quantil02, glib1, glib2, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }


  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })

  tkconfigure(slider_df1, command = function(...) drawGraph())
  tkconfigure(slider_df2, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}
# Plot tk dist f para q > 1, regionb
.tkplotleemf04 <- function(q1, q2, df1, df2, ncp = 0, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: F Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df1_var <- tclVar(df1)
  df2_var <- tclVar(df2)
  ncp_var <- tclVar(ncp)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q1
  slider_q1 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q1_var,
                       resolution = 0.1,
                       label = gettext("q1", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para q2
  slider_q2 <- tkscale(slider_frame,
                       from = minimo,
                       to = maximo,
                       orient = "horizontal",
                       variable = q2_var,
                       resolution = 0.1,
                       label = gettext("q2", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_q2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df1 <- tkscale(slider_frame,
                        from = 1, to = df1  + 2 * df1,
                        orient = "horizontal",
                        variable = df1_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (numerator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df1, side = "top", fill = "x", padx=10, pady=2)

  # Slider para df
  slider_df2 <- tkscale(slider_frame,
                        from = 1, to = df2  + 2 * df2,
                        orient = "horizontal",
                        variable = df2_var,
                        resolution = 0.1,
                        label = gettext("Degree of freedom (denominator)", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_df2, side = "top", fill = "x", padx=10, pady=2)

  # Slider para ncp
  slider_ncp <- tkscale(slider_frame,
                        from = 0, to = ncp + 10,
                        orient = "horizontal",
                        variable = ncp_var,
                        resolution = 0.1,
                        label = gettext("Noncentrality parameter", domain="R-leem"),
                        showvalue = TRUE)
  tkpack(slider_ncp, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil01 <- as.numeric(tclvalue(q1_var))
    quantil02 <- as.numeric(tclvalue(q2_var))
    glib1 <- as.numeric(tclvalue(df1_var))
    glib2 <- as.numeric(tclvalue(df2_var))
    pnc <- as.numeric(tclvalue(ncp_var))


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpfbrtcltk(quantil01, quantil02, glib1, glib2, pnc, rounding, main, q), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }


  # Atualiza o slider_q1 para ter como maximo o valor atual de q2
  tkconfigure(slider_q1, command = function(...) {
    novo_q2 <- as.numeric(tclvalue(q2_var))
    tkconfigure(slider_q1, to = novo_q2)
    drawGraph()
  })

  # Atualiza o slider_q2 para ter como minimo o valor atual de q1
  tkconfigure(slider_q2, command = function(...) {
    novo_q1 <- as.numeric(tclvalue(q1_var))
    tkconfigure(slider_q2, from = novo_q1)
    drawGraph()
  })

  tkconfigure(slider_df1, command = function(...) drawGraph())
  tkconfigure(slider_df2, command = function(...) drawGraph())
  tkconfigure(slider_ncp, command = function(...) drawGraph())

  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}

# Plot tk dist binomial para q de comprimento 1 (lower.tail = T)
.tkplotleembinomial <- function(q, size, prob, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)

  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: Binomial Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  size_var <- tclVar(size)
  prob_var <- tclVar(prob)

  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
  slider_frame <- tkframe(base)
  tkpack(slider_frame, side = "bottom", fill = "x")

  # Slider para q
  slider_q <- tkscale(slider_frame,
                      from = minimo,
                      to = tclvalue(size_var),
                      orient = "horizontal",
                      variable = q_var,
                      resolution = 1,
                      label = gettext("Quantile", domain="R-leem"),
                      showvalue = TRUE)
  tkpack(slider_q, side = "top", fill = "x", padx=10, pady=2)

  # Slider para size
  slider_size <- tkscale(slider_frame,
                         from = tclvalue(q_var), to = size + 30,
                         orient = "horizontal",
                         variable = size_var,
                         resolution = 1,
                         label = gettext("Size", domain="R-leem"),
                         showvalue = TRUE)
  tkpack(slider_size, side = "top", fill = "x", padx=10, pady=2)

  # Slider para prob
  slider_prob <- tkscale(slider_frame,
                         from = 0.01, to = 0.99,
                         orient = "horizontal",
                         variable = prob_var,
                         resolution = 0.1,
                         label = gettext("Success", domain="R-leem"),
                         showvalue = TRUE)
  tkpack(slider_prob, side = "top", fill = "x", padx=10, pady=2)

  # Funcao para desenhar o grafico
  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)

    # Dimensoes do canvas
    height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
    width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

    # Pega os valores dos sliders
    quantil <- as.numeric(tclvalue(q_var))
    n <- as.numeric(tclvalue(size_var))
    p <- as.numeric(tclvalue(prob_var))

    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotpbinomiallttplot(quantil, n, p, rounding, main), silent = TRUE)
    dev.off()

    # Cria imagem no Tk
    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Limpa o canvas antes de desenhar
    tkdelete(canvas, "all")
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    options(warn = oldw)
  }


  # Atualiza o slider_q para ter como maximo o valor atual de size
  tkconfigure(slider_size, command = function(...) {
    novo_q <- as.numeric(tclvalue(size_var))
    tkconfigure(slider_q, to = novo_q)
    drawGraph()
  })

  # Slider q
  tkconfigure(slider_q, command = function(...) {
    drawGraph()
  })

  # Slider prob
  tkconfigure(slider_prob, command = function(...) {
    drawGraph()
  })







  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }



  # Atualiza o grafico quando redimensiona
  tkbind(base, '<Configure>', onResize)

  drawGraph()

  # Activate GUI
  finish <- tclServiceMode(oldmode)
  tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
    response <- tk_messageBox(
      title = gettext("Tell me something:", domain = "R-leem"),
      message = gettext("Do you want to close?", domain = "R-leem"),
      icon = "question",
      type = "yesno"
    )
    if (response == "yes") {
      tkdestroy(base)
    }
  })
}


##
.plotcurve <- function(gui) {

  # Parameters
  media <- sample(-50:50, 1)
  stdvar <- sample(5:10, 1)

  # Plot
  plotcurve <- function(mu, sigma = 5) {
    x <- seq(mu - 4 * sigma, mu + 4 * sigma, by = 0.01)
    fx <- dnorm(x, mean = mu, sd = sigma)


    curve(dnorm(x, mean = mu, sd = sigma), mu - 4 * sigma, mu + 4 * sigma ,
          xlim = c(-70, 70), ylim = c(0, 0.1), ylab = expression(f[X](x)), xlab="X",
          panel.first = grid(col = "gray"),
          main = gettext("Normal distribution", domain = "R-leem"))

    polygon(c(x, rev(x)),
            c(fx, rep(0, length(fx))),
            col="red")
    abline(v=mu, lty=2)
    legend("topleft", bty="n", fill="red",
           legend=substitute(mu == media~","~sigma == vari, list(media = mu, vari = sigma)))
  }

  # Tcl/TK
  if (gui == "tcltk") {
    # Disabled GUI (Type I)
    oldmode <- tclServiceMode(FALSE)


    # Main Window
    base <- tktoplevel(padx=10, pady=10)

    tkwm.geometry(base, "600x700")

    # Title
    tkwm.title(base,
               gettext("leem package: Demonstration - Normal Distribution", domain = "R-leem"))

    # Variables for sliders
    media_var <- tclVar(media)
    sd_var <- tclVar(stdvar)

    # Main Frame
    main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
    canvas <- tkcanvas(main_frame)
    tkpack(main_frame, canvas, fill = "both", expand=TRUE)

    # Slider Frame
    tkpack(tklabel(base, text = gettext("Input(s):", domain="R-leem")))
    slider_frame <- tkframe(base)
    tkpack(slider_frame, side = "bottom", fill = "x")

    # Slider para media
    slider_media <- tkscale(slider_frame,
                            from = -50, to = 50,
                            orient = "horizontal",
                            variable = media_var,
                            resolution = 0.1,
                            label = gettext("Mean", domain="R-leem"),
                            showvalue = TRUE)
    tkpack(slider_media, side = "top", fill = "x", padx=10, pady=2)

    # Slider para desvio
    slider_sd <- tkscale(slider_frame,
                         from = 5, to = 10,
                         orient = "horizontal",
                         variable = sd_var,
                         resolution = 0.1,
                         label = gettext("Standard Deviation", domain="R-leem"),
                         showvalue = TRUE)
    tkpack(slider_sd, side = "top", fill = "x", padx=10, pady=2)

    # Funcao para desenhar o grafico
    drawGraph <- function() {
      oldw <- getOption("warn")
      options(warn = -1)

      # Dimensoes do canvas
      height <- as.numeric(tclvalue(tkwinfo("height", main_frame)))
      width <- as.numeric(tclvalue(tkwinfo("width", main_frame)))

      # Pega os valores dos sliders
      media <- as.numeric(tclvalue(media_var))
      desvpad <- as.numeric(tclvalue(sd_var))

      # Arquivo temporario
      fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

      # Cria a Imagem do Grafico
      png(filename = fp2, width = width, height = height, units = "px")
      try(plotcurve(mu = media, sigma = desvpad), silent = TRUE)
      dev.off()

      # Cria imagem no Tk
      tkimage.create("photo", "::image::imgteste2", file = fp2)

      # Limpa o canvas antes de desenhar
      tkdelete(canvas, "all")
      tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

      options(warn = oldw)
    }

    # Atualiza o grafico quando qualquer slider eh movido
    tkconfigure(slider_media, command = function(...) drawGraph())
    tkconfigure(slider_sd, command = function(...) drawGraph())




    # CALLBACKS
    onResize <- function() {
      drawGraph()
    }



    # Atualiza o grafico quando redimensiona
    tkbind(base, '<Configure>', onResize)

    drawGraph()

    # Activate GUI
    finish <- tclServiceMode(oldmode)
    tkwm.protocol(base, "WM_DELETE_WINDOW", function() {
      response <- tk_messageBox(
        title = gettext("Tell me something:", domain = "R-leem"),
        message = gettext("Do you want to close?", domain = "R-leem"),
        icon = "question",
        type = "yesno"
      )
      if (response == "yes") {
        tkdestroy(base)
      }
    })

  }

}



