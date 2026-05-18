# Low-level Tcl/Tk GUI function for the leem package.
# Generates the Normal Distribution interface associated with
# P(X <= q) (lower.tail = TRUE) using gui = "tcltk".
.tkplotnormalltt <- function(
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
    anchor = "nw"
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
    anchor = "nw",
    padx = 5,
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
