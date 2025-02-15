# Plot tk dist normal para q de comprimento 1 (lower.tail = T)
.tkplotleemnormal <- function(q, mu, sigma, rounding, main, minimo, maximo) {
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
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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
    try(plotpnormallttplot(quantil, media, desvpad, rounding, main), silent = TRUE)
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
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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



