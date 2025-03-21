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
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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
.tkplotleemtstudent <- function(q, df, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: t-Student Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)


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

  # Slider para df
  slider_df <- tkscale(slider_frame,
                          from = 1, to = 10 * df,
                          orient = "horizontal",
                          variable = df_var,
                          resolution = 0.1,
                          label = gettext("Degree of freedom", domain="R-leem"),
                          showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

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


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentlttplot(quantil, glib, rounding, main), silent = TRUE)
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
.tkplotleemtstudent2 <- function(q, df, rounding, main, minimo, maximo) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: t-Student Distribution", domain = "R-leem"))

  # Variables for sliders
  q_var <- tclVar(q)
  df_var <- tclVar(df)


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

  # Slider para df
  slider_df <- tkscale(slider_frame,
                       from = 1, to = 10 * df,
                       orient = "horizontal",
                       variable = df_var,
                       resolution = 0.1,
                       label = gettext("Degree of freedom", domain="R-leem"),
                       showvalue = TRUE)
  tkpack(slider_df, side = "top", fill = "x", padx=10, pady=2)

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


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentltfplot(quantil, glib, rounding, main), silent = TRUE)
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
.tkplotleemtstudent3 <- function(q1, q2, df, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: t-Student Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentartcltk(quantil01, quantil02, df, rounding, main, q), silent = TRUE)
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

.tkplotleemtstudent4 <- function(q1, q2, df, rounding, main, minimo, maximo, q) {
  # Disabled GUI (Type I)
  oldmode <- tclServiceMode(FALSE)


  # Main Window
  base <- tktoplevel(padx=10, pady=10)

  tkwm.geometry(base, "600x700")

  # Title
  tkwm.title(base,
             gettext("leem package: t-Student Distribution", domain = "R-leem"))

  # Variables for sliders
  q1_var <- tclVar(q1)
  q2_var <- tclVar(q2)
  df_var <- tclVar(df)


  # Main Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)
  canvas <- tkcanvas(main_frame)
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)

  # Slider Frame
  tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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


    # Arquivo temporario
    fp2 <- tempfile(pattern = "leem.", tmpdir = tempdir(), fileext = ".png")

    # Cria a Imagem do Grafico
    png(filename = fp2, width = width, height = height, units = "px")
    try(plotptstudentbrtcltk(quantil01, quantil02, df, rounding, main, q), silent = TRUE)
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
    tkpack(tklabel(base, text = gettext("Parameters:", domain="R-leem")))
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



