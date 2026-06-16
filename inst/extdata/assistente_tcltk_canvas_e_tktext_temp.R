.assistente_tcltk_leem <- function() {
  # Tk ctext package
  tclRequire("ctext")

  # Verifica se tcltk esta disponivel
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    cat("Package tcltk is required for the graphical interface.\n")
    cat("Falling back to console version...\n")
    return(.assistente_leem_console())
  }
  # Configuracao de cores
  hcolors <- list(normal = "black",
                  background = "white",
                  functions = "purple",
                  rcomments = "darkgreen",
                  operators = "blue",
                  brackets = "darkblue",
                  digits = "orange",
                  characters = "gray",
                  latexmacros = "darkred",
                  latexequations = "blue",
                  latexcomments = "red",
                  rnwchunks = "blue",
                  rtexchunks = "blue",
                  rmd = "darkred",
                  rmdchunks = "blue",
                  xml = "darkred",
                  xmlcomments = "red",
                  roxygentext = "black",
                  roxygenchunks = "blue",
                  brewcomments = "red",
                  brewchunks = "blue",
                  brewtemplate = "black",
                  restchunks = "blue"
  )

  # ============================================================
  # VARIAVEL GLOBAL PARA ARMAZENAR WIDGETS DE TEXTO
  # ============================================================
  all_text_widgets <- list()

  # Funcao para atualizar wraplength de todos os labels existentes
  atualizar_todos_wraplength <- function() {
    canvas_width <- as.numeric(tkwinfo("width", canvas))
    if (is.na(canvas_width) || canvas_width == 0) return()

    novo_wraplength <- max(100, canvas_width - 100)

    # Atualiza widgets armazenados
    if (length(all_text_widgets) > 0) {
      for (widget in all_text_widgets) {
        tryCatch({
          tkconfigure(widget, wraplength = novo_wraplength)
        }, error = function(e) {})
      }
    }

    # Metodo alternativo: percorre todos os frames
    tryCatch({
      msg_frames <- as.character(tkwinfo("children", conversation_frame))
      for (frame_id in msg_frames) {
        filhos <- as.character(tkwinfo("children", frame_id))
        for (filho in filhos) {
          classe <- tryCatch(tclvalue(tkwinfo("class", filho)), error = function(e) "")
          if (classe == "Label") {
            texto <- tryCatch(tclvalue(tkget(filho, "-text")), error = function(e) "")
            if (!is.null(texto) && nchar(texto) > 20) {
              tkconfigure(filho, wraplength = novo_wraplength)
            }
          }
        }
      }
    }, error = function(e) {})
  }

  # ============================================================
  # FUNCAO DE QUEBRA DE TEXTO - AJUSTADA AO TAMANHO DA FONTE
  # ============================================================

  # Largura maxima para quebra de texto (valor padrao)
  max_text_width <- 80

  # Variavel que sera definida depois
  current_font_size <- 10

  # Canvas sera definido depois, entao criar referencia
  canvas_ref <- NULL

  # Funcao para calcular largura maxima baseada no tamanho da fonte
  calcular_max_width <- function(font_size) {
    # Se canvas ainda nao existe, retorna valor baseado no tamanho da fonte
    if (is.null(canvas_ref) || !exists("canvas_ref")) {
      return(floor(80 * (10 / font_size)))
    }

    canvas_width <- tryCatch({
      as.numeric(tkwinfo("width", canvas_ref))
    }, error = function(e) NA)

    if (is.na(canvas_width) || canvas_width == 0) {
      return(floor(80 * (10 / font_size)))
    }

    char_width <- font_size * 0.6
    max_chars <- floor(canvas_width / char_width) - 5
    return(max(30, min(120, max_chars)))
  }

  # Funcao de quebra de texto simples (atualizada)
  wrap_text_simple <- function(text, font_size) {
    # Calcula a largura maxima baseada no tamanho da fonte atual
    max_width <- calcular_max_width(font_size)

    # Se o texto for curto, retorna ele mesmo
    if (nchar(text) <= max_width * 1.5) return(text)

    # Quebra em palavras e reconstroi
    words <- strsplit(text, " ")[[1]]
    lines <- character()
    current_line <- ""

    for (word in words) {
      if (nchar(word) > max_width) {
        if (nchar(current_line) > 0) {
          lines <- c(lines, current_line)
          current_line <- ""
        }
        for (i in seq(1, nchar(word), by = max_width - 5)) {
          lines <- c(lines, substr(word, i, min(i + max_width - 6, nchar(word))))
        }
      } else if (nchar(paste(current_line, word)) <= max_width) {
        if (nchar(current_line) == 0) {
          current_line <- word
        } else {
          current_line <- paste(current_line, word)
        }
      } else {
        lines <- c(lines, current_line)
        current_line <- word
      }
    }
    if (nchar(current_line) > 0) lines <- c(lines, current_line)

    paste(lines, collapse = "\n")
  }

  # Funcao para atualizar a largura baseada no canvas e fonte
  update_text_width <- function() {
    if (is.null(canvas_ref)) return()

    canvas_width <- tryCatch({
      as.numeric(tkwinfo("width", canvas_ref))
    }, error = function(e) NA)

    if (!is.na(canvas_width) && canvas_width > 0) {
      char_width <- current_font_size * 0.6
      max_chars <- floor(canvas_width / char_width) - 5
      max_text_width <<- max(30, min(120, max_chars))
    } else {
      max_text_width <<- floor(80 * (10 / current_font_size))
    }
  }
  # ============================================================

  # Funcao para remover acentos - USANDO ESCAPES UNICODE
  remover_acentos <- function(texto) {
    texto_sem_acento <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
    if (is.na(texto_sem_acento)) {
      # Mapeamento de acentos usando escapes Unicode
      acentos <- list(
        "\u00E1" = "a", "\u00E0" = "a", "\u00E3" = "a", "\u00E2" = "a", "\u00E4" = "a",
        "\u00C1" = "a", "\u00C0" = "a", "\u00C3" = "a", "\u00C2" = "a", "\u00C4" = "a",
        "\u00E9" = "e", "\u00E8" = "e", "\u00EA" = "e", "\u00EB" = "e",
        "\u00C9" = "e", "\u00C8" = "e", "\u00CA" = "e", "\u00CB" = "e",
        "\u00ED" = "i", "\u00EC" = "i", "\u00EE" = "i", "\u00EF" = "i",
        "\u00CD" = "i", "\u00CC" = "i", "\u00CE" = "i", "\u00CF" = "i",
        "\u00F3" = "o", "\u00F2" = "o", "\u00F5" = "o", "\u00F4" = "o", "\u00F6" = "o",
        "\u00D3" = "o", "\u00D2" = "o", "\u00D5" = "o", "\u00D4" = "o", "\u00D6" = "o",
        "\u00FA" = "u", "\u00F9" = "u", "\u00FB" = "u", "\u00FC" = "u",
        "\u00DA" = "u", "\u00D9" = "u", "\u00DB" = "u", "\u00DC" = "u",
        "\u00E7" = "c", "\u00C7" = "c",
        "\u00F1" = "n", "\u00D1" = "n"
      )
      for (acento in names(acentos)) {
        texto <- gsub(acento, acentos[[acento]], texto, fixed = TRUE)
      }
      texto_sem_acento <- texto
    }
    return(tolower(texto_sem_acento))
  }

  # ============================================================
  # ESTRUTURA HIERARQUICA DE CONHECIMENTO - BILINGUE
  # ============================================================

  leem_knowledge <- list(
    new_leem = list(
      keywords = c("new_leem", "criar objeto", "objeto leem", "new leem", "create object", "leem object"),
      categories = list(
        intro = list(
          trigger_pt = c("o que e", "entender", "finalidade", "para que serve", "introducao"),
          trigger_en = c("what is", "understand", "purpose", "what does", "introduction"),
          response_pt = "A funcao new_leem() cria um objeto da classe 'leem', que e a base para todas as analises do pacote. Este objeto armazena os dados e metadados necessarios para calculos estatisticos.",
          response_en = "The new_leem() function creates an object of class 'leem', which is the foundation for all analyses in the package.",
          code_example = NULL
        ),
        args = list(
          trigger_pt = c("argumentos", "parametros", "argument"),
          trigger_en = c("arguments", "parameters", "params"),
          response_pt = "Argumentos de new_leem():\n\n- x: vetor numerico com os dados\n- variable: tipo da variavel - 'continuous' (continua) ou 'discrete' (discreta)",
          response_en = "Arguments of new_leem():\n\n- x: numeric vector with data\n- variable: variable type - 'continuous' or 'discrete'",
          code_example = NULL
        ),
        example = list(
          trigger_pt = c("exemplo", "criar", "como usar", "exemplo basico"),
          trigger_en = c("example", "create", "how to use", "basic example"),
          response_pt = "Exemplo basico:\n\n# Dados\ndados <- c(10, 12, 15, 18, 20)\n\n# Criar objeto\nobj <- new_leem(dados, variable = 'continuous')\n\n# Visualizar\nprint(obj)",
          response_en = "Basic example:\n\n# Data\ndata <- c(10, 12, 15, 18, 20)\n\n# Create object\nobj <- new_leem(data, variable = 'continuous')\n\n# View\nprint(obj)",
          code_example = '
# Create sample data
dados <- c(10, 12, 15, 18, 20)
cat("Data:", dados, "\n")

# Create leem object
objeto <- new_leem(dados, variable = "continuous")
cat("Leem object created successfully!\n")
print(objeto)
'
        ),
        pipe = list(
          trigger_pt = c("pipe", "%>%", "|>", "encadeamento", "operador pipe"),
          trigger_en = c("pipe", "%>%", "|>", "chaining"),
          response_pt = "Uso com pipe (%>% do magrittr ou |> do R base):\n\n# Com pipe\ndados <- c(10, 12, 15, 18, 20)\ndados |> new_leem(variable = \"continuous\") |> tabfreq()",
          response_en = "Using pipe (%>% from magrittr or |> from base R):\n\n# With pipe\ndata <- c(10, 12, 15, 18, 20)\ndata |> new_leem(variable = \"continuous\") |> tabfreq()",
          code_example = '
# Using pipe with leem
library(magrittr)
dados <- c(10, 12, 15, 18, 20)
resultado <- dados |>
  new_leem(variable = "continuous") |>
  tabfreq()
print(resultado)
'
        )
      )
    ),

    tabfreq = list(
      keywords = c("tabfreq", "tabela", "frequencia", "tabela de frequencia", "table", "frequency"),
      categories = list(
        intro = list(
          trigger_pt = c("o que e", "entender", "finalidade", "para que serve"),
          trigger_en = c("what is", "understand", "purpose", "what does"),
          response_pt = "A funcao tabfreq() gera tabelas de frequencia para objetos da classe leem.",
          response_en = "The tabfreq() function generates frequency tables for leem class objects.",
          code_example = NULL
        ),
        args = list(
          trigger_pt = c("argumentos", "parametros"),
          trigger_en = c("arguments", "parameters"),
          response_pt = "Argumentos de tabfreq():\n\n- x: objeto da classe leem\n- rounding: numero de casas decimais (padrao = 2)",
          response_en = "Arguments of tabfreq():\n\n- x: leem class object\n- rounding: number of decimal places (default = 2)",
          code_example = NULL
        ),
        example = list(
          trigger_pt = c("exemplo", "como usar", "exemplo basico"),
          trigger_en = c("example", "how to use", "basic example"),
          response_pt = "Exemplo:\n\n# Criar objeto\nobj <- new_leem(dados, variable = 'continuous')\n\n# Tabela de frequencia\ntabfreq(obj, rounding = 2)",
          response_en = "Example:\n\n# Create object\nobj <- new_leem(data, variable = 'continuous')\n\n# Frequency table\ntabfreq(obj, rounding = 2)",
          code_example = '
# Create sample data
dados <- c(10, 12, 15, 18, 18, 20, 20, 20, 20)
objeto <- new_leem(dados, variable = "discrete")

# Create frequency table
cat("Frequency table:\n")
print(tabfreq(objeto, rounding = 2))
'
        )
      )
    ),

    mean = list(
      keywords = c("mean", "media", "calcular media", "calculate mean", "average"),
      categories = list(
        intro = list(
          trigger_pt = c("o que e", "entender", "finalidade", "para que serve"),
          trigger_en = c("what is", "understand", "purpose", "what does"),
          response_pt = "O metodo mean.leem() calcula a media aritmetica para objetos da classe leem.",
          response_en = "The mean.leem() method calculates the arithmetic mean for leem class objects.",
          code_example = NULL
        ),
        args = list(
          trigger_pt = c("argumentos", "parametros"),
          trigger_en = c("arguments", "parameters"),
          response_pt = "Parametros de mean.leem():\n\n- grouped: TRUE (usa dados agrupados) ou FALSE (usa dados brutos)\n- details: TRUE (mostra detalhes do calculo)\n- rounding: casas decimais",
          response_en = "Parameters of mean.leem():\n\n- grouped: TRUE (uses grouped data) or FALSE (uses raw data)\n- details: TRUE (shows calculation details)\n- rounding: decimal places",
          code_example = NULL
        ),
        example = list(
          trigger_pt = c("exemplo", "como usar", "calcular"),
          trigger_en = c("example", "how to use", "calculate"),
          response_pt = "Exemplo:\n\n# Media com dados agrupados\nmean(objeto, grouped = TRUE, details = TRUE)",
          response_en = "Example:\n\n# Mean with grouped data\nmean(object, grouped = TRUE, details = TRUE)",
          code_example = '
# Create sample data
dados <- c(10, 12, 15, 18, 20)
objeto <- new_leem(dados, variable = "continuous")

# Calculate mean
cat("Calculating mean...\n")
resultado <- mean(objeto, grouped = TRUE, details = TRUE)
cat("Mean:", resultado, "\n")
'
        )
      )
    ),

    median = list(
      keywords = c("median", "mediana", "calcular mediana", "calculate median"),
      categories = list(
        intro = list(
          trigger_pt = c("o que e", "entender", "finalidade"),
          trigger_en = c("what is", "understand", "purpose"),
          response_pt = "O metodo median.leem() calcula a mediana para objetos da classe leem.",
          response_en = "The median.leem() method calculates the median for leem class objects.",
          code_example = NULL
        ),
        example = list(
          trigger_pt = c("exemplo", "como usar", "calcular"),
          trigger_en = c("example", "how to use", "calculate"),
          response_pt = "Exemplo:\n\nmedian(objeto)",
          response_en = "Example:\n\nmedian(object)",
          code_example = '
# Create sample data
dados <- c(10, 12, 15, 18, 20)
objeto <- new_leem(dados, variable = "continuous")

# Calculate median
cat("Calculating median...\n")
resultado <- median(objeto)
cat("Median:", resultado, "\n")
'
        )
      )
    ),

    plot = list(
      keywords = c("plot", "grafico", "histograma", "plotar", "graph", "histogram"),
      categories = list(
        intro = list(
          trigger_pt = c("o que e", "entender", "para que serve"),
          trigger_en = c("what is", "understand", "purpose"),
          response_pt = "O metodo plot.leem() gera graficos a partir de objetos leem, incluindo histogramas e poligonos de frequencia.",
          response_en = "The plot.leem() method generates plots from leem objects, including histograms and frequency polygons.",
          code_example = NULL
        ),
        args = list(
          trigger_pt = c("argumentos", "parametros", "tipo", "type"),
          trigger_en = c("arguments", "parameters", "type"),
          response_pt = "Tipos de grafico:\n\n- type = 'hist': histograma\n- type = 'freqpoly': poligono de frequencia\n\nOutros parametros: col (cor), main (titulo)",
          response_en = "Plot types:\n\n- type = 'hist': histogram\n- type = 'freqpoly': frequency polygon\n\nOther parameters: col (color), main (title)",
          code_example = NULL
        ),
        example = list(
          trigger_pt = c("exemplo", "como usar", "criar grafico"),
          trigger_en = c("example", "how to use", "create plot"),
          response_pt = "Exemplo:\n\nplot(objeto, type = 'hist', col = 'blue', main = 'Histograma')",
          response_en = "Example:\n\nplot(object, type = 'hist', col = 'blue', main = 'Histogram')",
          code_example = '
# Create sample data
set.seed(123)
dados <- rnorm(100, mean = 50, sd = 10)
objeto <- new_leem(dados, variable = "continuous")

# Create histogram
cat("Creating histogram...\n")
plot(objeto, type = "hist", col = "lightblue", main = "Sample Histogram")
'
        )
      )
    )
  )

  # Traducoes das respostas
  respostas <- list(
    pt = list(
      user_label = "Voce: ",
      assistant_label = "Assistente: ",
      welcome = "Bem-vindo ao Assistente LEEM! Pergunte sobre o pacote leem.\n\nVoce pode perguntar sobre:\n- new_leem (criar objeto)\n- tabfreq (tabela de frequencia)\n- mean (media)\n- median (mediana)\n- plot (graficos)\n\nDigite 'ajuda' para mais opcoes.",
      goodbye = "Ate logo! Continue aprendendo o pacote leem!",
      language_changed = "Idioma alterado para Portugues! Agora responderei em portugues.",
      run_button_text = "Executar este exemplo",
      copy_button_text = "Copiar",
      not_understood = "Desculpe, nao entendi sua pergunta.\n\nTente perguntar sobre:\n- new_leem - como criar objetos\n- tabfreq - tabelas de frequencia\n- mean - calculo da media\n- median - calculo da mediana\n- plot - graficos\n\nDigite 'ajuda' para ver todos os comandos disponiveis"
    ),
    en = list(
      user_label = "You: ",
      assistant_label = "Assistant: ",
      welcome = "Welcome to LEEM Assistant! Ask questions about the leem package.\n\nYou can ask about:\n- new_leem (create object)\n- tabfreq (frequency table)\n- mean (average)\n- median\n- plot (graphs)\n\nType 'help' for more options.",
      goodbye = "Goodbye! Keep learning the leem package!",
      language_changed = "Language changed to English! Now I will respond in English.",
      run_button_text = "Run this example",
      copy_button_text = "Copy",
      not_understood = "Sorry, I didn't understand your question.\n\nTry asking about:\n- new_leem - how to create objects\n- tabfreq - frequency tables\n- mean - calculate mean\n- median - calculate median\n- plot - create graphs\n\nType 'help' to see available commands"
    )
  )

  # Funcao auxiliar para encontrar a melhor correspondencia
  find_best_match <- function(pergunta, idioma) {
    pergunta_clean <- remover_acentos(pergunta)

    # Remove pontuacao da pergunta
    pergunta_clean <- gsub("[?!\u00BF\u00A1.,;:]", "", pergunta_clean)

    # Define quais triggers usar baseado no idioma
    trigger_suffix <- ifelse(idioma == "pt", "_pt", "_en")

    for (func_name in names(leem_knowledge)) {
      func <- leem_knowledge[[func_name]]

      # Verifica se a pergunta menciona a funcao ou suas keywords
      if (any(sapply(func$keywords, function(k) grepl(k, pergunta_clean)))) {

        # Agora encontrar qual categoria dentro da funcao
        for (cat_name in names(func$categories)) {
          cat_info <- func$categories[[cat_name]]

          # Obtem o trigger apropriado baseado no idioma
          trigger_field <- paste0("trigger", trigger_suffix)
          triggers <- cat_info[[trigger_field]]

          if (!is.null(triggers)) {
            # Usa fixed = TRUE para evitar problemas com |>
            if (any(sapply(triggers, function(t) grepl(t, pergunta_clean, fixed = TRUE)))) {
              # Obtem a resposta apropriada baseada no idioma
              response_field <- paste0("response", trigger_suffix)
              response <- cat_info[[response_field]]

              return(list(
                found = TRUE,
                function_name = func_name,
                category = cat_name,
                response = response,
                code = cat_info$code_example
              ))
            }
          }
        }

        # Se encontrou a funcao mas nenhuma categoria especifica, retorna introducao
        intro_cat <- func$categories[["intro"]]
        if (!is.null(intro_cat)) {
          response_field <- paste0("response", trigger_suffix)
          response <- intro_cat[[response_field]]

          return(list(
            found = TRUE,
            function_name = func_name,
            category = "intro",
            response = response,
            code = intro_cat$code_example
          ))
        }
      }
    }

    return(list(found = FALSE))
  }

  # Cria janela principal
  main_window <- tktoplevel()
  tkwm.title(main_window, "LEEM Assistant - Interactive Help")
  tkwm.geometry(main_window, "950x700")

  # Frame superior para controles de idioma e fonte
  top_frame <- tkframe(main_window)
  tkpack(top_frame, side = "top", fill = "x", padx = 10, pady = 5)

  # Frame de selecao de idioma
  lang_frame <- tkframe(top_frame)
  tkpack(lang_frame, side = "left", fill = "x", expand = TRUE, padx = 5)

  tklabel(lang_frame, text = "Select Language / Selecione o Idioma:", font = tkfont.create(weight = "bold")) |>
    tkpack(side = "left", padx = 5)

  idioma_var <- tclVar("pt")

  on_language_change <- function() {
    idioma <- tclvalue(idioma_var)
    add_message(respostas[[idioma]]$language_changed, is_user = FALSE)
  }

  tkradiobutton(lang_frame, text = "English", variable = idioma_var, value = "en",
                command = on_language_change) |>
    tkpack(side = "left", padx = 5)

  tkradiobutton(lang_frame, text = "Portugues", variable = idioma_var, value = "pt",
                command = on_language_change) |>
    tkpack(side = "left", padx = 5)

  # Frame de controle de fonte
  font_frame <- tkframe(top_frame)
  tkpack(font_frame, side = "right", padx = 5)

  tklabel(font_frame, text = gettext("Font Size:", domain = "R-leem"),
          font = tkfont.create(weight = "bold")) |>
    tkpack(side = "left", padx = 5)

  font_size_var <- tclVar(10)
  current_font_size <- 10

  # Cria canvas e scrollbar para area de conversa
  canvas_frame <- tkframe(main_window)
  tkpack(canvas_frame, side = "top", fill = "both", expand = TRUE, padx = 10, pady = 5)

  canvas <- tkcanvas(canvas_frame, bg = "white", highlightthickness = 0)
  canvas_ref <- canvas  # <--- IMPORTANTE: atribui a referencia

  scrollbar <- tkscrollbar(canvas_frame, command = function(...) tkyview(canvas, ...))
  tkconfigure(canvas, yscrollcommand = function(...) tkset(scrollbar, ...))

  tkpack(canvas, side = "left", fill = "both", expand = TRUE)
  tkpack(scrollbar, side = "right", fill = "y")


  # Frame dentro do canvas para manter os itens da conversa
  conversation_frame <- tkframe(canvas, bg = "white")
  canvas_window <- tkcreate(canvas, "window", 0, 0, window = conversation_frame, anchor = "nw")


  # ============================================================
  # SCROLL COM A RODA DO MOUSE - SIMPLES E EFICAZ
  # ============================================================

  # Scroll suave
  do_scroll <- function(delta) {
    if (delta > 0) {
      tkyview(canvas, "scroll", -2, "units")
    } else {
      tkyview(canvas, "scroll", 2, "units")
    }
  }

  # Binds no canvas
  tkbind(main_window, "<Button-4>", function(event) tkyview(canvas, "scroll", -2, "units"))
  tkbind(main_window, "<Button-5>", function(event) tkyview(canvas, "scroll", 2, "units"))
  tkbind(main_window, "<MouseWheel>", function(event) {
    delta <- as.numeric(event$delta)
    if (delta > 0) tkyview(canvas, "scroll", -2, "units") else tkyview(canvas, "scroll", 2, "units")
  })

  # Binds no conversation_frame (captura eventos quando mouse estiver sobre as mensagens)
  tkbind(conversation_frame, "<Button-4>", function(event) tkyview(canvas, "scroll", -2, "units"))
  tkbind(conversation_frame, "<Button-5>", function(event) tkyview(canvas, "scroll", 2, "units"))
  tkbind(conversation_frame, "<MouseWheel>", function(event) {
    delta <- as.numeric(event$delta)
    if (delta > 0) tkyview(canvas, "scroll", -2, "units") else tkyview(canvas, "scroll", 2, "units")
  })

  # Foco automatico
  tkbind(canvas, "<Enter>", function() tkfocus(canvas))
  tkbind(conversation_frame, "<Enter>", function() tkfocus(canvas))

  # ============================================================


  # Configura scroll do canvas
  configure_canvas <- function() {
    req_width <- as.numeric(tkwinfo("reqwidth", conversation_frame))
    req_height <- as.numeric(tkwinfo("reqheight", conversation_frame))
    tkconfigure(canvas, scrollregion = paste("0 0", req_width, req_height))
  }

  tkbind(conversation_frame, "<Configure>", function() configure_canvas())
  tkbind(canvas, "<Configure>", function() {
    canvas_width <- as.numeric(tkwinfo("width", canvas))
    tkitemconfigure(canvas, canvas_window, width = canvas_width - 10)
    update_text_width()  # Atualiza a largura do texto quando canvas redimensiona

    # ATUALIZA O WRAPLENGTH DE TODAS AS MENSAGENS EXISTENTES
    atualizar_todos_wraplength()
  })

  # Funcao para rolar para o final
  scroll_to_bottom <- function() {
    req_height <- as.numeric(tkwinfo("reqheight", conversation_frame))
    canvas_height <- as.numeric(tkwinfo("height", canvas))
    if (req_height > canvas_height) {
      tkyview(canvas, "moveto", 1.0)
    }
    tcl("update", "idletasks")
  }

  # Funcao para adicionar bloco de codigo
  add_code_block <- function(code, is_user = FALSE) {
    code_frame <- tkframe(conversation_frame, bg = "#f5f5f5", relief = "sunken", bd = 1)

    num_lines <- length(strsplit(code, "\n")[[1]])
    height <- min(15, max(8, num_lines + 2))
    background <- tclvalue(.Tcl("ttk::style lookup TEntry -fieldbackground"))
    code_text <- tkwidget(code_frame, "ctext", height = height, width = 90, wrap = "word",
                          bg = "#f5f5f5", fg = "black",
                          font = tkfont.create(family = "TkFixedFont", size = current_font_size))
    tkconfigure(code_text, relief = "flat")
    tkinsert(code_text, "1.0", code)

    # Highlight simples para comentarios
    start_pos <- "1.0"
    while(TRUE) {
      match_pos <- tclvalue(tksearch(code_text, "--", "#", start_pos, "end"))
      if (match_pos == "") break
      line_end <- tclvalue(tkindex(code_text, paste(match_pos, "lineend")))
      tktag.add(code_text, "comment", match_pos, line_end)
      start_pos <- line_end
      if (start_pos == "end") break
    }

    configure_r_highlight(code_text, hcolors, highlight = "r")
    tkconfigure(code_text, state = "normal")

    code_scroll <- tkscrollbar(code_frame, command = function(...) tkyview(code_text, ...))
    tkconfigure(code_text, yscrollcommand = function(...) tkset(code_scroll, ...))

    tkpack(code_text, side = "left", fill = "both", expand = TRUE)
    tkpack(code_scroll, side = "right", fill = "y")
    tkpack(code_frame, side = "top", anchor = "nw", fill = "both", expand = TRUE, pady = 5, padx = 20)

    return(list(frame = code_frame, text_widget = code_text))
  }

  # Funcao para adicionar mensagem a conversa (COM WRAPLENGTH DINAMICO)
  add_message <- function(text, is_user = TRUE) {
    idioma <- tclvalue(idioma_var)

    # Cria frame para esta mensagem
    msg_frame <- tkframe(conversation_frame, bg = "white")

    # Adiciona label
    if (is_user) {
      label <- respostas[[idioma]]$user_label
      label_color <- "blue"
    } else {
      label <- respostas[[idioma]]$assistant_label
      label_color <- "green"
    }

    lbl <- tklabel(msg_frame, text = label, font = tkfont.create(weight = "bold"),
                   foreground = label_color, bg = "white")
    tkpack(lbl, side = "left", anchor = "nw")

    # Calcula wraplength inicial baseado na largura do canvas
    canvas_width <- as.numeric(tkwinfo("width", canvas))
    if (is.na(canvas_width) || canvas_width == 0) {
      wraplength <- 400  # valor padrao
    } else {
      wraplength <- canvas_width - 100  # margem
    }

    # Adiciona texto com wraplength (sem quebra manual!)
    txt <- tklabel(msg_frame, text = text, justify = "left", bg = "white",
                   font = tkfont.create(family = "Courier", size = current_font_size),
                   wraplength = wraplength)  # <-- USANDO WRAPLENGTH
    tkpack(txt, side = "left", anchor = "nw", padx = 5)

    tkpack(msg_frame, side = "top", anchor = "nw", fill = "x", pady = 5)

    # Armazena o widget txt em uma lista global para atualizacao futura
    # Isso permite atualizar o wraplength quando a janela for redimensionada
    if (!exists("all_text_widgets")) {
      all_text_widgets <<- list()
    }
    all_text_widgets <<- append(all_text_widgets, txt)

    configure_canvas()

    tcl("after", 10, function() {
      scroll_to_bottom()
      tcl("update", "idletasks")
    })

    return(msg_frame)
  }

  # Funcao para executar codigo e mostrar saida (com suporte a pipe)
  execute_and_show <- function(code) {
    idioma <- tclvalue(idioma_var)

    # Armazenar todas as saidas
    all_output <- character(0)

    # Parsear o codigo em expressoes completas
    # O parse ja lida com expressoes de multiplas linhas automaticamente
    expressoes <- tryCatch({
      parse(text = code, keep.source = FALSE)
    }, error = function(e) {
      add_message(paste("Erro de sintaxe:", e$message), is_user = FALSE)
      return(NULL)
    })

    if (is.null(expressoes) || length(expressoes) == 0) {
      add_message("Nenhuma expressao valida para executar", is_user = FALSE)
      return()
    }

    # Executar cada expressao completa
    for (i in seq_along(expressoes)) {
      expr <- expressoes[i]

      # Verificar se eh uma expressao de atribuicao
      expr_str <- paste(deparse(expr), collapse = " ")
      eh_atribuicao <- grepl("<-", expr_str, fixed = TRUE) ||
        grepl("=", expr_str, fixed = TRUE) && !grepl("==", expr_str, fixed = TRUE)

      # Verificar se tem output explicito
      tem_output_explicito <- grepl("print\\(", expr_str) ||
        grepl("cat\\(", expr_str) ||
        grepl("message\\(", expr_str) ||
        grepl("warning\\(", expr_str)

      # Capturar saida da expressao
      output_lines <- capture.output({
        result <- tryCatch({
          eval(expr)
        }, error = function(e) {
          cat("Error:", e$message, "\n")
          NULL
        })

        # Mostrar resultado se nao for atribuicao e nao tiver output explicito
        if (!eh_atribuicao && !tem_output_explicito) {
          if (!is.null(result)) {
            print(result)
          }
        }
      })

      # Adicionar saida capturada
      if (length(output_lines) > 0) {
        all_output <- c(all_output, output_lines)
      }
    }

    # Formatar saida final
    if (length(all_output) > 0) {
      # Remover linhas vazias
      all_output <- all_output[all_output != ""]
      if (length(all_output) > 0) {
        output_text <- paste(all_output, collapse = "\n")
      } else {
        output_text <- "Codigo executado com sucesso (sem saida)"
      }
    } else {
      output_text <- "Codigo executado com sucesso (sem saida)"
    }

    # Adiciona saida como mensagem do assistente
    output_msg <- paste("--- Saida ---\n", output_text, sep = "")
    add_message(output_msg, is_user = FALSE)

    tcl("after", 50, function() {
      configure_canvas()
      scroll_to_bottom()
      tcl("update", "idletasks")
    })
  }

  # Funcao para adicionar resposta com botao e bloco de codigo
  add_response_with_button <- function(text, code) {
    idioma <- tclvalue(idioma_var)
    button_label <- respostas[[idioma]]$run_button_text
    button_label2 <- respostas[[idioma]]$copy_button_text

    # Adiciona o texto da resposta
    add_message(text, is_user = FALSE)

    if (!is.null(code)) {
      bloco <- add_code_block(code, is_user = FALSE)

      btn_frame <- tkframe(conversation_frame, bg = "white")
      btn <- tkbutton(btn_frame, text = button_label,
                      command = function() {
                        # LER O CONTEÚDO ATUAL DO WIDGET (editado pelo usuario)
                        codigo_atual <- tclvalue(tkget(bloco$text_widget, "1.0", "end"))
                        # Remover espaços extras no final
                        codigo_atual <- gsub("\n+$", "", codigo_atual)
                        # Executar o codigo editado
                        execute_and_show(codigo_atual)
                      },
                      bg = "lightblue", fg = "blue")
      tkpack(btn, side = "left", padx = 10)

      btn2 <- tkbutton(btn_frame, text = button_label2,
                       bg = "lightblue", fg = "blue",
                       command = function() {
                         tkclipboard.clear()
                         tkclipboard.append(tclvalue(tkget(bloco$text_widget, "1.0", "end-1c")))
                       })
      tkpack(btn2, side = "left", padx = 10)

      tkpack(btn_frame, side = "top", anchor = "nw", fill = "x", pady = 5)
    }

    tcl("after", 50, function() {
      configure_canvas()
      scroll_to_bottom()
      tcl("update", "idletasks")
    })
  }

  # Funcao para atualizar tamanho da fonte
  update_font_size <- function(delta) {
    current_size <- as.numeric(tclvalue(font_size_var))
    new_size <- current_size + delta
    if (new_size >= 8 && new_size <= 20) {
      tclvalue(font_size_var) <- new_size
      current_font_size <<- new_size

      # ATUALIZA A LARGURA MAXIMA BASEADA NO NOVO TAMANHO DA FONTE
      update_text_width()

      # Adiciona mensagem informando a mudanca
      idioma <- tclvalue(idioma_var)
      if (idioma == "pt") {
        add_message(paste("Tamanho da fonte alterado para", new_size, ". Novas mensagens usarao este tamanho."), is_user = FALSE)
      } else {
        add_message(paste("Font size changed to", new_size, ". New messages will use this size."), is_user = FALSE)
      }
      tcl("update", "idletasks")
    }
  }

  # Display do tamanho da fonte
  font_size_label <- tklabel(font_frame, textvariable = font_size_var, width = 3)
  tkpack(font_size_label, side = "left", padx = 5)

  btn_font_minus <- tkbutton(font_frame, text = "A-",
                             command = function() update_font_size(-1),
                             width = 3, bg = "lightgray")
  tkpack(btn_font_minus, side = "left", padx = 2)

  btn_font_plus <- tkbutton(font_frame, text = "A+",
                            command = function() update_font_size(1),
                            width = 3, bg = "lightgray")
  tkpack(btn_font_plus, side = "left", padx = 2)

  btn_font_reset <- tkbutton(font_frame, text = gettext("Reset", domain = "R-leem"),
                             command = function() {
                               tclvalue(font_size_var) <- 10
                               current_font_size <<- 10
                               update_text_width()
                               idioma <- tclvalue(idioma_var)
                               if (idioma == "pt") {
                                 add_message("Tamanho da fonte resetado para 10. Novas mensagens usarao este tamanho.", is_user = FALSE)
                               } else {
                                 add_message("Font size reset to 10. New messages will use this size.", is_user = FALSE)
                               }
                               tcl("update", "idletasks")
                             },
                             width = 5, bg = "lightgray")
  tkpack(btn_font_reset, side = "left", padx = 5)

  # Frame de entrada
  input_frame <- tkframe(main_window)
  tkpack(input_frame, side = "bottom", fill = "x", padx = 10, pady = 10)

  entry_var <- tclVar("")
  entry_widget <- tkentry(input_frame, textvariable = entry_var, width = 60,
                          font = tkfont.create(size = 10))
  tkpack(entry_widget, side = "left", padx = 5, expand = TRUE, fill = "x")

  # Processa a pergunta do usuario
  process_question <- function() {
    idioma <- tclvalue(idioma_var)
    pergunta <- tclvalue(entry_var)
    if (pergunta == "") return()

    tclvalue(entry_var) <- ""
    add_message(pergunta, is_user = TRUE)

    # Comandos especiais
    if (tolower(pergunta) %in% c("sair", "exit", "quit", "q")) {
      add_message(respostas[[idioma]]$goodbye, is_user = FALSE)
      tkdestroy(main_window)
      return()
    }

    if (tolower(pergunta) %in% c("ajuda", "help")) {
      help_msg <- if(idioma == "pt") {
        paste("FUNCOES DISPONIVEIS:\n\n",
              paste(names(leem_knowledge), collapse = "\n"),
              "\n\nDigite o nome da funcao para saber mais.\nExemplos:\n- new_leem - como criar objetos\n- tabfreq - tabela de frequencia\n- mean - calcular media")
      } else {
        paste("AVAILABLE FUNCTIONS:\n\n",
              paste(names(leem_knowledge), collapse = "\n"),
              "\n\nType the function name to learn more.\nExamples:\n- new_leem - how to create objects\n- tabfreq - frequency table\n- mean - calculate mean")
      }
      add_message(help_msg, is_user = FALSE)
      return()
    }

    # Busca na base de conhecimento
    match_result <- find_best_match(pergunta, idioma)

    if (match_result$found) {
      add_response_with_button(match_result$response, match_result$code)
    } else {
      add_message(respostas[[idioma]]$not_understood, is_user = FALSE)
    }

    tcl("after", 100, function() {
      configure_canvas()
      scroll_to_bottom()
      tcl("update", "idletasks")
    })
  }

  # Botoes
  send_button <- tkbutton(input_frame, text = gettext("Send", domain = "R-leem"),
                          command = process_question,
                          width = 10, bg = "lightgray")
  tkpack(send_button, side = "left", padx = 5)

  # Funcao para limpar a conversa (mesma acao do botao Clear)
  limpar_conversa <- function() {
    tkdestroy(conversation_frame)
    conversation_frame <<- tkframe(canvas, bg = "white")
    canvas_window <<- tkcreate(canvas, "window", 0, 0,
                               window = conversation_frame,
                               anchor = "nw")
    tkbind(conversation_frame, "<Configure>", function() configure_canvas())
    configure_canvas()
    scroll_to_bottom()
  }
  clear_button <- tkbutton(input_frame, text = gettext("Clear", domain = "R-leem"),
                           command = limpar_conversa,
                           width = 10, bg = "lightgray")
  tkpack(clear_button, side = "left", padx = 5)

  # Adicione os binds para Ctrl+L
  tkbind(main_window, "<Control-l>", function(event) limpar_conversa())
  tkbind(main_window, "<Control-L>", function(event) limpar_conversa())
  tkbind(entry_widget, "<Control-l>", function(event) limpar_conversa())
  tkbind(entry_widget, "<Control-L>", function(event) limpar_conversa())

  tkbind(entry_widget, "<Return>", function() process_question())

  idioma_inicial <- tclvalue(idioma_var)
  add_message(respostas[[idioma_inicial]]$welcome, is_user = FALSE)

  # ============================================================
  # ATALHOS DE TECLADO: + e - PARA AUMENTAR/DIMINUIR FONTE
  # ============================================================

  # Funcao para aumentar fonte
  aumentar_fonte_atalho <- function(event) {
    update_font_size(1)
  }

  # Funcao para diminuir fonte
  diminuir_fonte_atalho <- function(event) {
    update_font_size(-1)
  }

  # Atalhos usando Ctrl + e Ctrl - (opcional)
  tkbind(main_window, "<Control-plus>", aumentar_fonte_atalho)
  tkbind(main_window, "<Control-equal>", aumentar_fonte_atalho)  # Ctrl + = (Shift + = faz +)
  tkbind(main_window, "<Control-minus>", diminuir_fonte_atalho)


  tkfocus(entry_widget)

}

# Funcoes auxiliares para syntax highlighting
add_function_highlight <- function(txt_edit, hcolors) {
  HLfuns <- lapply(search(),FUN=function(x) { paste(unique(gsub("<-","",objects(x))),collapse=" ") })
  uniq <- sort(unique(unlist(lapply(search(), FUN=function(x) {strsplit(gsub("<-","",objects(x)),".",fixed=TRUE)} ))))
  uniq <- uniq[grep("abbreviate",uniq):length(uniq)]
  tmpx <- sort(rep(1:ceiling(length(uniq)/30),30))
  tmpsplit <- split(uniq,tmpx[1:length(uniq)])
  uniqtmp <- sapply(tmpsplit, FUN=function(x) { paste(" [list",paste(x,collapse=" ")," ]") })
  for(j in 1:length(uniqtmp)){
    .Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit),
               " basefunctions", j," ", hcolors$functions, uniqtmp[j], sep=""))
  }
  rm(HLfuns,uniq,tmpx,tmpsplit,uniqtmp)
}

# Funcao principal de configuracao de highlight
configure_r_highlight <- function(txt_edit, hcolors, highlight = c("r", "roxygen")) {

  if("r" %in% highlight) {

    add_function_highlight(txt_edit, hcolors)

    .Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit)," specials ",hcolors$operators,"  [list TRUE FALSE NULL NA if else ]",sep=""))
    .Tcl(paste("ctext::addHighlightClassForSpecialChars ",.Tk.ID(txt_edit)," operators ",hcolors$operators," {@-+!~?:;*/^<>=&|$,.}",sep=""))
    .Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," percoperators ",hcolors$operators," {%[[:alnum:][:punct:]]+%}",sep=""))
    .Tcl(paste("ctext::addHighlightClassForSpecialChars ",.Tk.ID(txt_edit)," brackets ",hcolors$brackets," {[]{}()}",sep=""))
    .Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," digits ",hcolors$digits," {\\m[-+]?[0-9]*\\.?[0-9]+\\M}",sep=""))
    .Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(txt_edit),' character1 ',hcolors$characters,' {"(?:[^\\"]|\\.)*"}',sep=""))
    .Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," character2 ",hcolors$characters," {'(?:[^\\']|\\.)*'}",sep=""))

    if(!"roxygen" %in% highlight) {
      .Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," comments ",hcolors$rcomments," {#[^\n\r]*}",sep=""))
    }
  }
}


# Funcao para atualizar wraplength de todos os labels existentes
atualizar_todos_wraplength <- function() {
  # Obtem a largura atual do canvas
  canvas_width <- as.numeric(tkwinfo("width", canvas))
  if (is.na(canvas_width) || canvas_width == 0) return()

  # Calcula o novo wraplength
  novo_wraplength <- max(100, canvas_width - 100)

  # Atualiza todos os text widgets armazenados
  if (exists("all_text_widgets") && length(all_text_widgets) > 0) {
    for (widget in all_text_widgets) {
      tryCatch({
        tkconfigure(widget, wraplength = novo_wraplength)
      }, error = function(e) {})
    }
  }

  # Metodo alternativo: percorre todos os frames do conversation_frame
  # (mais robusto, nao depende da lista all_text_widgets)
  tryCatch({
    msg_frames <- as.character(tkwinfo("children", conversation_frame))
    for (frame_id in msg_frames) {
      filhos <- as.character(tkwinfo("children", frame_id))
      for (filho in filhos) {
        classe <- tryCatch(tclvalue(tkwinfo("class", filho)), error = function(e) "")
        if (classe == "Label") {
          texto <- tryCatch(tclvalue(tkget(filho, "-text")), error = function(e) "")
          # Atualiza apenas labels com texto longo (ignora labels de identificacao "Voce:" e "Assistente:")
          if (!is.null(texto) && nchar(texto) > 20) {
            tkconfigure(filho, wraplength = novo_wraplength)
          }
        }
      }
    }
  }, error = function(e) {})
}

