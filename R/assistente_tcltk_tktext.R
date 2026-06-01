# .assistente_tcltk_leem <- function() {
#
#   # Check if tcltk is available
#   if (!requireNamespace("tcltk", quietly = TRUE)) {
#     cat("Package tcltk is required for the graphical interface.\n")
#     cat("Falling back to console version...\n")
#     return(.assistente_leem_console())
#   }
#
#   require(tcltk)
#
#   # Function to remove accents (Unicode normalization)
#   remover_acentos <- function(texto) {
#     texto_sem_acento <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
#     if (is.na(texto_sem_acento)) {
#       acentos <- list(
#         "á" = "a", "à" = "a", "ã" = "a", "â" = "a", "ä" = "a",
#         "é" = "e", "è" = "e", "ê" = "e", "ë" = "e",
#         "í" = "i", "ì" = "i", "î" = "i", "ï" = "i",
#         "ó" = "o", "ò" = "o", "õ" = "o", "ô" = "o", "ö" = "o",
#         "ú" = "u", "ù" = "u", "û" = "u", "ü" = "u",
#         "ç" = "c", "ñ" = "n"
#       )
#       for (acento in names(acentos)) {
#         texto <- gsub(acento, acentos[[acento]], texto, fixed = TRUE)
#       }
#       texto_sem_acento <- texto
#     }
#     return(tolower(texto_sem_acento))
#   }
#
#   # Bilingual knowledge base
#   respostas <- list(
#     pt = list(
#       criar = "
# \U0001F4E6 COMO CRIAR UM OBJETO LEEM:
#
# Leem <- new_leem(dados, variable = 'continuous')
#
# Exemplo:
# dados <- c(10, 12, 15, 18, 20)
# objeto <- new_leem(dados, variable = 'continuous')
# ",
#       tabela = "
# \U0001F4CA TABELA DE FREQUENCIA:
#
# tabfreq(objeto, rounding = 2)
#
# Exemplo:
# tabfreq(objeto, rounding = 2)
# ",
#       media = "
# \U0001F4C8 CALCULO DA MEDIA:
#
# mean(objeto, grouped = TRUE, details = FALSE)
#
# Parametros:
# - grouped = TRUE  -> usa dados agrupados
# - grouped = FALSE -> usa dados brutos
# - details = TRUE  -> mostra detalhes do calculo
#
# Exemplo:
# mean(objeto, grouped = TRUE, details = TRUE)
# ",
#       mediana = "
# \U0001F4C9 CALCULO DA MEDIANA:
#
# median(objeto)
#
# Exemplo:
# median(objeto)
# ",
#       grafico = "
# \U0001F3A8 GRAFICOS:
#
# plot(objeto, type = 'hist', col = 'blue')
#
# Tipos disponiveis:
# - 'hist'      -> histograma
# - 'freqpoly'  -> poligono de frequencia
#
# Exemplo:
# plot(objeto, type = 'hist', col = 'red', main = 'Meu Histograma')
# ",
#       summary = "
# \U0001F4CB RESUMO ESTATISTICO:
#
# summary(objeto)
#
# Exemplo:
# summary(objeto)
# ",
#       exemplo = "
# \U0001F4A1 EXEMPLO COMPLETO:
#
# library(leem)
#
# # 1. Criar dados
# set.seed(123)
# dados <- rnorm(100, mean = 50, sd = 10)
#
# # 2. Criar objeto leem
# obj <- new_leem(dados, variable = 'continuous')
#
# # 3. Ver tabela de frequencia
# tabfreq(obj)
#
# # 4. Calcular estatisticas
# mean(obj)
# median(obj)
#
# # 5. Fazer grafico
# plot(obj, type = 'hist', col = 'lightblue')
# ",
#       ajuda = "
# \U00002753 COMANDOS DISPONIVEIS:
#
# Digite qualquer palavra-chave (acentos sao ignorados):
# - 'criar'   - Como criar objeto leem
# - 'tabela'  - Tabela de frequencia
# - 'media'   - Calcular media
# - 'mediana' - Calcular mediana
# - 'grafico' - Criar graficos
# - 'summary' - Resumo estatistico
# - 'exemplo' - Exemplo completo
# - 'ajuda'   - Mostrar esta ajuda
# - 'sair'    - Encerrar assistente
# ",
#       user_label = "Voce: ",
#       assistant_label = "Assistente: ",
#       welcome = "Bem-vindo ao Assistente LEEM! Pergunte sobre o pacote leem.\nDigite 'ajuda' para ver os comandos disponiveis.",
#       goodbye = "Ate logo! Continue aprendendo o pacote leem!",
#       language_changed = "Idioma alterado para Portugues! Agora responderei em portugues."
#     ),
#     en = list(
#       criar = "
# \U0001F4E6 HOW TO CREATE A LEEM OBJECT:
#
# Leem <- new_leem(data, variable = 'continuous')
#
# Example:
# data <- c(10, 12, 15, 18, 20)
# object <- new_leem(data, variable = 'continuous')
# ",
#       tabela = "
# \U0001F4CA FREQUENCY TABLE:
#
# tabfreq(object, rounding = 2)
#
# Example:
# tabfreq(object, rounding = 2)
# ",
#       media = "
# \U0001F4C8 MEAN CALCULATION:
#
# mean(object, grouped = TRUE, details = FALSE)
#
# Parameters:
# - grouped = TRUE  -> uses grouped data
# - grouped = FALSE -> uses raw data
# - details = TRUE  -> shows calculation details
#
# Example:
# mean(object, grouped = TRUE, details = TRUE)
# ",
#       mediana = "
# \U0001F4C9 MEDIAN CALCULATION:
#
# median(object)
#
# Example:
# median(object)
# ",
#       grafico = "
# \U0001F3A8 PLOTS:
#
# plot(object, type = 'hist', col = 'blue')
#
# Available types:
# - 'hist'      -> histogram
# - 'freqpoly'  -> frequency polygon
#
# Example:
# plot(object, type = 'hist', col = 'red', main = 'My Histogram')
# ",
#       summary = "
# \U0001F4CB STATISTICAL SUMMARY:
#
# summary(object)
#
# Example:
# summary(object)
# ",
#       exemplo = "
# \U0001F4A1 COMPLETE EXAMPLE:
#
# library(leem)
#
# # 1. Create data
# set.seed(123)
# data <- rnorm(100, mean = 50, sd = 10)
#
# # 2. Create leem object
# obj <- new_leem(data, variable = 'continuous')
#
# # 3. View frequency table
# tabfreq(obj)
#
# # 4. Calculate statistics
# mean(obj)
# median(obj)
#
# # 5. Create plot
# plot(obj, type = 'hist', col = 'lightblue')
# ",
#       ajuda = "
# \U00002753 AVAILABLE COMMANDS:
#
# Type any keyword:
# - 'create'  - How to create leem object
# - 'table'   - Frequency table
# - 'mean'    - Calculate mean
# - 'median'  - Calculate median
# - 'plot'    - Create plots
# - 'summary' - Statistical summary
# - 'example' - Complete example
# - 'help'    - Show this help
# - 'exit'    - Exit assistant
# ",
#       user_label = "You: ",
#       assistant_label = "Assistant: ",
#       welcome = "Welcome to LEEM Assistant! Ask questions about the leem package.\nType 'help' to see available commands.",
#       goodbye = "Goodbye! Keep learning the leem package!",
#       language_changed = "Language changed to English! Now I will respond in English."
#     )
#   )
#
#   # Keywords by language
#   palavras_chave <- list(
#     pt = list(
#       criar = "criar|new_leem|objeto|novo",
#       tabela = "tabela|frequencia|tabfreq|frequencia",
#       media = "media|mean|media",
#       mediana = "mediana|median",
#       grafico = "grafico|plot|histograma|grafico",
#       summary = "summary|resumo",
#       exemplo = "exemplo|demo|demonstracao|demonstracao",
#       ajuda = "ajuda|help|comando|socorro"
#     ),
#     en = list(
#       criar = "create|new_leem|object|new",
#       tabela = "table|frequency|tabfreq",
#       media = "mean|average",
#       mediana = "median",
#       grafico = "plot|graph|histogram|chart",
#       summary = "summary",
#       exemplo = "example|demo",
#       ajuda = "help|command|assist"
#     )
#   )
#
#   # Create main window
#   main_window <- tktoplevel()
#   tkwm.title(main_window, "LEEM Assistant - Interactive Help")
#   tkwm.geometry(main_window, "850x600")
#
#   # Top frame for language and font controls
#   top_frame <- tkframe(main_window)
#   tkpack(top_frame, side = "top", fill = "x", padx = 10, pady = 5)
#
#   # Language selection frame (left side)
#   lang_frame <- tkframe(top_frame)
#   tkpack(lang_frame, side = "left", fill = "x", expand = TRUE, padx = 5)
#
#   tklabel(lang_frame, text = "Select Language / Selecione o Idioma:", font = tkfont.create(weight = "bold")) |>
#     tkpack(side = "left", padx = 5)
#
#   # Language variable
#   idioma_var <- tclVar("pt")
#
#   # Function to handle language change
#   on_language_change <- function() {
#     idioma <- tclvalue(idioma_var)
#     add_message(respostas[[idioma]]$language_changed, is_user = FALSE)
#   }
#
#   tkradiobutton(lang_frame, text = "English", variable = idioma_var, value = "en",
#                 command = on_language_change) |>
#     tkpack(side = "left", padx = 5)
#
#   tkradiobutton(lang_frame, text = "Portugues", variable = idioma_var, value = "pt",
#                 command = on_language_change) |>
#     tkpack(side = "left", padx = 5)
#
#   # Font control frame (right side)
#   font_frame <- tkframe(top_frame)
#   tkpack(font_frame, side = "right", padx = 5)
#
#   tklabel(font_frame, text = "Font Size:", font = tkfont.create(weight = "bold")) |>
#     tkpack(side = "left", padx = 5)
#
#   # Font size variable (starting at 10)
#   font_size_var <- tclVar(10)
#
#   # Reference to text widget height (will be updated)
#   current_height <- 25
#
#   # Function to update font size
#   update_font_size <- function(delta) {
#     current_size <- as.numeric(tclvalue(font_size_var))
#     new_size <- current_size + delta
#     # Limit between 8 and 20
#     if (new_size >= 8 && new_size <= 20) {
#       tclvalue(font_size_var) <- new_size
#       # Create new font for text widget
#       new_font <- tkfont.create(family = "Courier", size = new_size)
#       tkconfigure(text_widget, font = new_font, width = 30)
#
#       # Adjust text widget height inversely to maintain window layout
#       # Smaller font = more lines, larger font = fewer lines
#       new_height <- round(25 * (10 / new_size))
#       new_height <- max(15, min(35, new_height))  # Keep between 15 and 35
#       tkconfigure(text_widget, height = new_height)
#
#       # Force window update
#       tcl("update", "idletasks")
#     }
#   }
#
#   # Font size display
#   font_size_label <- tklabel(font_frame, textvariable = font_size_var, width = 3)
#   tkpack(font_size_label, side = "left", padx = 5)
#
#   # Buttons to increase/decrease font
#   btn_font_minus <- tkbutton(font_frame, text = "A-",
#                              command = function() update_font_size(-1),
#                              width = 3, bg = "lightgray")
#   tkpack(btn_font_minus, side = "left", padx = 2)
#
#   btn_font_plus <- tkbutton(font_frame, text = "A+",
#                             command = function() update_font_size(1),
#                             width = 3, bg = "lightgray")
#   tkpack(btn_font_plus, side = "left", padx = 2)
#
#   # Reset font button
#   btn_font_reset <- tkbutton(font_frame, text = "Reset",
#                              command = function() {
#                                tclvalue(font_size_var) <- 10
#                                new_font <- tkfont.create(family = "Courier", size = 10)
#                                tkconfigure(text_widget, font = new_font)
#                                tkconfigure(text_widget, height = 25)
#                                tcl("update", "idletasks")
#                              },
#                              width = 5, bg = "lightgray")
#   tkpack(btn_font_reset, side = "left", padx = 5)
#
#   # Create text widget for conversation
#   text_frame <- tkframe(main_window)
#   tkpack(text_frame, side = "top", fill = "both", expand = TRUE, padx = 10, pady = 5)
#
#   # Text widget with scrollbar (initial font size 10, height 25)
#   text_widget <- tktext(text_frame, height = 25, width = 85, wrap = "word",
#                         bg = "white", fg = "black",
#                         font = tkfont.create(family = "Courier", size = 10))
#   scrollbar <- tkscrollbar(text_frame, command = function(...) tkyview(text_widget, ...))
#   tkconfigure(text_widget, yscrollcommand = function(...) tkset(scrollbar, ...))
#
#   tkpack(text_widget, side = "left", fill = "both", expand = TRUE)
#   tkpack(scrollbar, side = "right", fill = "y")
#
#   # Input frame
#   input_frame <- tkframe(main_window)
#   tkpack(input_frame, side = "bottom", fill = "x", padx = 10, pady = 10)
#
#   # Entry widget (fixed font size)
#   entry_var <- tclVar("")
#   entry_widget <- tkentry(input_frame, textvariable = entry_var, width = 60,
#                           font = tkfont.create(size = 10))
#   tkpack(entry_widget, side = "left", padx = 5, expand = TRUE, fill = "x")
#
#   # Function to add message to conversation
#   add_message <- function(text, is_user = TRUE) {
#     idioma <- tclvalue(idioma_var)
#     if (is_user) {
#       label <- respostas[[idioma]]$user_label
#       tkinsert(text_widget, "end", label, "user_tag")
#       tkinsert(text_widget, "end", paste0(text, "\n\n"))
#     } else {
#       label <- respostas[[idioma]]$assistant_label
#       tkinsert(text_widget, "end", label, "assistant_tag")
#       tkinsert(text_widget, "end", paste0(text, "\n\n"))
#     }
#     tksee(text_widget, "end")
#     tcl("update")
#   }
#
#   # Configure text tags
#   tktag.configure(text_widget, "user_tag", foreground = "blue", font = tkfont.create(weight = "bold"))
#   tktag.configure(text_widget, "assistant_tag", foreground = "green", font = tkfont.create(weight = "bold"))
#
#   # Function to process user input
#   process_question <- function() {
#     # Get current language
#     idioma <- tclvalue(idioma_var)
#
#     # Get user input
#     pergunta <- tclvalue(entry_var)
#     if (pergunta == "") return()
#
#     # Clear entry
#     tclvalue(entry_var) <- ""
#
#     # Add user message to conversation
#     add_message(pergunta, is_user = TRUE)
#
#     # Check for exit command
#     if (tolower(pergunta) %in% c("sair", "exit", "quit", "q")) {
#       add_message(respostas[[idioma]]$goodbye, is_user = FALSE)
#       tkdestroy(main_window)
#       return()
#     }
#
#     # Check for help command
#     if (tolower(pergunta) %in% c("ajuda", "help")) {
#       add_message(respostas[[idioma]]$ajuda, is_user = FALSE)
#       return()
#     }
#
#     # Process question
#     pergunta_sem_acento <- remover_acentos(pergunta)
#     resposta_encontrada <- FALSE
#
#     for (nome_chave in names(palavras_chave[[idioma]])) {
#       padrao <- palavras_chave[[idioma]][[nome_chave]]
#       if (grepl(padrao, pergunta_sem_acento, ignore.case = TRUE)) {
#         add_message(respostas[[idioma]][[nome_chave]], is_user = FALSE)
#         resposta_encontrada <- TRUE
#         break
#       }
#     }
#
#     if (!resposta_encontrada) {
#       if (idioma == "pt") {
#         msg <- "Desculpe, nao entendi sua pergunta.\nTente palavras como: criar, tabela, media, mediana, grafico\nDigite 'ajuda' para ver os comandos disponiveis"
#       } else {
#         msg <- "Sorry, I didn't understand your question.\nTry keywords like: create, table, mean, median, plot\nType 'help' to see available commands"
#       }
#       add_message(msg, is_user = FALSE)
#     }
#   }
#
#   # Send button
#   send_button <- tkbutton(input_frame, text = "Send", command = process_question,
#                           width = 10, bg = "lightgray")
#   tkpack(send_button, side = "left", padx = 5)
#
#   # Clear button
#   clear_button <- tkbutton(input_frame, text = "Clear",
#                            command = function() tkdelete(text_widget, "1.0", "end"),
#                            width = 10, bg = "lightgray")
#   tkpack(clear_button, side = "left", padx = 5)
#
#   # Bind Enter key to send message
#   tkbind(entry_widget, "<Return>", function() process_question())
#
#   # Get initial language from the radiobutton
#   idioma_inicial <- tclvalue(idioma_var)  # "pt" default
#   add_message(respostas[[idioma_inicial]]$welcome, is_user = FALSE)
#
#   # Focus on entry widget
#   tkfocus(entry_widget)
# }
#
# # Console fallback version
# .assistente_leem_console <- function() {
#   cat("Console version - GUI not available\n")
# }
