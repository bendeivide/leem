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
#       criar = "COMO CRIAR UM OBJETO LEEM:\n\nnew_leem(dados, variable = 'continuous')\n\nExemplo:\ndados <- c(10, 12, 15, 18, 20)\nobjeto <- new_leem(dados, variable = 'continuous')",
#       tabela = "TABELA DE FREQUENCIA:\n\ntabfreq(objeto, rounding = 2)\n\nExemplo:\ntabfreq(objeto, rounding = 2)",
#       media = "CALCULO DA MEDIA:\n\nmean(objeto, grouped = TRUE, details = FALSE)\n\nParametros:\n- grouped = TRUE  -> usa dados agrupados\n- grouped = FALSE -> usa dados brutos\n- details = TRUE  -> mostra detalhes do calculo\n\nExemplo:\nmean(objeto, grouped = TRUE, details = TRUE)",
#       mediana = "CALCULO DA MEDIANA:\n\nmedian(objeto)\n\nExemplo:\nmedian(objeto)",
#       grafico = "GRAFICOS:\n\nplot(objeto, type = 'hist', col = 'blue')\n\nTipos disponiveis:\n- 'hist'      -> histograma\n- 'freqpoly'  -> poligono de frequencia\n\nExemplo:\nplot(objeto, type = 'hist', col = 'red', main = 'Meu Histograma')",
#       summary = "RESUMO ESTATISTICO:\n\nsummary(objeto)\n\nExemplo:\nsummary(objeto)",
#       exemplo = "EXEMPLO COMPLETO:\n\nlibrary(leem)\n\n# 1. Criar dados\nset.seed(123)\ndados <- rnorm(100, mean = 50, sd = 10)\n\n# 2. Criar objeto leem\nobj <- new_leem(dados, variable = 'continuous')\n\n# 3. Ver tabela de frequencia\ntabfreq(obj)\n\n# 4. Calcular estatisticas\nmean(obj)\nmedian(obj)\n\n# 5. Fazer grafico\nplot(obj, type = 'hist', col = 'lightblue')",
#       ajuda = "COMANDOS DISPONIVEIS:\n\nDigite qualquer palavra-chave:\n- 'criar'   - Como criar objeto leem\n- 'tabela'  - Tabela de frequencia\n- 'media'   - Calcular media\n- 'mediana' - Calcular mediana\n- 'grafico' - Criar graficos\n- 'summary' - Resumo estatistico\n- 'exemplo' - Exemplo completo\n- 'ajuda'   - Mostrar esta ajuda\n- 'sair'    - Encerrar assistente",
#       user_label = "Voce: ",
#       assistant_label = "Assistente: ",
#       welcome = "Bem-vindo ao Assistente LEEM! Pergunte sobre o pacote leem.\nDigite 'ajuda' para ver os comandos disponiveis.",
#       goodbye = "Ate logo! Continue aprendendo o pacote leem!",
#       language_changed = "Idioma alterado para Portugues! Agora responderei em portugues.",
#       run_button_text = "Executar este exemplo"
#     ),
#     en = list(
#       criar = "HOW TO CREATE A LEEM OBJECT:\n\nnew_leem(data, variable = 'continuous')\n\nExample:\ndata <- c(10, 12, 15, 18, 20)\nobject <- new_leem(data, variable = 'continuous')",
#       tabela = "FREQUENCY TABLE:\n\ntabfreq(object, rounding = 2)\n\nExample:\ntabfreq(object, rounding = 2)",
#       media = "MEAN CALCULATION:\n\nmean(object, grouped = TRUE, details = FALSE)\n\nParameters:\n- grouped = TRUE  -> uses grouped data\n- grouped = FALSE -> uses raw data\n- details = TRUE  -> shows calculation details\n\nExample:\nmean(object, grouped = TRUE, details = TRUE)",
#       mediana = "MEDIAN CALCULATION:\n\nmedian(object)\n\nExample:\nmedian(object)",
#       grafico = "PLOTS:\n\nplot(object, type = 'hist', col = 'blue')\n\nAvailable types:\n- 'hist'      -> histogram\n- 'freqpoly'  -> frequency polygon\n\nExample:\nplot(object, type = 'hist', col = 'red', main = 'My Histogram')",
#       summary = "STATISTICAL SUMMARY:\n\nsummary(object)\n\nExample:\nsummary(object)",
#       exemplo = "COMPLETE EXAMPLE:\n\nlibrary(leem)\n\n# 1. Create data\nset.seed(123)\ndata <- rnorm(100, mean = 50, sd = 10)\n\n# 2. Create leem object\nobj <- new_leem(data, variable = 'continuous')\n\n# 3. View frequency table\ntabfreq(obj)\n\n# 4. Calculate statistics\nmean(obj)\nmedian(obj)\n\n# 5. Create plot\nplot(obj, type = 'hist', col = 'lightblue')",
#       ajuda = "AVAILABLE COMMANDS:\n\nType any keyword:\n- 'create'  - How to create leem object\n- 'table'   - Frequency table\n- 'mean'    - Calculate mean\n- 'median'  - Calculate median\n- 'plot'    - Create plots\n- 'summary' - Statistical summary\n- 'example' - Complete example\n- 'help'    - Show this help\n- 'exit'    - Exit assistant",
#       user_label = "You: ",
#       assistant_label = "Assistant: ",
#       welcome = "Welcome to LEEM Assistant! Ask questions about the leem package.\nType 'help' to see available commands.",
#       goodbye = "Goodbye! Keep learning the leem package!",
#       language_changed = "Language changed to English! Now I will respond in English.",
#       run_button_text = "Run this example"
#     )
#   )
#
#   # Executable examples with code
#   exemplos_executaveis <- list(
#     criar = list(
#       code = '
# # Create sample data
# dados <- c(10, 12, 15, 18, 20)
# cat("Data:", dados, "\n")
#
# # Create leem object
# objeto <- new_leem(dados, variable = "continuous")
# cat("Leem object created successfully!\n")
# print(objeto)
# '
#     ),
#     media = list(
#       code = '
# # Create sample data
# dados <- c(10, 12, 15, 18, 20)
# objeto <- new_leem(dados, variable = "continuous")
#
# # Calculate mean
# cat("Calculating mean...\n")
# resultado <- mean(objeto, grouped = TRUE, details = TRUE)
# cat("Mean:", resultado, "\n")
# '
#     ),
#     mediana = list(
#       code = '
# # Create sample data
# dados <- c(10, 12, 15, 18, 20)
# objeto <- new_leem(dados, variable = "continuous")
#
# # Calculate median
# cat("Calculating median...\n")
# resultado <- median(objeto)
# cat("Median:", resultado, "\n")
# '
#     ),
#     grafico = list(
#       code = '
# # Create sample data
# set.seed(123)
# dados <- rnorm(100, mean = 50, sd = 10)
# objeto <- new_leem(dados, variable = "continuous")
#
# # Create histogram
# cat("Creating histogram...\n")
# plot(objeto, type = "hist", col = "lightblue", main = "Sample Histogram")
# '
#     ),
#     tabela = list(
#       code = '
# # Create sample data
# dados <- c(10, 12, 15, 18, 18, 20, 20, 20, 20)
# objeto <- new_leem(dados, variable = "discrete")
#
# # Create frequency table
# cat("Frequency table:\n")
# print(tabfreq(objeto, rounding = 2))
# '
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
#   tkwm.geometry(main_window, "900x650")
#
#   # Top frame for language and font controls
#   top_frame <- tkframe(main_window)
#   tkpack(top_frame, side = "top", fill = "x", padx = 10, pady = 5)
#
#   # Language selection frame
#   lang_frame <- tkframe(top_frame)
#   tkpack(lang_frame, side = "left", fill = "x", expand = TRUE, padx = 5)
#
#   tklabel(lang_frame, text = "Select Language / Selecione o Idioma:", font = tkfont.create(weight = "bold")) |>
#     tkpack(side = "left", padx = 5)
#
#   idioma_var <- tclVar("pt")
#
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
#   # Font control frame
#   font_frame <- tkframe(top_frame)
#   tkpack(font_frame, side = "right", padx = 5)
#
#   tklabel(font_frame, text = "Font Size:", font = tkfont.create(weight = "bold")) |>
#     tkpack(side = "left", padx = 5)
#
#   font_size_var <- tclVar(10)
#
#   # Global variable to store current font size for new messages
#   current_font_size <- 10
#
#   # Create canvas and scrollbar for conversation area
#   canvas_frame <- tkframe(main_window)
#   tkpack(canvas_frame, side = "top", fill = "both", expand = TRUE, padx = 10, pady = 5)
#
#   canvas <- tkcanvas(canvas_frame, bg = "white", highlightthickness = 0)
#   scrollbar <- tkscrollbar(canvas_frame, command = function(...) tkyview(canvas, ...))
#   tkconfigure(canvas, yscrollcommand = function(...) tkset(scrollbar, ...))
#
#   tkpack(canvas, side = "left", fill = "both", expand = TRUE)
#   tkpack(scrollbar, side = "right", fill = "y")
#
#   # Frame inside canvas to hold conversation items
#   conversation_frame <- tkframe(canvas, bg = "white")
#   canvas_window <- tkcreate(canvas, "window", 0, 0, window = conversation_frame, anchor = "nw")
#
#   # Configure canvas scrolling
#   configure_canvas <- function() {
#     req_width <- as.numeric(tkwinfo("reqwidth", conversation_frame))
#     req_height <- as.numeric(tkwinfo("reqheight", conversation_frame))
#     tkconfigure(canvas, scrollregion = paste("0 0", req_width, req_height))
#   }
#
#   tkbind(conversation_frame, "<Configure>", function() configure_canvas())
#   tkbind(canvas, "<Configure>", function() {
#     canvas_width <- as.numeric(tkwinfo("width", canvas))
#     tkitemconfigure(canvas, canvas_window, width = canvas_width)
#   })
#
#   # Function to scroll to bottom
#   scroll_to_bottom <- function() {
#     req_height <- as.numeric(tkwinfo("reqheight", conversation_frame))
#     canvas_height <- as.numeric(tkwinfo("height", canvas))
#     if (req_height > canvas_height) {
#       tkyview(canvas, "moveto", 1.0)
#     }
#     tcl("update", "idletasks")
#   }
#
#   # Function to add text to conversation
#   add_message <- function(text, is_user = TRUE, with_button = FALSE, button_code = NULL, button_label = NULL) {
#     idioma <- tclvalue(idioma_var)
#
#     # Create frame for this message
#     msg_frame <- tkframe(conversation_frame, bg = "white")
#
#     # Add label
#     if (is_user) {
#       label <- respostas[[idioma]]$user_label
#       label_color <- "blue"
#     } else {
#       label <- respostas[[idioma]]$assistant_label
#       label_color <- "green"
#     }
#
#     lbl <- tklabel(msg_frame, text = label, font = tkfont.create(weight = "bold"),
#                    foreground = label_color, bg = "white")
#     tkpack(lbl, side = "left", anchor = "nw")
#
#     # Add text with current font size
#     txt <- tklabel(msg_frame, text = text, justify = "left", bg = "white",
#                    font = tkfont.create(family = "Courier", size = current_font_size))
#     tkpack(txt, side = "left", anchor = "nw", padx = 5)
#
#     # Add button if needed
#     if (with_button && !is.null(button_code)) {
#       btn <- tkbutton(msg_frame, text = button_label,
#                       command = function() execute_and_show(button_code),
#                       bg = "lightgreen", fg = "darkgreen")
#       tkpack(btn, side = "left", padx = 10)
#     }
#
#     tkpack(msg_frame, side = "top", anchor = "nw", fill = "x", pady = 5)
#
#     # Update scroll region and scroll to bottom
#     configure_canvas()
#     scroll_to_bottom()
#
#     return(msg_frame)
#   }
#
#   # Function to execute code and show output
#   execute_and_show <- function(code) {
#     idioma <- tclvalue(idioma_var)
#
#     # Capture output
#     output_lines <- capture.output({
#       result <- tryCatch({
#         eval(parse(text = code))
#       }, error = function(e) {
#         cat("Error:", e$message, "\n")
#         NULL
#       })
#     })
#
#     # Format output
#     if (length(output_lines) > 0 && !(length(output_lines) == 1 && output_lines[1] == "")) {
#       output_text <- paste(output_lines, collapse = "\n")
#     } else if (!is.null(result)) {
#       output_text <- paste(capture.output(print(result)), collapse = "\n")
#     } else {
#       output_text <- "Code executed successfully (no output)"
#     }
#
#     # Add output as assistant message
#     output_msg <- paste("--- Output ---\n", output_text, sep = "")
#     add_message(output_msg, is_user = FALSE, with_button = FALSE)
#   }
#
#   # Function to add response with run button
#   add_response_with_button <- function(text, code, is_user = FALSE) {
#     idioma <- tclvalue(idioma_var)
#     button_label <- respostas[[idioma]]$run_button_text
#     add_message(text, is_user = FALSE, with_button = TRUE, button_code = code, button_label = button_label)
#   }
#
#   # Function to update font size (only affects new messages)
#   update_font_size <- function(delta) {
#     current_size <- as.numeric(tclvalue(font_size_var))
#     new_size <- current_size + delta
#     if (new_size >= 8 && new_size <= 20) {
#       tclvalue(font_size_var) <- new_size
#       current_font_size <<- new_size
#       # Add a system message informing about font change
#       idioma <- tclvalue(idioma_var)
#       if (idioma == "pt") {
#         add_message(paste("Tamanho da fonte alterado para", new_size, ". Novas mensagens usarão este tamanho."), is_user = FALSE)
#       } else {
#         add_message(paste("Font size changed to", new_size, ". New messages will use this size."), is_user = FALSE)
#       }
#       tcl("update", "idletasks")
#     }
#   }
#
#   # Font size display
#   font_size_label <- tklabel(font_frame, textvariable = font_size_var, width = 3)
#   tkpack(font_size_label, side = "left", padx = 5)
#
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
#   btn_font_reset <- tkbutton(font_frame, text = "Reset",
#                              command = function() {
#                                tclvalue(font_size_var) <- 10
#                                current_font_size <<- 10
#                                idioma <- tclvalue(idioma_var)
#                                if (idioma == "pt") {
#                                  add_message("Tamanho da fonte resetado para 10. Novas mensagens usarão este tamanho.", is_user = FALSE)
#                                } else {
#                                  add_message("Font size reset to 10. New messages will use this size.", is_user = FALSE)
#                                }
#                                tcl("update", "idletasks")
#                              },
#                              width = 5, bg = "lightgray")
#   tkpack(btn_font_reset, side = "left", padx = 5)
#
#   # Input frame
#   input_frame <- tkframe(main_window)
#   tkpack(input_frame, side = "bottom", fill = "x", padx = 10, pady = 10)
#
#   entry_var <- tclVar("")
#   entry_widget <- tkentry(input_frame, textvariable = entry_var, width = 60,
#                           font = tkfont.create(size = 10))
#   tkpack(entry_widget, side = "left", padx = 5, expand = TRUE, fill = "x")
#
#   # Process user input
#   process_question <- function() {
#     idioma <- tclvalue(idioma_var)
#     pergunta <- tclvalue(entry_var)
#     if (pergunta == "") return()
#
#     tclvalue(entry_var) <- ""
#     add_message(pergunta, is_user = TRUE)
#
#     if (tolower(pergunta) %in% c("sair", "exit", "quit", "q")) {
#       add_message(respostas[[idioma]]$goodbye, is_user = FALSE)
#       tkdestroy(main_window)
#       return()
#     }
#
#     if (tolower(pergunta) %in% c("ajuda", "help")) {
#       add_message(respostas[[idioma]]$ajuda, is_user = FALSE)
#       return()
#     }
#
#     pergunta_sem_acento <- remover_acentos(pergunta)
#     resposta_encontrada <- FALSE
#
#     for (nome_chave in names(palavras_chave[[idioma]])) {
#       padrao <- palavras_chave[[idioma]][[nome_chave]]
#       if (grepl(padrao, pergunta_sem_acento, ignore.case = TRUE)) {
#         resposta_text <- respostas[[idioma]][[nome_chave]]
#
#         if (nome_chave %in% names(exemplos_executaveis)) {
#           add_response_with_button(resposta_text, exemplos_executaveis[[nome_chave]]$code)
#         } else {
#           add_message(resposta_text, is_user = FALSE)
#         }
#
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
#   send_button <- tkbutton(input_frame, text = "Send", command = process_question,
#                           width = 10, bg = "lightgray")
#   tkpack(send_button, side = "left", padx = 5)
#
#   # Clear button - Versão corrigida (recria o frame)
#   clear_button <- tkbutton(input_frame, text = "Clear",
#                            command = function() {
#                              # Destroy the conversation frame
#                              tkdestroy(conversation_frame)
#
#                              # Recreate conversation frame
#                              conversation_frame <<- tkframe(canvas, bg = "white")
#                              canvas_window <<- tkcreate(canvas, "window", 0, 0,
#                                                         window = conversation_frame,
#                                                         anchor = "nw")
#
#                              # Rebind the configure event
#                              tkbind(conversation_frame, "<Configure>", function() configure_canvas())
#
#                              # Update canvas
#                              configure_canvas()
#                              scroll_to_bottom()
#                            },
#                            width = 10, bg = "lightgray")
#   tkpack(clear_button, side = "left", padx = 5)
#
#   tkbind(entry_widget, "<Return>", function() process_question())
#
#   idioma_inicial <- tclvalue(idioma_var)
#   add_message(respostas[[idioma_inicial]]$welcome, is_user = FALSE)
#
#   tkfocus(entry_widget)
# }
#
# # Console fallback version
# .assistente_leem_console <- function() {
#   cat("Console version - GUI not available\n")
# }
