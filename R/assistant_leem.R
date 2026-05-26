#' Assistente Interativo do Pacote leem com Seleção de Idioma
#'
#' A função pergunta o idioma no início. Padrão é inglês.
#'
#' @export
assistente_leem <- function() {

  # Função para remover acentos (normalização Unicode)
  remover_acentos <- function(texto) {
    # Método 1: Usar iconv (R base, mais simples)
    texto_sem_acento <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")

    # Se falhar, tenta método alternativo
    if (is.na(texto_sem_acento)) {
      # Mapeamento manual de acentos comuns
      acentos <- list(
        "á" = "a", "à" = "a", "ã" = "a", "â" = "a", "ä" = "a",
        "é" = "e", "è" = "e", "ê" = "e", "ë" = "e",
        "í" = "i", "ì" = "i", "î" = "i", "ï" = "i",
        "ó" = "o", "ò" = "o", "õ" = "o", "ô" = "o", "ö" = "o",
        "ú" = "u", "ù" = "u", "û" = "u", "ü" = "u",
        "ç" = "c", "ñ" = "n"
      )

      for (acento in names(acentos)) {
        texto <- gsub(acento, acentos[[acento]], texto, fixed = TRUE)
      }
      texto_sem_acento <- texto
    }

    return(tolower(texto_sem_acento))
  }

  # Base de conhecimento bilíngue
  respostas <- list(
    pt = list(
      criar = "
📦 COMO CRIAR UM OBJETO LEEM:

Leem <- new_leem(dados, variable = 'continuous')

Exemplo:
dados <- c(10, 12, 15, 18, 20)
objeto <- new_leem(dados, variable = 'continuous')
",
      tabela = "
📊 TABELA DE FREQUÊNCIA:

tabfreq(objeto, rounding = 2)

Exemplo:
tabfreq(objeto, rounding = 2)
",
      media = "
📈 CÁLCULO DA MÉDIA:

mean(objeto, grouped = TRUE, details = FALSE)

Parâmetros:
- grouped = TRUE  → usa dados agrupados
- grouped = FALSE → usa dados brutos
- details = TRUE  → mostra detalhes do cálculo

Exemplo:
mean(objeto, grouped = TRUE, details = TRUE)
",
      mediana = "
📉 CÁLCULO DA MEDIANA:

median(objeto)

Exemplo:
median(objeto)
",
      grafico = "
🎨 GRÁFICOS:

plot(objeto, type = 'hist', col = 'blue')

Tipos disponíveis:
- 'hist'      → histograma
- 'freqpoly'  → polígono de frequência

Exemplo:
plot(objeto, type = 'hist', col = 'red', main = 'Meu Histograma')
",
      summary = "
📋 RESUMO ESTATÍSTICO:

summary(objeto)

Exemplo:
summary(objeto)
",
      exemplo = "
💡 EXEMPLO COMPLETO:

library(leem)

# 1. Criar dados
set.seed(123)
dados <- rnorm(100, mean = 50, sd = 10)

# 2. Criar objeto leem
obj <- new_leem(dados, variable = 'continuous')

# 3. Ver tabela de frequência
tabfreq(obj)

# 4. Calcular estatísticas
mean(obj)
median(obj)

# 5. Fazer gráfico
plot(obj, type = 'hist', col = 'lightblue')
",
      ajuda = "
❓ COMANDOS DISPONÍVEIS:

Digite qualquer palavra-chave (acentos são ignorados):
• 'criar'   - Como criar objeto leem
• 'tabela'  - Tabela de frequência
• 'media'   - Calcular média
• 'mediana' - Calcular mediana
• 'grafico' - Criar gráficos
• 'summary' - Resumo estatístico
• 'exemplo' - Exemplo completo
• 'ajuda'   - Mostrar esta ajuda
• 'sair'    - Encerrar assistente
"
    ),
    en = list(
      criar = "
📦 HOW TO CREATE A LEEM OBJECT:

Leem <- new_leem(data, variable = 'continuous')

Example:
data <- c(10, 12, 15, 18, 20)
object <- new_leem(data, variable = 'continuous')
",
      tabela = "
📊 FREQUENCY TABLE:

tabfreq(object, rounding = 2)

Example:
tabfreq(object, rounding = 2)
",
      media = "
📈 MEAN CALCULATION:

mean(object, grouped = TRUE, details = FALSE)

Parameters:
- grouped = TRUE  → uses grouped data
- grouped = FALSE → uses raw data
- details = TRUE  → shows calculation details

Example:
mean(object, grouped = TRUE, details = TRUE)
",
      mediana = "
📉 MEDIAN CALCULATION:

median(object)

Example:
median(object)
",
      grafico = "
🎨 PLOTS:

plot(object, type = 'hist', col = 'blue')

Available types:
- 'hist'      → histogram
- 'freqpoly'  → frequency polygon

Example:
plot(object, type = 'hist', col = 'red', main = 'My Histogram')
",
      summary = "
📋 STATISTICAL SUMMARY:

summary(object)

Example:
summary(object)
",
      exemplo = "
💡 COMPLETE EXAMPLE:

library(leem)

# 1. Create data
set.seed(123)
data <- rnorm(100, mean = 50, sd = 10)

# 2. Create leem object
obj <- new_leem(data, variable = 'continuous')

# 3. View frequency table
tabfreq(obj)

# 4. Calculate statistics
mean(obj)
median(obj)

# 5. Create plot
plot(obj, type = 'hist', col = 'lightblue')
",
      ajuda = "
❓ AVAILABLE COMMANDS:

Type any keyword:
• 'create'  - How to create leem object
• 'table'   - Frequency table
• 'mean'    - Calculate mean
• 'median'  - Calculate median
• 'plot'    - Create plots
• 'summary' - Statistical summary
• 'example' - Complete example
• 'help'    - Show this help
• 'exit'    - Exit assistant
"
    )
  )

  # Palavras-chave por idioma
  palavras_chave <- list(
    pt = list(
      criar = "criar|new_leem|objeto|novo",
      tabela = "tabela|frequencia|tabfreq|frequência",
      media = "media|mean|média",
      mediana = "mediana|median",
      grafico = "grafico|plot|histograma|gráfico",
      summary = "summary|resumo",
      exemplo = "exemplo|demo|demonstracao|demonstração",
      ajuda = "ajuda|help|comando|socorro"
    ),
    en = list(
      criar = "create|new_leem|object|new",
      tabela = "table|frequency|tabfreq",
      media = "mean|average",
      mediana = "median",
      grafico = "plot|graph|histogram|chart",
      summary = "summary",
      exemplo = "example|demo",
      ajuda = "help|command|assist"
    )
  )

  # ============================================================
  # SELEÇÃO INICIAL DO IDIOMA
  # ============================================================

  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║           🤖 LEEM PACKAGE INTERACTIVE ASSISTANT           ║\n")
  cat("╠═══════════════════════════════════════════════════════════╣\n")
  cat("║                                                           ║\n")
  cat("║  🌐 Select your language / Selecione o idioma:            ║\n")
  cat("║                                                           ║\n")
  cat("║     1 - English (default)                                 ║\n")
  cat("║     2 - Português                                         ║\n")
  cat("║                                                           ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  cat("\n")

  # Loop para garantir escolha válida
  escolha_valida <- FALSE
  idioma <- "en"  # padrão inglês

  while(!escolha_valida) {
    cat("Your choice / Sua escolha [1/2]: ")
    escolha <- readLines(n = 1, warn = FALSE)

    # Se usuário não digitar nada, usa padrão (inglês)
    if(escolha == "") {
      escolha <- "1"
    }

    if(escolha == "1") {
      idioma <- "en"
      escolha_valida <- TRUE
    } else if(escolha == "2") {
      idioma <- "pt"
      escolha_valida <- TRUE
    } else {
      cat("❌ Invalid option / Opção inválida. Please choose 1 or 2.\n")
    }
  }

  # ============================================================
  # INTERFACE PRINCIPAL
  # ============================================================

  cat("\n")
  cat("╔════════════════════════════════════════════════════╗\n")

  if(idioma == "pt") {
    cat("║     🤖 ASSISTENTE INTERATIVO DO PACOTE leem        ║\n")
    cat("╠════════════════════════════════════════════════════╣\n")
    cat("║  Digite sua dúvida (palavras-chave em português)   ║\n")
    cat("║  Digite 'ajuda' para ver os comandos disponíveis   ║\n")
    cat("║  Digite 'sair' para encerrar                       ║\n")
  } else {
    cat("║     🤖 LEEM PACKAGE INTERACTIVE ASSISTANT           ║\n")
    cat("╠════════════════════════════════════════════════════╣\n")
    cat("║  Type your question (keywords in English)          ║\n")
    cat("║  Type 'help' to see available commands             ║\n")
    cat("║  Type 'exit' to exit                               ║\n")
  }

  cat("╚════════════════════════════════════════════════════╝\n")
  cat("\n")

  # ============================================================
  # LOOP PRINCIPAL
  # ============================================================

  while(TRUE) {
    # Ler pergunta do usuário
    if(idioma == "pt") {
      cat("Você: ")
    } else {
      cat("You: ")
    }

    pergunta <- readLines(n = 1, warn = FALSE)

    # Verificar codificação e converter se necessário
    if(Encoding(pergunta) == "unknown" && idioma == "pt") {
      Encoding(pergunta) <- "UTF-8"
    }

    # Comandos de saída (bilíngue)
    if(tolower(pergunta) %in% c("sair", "exit", "quit", "q")) {
      if(idioma == "pt") {
        cat("\n👋 Até logo! Continue aprendendo o pacote leem!\n\n")
      } else {
        cat("\n👋 Goodbye! Keep learning the leem package!\n\n")
      }
      break
    }

    # Remover acentos e normalizar
    pergunta_sem_acento <- remover_acentos(pergunta)

    # Verificar se a pergunta contém alguma palavra-chave
    resposta_encontrada <- FALSE

    # Procurar correspondência
    for(nome_chave in names(palavras_chave[[idioma]])) {
      padrao <- palavras_chave[[idioma]][[nome_chave]]

      if(grepl(padrao, pergunta_sem_acento, ignore.case = TRUE)) {
        # Encontrou resposta!
        cat("\n", respostas[[idioma]][[nome_chave]], "\n")
        cat("─────────────────────────────────────────────────\n\n")
        resposta_encontrada <- TRUE
        break
      }
    }

    # Se não encontrou resposta
    if(!resposta_encontrada) {
      if(idioma == "pt") {
        cat("\n❓ Desculpe, não entendi sua pergunta.\n")
        cat("💡 Tente palavras como: criar, tabela, media, mediana, grafico\n")
        cat("📚 Digite 'ajuda' para ver todos os comandos disponíveis\n\n")
      } else {
        cat("\n❓ Sorry, I didn't understand your question.\n")
        cat("💡 Try keywords like: create, table, mean, median, plot\n")
        cat("📚 Type 'help' to see all available commands\n\n")
      }
    }
  }
}

# ============================================================
# EXEMPLOS DE USO
# ============================================================

# Para usar, basta chamar a função sem argumentos:
# assistente_leem()
#
# O programa vai perguntar:
# 1 - English (default)
# 2 - Português
#
# Digite 1 ou 2 e pressione Enter
