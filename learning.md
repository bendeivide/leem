---
editor_options: 
  markdown: 
    wrap: 72
---

# Função P()

``` text
P()
|
|-- dist
  |-- "normal"
  |-- "binomial"
  |-- ...
|-- plot.type
  |-- "pdf" => Função densidade
  |-- "pmf" => Função de probabilidade
  |-- "cdf" => Função de distribuição
|-- length(q)
  |-- 1
    |-- lower.tail
      |-- TRUE
        |-- gui
          |-- "none"
          |-- "plot"
          |-- "tcltk"
          |-- "shiny"
      |-- FALSE
        |-- gui
          |-- "none"
          |-- "plot"
          |-- "tcltk"
          |-- "shiny"
      |-- NULL (LEMBRAR DE INSERIR WARNING NESSES CASOS)
        |-- gui
          |-- "none"
          |-- "plot"
          |-- "tcltk"
          |-- "shiny"
  |-- 2
    |-- região A
      |-- gui
          |-- "none"
          |-- "plot"
          |-- "tcltk"
          |-- "shiny"
    |-- região B
      |-- gui
          |-- "none"
          |-- "plot"
          |-- "tcltk"
          |-- "shiny"
```

## Distribuição normal

### Checagem de Marko sobre as interfaces

-   lower.tail = TRUE
    -   [x] TCLTK
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
        -   [x] exportar png
        -   [x] exportar svg
        -   [x] exportar pdf
    -   [x] RSTUDIO
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
    -   [x] SHINY
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
        -   [x] exportar png
        -   [x] exportar svg
        -   [x] exportar pdf
-   lower.tail = FALSE
    -   [x] TCLTK
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
        -   [x] exportar png
        -   [x] exportar svg
        -   [x] exportar pdf
    -   [x] RSTUDIO
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
    -   [ ] SHINY
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
        -   [x] exportar png
        -   [x] exportar svg
        -   [x] exportar pdf
            -   [ ] Eixo x não aparece
-   lower.tail = NULL
    -   [ ] TCLTK
        -   [ ] quantil
        -   [ ] média
        -   [ ] desvio padrão
        -   [ ] tamanho de texto
        -   [ ] orientação do título
        -   [ ] vírgula
        -   [ ] exportar png
        -   [ ] exportar svg
        -   [ ] exportar pdf
            -   [ ] Gráfico não é gerado
    -   [x] RSTUDIO
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
    -   [x] SHINY
        -   [x] quantil
        -   [x] média
        -   [x] desvio padrão
        -   [x] tamanho de texto
        -   [x] orientação do título
        -   [x] vírgula
        -   [x] exportar png
        -   [x] exportar svg
        -   [x] exportar pdf
-   Testar os argumentos da função P()
    -   [ ] lower.tail = TRUE
    -   [ ] lower.tail = FALSE
    -   [ ] lower.tail = NULL

### Funções comentadas

-   [ ] lower.tail = NULL
    -   [x] plotdnormalltnplot
    -   [x] plotdnormalltntcltk
    -   [x] plotdnormalltnrstudio
    -   [x] plotdnormalltnshiny

### Implementação/organização dos argumentos

-   lower.tail = TRUE
    -   [ ] gui
        -   [x] "plot"
            -   [x] title
            -   [x] decimals
            -   [x] long.segment
        -   [x] "rstudio"
            -   [x] amplitude dos sliders (0,1 a 0,1)
        -   [x] "tcltk"
            -   [x] amplitude dos sliders (0,1 a 0,1)
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
        -   [x] "shiny"
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
            -   [x] amplitude dos sliders (0,1 a 0,1)
-   lower.tail = NULL
    -   [ ] gui
        -   [x] Mensagem reforçando que o output é uma f(x) e não
            probabilidade
        -   [x] "plot"
            -   [x] title
            -   [x] decimals
            -   [x] long.segment
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
        -   [ ] tcltk
            -   [x] Mensagem reforçando que o output é uma f(x) e não
                probabilidade
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
        -   [ ] "rstudio"
            -   [x] Mensagem reforçando que o output é uma f(x) e não
                probabilidade
            -   [x] janela de argumentos reformulada
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
        -   [ ] shiny
            -   [x] Mensagem reforçando que o output é uma f(x) e não
                probabilidade
            -   [x] A gui recebe o gráfico no estado determinado. Ex.:
                Se foi plotado com segmentos longos, o gráfico aparece
                como tal, e assim, com os demais
-   lower.tail = FALSE
    -   [x] title
    -   [x] decimals
-   [ ] Cor azul
    -   [x] `q = 1`
        -   lower.tail = TRUE
        -   [ ] lower.tail = FALSE
        -   [ ] região B (`a <X< b`)
        -   [ ] região A (`a >X> b`)
-   [ ] Apresentar a informação da pdf
    -   [x] `q = 1`
        -   lower.tail = TRUE
        -   [ ] lower.tail = FALSE
        -   [ ] região B (`a <X< b`)
        -   [ ] região A (`a >X> b`)
