# Função P()

```text
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

- lower.tail = TRUE
  - [ ] gui
    - [X] "plot"
      - [X] title
      - [X] decimals
      - [X] long.segment
      - [X] amplitude dos sliders (0,1 a 0,1)
    - [X] "rstudio"
      - [X] amplitude dos sliders (0,1 a 0,1)
    - [X] "tcltk"
      - [X] amplitude dos sliders (0,1 a 0,1)
    - [X] "shiny"
      - [X] amplitude dos sliders (0,1 a 0,1)
- lower.tail = FALSE
  - [X] title
  - [X] decimals

- [ ] Cor azul
  - `q = 1` 
    - [X] lower.tail = TRUE
    - [ ] lower.tail = FALSE
    - [ ] região B (`a <X< b`)
    - [ ] região A (`a >X> b`)
- [ ] Apresentar a informação da pdf
  - `q = 1` 
    - [X] lower.tail = TRUE
    - [ ] lower.tail = FALSE
    - [ ] região B (`a <X< b`)
    - [ ] região A (`a >X> b`)




