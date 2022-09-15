
<!-- README.md is generated from README.Rmd. Please edit that file -->

# leem <img src="man/figures/logo.png" align="right" alt="" width="120" />

Esse projeto tem por objetivo de criar uma interface gráfica ao usuário
no [R](http://r-project.org/), usando os pacotes
[tcltk](http://r-project.org/) e [shiny](https://shiny.rstudio.com/).
Tentaremos apresentar temas simples e complexos de forma interativa,
para que o professor dessas área possam abordar o tema de modo mais
facilmente.

A versão lançada no [CRAN](https://CRAN.R-project.org) está disponível,
por enquanto, com a interface em `tcltk`.

## Instalação

A versão lançada do pacote `leem` está disponível no
[CRAN](https://CRAN.R-project.org), e pode ser instalado com as
seguintes linhas de comando:

``` r
install.packages("leem")
```

E a versão em desenvolvimento no [GitHub](https://github.com/) com:

``` r
# Instalar os pacotes dependentes
pkgs <- c("manipulate", "tkRplotR", "tkrplot", "crayon")
install.packages(pkgs)
# install.packages("devtools")
devtools::install_github("bendeivide/leem")
```

## Exemplos

``` r
library(leem)
leem()
```

## Projetos integrados

Temos dentro desse projetos, diversos projetos integrados, sendo:

-   Projeto de dissertação PROFMAT/CAP/UFSJ, da orientanda [Juliane
    Nassaralla Almeida](http://lattes.cnpq.br/5176118169651142);
-   Grupo de Iniciação Científica em Estatística/R:
    -   [Amanda Kelly
        Costa](https://www.linkedin.com/in/amandakellycosta/)
    -   [Lívia Dias de Sá
        Amorim](https://www.linkedin.com/in/líviasdias/)
    -   [Gustavo do Remédios da Silva
        Paula](https://www.linkedin.com/in/gustavo-dos-remédios-da-silva-paula-995a04181)
    -   [Dailon Vinicius Barbosa de
        Fátima](linkedin.com/in/dailon-vinicius-6502a9207)
    -   [André Felipe Duarte
        Gonçalves](https://www.linkedin.com/in/andrefdg/)

## Propostas da GUI

-   GUI 1 (Proposta de Gustavo):
    <https://www.figma.com/file/qYH2QntEZcVZDvq1ejp3W7/Untitled?node-id=0%3A1>

-   GUI 2 (Proposta de Juliane):
    <https://www.figma.com/proto/isRxuZbvduJe4bww0FvlTW/Untitled?node-id=7%3A18&scaling=min-zoom>

-   GUI 3 (Proposta de André Felipe):
    <https://www.figma.com/proto/lapHIsabTlxt6NLO0Grsur/Untitled?node-id=13%3A582&scaling=contain&page-id=0%3A1>

-   GUI 4 (Proposta de Dailon): [Imagem](images/gui-dailon.jpeg)
