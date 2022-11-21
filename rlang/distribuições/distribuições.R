pacotes <- c(
  "plotly",
  "plyr",
  "data.frame",
  "ggplot2",
  "ggthemes", 
  "hrbrthemes"
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

## DISTRIBUIÇÃO NORMAL

### Calcular a probabilidde de rendimentos de ações
## Analisando investimentos vimos que um retorno médio de uma ação "BANANINHA"
## na bolsa tem um retorno de 2.80% dentro de um período
## no mesmo período o desvio padrão foi de 1.20%

### Utilizando distribuição normal, vamos responder as seguintes perguntas: 

## A) Qual a probabilidade de que o retorno da ação seja de 4% ao mês

## Resposta: 15.87% / 0.1586553

media <- 0.028 # 2.8%
desvio_padrao <- 0.012 # 1.20%
probabilidade_de_retorno <- 0.04

distribuicao <- pnorm(probabilidade_de_retorno, sd = desvio_padrao, mean = media, lower.tail=FALSE)
distribuicao

distribuicao * 100

## B) Qual a probabilidade de que o retorno da ação seja menor que 3% ao mês

## Resposta: 56.62% / 56.61838

media <- 0.028 # 2.8%
desvio_padrao <- 0.012 # 1.20%
probabilidade_de_retorno <- 0.03

# Como o calculo busca por um valor menor, setamos o lower.tail=TRUE
distribuicao <- pnorm(probabilidade_de_retorno, sd = desvio_padrao, mean = media, lower.tail=TRUE)
distribuicao

distribuicao * 100


## C) Qual a probabilidade de que o retorno da ação seja negativo

## Resposta: 0.98% / 0.009815329

media <- 0.028 # 2.8%
desvio_padrao <- 0.012 # 1.20%
probabilidade_de_retorno <- 0

# Como o calculo busca por um valor menor que 0, setamos o lower.tail=TRUE
distribuicao <- pnorm(probabilidade_de_retorno, sd = desvio_padrao, mean = media, lower.tail=TRUE)
distribuicao

distribuicao * 100


## D) Qual a probabilidade de que o retorno da ação seja maior que 1% e menor que 5%

## Resposta: 89.98% / 89.98163

media <- 0.028 # 2.8%
desvio_padrao <- 0.012 # 1.20%
probabilidade_de_retorno_menor_que_5 <- 0.05
probabilidade_de_retorno_maior_que_1 <- 0.01

# Fazemos a distribuição normal aplicando a probabilidade nos dois cenários

# Qual a probabilidade do retorno ser menor que 5%?
dist_menor_que_5 <- pnorm(probabilidade_de_retorno_menor_que_5, sd = desvio_padrao, mean = media, lower.tail=TRUE)
dist_menor_que_5

# Qual a probabilidade do retorno ser maior que 1%?
dist_maior_que_1 <- pnorm(probabilidade_de_retorno_maior_que_1, sd = desvio_padrao, mean = media, lower.tail = TRUE)
dist_maior_que_1

# Calculamos as duas probabilidades e fazemos um calculo subtração entre as duas
distribuicao < dist_menor_que_5 - dist_maior_que_1

distribuicao * 100
