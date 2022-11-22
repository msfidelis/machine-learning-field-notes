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

## DISTRIBUIÇÃO DE POISSON

## A distribuição de Poisson é a distribuição de probabilidade de ocorrências 
##de eventos independentes em um intervalo

## A distribuição Poisson indica a probabilidade do número de sucessos (k) em
# uma determinada exposição contínua

# dpois	- Função de massa de probabilidade de Poisson (função de probabilidade)
# ppois	- Distribuição de Poisson (função de distribuição cumulativa)
# qpois	- função quantil de Poisson
# rpois - Geração de números pseudo-aleatórios de Poisson





# Um médico notou que atende uma taxa média de ocorrência de pacientes 
#com disfunção erétil em seu consultório. Essa média é 2 por semana. 


### Utilizando distribuição de poisson, vamos responder as seguintes perguntas: 


## A) Qual a probabilidade de que ele receba 1 paciente por semana?

## Resposta: 27.067% / 0.2706706

taxa_media_ocorrencias <- 2
quantidade_sucessos_interesse <- 1

probabilidade <- dpois(quantidade_sucessos_interesse, lambda=taxa_media_ocorrencias)
probabilidade
probabilidade * 100


## B) Qual a probabilidade de que ele receba 3 paciente por semana?

## Resposta: 18,04% / 0.180447

taxa_media_ocorrencias <- 2
quantidade_sucessos_interesse <- 3

probabilidade <- dpois(quantidade_sucessos_interesse, lambda=taxa_media_ocorrencias)
probabilidade
probabilidade * 100


## C) Qual a probabilidade de que ele não receba nenhum paciente na semana?

## Resposta: 13,53% / 0.1353353

taxa_media_ocorrencias <- 2
quantidade_sucessos_interesse <- 0

probabilidade <- dpois(quantidade_sucessos_interesse, lambda=taxa_media_ocorrencias)
probabilidade
probabilidade * 100





# Se houver 12 carros atravessando uma ponte por minuto, em média

## Qual a probabilidade A probabilidade de haver 16 carros ou menos 
## atravessando a ponte em um determinado minuto é dada pela função ppois .

## Resposta 89,87% / 0.89871

taxa_media_ocorrencias_carros <- 12
quantidade_sucessos_interesse <- 16

# "Ou menos", cauda inferior lower=TRUE
probabilidade <- ppois(quantidade_sucessos_interesse, lambda=taxa_media_ocorrencias_carros, lower=TRUE) 

probabilidade
probabilidade * 100


## Qual a probabilidade A probabilidade de haver 16 carros ou mais 
## atravessando a ponte em um determinado minuto é dada pela função ppois .

## Resposta 10,12% / 10.1291

taxa_media_ocorrencias_carros <- 12
quantidade_sucessos_interesse <- 16

# "Ou mais", cauda superior  lower=FALSE
probabilidade <- ppois(quantidade_sucessos_interesse, lambda=taxa_media_ocorrencias_carros, lower=FALSE) 

probabilidade
probabilidade * 100
