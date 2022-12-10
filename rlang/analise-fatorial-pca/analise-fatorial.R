pacotes <- c(
  "plotly",
  "plyr",
  "ggplot2",
  "ggthemes", 
  "hrbrthemes",
  "reshape2",
  "psych",
  "Hmisc",
  "PerformanceAnalytics",
  "corrplot"
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

# Análise Fatorial - Quantitativa

## Abrindo o arquivo inicial para efetuar análise 
idh_raw <- read.table("idh.csv", 
                  header=TRUE, 
                  skip=0, 
                  sep=","
)


## Estatisticas Descritivas IDH - Raw
summary(idh_raw)

## Deixando somente os dados válidos

### Transformando os 0 em NA
idh_raw[idh_raw==0] <- NA
idh_raw
#View(idh)

### Removendo as linhas que possuem dados vazios
idh <-idh_raw[complete.cases(idh_raw),]

## Criar uma Matriz de Correlação
matcor <- cor(idh)
print(matcor, digits = 2)


## Grafico de Correlação 
corrplot(matcor, method="circle")

# Teste de Kaiser, para verificar se a Matriz de Correlação é Identica a matriz 
# Original

## Ho: A matriz de correlação da população é uma matriz identidade, 
## ou seja as variáveis não são correlacionadas na população 
## - Não é possível continuar

## H1: A matriz de correlação da população não é uma matriz identidade, 
## ou seja as variáveis são correlacionadas na população. 
## - É possivel continuar!

barlett <- cortest.bartlett(idh)
barlett

## $chisq <- Teste qui-quadrado
barlett$chisq

## $p.value <- P-Valor
barlett$p.value

## $df <- Graus de Liberdade
barlett$df


# Kaiser-Meyer-Olkin factor
## Valores da estatistica Kaiser maiores que 0.5 indicam que a análise fatorial
## Pode ser utilizada para analisar os dados
KMO(idh)


# Determinando o número mínimo de fatores que respondem pela 
# maxima variância dos dados

fit<-princomp(idh,cor=TRUE)
fit

## É possível reter apenas os fatores estatisticamente significativos 
## com base na significância estatística dos autovalores separados.

summary(fit)
screeplot(fit)

# Análise dos Componentes Principais

PCAdente<-principal(idh, nfactors=2,
                    n.obs=30,rotate="none", scores=TRUE)
PCAdente


PCAdentevarimax<-principal(idh, nfactors=2,
                           n.obs=30,rotate="varimax",scores=TRUE)
PCAdentevarimax


# Gerando Scores

factor.scores(idh,PCAdentevarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)
