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
idh <- read.table("idh.csv", 
                   header=TRUE, 
                   skip=0, 
                   sep=","
)

#View(idh)

# Deixando somente os dados válidos

### Transformando os 0 em NA
idh[idh==0] <- NA
idh
#View(idh)

### Removendo as linhas que possuem dados vazios
idh_validos <-idh[complete.cases(idh),]

#View(idh_validos)

### Criando estatísticas descritivas
summary(idh_validos)


# Criando a Matriz de Correlação dos Dados

### Vetor sem a coluna do ano do IDH
idh_analise <- idh_validos[,2:10]
#View(idh_analise)

### Gerando os Coeficientes de Correlação para Cada Par de Variáveis

## Correlação de Pearson
rho <- rcorr(as.matrix(idh_analise), type="pearson")

## Isolando a Variável de Matriz de Correlação
matriz_correlacao <- rho$r
matriz_correlacao
#View(matriz_correlacao)

## Isolando a Variável do P-Valor dos Coeficientes
p_valores <- round(rho$P, 5)
p_valores

## Grafico de Correlações
chart.Correlation(idh_analise, histogram = TRUE, pch = "+")
corrplot(as.matrix(matriz_correlacao), method="circle")
# corrgram(idh_analise, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

## Teste de Esferecidade de Bartlett
cortest.bartlett(idh_analise)

## Criando os Fatoriais
fatorial <- principal(idh_analise,
                      nfactors = length(idh_analise),
                      rotate = "none",
                      scores = TRUE)
fatorial

autovalores <- round(fatorial$values, 5)
autovalores
round(sum(autovalores), 2)


variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>%
  slice(1:3)






