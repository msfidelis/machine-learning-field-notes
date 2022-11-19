

##### Colinha para gráficos fodas em R

pacotes <- c(
  "plotly",
  "plyr",
  "data.frame"
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


corridas <- read.csv("taxigov-corridas-2022-09.csv",
                       sep = ",",
                       dec = ".")

corridas
View(corridas)

##### Grafico de Barras - Count de Ocorrências

ggplot(data = corridas, aes(x=nome_orgao)) + 
  ggtitle("Utilização de Taxi dos Orgãos Públicos") +
  geom_bar() 

##### Ordenando os resultados

## Selecionando apenas as viagens concluidas

concluidas <- subset(corridas, status_corrida == "CONCLUÍDA")
concluidas

## Fazendo o sort 

## Criando a tabela de frequencias das variáveis qualitativas
orgaos <- concluidas$nome_orgao
freq_table <- table(orgaos)
freq_table

## Ordenando as tabelas maior valor
freq_table <- sort(freq_table)

## Criando o data frame
freq_table <- as.data.frame(freq_table)
freq_table

ggplot(freq_table, aes(x = Freq, y = orgaos)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos") +
  theme_classic()

## Selecionando somente os top values

orgaos <- concluidas$nome_orgao
freq_table <- table(orgaos)
freq_table

## Ordenando as tabelas maior valor
freq_table <- sort(freq_table, decreasing = TRUE)
freq_table

## Criando o data frame
freq_table <- as.data.frame(freq_table)
freq_table

freq_table_graph <- head(freq_table, n = 15)
freq_table_graph

## Criando o grafico de top values

ggplot(freq_table_graph, aes(x = Freq, y = orgaos)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos") +
  theme_classic()

## Alterando o eixo do grafico

ggplot(freq_table_graph, aes(y = orgaos, x = Freq)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos") +
  theme_classic()

## Adicionando valores ao grafico
ggplot(freq_table_graph, aes(y = orgaos, x = Freq), position = "fill") + 
  geom_bar(color = "black", fill = "grey") +
  geom_text(aes(label=orgaos), vjust=0) +
  
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos") 
