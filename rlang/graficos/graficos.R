

##### Colinha para gráficos fodas em R

pacotes <- c(
  "plotly",
  "plyr",
  "data.frame",
  "ggplot2",
  "ggthemes"
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
ggplot(freq_table_graph, aes(x=Freq, y=orgaos)) + 
  geom_bar(stat='identity') +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos - 2022-09") +
  geom_text(aes(label=Freq)) 

## Labels dos eixos
ggplot(freq_table_graph, aes(x=Freq, y=orgaos)) + 
  geom_bar(stat='identity') +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos - 2022-09") +
  labs(x = "Quantidade de Corridas", y = "Orgãos", fill = "Orgãos:") +
  geom_text(aes(label=Freq)) 

## Usando Temas

### Global
theme_set(theme_calc())

### Diretamente no plot
ggplot(freq_table_graph, aes(x=Freq, y=orgaos)) + 
  geom_bar(stat='identity') +
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos - 2022-09") +
  geom_text(aes(label=Freq)) +
  theme_calc()
  # theme_void()
  # theme_foundation()
  # theme_fivethirtyeight()
  # theme_gray()
  # theme_economist_white()
  # theme_economist()
  # theme_few()
  # theme_excel_new()
  # theme_excel()
  # theme_base()
  # theme_classic()
  # theme_dark()
  # theme_bw()
  # theme_clean()

## Customizando estilo das barras

## Adicionando labels descritivas e cores baseados nas observações
ggplot(freq_table_graph, aes(x=Freq, y=orgaos)) + 
  geom_bar(stat='identity', aes(fill=orgaos)) + # aes(fill=orgaos) determina as cores
  ggtitle("Utilização de TaxiGov dos Orgãos Públicos - 2022-09") +
  labs(x = "Quantidade de Corridas", y = "Orgãos", fill = "Orgãos:") +
  geom_text(aes(label=Freq)) +
  scale_color_discrete("Corridas:")


##### Grafico de Pizza - Count de Ocorrências

## Grafico simples

pie(freq_table_graph$Freq, labels = freq_table_graph$orgaos)

## Grafico colorido

pie(
  freq_table_graph$Freq, 
  main = "Utilização de TaxiGov dos Orgãos Públicos",
  labels = freq_table_graph$orgaos, 
  col = rainbow(length(freq_table_graph$orgaos))
)

## Utilizando a porcentagem

freq_table_graph_pie <- freq_table_graph

## Criando um campo de porcentagem na tabela de frequencia na mão
freq_table_graph_pie$percent = round(
  100 * freq_table_graph_pie$Freq / sum(freq_table_graph_pie$Freq),
  digits = 0
)

freq_table_graph_pie

## Verificando a soma 100
sum(freq_table_graph_pie$percent)

## Criando a label

freq_table_graph_pie$label = paste(
  freq_table_graph_pie$orgaos, 
  " (",
  freq_table_graph_pie$percent, 
  "%", 
  ")",
  sep = ""
)

freq_table_graph_pie

pie(
  freq_table_graph_pie$Freq, 
  main = "Utilização de TaxiGov dos Orgãos Públicos",
  labels = freq_table_graph_pie$label, 
  col = rainbow(length(freq_table_graph_pie$orgaos))
)

## Utilizando via ggplot2
ggplot(freq_table_graph_pie, aes(x="", y=Freq, fill=orgaos)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(percent, "%", sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Utilização de TaxiGov dos Orgãos Públicos") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

