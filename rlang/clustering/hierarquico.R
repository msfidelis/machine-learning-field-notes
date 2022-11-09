##### Teste de Clustering de Dados - Método Hierarquico Aglomerativo

# Importanto as libs do laboratório

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##### Carregando o Dataset
paises <- read.csv("dados_paises.csv", sep = ",", dec = ".")
paises

paises_original <- paises
paises_cluster <- paises


##### Selecionando apenas as colunas importantes
paises_cluster <- select(
  paises_cluster, 
  child_mort, 
  exports, 
  health, 
  imports, 
  income, 
  inflation, 
  life_expec, 
  total_fer,
  gdpp
)

paises_cluster

##### Padronizando os dados utilizando Z-Score

paises_cluster <- scale(paises_cluster)
paises_cluster

####### Transformando a escala para um dataframe
paises_cluster <- as.data.frame(paises_cluster)
paises_cluster

######## Voltando os Rownames (nome dos paises) para o dataset
rownames(paises_cluster) <- paises$country
paises_cluster


######## Teste do Z-Score; 
######## Todas as variáveis passam a ter média 0 e desvio padrão 1

round(mean(paises_cluster$exports),3)
round(sd(paises_cluster$exports))

round(mean(paises_cluster$gdpp),3)
round(sd(paises_cluster$gdpp))

paises_cluster

# Gera uma Matriz de dissimilaridades com as distâncias entre as variáveis
# utilizando o método euclidiano
matriz_D <- paises_cluster %>% 
  dist(method = "euclidean")

matriz_D

# Clusterização hierarquica utilizando single linkage.
# Busca por menores distâncias entre as observações. 
# Recomendado em caso de observações muito diferentes. 

cluster_single_linkage <- agnes(x = matriz_D, method = "single")
cluster_single_linkage

# Clusterização hierarquica utilizando complete linkage.
# Busca por maiores distâncias entre as observações. 
# Recomendado em caso de observações muito parecidas 

cluster_complete_linkage <- agnes(x = matriz_D, method = "complete")
cluster_complete_linkage

# Clusterização hierarquica utilizando average linkage.
# Busca pela distâncias média entre as observações. 
# Não entendi quando usar essa porra 

cluster_average_linkage <- agnes(x = matriz_D, method = "average")
cluster_average_linkage

# Análise do Dendograma do método Single Linkage sobre as observações

# Análise: o encadeamento por single linkage não permite uma boa performance 
# e fidelidade para a clusterização nas observações, pois a mesma apresenta 
# valores muito proximos uns dos outros, fazendo com que a melhor alterantiva s
# seja a complete linkage por exemplo

dev.off()
fviz_dend(x = cluster_single_linkage, show_labels = F)

# Análise do Dendograma do método Complete Linkage sobre as observações

# Análise: O complete linkage melhora significamente a vizualização dos clusters

dev.off()
fviz_dend(x = cluster_complete_linkage, show_labels = F)

# Análise do Dendograma do método Average Linkage sobre as observações

# Análise: Mesmo se aproximando, o complete linkage fornece uma melhor observação
# dos clusters criados, e também criando menos clusters por consequencia
dev.off()
fviz_dend(x = cluster_complete_linkage, show_labels = F)

# Criando os coeficientes do cluster selecionado
coeficientes <- sort(cluster_complete_linkage$height, decreasing = FALSE) 
schema <- as.data.frame(cbind(cluster_complete_linkage$merge, coeficientes))

names(schema) <- c("Cluster1", "Cluster2", "Coeficientes")
schema

# Adicionando o cluster na tabela de observações
paises_original$cluster_H <- factor(cutree(tree = cluster_complete_linkage, k = 6))
paises_original

# Análisendo os resultados de um cluster especifico; no caso cluster 2
cluster_teste <- subset(paises_original, cluster_H == 2)
cluster_teste
