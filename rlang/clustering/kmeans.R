##### Teste de Clustering de Dados - Kmeans

# Exemplo 1

# O exemplo é encontrar uma familiariade entre alunos que possuem uma correlação
# entre notas de Matemática e Física 

# Importanto as libs do laboratório

pacotes <- c(
             "cluster", 
             "factoextra"
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


##### Carregando o Dataset
load(file = "Vestibular.RData")
Vestibular
View(Vestibular)

base_original <- Vestibular
base_cluster <- Vestibular

###### Transformando todas as variáveis para a mesma unidade de medida
base_cluster_vars <- scale(base_cluster[,2:3])
base_cluster_vars

###### Transformando em Dataframe para análise 
base_cluster_vars <- as.data.frame(base_cluster_vars)
base_cluster_vars


##### Buscando o numero ideal de Clusters para a amostra
fviz_nbclust(base_cluster_vars, kmeans, method = "gap_stat")

##### Criando os clusters 
clus <- kmeans(base_cluster_vars, 3)

##### Visualização
fviz_cluster(clus, data = base_cluster_vars)


##### Combinação da informação do cluster na base original
base_original <- cbind(base_original, clus$cluster)
base_original

names(base_original)[5] <- 'cluster'
base_original 

##### Criando um dataaset com os melhores alunos de exatas

melhores_alunos_exatas <- subset(base_original, cluster == "1")
melhores_alunos_exatas 


##### Criando um dataaset com os piiores alunos de exatas

piores_alunos_exatas <- subset(base_original, cluster == "3")
piores_alunos_exatas 
