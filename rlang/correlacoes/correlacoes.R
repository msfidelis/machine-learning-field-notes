pacotes <- c("Hmisc", "corrgram") 

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


## Abrindo o arquivo inicial para efetuar análise
idh_raw <- read.table("idh.csv",
                      header=TRUE,
                      skip=0,
                      sep=","
)
idh_raw

ipca_raw <- read.table("ipca_recente.csv",
                       header=TRUE,
                       skip=0,
                       sep=","
)
ipca_raw

inpc_raw <- read.table("inpc_recente.csv",
                       header=TRUE,
                       skip=0,
                       sep=","
)
inpc_raw

# Unindo os Datasets de IPCA E INPC

dataset_correlacao <- as.data.frame(ipca_raw$mes_ano)


dataset_correlacao$inpc <- inpc_raw$variacao_mes
dataset_correlacao$ipca <- ipca_raw$variacao_mes
dataset_correlacao
  


# Correlação Simples
  
# corr - KENDALL  
dataset_correlacao[,2:3]
cor(dataset_correlacao[,2:3], use="all.obs", method="kendall")  
cor(dataset_correlacao[,2:3], use="complete.obs", method="kendall")  
cor(dataset_correlacao[,2:3], use="pairwise.complete.obs", method="kendall")  

# corr - SPEARMAN  
dataset_correlacao[,2:3]
cor(dataset_correlacao[,2:3], use="all.obs", method="spearman")  
cor(dataset_correlacao[,2:3], use="complete.obs", method="spearman")  
cor(dataset_correlacao[,2:3], use="pairwise.complete.obs", method="spearman") 

# corr - PEARSON  
dataset_correlacao[,2:3]
cor(dataset_correlacao[,2:3], use="all.obs", method="pearson")  
cor(dataset_correlacao[,2:3], use="complete.obs", method="pearson")  
cor(dataset_correlacao[,2:3], use="pairwise.complete.obs", method="pearson") 


# rcorr - Correlação com Níveis de Significancia

# CORRELAÇÃO DE PEARSON 

## Correlação de todas as variáveis
rcorr(as.matrix(dataset_correlacao[,2:3]), type="pearson")

## Correlação entre dois tipos de variáveis
x <- dataset_correlacao$ipca
y <- dataset_correlacao$inpc

pearson_inflacao <- rcorr(x, y, type="pearson")

## Matriz de Correlação
pearson_inflacao$r

## P-Valor
pearson_inflacao$P

## Significancia
pearson_inflacao$n


# CORRELAÇÃO DE SPEARMAN 

## Correlação de todas as variáveis
rcorr(as.matrix(dataset_correlacao[,2:3]), type="spearman")

## Correlação entre dois tipos de variáveis
x <- dataset_correlacao$ipca
y <- dataset_correlacao$inpc

spearman_inflacao <- rcorr(x, y, type="spearman")

## Matriz de Correlação
spearman_inflacao$r

## P-Valor
spearman_inflacao$P

## Significancia
spearman_inflacao$n

# CORRELAÇÃO IDH

### Transformando os 0 em NA
idh_raw[idh_raw==0] <- NA
idh_raw
#View(idh)

### Removendo as linhas que possuem dados vazios
idh <-idh_raw[complete.cases(idh_raw),]

## Correlação entre IDH e expectativa de vida

idh_comparacao <- as.data.frame(idh$ano_referencia)
idh_comparacao
idh_comparacao$idh <- idh$idh
idh_comparacao$expectativa_de_vida <- idh$expectativa_de_vida

## Comparacao
rcorr(as.matrix(idh_comparacao[,2:3]), type="pearson")

## Correlação entre Anos na Escola e Expectativa de Vida
idh_comparacao <- as.data.frame(idh$ano_referencia)
idh_comparacao
idh_comparacao$expectativa_de_anos_escola <- idh$expectativa_de_anos_escola
idh_comparacao$expectativa_de_vida <- idh$expectativa_de_vida

## Comparacao
rcorr(as.matrix(idh_comparacao[,2:3]), type="pearson")


# GRAFICOS DE CORRELACAO

corrgram(idh, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlação entre INPC e IPCA")

