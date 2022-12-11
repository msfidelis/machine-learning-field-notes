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

# COLA DE CORRELAÇÕES QUANTITATIVAS

## A correlação é uma análise bivariada que mede a força de associação entre duas 
## variáveis e a direção da relação.  Em termos da força da relação, 
## o valor do coeficiente de correlação varia entre +1 e -1.  
## Um valor de ± 1 indica um grau de associação perfeito entre as duas variáveis.
## Como o valor do coeficiente de correlação vai para 0, a relação entre as duas 
## variáveis será mais fraca.


# TIPOS DE COEFIENTES DE CORRELAÇÃO 


# PEARSON - CORRELAÇÃO LINEAR

## Sejam duas variáveis X e Y, ambas quantitativas, preferencialmente contínuas. 
## A existência de relação linear entre essas variáveis pode ser detectada 
## com auxílio do Diagrama de Dispersão, mas, também, com auxílio do Coeficiente 
## de Correlação Linear de Pearson.

## A correlação de Pearson avalia a relação linear entre duas variáveis contínuas. 
## Uma relação é linear quando a mudança em uma variável é associada a uma 
## mudança proporcional na outra variável.

## Por exemplo, você poderia usar uma correlação de Pearson para avaliar se 
## aumentos na temperatura da instalação de produção estão associados a uma 
## redução da espessura da cobertura de chocolate.

## Utilizado quando: Temos normalidade dos dados, e eles são LINEARES



# SPEARMAN - CORRELAÇÃO MONOTÔNICA

## A correlação de Spearman avalia a relação monotônica entre duas variáveis 
## contínuas ou ordinais. Em uma relação monotônica, as variáveis tendem a mudar 
## juntas mas não necessariamente a uma taxa constante. O coeficiente de 
## correlação de Spearman baseia-se nos valores classificados de cada variável, 
## em vez de os dados brutos.

## A correlação de Spearman é muito usada para avaliar relações envolvendo 
## variáveis ordinais. Por exemplo, você poderia usar a correlação de Spearman 
## para avaliar se a ordem na qual os funcionários executam um teste está 
## relacionada ao número de meses de emprego.
## Usado para amoastras maiores



# KENDAL - CORRELAÇÃO ORDINAL

## O coeficiente de correlação Tau de Kendall serve para verificar se existe 
## correlação entre duas variáveis ordinais. É um método adequado quando 
## amostras têm tamanhos reduzidos, pois o método é mais preciso. 
## E pode ser estendido a correlações parciais, quando o efeito de uma 
## terceira variável, que age sobre X e Y, é retirado antes de determinar 
## se X e Y estão relacionadas.
## Coeficiente de Kendall é, muitas vezes, interpretado como uma medida de 
## concordância entre dois conjuntos de classificações relativas a um conjunto 
## de objetos de estudo.

## Parecido com Spearman, mas utilizado para amostras menores

## Os números ordinais representam posições em uma determinada sequência, 
## como: primeiro, segundo, décimo quinto, entre outros.


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

# TESTANDO CORRELAÇÕES

# TESTANDO SE AS CORRELAÇÕES SÃO SIGNIFICATIVAS

## Pearson
cor.test(
  dataset_correlacao$ipca, 
  dataset_correlacao$inpc, 
  method = "pearson"
)

cor.test(
  idh_comparacao$expectativa_de_vida, 
  idh_comparacao$expectativa_de_anos_escola, 
  method = "spearman"
)

## Spearman
cor.test(
  dataset_correlacao$ipca, 
  dataset_correlacao$inpc, 
  method = "spearman"
)

cor.test(
  idh_comparacao$expectativa_de_vida, 
  idh_comparacao$expectativa_de_anos_escola, 
  method = "spearman"
)

## Kendall
cor.test(
  dataset_correlacao$ipca, 
  dataset_correlacao$inpc, 
  method = "kendall"
)

cor.test(
  idh_comparacao$expectativa_de_vida, 
  idh_comparacao$expectativa_de_anos_escola, 
  method = "kendall"
)


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
rcorr(as.matrix(dataset_correlacao[,2:3]), type="spearman")

## Correlação de todas as variáveis
## inpc ipca
## inpc 1.00 0.97
## ipca 0.97 1.00

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


# HISTOGRAMAS DE DISTRIBUIÇÃO 

histogram=function(x){
  hist(x,prob=T)
  lines(density(x),col="red")
  curve(dnorm(x,mean(x), sd(x)),add=T,col="blue")
}

histogram(dataset_correlacao$ipca)
histogram(dataset_correlacao$inpc)

# VERIFICAÇÃO DE NORMALIDADES - QQPLOT

qq = function(x){
  qqnorm(x,main = "", xlab = "Quantis teóricos N(0,1)", pch = 20)
  qqline(x, lty = 1, col = "red")
}

qq(dataset_correlacao$ipca)
qq(dataset_correlacao$inpc)

qq(idh$idh)
qq(idh$idh_masculino)
qq(idh$idh_feminino)
qq(idh$expectativa_de_vida)
qq(idh$expectativa_de_vida_masculina)
qq(idh$expectativa_de_vida_feminina)


