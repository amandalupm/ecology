# Tutorial da aula 7
# Análises exploratória de dados



# Carregando os dados existentes no R
data("anscombe")
 
# Funções básicas para checar os dados
dim(anscombe) # dimensao dos dados, N de linhas e N de colunas (11 e 8)
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto a classe é = (data.frame)
str(anscombe) # estrutura do objeto

# Fazendo as médias
mean(anscombe$x1) #= 9
mean(anscombe$x2) #= 9
mean(anscombe$x3) #= 9
mean(anscombe$x4) #= 9

# o mesmo calculo, agora apenas em 1 linha de comando
## media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
# Resultado é 9 para todos

## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)
# Resultado y1 = 7.5, y2= 7.5, y3= 7.5 e y4= 7.5

# variância dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto
# Resultado 
#x1        x2        x3        x4        y1        y2        y3        y4 
#11.000000 11.000000 11.000000 11.000000  4.127269  4.127629  4.122620  4.123249 

# correlação
cor(anscombe$x1, anscombe$y1) # = 0.81
cor(anscombe$x2, anscombe$y2) # = 0.81
cor(anscombe$x3, anscombe$y3) # = 0.81
cor(anscombe$x4, anscombe$y4) # = 0.81
# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

anscombe
# Apenas até o X3 apresenta os mesmos valores, o x4 é diferente de todos como tbm o de y

# Para saber o quão diferente são os números
# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L 
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna


# Padrões morfológicos de espécies "Iris"
data("iris")
head(iris)
summary(iris)

# Tem a média de cada medida como altura, largura e assim como as suas respectivas espécies

table(iris$Species) # = Há 50 indivíduos (informações) por espécie

# Para saber a média de cada espécie
# media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# O resultado foi para setosa = 5 para versicolor = 5.9 e para virginica = 6.6

# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

#Para outras variáveis
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)

# Calcuando o desvio padrão das variáveis
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# uma solução de como calular a média por espécie de todas as variáveis

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){medias[i,] <- tapply(iris[,i], iris$Species, mean)}
medias # Para visualizar os dados com as suas respectivas médias para cada variável
 
vars <- iris[, -5]
apply(vars, 2, mean)
# Resultado Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#             5.843333     3.057333     3.758000     1.199333
# Mediana
apply(vars, 2, median)

# Moda
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1] # 5 e 10

# Medidas de dispersão
# Variância
apply(vars, 2, var)

#Desvio Padrão (Raiz quadrada da variância)
sd01 <- apply(vars, 2, sd)
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02 # mesmo valor, está true pq são iguais os valores

# Calculando o coeficiente de variação (criando função)
cv <- function(x){sd(x)/mean(x)*100}
apply(vars, 2, cv) #Resultado abaixo
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#  14.17113     14.25642     46.97441     63.55511

# Quantis
# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){diff(range(x))}
apply(vars, 2, my_range)

#O IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).
apply(vars, 2, IQR)

# Matriz de correlação
cor(vars)
# criando gráficos

barplot(table(iris$Species)) # todas as espécies tem o mesmo número de observações.
par(mfrow=c(2, 2))

# Histograma padrão para os dados das espécies de Iris
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 1))
# comprimento da sépala das espécies de Iris
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))
# Curva de densidade

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE) # Com a densidade probabilistica

par(mfrow=c(1, 2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="pink") # note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1, 1))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#Agora vamos olhar para os valores por espécie.
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

#Identificando os outilers
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]
boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 & 
       iris$Species == "setosa", 
     c("Sepal.Width", "Species")]
# Vamos olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"], 
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"], 
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"], 
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow=c(1,1))

pairs(vars)
# EXEPCIONALMENTE vamos carregar o pacote agora, já que esse é um exercício bonus. 
library("GGally")
ggpairs(vars)
