# Tutoria do scrip aula 8

# carregando arquivos

comm <- read.csv("data/cestes/comm.csv")
envir <- read.csv("data/cestes/envir.csv")

summary(comm)
summary(envir)

#Extraido a riqueza por site
comm.pa <- comm[, -1] > 0
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
head(rich)
## [1] 6 4 3 6 3 3
summary(rich)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   4.000   6.000   6.268   8.000  15.000

# Observando os dados
boxplot(rich)
localidades <- cbind(envir, rich)
head(localidades)

# Gráfico de dispersão, plot com abline do modelo de regressão

# criando modelos lineares
riqsilt <- lm(rich ~ Silt, data = localidades)
riqclay <- lm(rich ~ Clay, data = localidades)
riqsand <- lm(rich ~ Sand, data = localidades)

# extraindo os coeficientes do modelo
coef_s <- coef(riqsilt)
coef_c <- coef(riqclay)
coef_d <- coef(riqsand)

# definindo os limites dos eixos
limy <- c(min(localidades$rich),
          max(localidades$rich))
limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
          max(localidades[,c("Clay", "Sand", "Silt")]))

## definindo o nome do eixo y
laby <- "Riqueza de espécies"

# construindo o gráfico
# define parametros graficos

par(mfrow = c(1, 3),
    las = 1,
    bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades, 
     col = "tomato", 
     ylim = limy, xlim = limx, 
     ylab = laby, 
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_s[1], b = coef_s[2], 
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "", 
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "", 
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)

# Exportando o gráfico com as funções png() e dev.off()

# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("figs/figura01.png", res = 300, width = 2400, height = 1200)
# define parametros graficos

par(mfrow = c(1, 3),
    las = 1,
    bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades, 
     col = "tomato", 
     ylim = limy, xlim = limx, 
     ylab = laby, 
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_s[1], b = coef_s[2], 
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "", 
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "", 
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x 
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()
## png 
##   2

#boxplot

# criando vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")
# criando vetor com o nome das espécies
sp <- paste("I.", unique(iris$Species), sep = " ")
par(mfrow = c(2, 2),
    mar = c(4, 1, 1, 1), 
    bty = 'l',
    las = 1)
boxplot(Sepal.Length ~ Species,
        data = iris,
        xlab = "",
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Sepal.Width ~ Species, 
        data = iris, 
        xlab = "", 
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Length ~ Species, data = iris,  col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Width ~ Species,
        data = iris,
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)

# Gráfico de média com desvio padrão com arrows
# fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
set.seed(42)
# criando um data frame com valores medios e desvio padrão de uma variável
d2 <- data.frame(name = letters[1:5],
                 value = sample(seq(4, 15), 5),
                 sd = c(1, 0.2, 3, 2, 4))
plot(x = 1:5, d2$value, las = 1, bty = 'l', 
     ylim = c(0, 18), pch = 19, xaxt = 'n',
     xlab = "names", ylab = "value")
axis(1, at = 1:5, labels = d2$name)
arrows(x0 = 1:5,
       y0 = d2$value + d2$sd,
       y1 = d2$value - d2$sd, angle = 90, length = 0.05, code = 3)
