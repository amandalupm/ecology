# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#

# carregando os pacotes necessários
library("tidyr")
library("tidyverse")
?list.files
files.path <- list.files(path = "data/cestes", 
                         pattern = ".csv", 
                         full.names = TRUE)
files.path

comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

data <- sapply(files.path, read.csv)
?head
?summary
?dim

#summary = resumir os dados
#head = mostra do primeiro ao último dado do objeto
#dim = dimensoes do objeto

# Entendendo o objeto comm
head(comm)
dim(comm)
summary(comm)
# Tem 57 variáveis (colunas) com 97 linhas
# resutado do dim = [1] 97  57 (dimensao)
#summary mostra que tem um máximo de 97 sites

# Entendendo o objeto coord
head(coord)
dim(coord)
summary(coord)
# Tem colunas de sites, x e y
# 97 linhas e 3 colunas
# summary mostra o máximo de 97 sites

# Entendendo o objeto envir
head(envir)
dim(envir)
summary(envir)
# Tem 3 colunas e 56 linhas 
# neste já mostra que existem outras linhas segundo o summary

# Entendendo o objeto splist
head(splist)
dim(splist)
summary(splist)
# Esta tamém tem dados com vários 0 e 1
# com 56 linhas e 15 colunas
# O summary mostra que existem 56 linhas e cada uma é responsável por cada espécie

# Entendendo o objeto traits
head(traits)
dim(traits)
summary(traits)
# 56 linhas e 15 colunas
# Cada especie por cada linha

#Temos dados de quantas espécies? Podemos simplesmente contar o número de linhas do objeto splist.
nrow(splist)
nrow(comm)
nrow(envir)
nrow(coord)
nrow(traits)

# O resultado  do splist foi 56
# O resultado  do comm foi 97
# O resultado  do envir foi 56
# O resultado  do coord foi 97
# O resultado  do traits foi 56

#Quantas áreas amostradas? Podemos contar o número de linhas dos objetos comm ou envir.
nrow(comm)
nrow(envir)
nrow(coord)
nrow(splist)
nrow(traits)

# O resultado do comm é 97
# O resultado do envir é 56
# O resultado do coord é 97
# O resultado do splist é 56
# O resultado do traits é 56

# Quantas variáveis ambientais?
# todas as variáveis exceto a primeira coluna com o id
names(envir)[-1]
names (comm) [-1]
names (coord) [-1]
names (splist) [-1]
names (traits) [-1]

# contando quantas variáveis
length(names(envir)[-1])
length(names(comm)[-1])
length(names(coord)[-1])
length(names(splist)[-1])
length(names(traits)[-1])

# Qual a riqueza de cada área? Primeiro, precisamos transformar 
# a nossa matriz que possui dados de abundância em uma matriz de presença e ausência.

comm.pa <- comm[, -1] > 0
# vamos nomear as linhas das planilhas com o id dos sites
row.names(comm.pa) <- envir$Sites

#No R, os valores de TRUE e FALSE contam como 1 e 0. Vamos calcular a riqueza da área 1
sum(comm.pa[1, ])

#Como podemos fazer a soma de forma automatizada para as 97 áreas? Podemos usar a
# função apply. Essa função aplica uma função às linhas ou colunas de um objeto
# (do tipo data.frame ou matrix).

rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

#Resultado
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   4.000   6.000   6.268   8.000  15.000

envir$Sites
summary(envir$Sites)

#Resultado do summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1      25      49      49      73      97 

#No R, a coluna Sites que representa uma variável categórica com o id de cada área
#está sendo entendida como uma variável numérica. Vamos converter essa coluna 
#para um fator, dessa forma irá representar melhor o significado da variável 
#que é simplesmente o id de cada área. Para isso, usamos a função factor()

# se checarmos a classe desse vetor, veremos que é numerica
class(envir$Sites)
# queremos que seja uma variável categórica. Para isso, convertemos em fator
as.factor(envir$Sites)
# se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)

#Vamos fazer o mesmo para a variável Sites do objeto coord
coord$Sites <- as.factor(coord$Sites)

# Juntando coord e envir

#Vamos então aplicar a função merge.
envir.coord <- merge(x = envir, 
                     y = coord, 
                     by = "Sites")
# Podemos checar a junção com as funções dim() e head(). 
# Quantas colunas deveríamos ter ao final? Quais colunas foram adicionadas?

dim(envir) # 97 e 9
dim(coord) # 97 e 3
dim(envir.coord) # 97 e 11
head(envir.coord) # tem várias colunas e linhas

#Transformando uma matrix espécie vs. área em uma tabela de dados

# queremos transformar a nossa matriz de espécie vs.área em uma planilha
# que contenha cada observação em uma linha e cada variável em uma coluna. 
# Cada observação é a abundância de uma espécie em uma determinada área. 
# Para fazer essa transformação iremos usar a função gather() do pacote tidyr.
# Como temos 97 sites e 56 espécies, terminaremos com um objeto com 5432 linhas (97 x 56).

# vetor contendo todos os Sites
Sites <- envir$Sites
length(Sites)

# vetor número de espécies
n.sp <- nrow(splist)
n.sp 

# criando tabela com cada especie em cada area especies em linhas 
comm.df <- tidyr::gather(comm[, -1])

#Vamos checar o cabeçalho e as dimensões do objeto.
dim(comm.df)
head(comm.df)     

#Queremos alterar o nome das colunas de comm.df. Para isso, usaremos a função colnames().
# nomes atuais
colnames(comm.df)
# modificando os nomes das colunas
colnames(comm.df) <-  c("TaxCode", "Abundance")
# checando os novos nomes
colnames(comm.df)

# Queremos agora adicionar a coluna Sites ao novo objeto. Vamos usar a função rep().
# Esta função cria sequências. Vamos criar uma sequência de localidades, 
# em que cada uma das 97 localidades se repete 56 vezes. A sequência deve ter
# também 5432 elementos.

# primeiro criamos a sequência
seq.site <- rep(Sites, times = n.sp)
# checando a dimensão
length(seq.site)
# adicionando ao objeto comm.df
comm.df$Sites <- seq.site
# checando como ficou
head(comm.df)

#Juntando todas as variáveis à comm.df
# Para terminar, vamos juntar splist, traits e envir.coord à planilha comm.df.

#Primeiro, vamos adicionar as informações das espécies contidas em splist à comm.df usando a coluna TaxCode.
comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)

# Antes de fazer a junção, precisamos mudar o nome para bater com o nome da coluna em comm.sp que é TaxCode.
names(traits)
# renomeando o primeiro elemento
colnames(traits)[1] <- "TaxCode"

comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)

#Finalmente, juntamos as variáveis ambientais (que aqui já contém as coordenadas) 
# à tabela geral da comunidade por meio da coluna Sites.
comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

#finalizamos nossa rotina de manipulação de dados exportando a planilha final modificada.

write.csv(x = comm.total, 
          file = "data/01_data_format_combined.csv", 
          row.names = FALSE)


