# Script para ler a tabela
# Os dados originais estão em xlsx 

dir()
dados<-read.table("exercicio_planilha.txt", h=T)
dados 
attach(dados)
summary(dados)

