#' ---
#' title: "Métodos de ordenação: Coordenadas Principais"
#' subtitle: "Espécie de plantas na Reserva Natural de Steneryd"
#' author: "Werlleson Nascimento e Carlos Tadeu dos Santos Dias"
#' modif: "Gabriel Passos"
#' date: "11/07/2024"
#' output: html_document
#' ---


#' # Carregar pacotes
library(vegan)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(GGally)
library(ggrepel)


#' # Carregar dados
plantas <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)
plantas1 <- data.frame(plantas[,2:18])
dados <- data.frame(t(data.frame(plantas1)))
colnames(dados) <- dados[,1]
rownames(dados) <- as.character(1:17)
dados


#' # Criando a matriz de distâncias
Dist.manhattan <- dist(dados, method="manhattan")


#' # Coordenadas Principais
pcoa.dados <- cmdscale(Dist.manhattan, eig = TRUE, x.ret=TRUE)
pcoa.dados


#' # Autovalores não nulos
sum.eigpcoa <- sum(pcoa.dados$eig[pcoa.dados$eig >=0])
perc.var <- (pcoa.dados$eig[pcoa.dados$eig >= 0]/sum.eigpcoa)*100
matlambdas <- cbind(1:length(which(pcoa.dados$eig >= 0)),pcoa.dados$eig[pcoa.dados$eig >= 0],perc.var)
colnames(matlambdas) <- c("Coordinate","Non negative eigenvalue","% of Total")
(mateigen.csv <- round(matlambdas,1)) # AUtovalores não nulos

#write.csv(mateigen.csv,"mateigen.csv",row.names=FALSE)


#' # Gráfico de Draftsman
plot <- c(1:nrow(dados))
coordenadas <- pcoa.dados$points[,1:2] 
mat.plot <- data.frame(plot, coordenadas)
ggpairs(mat.plot, columns = 1:3)


#' # Gerando os gráficos
pcoa_data <- as.data.frame(pcoa.dados$points)
pcoa_data$labels <- rownames(dados)

ggplot(pcoa_data, aes(x = V1, y = V2, label = labels)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  geom_text_repel(size = 3, box.padding = 0.5) +
  labs(x = "PCO1", y = "PCO2", 
       title = "Principal Coordinates Analysis.\nPlants on 17 plots in dados Nature Reserve") +
  theme_test()


#' Ambos componentes mostram uma relação com o número do lote.