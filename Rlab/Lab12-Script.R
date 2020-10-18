
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#          Lab12 ** Analisi in Componenti Principali **                   # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

dati_raw <- read.csv("dati.csv", header=T) # carichiamo il nostro file .csv

dati <- dati_raw[,-1]
rownames(dati) <- dati_raw[,1] # uso la prima colonna come etichette per le unità (righe)

last.var = dati[,8] #estrapolo la variabile 8 e la metto in un vettore chiamato last.var
dati = dati[,-8]

dati$hurdles = -dati$hurdles # sostituisco la variabile hurdles con se stessa con il meno davanti
dati$run200m = -dati$run200m # sostituisco la variabile run200m con se stessa con il meno davanti
dati$run800m = -dati$run800m # sostituisco la variabile run800m con se stessa con il meno davanti

summary(dati) # produco gli indici descrittivi (min, mediana, media, max, quartili)

dati.standardizzati = scale(dati) #center = TRUE e scale = TRUE per default
cor(dati)
cor(dati.standardizzati) # la correlazione delle variabili e delle variabili standardizzate è la stessa

mod.pca = prcomp(dati.standardizzati, center=F, scale=F) # acp 
names(mod.pca) # vediamo l'oggetto prodotto da prcomp che attributi ha
summary(mod.pca) # produce l'importanza di ogni componente

mod.pca$rotation # loadings delle componenti
mod.pca$x # score delle componenti
mod.pca$sdev # deviazione standard delle componenti

cor(mod.pca$x,dati.standardizzati) #la correlazione tra le variabili e le componenti

lambdas = mod.pca$sdev^2/sum(mod.pca$sdev^2) #la varianza spiegata da ogni componente in %
sum(lambdas) # check ==1

barplot(lambdas, xlab="Componente",col="blue",names=c("C1","C2","C3","C4","C5","C6","C7"),
        ylab="Varianza Spiegata",main="Quota di Varianza per componente")
barplot(mod.pca$sdev, xlab="Componente",col="red",names=c("C1","C2","C3","C4","C5","C6","C7"),
        ylab="Deviazione standard",main="Deviazione standard per componente")

biplot(mod.pca,col=c("red","blue"))

var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
} # vediamo come si scrive una funzione in R

var.coord <- var.cor <- t(apply(mod.pca$rotation, 1, var_cor_func, mod.pca$sdev)) #coordinate per il cerchio delle correlazioni 

a <- seq(0, 2*pi, length = 100)
plot(cos(a), sin(a), type = 'l', col="gray",
      xlab = "PC1",  ylab = "PC2")
abline(h = 0, v = 0, lty = 2)

arrows(0, 0, var.coord[, 1], var.coord[, 2], 
       length = 0.1, angle = 15, code = 2) # qui ho fatto la 1 e la 2

# se volessi fare 1-3
#arrows(0, 0, var.coord[, 1], var.coord[, 3], length = 0.1, angle = 15, code = 2)

# Aggiungiamo le etichette
text(var.coord, labels=rownames(var.coord), cex = .7, adj=1)

# o anche:
library(factoextra)
fviz_pca_var(mod.pca)

# relazione tra score e la prima componente
cor(mod.pca$x[,1],last.var)

#-----------------------------------------------------------------------------------------------
x <- iris[,-5] #la quinta colonna di iris è una variabile categoriale che ci dice la specie del fiore

scale.x <- scale(x,center = TRUE,scale=TRUE) 
corr.x <- cor(x) 
corr.sx <- cor(scale.x) 

mod.pca <- prcomp(scale.x) 
summary(mod.pca)
mod.pca$rotation 
mod.pca$x 

cor(scale.x,mod.pca$x) 

biplot(mod.pca,col=c("red","blue"))

lambdas = mod.pca$sdev^2/sum(mod.pca$sdev^2)
barplot(lambdas, xlab="Componente",col="blue",names=c("C1","C2","C3","C4"),
        ylab="Varianza Spiegata",main="Quota di Varianza per componente")
