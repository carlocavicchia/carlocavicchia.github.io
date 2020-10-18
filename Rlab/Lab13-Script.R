
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#                    Lab13 ** Clustering **                               # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

data("USArrests") #carichiamo i dati USArrests presenti su R
df <- scale(USArrests) #standardizziamo i dati 

head(df) 
pairs(df)

km2 <- kmeans(df, 2, iter.max = 10, nstart = 10) #kmeans con 2 clusters
km3 <- kmeans(df, 3, iter.max = 10, nstart = 10) #kmeans con 3 clusters
km4 <- kmeans(df, 4, iter.max = 10, nstart = 10) #kmeans con 4 clusters
km5 <- kmeans(df, 5, iter.max = 10, nstart = 10) #kmeans con 5 clusters
km6 <- kmeans(df, 6, iter.max = 10, nstart = 10) #kmeans con 6 clusters
km7 <- kmeans(df, 7, iter.max = 10, nstart = 10) #kmeans con 7 clusters
km8 <- kmeans(df, 8, iter.max = 10, nstart = 10) #kmeans con 8 clusters

plot(c(2,3,4,5,6,7,8), 
     c(km2$tot.withinss,km3$tot.withinss,km4$tot.withinss,km5$tot.withinss,km6$tot.withinss,km7$tot.withinss,km8$tot.withinss),
     xlab="Number of clusters",
     ylab="Total of within deviance",
     col="black"
) 

lines(c(2,3,4,5,6,7,8),
      c(km2$tot.withinss,km3$tot.withinss,km4$tot.withinss,km5$tot.withinss,km6$tot.withinss,km7$tot.withinss,km8$tot.withinss),
      col="red")

points(4,km4$tot.withins,
       pch=20,
       col = "red"
       ) #selezioniamo il numero di clusters per cui la funzione si inizia a stabilizzare

km.opt <- kmeans(df, 4, iter.max = 10, nstart = 10)

#osserviamo alcuni attributi dell'oggetto km.opt
km.opt$cluster
km.opt$centers
km.opt$withinss
km.opt$tot.withinss
km.opt$size

print(km.opt) #stampiamo i risultati 

aggregate(USArrests, by=list(cluster=km.opt$cluster), mean) #calcolimao le medie condizionate sul dataset originale (non standardizzato)

dd <- cbind(USArrests, cluster = km.opt$cluster) #aggiungiamo il vettore di classificazione al dataset originale 

#plottiamo i punti sulle prime due componenti principali

library(factoextra)
fviz_cluster(km.opt,df) 

