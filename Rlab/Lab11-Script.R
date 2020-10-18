
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#          Lab11 ** Modello di Regressione Lineare Multipla **            # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

install.packages("datarium")
library(datarium)
x <- marketing

head(x)
pairs(x)

model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)

#La prima cosa da fare per intepretare i risultati della funzione "lm", è quella di controllore il valore della statistica F e il suo p-value. Nel nostro esempio il p-value è < 2e-16, il che significa che c'è molta significatività. Quindi almeno un predittore è risultato essere significativo. 

#studiamo i coefficienti
summary(model)$coefficient

#la statistica t valuta se esiste un'associazione significativa tra ciascun predittore e la variabile dipendente. Cioè se il valore corrispondente in beta è significativamente diverso da 0. Nel nostro esempio il coefficiente relativo al predittore "newspaper" è risultato non essere statisticamente significativo. 

#per capire bene il significato di beta: spendendo 1000 dollari in pubblicità su facebook porterebbe ad un incremento in termini di vendite approsimativamente uguale a 0.1885*1000 = 189 unità in media. 

#quindi il modello corrispondente risulterebbe: sales = 3.5 + 0.045*youtube + 0.187*facebook.

#-----------------------------------
y <- iris
model <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
summary(model)


