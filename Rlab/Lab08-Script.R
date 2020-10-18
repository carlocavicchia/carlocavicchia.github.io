
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#     Lab08 ** Tabelle doppie, Chi quadrato, Paradosso di Simpson **      #  
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

# Carichiamo il dataframe Nadal
load("Nadal.RData")
# che contiene le seguenti variabili:
#  * superficie: tipo di campo: "terra rossa", "altro"
#  * risultato: risultato del game "vittoria", "sconfitta"
#  * servizio: game in cui Nadal è al servizio: "si", "no"
str(Nadal)

# Consideriamo la tabella doppia:
table(Nadal$superficie, Nadal$risultato)

# per comodità
X = Nadal$superficie
Y = Nadal$risultato

tabella = table(X,Y)

# Rappresentazione grafica (1): mosaic plot
# quick&dirty

plot(tabella)
?mosaicplot
mosaicplot(tabella)
mosaicplot(tabella, color = terrain.colors(2),
           main = "Le performance di Nadal",
           xlab = "superficie",
           ylab = "risultato")

# ---> Distribuzioni Marginali di riga e di colonna:

addmargins(tabella)
marg.X = margin.table(tabella,1)
marg.Y = margin.table(tabella,2)
marg.X
marg.Y

# Rappresentazione grafica (2): barplot
# utile per il confronto (...con un po' di pazienza...)

barplot(tabella,
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Le performance di Nadal"
)

barplot(tabella,
        beside=T, 
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Le performance di Nadal"
)
# Mmmmh...ma cosa stiamo guardando in questo modo??
# Le frequenze assolute...

# Per effettuare un confronto magari è più utile fare riferimento alle:

# ---> Distribuzioni Condizionate:

#   della Y rispetto a (X = x_i)
addmargins(tabella)
prop.table(tabella,1) # (n_{ij} / n_{i.})
# in pratica, stiamo dividendo le frequenze assolute per i totali di riga
# infatti la somma per riga ci restituisce il valore 1
addmargins(prop.table(tabella,1),2)

#   della X rispetto a (Y = y_j)
addmargins(tabella)
prop.table(tabella,2) # (n_{ij} /n_{.j})
# in pratica, stiamo dividendo le frequenze assolute per i totali di colonna
# infatti la somma per colonna ci restituisce il valore 1
addmargins(prop.table(tabella,2),1)

# Come cambiano i plot se rappresentiamo le distribuzioni condizionate...

par(mfrow = c(1,3))
plot(tabella, color = T)
plot(prop.table(tabella,1), color = T)
plot(t(prop.table(tabella,2)), color = T)

# Come facciamo a capirci qualcosa?
# Come dicevamo, grazie alle distribuzioni condizionate il confronto 
# è più chiaro perchè eliminiamo il problema della differente numerosità 

par(mfrow = c(1,2)) 

barplot(prop.table(tabella,2),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "")
# frequenze percentuali di colonna

barplot(t(prop.table(tabella,1)),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "")
# frequenze percentuali di riga

par(mfrow = c(1,2))

barplot(prop.table(tabella,2),
        beside = T,
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "")
# frequenze percentuali di colonna

barplot(t(prop.table(tabella,1)),
        beside = T,
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "")
# frequenze percentuali di riga

# Calcolo dell'indice di connessione Chi quadrato -------------------------

# Ripartiamo dalla tabella di contingenza
tabella
# e dalle sue marginali
marg.X
marg.Y

# e calcoliamo l'indice Chi quadrato in base alla formula semplificata (vedi slide):

k = nrow(tabella)
h = ncol(tabella)
n = sum(tabella)

cont2 = matrix(NA,k,h)
for (i in 1:k){
  for(j in 1:h){
    cont2[i,j] = tabella[i,j]^2/(marg.X[i]*marg.Y[j])
  }
}

chi2 = n * (sum(cont2)-1)
chi2

# tutto questo è molto elaborato e traduce letteralmente la formula 
# (in particolare gli indici delle sommatorie sono proprio gli indici dei cicli for)
# ma ragionando un attimo sulla formula...

# n_{ij}^2 / (n_{i.} n_{.j}) = (n_{ij} / n_{i.}) * (n_{ij} /n_{.j})

# e i due rapporti a secondo membro sono esattamente le frequenza relative delle
# distribuzioni condizionate:

prop.table(tabella,1) # (n_{ij} / n_{i.})

prop.table(tabella,2) # (n_{ij} /n_{.j})

# quindi il prodotto elemento per elemento delle due matrici che ne risultano ci dà proprio
# n_{ij}^2 / (n_{i.} n_{.j})

# in altre parole 
prop.table(tabella,1) * prop.table(tabella,2) 
# è quella che prima abbiamo chiamato
cont2

# quindi per calcolare chi2 basta scrivere 
n*(sum(prop.table(tabella,1)*prop.table(tabella,2))-1)
# che infatti coincide con
chi2

# Se vogliamo passare all'indice relativo V di Cramer:

V = sqrt(chi2 / (n * min(k-1,h-1)))
V

## Concludendo: ########################################################
# Nadal ha vinto circa il 66% delle partite giocate su terra rossa 
# e circa il 57% delle partite giocate su altre superfici.
# Girando il punto di vista, circa il 24% delle sconfitte è 
# avvenuto su terra rossa, contro circa il 31% delle vittorie.
# L'indice Chi quadrato chi2 = 52.9 e l'indice (relativo) V di Cramer 
# V = 0.077 indicano la presenza di una certa associazione tra il tipo 
# di campo e l'esito della gara.
#########################################################################

# Paradosso di Simpson ----------------------------------------------------

# Fin qui abbiamo trascurato la terza variabile presente nel dataframe
# grazie alla quale possiamo distinguere i game in cui Nadal serve
# rispetto a quelli in cui serve l'avversario

# stiamo cioè stratificando rispetto alla variabile servizio
idx.servizio = which(Nadal$servizio == "si")
# e costruendo due nuove tabelle di contingenza
# per le altre due variabili
tab.serv  = table(Nadal$superficie[idx.servizio],Nadal$risultato[idx.servizio])
tab.serv
tab.noserv = table(Nadal$superficie[-idx.servizio],Nadal$risultato[-idx.servizio])
tab.noserv

# Cambia qualcosa?

par(mfrow = c(1,2)) 

barplot(prop.table(tab.serv,2),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Nadal al servizio")
barplot(prop.table(tab.noserv,2),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Avversario al servizio")
# frequenze percentuali di colonna


barplot(t(prop.table(tab.serv,1)),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Nadal al servizio")
barplot(t(prop.table(tab.noserv,1)),
        col = c("darkgreen", "brown"),
        legend.text = T,
        main = "Avversario al servizio")
# frequenze percentuali di riga

n.serv = sum(tab.serv)
n.noserv = sum(tab.noserv)

# Chi quadrato e V di Cramer..

chi2.serv   = n.serv*(sum(prop.table(tab.serv,1)*prop.table(tab.serv,2))-1)
chi2.serv
chi2.noserv = n.noserv*(sum(prop.table(tab.noserv,1)*prop.table(tab.noserv,2))-1)
chi2.noserv


V.serv = sqrt(chi2.serv / (n.serv * min(k-1,h-1)))
V.serv

V.noserv = sqrt(chi2.noserv / (n.noserv * min(k-1,h-1)))
V.noserv

## Concludendo: ########################################################
# Quando Nadal è al servizio Nadal il tipo di campo non incide 
# sulla sua probabilità di vittoria (V = 0.005)
# Quando l'avversario è al servizio invece la performance di Nadal 
# è decisamente migliore sulla terra rossa  (V = 0.158)
# L'interpretazione dell'associazione tra risultato e superficie era 
# stata attenuata e quindi falsata dal fatto che precedentemente avevamo 
# trascurato la variabile servizio!!
#########################################################################