
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#       Lab09 ** Dipendenza in media, Asimmetria e curtosi **             # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #


# Dipendenza in media -----------------------------------------------------


# Carichiamo nel workspace il file reddito-geo.RData
# che contiene i dati riportati sugli appunti (a pag. 6)
load("reddito-geo.RData")

# Ecco il dataframe
dati

# Per semplicità chiamiamo Y la nostra variabile quantitativa (reddito)
Y = dati$reddito
# e X la nostra variabile qualitativa (ripartizione geografica)
X = dati$geo

# Come già sappiamo, la rappresentazione grafica più opportuna è il boxplot affiancato:
boxplot(Y ~ X, 
        main = "Esempio numerico", 
        ylab = "reddito",
        col = c("brown", "orange", "lightgreen") ,
        ylim = c(500,3500), range = 0)

# Calcoliamo tutti gli ingredienti che ci servono:

# ---> numerosità dei singoli sottogruppi
n.i0 =  table(X)
n.i0
# ---> numerosità complessiva
N = sum(n.i0)
N

# ---> medie parziali
# su ciascun sottogruppo calcolo la media del reddito
mean(Y[X == "Nord"])
mean(Y[X == "Centro"])
mean(Y[X == "Sud"])
# o, molto più rapidamente con la funzione by applico la funzione media a ciascun sottogruppo
medie.condizionate = c(by(Y, X, mean))
medie.condizionate
# questa informazione può essere riportata sul boxplot...
points(medie.condizionate, pch = 19, col = 1, cex = 2)
points(medie.condizionate, pch = c("N","C","S"), col = "white")
# ---> media globale
# la cosa più immediata è:
media.globale = mean(Y)
media.globale
# ma sappiamo bene che questo corrisponde anche alla 
# media ponderata delle medie parziali con pesi espressi dalle numerosità dei sottogruppi
sum(medie.condizionate * n.i0) / N
# anche questa informazione può essere riportata sul boxplot...
abline(h = media.globale, lty = 2, col = "grey", lwd = 3)

# ---> devianze parziali
# su ciascun sottogruppo calcolo la varianza del reddito
dev.nord = sum((Y[X == "Nord"] - medie.condizionate["Nord"])^2)
dev.centro = sum((Y[X == "Centro"] - medie.condizionate["Centro"])^2)
dev.sud = sum((Y[X == "Sud"] - medie.condizionate["Sud"])^2)
# e le salvo tutte nell'oggetto
dev.condizionate = c(dev.nord, dev.centro, dev.sud)
dev.condizionate
# che equivale a:
dev.condizionate = c(by(Y, X, var)) * (n.i0-1) # ** ricordandosi che in var c'è  n-1 a denominatore!
dev.condizionate

# ---> devianza totale
D.Y = sum((Y-mean(Y))^2)
D.Y

# A questo punto possiamo verificare empiricamente 
# che vale la scomposizione della devianza in due termini:

# 1. la devianza delle medie parziali rispetto alla media generale
D.S = sum( n.i0* (medie.condizionate - media.globale)^2)
D.S
# (che esprime la VARIABILITA' TRA LE MEDIE)

# 2. la media ponderata delle varianze parziali con pesi dati dalle numerosità dei sottogruppi
D.R = sum(dev.condizionate)
D.R
# (che esprime la VARIABILITA' ALL'INTERNO DEI SOTTOGRUPPI, cioè RISPETTO A CIASCUNA MEDIA PARZIALE)

# La somma di questi due termini
D.S + D.R
# coincide infatti con la devianza totale
D.Y

# Infine è immediato calcolare l'indice eta quadro
# che rappresenta la PROPORZIONE DI VARIABILITA' TOTALE DOVUTA ALLA VARIABILITA' TRA LE MEDIE:
eta2 = D.S / D.Y
eta2
# In questo caso un valore pari a 0.75 (quindi abbastanza vicino a 1) indica una dipendenza in media del reddito
# dalla ripartizione geografica piuttosto forte. 


# Creazione di una "nuova" funzione ---------------------------------------

# Siamo stati costretti ad eseguire tutti questi passaggi
# perchè non esiste in R una funzione che calcola direttamente 
# l'indice di dipendenza in media...

# Ma questa è un'ottima occasione per parlare di una ulteriore potenzialità di R:
# Possiamo creare una qualunque funzione che, prendendo in input uno o più argomenti,
# esegua una serie di istruzioni e infine ci restituisca un certo output.

# La sintassi per definire una nuova funzione è molto semplice:

# 1. scegliamo un nome, ad esempio 'mia.fun'
# 2. facciamo la seguente assegnazione (con l'=)
# 3. usiamo la parola chiave function() 
#    con cui in effetti stiamo creando un oggetto di modo function
#    all'interno delle tonde elenchiamo gli argomenti in input
# 4. apriamo e chiudiamo due parentesi graffe {...}
#    al posto dei ... scriviamo una serie di istruzioni da eseguire
# 5. con l'istruzione finale return() ci facciamo restituire un output

# esempio facile
mia.fun = function(a,b){
  somma = a+b
  return(somma)  
}
# questa funzione può essere richiamata come tutte le altre funzioni già esistenti in R
mia.fun(2,4)
mia.fun(a = 2,b = 4)

# Ci sarebbero molti altri dettagli di cui parlare...
# ma per adesso veniamo al punto e costruiamo una funzione che calcoli eta quadro 
# ovviamente attingo alle istruzioni che abbiamo scritto prima...


dip.in.media.fun = function(X,Y){ # assumiamo di dare in input le due variabili X e Y
  
  # ---> numerosità dei singoli sottogruppi
  n.i0 =  table(X)
  # ---> numerosità complessiva
  N = sum(n.i0)
  # ---> medie parziali
  medie.condizionate = c(by(Y, X, mean))
  # ---> media globale
  media.globale = mean(Y)
  # ---> varianze parziali
  dev.condizionate = c(by(Y, X, var)) * (n.i0-1) # ** ricordandosi che in var c'è  n-1 a denominatore!
  # ---> devianza globale
  D.Y = var(Y)*(N-1) # ** ricordandosi che il totale va diviso per n e non per n-1!

  # devianza delle medie parziali rispetto alla media generale
  D.S = sum( n.i0* (medie.condizionate - media.globale)^2)
  
  # media ponderata delle varianze parziali con pesi dati dalle numerosità dei sottogruppi
  D.R = sum(dev.condizionate)
  
  eta2 = D.S/D.Y
  
  return(eta2)
}

# Richiamiamo la funzione appena definita
# specificando gli argomenti di input dati$geo e dati$reddito
dip.in.media.fun(X = dati$geo,Y = dati$reddito)

# Se vogliamo aggiungere qualcosina...

dip.in.media.fun = function(X,Y,          # assumiamo di dare in input le due variabili X e Y
                            grafico = T){ # assumiamo il valore T (di default) se voglio che sia prodotto un grafico 
  
  if(grafico == T){                       # quando grafico == T viene prodotto un grafico 
    boxplot(Y ~ X, 
            col = topo.colors(length(unique(X))),
            range = 0) 
  }
  
  # ---> numerosità dei singoli sottogruppi
  n.i0 =  table(X)
  # ---> numerosità complessiva
  N = sum(n.i0)
  # ---> medie parziali
  medie.condizionate = c(by(Y, X, mean))
  # ---> media globale
  media.globale = mean(Y)
  # ---> varianze parziali
  dev.condizionate = c(by(Y, X, var)) * (n.i0-1) # ** ricordandosi che in var c'è  n-1 a denominatore!
  # ---> varianza globale
  D.Y = var(Y)*(N-1) # ** ricordandosi che il totale va diviso per n e non per n-1!

  # devianza delle medie parziali rispetto alla media generale
  D.S = sum( n.i0* (medie.condizionate - media.globale)^2)
  
  # media ponderata delle varianze parziali con pesi dati dalle numerosità dei sottogruppi
  D.R = sum(dev.condizionate)
  
  eta2 = D.S/D.Y
  
  out = list(D.Y = D.Y,   # -------> per esempio arricchiamo l'output creando una lista
             D.S = D.S,
             D.R = D.R,
             eta2 = eta2)
  
  return(out)
}

dip.in.media.fun(X = dati$geo,Y = dati$reddito)

dip.in.media.fun(X = dati$geo,Y = dati$reddito, grafico = F) # se non voglio che sia mostrato il grafico



# Asimmetria e curtosi ----------------------------------------------------

# Infine recuperiamo le formulette degli indici di forma

hist(Y, prob = T, col = "purple",  main = "")

# Calcoliamo prima tutte le quantità coinvolte...

N = length(Y)
mu = mean(Y)
m  = median(Y)
sigma = sd(Y) * sqrt((N-1)/N)
Q1 = quantile(Y, 0.25)
Q3 = quantile(Y, 0.75)

# e poi due indici di asimmetria 

alpha.1 = (mu - m)/sigma
alpha.1

alpha.2 = 1/sigma^3 * 1/N * sum( (Y - mu)^3 )
alpha.2

# calcoliamo allora anche l'indice di curtosi 

gamma.curtosi =  1/sigma^4 * 1/N * sum( (Y - mu)^4 ) - 3
gamma.curtosi

# distribuzione iponormale:: rispetto alla distribuzione normale si ha una minore frequenza per valori centrali ed estremi, maggiore per valori intermedi

curve(dnorm(x, mu, sigma), col = 3, add=T, lwd = 3)



