
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#       Lab04 ** Indici di posizione **                                   # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

load("studenti.RData")

# Mediana: calcolo e proprietà --------------------------------------------------------

x = studenti$ALTEZZA

# La cosa più banale del mondo:
median(x)
Me = median(x)
Me

# che ovviamente corrisponde ai seguenti passaggi:

# a. partire dagli n valori della variabile X
x
# b. metterli in ordine
?sort
y = sort(x)
y
# c. distinguere i due casi: n pari e n dispari
n = length(x)
n
# poiché in questo caso n è dispari 
# la mediana Me è il valore di x che occupa il posto (n+1)/2 nel vettore ordinato
(n+1)/2
y[(n+1)/2]


# Verifichiamo empiricamente le proprietà che abbiamo dimostrato:

# Proprietà 1: INTERNALITA'
m = min(x)
m
M = max(x)
M
m <= Me
M >= Me
(m <= Me) & (M >= Me)
# ovvio, visto che si tratta di uno dei valori effettivamente assunti dalla variabile x

# Proprietà 2: è un CENTRO di ordine 1
# ovvero Me è il valore che minimizza la somma degli scarti in valore assoluto

Me = median(x)

# costruiamo una griglia di valori generici c
# cioè una sequenza che va da Me-100 a Me+100 con passo 5
c = seq(Me-100,Me+100,5)
# per ciascun valore di c calcoliamo la somma degli scarti in valore assoluto
# in modo un po' pedissequo con un ciclo for
scarti.c = rep(NA,length(c))
for (i in 1:length(c)){
  scarti.c[i] = sum(abs(x-c[i]))
}
# poi plottiamo gli scarti al variare di c
plot(c,scarti.c, pch = 19, type = "l")
points(Me,scarti.c[c==Me], col = "green", pch = 19)

# Proprietà 3: ROBUSTEZZA rispetto alla presenza di valori anomali!
# più robusta rispetto alla media

# Diamo un'occhiata brutale ai nostri dati
plot(x)
mu = mean(x)
abline(h = mu, col = 3)
Me = median(x)
abline(h = Me, col = 2)
# ci sono dei valori molto grandi?
# ad esempio:
x > 180
# ce ne sono?
any(x > 180)
# quanti sono?
sum(x > 180)
# quali sono?
which(x > 180)
x[which(x > 180)]
# beh sì...

# ora vediamo cosa succede se rimuoviamo queste due osservazioni dal dataframe:
x.ridotto = x[-which(x > 180)]
mean(x.ridotto)
abline(h = mean(x.ridotto), col = 3)
median(x.ridotto)
abline(h = median(x.ridotto), col = 2)
# mentre la media subisce una sensibile riduzione, la mediana non si è spostata.

# Altre possibili soluzioni in presenza di valori anomali:

#### Media troncata
mean(x)
mean(x, trim = 0.025)
mean(x, trim = 0.05)

#### Media di Winsor
install.packages("psych")
library(psych)
winsor(x, trim = 0.025, na.rm = TRUE)
winsor.mean(x, trim = 0.025, na.rm = TRUE)

# piccolo dubbio: a cosa serve l'opzione na.rm
?mean
# vediamo ad esempio che la variabile AIDS.x.100000.ab
# contiene degli NA (Not Available) ovvero dei valori mancanti
xna <- c(x,NA,NA)
?is.na
is.na(xna)
# quanti sono gli NA?
sum(is.na(xna))
# e perchè ci danno fastidio?
# se provo a calcolare la media ottengo questo:
mean(xna)
# ed ecco che ci viene in aiuto l'opzione na.rm = T
# che ci consente di restringere il calcolo ai soli dati completi
mean(xna, na.rm=TRUE)


# Funzione di ripartizione empirica e quantili ----------------------------

# Ora lavoriamo sul dataset decathlon
decathlon <- read.csv("dati.csv")
str(decathlon)

# concentriamoci sulla variabile Long.jump
x = decathlon$highjump

# ricordiamo come si ottiene il grafico della Funzione di ripartizione empirica F(x)
plot(ecdf(x))
# la mediana Me è quel valore tale per cui F(Me) = 0.50
Me = median(x)
Me

abline(h = 0.5, col = 4, lty = 2)
abline(h = Me, col = 4, lty = 2)

# quartili
?quantile
# il primo e il terzo quartile sono rispettivamente 
# quei valori tali per cui si ha F(Q1) = 0.25 e F(Q3) = 0.75
Q1 = quantile(x, probs = 0.25)

# piccola precisazione sugli argomenti aggiuntivi
quantile(x, 0.25)
quantile(x, probs = 0.25)
# danno lo stesso risultato perchè probs è il secondo argomento
# della funzione quantile (per sapere l'ordine vedi help ?quantile)

Q3 = quantile(x, 0.75)

Q1
Q3

# sovrapponiamo al grafico della funzione di ripartizione
# la retta orizzontale a livello 0.25
abline(h = 0.25, col = 4, lty = 2)
# la retta verticale in corrispondenza del valore di Q1
abline(v = Q1, col = 4, lty = 2)

# la retta orizzontale a livello 0.75
abline(h = 0.75, col = 4, lty = 2)
# la retta verticale in corrispondenza del valore di Q3
abline(v = Q3, col = 4, lty = 2)

# e ovviamente il secondo quartile 
quantile(x, 0.5)
# coincide con la mediana
Me

# si possono stabilire i livelli anche tutti insieme...
livelli = seq(from = 0, to = 1, by = 0.25)
livelli
quantile(x, probs = livelli)

# la funzione quantile() consente di calcolare i quantili di una distribuzione a qualsiasi livello
quantile(x, 1/3)

# Mediana per distribuzioni in classi ---------------------------------------

# Consideriamo la seguente distribuzione in classi 
tagli = seq(1.49,1.90,by = .05) 
frequenze = table(cut(x, breaks = tagli))
frequenze
# Vogliamo confrontare la mediana generale calcolata sulla distribuzione per unità
Me
# Con la mediana calcolata a partire dalla distribuzione di frequenza
# Calcoliamo innanzi tutto le frequenze relative e le frequenze relative cumulate:
frequenze/sum(frequenze)
freq.cum = cumsum(frequenze/sum(frequenze))
freq.cum

classe.mediana = which(freq.cum > 0.50)[1]

Me.classi = tagli[classe.mediana] + diff(tagli)[classe.mediana] * (0.5-freq.cum[classe.mediana-1])/(freq.cum[classe.mediana]-freq.cum[classe.mediana-1])
# Attenzione! la suddivisione in classi comporta una perdita di informazione
Me.classi
Me

# vediamo questi due valori sull'istogramma
hist(x, breaks = tagli, prob = T)
abline(v=Me, col = 2)
abline(v=Me.classi, col = 3)

# rappresentiamo la funzione di ripartizione empirica
# innanzi tutto disegniamo i punti di coordinate 
# (estremo della classe, frequenza relativa cumulata)
plot(tagli,c(0,freq.cum))
# e poi congiungiamo i punti (con l'opzione type = "l")
points(tagli,c(0,freq.cum), type = "l")

# a questo punto sul grafico della funzione di ripartizione
# possiamo tracciare la retta parallela all'asse x a livello 0.5
abline(h = 0.5, col = 2)
# e la retta verticale che corrisponde alla mediana
abline(v = Me.classi, col = 2)

# ultima cosa:
# ripartiamo dal grafico della funzione di ripartizione 
# costruito sui dati originari (distr. per unità)
plot(ecdf(x))
# e sovrapponiamo la funzione costante a tratti
# che abbiamo ottenuto nel caso delle classi
points(tagli,c(0,freq.cum), type = "l", col = 5, lwd=3)
# (l'opzione lwd serve solo per scegliere una linea più marcata)

# Boxplot -----------------------------------------------------------------

# Ecco un grafico che riassume tutte queste informazioni:
?boxplot
boxplot(x)

# Moda --------------------------------------------------------------------

# Lavoriamo ad esempio su una variabile qualitativa e calcoliamo la moda:
# le frequenze assolute sono:
table(frequenze)
# la frequenza più elevata è:
max(frequenze)
# la moda, per definizione, è la modalità alla quale è associata la massima frequenza
which.max(frequenze)
# attenzione! 7 è soltanto la posizione occupata dalla modalità '(1.79,1.84]' 

