
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#      Lab03 ** Medie: calcolo e proprietà **                             # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

# Importiamo nuovamente il dataset x
library(clustrd)
data(macro)
x <- macro

# Media aritmetica: calcolo e proprietà --------------------------------------------------------

# Per evitare di perdere tempo possiamo anche caricare direttamente il file paesi.RData
x$GDP
x = x$GDP

# La cosa più banale del mondo:
mean(x)
mu = mean(x)

# che ovviamente corrisponde a:
sum(x)/length(x)

# Verifichiamo empiricamente le proprietà della media:

# Proprietà 1: INTERNALITA'
m = min(x)
m
M = max(x)
M
m <= mu
M >= mu
(m <= mu) & (M >= mu)

# Proprietà 2: BARICENTRO 

# ovvero la somma degli scarti deve essere uguale a 0
# calcoliamo gli scarti (ricordandoci che R lavora vettorialmente)
scarti = x - mu
sum(scarti)
# che ne pensate?
# non è esattamente 0 per via dell'approssimazione...ma è un numero trascurabile!


# Proprietà 3: LINEARITA' 
# applichiamo una trasformazione lineare ai dati originari, del tipo: a * x + b

# ad esempio una trasformazione che li normalizza (cioè li mappa tra 0 e 1), cioè
# a * x + b,    con a = 1/(M-m), b = -m/(M-m)
# equivalente a scrivere: 
# (x - m)/(M-m)
a = 1/(M-m)
b = -m/(M-m)
x.normalizzati = a * x + b
# x.normalizzati = (x - m)/(M-m)
# vediamo se è vero che vanno da 0 a 1
range(x.normalizzati)

# benissimo: ora calcoliamo la media dei dati trasformati:
mean(x.normalizzati)
# e controlliamo che sia uguale a:
a * mu + b

# Proprietà 4: è un CENTRO di ordine 2
# ovvero mu è il valore che minimizza la somma dei quadrati degli scarti

# costruiamo una griglia di valori generici c
# cioè una sequenza che va da mu-10000 a mu+10000 con passo 20
c = seq(mu-1000,mu+1000,20)
# per ciascun valore di c calcoliamo la somma degli scarti al quadrato
# in modo un po' pedissequo con un ciclo for
scarti.c = rep(NA,length(c))
for (i in 1:length(c)){
  scarti.c[i] = sum((x-c[i])^2)
}
# poi plottiamo gli scarti al variare di c
plot(c,scarti.c, pch = 19)

# Proprietà 5: PROPRIETA' ASSOCIATIVA
# ovvero la media di un gruppo totale è pari alla media ponderata
# dei sottogruppi con mesi dati dalle numerosità dei singoli sottogruppi

# consideriamo come sottogruppi le classi
class = c(rep(1,10),rep(2,10))
# le numerosità dei singoli sottogruppi sono le seguenti:
dim.class = table(class)
dim.class
# le medie per ciascuna class sono invece
mean(x[class == 1])
mean(x[class == 2])
# molto più velocemente
?by
by(x, INDICES = class, FUN = mean)
# salviamo questo simpatico output in un nuovo oggetto
medie.per.class = by(x, INDICES = class, FUN = mean)
medie.per.class
# perfetto, ma liberiamoci della struttura della lista
medie.per.class = c(medie.per.class)
# bene quello che vogliamo verificare è che 
mu 
# si ottiene come media ponderata delle medie.per.class
# con pesi espressi dalle numerosità dei singoli sottogruppi (numero di paesi per class)
sum(dim.class*medie.per.class)/sum(dim.class)
# dove abbiamo sfruttato la caratteristica di R di operare elemento per elemento

# Più che una proprietà un difetto: NON è ROBUSTA rispetto alla presenza di valori anomali!

# Diamo un'occhiata brutale ai nostri dati
plot(x)
abline(h = mu, col = 3)
# ci sono dei valori molto grandi?
# ad esempio:
x > 3
# ce ne sono?
any(x > 3)
# quanti sono?
sum(x > 3)
# quali sono?
which(x > 3)
x[which(x > 3)]
# beh sì...

# ora vediamo cosa succede se rimuoviamo queste due osservazioni dal dataframe:
x.ridotto = x[-which(x > 3)]
mean(x.ridotto)
abline(h = mean(x.ridotto), col = 5)

# Media aritmetica per distribuzioni in classi ---------------------------------------

# Consideriamo la seguente distribuzione in classi per la Popolazione
tagli = c(0,1,2,3,max(x))
frequenze = table(cut(x, breaks = tagli))
frequenze
# Vogliamo confrontare la media generale calcolata sulla distribuzione per unità
mu
# Con la media calcolata a partire dalla distribuzione di frequenza
# Abbiamo bisogno dei valori centrali, che per semplicità possiamo estrarre da qui:
x.hist = hist(x, breaks = tagli)
valori.centrali = x.hist$mids
valori.centrali
# da moltiplicare per le rispettive frequenze:
valori.centrali*frequenze
# e applicando la formula della media per distribuzioni di frequenza in classi:
sum(valori.centrali*frequenze)/sum(frequenze)
# come previsto in generale otteniamo un valore diverso da mu!!!
# N.B.: ovviamente questo è legato alla perdita di informazione 
# dovuta al passaggio dalla distribuzione per unità a quella in classi
# ma se abbiamo a disposizione solo la seconda non possiamo fare altro

# Media geometrica --------------------------------------------------------
G = prod(x)^(1/length(x))
G

### vediamo una relazione interessante
mean(x)
# la media aritmetica non sembra adeguata, provo una trasformazione logaritmica dei paesi
mean(log(x))
exp(mean(log(x)))

# vi ricordate a cosa equivale?...
n = length(x)
prod(x)^(1/n)

# ...certo, alla media geometrica! ovviamente c'è anche una funzione già implementata...
# nel pacchetto psych che è necessario caricare:
library(psych)
geometric.mean(x,na.rm=TRUE)

# Media armonica ----------------------------------------------------------

A = n/sum(1/x)

# Confronto *empirico* tra le tre medie
mu = mean(x) 
G = prod(x)^(1/n)
A = n/sum(1/x)

A
G
mu
# A < mu < G


