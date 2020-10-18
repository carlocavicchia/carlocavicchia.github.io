
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#              Lab02 ** Rappresentazioni grafiche **                      # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

# Distribuzione di frequenza per variabili qualitative sconnesse ---------------

# Ripartiamo da qui e vediamo come costruire una distribuzione di frequenza
# e come rappresentare graficamente una variabile qualitativa sconnessa

#  Importare dati da un file esterno (.csv)
#         la cosa più semplice è point&click:
#         dal panel Workspace - Import Dataset - From text file...
#         questi comandi si producono automaticamente sulla console
# 
# > View(PIPPO)

PIPPO = read.csv("students.csv")
str(PIPPO)

# per lavorare su una singola variabile si usa il dollaro, ad esempio
PIPPO$Sex
# estrae dal dataframe PIPPO la variabile sesso

# qualche piccola operazione su cui torneremo...

# il comando fondamentale per ottenere delle tabelle ? table
table(PIPPO$Sex)
n = sum(table(PIPPO$Sex))

# costruiamo la distribuzione di frequenze
freq.ass = table(PIPPO$Sex)
freq.ass
freq.rel = prop.table(freq.ass)
freq.rel = freq.ass/n
freq.rel
sum(freq.rel)
round(freq.rel,3) # per arrotondare al terzo decimale

freq.rel = round(prop.table(freq.ass),3)

freq.perc = freq.rel*100
freq.perc

distr.frequenze = cbind(freq.ass,freq.rel,freq.perc)
distr.frequenze = addmargins(distr.frequenze,1) # per aggiungere i totali
distr.frequenze

# alcuni grafici da associare alla tabella:
barplot(table(PIPPO$Sex))
barplot(table(PIPPO$Sex), col = c("pink","lightblue"))
barplot(table(PIPPO$Sex), col = c("pink","lightblue"), 
        main = "Distribuzione di frequenza", sub = "Variabile :: sesso")


# possiamo salvare anche uno o più oggetti in un file con estensione .RData
save(PIPPO, file = "PIPPO.Rdata")
# che pu? essere successivamente ricaricato cos?
load("PIPPO.RData")
# o semplicemente cliccando il nome del file nel pannello Files.


# Rappresentazione grafica di variabili qualitative ordinate --------------
# o in generale di tutte le variabili discrete 

str(PIPPO)
# prendiamo ad esempio la variabile 
PIPPO$new <- c(rep(1,20),rep(2,12))
str(PIPPO)
# notiamo che questa variabile è stata importata come numerica
# in realtà sappiamo che a rigore dovrebbe essere un fattore
PIPPO$new = factor(PIPPO$new)
# notiamo anche stiamo modificando la variabile originaria interna al dataframe
str(PIPPO)

# invece in questo modo
new = factor(PIPPO$new)
# stiamo creando un nuovo oggetto esterno al dataframe (vedi Workspace)
# se vogliamo rimuoverlo...
rm(new)

# abbiamo già visto come ottenere la distribuzione di frequenza
table(PIPPO$new)
# e il corrispondente diagramma a barre
barplot(table(PIPPO$new))
# aggiungiamo un po d'interpretazione: ipotizziamo che new è un'indicatore di preoccupazione Si/No
barplot(table(PIPPO$new), 
        main = "Distribuzione di frequenza della preoccupazione per l'università",
        xlab = "variabile :: preoccupazione",
        sub = "NO <-----------------------------------------------------------------> SI",
        ylab = "frequenza assoluta",
        ylim = c(0,35),
        col = heat.colors(5))
# altra cosa interessante :: confrontiamo la distribuzione della preoccupazione
# stratificando per sesso...
table(PIPPO$Sex,PIPPO$new)
barplot(table(PIPPO$Sex,PIPPO$new),
        beside=T, 
        col = c("pink","light blue"),
        legend.text = c("F","M"),
        main = "Preoccupazione per l'universit? in base al sesso",
        xlab = "variabile :: preoccupazione",
        sub = "NO <-----------------------------------------------------------------> SI",
        ylab = "frequenza assoluta",
        ylim = c(0,25)        
        )
# occhio: queste sono frequenze assolute!
# proviamo cosa succede se l'opzione beside = F
barplot(table(PIPPO$Sex,PIPPO$new),
        beside=F)
# e invertendo l'ordine delle variabili nella table
barplot(table(PIPPO$new,PIPPO$Sex),
        beside=F)

# cos? aggiungiamo i totali di riga e colonna
addmargins(table(PIPPO$Sex,PIPPO$new))

# cos? dividiamo ciascuna riga della tabella per il numero totale di femmine e di maschi
prop.table(table(PIPPO$Sex,PIPPO$new),1)
addmargins(prop.table(table(PIPPO$Sex,PIPPO$new),1),2)
# infatti la somma per riga ci restituisce il valore 1
# e questa distribuzione consente un confronto più chiaro
# perch? eliminiamo il problema della differente numerosit? dei gruppi di femmine e maschi
barplot(prop.table(table(PIPPO$Sex,PIPPO$new),1),
        beside=T, 
        col = c("pink","light blue"),
        legend.text = c("F","M"),
        main = "Preoccupazione per l'università in base al sesso",
        xlab = "variabile :: preoccupazione",
        sub = "NO <-----------------------------------------------------------------> SI",
        ylab = "frequenza relativa"
)
# di nuovo cambiando l'opzione beside...
barplot(prop.table(table(PIPPO$Sex,PIPPO$new),1),
        beside=F, 
        col = c("pink","light blue"),
        legend.text = c("F","M"),
        main = "Preoccupazione per l'università in base al sesso",
        xlab = "variabile :: preoccupazione",
        sub = "NO <-----------------------------------------------------------------> SI",
        ylab = "frequenza relative"
)

barplot(prop.table(table(PIPPO$new,PIPPO$Sex),2),
        beside=F, 
        col = rainbow(5),
        legend.text = c("1","2"),
        main = "Preoccupazione per l'università in base al sesso",
        xlab = "variabile :: preoccupazione",
        sub = "NO <-----------------------------------------------------------------> SI",
        ylab = "frequenza assoluta"
) 

# sicuramente più facile da confrontare rispetto a questo...o no?
par(mfrow = c(1,2)) # solo per dividere la finestra grafica in due parti
pie(table(PIPPO$new[PIPPO$Sex=="Female"]), 
    col = heat.colors(5),
    sub = "Females")
mtext("Preoccupazione per l'università in base al sesso")
pie(table(PIPPO$new[PIPPO$Sex=="Male"]), col = heat.colors(5),
    sub = "Maschi")

# Attenzione: qui abbiamo usato brutalmente un tipo di sintassi che ancora non avevamo introdotto:

# ----> Due parole sull'estrazione di elementi di vettori, matrici e dataframe

# partendo da un semplice vettore
a = c("a","b","c","d")
# possiamo estrarne gli elementi usando gli indici ovvero le posizioni degli elementi nel vettore
a[3]
# se invece abbiamo a che fare con una matrice
aa = matrix(c("a","b","c","d","e","f","g","h"),2,4)
# per estrarre ciascun elemento dobbiamo specificare una coppia di indici
aa[1,1]
# per estrarre un'intera riga basta
aa[1,]
# e per una colonna ovviamente
aa[,2]

# questo vale quindi anche per PIPPO che ha una struttura bidimensionale
PIPPO[4,3]
# in particolare la variabile eta pu? quindi essere estratta in due modi:
PIPPO[,5]
PIPPO$Freq
# anzi tre
PIPPO[,"Freq"]

# per quanto riguarda l'estrazione delle unità (che occupano le righe del dataframe)
# E' molto frequente selezionare le unità in base ad una condizione logica
# ad esempio una condizione logica è
PIPPO$Freq == 18
# che può essere usata all'interno delle parentesi quadre [ ]
PIPPO$Freq[PIPPO$Freq == 18]
# o se lavoriamo direttamente sull'intero dataframe di cui vogliamo estrarre le unità
# che soddisfano quella condizioni e tutte le colonne [condizione,]
PIPPO[PIPPO$Freq == 18,]

# quindi in questo modo costruiamo due nuovi oggetti che contengono i due sottogruppi 
# dei maschi
PIPPOmaschi = PIPPO[PIPPO$Sex=="Male",]
# e delle femmine
PIPPOfemmine = PIPPO[PIPPO$Sex=="Female",]
# notare che ciascuno degli oggetti ? ancora un dataframe con 15 variabili
# e numero di unit? pari rispettivamente al numero dei maschi e delle femmine

# lo stesso grafico di prima si pu? quindi ottenere anche cos?:
pie(table(PIPPOfemmine$new), 
    col = heat.colors(5),
    sub = "Femmine")
mtext("Preoccupazione per l'università in base al sesso")
pie(table(PIPPOmaschi$new), col = heat.colors(5),
    sub = "Maschi")

par(mfrow=c(1,1))


# Rappresentazione grafica della distribuzione di frequenza ----------
# per classi di variabili quantitative continue

PIPPO$Freq
# come abbiamo gi? detto questa variabile ? stata discretizzata, 
# ma ? per sua natura una variabile continua 
# (N.B. ha sempre senso pensare a un'età compresa tra due valori, per quanto possano essere vicini...)

# il tipo di grafico più adatto per rappresentare questo tipo di carattere ? l'istogramma
hist(PIPPO$Freq)

hist(PIPPO$Freq, 
     main = "Distribuzione in classi (equiampie) di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66))
# come vedete di default R propone una suddivisione in classi equiampie
# ma ovviamente possiamo cambiarla...
# ad esempio decidendo che vogliamo più classi:
hist(PIPPO$Freq, 
     breaks = 20,
     main = "Distribuzione in classi (equiampie) di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66))
# o meno classi
hist(PIPPO$Freq, 
     3, # NOTA: anche senza specificare l'argomento 'breaks =' 
        # R lo capisce perch? è il secondo argomento nell'ordine (vedi help)
     main = "Distribuzione in classi (equiampie) di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66))
hist(PIPPO$Freq, 
     3, # NOTA: anche senza specificare l'argomento 'breaks =' 
     # R lo capisce perch? ? il secondo argomento nell'ordine (vedi help)
     main = "Distribuzione in classi (equiampie) di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66),
     prob = T)
# oppure fissando esplicitamente gli estremi delle classi:
# vediamo innanzi tutto quale ? il range della variabile età
range(PIPPO$Freq)
hist(PIPPO$Freq, 
     breaks = c(2,18.5,25,66),
     main = "Distribuzione in classi di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66))
# N.B. sull'asse delle y non c'è più la frequenza ma la densità di frequenza!!!

# Abbiamo ottenuto direttamente la rappresentazione grafica, 
# ma come possiamo recuperare la distribuzione in classi?
eta.hist = hist(PIPPO$Freq, 
     breaks = c(2,18.5,25,66),
     main = "Distribuzione in classi di età",
     xlab = "età",
     col = "purple", 
     xlim = c(2,66))
# alternativamente costruisco una nuova variabile
eta.cl = cut(PIPPO$Freq,c(2,18.5,25,66))
eta.cl = cut(PIPPO$Freq,c(2,18.5,25,66), 
             include.lowest = T)
# che è un fattore che indica l'appartenza alle classi
table(eta.cl)

# Funzione di ripartizione empirica ---------------------------------------
# consideriamo una variabile quantitativa
PIPPO$Freq

# fatto a mano:
# 1. metto in ordine le modalit?
sort(unique(PIPPO$Freq))
# 2. calcolo le frequenze associate a ciascuna modalit?
table(PIPPO$Freq)
# 3. cumulo le frequenze 
freq.cum = cumsum(table(PIPPO$Freq))
# 4. passo alle frequenze relative cumulate dividendo per la numerosit? totale n
n = length(PIPPO$Freq)
freq.rel.cum = freq.cum/n
# 5. rappresento la funzione di ripartizione empirica (step function)
plot(sort(unique(PIPPO$Freq)), freq.rel.cum,  type = "s")
# per finire...imbelletto un po'
plot(sort(unique(PIPPO$Freq)), freq.rel.cum,  type = "s",
     main = "Funzione di ripartizione empirica",
     xlab = "Età",
     ylab = "frequenza relativa cumulata")

# in un solo comando...
plot(ecdf(PIPPO$Freq))


# Altre rappresentazioni grafiche di base -----------------------------------------

# plot()

# a. una sequenza di dati numerici (nell'ordine in cui si presentano nel dataset...)
plot(PIPPO$Freq)

# b. una coppia di variabili quantitative
plot(x = PIPPO$Freq, y = PIPPO$Freq)

# c. una funzione "matematica"
x = seq(-10,10,.1)
y = sin(x)
plot(x,y)
plot(x,y,
     type ="l", 
     main = "Funzione y = sin(x)")
# oppure
curve(sin(x),-10,10)
# e poi si possono sovrapporre altre curve, ad esempio
curve(cos(x),-10,10, add=T, col = 3)

# d. una serie storica
data(WWWusage)
str(WWWusage)
plot(WWWusage)
?WWWusage

# e. una serie territoriale

install.packages("maps")
library(maps)
map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)
map("state")$names

map("world", interior = FALSE)
map("world", boundary = FALSE, col="gray", add = TRUE)
map("world")$names
# solo un esempio...che ovviamente non ha senso
# [E' come se la variabile che ho rilevato fosse la lunghezza del nome del paese...]
filled.states = terrain.colors(nchar(map("world")$names),.7) 
map("world", col = filled.states, fill = T, resolution = 0)



