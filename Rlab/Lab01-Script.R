
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#       Lab01 ** Introduzione all'uso di R e di Rstudio **                # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

# step 0. installare R e Rstudio ------------------------------------------

# scaricare l'ultima versione di R da questo link 
# (in base al proprio sistema operativo: Windows, MacOS, Linux): 
# http://cran.mirror.garr.it/mirrors/CRAN/
# continuare a cliccare su download
# scaricare il file eseguibile
# procedere all'installazione base (dire sempre 'continua' fino alla 'fine')

# scaricare la versione Desktop di Rstudio da questo link
# (in base al proprio sistema operativo: Windows, MacOS, Linux): 
# http://www.rstudio.com/products/rstudio/download/
# scaricare il file eseguibile
# procedere all'installazione base (dire sempre 'continua' fino alla 'fine')

# se tutto è andato a buon fine da questo momento in poi 
# potete usare R attraverso l'interfaccia Rstudio


# step 1. aprire un progetto in una cartella di lavoro --------------------

# in alto a destra cliccare su 'Project:(None)'
# cliccare su 'Create project' - 'New directory' - 'Empty Project'
# assegnare un nome alla nuova cartella di lavoro

# step 2. guardarsi un po' intorno: i quattro pannelli --------------------

# a. PANNELLO in ALTO a SINISTRA
# per prima cosa dall'icona in alto a sinistra creare un nuovo script
# per salvare il nuovo script cliccare su salva, assegnare un nome
# notare che il file di estensione .R risulterà salvato nella cartella di lavoro

# b. PANNELLO in BASSO a SINISTRA
# è la Console di R in cui vengono eseguite le linee di comando
# (direttamente o mandandole in esecuzione dallo script <Ctrl+Invio>)
 
# c. PANNELLO in ALTO a DESTRA
# contiene due tab:
# * History: tiene memoria di tutti i comandi eseguiti nel corso della sessione
# * Environment: elenca tutti gli oggetti creati nell'area di lavoro

# d. PANNELLO in BASSO a DESTRA
# contiene cinque tab:
# * Files: consente di esplorare i file sul vostro pc
#          di default punta alla cartella di lavoro in cui avete aperto il progetto
# * Help: finestra della documentazione di supporto a tutte le funzioni R
# * Plots: device grafica in cui vengono mostrati i plot
# * Packages: lista dei pacchetti aggiuntivi disponibilità
# * Viewer: finestra per vedere pdf creati con R


# step 3. chiedi (sullo script) e ti sarà dato (sulla console) ------------

# istruzioni elementari...ad esempio operazioni
2 + 2
1 * 5
log(2)
# notare che log è una funzione
# per questo richiede le parentesi tonde ()
# all'interno delle quali si può inserire un argomento numerico
# in questo caso un numero di cui vogliamo calcolare il logaritmo

# step 4. ricordarsi che si può sempre cercare aiuto...HELP! --------------

# per capire meglio come funziona...
?log
# notare in particolare: 
# la descrizione, l'uso, gli esempi e i link a funzioni affini

# a proposito dell'uso:
log(2)
log(2, base = exp(1))
# danno lo stesso risultato perché il secondo argomento della funzione
# che consente di specificare la base del logaritmo, ha come valore di 
# default exp(1), cioè il numero e di Nepero 
# in altre parole: di default viene restituito il logaritmo naturale,
# ma se vogliamo calcolare il logaritmo in base 2
log(2, base = 2)
# viceversa per l'argomento x non è previsto un default
# e quindi viene restituito un messaggio di errore
log(, base = 2)


# step 5. cominciare a creare oggetti -------------------------------------

# per creare un nuovo oggetto è necessario scegliere un nome
# e usare l'operatore di assegnazione che è l'uguale =
numero = 2
numero
# si crea un nuovo oggetto nel Workspace
# attenzione! le maiuscole contano (ovvero R è case sensitive)
NUMERO = 1+1
NUMERO
# è un oggetto distinto dal precendente

# che tipo di oggetto abbiamo creato?
mode(numero)
# è un oggetto di tipo numerico...

# ma esistono anche altri tipo di oggetti in R...
# ad esempio, se vogliamo confrontare i due oggetti che abbiamo appena creato
# possiamo chiederci in prima battuta se sono uguali o no:
numero == NUMERO
# notare la differenza tra:
# * uguale =          che serve per assegnare
# * doppio uguale ==  che serve per confrontare
# la risposta che riceviamo è del tipo TRUE/FALSE, cioè una risposta binaria
    # altri possibili confronti:
    numero != NUMERO
    numero > NUMERO
    numero >= NUMERO

numero_new <- 9
# a questo punto assegnando il risultato di uno dei precedenti confronti
# ad un nuovo oggetto, possiamo creare un oggetto di tipo logical
risposta = numero == NUMERO
mode(risposta)
    # un oggetto di tipo logical può essere anche creato così:
    risposta2 = T
    risposta3 = TRUE
    risposta4 = F
    risposta5 = FALSE

# per finire, ci siamo accorti che alcuni output sulla console vengono 
# mostrati tra virgolette " "
# ad esempio
mode(risposta)
# l'output della funzione mode può essere a sua volta assegnato a un nuovo oggetto
# che sarà una stringa di caratteri ovvero di tipo character
stringa = mode(risposta)
    # un oggetto di tipo character può essere anche creato così:
    stringa2 = "Ci scrivo quello che voglio..."

# ricapitolando i tre tipi di oggetti che abbiamo visto:
# * numeric
# * logical
# * character


# step 6. usare un minimo di fantasia -------------------------------------

# è possibile definire un vettore (ad esempio numerico),
# mettendo insieme vari elementi in un unico oggetto,
# ad esempio usando la funzione c(elemento1,elemento2,...)
vettore = c(numero,numero+1,numero*5,45,50)

# perché un minimo di fantasia?
# perché molte funzioni R hanno dei nomi facili da intuire e da memorizzare
# ad esempio:
sum(vettore)
# ci darà la somma
min(vettore)
# ci darà il minimo
max(vettore)
# ci darà il massimo

# step 7. ragionare sulla diversa natura degli oggetti --------------------

# abbiamo visto che esistono oggetti di natura differente
# ma che succede se proviamo a combinarli?
mix = c(numero,NUMERO,risposta,stringa,stringa2)
# i singoli elementi vengono "forzati" tutti ad essere stringhe di caratteri
# (notare le virgolette)

# stessa cosa se vogliamo affiancare più vettori (della stessa lunghezza) 
# in particolare, immaginiamo di aver rilevato due variabili statistiche 
# su un collettivo di 5 unità:
# ad es. altezza è una variabile quantitativa:
altezza = c(165,169,173,162,170)
# occhi è una variabile qualitativa:
occhi = c("azzurri","neri","verdi","marroni","marroni")
# se per organizzare i nostri dati nel formato di una matrice a partire
# dai due vettori di 5 elementi altezza e occhi usassimo la funzione cbind
cbind(altezza, occhi) # matrice 5 x 2
# oppure la funzione rbind
rbind(altezza, occhi) # matrice 2 x 5
# forzeremmo tutti gli elementi a diventare stringhe di carattere
# cosa che non va bene se vogliamo mantenere la natura numerica dell'altezza

# quindi la matrice non va bene, vediamo una soluzione...

# step 8. costruire un dataframe ------------------------------------------

# il modo giusto per organizzare i nostri dati è il data.frame
dati = data.frame(altezza,occhi)
dati
# notare la struttura tipica della distribuzione per unità 
# in questo caso abbiamo 5 osservazioni per due variabili
str(dati)

# in effetti quando importiamo dei dati da un file esterno
# vengono organizzati in R come data.frame. Vediamo come:

# step 9. utilizziamo iris, dataset già implementato in R

PIPPO = iris
str(PIPPO)

# step 10. ottenere una semplice distribuzione di frequenza ---------------

# per lavorare su una singola variabile si usa il dollaro, ad esempio
PIPPO$Species
# estrae dal dataframe PIPPO la variabile species

# qualche piccola operazione su cui torneremo...

# il comando fondamentale per ottenere delle tabelle è table
table(PIPPO$Species)
n = sum(table(PIPPO$Species))

# costruiamo la distribuzione di frequenze
freq.ass = table(PIPPO$Species)
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
barplot(table(PIPPO$Species))
barplot(table(PIPPO$Species), col = c("pink","lightblue","lightgreen"))
barplot(table(PIPPO$Species), col = c("pink","lightblue","lightgreen"), 
        main = "Distribuzione di frequenza", sub = "Variabile :: species")

pie(table(PIPPO$Species), col = c("pink","lightblue","lightgreen"))
title(main = "Distribuzione di frequenza", sub = "Variabile :: species")

# N.B. Tra i due grafici è da preferire il barplot, 
# perchè l'occhio umano è in grado di cogliere e quindi confrontare meglio
# delle lunghezze piuttosto che delle aree!

# The End. 

# lavorando con il progetto potete anche solo preoccuparvi di salvare lo script
# chiudere Rstudio, certi di ritrovare le cose come le avevate lasciate 
# alla prossima apertura