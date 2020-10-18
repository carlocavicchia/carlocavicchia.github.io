
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#       Lab05 ** Indici di variabilità **                                 # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

decathlon <- read.csv("dati.csv")


# concentriamoci sulla variabile Long.jump
x = decathlon$highjump

# una sintesi complessiva
summary(x)
 
# la funzione summary restituisce un output "dedicato" a seconda del
# tipo di oggetto che riceve come input
# quindi ad esempio per un factor (variabile qualitativa)
# ci restituisce la tabella di frequenza
decathlon$fact <- factor(c(rep(1,10),rep(2,15)))
summary(decathlon$fact)

# Minimo&Massimo
Min = min(x)
Min
Max = max(x)
Max

# mediana
median(x)
Me = median(x)
Me 

quantile(x)
probs = seq(0, 1, 0.25)

# quartili
Q1 = quantile(x, probs = 0.25)
Q1
Q3 = quantile(x,0.75)
Q3

# Dove li vedete di nuovo tutti insieme appassionatamente??

boxplot(x, col = "yellowgreen", main = "Boxplot")

text(0.7, Me, "Mediana")
text(0.7, Q1, "1° Quartile")
text(0.7, Q3, "3° Quartile")

text(0.7, Min, "Minimo")
text(0.7, Max, "Massimo")

# e la media
media = mean(x, na.rm = T)
points(media, col = "yellow", pch = "*", cex = 3)

# notate la corrispondenza tra l'istogramma e il boxplot
par(mfrow = c(2,1))
hist(x)
boxplot(x, horizontal = T)

par(mfrow = c(2,1))
hist(x)
boxplot(x, horizontal = T, range = 0) # l'opzione range = 0 fa sì che i baffi vadano dal Min al Max


# Indici di variabilità ---------------------------------------------------


# Campo di Variazione
Range = Max - Min
Range
diff(range(x))

# Differenza Interquartile
Diff.IntQ = Q3 - Q1
Diff.IntQ

# Varianza
# Calcoliamocela pezzo per pezzo:

# * scarti dalla media:
x - mean(x)

# (che peraltro sappiamo che sommano a 0)
sum(x - mean(x))
# (...praticamente 0...)

# * scarti dalla media elevati al quadrato
(x - mean(x))^2

# * somma degli scarti dalla media elevati al quadrato
sum((x - mean(x))^2)
# (detta anche DEVIANZA)

# * somma degli scarti dalla media elevati al quadrato diviso n
n = length(x)
sum((x - mean(x))^2)/n
# o equivalentemente
# * media degli scarti dalla media elevati al quadrato
mean((x - mean(x))^2)

# Ma ti pare che qualcuno non ci ha pensato prima a una funzione che la calcola???
var(x)
# ops! Ma non è uguale...come mai??

# In effetti quella che ci propone R è la cosiddetta varianza campionaria corretta...
sum((x - mean(x))^2)/(n-1)
# che è tale e quale alla nostra ma divisa per (n-1)
# Il motivo vero sarà noto quando studierete inferenza
# Per adesso possiamo comunque dire che quando n è grande c'è poca differenza, giusto?
# e per ottenere quello che intediamo noi...possiamo sempre usare 
var(x)
# e "correggerla" moltiplicando per un fattore (n-1)/n
sigma2 = var(x)*(n-1)/n
sigma2

# Deviazione standard o Scarto quadratico medio
# * è definito come la radice quadrata della varianza
sigma = sqrt(sigma2)
sigma
# anche qui esiste la funzione
sd(x)
# che è calcolata coerentemente rispetto a var() 
# e per ottenere quello che intediamo noi...possiamo sempre "correggere"
sigma = sd(x)*sqrt((n-1)/n)
sigma

# Coefficiente di variazione
cv = sigma/media

# Median Absolute Deviation
mad(x)

# Piccola <IMPORTANTE> parentesi: Definizione di funzioni -----------------

# Possiamo definire anche noi delle nuove funzioni!!

# ad esempio ci può servire una funzione che ci restituisce la varianza non corretta

sigma2.fun = function(dati){
  
  n = length(dati)
  
  sigma2 = var(dati)*(n-1)/n
  
  return(sigma2)
}

mode(sigma2.fun)
sigma2.fun(x)

# o analogamente la deviazione standard non corretta

sigma.fun = function(dati){
  
  n = length(dati)
  
  sigma = sqrt(var(dati)*(n-1)/n)
  
  return(sigma)
}

mode(sigma.fun)
sigma.fun(x)

# o ancora il coefficiente di variazione

cv.fun = function(dati){
  
  n = length(dati)
  
  sigma = sqrt(var(dati)*(n-1)/n)
  
  cv = sigma/mean(dati)
  
  return(cv)
}

mode(cv.fun)
cv.fun(x)

myvar.fun = function(dati){
  
  n = length(dati)
  sigma2 = var(dati)*(n-1)/n
  sigma = sqrt(sigma2)
  cv = sigma/mean(dati)
  rr = range(dati)
  
  out = list(sigma2 = sigma2, sigma = sigma, cv = cv, rr = rr)
  
  return( out )
}

indici.var = myvar.fun(x)
indici.var$sigma

# Analisi per gruppi ------------------------------------------------------

# Consideriamo i diversi sottogruppi ottenuti stratificando per Competizione
par(mfrow = c(1,1))
boxplot(highjump ~ fact, data = decathlon, col = topo.colors(6))

aggregate(highjump ~ fact, data = decathlon, FUN = min)
aggregate(highjump ~ fact, data = decathlon, FUN = max)
aggregate(highjump ~ fact, data = decathlon, FUN = range)

aggregate(highjump ~ fact, data = decathlon, FUN = quantile)
aggregate(highjump ~ fact, data = decathlon, FUN = quantile, c(0.33))

aggregate(highjump ~ fact, data = decathlon, FUN = var)
aggregate(highjump ~ fact, data = decathlon, FUN = sd)
aggregate(highjump ~ fact, data = decathlon, FUN = sigma.fun)
aggregate(highjump ~ fact, data = decathlon, FUN = sigma2.fun)


