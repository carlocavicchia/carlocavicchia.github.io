
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#      Lab10 ** Correlazione lineare e Regressione lineare semplice **    #
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #


# Relazione tra due variabili quantitative --------------------------------

# Riprendiamo i dati su cui abbiamo lavorato in classe 

load("dati.TI.RData")


# Questi sono i dati che abbiamo usato a lezione 
# che riguardano uno studio della Transparency International
# su 27 paesi europei per i quali vengono  misurati gli indici:
# - HDI Human development index: 
# misura il livello di sviluppo di un paese sulla base di tre aspetti: 
# aspettativa di vita alla nascita, numero medio di anni di istruzione, 
# reddito nazionale lordo pro capite
# - CPI Corruption perception index: misura il livello di integrità 
# percepito dai cittadini (minor valore maggiore corruzione) 

# La prima cosa che avete fatto nell'Ese G è rappresentare graficamente i dati:

X = dati.TI$HDI
Y = dati.TI$CPI

plot(X, Y, pch = 19, col = rgb(.1,.4,.7),
     xlab = "Human Development Index",
     ylab = "Corruption Perception Index")
grid()

plot(scale(X), scale(Y), pch = 19, col = rgb(.1,.4,.7),
     main = "* variabili standardizzate *", 
     xlim = c(range(scale(X))),
     ylim = c(range(scale(Y))))
abline(h = 0, col = rgb(.1,.4,.7,.5), lwd = 3)
abline(v = 0, col = rgb(.1,.4,.7,.5), lwd = 3)
grid()

# Correlazione ------------------------------------------------------------

# Per calcolare il coefficiente di correlazione lineare, 
# per implementare la prima formula, ho bisogno di:

N = length(X)

mu.X = mean(X)
sd.X = sqrt(sum((X-mu.X)^2)/N)

z.X = (X-mu.X)/sd.X
# o più brevemente
z.X = scale(X)

mu.Y = mean(Y)
sd.Y = sqrt(sum((Y-mu.Y)^2)/N)

z.Y = (Y-mu.Y)/sd.Y
# o più brevemente
z.Y = scale(Y)

r.1 = mean(z.X * z.Y)

# per implementare la seconda formula, ho bisogno di:

Sxy = sum(X*Y)
S1x = sum(X)
S1y = sum(Y)
S2x = sum(X^2)
S2y = sum(Y^2)

r.2 = (N*Sxy - S1x*S1y)/sqrt((N*S2x - S1x^2)*(N*S2y - S1y^2))

# la cosa più ovvia e più rapida in R è usare 
?cor
r = cor(X,Y)

r.1
r.2
r


# Modello di regressione lineare semplice ------------------------------------------------------

# A questo punto interpretiamo:
#     Y come variabile risposta e 
#     X come variabile esplicativa
# e calcolare i coefficienti della retta di regressione 
# con il metodo dei minimi quadrati

# In base alle formule che abbiamo ricavato:

beta1 = (Sxy- S1x*S1y/N)/(S2x -S1x^2/N)
beta1

beta0 = mean(Y)-beta1*mean(X)
beta0

# quindi per sovrapporre allo scatterplot la retta di regressione 
# è sufficiente aggiungere il comando abline
plot(X, Y, pch = 19, col = rgb(.1,.4,.7),
     xlab = "Human Development Index",
     ylab = "Corruption Perception Index")
grid()

abline(beta0,beta1, col = rgb(.6,.1,.6,.5), lwd = 5)

# è facile anche controllare la relazione che abbiamo determinato
# tra r e beta1:

r*sd(Y)/sd(X)

beta1

# ed è anche immediato calcolare l'indice di determinazione multipla R2
# come quadrato del coefficiente di correlazione lineare

R2 = cor(X,Y)^2
R2

# o come quota della variabilità totale spiegata dal modello

# calcolando prima i valori predetti
Y.pred = beta0 + beta1*X
# poi la devianza del modello
Dev.mod = sum((Y.pred - mu.y)^2)
# e la devianza totale
Dev.tot = sum((Y - mu.y)^2)

Dev.mod/Dev.tot

# o equivalentemente i residui
res = Y-Y.pred
# poi la devianza residua
Dev.res = sum((Y - Y.pred)^2)

1 - Dev.res/Dev.tot

# Anche qui, come potete immaginare, tutto ciò è già stato implementato
# in un'unica funzione R che ci consente di ottenere un output piuttosto completo:

?lm
modello.regressione = lm(Y ~ X)
modello.regressione
class(modello.regressione)
# modello.regressione è un oggetto di classe lm (sta per linear model)
# cioè proprio un oggetto che è strutturato come output di un modello lineare
# perciò alcune funzioni che abbiamo già usato (ad es. abline, plot, summary) 
# si comporteranno in modo appropriato per questo tipo di oggetto
# (ricordate quando accennavamo alla filosofia object oriented alla base di R???)

# Ripartendo dallo scatterplot basta questo:

plot(X, Y, pch = 19, col = rgb(.1,.4,.7),
     xlab = "Human Development Index",
     ylab = "Corruption Perception Index")
grid()

abline(modello.regressione, col = rgb(.6,.1,.6,.5), lwd = 5)

# cosa c'è dentro a modello.regressione?
summary(modello.regressione)

# Call:
#   lm(formula = Y ~ X)                         ----> Modello che abbiamo scelto di adattare
# 
# Residuals:                                    ----> Sintesi della distribuzione dei residui
#   Min       1Q   Median       3Q      Max 
# -0.04947 -0.02435 -0.00823  0.02258  0.06410 
# 
# Coefficients:                                 ----> Valori dei coefficienti
#                                                     (N.B. il resto riguarda aspetti inferenziali
#                                                           che per il momento non conoscete)
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.736200   0.020432  36.032  < 2e-16 ***
#   X           0.015630   0.003112   5.022 3.52e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02985 on 25 degrees of freedom   
# Multiple R-squared:  0.5022                   ----> Indice di determinazione multipla R2
# Adjusted R-squared:  0.4823 
# F-statistic: 25.22 on 1 and 25 DF,  p-value: 3.522e-05

mode(modello.regressione)
# è una lista 

names(modello.regressione)
# da cui possiamo estrarre (con il solito $) i singoli elementi dell'output, tra cui:

# coefficienti beta0 e beta1
modello.regressione$coefficients

# residui
modello.regressione$residuals

# valori predetti
modello.regressione$fitted.values

# Anche la funzione plot ha un comportamento "dedicato":
par(mfrow = c(2,2))
plot(modello.regressione)
# anche l'interpretazione di questo output va al di là delle vostre conoscenze
# attuali, ma magari un giorno ve ne ricorderete...


# Osservazioni anomale ----------------------------------------------------

# Lavoriamo ora su un nuovo set di dati estratti dal database della FAO
# che riguardano il cosiddetto Food Balance Sheet (FBS)

load("FBS.RData")

# Rappresentiamo la relazione tra Production e Tot.pop
# in un diagramma a dispersione
plot(FBS$Tot.pop,FBS$Production, 
     col = rgb(.7,0,.2,.5), pch = 19, 
     xlab = "Population", ylab = "Production", 
     xlim = c(0,1500000))
cor(FBS$Production,FBS$Tot.pop, use="complete")
# l'opzione use = "complete" serve a risolvere il problema della presenza di dati mancanti (NA)

# Possiamo identificare i (due) paesi con un elevato livello di Production e Tot.pop
plot(FBS$Tot.pop,FBS$Production, 
     col = rgb(.7,0,.2,.5), pch = 19, 
     xlab = "Population", ylab = "Production", xlim = c(0,1500000))
# in modo interattivo
identify(FBS$Tot.pop,FBS$Production, labels=FBS$Country)

# o anche 
plot(FBS$Tot.pop,FBS$Production, 
     col = "white", xlab = "Population", ylab = "Production", 
     xlim = c(0,1500000))
text(FBS$Tot.pop,FBS$Production,labels=as.character(FBS$Country), 
     cex = 2*FBS$Tot.pop/1000000, col = 4)

# Proviamo ad adattare un modello di regressione lineare semplice...
plot(FBS$Tot.pop,FBS$Production, 
     col = rgb(.7,0,.2,.5), pch = 19, 
     xlab = "Population", ylab = "Production", xlim = c(0,1500000))
Prod.mod = lm(Production ~ Tot.pop, data = FBS)
summary(Prod.mod)
abline(Prod.mod, col = rgb(.2,0,.7,.5), lwd = 5)

# Cosa succede se escludiamo i due paesi anomali??
FBS.2 = FBS[(FBS$Country != "China") & (FBS$Country != "India"),]
dim(FBS.2)
cor(FBS.2$Production,FBS.2$Tot.pop, use = "complete")

points(FBS.2$Tot.pop,FBS.2$Production, 
       col = rgb(.2,0,.7,.5), pch = 19, 
       xlab = "Population", ylab = "Production",xlim = c(0,1500000))
Prod.mod.2 = lm(Production ~ Tot.pop, data = FBS.2)
summary(Prod.mod.2)
abline(Prod.mod.2, col = rgb(.1,.7,.1,.5), lwd = 5)

# Come possiamo interpretare questo risultato?


# Più in generale vediamo cosa succede quando ci sono dei dati anomali....

x = scale(rnorm(100,0,1))
y = scale(2*x + 1 + rnorm(100,0,1))
cor(x,y)^2

plot(x, y, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y ~ x)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)

# inserisco un dato anomalo per le x e le y
x1 = c(x,5)
y1 = c(y,5)
cor(x1,y1)^2

plot(x1, y1, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y1 ~ x1)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)

# inserisco un dato anomalo per le x ma in media con le y

x2 = c(x,5)
y2 = c(y,mean(y))
cor(x2,y2)^2

plot(x2, y2, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y2 ~ x2)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)


# inserisco un dato anomalo per le y ma in media con le x

x3 = c(x,mean(x))
y3 = c(y,5)
cor(x3,y3)^2

plot(x3, y3, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y3 ~ x3)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)


# partendo da una situazione di incorrelazione

x4 = scale(rnorm(100,0,1))
y4 = scale(rnorm(100,0,1))
cor(x4,y4)^2

plot(x4, y4, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y4 ~ x4)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)

# inserisco un dato anomalo per le x e per le y

x5 = c(x4,5)
y5 = c(y4,5)
cor(x5,y5)^2

plot(x5, y5, pch = 19, col = rgb(.1,.4,.7), xlim = c(-2,5), ylim = c(-2,5))
box()
grid()
mod = lm(y5 ~ x5)
abline(mod, col = rgb(.6,.1,.6,.3), lwd = 6)
