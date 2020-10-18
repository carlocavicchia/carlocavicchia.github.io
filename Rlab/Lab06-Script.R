
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#             Lab06 * Cenni alla Distribuzione Normale *                  #
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

## Nota: in questo script ci sono tutti i comandi usati per riprodurre
## i grafici che trovate sulle slide di questo Lab 06...
## Come sempre, alcune cose sono essenziali, altre no.
## Per chiarezza le istruzioni unicamente finalizzate a "imbellettare"
## i grafici verranno di seguito evidenziate come *fronzoli*

# Esempio: il pi?? fesso... ------------------------------------------------

load("altezza.RData")

# il vettore altezza contiene 172 valori numerici riferiti 
# all'altezza di 172 donne

media.altezza = mean(altezza)
sd.altezza    = sd(altezza)
var.altezza=sd.altezza^2

# distribuzione empirica - istogramma
hist(altezza, col = rgb(.4,0,.6,1), main = "", xlab = "", prob = T, border = F)
# *fronzoli*:
# disegno la media
points(media.altezza,0, col = "green", lwd = 5)
# disegno la mediana
points(median(altezza),0, col = "yellow", pch = 19)
# aggiungo un titolo
title(expression("Distribuzione empirica : Istogramma"))  
# * *


# distribuzione empirica - istogramma + stima kernel
hist(altezza, col = rgb(.4,0,.6,.4), main = "", xlab = "", prob = T, border = F)
# nota: stiamo usando l'opzione prob = T per ottenere l'istogramma di densità
# sovrappongo all'istogramma la stima kernel di densità
lines(density(altezza), col = rgb(.4,0,.6,1), lwd = 5)
# *fronzoli*:
# disegno la media
points(media.altezza,0, col = "green", lwd = 5)
# disegno la mediana
points(median(altezza),0, col = "yellow", pch = 19)
# aggiungo un titolo
title(expression("Distribuzione empirica :: Istogramma & Stima Kernel"))
# * *


# distribuzione empirica -> modello teorico
hist(altezza, col = rgb(.4,0,.6,.4), main = "", xlab = "", prob = T, border = F)
# sovrappongo all'istogramma la stima kernel di densità
lines(density(altezza), col = rgb(.4,0,.6,1), lwd = 5)
# sovrappongo (opzione add = T) all'istogramma la curva di densità di una normale con 
# parametri media.altezza e sd.altezza
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), add = T)
# *fronzoli*:
# disegno la media
points(media.altezza,0, col = "green", lwd = 5)
# disegno la mediana
points(median(altezza),0, col = "yellow", pch = 19)
# aggiungo la legenda
legend("topright", c("densità kernel", "densità normale"), 
       col = c(rgb(.4,0,.6,1),  rgb(0,.4,.6,1)),
       lwd = c(5))
# aggiungo un titolo
title(expression("Dalla distribuzione empirica al modello teorico"))
# * *


# modello teorico -> Normale(mu,sigma)
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "")
title(expression(paste("Modello teorico :: Normale(",mu,",",sigma,")")))

# modello teorico -> Normale standardizzata (0,1)
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
title(expression(paste("Standardizzazione :: Normale(",mu," = 0,",sigma," = 1)")))

# al variare della media, la curva trasla

curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(-4,4), ylab = "density", xlab = "")
# disegno la media
points(0,0, col = "green", lwd = 5)

curve(dnorm(x,1,1), lwd = 5, col =  rgb(0,.4,.6,.5), 
      xlim = c(-4,4), ylab = "density", xlab = "", add = T)
# disegno la media
points(1,0, col = rgb(0,.4,.6,.5), lwd = 5)

curve(dnorm(x,-1,1), lwd = 5, col =  rgb(0,.4,.6,.3), 
      xlim = c(-4,4), ylab = "density", xlab = "", add = T)
# disegno la media
points(-1,0, col = rgb(0,.4,.6,.3), lwd = 5)
title(expression(paste("Al variare di ", mu)))


# al variare della deviazione standard, la curva diventa più o meno concentrata
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(-4,4), ylab = "density", xlab = "")
# disegno la media
points(0,0, col = "green", lwd = 5)

curve(dnorm(x,0,2), lwd = 5, col =  rgb(0,.4,.6,.5), 
      xlim = c(-4,4), ylab = "density", xlab = "", add = T)

curve(dnorm(x,0,0.5), lwd = 5, col =  rgb(0,.4,.6,.3), 
      xlim = c(-4,4), ylab = "density", xlab = "", add = T)
title(expression(paste("Al variare di ", sigma)))

# area sotto la curva
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
# *fronzoli*:
# qui stiamo colorando l'area sottesa dalla curva...(decisamente un optional!!!!)
xx = seq(-4,4,.01)
polygon(c(xx, rev(xx)), c(dnorm(xx,0,1),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), add = T, 
      xlim = c(-4,4), ylab = "density", xlab = "")
# solo per scrivere il numeretto 1 nella parte colorata
text(0,0.2,"1", col = "white", lwd = 3)
title(expression(paste("Area sotto la funzione di densità")))
# * *

# mediana
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
points(0,0, col = "yellow", pch = 19)
# *fronzoli*:
# qui stiamo colorando l'area sottesa dalla curva...(decisamente un optional!!!!)
xx = seq(-4,0,.01)
polygon(c(xx, rev(xx)), c(dnorm(xx,0,1),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), add = T, 
      xlim = c(-4,4), ylab = "density", xlab = "")
text(-0.5,0.2,"0.5", col = "white", lwd = 3)
text(-0.5,0.2,"0.5", col = "white", lwd = 3)
text(0.5,0.2,"0.5", col = rgb(0,.4,.6,1), lwd = 3)
text(0.5,0.2,"0.5", col = rgb(0,.4,.6,1), lwd = 3)
title(expression(paste("Mediana")))
# * *

# regola 68 - 95 - 99.7
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
# *fronzoli*:
# qui stiamo colorando l'area sottesa dalla curva...(decisamente un optional!!!!)
xx = seq(-1,1,.01)
polygon(c(xx, rev(xx)), c(dnorm(xx,0,1),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), add = T, 
      xlim = c(-4,4), ylab = "density", xlab = "")
text(0,0.2,"0.68", col = "white", lwd = 3)
text(0,0.2,"0.68", col = "white", lwd = 3)
abline(v = c(-1,1), col =  rgb(0,.4,.6,1), lty = 3)
title(expression(paste("Regola 68 - 95 - 99.7")))

curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
# *fronzoli*:
# qui stiamo colorando l'area sottesa dalla curva...(decisamente un optional!!!!)
xx = seq(-2,2,.01)
polygon(c(xx, rev(xx)), c(dnorm(xx,0,1),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), add = T, 
      xlim = c(-4,4), ylab = "density", xlab = "")
text(0,0.2,"0.95", col = "white", lwd = 3)
text(0,0.2,"0.95", col = "white", lwd = 3)
abline(v = c(-2,2), col =  rgb(0,.4,.6,1), lty = 3)
title(expression(paste("Regola 68 - 95 - 99.7")))

curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), xlim = c(-4,4), ylab = "density", xlab = "")
# *fronzoli*:
# qui stiamo colorando l'area sottesa dalla curva...(decisamente un optional!!!!)
xx = seq(-3,3,.01)
polygon(c(xx, rev(xx)), c(dnorm(xx,0,1),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
curve(dnorm(x,0,1), lwd = 5, col =  rgb(0,.4,.6,1), add = T, 
      xlim = c(-4,4), ylab = "density", xlab = "")
text(0,0.2,"0.997", col = "white", lwd = 3)
text(0,0.2,"0.997", col = "white", lwd = 3)
abline(v = c(-3,3), col =  rgb(0,.4,.6,1), lty = 3)
title(expression(paste("Regola 68 - 95 -99.7")))

# Esempio: 
# a) proporzione di donne alte meno di 178 ??
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "")
# segnalo il punto 178
points(178,0, col = rgb(0,.4,.6,1), lwd = 5, pch = 17)
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "")
# coloro l'area sottesa dalla curva fino a 178...
xx = seq(min(altezza),178,.1)
polygon(c(xx, rev(xx)), c(dnorm(xx,media.altezza,sd.altezza),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
text(163,0.02,"0.985", col = "white", lwd = 3)
text(163,0.02,"0.985", col = "white", lwd = 3)
# *fronzoli*:
# disegno una linea verticale
abline(v = c(178), col =  rgb(0,.4,.6,1), lty = 3)
# ripasso la curva
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "", add = T)
# ci aggiungo una freccina (pch = 17 ?? il tipo di punto :)
points(178,0, col = rgb(0,.4,.6,1), lwd = 5, pch = 17)
title(expression(paste("Esempio: altezza donne N(",mu," = 163, ", sigma, " = 6.9)")),
      sub = "Quale è la proporzione di donne alte meno di 178 cm?")


# Esempio: 
# b) 90-mo percentile dell'altezza delle donne è
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "")
# *fronzoli*:
# coloro un'area pari a 0.90
xx = seq(min(altezza),qnorm(0.90,163,6.9),.1)
polygon(c(xx, rev(xx)), c(dnorm(xx,media.altezza,sd.altezza),rep(0,length(xx))),
        col=rgb(0,.4,.6,.5), border = "white")
abline(v = c(qnorm(0.90,163,6.9)), col =  rgb(0,.4,.6,1), lty = 3)
curve(dnorm(x,media.altezza,sd.altezza), lwd = 5, col =  rgb(0,.4,.6,1), 
      xlim = c(range(altezza)), ylab = "density", xlab = "", add = T)
text(163,0.02,"0.90", col = "white", lwd = 3) 
points(qnorm(0.90,163,6.9),0, col = rgb(0,.4,.6,1), lwd = 5, pch = 17)
title(expression(paste("Esempio: altezza donne N(",mu," = 163, ", sigma, " = 6.9)")),
      sub = "Quale è il 90esimo percentile della distribuzione dell'altezza?")

# Ho barato...  -----------------------------------------------------------

# I dati erano estratti casualmente usando la funzione rnorm!!

media.altezza = 163
sd.altezza = 9.6
altra.altezza = rnorm(172, media.altezza, sd.altezza)

# sono dati "simulati"...ovviamente, ogni volta che usate la funzione rnorm
# ottenete valori diversi....provare per credere...
rnorm(172, media.altezza, sd.altezza)
hist(rnorm(172, media.altezza, sd.altezza))
hist(rnorm(172, media.altezza, sd.altezza))
hist(rnorm(172, media.altezza, sd.altezza))
hist(rnorm(172, media.altezza, sd.altezza))
hist(rnorm(172, media.altezza, sd.altezza))
## convinti???

##############################################################################
#   A futura memoria: Distribuzioni (discrete e continue) celebri            #     
##############################################################################
# Per le principali variabili aleatorie abbiamo a disposizione:              #                       
#   1) funzione di densita'                 -> prefisso - "d"                #
#   2) funzione di ripartizione             -> prefisso - "p"                #  
#   3) quantili                             -> prefisso - "q"                #
#   4) generatore di numeri pseudo-casuali  -> prefisso - "r"                # 
##############################################################################

