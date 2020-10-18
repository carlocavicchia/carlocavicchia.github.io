
# *********************************************************************** #
#     METODI E MODELLI QUANTITATIVI DI SUPPORTO ALLE DECISIONI (MEMO)     #
#                      Lab07 ** Concentrazione **                         # 
#                       Dr. Carlo Cavicchia                               # 
# *********************************************************************** #

# Concentrazione ----------------------------------------------------------

# Vediamo un esempio di rappresentazione della spezzata di concentrazione
# e calcolo del rapporto di concentrazione R

load("friends.RData")

### Opzione 1 ###
x = friends$opz1
n = length(x)

# Procediamo in base ai seguenti step:

# 1. ordiniamo i valori osservati
x = sort(x) # notate che andiamo a sovrascrivere sul vettore x di partenza

# 2. aggiungiamo al vettore x il valore x_0 = 0 (solo per comodità)
x = c(0,x)

# 3. definiamo le p_i (frazione delle i unit?? pi?? povere sul totale)
i = 0:n
p = i/n

# 4. definiamo le q_i (quota del reddito appartenente alle i unit?? pi?? povere sul totale)
q = cumsum(x)/sum(x)

# 5. costruiamo la spezzata di concentrazione
# prima i punti di coordinate (p_i,q_i)
plot(p,q, pch = 19, cex = .6)
# poi la spezzata che li congiunge
lines(p,q, type = "l")

# sovrappongo una griglia
grid()

# 6. infine calcoliamo l'indice di Gini
G = 2*sum(p-q)/(n-1)
G
### ==> L'Opzione 1 corrisponde al caso di Massima concentrazione!!! ###


### Opzione 2 ###
x = friends$opz2
n = length(x)

# Procediamo in base ai seguenti step:

# 1. ordiniamo i valori osservati
x = sort(x) # notate che andiamo a sovrascrivere sul vettore x di partenza

# 2. aggiungiamo al vettore x il valore x_0 = 0 (solo per comodit??)
x = c(0,x)

# 3. definiamo le p_i (frazione delle i unit?? pi?? povere sul totale)
i = 0:n
p = i/n

# 4. definiamo le q_i (quota del reddito appartenente alle i unit?? pi?? povere sul totale)
q = cumsum(x)/sum(x)

# 5. costruiamo la spezzata di concentrazione
# prima i punti di coordinate (p_i,q_i)
plot(p,q, pch = 19, cex = .6)
# poi la spezzata che li congiunge
lines(p,q, type = "l")
# sovrappongo una griglia
grid()

# 6. infine calcoliamo l'indice di Gini
G = 2*sum(p-q)/(n-1)
G

### ==> L'Opzione 2 corrisponde al caso di Equidistribuzione!!! ###


### Opzione 3 ###
x = friends$opz3
n = length(x)

# Procediamo in base ai seguenti step:

# 1. ordiniamo i valori osservati
x = sort(x) # notate che andiamo a sovrascrivere sul vettore x di partenza

# 2. aggiungiamo al vettore x il valore x_0 = 0 (solo per comodit??)
x = c(0,x)

# 3. definiamo le p_i (frazione delle i unit?? pi?? povere sul totale)
i = 0:n
p = i/n

# 4. definiamo le q_i (quota del reddito appartenente alle i unit?? pi?? povere sul totale)
q = cumsum(x)/sum(x)

# 5. costruiamo la spezzata di concentrazione
# prima i punti di coordinate (p_i,q_i)
plot(p,q, pch = 19, cex = .6)
# poi la spezzata che li congiunge
lines(p,q, type = "l")
# sovrappongo una griglia
grid()

# 6. disegniamo anche la retta di equidistribuzione (p_i = q_i per ogni i)
p.equi = p
q.equi = p
points(p.equi, q.equi, col = "green3", cex = 0.6, pch = 19)
lines(p.equi,q.equi, type = "l", col = "green3")

# 7. disegniamo anche la spezzata che si avrebbe nel caso di massima concentrazione
# (le prime n-1 unit?? non hanno nulla, una unit?? l'intero ammontare)
p.max = p
q.max = c(rep(0,n),1)
points(p.max, q.max, col = "purple", cex = 0.6, pch = 19)
lines(p.max,q.max, type = "l", col = "purple")

legend(0.02,1,
       legend = c("Opzione 1: massima concentrazione", 
                  "Opzione 2: equidistribuzione",
                  "Opzione 3: concentrazione"),
       col = c("purple", "green2", "black"), 
       lty = 1, pch = 19, cex = 0.6, bty = "n")

# la funzione polygon() ci consente di colorare le aree che ci interessano...
polygon(x = c(0,(n-1)/n,1), y = c(0,0,1), col = rgb(.4,0,.6,.2), border = F)

polygon(x = c(p,rev(p.equi)), y = c(q,rev(q.equi)), col = rgb(0,.8,0,.8), 
        border = F, density = 20, angle = 90)

# 8. infine calcoliamo l'indice di Gini
G = 2*sum(p-q)/(n-1)
G


