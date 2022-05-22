# Jakub Bodek

# Zadanie 1

# h0 : średnia = 30
# h1 : średnia != 30

punkty <- c(26, 36, 23, 18, 24, 35, 32, 21, 38, 29, 29, 23, 28, 28, 21, 34, 30, 25, 22, 30, 12, 10)
m0 <- 30

t_obl <- ((mean(punkty) - m0)/sd(punkty)*sqrt(length(punkty)))
print(t_obl)

alfa <- 0.05
t_kryt <- qt(1-alfa/2, length(punkty)-1)
print(t_kryt)

# Wniosek:
# ponieważ |t_obl| = 2.547526 > t_kryt = 2.079614
# to hipotezę zerową należy odrzucić


# h0 : średnia = 30
# h1 : średnia > 30

# Próba
punkty <- c(26, 36, 23, 18, 24, 35, 32, 21, 38, 29, 29, 23, 28, 28, 21, 34, 30, 25, 22, 30, 12, 10)
# wartość m0
m0 <- 30


t_obl <- ((mean(punkty) - m0)/sd(punkty)*sqrt(length(punkty)))
print(t_obl)

alfa <- 0.05
# Wartość krytyczna
t_kryt <- qt(1-alfa, length(punkty)-1)
print(t_kryt)

# Wniosek:
# ponieważ t_obl = -2.547526 < -t_kryt = -1.720743
# to hipotezę zerową należy odrzucić


# h0 : średnia = 30
# h1 : średnia < 30

punkty <- c(26, 36, 23, 18, 24, 35, 32, 21, 38, 29, 29, 23, 28, 28, 21, 34, 30, 25, 22, 30, 12, 10)
m0 <- 30

t_obl <- ((mean(punkty) - m0)/sd(punkty)*sqrt(length(punkty)))
print(t_obl)

alfa <- 0.05
t_kryt <- qt(1-alfa, length(punkty)-1)
print(t_kryt)

# Wniosek:
# ponieważ t_obl = -2.547526 < t_kryt = 1.720743
# to hipotezy zerowej nie należy odrzucać



# Zadanie 2

# Wczytywanie danych
kwoty <- read.table("hipotezy1.txt")
# sprawdza strukture
str(kwoty)
summary(kwoty)

# Testowanie
#Na poziomie istotności α = 0.05 przeprowadzić weryfikację hipotezy 
#H0 m0 = 310 wobec hipotezy H1 m0 ≠ 310 dla tych wydatków.

mean(kwoty$V1)

# h0: średnia = 310
# h1: średnia != 310

m0 <- 310
u_obl <- (mean(kwoty$V1)-m0)/sd(kwoty$V1)*sqrt(dim(kwoty))
print(u_obl)
alfa <- 0.05
u_kryt <- qnorm(1-alfa/2)
print(u_kryt)

# Wnioski
# Ponieważ |u_obl| = 2.274149 > u_kryt = 1.959964
# hipoteze zerową należy odrzucić


# h0: średnia = 310
# h1: średnia < 310

m0 <- 310
u_obl <- (mean(kwoty$V1)-m0)/sd(kwoty$V1)*sqrt(dim(kwoty))
print(u_obl)
alfa <- 0.05
u_kryt <- qnorm(1-alfa)
print(u_kryt)

# Wnioski
# Ponieważ u_obl = 2.274149 > u_kryt = 1.644854
# hipoteze zerową nie należy odrzucać


# h0: średnia = 310
# h1: średnia > 310

m0 <- 310
u_obl <- (mean(kwoty$V1)-m0)/sd(kwoty$V1)*sqrt(dim(kwoty))
print(u_obl)
alfa <- 0.05
u_kryt <- qnorm(1-alfa)
print(u_kryt)

# Wnioski
# Ponieważ u_obl = 2.274149 > u_kryt = 1.644854
# hipoteze zerową należy odrzucić

############### funkcja t.test #####################
# h0: średnia = 310
# h1: średnia != 310

m0 <- 310
alfa <- 0.05
t.test(kwoty$V1,mu=m0,sd=sd(kwoty$V1),conf.level = 0.95)
# ?t.test - wywołaj funcje pomocniczą t.test

# Testowanie hipotezy jednostronnej
# h0: średnia = 310
# h1: średnia < 310

m0 <- 310
alfa <- 0.05
t.test(kwoty$V1,mu=m0,sd=sd(kwoty$V1),conf.level = 0.95, alternative="less")

# h0: średnia = 310
# h1: średnia > 310

m0 <- 310
alfa <- 0.05
t.test(kwoty$V1,mu=m0,sd=sd(kwoty$V1),conf.level = 0.95, alternative="greater")



# Zadanie
dane<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
head(dane)

x<-dane$Milk
# średnia z x
mean(x)
# odchylenie standardowe z x
sd(x)

# h0: średnia = 5025
# h1: średnia < 5025

m0 <- 5025
u_obl <- (mean(x)-m0)/sd(x)*sqrt(length(x))
print(u_obl)
alfa <- 0.05
u_kryt <- qnorm(1-alfa)
print(u_kryt)

# Wnioski
# Ponieważ u_obl = 2.192057 > u_kryt = 1.644854
# hipoteze zerową nie należy odrzucać

# Testowanie hipotezy jednostronnej
# h0: średnia = 5025
# h1: średnia < 5025

m0 <- 5025
alfa <- 0.05
t.test(x,mu=m0,sd=sd(x),conf.level = 0.95, alternative="less")


## PORÓWNANIE DWÓCH ŚREDNICH

# Zadanie 4

table(dane$Channel)

# wymiar danych
dim(dane)

x<-dane$Milk[dane$Channel==2]
y<-dane$Milk[dane$Channel==1]

# długość x
length(x)
# długość y
length(y)

# h0: średnia  1 == średnia 2
# h1: średnia 1 != średnia 2

u_obl <- (mean(x)-mean(y))/ sqrt(var(x)/length(x) + var(y)/length(y))
print(u_obl)

alfa <- 0.05
u_kryt <- qnorm(1-alfa/2)
print(u_kryt)

# Wniosek:
# Ponieważ |u_obl| = 8.541551 > u_kryt = 1.959964
# h0 należy odrzucić na korzyść h1

# Testowanie
t.test(x,y)
t.test(Milk~Channel, data=dane)



## TESTOWANIE NORMALNOŚCI ROZKŁADU

reszty <- c(-3.9, -0.3, -3.1, -5.5, -6, -2.6, -2.3, 3.4, 4.7, 2.6, 3.1, 0.8, 9.7, 5.4, 1.6, 1.4, 4.2, -0.3, -7.4, -3.4, 1, 4.6, -2.7, -5.2)

# h0: reszty z rozkladu normalnego
# h1: reszty nie są z rozkładu normalnego

# test shapiro-wilka
shapiro.test(reszty)

# Wniosek:
# Ponieważ obliczone p-value = 0.7858 > alfa = 0.05 
# to nie ma podstaw do odrzucenia h0

# To co potrzebne do projektu
shapiro.test(dane$Milk)

# Wniosek
# Ponieważ obliczone p-value < 2.2e-16 = 2.2 * 10**-16 < alfa = 0.05
# h0 należy odrzucić


# funkcja 'iteracja'
iteracja <- function(n,alfa){
  x<-rt(n,6)
  pwartosc <- shapiro.test(x)$p.value
  pwartosc < alfa
}
iteracja(30,0.05)


set.seed(10)
n <- 30 ; alfa <- 0.05 ; m <- 10000
sym <- replicate(m, iteracja(n,alfa))
moc <- sum(sym)/m ; moc ; mean(sym)










