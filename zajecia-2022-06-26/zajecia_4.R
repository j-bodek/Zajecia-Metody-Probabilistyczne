# Jakub Bodek
# ZADANIA Z ĆWICZEŃ

# Zadanie 1.
# help funkcji aov
?aov

# wczytujemy dane
zad1<-read.table("zajecia-2022-06-26/zad1.txt", header = T,sep=",",dec=".")
str(zad1)

zad1$Stanowisko <- as.factor(zad1$Stanowisko)
str(zad1)

#wykres 
boxplot(zad1$Czas~zad1$Stanowisko,col="red",border="darkblue",pch=19,main="Średnie czasy")
points(c(1,2,3),tapply(zad1$Czas,zad1$Stanowisko,mean),col="yellow",pch=19)

tapply(zad1$Czas,zad1$Stanowisko, mean)
tapply(zad1$Czas,zad1$Stanowisko, sd)

plot.design(Czas~Stanowisko,data = zad1,col="blue")

#analiza wariancji
av <- aov(Czas ~ Stanowisko,data=zad1)
summary(av)

#wartość krytyczna
alfa= 0.05;k=3;n=dim(zad1)[1]
qf(1-alfa,k-1,n-k)

# H0: średni czas wykonania zadania na kazdym stanowisku jest taki sam
# H1: nie jest

# wniosek:
# Wartosc obliczona F = 21.55 > wartosc krytyczna = 3.055558
# czyli H0 nalezy odrzucic


# ANALIZA KORELACJI

zad2 <- read.table('zajecia-2022-06-26/zadanieRegresja.txt', header=TRUE, fileEncoding = 'UTF-8')
str(zad2)
# liczenie korelacji
cor(zad2$Produkcja, zad2$Koszty)
# - Współczynnik korelacji Pearsona przyjmuje zawsze wartości z przedziału [-1, 1]
# - Znak współczynnika informuje o kierunku korelacji (liniowa ujemna lub liniowa dodatnia)
# - Wartość bezwzględna |r| informuje o sile korelacji

# wspolczynik korelacji Pearsona wynosi 0.9708163 i swiadczy o silnej korelacji linkowej

# Wykres rozrzutu (scatterplot)
plot(Koszty~Produkcja, data=zad2, col=rgb(0,0,1,.6),pch=19)

# GENEROWANIE DANYCH
x<-runif(250, 0, 100)
y<-rnorm(250, 50, 10)
plot(x, y, col=rgb(0,0,1,.6),pch=19)

# GENEROWANIE SKORELOWANYCH DANYCH
z <- 2*x + 1 + rnorm(200, 0, 10)
plot(x,z,col=rgb(0,0,1,.6),pch=19)

# korelacja pomiedzy x, y
cor(x, y)
# korelacja pomiedzy x, z
cor(x, z)


# REGRESJA LINIOWA
# Model : koszty = a * Produkcja + b + ε

# tworzenie modelu
attach(zad2)
model <- lm(Koszty~Produkcja, data=zad2)
model

# Model : koszty = 200.1 * Produkcja + 17006.6 + ε
# Czyli jezeli zwieksze produkcje o 1 to koszty wzrosna srednio o 200.1
# bo:
# (200.1 * x + + 17006.6 + ε) - (200.1 * (x + 1) + + 17006.6 + ε) = 200.1

summary(model)

# model rozszerzony
# Koszty = 200.1 (+-8.74) * Produkcja + 17006.6 (+-5177.4) + ε

# H0: a = 0
# H1: a != 0

alfa = 0.05
# Poniewaz p-value dla wsp a = 0.00248 < 0.05 to H0 nalezy odrzucic

# H0: a = 0
# H1: a != 0

alfa = 0.05
# Poniewaz p-value dla wsp b = 2e-16 < 0.05 to H0 nalezy odrzucic

# Wniosek: wartosci wspolczynnikow istotne statystycznie



# 3. W jakiej części zmienność kosztów jest wyjaśniona przez zmianę wielkości produkcji?
# na podstawie wspolczynnika determinacji (R2) = 0.9425 mozna powiedziec
# ze zmiennosc kosztow jest wyjasniana w 94% przez zbudowany model


# 4. W jakim procencie zmiany kosztów nie zależą od zmian wielkości produkcji?
# Na podstawie wspolczynnika zbieznosci fi = 1-R2 = 6%


# 5. O ile średnio różnią się empiryczne wartości Kosztów od odpowiadających im wartości teoretycznych, wyznaczonych z oszacowanego modelu regresji?
# (o tym mowi wartosc bledu standardowego reszt)
# na podstawie bledu standardowego reszt Residual standard error: 10820
# empiryczne wartosci kosztow roznia sie od odpowiadajacych im wartosci teoretycznych
# srednio o 10820


# TESTOWANIE HIPOTEZY O NORMALNOSCI RESZT
model$residuals
shapiro.test(model$residuals)
# wniosek: poniewaz obliczona wartosc p-value = 0.6463 > 0.05
# nie ma podstaw do odrzucenia H0








