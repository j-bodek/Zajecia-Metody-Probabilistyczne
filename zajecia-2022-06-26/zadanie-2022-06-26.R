# Jakub Bodek

dane <- read.table('zajecia-2022-06-26/zadanieAczel.txt', header=TRUE, sep=';')
str(dane)

# korelacja pomiedzy miles a dolars
cor(dane$miles, dane$dolars)

# 1. Czy i jaka zależność wystepuje pomiędzy długością trasy (miles) i obciążeniem karty kredytowej (dolars)?
# wspolczynik korelacji Pearsona wynosi 0.9824339 i swiadczy o silnej korelacji linkowej


# 2. Jaki wpływ ma zmiana długości trasy (miles) i obciążenie karty kredytowej (dolars)?
# model regresji liniowej
# Model : dolars = a * miles + b + ε
attach(dane)
model <- lm(dolars~miles, data=dane)
model

# Wykres rozrzutu (scatterplot)
plot(dolars~miles, data=dane, col=rgb(0,0,1,.6),pch=19)

# model summary
summary(model)

# gdy zwiększymy wartość miles o 1 wartość dolars zwiększy się średnio o 1.255
# bo:
# (1.255 * x + 274.850 + ε) - (1.255 * (x + 1) + 274.850 + ε) = 1.255


# 3. Podaj model z błędami parametrów i oceń ich istotność
# Model : miles = 1.255 * dolars + 274.850 + ε
# H0: a = 0
# H1: a != 0
# poniewaz p-value dla a przyjmuje 0.12 > 0.05, to H0 nalezy przyjac
# H0: b = 0
# H1: b != 0
# poniewaz p-value dla b przyjmuje 2e-16 < 0.05, to H0 nalezy odrzucic
# Ponieważ p-value dla obu współczynników (0.002 i 2e-16) są bardzo małe 

# wniosek: wartosc wspolczynnika a nie istotna statystycznie, natomiast
# wartosc wspolczynnika b istotna statystycznie


# 4. W jakiej części zmienność obciążenie karty kredytowej (dolars) jest wyjaśniona przez zmianę długości trasy (miles)?
# na podstawie wspolczynnika determinacji (R2) = 0.9637 mozna powiedziec
# ze zmiennosc obciążenie karty kredytowej jest wyjasniana w 96% przez zbudowany model


# 5. W jakim procencie zmiany obciążenia karty kredytowej (dolars) nie zależą od zmian długości trasy (miles)? (ocena dopasowania modelu do danych empirycznych)
# Na podstawie wspolczynnika zbieznosci fi = 1-R2 = 1 - 0.9637 = 0.0363
# mozna powiedziec, ze zmiennosc obciążenia karty kredytowej jest nie wyjasniona w około 3.5% przez zbudowany model.


# 6. O ile średnio różnią się empiryczne wartości obciążenia karty kredytowej (dolars) od odpowiadających im wartości teoretycznych, wyznaczonych z oszacowanego modelu regresji?
# na podstawie Residual standard error: 318.2 okresla sie ze empiryczne wartosci 
# obciążenia karty kredytowej roznia sie od odpowiadajacych im wartosci teoretycznych srednio o 318.2














