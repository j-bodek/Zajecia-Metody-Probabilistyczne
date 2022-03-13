# Jakub Bodek

# typy danych

# zmienna tupu null
x<-Null
class(x)

# zmienna typu numerycznego
a<-2.33
class(a)
print(a)

s<-15
class(s)
mode(s)
str(s)

# zmienna typu logical
d<-TRUE
class(d)

# zmienna typu znakowego
imie<-"Jakub"
class(imie)

# operator c tworzy wektor
wzrost<-c("wysoki","sredni","niski",
          "wysoki","sredni","niski",
          "wysoki","sredni","niski")
class(wzrost)
table(wzrost)
str(wzrost)

# zamiana z wektora znakowego na wektor czynnikowy
# zmienna typu czynnikowego
wz<-as.factor(wzrost)
class(wz)
table(wz)
str(wz)

# zmienna typu data
data1<-as.Date("2022-03-13")
print(data1)
class(data1)

# data 7 dni po data1
data2<-data+7
print(format(data2, "%y %b %d"))

# wektor
# elementy od 25 do 45 liczone co 3
x<-seq(25,45,3)
mode(x)
str(x)


y<-c(x,"ala")
class(y)
str(y)

# lista
lista<-list(x=x, a="ala")
class(lista)
str(lista)

# sample - funkcja losujaca elementy z listy
# wylosuj 150 elementow z listy x z powtorzeniami
xx<-sample(x, 150, replace=T)

# wylosuj 150 elementy z wektora z powtorzeniami
y<-sample(c("Anna", "Dominik", "Jan", "Maja", "Arek", "Joanna", "Marta"), 150, replace=T)
print(y)

# wygeneruj 150 data zaczynajac od 2022-03-13 co jeden dzien
zz<-seq(as.Date("2022-03-13"), by=1, length.out=150)

# DataFrame
dane<-data.frame(wiek=xx, imie=y, data=zz)
str(dane)
# Wyswietl dane w tabeli
View(dane)

# wczytaj dane z mieszkania-pow.csv
mieszkania<-read.csv(file="mieszkania-pow.csv", sep=";")
str(mieszkania)
# przekonwertuj dane w kolumnie data na typ data
mieszkania$data<-as.Date(mieszkania$data, origin="1900-01-01")
str(mieszkania$data)


# podstawowe statystkyki opisowe

# srednia
srednia_cena<-mean(mieszkania$cena)
print(srednia_cena)

# mediana
mediana_cena<-median(mieszkania$cena)
print(mediana_cena)

# kwartyle
kwartyle_cena<-quantile(mieszkania$cena)
print(kwartyle_cena)

# podsumowanie
podsumowanie_cena<-summary(mieszkania$cena)
print(podsumowanie_cena)

# histogram
histogram<-hist(mieszkania$cena, plot=F)
print(histogram)

# wczytaj dane z linku
dane<-read.csv(file="http://artemis.wszib.edu.pl/~basiura/szeregi_d.csv", sep=";",dec=".", head=T)
attach(dane)
plot(szereg10,type="l",col="darkblue")
hist(dane$szereg10, breaks=25, col="lightblue", border="red", probability=T)


rug(dane$szereg10,col="red")
lines(density(dane$szereg10),lty=2,lwd =3, col="red")
curve(dnorm(x,mean = mean(dane$szereg10), sd = sd(dane$szereg10)),add=T,col="darkgreen",lty=3,lwd=3)


# miary rozproszenia dla mieszkan
rozstep<-max(mieszkania$cena)-min(mieszkania$cena)
print(rozstep)

# rozstep miedzy kwartylny
rozstep_miedzykwartylny<-quantile(mieszkania$cena)[4]-quantile(mieszkania$cena)[2]
print(rozstep_miedzykwartylny)

# wariancja
wariancja<-var(mieszkania$cena)
print(wariancja)
odchylenie_standardowe<-sd(mieszkania$cena)
print(odchylenie_standardowe)

srednia_cena_mieszkania<-mean(mieszkania$cena)
print(srednia_cena_mieszkania)

# blad standardowy
blad_standardowy<-sd(mieszkania$cena)/sqrt(length(mieszkania$cena))
print(blad_standardowy)

# odchylenie przecietne od mediany
odchylenie_przecietne_od_mediany<-mad(mieszkania$cena)
print(odchylenie_przecietne_od_mediany)

# wspolczynnik zmiennosci
wspolczynnik_zmiennosci<-sd(mieszkania$cena)/mean(mieszkania$cena)
print(wspolczynnik_zmiennosci)

# wykres "ramka z wasami"
boxplot(mieszkania$cena, horizontal = T, col = "red")
