# =========================== #
# Alkalmazott statisztika     #
# 1. gyakorlat                #
# Bevezetés az R nyelvbe      #
# =========================== #

# Utasítások futtatása:
# Console ablakban: Enter
# Script ablakban: Ctrl+Enter
# RStudioban Script ablakban: Ctrl+R (Windows operacios rendszer eseten)

# ======================================================================

# Csomagok telepítése: install.packages() függvény
# Csomagok betöltése: library() függvény

# ======================================================================

# Munkakönyvtár: getwd() és setwd()
# Kérdezzük le az aktuális munkakönyvtárat. 

getwd()

# Hozzunk létre egy saját könyvtárat, és állítsuk be ezt munkakönyvtárnak.

setwd("C:/")

# ======================================================================

# Súgó egy függvényrõl:

help("log")
?log

# Milyen kötelezõ és opcionális argumentuma van a függvénynek? 
# Mi az opcionális argumentum alapértéke?

args(log)

# Ha nem tudjuk pontosan a függvény nevét:

help.search("deviation") # a telepített csomagok között keres
RSiteSearch("deviation") # mindenhol keres

# ======================================================================

# Változók:

# Értékadás:

a = 5*4
a
(b = 2^3)
(c = sin(pi/4))
d = "100"
d

# Változók típusának lekérdezése:

typeof(a)
typeof(d)
is.numeric(a)
is.character(a)
str(a)
str(d)

# Vektorok:

# Vektor definiálása: c() függvény:

(v1 = c(2, 4, 8, 16, 32))
(v2 = c(6:10))
(v3 = seq(1, 9, by = 2))
(bool = (6 < v2 & v2 < 10))

# Mûveletek vektorokkal:

v1 + v2 # komponensenkénti összeadás
v1 * v2 # komponensenkénti szorzás
v1 %*% v1 # mátrixszorzás

log(v1, base = 2)

# Részvektorok képzése: szögletes zárójel

v2[2]
v2[1:3]
v2[bool]

# Lista:

lista = list(team = "Manchester United", PLTitle = 20, 
             bestPlayers = c("Best", "Cantona", "Schmeichel", "Giggs", "Rooney"))
lista[[1]]
lista$team

# Adattáblázat:

nevek = c("Anna", "Béla", "Csaba")
pontszamok = c(123, 84, 96)
adat = data.frame(nevek, pontszamok)
adat[1, 2]
elsosor = adat[1, ]
masodikoszlop = adat[, 2]
kisebbmintszaz = adat[adat$pontszamok < 100, ]
rendezo = order(adat$pontszamok)
rendezett = adat[rendezo, ] 

# ======================================================================

# Adatok beolvasása:

# Beépített adatbázis:

airLine = data.frame(AirPassengers)
?AirPassengers

# szövegfájlból:

salary = read.table("salary.txt", header = TRUE)

# pontosvesszõvel tagolt csv fájlból:

bank = read.csv2("bankloan.csv")


# ======================================================================

# Saját függvény definiálása:

myfunction = function(x) { return(20 + (x*x)) }
myfunction(10)


# =====================================================================

# For-ciklus: 10! kiszamolasa.
f<-1; 
for(i in 2:10) {f<-f*i}; 
f

# ======================================================================

# Töltsük be a 'datasets' nevû könyvtárat, majd olvassuk be
# a 'state.x77' adatsort az 'input' nevû változóba. Nézzük meg
# az 'input' adatsor tartalmát.

library(datasets)
?state.x77
input = data.frame(state.x77)

# Hozzunk létre két listát, melyek a tagállamok nevét illetve
# területét tartalmazzák. Írjuk ki minden tagállamra a területét.

terulet = input$Area
nev = row.names(input)
for(i in 1:50){
  cat(nev[i], "területe", terulet[i], "négyzetmérföld\n")
}

# Hozzunk létre egy új táblázatot, mely csupán a tagállamok nevét és
# területét tartalmazza, majd ezt mentsük el 'tagallamok.txt' néven.

output = data.frame(nev, terulet)
write.table(output, file = "tagallamok.txt")

