# =========================== #
# Alkalmazott statisztika     #
# 1. gyakorlat                #
# Bevezet�s az R nyelvbe      #
# =========================== #

# Utas�t�sok futtat�sa:
# Console ablakban: Enter
# Script ablakban: Ctrl+Enter
# RStudioban Script ablakban: Ctrl+R (Windows operacios rendszer eseten)

# ======================================================================

# Csomagok telep�t�se: install.packages() f�ggv�ny
# Csomagok bet�lt�se: library() f�ggv�ny

# ======================================================================

# Munkak�nyvt�r: getwd() �s setwd()
# K�rdezz�k le az aktu�lis munkak�nyvt�rat. 

getwd()

# Hozzunk l�tre egy saj�t k�nyvt�rat, �s �ll�tsuk be ezt munkak�nyvt�rnak.

setwd("C:/")

# ======================================================================

# S�g� egy f�ggv�nyr�l:

help("log")
?log

# Milyen k�telez� �s opcion�lis argumentuma van a f�ggv�nynek? 
# Mi az opcion�lis argumentum alap�rt�ke?

args(log)

# Ha nem tudjuk pontosan a f�ggv�ny nev�t:

help.search("deviation") # a telep�tett csomagok k�z�tt keres
RSiteSearch("deviation") # mindenhol keres

# ======================================================================

# V�ltoz�k:

# �rt�kad�s:

a = 5*4
a
(b = 2^3)
(c = sin(pi/4))
d = "100"
d

# V�ltoz�k t�pus�nak lek�rdez�se:

typeof(a)
typeof(d)
is.numeric(a)
is.character(a)
str(a)
str(d)

# Vektorok:

# Vektor defini�l�sa: c() f�ggv�ny:

(v1 = c(2, 4, 8, 16, 32))
(v2 = c(6:10))
(v3 = seq(1, 9, by = 2))
(bool = (6 < v2 & v2 < 10))

# M�veletek vektorokkal:

v1 + v2 # komponensenk�nti �sszead�s
v1 * v2 # komponensenk�nti szorz�s
v1 %*% v1 # m�trixszorz�s

log(v1, base = 2)

# R�szvektorok k�pz�se: sz�gletes z�r�jel

v2[2]
v2[1:3]
v2[bool]

# Lista:

lista = list(team = "Manchester United", PLTitle = 20, 
             bestPlayers = c("Best", "Cantona", "Schmeichel", "Giggs", "Rooney"))
lista[[1]]
lista$team

# Adatt�bl�zat:

nevek = c("Anna", "B�la", "Csaba")
pontszamok = c(123, 84, 96)
adat = data.frame(nevek, pontszamok)
adat[1, 2]
elsosor = adat[1, ]
masodikoszlop = adat[, 2]
kisebbmintszaz = adat[adat$pontszamok < 100, ]
rendezo = order(adat$pontszamok)
rendezett = adat[rendezo, ] 

# ======================================================================

# Adatok beolvas�sa:

# Be�p�tett adatb�zis:

airLine = data.frame(AirPassengers)
?AirPassengers

# sz�vegf�jlb�l:

salary = read.table("salary.txt", header = TRUE)

# pontosvessz�vel tagolt csv f�jlb�l:

bank = read.csv2("bankloan.csv")


# ======================================================================

# Saj�t f�ggv�ny defini�l�sa:

myfunction = function(x) { return(20 + (x*x)) }
myfunction(10)


# =====================================================================

# For-ciklus: 10! kiszamolasa.
f<-1; 
for(i in 2:10) {f<-f*i}; 
f

# ======================================================================

# T�lts�k be a 'datasets' nev� k�nyvt�rat, majd olvassuk be
# a 'state.x77' adatsort az 'input' nev� v�ltoz�ba. N�zz�k meg
# az 'input' adatsor tartalm�t.

library(datasets)
?state.x77
input = data.frame(state.x77)

# Hozzunk l�tre k�t list�t, melyek a tag�llamok nev�t illetve
# ter�let�t tartalmazz�k. �rjuk ki minden tag�llamra a ter�let�t.

terulet = input$Area
nev = row.names(input)
for(i in 1:50){
  cat(nev[i], "ter�lete", terulet[i], "n�gyzetm�rf�ld\n")
}

# Hozzunk l�tre egy �j t�bl�zatot, mely csup�n a tag�llamok nev�t �s
# ter�let�t tartalmazza, majd ezt ments�k el 'tagallamok.txt' n�ven.

output = data.frame(nev, terulet)
write.table(output, file = "tagallamok.txt")

