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

# Súgó egy függvényről:

help("log")
?log

# Milyen kötelező és opcionális argumentuma van a függvénynek? 
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

# Műveletek vektorokkal:

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

# pontosvesszővel tagolt csv fájlból:

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

# Töltsük be a 'datasets' nevű könyvtárat, majd olvassuk be
# a 'state.x77' adatsort az 'input' nevű változóba. Nézzük meg
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




# ============================== #
# Alkalmazott statisztika        #
# 2. gyakorlat                   #
# Alapstatisztikák és grafikonok #
# ============================== #

# Olvassuk be a salary.txt állomány tartalmát az 'input' nevű táblázatba.
# Csatoljuk fel a táblázatot, hogy így könnyebben tudjunk hivatkozni
# az oszlopokra.

input = read.table("salary.txt", header = TRUE)
attach(input)

# Töltsük be a 'moments' csomagot a skewness függvény használatához.

library(moments)      # skewness

# =====================================================================

# Hány alkalmazott szerepel a táblázatban?

length(salary)

# Adjunk pontbecslést a 'salary' változó várható értékére, szórására,
# varianciájára és mediánjára. 

mean(salary)
sd(salary)
var(salary)
median(salary)

# A kapott empririkus szórás (és variancia) korrigált vagy korrigálatlan?

# Milyen irányú egyenlőtlenség tapasztalható a mintaátlag és a medián között?
# Ez alapján mit mondhatunk, várhatóan milyen az eloszlás alakja? 
# Kérdezzük le a minta ferdeségét is.

skewness(salary)

# Milyen értékeket kapunk normális eloszlású változó ferdeségére?

# Ábrázoljuk a 'salary' változó hisztogramját először az
# alapbeállításokkal, majd 20 beosztással.

hist(salary)
hist(salary, breaks = 20)

# A hisztogram alapján mit mondhatunk az eloszlás alakjáról?

# Boxdiagram alapján mit mondhatunk az eloszlás alakjáról?

boxplot(salary)

# Vannak-e kiugró értékek?

# Határozzuk meg a 'salary' változó esetében a kvartiliseket,
# a legkisebb és a legnagyobb értéket, valamint a minta terjedelmét és
# interkvartilis terjedelmét.

quantile(salary)
quantile(salary, probs = c(0.25, 0.5, 0.75))
max(salary)
min(salary)
max(salary) - min(salary)
IQR(salary)

summary(salary)

# =====================================================================

# Határozzuk meg a 'child' változó értékeinek gyakoriságát,
# relatív gyakoriságát, majd ábrázoljuk a gyakoriságokat 
# oszlopdiagrammal és tortadiagrammal.

child.freq = table(child)
child.freq
prop.table(child.freq)
barplot(child.freq)
pie(child.freq)

# Határozzuk meg, hogy hány gyermeke van az alkalmazottaknak összesen.
# Ehhez először töröljük ki a hiányzó adatokat a 'child' vektorból. Hány
# alkalmazottnál nem tudjuk, hogy hány gyermeke van?

sum(child)
child2 = child[!is.na(child)]
sum(child2)
length(child) - length(child2)

# Egy táblázatban adjuk meg a 'jobkat' és a 'child' változó együttes
# eloszlását. Kérdezzük le az átlagos gyerekszámot a munkahelyi beosztás
# szerinti bontásban.

table(jobkat, child)
tapply(child, jobkat, mean, na.rm = TRUE)

# =====================================================================

# Válasszuk le az 'input' táblázatot. Ellenőrizzük is le,
# hogy megtörtént a leválasztás.

detach(input)
salary




# =============================== #
# Alkalmazott statisztika         #
# 3. gyakorlat                    #
# Az egymintás és a páros t-próba #
# =============================== #


# Egy vizsgálat során arra voltunk kíváncsiak, hogy egy adott egyetem 
# hallgatóinak átlagos IQ-szintje eltér-e a magyarországi átlagtól, ami 110. 
# 80 főből álló véletlen minta alapján kapott mintaátlag 118 lett, 
# 12 szórással. Normális eloszlást feltételezve, mondhatjuk-e, hogy ennek az
# egyetemnek a hallgatói a magyarországi átlagtól eltérően teljesítenek 
# az IQ-teszten?

elemszam = 80   # n
mintaatlag = 118
mintaszoras = 12
teszt = 110  # mu0
alpha = 0.05

# 95% megbízhatóságú konfidencia intervallum a várható értékre

( konf.int_also = mintaatlag - qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )
( konf.int_felso = mintaatlag + qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )

# Tesztstatisztika alapján

(t = mintaatlag-teszt)/(mintaszoras/sqrt(elemszam))

# Kritikus érték
(t_alpha = qt(1-alpha/2, df = elemszam-1))

# p-érték alapján
(p = pt(abs(t), df = elemszam-1, lower.tail = F)*2)

# =========================================================================

# Olvassuk be a survey adatbázist a MASS csomagból.
# Nézzük meg az adatbázis leírását.

library(MASS)
input = data.frame(survey)
?survey

# CSAK AKKOR, HA A MASS CSOMAG NINCS TELEPÍTVE:
input = read.table("survey.txt", header = TRUE)

attach(input)

# =========================================================================

# Adjuk meg a pulzusszám (Pulse változó) mintabeli jellemzőit, majd
# készítsünk hisztogramot és boxdiagramot.

summary(Pulse)
sd(Pulse, na.rm = T)
hist(Pulse)
boxplot(Pulse)

# 5 százalékos szignifikancia szinten teszteljük azt a nullhipotézist,
# hogy az átlagos pulzusszám 75-tel egyenlő.

t.test(Pulse, mu = 75)

# =========================================================================

# Kérdezzük le a Wr.Hnd és a NW.Hnd változók alapstatisztikáit. 

summary(Wr.Hnd)
sd(Wr.Hnd, na.rm = T)

Wr.Hnd2 = Wr.Hnd[!is.na(Wr.Hnd)]

( hianyzo = length(Wr.Hnd) - length(Wr.Hnd2) ) # hiányzó adatok száma

( n = length(Wr.Hnd) - hianyzo ) # mintaméret

sd(Wr.Hnd, na.rm = T)/sqrt(n) # standard hiba

summary(NW.Hnd)
sd(NW.Hnd, na.rm = T)

NW.Hnd2 = NW.Hnd[!is.na(NW.Hnd)]

( hianyzo = length(NW.Hnd) - length(NW.Hnd2) )

( n = length(NW.Hnd) - hianyzo )

sd(NW.Hnd, na.rm = T)/sqrt(n)

# Ábrázoljuk a változók hisztogramját és boxplotját!

hist(Wr.Hnd)
boxplot(Wr.Hnd)

hist(NW.Hnd)
boxplot(NW.Hnd)

# Hasonlítsuk össze a két kéz fesztávolságát. 5 százalékos szignifikancia
# szinten teszteljük azt a nullhipotézist, hogy a két kéz átlagos 
# fesztávolsága megegyezik.

t.test(Wr.Hnd, NW.Hnd, paired = T)
t.test(Wr.Hnd-NW.Hnd, mu = 0)

# =========================================================================

# Válasszuk le az 'input' táblázatot. 

detach(input)





# =============================== #
# Alkalmazott statisztika         #
# 4. gyakorlat                    #
# A kétmintás t-próba és az ANOVA #
# =============================== #


# Töltsük be a 'car' csomagot.

library(car)   # leveneTest

# =========================================================================

# Olvassuk be a survey adatbázist a MASS csomagból.

library(MASS)
input = data.frame(survey)
?survey
attach(input)

# =========================================================================

# Adjuk meg a pulzusszámok (Pulse változó) mintabeli jellemzőit nemek 
# szerint, majd készítsünk hisztogramot és boxdiagramot nemek szerint.

tapply(Pulse, Sex, summary)
tapply(Pulse, Sex, sd, na.rm = T)
hist(Pulse[Sex == "Male"])
hist(Pulse[Sex == "Female"])
boxplot(Pulse ~ Sex)

# 1 százalékos szignifikancia szinten teszteljük azt a nullhipotézist,
# hogy az átlagos pulzusszám azonos férfiak és nők esetén.
# Adjunk meg egy 99 százalék megbízhatóságú konfidencia intervallumot 
# a két várható érték különbségére.

var.test(Pulse ~ Sex, conf.level = 0.99)
t.test(Pulse ~ Sex, var.equal = T, conf.level = 0.99)

# =========================================================================

# Vizsgáljuk meg a pulzusszámokat a testmozgás gyakorisága 
# (Exer változó) szerint. 

summary(Exer,na.rm = TRUE) # Osszesen 115+24+98=237 egyenrol van Exer adat. 
tapply(Pulse, Exer, summary, na.rm = TRUE)
# 20+7+18=45 embernek nincs pulzusszam adata, igy osszesen 237-45 = 192 emberrol van pulzusszam adat.
tapply(Pulse, Exer, sd, na.rm = TRUE)
boxplot(Pulse ~ Exer)

# Teszteljük azt a nullhipotézist, hogy a pulzusszámok várható értéke
# minden csoportban ugyanakkora.

leveneTest(Pulse ~ Exer, center = mean)

oneway.test(Pulse ~ Exer, var.equal = TRUE)

# Hány százalékban magyarázza a pulzusszámot az, hogy az alany 
# milyen gyakran végez testmozgást?

summary(aov(Pulse ~ Exer))

900 / (900 + 25188)

# =========================================================================

detach(input)

# =========================================================================

# Welch-proba.
# Ket gep altal gyartott csavarok hosszat vizsgaljuk. 
# Felteheto, hogy a csavarok hossza mindket gep eseteben normalis eloszlast kovet. 
# Az alabbi 5, illetve 7 elemu minta all rendelkezesunkre:

# 1. gep:
minta1<-c(9.8,9.7,9.9,10.4,9.5)
# 2. gep:
minta2<-c(10,9.9,10.1,10,10.1,9.8,10.1)

# A mintak alapjan vizsgaljuk 5%-os szignifikancia szinten azt a nullhipotezist, 
# hogy a csavarhosszak varhato erteke megegyezik a ket gep eseteben, nem feltetelezve, hogy a mintak szorasa ugyanakkora.

t.test(minta1,minta2,var.equal=FALSE)

# A kapott output fejlece ''Welch Two Sample t-test''. A probastatisztika erteke mas, mint 
# a var.equal=TRUE kapcsolo hasznalatakor. Tovabba a szabadsagi fok nem egesz szam: df = 4.6805,
# mely kulonbozik a var.equal=TRUE kapcsolo hasznalatakor kapott 10 szabadsagi foktol.

# Mivel p-value = 0.4148 > 0.05, ezert elfogadjuk H_0-t.





# ==================================== #
# Alkalmazott statisztika              #
# 5. gyakorlat                         #
# Illeszkedés- és homogenitásvizsgálat #
# ==================================== #

# Olvassuk be a salary.txt állomány tartalmát az 'input' nevű táblázatba.

input = read.table("salary.txt", header = TRUE)
attach(input)

# Töltsük be a 'moments' és a "nortest" csomagokat.

library(moments) # skewness
library(nortest) # pearson.test

# =========================================================================

# Normalitásvizsgálat

# Adjunk pontbecslést a 'salary' változó várható értékére és szórására,
# majd kérdezzük le a minta mediánját, ferdeségét is.
# Ezek alapján mit várunk, milyen a 'salary' változó sűrűségfüggvénye
# a normális eloszláshoz viszonyítva?

mean(salary)
sd(salary)
median(salary)
skewness(salary) # ferdeség

# Standard normális:
set.seed(1234)
minta = rnorm(1000)
mean(minta)
sd(minta)
median(minta)
skewness(minta)
IQR(minta) # tapasztalati interkvartilis terjedelem.
IQR_elmeleti = qnorm(0.75) - qnorm(0.25) # standard normalis eloszlasl (elmeleti) interkvartilis terjedelme 1.34898.

# Ábrázoljuk a 'salary' változó relatív gyakorisági hisztogramját, 
# majd illesszük rá a paraméterbecslésekből kapott normális eloszlás 
# sűrűségfüggvényét. Ábrázoljuk a 'salary' változó boxplotját is.

hist(salary, prob = TRUE)
curve(dnorm(x, mean = mean(salary), sd = sd(salary)), add = TRUE)
boxplot(salary)

# Standardizáljuk a 'salary' változót, majd QQ-ábra segítségével 
# vizsgáljuk meg, hogy az így kapott változó standard normális 
# eloszlást követ-e.

std_salary = scale(salary)
qqnorm(std_salary)
qqline(std_salary)

# A Kolmogorov-Szmirnov-próba alkalmazásával teszteljük le
# 5 százalékos szignifikanciaszinten azt a nullhipotézist, hogy
# a 'salary' változó normális eloszlást követ.

ks.test(salary, "pnorm", mean(salary), sd(salary))

# Teszteljük a normalitást khi-négyzet próbával is.

pearson.test(salary)

# Definiáljunk egy 'logsalary' nevű változót olyan módon,
# hogy a 'salary' változó logartimusát vesszük. Végezzük el a fenti
# elemzést a 'logsalary' változóra is.

# =========================================================================

# Kétmintás Kolmogorov-Szmirnov-próba

# Ábrázoljuk a 'salary' változó boxplotját nemek szerinti bontásban.

boxplot(salary ~ gender)

# Kétmintás Kolmogorov-Szmirnov-próba segítségével teszteljük le
# azt a nullhipotézist, hogy a férfiak és a nők körében azonos 
# a fizetés eloszlása.

ks.test(salary[gender == "Male"], salary[gender == "Female"])

# =========================================================================

detach(input)





# =============================== #
# Alkalmazott statisztika         #
# 6. gyakorlat                    #
# Diszkrét illeszkedésvizsgálat   #
# Valószínűségek tesztelése       #
# =============================== #

# Egy dobókockával százszor egymás után dobva a következő
# gyakoriságokat kaptuk:
# 1: 15, 2: 16, 3: 14, 4: 15, 5: 20, 6: 20
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.

gyak = c(15, 16, 14, 15, 20, 20)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# Ugyanezt a dobókockát most 1000 alkalommal dobjuk fel, melynek során a
# következő gyakoriságokat kaptuk:
# 1: 150, 2: 160, 3: 140, 4: 150, 5: 200, 6: 200
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.

gyak = c(150, 160, 140, 150, 200, 200)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# ====================================================================

# Egy adott növény háromfajta színben fordul elő, van piros, rózsaszín és 
# fehér változata. Genetikusok azt sejtik, hogy a szín recesszív módon öröklődik. 
# Ennek ellenőrzésére piros és fehér színű növényeket házasítanak össze egymással, 
# és megvizsgálják, hogy az utódnövények milyen színűek. Recesszív öröklődés esetén 
# egy-egy utódnövény 0,25; 0,5 illetve 0,25 valószínűséggel lesz piros, rózsaszín 
# illetve fehér. A kikelt utódnövények közül 30 lett piros, 50 rózsaszín és 35 fehér. 
# Teszteljük 5 százalékos szignifikancia szinten azt a nullhipotézist, hogy a szín 
# recesszíven öröklődik.

valsz = c(0.25, 0.5, 0.25)
gyak = c(30, 50, 35)
chisq.test(gyak, p = valsz)

# ====================================================================

# Egy felmérés során a megkérdezett 127 ember közül 51 az aktuális polgármestert
# támogatja, 76 pedig a másik jelöltet. Mondhatjuk-e, hogy azonos a két jelölt
# támogatottsága?

valsz = c(0.5, 0.5)
gyak = c(51, 76)
chisq.test(gyak, p = valsz)

# ====================================================================

# Az 'ertekek20' nevű vektor 20 kockadobás eredményét tartalmazza. 
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.
# Milyen figyelmeztető üzenetet kapunk, és ennek mi az oka?

ertekek20 = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6)
(gyak20 = table(ertekek20))
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak20, p = valsz)

# ====================================================================

# Egy egész értékű valószínűségi változó értékeire az alábbi
# gyakoriságokat kaptuk:
# 1   2  3  4  5  6  7 >=8
# 126 68 56 26 18 14 4 8
# Feltesszük, hogy a minta geometriai eloszlásból származik.
# Hozzunk létre egy vektort a megadott gyakoriságokkal, határozzuk meg
# a mintaátlagot, majd adjunk becslést a geometriai eloszlás paraméterére.

ertekek = c(1:8)
gyakorisag = c(126,68,56,26,18,14,4,8)
m = sum(ertekek*gyakorisag)/sum(gyakorisag)
m
p = 1/m
p

# Teszteljük 5 százalékos szignifikanciaszinten azt a nullhipotézist, hogy
# a minta p paraméteres geometriai eloszlásból származik.

valsz = (1-p)^(ertekek-1)*p
valsz[8] = 1 - sum(valsz[1:7])
valsz
chisq.test(gyakorisag, p = valsz)

