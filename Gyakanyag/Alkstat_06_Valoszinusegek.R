# =============================== #
# Alkalmazott statisztika         #
# 6. gyakorlat                    #
# Diszkrét illeszkedésvizsgálat   #
# Valószínûségek tesztelése       #
# =============================== #

# Egy dobókockával százszor egymás után dobva a következõ
# gyakoriságokat kaptuk:
# 1: 15, 2: 16, 3: 14, 4: 15, 5: 20, 6: 20
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.

gyak = c(15, 16, 14, 15, 20, 20)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# Ugyanezt a dobókockát most 1000 alkalommal dobjuk fel, melynek során a
# következõ gyakoriságokat kaptuk:
# 1: 150, 2: 160, 3: 140, 4: 150, 5: 200, 6: 200
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.

gyak = c(150, 160, 140, 150, 200, 200)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# ====================================================================

# Egy adott növény háromfajta színben fordul elõ, van piros, rózsaszín és 
# fehér változata. Genetikusok azt sejtik, hogy a szín recesszív módon öröklõdik. 
# Ennek ellenõrzésére piros és fehér színû növényeket házasítanak össze egymással, 
# és megvizsgálják, hogy az utódnövények milyen színûek. Recesszív öröklõdés esetén 
# egy-egy utódnövény 0,25; 0,5 illetve 0,25 valószínûséggel lesz piros, rózsaszín 
# illetve fehér. A kikelt utódnövények közül 30 lett piros, 50 rózsaszín és 35 fehér. 
# Teszteljük 5 százalékos szignifikancia szinten azt a nullhipotézist, hogy a szín 
# recesszíven öröklõdik.

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

# Az 'ertekek20' nevû vektor 20 kockadobás eredményét tartalmazza. 
# Teszteljük azt a nullhipotézist, hogy a dobókocka szabályos.
# Milyen figyelmeztetõ üzenetet kapunk, és ennek mi az oka?

ertekek20 = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6)
(gyak20 = table(ertekek20))
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak20, p = valsz)

# ====================================================================

# Egy egész értékû valószínûségi változó értékeire az alábbi
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

