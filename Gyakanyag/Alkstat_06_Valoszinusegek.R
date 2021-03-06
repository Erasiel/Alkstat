# =============================== #
# Alkalmazott statisztika         #
# 6. gyakorlat                    #
# Diszkr�t illeszked�svizsg�lat   #
# Val�sz�n�s�gek tesztel�se       #
# =============================== #

# Egy dob�kock�val sz�zszor egym�s ut�n dobva a k�vetkez�
# gyakoris�gokat kaptuk:
# 1: 15, 2: 16, 3: 14, 4: 15, 5: 20, 6: 20
# Tesztelj�k azt a nullhipot�zist, hogy a dob�kocka szab�lyos.

gyak = c(15, 16, 14, 15, 20, 20)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# Ugyanezt a dob�kock�t most 1000 alkalommal dobjuk fel, melynek sor�n a
# k�vetkez� gyakoris�gokat kaptuk:
# 1: 150, 2: 160, 3: 140, 4: 150, 5: 200, 6: 200
# Tesztelj�k azt a nullhipot�zist, hogy a dob�kocka szab�lyos.

gyak = c(150, 160, 140, 150, 200, 200)
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak, p = valsz)

# ====================================================================

# Egy adott n�v�ny h�romfajta sz�nben fordul el�, van piros, r�zsasz�n �s 
# feh�r v�ltozata. Genetikusok azt sejtik, hogy a sz�n recessz�v m�don �r�kl�dik. 
# Ennek ellen�rz�s�re piros �s feh�r sz�n� n�v�nyeket h�zas�tanak �ssze egym�ssal, 
# �s megvizsg�lj�k, hogy az ut�dn�v�nyek milyen sz�n�ek. Recessz�v �r�kl�d�s eset�n 
# egy-egy ut�dn�v�ny 0,25; 0,5 illetve 0,25 val�sz�n�s�ggel lesz piros, r�zsasz�n 
# illetve feh�r. A kikelt ut�dn�v�nyek k�z�l 30 lett piros, 50 r�zsasz�n �s 35 feh�r. 
# Tesztelj�k 5 sz�zal�kos szignifikancia szinten azt a nullhipot�zist, hogy a sz�n 
# recessz�ven �r�kl�dik.

valsz = c(0.25, 0.5, 0.25)
gyak = c(30, 50, 35)
chisq.test(gyak, p = valsz)

# ====================================================================

# Egy felm�r�s sor�n a megk�rdezett 127 ember k�z�l 51 az aktu�lis polg�rmestert
# t�mogatja, 76 pedig a m�sik jel�ltet. Mondhatjuk-e, hogy azonos a k�t jel�lt
# t�mogatotts�ga?

valsz = c(0.5, 0.5)
gyak = c(51, 76)
chisq.test(gyak, p = valsz)

# ====================================================================

# Az 'ertekek20' nev� vektor 20 kockadob�s eredm�ny�t tartalmazza. 
# Tesztelj�k azt a nullhipot�zist, hogy a dob�kocka szab�lyos.
# Milyen figyelmeztet� �zenetet kapunk, �s ennek mi az oka?

ertekek20 = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6)
(gyak20 = table(ertekek20))
valsz = c( 1/6, 1/6, 1/6, 1/6, 1/6, 1/6 )
chisq.test(gyak20, p = valsz)

# ====================================================================

# Egy eg�sz �rt�k� val�sz�n�s�gi v�ltoz� �rt�keire az al�bbi
# gyakoris�gokat kaptuk:
# 1   2  3  4  5  6  7 >=8
# 126 68 56 26 18 14 4 8
# Feltessz�k, hogy a minta geometriai eloszl�sb�l sz�rmazik.
# Hozzunk l�tre egy vektort a megadott gyakoris�gokkal, hat�rozzuk meg
# a minta�tlagot, majd adjunk becsl�st a geometriai eloszl�s param�ter�re.

ertekek = c(1:8)
gyakorisag = c(126,68,56,26,18,14,4,8)
m = sum(ertekek*gyakorisag)/sum(gyakorisag)
m
p = 1/m
p

# Tesztelj�k 5 sz�zal�kos szignifikanciaszinten azt a nullhipot�zist, hogy
# a minta p param�teres geometriai eloszl�sb�l sz�rmazik.

valsz = (1-p)^(ertekek-1)*p
valsz[8] = 1 - sum(valsz[1:7])
valsz
chisq.test(gyakorisag, p = valsz)

