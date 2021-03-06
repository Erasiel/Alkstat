# =============================== #
# Alkalmazott statisztika         #
# 3. gyakorlat                    #
# Az egymint�s �s a p�ros t-pr�ba #
# =============================== #


# Egy vizsg�lat sor�n arra voltunk k�v�ncsiak, hogy egy adott egyetem 
# hallgat�inak �tlagos IQ-szintje elt�r-e a magyarorsz�gi �tlagt�l, ami 110. 
# 80 f�b�l �ll� v�letlen minta alapj�n kapott minta�tlag 118 lett, 
# 12 sz�r�ssal. Norm�lis eloszl�st felt�telezve, mondhatjuk-e, hogy ennek az
# egyetemnek a hallgat�i a magyarorsz�gi �tlagt�l elt�r�en teljes�tenek 
# az IQ-teszten?

elemszam = 80   # n
mintaatlag = 118
mintaszoras = 12
teszt = 110  # mu0
alpha = 0.05

# 95% megb�zhat�s�g� konfidencia intervallum a v�rhat� �rt�kre

( konf.int_also = mintaatlag - qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )
( konf.int_felso = mintaatlag + qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )

# Tesztstatisztika alapj�n

(t = mintaatlag-teszt)/(mintaszoras/sqrt(elemszam))

# Kritikus �rt�k
(t_alpha = qt(1-alpha/2, df = elemszam-1))

# p-�rt�k alapj�n
(p = pt(abs(t), df = elemszam-1, lower.tail = F)*2)

# =========================================================================

# Olvassuk be a survey adatb�zist a MASS csomagb�l.
# N�zz�k meg az adatb�zis le�r�s�t.

library(MASS)
input = data.frame(survey)
?survey

# CSAK AKKOR, HA A MASS CSOMAG NINCS TELEP�TVE:
input = read.table("survey.txt", header = TRUE)

attach(input)

# =========================================================================

# Adjuk meg a pulzussz�m (Pulse v�ltoz�) mintabeli jellemz�it, majd
# k�sz�ts�nk hisztogramot �s boxdiagramot.

summary(Pulse)
sd(Pulse, na.rm = T)
hist(Pulse)
boxplot(Pulse)

# 5 sz�zal�kos szignifikancia szinten tesztelj�k azt a nullhipot�zist,
# hogy az �tlagos pulzussz�m 75-tel egyenl�.

t.test(Pulse, mu = 75)

# =========================================================================

# K�rdezz�k le a Wr.Hnd �s a NW.Hnd v�ltoz�k alapstatisztik�it. 

summary(Wr.Hnd)
sd(Wr.Hnd, na.rm = T)

Wr.Hnd2 = Wr.Hnd[!is.na(Wr.Hnd)]

( hianyzo = length(Wr.Hnd) - length(Wr.Hnd2) ) # hi�nyz� adatok sz�ma

( n = length(Wr.Hnd) - hianyzo ) # mintam�ret

sd(Wr.Hnd, na.rm = T)/sqrt(n) # standard hiba

summary(NW.Hnd)
sd(NW.Hnd, na.rm = T)

NW.Hnd2 = NW.Hnd[!is.na(NW.Hnd)]

( hianyzo = length(NW.Hnd) - length(NW.Hnd2) )

( n = length(NW.Hnd) - hianyzo )

sd(NW.Hnd, na.rm = T)/sqrt(n)

# �br�zoljuk a v�ltoz�k hisztogramj�t �s boxplotj�t!

hist(Wr.Hnd)
boxplot(Wr.Hnd)

hist(NW.Hnd)
boxplot(NW.Hnd)

# Hasonl�tsuk �ssze a k�t k�z feszt�vols�g�t. 5 sz�zal�kos szignifikancia
# szinten tesztelj�k azt a nullhipot�zist, hogy a k�t k�z �tlagos 
# feszt�vols�ga megegyezik.

t.test(Wr.Hnd, NW.Hnd, paired = T)
t.test(Wr.Hnd-NW.Hnd, mu = 0)

# =========================================================================

# V�lasszuk le az 'input' t�bl�zatot. 

detach(input)

