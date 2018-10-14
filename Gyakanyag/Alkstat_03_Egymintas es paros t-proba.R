# =============================== #
# Alkalmazott statisztika         #
# 3. gyakorlat                    #
# Az egymintás és a páros t-próba #
# =============================== #


# Egy vizsgálat során arra voltunk kíváncsiak, hogy egy adott egyetem 
# hallgatóinak átlagos IQ-szintje eltér-e a magyarországi átlagtól, ami 110. 
# 80 fõbõl álló véletlen minta alapján kapott mintaátlag 118 lett, 
# 12 szórással. Normális eloszlást feltételezve, mondhatjuk-e, hogy ennek az
# egyetemnek a hallgatói a magyarországi átlagtól eltérõen teljesítenek 
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

# Adjuk meg a pulzusszám (Pulse változó) mintabeli jellemzõit, majd
# készítsünk hisztogramot és boxdiagramot.

summary(Pulse)
sd(Pulse, na.rm = T)
hist(Pulse)
boxplot(Pulse)

# 5 százalékos szignifikancia szinten teszteljük azt a nullhipotézist,
# hogy az átlagos pulzusszám 75-tel egyenlõ.

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

