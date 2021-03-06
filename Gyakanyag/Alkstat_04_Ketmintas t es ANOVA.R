# =============================== #
# Alkalmazott statisztika         #
# 4. gyakorlat                    #
# A k�tmint�s t-pr�ba �s az ANOVA #
# =============================== #


# T�lts�k be a 'car' csomagot.

library(car)   # leveneTest

# =========================================================================

# Olvassuk be a survey adatb�zist a MASS csomagb�l.

library(MASS)
input = data.frame(survey)
?survey
attach(input)

# =========================================================================

# Adjuk meg a pulzussz�mok (Pulse v�ltoz�) mintabeli jellemz�it nemek 
# szerint, majd k�sz�ts�nk hisztogramot �s boxdiagramot nemek szerint.

tapply(Pulse, Sex, summary)
tapply(Pulse, Sex, sd, na.rm = T)
hist(Pulse[Sex == "Male"])
hist(Pulse[Sex == "Female"])
boxplot(Pulse ~ Sex)

# 1 sz�zal�kos szignifikancia szinten tesztelj�k azt a nullhipot�zist,
# hogy az �tlagos pulzussz�m azonos f�rfiak �s n�k eset�n.
# Adjunk meg egy 99 sz�zal�k megb�zhat�s�g� konfidencia intervallumot 
# a k�t v�rhat� �rt�k k�l�nbs�g�re.

var.test(Pulse ~ Sex, conf.level = 0.99)
t.test(Pulse ~ Sex, var.equal = T, conf.level = 0.99)

# =========================================================================

# Vizsg�ljuk meg a pulzussz�mokat a testmozg�s gyakoris�ga 
# (Exer v�ltoz�) szerint. 

summary(Exer,na.rm = TRUE) # Osszesen 115+24+98=237 egyenrol van Exer adat. 
tapply(Pulse, Exer, summary, na.rm = TRUE)
# 20+7+18=45 embernek nincs pulzusszam adata, igy osszesen 237-45 = 192 emberrol van pulzusszam adat.
tapply(Pulse, Exer, sd, na.rm = TRUE)
boxplot(Pulse ~ Exer)

# Tesztelj�k azt a nullhipot�zist, hogy a pulzussz�mok v�rhat� �rt�ke
# minden csoportban ugyanakkora.

leveneTest(Pulse ~ Exer, center = mean)

oneway.test(Pulse ~ Exer, var.equal = TRUE)

# H�ny sz�zal�kban magyar�zza a pulzussz�mot az, hogy az alany 
# milyen gyakran v�gez testmozg�st?

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


