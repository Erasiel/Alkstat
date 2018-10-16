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

# Adjuk meg a pulzusszámok (Pulse változó) mintabeli jellemzõit nemek 
# szerint, majd készítsünk hisztogramot és boxdiagramot nemek szerint.

tapply(Pulse, Sex, summary)
tapply(Pulse, Sex, sd, na.rm = T)
hist(Pulse[Sex == "Male"])
hist(Pulse[Sex == "Female"])
boxplot(Pulse ~ Sex)

# 1 százalékos szignifikancia szinten teszteljük azt a nullhipotézist,
# hogy az átlagos pulzusszám azonos férfiak és nõk esetén.
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


