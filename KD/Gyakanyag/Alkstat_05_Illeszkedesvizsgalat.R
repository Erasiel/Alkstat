# ==================================== #
# Alkalmazott statisztika              #
# 5. gyakorlat                         #
# Illeszked�s- �s homogenit�svizsg�lat #
# ==================================== #

# Olvassuk be a salary.txt �llom�ny tartalm�t az 'input' nev� t�bl�zatba.

input = read.table("salary.txt", header = TRUE)
attach(input)

# T�lts�k be a 'moments' �s a "nortest" csomagokat.

library(moments) # skewness
library(nortest) # pearson.test

# =========================================================================

# Normalit�svizsg�lat

# Adjunk pontbecsl�st a 'salary' v�ltoz� v�rhat� �rt�k�re �s sz�r�s�ra,
# majd k�rdezz�k le a minta medi�nj�t, ferdes�g�t is.
# Ezek alapj�n mit v�runk, milyen a 'salary' v�ltoz� s�r�s�gf�ggv�nye
# a norm�lis eloszl�shoz viszony�tva?

mean(salary)
sd(salary)
median(salary)
skewness(salary) # ferdes�g

# Standard norm�lis:
set.seed(1234)
minta = rnorm(1000)
mean(minta)
sd(minta)
median(minta)
skewness(minta)
IQR(minta) # tapasztalati interkvartilis terjedelem.
IQR_elmeleti = qnorm(0.75) - qnorm(0.25) # standard normalis eloszlasl (elmeleti) interkvartilis terjedelme 1.34898.

# �br�zoljuk a 'salary' v�ltoz� relat�v gyakoris�gi hisztogramj�t, 
# majd illessz�k r� a param�terbecsl�sekb�l kapott norm�lis eloszl�s 
# s�r�s�gf�ggv�ny�t. �br�zoljuk a 'salary' v�ltoz� boxplotj�t is.

hist(salary, prob = TRUE)
curve(dnorm(x, mean = mean(salary), sd = sd(salary)), add = TRUE)
boxplot(salary)

# Standardiz�ljuk a 'salary' v�ltoz�t, majd QQ-�bra seg�ts�g�vel 
# vizsg�ljuk meg, hogy az �gy kapott v�ltoz� standard norm�lis 
# eloszl�st k�vet-e.

std_salary = scale(salary)
qqnorm(std_salary)
qqline(std_salary)

# A Kolmogorov-Szmirnov-pr�ba alkalmaz�s�val tesztelj�k le
# 5 sz�zal�kos szignifikanciaszinten azt a nullhipot�zist, hogy
# a 'salary' v�ltoz� norm�lis eloszl�st k�vet.

ks.test(salary, "pnorm", mean(salary), sd(salary))

# Tesztelj�k a normalit�st khi-n�gyzet pr�b�val is.

pearson.test(salary)

# Defini�ljunk egy 'logsalary' nev� v�ltoz�t olyan m�don,
# hogy a 'salary' v�ltoz� logartimus�t vessz�k. V�gezz�k el a fenti
# elemz�st a 'logsalary' v�ltoz�ra is.

# =========================================================================

# K�tmint�s Kolmogorov-Szmirnov-pr�ba

# �br�zoljuk a 'salary' v�ltoz� boxplotj�t nemek szerinti bont�sban.

boxplot(salary ~ gender)

# K�tmint�s Kolmogorov-Szmirnov-pr�ba seg�ts�g�vel tesztelj�k le
# azt a nullhipot�zist, hogy a f�rfiak �s a n�k k�r�ben azonos 
# a fizet�s eloszl�sa.

ks.test(salary[gender == "Male"], salary[gender == "Female"])

# =========================================================================

detach(input)
