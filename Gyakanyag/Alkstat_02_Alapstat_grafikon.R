# ============================== #
# Alkalmazott statisztika        #
# 2. gyakorlat                   #
# Alapstatisztik�k �s grafikonok #
# ============================== #

# Olvassuk be a salary.txt �llom�ny tartalm�t az 'input' nev� t�bl�zatba.
# Csatoljuk fel a t�bl�zatot, hogy �gy k�nnyebben tudjunk hivatkozni
# az oszlopokra.

input = read.table("salary.txt", header = TRUE)
attach(input)

# T�lts�k be a 'moments' csomagot a skewness f�ggv�ny haszn�lat�hoz.

library(moments)      # skewness

# =====================================================================

# H�ny alkalmazott szerepel a t�bl�zatban?

length(salary)

# Adjunk pontbecsl�st a 'salary' v�ltoz� v�rhat� �rt�k�re, sz�r�s�ra,
# varianci�j�ra �s medi�nj�ra. 

mean(salary)
sd(salary)
var(salary)
median(salary)

# A kapott empririkus sz�r�s (�s variancia) korrig�lt vagy korrig�latlan?

# Milyen ir�ny� egyenl�tlens�g tapasztalhat� a minta�tlag �s a medi�n k�z�tt?
# Ez alapj�n mit mondhatunk, v�rhat�an milyen az eloszl�s alakja? 
# K�rdezz�k le a minta ferdes�g�t is.

skewness(salary)

# Milyen �rt�keket kapunk norm�lis eloszl�s� v�ltoz� ferdes�g�re?

# �br�zoljuk a 'salary' v�ltoz� hisztogramj�t el�sz�r az
# alapbe�ll�t�sokkal, majd 20 beoszt�ssal.

hist(salary)
hist(salary, breaks = 20)

# A hisztogram alapj�n mit mondhatunk az eloszl�s alakj�r�l?

# Boxdiagram alapj�n mit mondhatunk az eloszl�s alakj�r�l?

boxplot(salary)

# Vannak-e kiugr� �rt�kek?

# Hat�rozzuk meg a 'salary' v�ltoz� eset�ben a kvartiliseket,
# a legkisebb �s a legnagyobb �rt�ket, valamint a minta terjedelm�t �s
# interkvartilis terjedelm�t.

quantile(salary)
quantile(salary, probs = c(0.25, 0.5, 0.75))
max(salary)
min(salary)
max(salary) - min(salary)
IQR(salary)

summary(salary)

# =====================================================================

# Hat�rozzuk meg a 'child' v�ltoz� �rt�keinek gyakoris�g�t,
# relat�v gyakoris�g�t, majd �br�zoljuk a gyakoris�gokat 
# oszlopdiagrammal �s tortadiagrammal.

child.freq = table(child)
child.freq
prop.table(child.freq)
barplot(child.freq)
pie(child.freq)

# Hat�rozzuk meg, hogy h�ny gyermeke van az alkalmazottaknak �sszesen.
# Ehhez el�sz�r t�r�lj�k ki a hi�nyz� adatokat a 'child' vektorb�l. H�ny
# alkalmazottn�l nem tudjuk, hogy h�ny gyermeke van?

sum(child)
child2 = child[!is.na(child)]
sum(child2)
length(child) - length(child2)

# Egy t�bl�zatban adjuk meg a 'jobkat' �s a 'child' v�ltoz� egy�ttes
# eloszl�s�t. K�rdezz�k le az �tlagos gyereksz�mot a munkahelyi beoszt�s
# szerinti bont�sban.

table(jobkat, child)
tapply(child, jobkat, mean, na.rm = TRUE)

# =====================================================================

# V�lasszuk le az 'input' t�bl�zatot. Ellen�rizz�k is le,
# hogy megt�rt�nt a lev�laszt�s.

detach(input)
salary
