# ============================== #
# Alkalmazott statisztika        #
# 2. gyakorlat                   #
# Alapstatisztikák és grafikonok #
# ============================== #

# Olvassuk be a salary.txt állomány tartalmát az 'input' nevû táblázatba.
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

# Milyen irányú egyenlõtlenség tapasztalható a mintaátlag és a medián között?
# Ez alapján mit mondhatunk, várhatóan milyen az eloszlás alakja? 
# Kérdezzük le a minta ferdeségét is.

skewness(salary)

# Milyen értékeket kapunk normális eloszlású változó ferdeségére?

# Ábrázoljuk a 'salary' változó hisztogramját elõször az
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
# Ehhez elõször töröljük ki a hiányzó adatokat a 'child' vektorból. Hány
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

# Válasszuk le az 'input' táblázatot. Ellenõrizzük is le,
# hogy megtörtént a leválasztás.

detach(input)
salary
