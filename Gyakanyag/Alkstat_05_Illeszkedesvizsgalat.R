# ==================================== #
# Alkalmazott statisztika              #
# 5. gyakorlat                         #
# Illeszkedés- és homogenitásvizsgálat #
# ==================================== #

# Olvassuk be a salary.txt állomány tartalmát az 'input' nevû táblázatba.

input = read.table("salary.txt", header = TRUE)
attach(input)

# Töltsük be a 'moments' és a "nortest" csomagokat.

library(moments) # skewness
library(nortest) # pearson.test

# =========================================================================

# Normalitásvizsgálat

# Adjunk pontbecslést a 'salary' változó várható értékére és szórására,
# majd kérdezzük le a minta mediánját, ferdeségét is.
# Ezek alapján mit várunk, milyen a 'salary' változó sûrûségfüggvénye
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
# majd illesszük rá a paraméterbecslésekbõl kapott normális eloszlás 
# sûrûségfüggvényét. Ábrázoljuk a 'salary' változó boxplotját is.

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

# Definiáljunk egy 'logsalary' nevû változót olyan módon,
# hogy a 'salary' változó logartimusát vesszük. Végezzük el a fenti
# elemzést a 'logsalary' változóra is.

# =========================================================================

# Kétmintás Kolmogorov-Szmirnov-próba

# Ábrázoljuk a 'salary' változó boxplotját nemek szerinti bontásban.

boxplot(salary ~ gender)

# Kétmintás Kolmogorov-Szmirnov-próba segítségével teszteljük le
# azt a nullhipotézist, hogy a férfiak és a nõk körében azonos 
# a fizetés eloszlása.

ks.test(salary[gender == "Male"], salary[gender == "Female"])

# =========================================================================

detach(input)
