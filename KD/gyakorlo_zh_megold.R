#-----------------------------------------

input = read.csv("admission.csv");
adat = input[input$De == "notadmit",]
t.test(adat$GPA, mu = 2.4)

library(car)
input = data.frame(Blackmore)
var.test(input$exercise ~ input$group, conf.level = 0.95)

library(MASS)
input = data.frame(survey)
boxplot(input$Pulse)$out


adat1 = input[input$De == "notadmit",]
adat2 = input[input$De == "admit",]
t.test(adat2$GPA, adat1$GPA, var.equal = TRUE)

elemszam = 50   # n
mintaatlag = 360
mintaszoras = 30
teszt = 320  # mu0
alpha = 0.01

( konf.int_also = (mintaatlag - teszt) - qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )
( konf.int_felso = (mintaatlag - teszt) + qt(1-alpha/2, df = elemszam-1)*mintaszoras/sqrt(elemszam) )

library(datasets)
input = data.frame(ToothGrowth)
oneway.test(input$len ~ input$dose, input, var.equal = TRUE)
mean(input$len)

install.packages("mice")
library(mice)
input = data.frame(nhanes)
length(input[is.na(input)])