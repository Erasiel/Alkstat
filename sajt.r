input = data.frame(airquality)
attach(input)
model = lm(Ozone ~ Temp)
summary(model)
detach(input)

attach(car_sales)
model = princomp(car_sales[3:13], cor=T)
summary(model)

cor.test(horsepow, price, method = "pearson")
detach(car_sales)

input = data.frame(airquality)
attach(input)
model = lm(Ozone ~ Temp)
model$coefficients
detach(input)

input = data.frame(stackloss)
attach(input)
model = princomp(input, cor = T)
summary(model)

data = loadings(model)
data
detach(input)

input = data.frame(airquality)
attach(input)
model = lm(Ozone ~ Temp)
model$coefficients
detach(input)

attach(car_sales)
model = princomp(car_sales[3:13], cor = T)
data = loadings(model)
data
detach(car_sales)

input = data.frame(chickwts)
attach(input)
oneway.test(weight ~ factor(feed), var.equal = T)
detach(input)

input = data.frame(stackloss)
attach(input)
model = princomp(input, cor = T)
summary(model)
