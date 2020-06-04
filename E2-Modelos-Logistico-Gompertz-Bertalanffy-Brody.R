# Autor: Antonio Mendes M. Jr
# Ano: 2020
# 
idade <- c(0,15,31,45,60,75,90,105,120,134,150,165,180)
peso <- c(35.4,37.8,41.2,66.5,97.1,125.5,201.6,300.2,355.8,413.3,428.4,424.5,414.9)

x=idade
y=peso

plot(x,y)

reg_log <- nls(y ~ a/(1+exp(k*(b-x))), start=c(a=450,b=100, k=0.1))
reg_gomp <- nls(y ~ a*exp(-exp(k*(b-x))), start=c(a=450,b=100, k=0.1))
reg_bertal <- nls(y ~ a*(1-exp(k*(b-x))/3)^3, start=c(a=450,b=50, k=0.1))

reg_brody <- nls(y ~ a*(1-exp(k*(b-x))), start=c(a=450,b=4, k=0.08)) # não converge

# verificar os coeficientes
coef(reg_log)
coef(reg_gomp)
coef(reg_bertal)
coef(reg_brody)

#coeficiente de determinação
library(qpcR)
Rsq(reg_log)
Rsq(reg_gomp)
Rsq(reg_bertal)
Rsq(reg_brody)

# desvio padrão residual
summary(reg_log)$sigma
summary(reg_gomp)$sigma
summary(reg_bertal)$sigma
summary(reg_brody)$sigma

# critério de Akaike
AIC(reg_log)
AIC(reg_gomp)
AIC(reg_bertal)
AIC(reg_brody)

plot(x,y, xlab="Idade",ylab="Peso")
lines(x,fitted(reg_log), col="red")
