# Autor: Antonio Mendes M. Jr
# Ano: 2020

min <- c(0,30,60,90,150,204,243,303,369,426,486,560,623,702,780,840)
umidade <- c(.99,.94,.89,.76,.62,.55,.41,.27,.16,.1,.07,.05,.041,.039,.038,.037)

x=min
y=umidade

plot(x,y)

reg_lewis <- nls(y ~ exp(-k*x), start=c(k=0.01))
reg_page <- nls(y ~ exp(-k*(x^n)), start=c(k=0.01,n=1))
reg_henderson <- nls(y ~ k0*exp(-k*x), start=c(k=0.01,k0=1))

# verificar os coeficientes
coef(reg_lewis)
coef(reg_page)
coef(reg_henderson)

#coeficiente de determinação
library(qpcR)
Rsq(reg_lewis)
Rsq(reg_page)
Rsq(reg_henderson)

# desvio padrão residual
summary(reg_lewis)$sigma
summary(reg_page)$sigma
summary(reg_henderson)$sigma

# critério de Akaike
AIC(reg_lewis)
AIC(reg_page)
AIC(reg_henderson)

plot(x,y, xlab="Minutos",ylab="Umidade")
lines(x,fitted(reg_page), col="red", type="l")
