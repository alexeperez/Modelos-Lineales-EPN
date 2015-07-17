library(readxl)
datarls <- read_excel("data_pib.xlsx",sheet = 1,col_names = TRUE,na = "")
str(datarls)
View(datarls)
names(datarls)
summary(datarls)


# Graf dispersion

library(ggplot2)
# pib vs Inflacion
g <- ggplot(data = datarls, aes(x=Inflacion, y=PIB))
g + geom_point() + geom_smooth(method="lm")

# pib vs 1/Inflacion
g <- ggplot(data = datarls, aes(x=1/Inflacion, y=PIB))
g + geom_point() + geom_smooth(method="lm")

inv_infla <- 1/datarls[,"Inflacion"]
datarls <- data.frame(datarls,inv_infla)

# pib vs EE
g <- ggplot(data = datarls, aes(x=EE, y=PIB))
g + geom_point() + geom_smooth(method="lm")

# Regresion entre PIB y EE + inv_infla
reg1 <- lm(PIB~EE + inv_infla, datarls)
summary(reg1)

# graficos residuales
u_t <- reg1$residuals

g <- ggplot(data = datarls, aes(x=inv_infla, y=u_t))
g + geom_point()
# var crece con el inverso de Inflacion

g <- ggplot(data = datarls, aes(x=EE, y=u_t))
g + geom_point()

# graf normal
qqnorm(u_t)
qqline(u_t)
# ley no normal


# problema de varianza no constante
# calculamos

ln_pib <- log(datarls[,"PIB"])
ln_infla <- log(datarls[,"Inflacion"])
ln_EE <- log(datarls[,"EE"])

# Regresion entre PIB y EE + inv_infla
reg1 <- lm(ln_pib~ln_EE + ln_infla, datarls)
summary(reg1)

# residuos
u_t <- reg1$residuals

g <- ggplot(data = datarls, aes(x=ln_infla, y=u_t))
g + geom_point()
# var crece con el inverso de Inflacion

g <- ggplot(data = datarls, aes(x=ln_EE, y=u_t))
g + geom_point()

# graf normal
qqnorm(u_t)
qqline(u_t)
# ley no normal

mean(u_t)
hist(u_t, breaks = 6)

# Pronosticos vs residuos
y_t <- reg1$fitted.values
plot(u_t,y_t)

# Puntos singulares
library(car)
outlierTest(reg1) # Bonferonni p-value for most extreme obs
qqPlot(reg1, main="QQ Plot") #qq plot for studentized resid 

# puntos influyentes
# Cook's plot
# D > 4/(n-k-1) 
cutoff <- 4/((nrow(datarls)-length(reg1$coefficients)-2)) 
plot(reg1, which=4, cook.levels=cutoff)
datarls[14,]
#plot(reg1)










