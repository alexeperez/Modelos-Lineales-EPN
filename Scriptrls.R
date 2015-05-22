
list.files()
datarls_ingr <- read.table("data_rls_ingr.txt", header = TRUE, dec=",", sep="\t")
str(datarls_ingr)

datarls_endeu <- read.table("data_rls_endeu.txt", header = TRUE, dec=",", sep="\t")
str(datarls_endeu)

# Union de bases de datos
data <- merge(x = datarls_ingr ,y = datarls_endeu, by = "IdNumerico", suffixes = c("","")) 
str(data)
names(data)
#View(data)


# Resumen
summary(data)

# Distribucion
library(ggplot2)
#install.packages(ggplot2, dependencies = TRUE)
# Ingreso
g <- ggplot(data = data, aes(x=Region, y=Ingreso, fill=Region))
g + geom_boxplot() 

# Endeudamiento
g <- ggplot(data = data, aes(x=Region, y=EndeudProm, fill=Region))
g + geom_boxplot() 

# Atipico
atip <- max(data[,"EndeudProm"])
atip <- 20000
data <- subset(data, subset=data[,"EndeudProm"]<=atip)
summary(data)
# Diag Cajas
g <- ggplot(data = data, aes(x=Region, y=Ingreso, fill=Region))
g + geom_boxplot() 

g <- ggplot(data = data, aes(x=Region, y=EndeudProm, fill=Region))
g + geom_boxplot() 


# Diag de puntos
g <- ggplot(data = data, aes(x=EndeudProm, y=Ingreso, colour=Region))
g + geom_point(alpha =0.6) 

# Diag de puntos
# Region
g <- ggplot(data = data, aes(x=EndeudProm, y=Ingreso, colour=Region))
g + geom_point(alpha =0.6) + geom_smooth(method="lm", color="steelblue4")+
        facet_grid(Region~.,margins = TRUE)

# Genero
g <- ggplot(data = data, aes(x=EndeudProm, y=Ingreso, colour=Genero))
g + geom_point(alpha =0.6) + geom_smooth(method="lm", color="steelblue4")+
        facet_grid(Genero~.,margins = TRUE)


# Correlacion
cor(x = data[,"Ingreso"], y= data[,"EndeudProm"])
cor(x = data[,"Ingreso"], y= data[,"Edad"])
cor(x = data[,"Ingreso"], y= data[,"Antiguedad"])


# Regresion lineal

reg1 <- lm(Ingreso ~ EndeudProm, data)
str(reg1)
summary(reg1)

reg2 <- lm(Ingreso ~ Edad, data)
str(reg2)
summary(reg2)

reg3 <- lm(Ingreso ~ Score, data)
str(reg3)
summary(reg3)

