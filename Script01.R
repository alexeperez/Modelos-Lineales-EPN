dir <- "C:/Users/Toshiba/Desktop/Clases EPN/Modelos Lineales/Modelos-Lineales-EPN"
setwd(dir)
list.files()

data <- read.table("data.txt", header = TRUE, dec=",", sep="\t")
str(data)
#View(data)
is.list(data)
is.atomic(data)

class(data)
names(data)
colnames(data)
nombres <- colnames(data)
nombres

v1 <- dim(data)
v1*2
nrow(data)b
ncol(data)

edad <- data[,3]
edad <- data[,"EDAD"]

typeof(edad)
is.list(edad)
is.atomic(edad)

mean(edad)
sd(edad)
min(edad)
max(edad)

hist(edad,col = "steelblue")

# subsetting
str(data)
data_f <- subset(data, subset=data[,"SEXO"]=="FEMENINO")
table(data[,"SEXO"])
table(data_f[,"SEXO"])

# Lazos
tipos <- numeric(ncol(data))
clase <- numeric(ncol(data))
for (i in 1:ncol(data)){
       tipos[i] <- typeof(data[,i])
       clase[i] <- class(data[,i])
}
tipos
clase

# codigo para contar los datos perdidos NA
vacios <- numeric(ncol(data))
for (i in 1:ncol(data)){
vacios[i] <- sum(is.na(data[,i]))
}
vacios
View(vacios)