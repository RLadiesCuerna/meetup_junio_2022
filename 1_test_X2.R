###CARGUEMOS LOS PAQUETES QUE USAREMOS

library(MASS)

###      A. LOS DATOS QUE USAREMOS

#  A.1 Tomaremos como ejemplo dos conjuntos de datos:

#  el data frame "survey" que está incluido en el paquete `MASS`
#  survey lista las respuestas de un grupo de 237 estudiantes
#  en una encuesta.

summary(survey)
View(survey)

#  Si no tienes el paquete `MASS` por favor carga la tabla survey.csv usando:
#survey <- read.csv("datos/survey.csv",header=TRUE,row.names = 1,stringsAsFactors = TRUE)


#  A.2 Datos de pobreza de CONEVAL
# https://www.coneval.org.mx/Paginas/principal.aspx (Consulta: 06 de agosto de 2019).
#  Población por condición de pobreza y tipo de localidad (rural o urbana)
#  en México. Unidad de medida: Miles de personas


pobreza<-read.table("datos/pobreza.csv", header=TRUE, sep=",", row.names=1)


###      B. CREANDO UNA TABLAS DE CONTEOS

#  B.1 Las siguientes tablas de conteos relacionan dos variables

#  La mano dominante (mano con la que escribes) con el sexo

sex_hand<-table(survey$Sex,survey$W.Hnd)

#  Condición socioeconómica y residencia rural o urbana

pobreza<-as.matrix(t(pobreza))

pobreza<-as.table(pobreza)

#  B.2 Hagamos represenaciones gráficas de estas tablas


mosaicplot(sex_hand, col=c(2,3,4,5))

mosaicplot(pobreza, col=c(2,3,4,5), las=2)


###      C. Test de independencia chi-cuadado
#
#        )  (
#        ( ) )
#          ( (
#      _______)_
#   .-'---------|
#  ( C| Yo <3 R |                       (based on: mrf)
#   '-.         |
#     '_________'
#      '-------'


#      El test de X2 evalúa si las variables son independientes entre sí
#      (Hipótesis nula -H0-) o, si por el contrario, los valores de una
#      variable dependen de los valores de la otra variable.


#  C.1 Llamando a la función chisq.test

#  C.1.a Tabla de contingencia de 2 x 2

chi_sex_hand<-chisq.test(sex_hand)

#  Aquí se nos avisa que se llevó a cabo la corrección de
#  Continuidad de Yates. Esta se usa en las tablas de 2x2
#  Y sirve para pevenir la sobre-estimación de la significancia
#  estadística cuando la tabla es pequeña.

#  C.1.b Tabla de contingencias mayor a 2 x 2

chi_pobreza<-chisq.test(pobreza)


#  C.2 Explorando el test de chisq.test


#  C.2.1 Qué valor de X se obtuvo, grados de libertad
#  y qué valor p le corresponde

chi_sex_hand
chi_pobreza


#  C.2.2 Valores esperados si no hay asociación de variables
#  y valores observados

chi_sex_hand$observed

chi_sex_hand$expected

chi_pobreza$observed

chi_pobreza$expected


#  C.2.3 Residuales

chi_pobreza$residuals

mosaicplot(pobreza, las=2, shade=TRUE)


# ¿Cuánto contribuye cada celda al valor total de
#  chi-cuadrado?

library(corrplot)

contrib <- 100*chi_pobreza$residuals^2/chi_pobreza$statistic

corrplot(contrib, is.cor=FALSE)



###      D. Test de Bondad de ajuste chi-cuadrado

#  ¿la probabilidad de estar en diferentes situaciones económico-sociales es similar?

pobreza_u<-pobreza[,1]

chi_pobreza_u<-chisq.test(pobreza_u, p=c(0.25,0.25,0.25,0.25))

mosaicplot(pobreza_u, las=2, dir="h")



###      D. Test exacto de Fisher

#  Este test se usa cuando en la tabla de contingencias hay cuentas pequeñas.
#  Es decir, cuando alguno de los valores esperados es menor a 5.
#  Tiene las mismas hipótesis que el test chi-cuadrado.

#  Tabla de conteos para: mano que queda arriba al aplaudir y mano dominante

hand_clap<-table(survey$W.Hnd,survey$Clap)

mosaicplot(hand_clap, col=c(2,3,4,5))

#   Hagamos un test de chi-cuadrado

chi_hand_clap<-chisq.test(hand_clap)

#  Aquí se nos advierte que nuestros resultados podrían
#  ser incorrectos, por lo que cambiamos al test de Fisher.

fisher.test(hand_clap)




######### EJERCICIOS Y TRUCOS ADICIONALES ###################################


#___ Midiendo el tamaño del efecto de asociacion  ___#

#  El valor de chi-cuadrada o el p.value no nos dicen por si mismos
#  Que tan fuerte es la asociacion entre las variables
#  Sino solamente que la asociacion es significativa
#  Es por eso que se hacen otros cálculos para medir el tamaño
#  del efecto


#  Tamaño del efecto para tablas de 2x2


infartos <- matrix(c(189, 104, 10845, 10933), nrow = 2)
dimnames(infartos) <- list("Grupo" = c("Placebo","Aspirina"), "infartos" = c("Si","No"))

infartos_p<-prop.table(infartos, margin = 1)

# riesgo relativo de infarto en placebo vs. infarto con aspirina

infartos_p[1,1]/infartos_p[2,1]

#  Hay un riesgo de infarto 1.81 veces mayor para los que toman el placebo
#  que para los que toman aspirina


# riesgo relativo de ser mujer zurda vs hombre zurdo

sex_hand_p<-prop.table(sex_hand, margin = 1)

sex_hand[1,1]/sex_hand[2,1]

#  hay un riesgo de ser zurdo de 0.7 para las mujeres en relación con
#  los hombres


#  Si queremos saber si estos riesgos son significativos debemos calcular un intervalo de confianza

#  install.packages("epitools") # Tal vez necesites instalar a epitools

library(epitools)


#  La funcion riskratio de epitools espera este orden de columnas y filas:

#                         disease=0   disease=1
#         tratamiento=0 (ref)    n00         n01
#         tratamiento=1          n10         n11


#  En el caso de la tabla de infartos tendremos que voltear las columnas

riskratio(infartos, rev="b")

#  El riesgo relativo es de 1.81 y su intervalo de confianza no pasa por
#  cero. Tenemos un efecto significativo.

oddsratio(infartos, rev="b")

#  El riesgo relativo es de 1.81 y su intervalo de confianza no pasa por
#  1. Esto quiere decir que por cada infarto con aspirina hay 1.83
#  infartos con el Placebo. Para muestras pequeñas el risk ratio y el
#  odds ratio suelen ser parecidos.

oddsratio(sex_hand, rev="b")

#  El riesgo relativo es 0.69 y su intervalo de confianza no pasa por
#  cero. Tenemos un efecto significativo, pero el test de asociación
#  de variables chi-cuadrado no es significativo.



#  C.1 Tamaño del efecto para tablas mayores a 2x2

#  La V de Cramer, que sirve para medir el tamaño del efecto en este
#  caso se calcula como:
#  V = sqrt(X^2 / [nobs * (min(ncols, nrows) – 1)])

install.packages("DescTools")


library("DescTools")

CramerV(pobreza_t, conf.level=0.95)

CramerV(hand_clap, conf.level=0.95)


# Interpretación de la V de cramer

#     Efecto
#	df* pequeño	mediano	grande
#	1	  0.10	0.30	0.50
#	2	  0.07	0.21	0.35
#	3	  0.06	0.17	0.29
#	4	  0.05	0.15	0.25
#	5	  0.04	0.13	0.22

#  *grados de libertad
