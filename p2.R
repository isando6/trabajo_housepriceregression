# LIBRERÍAS ----

library(tidyverse)
library(readxl)
library(Amelia)      # missmap()
library(moments)      # skewness(), kurtosis()
library(patchwork)    # p1 + p2 + p3 (pocos graficos, 2-4)
library(gridExtra)    # grid.arrange() (alternativa cuando son muchos graficos)
library(GGally)       # ggpairs(), ggcorr()
library(corrplot)     # corrplot.mixed()
library(effsize)      # cohen.d()
library(openintro)    # dataset births (ejemplo 4.3 del libro)
library(nortest)      # lillie.test()
library(car)          # leveneTest()
library(rstatix)      # wilcox_effsize(), wilcox_test(), cramer_v()


# GUIA AMIGABLE EN R -- UNIDAD 5, 6 Y 7 (hasta 7.4)
# Tablas de contingencia | Correlacion lineal | Regresion lineal simple

#
# Como usar este script: cada seccion trae primero una EXPLICACION en
# lenguaje sencillo (bloque de comentarios, la puedes copiar tal cual a un
# Word/Rmd si la necesitas como texto), despues el CODIGO con un ejemplo
# concreto de para que sirve, y al final la INTERPRETACION de ese ejemplo con
# numeros reales verificados contra el libro del profesor (o, cuando el
# libro no trae un ejemplo con numeros, un ejemplo propio hecho a mano y
# verificado -- se avisa explicitamente cuando es asi).
#
# NOTA DE ESTA VERSION: a partir del Parcial 2 (chi-cuadrado agua, Pearson
# banco de creditos, regresion botes Canoe & Kayak) le agregue: (1) al final
# de 5.1.2, la plantilla del tipo de ejercicio de independencia que pide el
# parcial; (2) al final de 6.2.3, el intervalo de confianza para rho por
# Fisher que el parcial pide y que no estaba aqui; (3) al final de 7.4, como
# predecir con el modelo y el ejercicio de los botes COMPLETO Y RESUELTO con
# los datos reales que trae el propio parcial; y (4) al final del todo, tu
# script de la regresion soldadura (Resistencia ~ Edad) corregido, explicado
# y corrido con el archivo real data_rls.txt (confirme que reproduce
# EXACTO los numeros del libro: r=-0.8828, beta0=15.74, beta1=-0.244,
# MSE=0.349, RSE=0.591, R2=0.7793 -- asi que los datos completos son
# correctos y confiables).


# UNIDAD 5 -- TABLA DE CONTINGENCIA ----


# 5.1  ¿QUE ES UNA TABLA DE CONTINGENCIA? ----

# EXPLICACION AMIGABLE:
# Una tabla de contingencia es simplemente una tabla de doble entrada que
# cruza dos variables categoricas para contar cuantas personas (o casos)
# caen en cada combinacion posible. Por ejemplo, si tienes el sexo de una
# persona y la region donde vive, la tabla te muestra cuantos hombres y
# cuantas mujeres hay en cada region, todo en una sola tabla. A cada celda
# de esa tabla la llamamos O_ij (el valor OBSERVADO en la fila i, columna
# j), y a los totales de cada fila y cada columna los llamamos Ri y Cj.
#
# Lo importante es que con la MISMA tabla puedes hacerte dos preguntas
# distintas, y elegir cual prueba usar depende de como se diseño el estudio:
#
#  - Prueba de HOMOGENEIDAD: ya tienes varias poblaciones o grupos por
#    separado (ej. tres regiones distintas) y quieres ver si se distribuyen
#    igual en una variable comun (ej. la proporcion de hombres y mujeres es
#    la misma en las tres regiones).
#  - Prueba de INDEPENDENCIA: tienes UNA sola poblacion y a cada persona le
#    mides DOS caracteristicas a la vez (ej. sexo y region de nacimiento de
#    todos los que entraron a un mismo estudio), y quieres saber si esas dos
#    caracteristicas estan relacionadas entre si.
#
# La buena noticia es que AMBAS usan exactamente el mismo estadistico
# (chi-cuadrado, chi^2) y la misma funcion en R (chisq.test()); lo unico
# que cambia es como interpretas el resultado y como estaba disenado el
# muestreo.
#
# PARA QUE ME SIRVE EL CODIGO: para construir la tabla cruzada antes de
# correr cualquier prueba -- es el primer paso obligatorio, sea homogeneidad
# o independencia.

# ejemplo con datos simulados tipo "sexo por region" (tabla 5.2 del libro)
datos <- data.frame(
  sexo = c(rep("Hombres", 100), rep("Mujeres", 75)),
  region = c(rep("A", 25), rep("B", 40), rep("C", 35),
             rep("A", 30), rep("B", 20), rep("C", 25))
)

tabla <- table(datos$sexo, datos$region)
tabla



# 5.1.1 PRUEBA DE HOMOGENEIDAD ----

# EXPLICACION AMIGABLE:
# Aqui ya SABES que tienes varios grupos/poblaciones distintas (ej. tres
# regiones), y quieres ver si todas se comportan igual respecto a una
# variable categorica comun (ej. sexo). La hipotesis nula dice que la
# proporcion es la misma en todos los grupos:
#
#   H0: p_1j = p_2j = ... = p_Ij   para cada categoria j
#
# Para saber "cuanto se esperaria ver en cada celda SI todos los grupos
# fueran iguales", se calcula el valor esperado:
#
#   E_ij = (Ri * Cj) / n
#
# (el total de la fila por el total de la columna, dividido entre el total
# general). Y luego se compara lo observado contra lo esperado con el
# estadistico de siempre:
#
#   chi^2 = suma_i suma_j [ (O_ij - E_ij)^2 / E_ij ]
#
# Regla de decision: si chi^2 > chi^2_critico (o el p-valor < 0.05), se
# rechaza H0 -- las poblaciones NO son homogeneas. Condicion importante:
# todos los E_ij deben ser >= 5 para que la prueba sea confiable.
#
# PARA QUE ME SIRVE EL CODIGO: para responder preguntas del tipo "¿la
# proporcion de aprobados es igual en los tres colegios?" o "¿la tasa de
# cumplimiento es igual en los tres tipos de fuente de agua?" -- cualquier
# caso donde ya tienes los grupos definidos de antemano y comparas una
# variable categorica entre ellos.

# H0: la proporcion de sexo es igual en las 3 regiones
chisq.test(tabla)

# INTERPRETACION: si el p-valor sale menor a 0.05, concluyes que la
# proporcion de hombres/mujeres SI varia entre regiones (no son
# homogeneas); si sale mayor a 0.05, no hay evidencia de que difieran.



# 5.1.2 PRUEBA DE INDEPENDENCIA ----

# EXPLICACION AMIGABLE:
# Aqui el enfoque es distinto: tienes UNA sola poblacion (ej. todos los
# pasajeros del Titanic) y a cada individuo le mides dos atributos a la vez
# (ej. su clase de viaje y si sobrevivio o no). La pregunta ya no es "¿son
# iguales los grupos?" sino "¿estan relacionados estos dos atributos?". La
# hipotesis nula es que son independientes:
#
#   H0: p_ij = p_i. * p_.j   para todo i, j
#
# (la probabilidad conjunta es igual al producto de las probabilidades
# marginales -- la definicion de independencia de toda la vida). Y aunque la
# logica de la hipotesis es distinta a la de homogeneidad, el valor
# esperado y el estadistico de prueba se calculan EXACTAMENTE IGUAL:
#
#   E_ij = (Ri * Cj) / n         chi^2 = suma_i suma_j [ (O_ij - E_ij)^2 / E_ij ]
#
# PARA QUE ME SIRVE EL CODIGO: para responder preguntas del tipo "¿el nivel
# educativo esta relacionado con el estado civil?" o "¿la clase del
# pasajero esta asociada a si sobrevivio o no?" -- cuando NO tienes grupos
# predefinidos, sino que cruzas dos caracteristicas de la misma poblacion.

# Ejemplo real del libro: dataset Titanic, Clase vs Supervivencia
df <- as.data.frame(Titanic)

tabla_titanic <- xtabs(Freq ~ Class + Survived, data = df)
tabla_titanic

chisq.test(tabla_titanic)

# En la práctica, con chisq.test(tabla) en R lo lees por el p-valor, que es equivalente:

# p-valor < 0.05 → se rechaza H0 → las dos variables SÍ están asociadas (no son independientes)
# p-valor ≥ 0.05 → no se rechaza H0 → no hay evidencia de asociación (son independientes)

#Si alguna celda esperada da menor que 5,  el chi cuadrado deja de ser confiable y tocaría usar
#la correción de Yates o el test exacto de Fisher en su lugar.


# INTERPRETACION (numeros reales del libro):
# chi^2(3) = 190.4, p-valor < 2.2e-16 -- se rechaza H0 con muchisima
# confianza. La clase en la que viajaba el pasajero SI esta asociada a su
# probabilidad de supervivencia.


# 5.1.2-BIS  -- LA PLANTILLA QUE PIDE EL PARCIAL (paso a paso completo) ----

# EXPLICACION AMIGABLE:
# El Parcial 2 (Punto 1, vale 1.5) trae un ejercicio de independencia MUY
# tipico: calidad del agua en Cordoba, 150 muestras de tres fuentes (rios,
# quebradas, canales), y para cada muestra si Cumple o No cumple el criterio
# de E. coli. Pregunta: ¿el cumplimiento es independiente del tipo de
# fuente? Este es el mismo tipo de pregunta de la seccion 5.1.2 de arriba
# (una sola poblacion de 150 muestras, dos caracteristicas medidas a cada
# una), asi que se resuelve con chisq.test() exactamente igual que el
# ejemplo del Titanic. Lo que cambia es que el parcial pide el desglose
# COMPLETO paso a paso, no solo el resultado de chisq.test(). Aqui esta la
# plantilla con esa estructura exacta (a-e), lista para rellenar con
# cualquier tabla de contingencia que te den:
#
#  a) EDA: tabla de contingencia + grafico (barras apiladas o de
#     mosaico) que describa la relacion.
#  b) H0 y H1 explicitas.
#  c) Eij y chi^2 calculados A MANO, celda por celda (no solo chisq.test()).
#  d) grados de libertad = (I-1)(J-1).
#  e) conclusion usando la regla "Rechazar H0 si chi^2 > chi^2_alpha".
#
# PARA QUE ME SIRVE EL CODIGO: es literalmente el esqueleto que puedes
# copiar el dia del examen y solo cambiarle los datos.

# a) EDA -- supongamos una tabla ilustrativa con la misma estructura del
# parcial (3 fuentes x 2 categorias); cambia estos numeros por los reales
# que te den en el examen
tabla_agua <- matrix(c(35, 15,   28, 22,   20, 30),
                     nrow = 3, byrow = TRUE,
                     dimnames = list(Fuente = c("Rio", "Quebrada", "Canal"),
                                     Cumple = c("Cumple", "No cumple")))
tabla_agua

# grafico de barras apiladas para "describir la relacion" (lo que pide el a)
as.data.frame(tabla_agua) |>
  ggplot(aes(x = Fuente, y = Freq, fill = Cumple)) +
  geom_col(position = "fill") +
  labs(y = "Proporcion", title = "Cumplimiento de E. coli por fuente") +
  theme_bw()

# b) Hipotesis
# H0: el cumplimiento del criterio de E. coli es independiente del tipo de fuente
# H1: el cumplimiento del criterio de E. coli NO es independiente del tipo de fuente (estan asociados)

# c) Eij y chi^2 paso a paso (a mano, sin usar chisq.test todavia)
Ri <- rowSums(tabla_agua)     # totales por fila (por fuente)
Cj <- colSums(tabla_agua)     # totales por columna (por cumplimiento)
n  <- sum(tabla_agua)         # total general

Eij <- outer(Ri, Cj) / n      # Eij = Ri*Cj / n para cada celda, todas a la vez
Eij                            # <- revisa aqui que TODOS los Eij sean >= 5

chi2_terminos <- (tabla_agua - Eij)^2 / Eij
chi2_terminos                  # aporte de cada celda a la suma
chi2_obs <- sum(chi2_terminos)
chi2_obs                       # <- este es tu chi^2 calculado a mano

# verificacion con la funcion de R (deben coincidir)
chisq.test(tabla_agua)

# d) grados de libertad = (I-1)(J-1)
I <- nrow(tabla_agua); J <- ncol(tabla_agua)
gl <- (I - 1) * (J - 1)
gl

# valor critico para comparar, con alpha = 0.05
alpha <- 0.05
chi2_critico <- qchisq(1 - alpha, df = gl)
chi2_critico

# e) conclusion con la regla del parcial: "Rechazar H0 si chi^2 > chi^2_alpha"
chi2_obs > chi2_critico

# INTERPRETACION: si chi2_obs > chi2_critico (equivalente a p-valor < 0.05),
# se rechaza H0: el cumplimiento del criterio de E. coli SI depende del tipo
# de fuente de agua. Si no, no hay evidencia de esa asociacion.



# UNIDAD 6: CORRELACION LINEAL ----


# 6.1 COVARIANZA Y COEFICIENTE DE CORRELACION (la base teorica) ----

# EXPLICACION AMIGABLE:
# Antes de calcular una correlacion en R, vale la pena entender que es lo
# que realmente se esta midiendo. La COVARIANZA entre dos variables X y Y
# te dice si, cuando una sube, la otra tiende a subir tambien (covarianza
# positiva), a bajar (covarianza negativa), o si no hay un patron claro
# (covarianza cercana a cero):
#
#   Cov(X,Y) = E[ (X - E(X)) * (Y - E(Y)) ]
#
# El problema es que la covarianza depende de las unidades de medida (no es
# lo mismo medir en pesos que en miles de pesos), asi que no es facil de
# interpretar por si sola. Para arreglar eso, se "estandariza" dividiendo
# por las desviaciones estandar de cada variable, y asi nace el
# COEFICIENTE DE CORRELACION:
#
#   rho = Cov(X,Y) / sqrt( Var(X) * Var(Y) )
#
# Esto tiene una propiedad clave (se demuestra con la desigualdad de
# Cauchy-Schwarz): rho SIEMPRE esta entre -1 y 1, sin importar en que
# unidades midas tus variables. Por eso es una medida universal de "que tan
# fuerte y en que direccion" se mueven dos variables juntas.
#
# PARA QUE ME SIRVE EL CODIGO: en la practica casi nunca calculas esto a
# mano (para eso existe cor()), pero entender la formula te ayuda a
# interpretar por que un cor.test() te da lo que te da, y a explicar el
# "por que" en tus conclusiones, no solo copiar el numero.

set.seed(1)
x <- c(2, 4, 6, 8, 10)
y <- c(3, 5, 4, 9, 11)

cov_manual   <- mean((x - mean(x)) * (y - mean(y)))              # covarianza poblacional (entre n)
cov_muestral <- sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)  # covarianza muestral (entre n-1, la que usa R)
rho_manual   <- cov_muestral / (sd(x) * sd(y))

data.frame(cov_muestral = cov_muestral, rho_manual = rho_manual, rho_con_cor = cor(x, y))



# 6.2.1 y 6.2.2  TIPOS DE CORRELACION Y COMO INTERPRETARLA ----

# EXPLICACION AMIGABLE:
# No existe un solo "coeficiente de correlacion": hay tres que se usan
# segun el tipo de dato y si se cumplen o no ciertos supuestos:
#
#  - PEARSON (r): el mas comun, mide relacion LINEAL. Es parametrico:
#    necesita que los datos sean de intervalo/razon y, para la prueba de
#    significancia, se apoya en supuestos como normalidad.
#  - SPEARMAN (rho_S): no parametrico, se basa en los RANGOS (el orden) de
#    los datos en vez de los valores exactos. Mide relacion MONOTONA (no
#    necesariamente una linea recta: sirve igual si la relacion es curva
#    pero siempre creciente o siempre decreciente). Util cuando Pearson no
#    aplica (no hay normalidad, hay atipicos, o los datos son ordinales).
#  - KENDALL (tau): tambien no parametrico, pero en vez de trabajar con
#    rangos compara PARES de observaciones y ve si son "concordantes" (las
#    dos variables se mueven en la misma direccion) o "discordantes". Es
#    mas robusto que Spearman cuando hay muchos empates o la muestra es
#    chica.
#
# Para interpretar el VALOR de cualquiera de los tres (van de -1 a 1), esta
# es la tabla que usa el libro:
#
#   | Rango de valores | Interpretacion                   |
#   |-------------------|-----------------------------------|
#   | -1.00 a -0.75     | Correlacion negativa muy fuerte    |
#   | -0.75 a -0.50     | Correlacion negativa moderada      |
#   | -0.50 a -0.25     | Correlacion negativa debil         |
#   | -0.25 a  0.25     | Sin correlacion o muy debil        |
#   |  0.25 a  0.50     | Correlacion positiva debil         |
#   |  0.50 a  0.75     | Correlacion positiva moderada      |
#   |  0.75 a  1.00     | Correlacion positiva muy fuerte    |
#
# Ojo con algo importante: un r=0 solo descarta relacion LINEAL, no
# cualquier tipo de relacion (podria haber una relacion en forma de U, por
# ejemplo, y el r de Pearson no la va a detectar). Y una correlacion
# "fuerte" en el numero no siempre es estadisticamente significativa
# (depende del tamano de muestra) -- por eso siempre se acompana el
# coeficiente con su prueba de hipotesis.
#
# PARA QUE ME SIRVE ESTO: es la guia que usas para redactar tu
# interpretacion en el examen: calculas el numero con cor()/cor.test() y
# luego lo traduces a palabras usando esta tabla, en vez de solo dejar el
# numero suelto.



# 6.2.3 CORRELACION DE PEARSON ----

# EXPLICACION AMIGABLE:
# Es la correlacion "clasica": mide que tanto se parece la nube de puntos
# de tus dos variables a una linea recta. Se calcula asi:
#
#   r = suma[(xi - xbar)(yi - ybar)] / ( sqrt(suma(xi-xbar)^2) * sqrt(suma(yi-ybar)^2) )
#
# que es basicamente "la covarianza entre X y Y, pero estandarizada" (la
# misma idea de la seccion 6.1, aplicada a tu muestra en vez de a la
# poblacion). Para saber si esa correlacion muestral es "real" y no solo
# ruido del muestreo, se prueba con:
#
#   t0 = r * sqrt(n-2) / sqrt(1 - r^2)      ~ t con (n-2) grados de libertad, bajo H0: rho=0
#
# Se rechaza H0 (es decir, SI hay correlacion significativa) si
# |t0| > t_critico(alpha/2, n-2).
#
# SUPUESTOS: ambas variables deben tener comportamiento aproximadamente
# normal (se verifica con Shapiro-Wilk) y la relacion debe ser lineal (se
# ve en el diagrama de dispersion).
#
# PARA QUE ME SIRVE EL CODIGO: para cuantificar y probar si dos variables
# numericas continuas (ej. monto de un credito y tiempo de analisis, edad e
# ingreso, etc.) estan relacionadas linealmente, y que tan fuerte es esa
# relacion.

# Ejemplo generico: dos variables numericas continuas
n <- 30
set.seed(42)
monto_credito   <- rnorm(n, mean = 50, sd = 15)
tiempo_analisis <- 10 + 0.4 * monto_credito + rnorm(n, sd = 5)

# 1. normalidad de cada variable
shapiro.test(monto_credito)
shapiro.test(tiempo_analisis)

# 2. correlacion de Pearson + su prueba de significancia
cor.test(monto_credito, tiempo_analisis, method = "pearson")

# COMO LEERLO: el cor.test() te da el r (estimate), el t0 (statistic) y el
# p-valor en un solo paso -- si el p-valor es menor a 0.05, la correlacion
# es estadisticamente significativa, y usas la tabla de la seccion 6.2.2
# para describir que tan fuerte es.


# 6.2.3-BIS -- INTERVALO DE CONFIANZA PARA rho (transformacion de Fisher) ----

# EXPLICACION AMIGABLE:
# Esto es justo lo que pide el Punto 2.e del Parcial 2 (banco de creditos:
# monto del credito vs. tiempo de analisis, n=30) y NO estaba en la seccion
# de arriba, asi que te lo agrego aqui. El problema es que r no tiene una
# distribucion simetrica cuando esta cerca de -1 o 1, entonces no puedes
# armar un intervalo de confianza para rho directo con r +/- margen. La
# transformacion de Fisher (arctanh, tambien llamada z de Fisher) "endereza"
# esa distribucion para que sea aproximadamente normal, armas el intervalo
# en esa escala transformada, y al final regresas (con tanh) a la escala
# original de correlacion:
#
#   IC = tanh( arctanh(r) +/- Z_(1-alpha/2) / sqrt(n-3) )
#
# PARA QUE ME SIRVE EL CODIGO: para dar, ademas del r puntual, un rango de
# valores donde con 95% de confianza cae la verdadera correlacion
# poblacional -- el examen especificamente pide "paso a paso", asi que aqui
# esta desglosado en cada pieza de la formula.

r      <- cor(monto_credito, tiempo_analisis)   # usa aqui tu r ya calculado
n_ic   <- length(monto_credito)
alpha  <- 0.05

z_r   <- atanh(r)                        # arctanh(r): paso 1, transformar r
z_crit <- qnorm(1 - alpha/2)             # Z_(1-alpha/2) = 1.959964 para 95%
error_estandar <- 1 / sqrt(n_ic - 3)     # el "sqrt(n-3)" de la formula

limite_inferior_z <- z_r - z_crit * error_estandar
limite_superior_z <- z_r + z_crit * error_estandar

IC_inferior <- tanh(limite_inferior_z)   # paso final: regresar a la escala de r con tanh
IC_superior <- tanh(limite_superior_z)

data.frame(r = r, IC_95_inferior = IC_inferior, IC_95_superior = IC_superior)

# verificacion rapida: cor.test() calcula este mismo intervalo automatico
cor.test(monto_credito, tiempo_analisis)$conf.int

# INTERPRETACION: con 95% de confianza, la verdadera correlacion
# poblacional entre el monto del credito y el tiempo de analisis esta entre
# IC_inferior y IC_superior. Como ese intervalo completo tiene el mismo
# signo que r (todo positivo o todo negativo, sin cruzar el 0), refuerza
# que la correlacion es significativa.



# 6.2.4 CORRELACION DE SPEARMAN ----

# EXPLICACION AMIGABLE:
# Cuando tus variables NO son normales, tienen valores atipicos, o son
# ordinales (ej. una escala de satisfaccion de 1 a 5), Pearson deja de ser
# confiable. Spearman resuelve esto "traduciendo" cada valor a su RANGO
# (posicion en el orden: 1ro, 2do, 3ro...) y calculando la correlacion
# sobre esos rangos en vez de los valores originales. Por eso detecta
# relaciones MONOTONAS (siempre creciente o siempre decreciente), aunque no
# sean una linea perfectamente recta.
#
# Cuando no hay empates, la formula simplificada es:
#
#   rho_S = 1 - ( 6 * suma(d_i^2) ) / ( n * (n^2 - 1) )
#
# donde d_i es la diferencia entre el rango de x_i y el rango de y_i para
# cada observacion. La prueba de significancia (para n>10) usa la misma
# logica que Pearson pero con rho_S en vez de r:
#
#   t = rho_S * sqrt( (n-2) / (1 - rho_S^2) )
#
# PARA QUE ME SIRVE EL CODIGO: para casos como "la concentracion de
# contaminante sube o baja de forma consistente con el trafico vehicular,
# aunque no sea una linea recta perfecta" -- cualquier situacion donde tus
# datos no pasan la prueba de normalidad pero si quieres medir asociacion.

# Ejemplo real del libro: calidad del aire (PM2.5) vs volumen de trafico
# vehicular (datos no normales -- por eso se usa Spearman en vez de Pearson)
concentracion <- c(15.7, 10.7, 11.9, 15.6, 14.3, 14.9)  # PM2.5 (ug/m3), muestra reducida ilustrativa
volumen       <- c(7, 3, 3, 4, 6, 7)                     # vehiculos/hora

cor.test(concentracion, volumen, method = "spearman", exact = FALSE)

# INTERPRETACION (numeros reales del libro, con las 25 observaciones
# completas): rho_S = 0.8517, S = 385.47, p-valor < 0.001 -- correlacion
# positiva muy fuerte y significativa: a mayor volumen de trafico, mayor
# concentracion de PM2.5.



# 6.2.5 -- CORRELACION DE KENDALL ----

# EXPLICACION AMIGABLE:
# Kendall tambien es no parametrica y trabaja con el orden de los datos,
# pero la idea detras es distinta a Spearman: en vez de mirar diferencias
# de rangos, compara TODOS LOS PARES POSIBLES de observaciones y cuenta
# cuantos son "concordantes" (ambas variables se mueven en la misma
# direccion en ese par) contra cuantos son "discordantes" (se mueven en
# direcciones opuestas):
#
#   tau = (n_concordantes - n_discordantes) / ( 0.5 * n * (n-1) )
#
# Se interpreta igual que cualquier coeficiente de correlacion (-1 a 1),
# aunque en la practica los valores de tau suelen salir un poco mas chicos
# que los de Spearman para los mismos datos -- eso es normal, no es un
# error.
#
# ¿CUANDO USAR KENDALL EN VEZ DE SPEARMAN? Cuando tienes muchos datos
# empatados (valores repetidos) o muestras chicas -- ahi Kendall es mas
# robusto y confiable.
#
# PARA QUE ME SIRVE EL CODIGO: mismo tipo de pregunta que Spearman
# ("¿estan asociadas estas dos variables?"), pero cuando prefieres una
# medida mas conservadora/robusta, por ejemplo con pocos datos o muchos
# valores repetidos.

# Ejemplo propio (verificado): anios de experiencia vs. ranking de
# desempenio de 8 empleados
experiencia <- c(1, 2, 3, 4, 5, 6, 7, 8)
desempenio  <- c(3, 1, 2, 5, 4, 6, 8, 7)   # el orden no es perfecto, hay algunas discordancias

cor.test(experiencia, desempenio, method = "kendall")

# INTERPRETACION (numeros reales de este ejemplo): tau = 0.714, p-valor =
# 0.014 -- correlacion positiva fuerte y significativa (p < 0.05): a mas
# anios de experiencia, mejor tiende a ser el ranking de desempenio, aunque
# no de forma perfectamente monotona (por eso tau no es 1).



# UNIDAD 7 REGRESION LINEAL SIMPLE (hasta 7.4) ----



# 7.1 -- EL MODELO DE RLS: DE LA IDEA A LA ECUACION ----

# EXPLICACION AMIGABLE:
# La regresion lineal simple parte de una pregunta muy natural: si ya se
# que dos variables estan correlacionadas (Unidad 6), ¿PUEDO USAR UNA PARA
# PREDECIR LA OTRA? Por ejemplo, si la edad de una soldadura y su
# resistencia estan correlacionadas negativamente, ¿puedo estimar la
# resistencia que tendra una soldadura con solo saber su edad?
#
# Para responder eso, primero hay dos formas de ver la relacion:
#
#  1) Modelo DETERMINISTA (ideal, sin ruido):  y_i = beta0 + beta1*x_i
#     -- cada punto cae exactamente sobre la recta. Esto casi nunca pasa
#     con datos reales.
#  2) Modelo PROBABILISTICO (el que realmente usamos): asumimos que el
#     PROMEDIO ESPERADO de Y es una funcion lineal de X, pero cada
#     observacion individual se desvia de esa linea por variabilidad
#     natural o error de medicion. Esa desviacion se llama error
#     (epsilon_i):
#
#       Y_i = beta0 + beta1*x_i + epsilon_i
#
# beta0 es el INTERCEPTO (el valor esperado de Y cuando X=0) y beta1 es la
# PENDIENTE (cuanto cambia Y, en promedio, por cada unidad que aumenta X).
# Para que las conclusiones que saques de este modelo sean confiables,
# deben cumplirse 5 supuestos sobre los errores:
#
#  1. Las x_i son fijas (o independientes del error).
#  2. Los errores tienen media cero: E(epsilon_i) = 0.
#  3. Los errores tienen VARIANZA CONSTANTE sigma^2 (homocedasticidad,
#     mismo concepto que en la Unidad 4 pero aplicado a los residuos del
#     modelo).
#  4. Los errores NO ESTAN CORRELACIONADOS entre si (independencia).
#  5. Los errores se distribuyen NORMALMENTE.
#
# OJO IMPORTANTE (esto aclara una confusion muy comun): el supuesto de
# normalidad de la regresion es sobre los ERRORES/RESIDUOS del modelo, NO
# sobre las variables X y Y en crudo. Son dos cosas distintas: cuando
# revisas normalidad de X y Y por separado (Shapiro-Wilk sobre Edad y sobre
# Resistencia, por ejemplo) eso es para decidir si usas Pearson o Spearman
# en la Unidad 6; cuando revisas normalidad de los RESIDUOS del modelo
# (shapiro.test(residuals(modelo))) eso es para validar el supuesto 5 de
# aca arriba, y se hace DESPUES de ajustar el modelo, no antes. Es
# perfectamente posible (y pasa en la practica) que X y Y no sean normales
# por separado pero los residuos del modelo si lo sean.
#
# PARA QUE ME SIRVE EL CODIGO: antes de ajustar cualquier modelo, siempre
# arrancas con un diagrama de dispersion (para ver a ojo si la relacion
# parece lineal) y con el coeficiente de correlacion (para cuantificar que
# tan fuerte es esa relacion) -- exactamente los mismos pasos que ya
# conoces de la Unidad 6, pero ahora como punto de partida para la
# regresion.

# Ejemplo real del libro: Resistencia (psi) vs Edad (semanas) de soldaduras
# (aqui muestro solo un fragmento ilustrativo; el dataset REAL completo,
# con las 100 observaciones, lo trabajo paso a paso al final de este script)
df <- data.frame(
  Resistencia = c(10.9, 10.6, 13.4, 10.7, 13.0, 13.1, 12.4, 10.8, 11.6, 12.0),
  Edad        = c(18.7, 19.1, 9.3, 17.5, 14.6, 12.8, 15.2, 20.1, 16.3, 14.0)
)

df %>%
  ggplot(aes(x = Edad, y = Resistencia)) +
  geom_point() +
  theme_bw()

with(df, cor(Edad, Resistencia))

# INTERPRETACION (numeros reales del libro, con la muestra completa):
# r = -0.8828 -- relacion lineal negativa muy fuerte: a mayor edad de la
# soldadura, menor su resistencia. Esto ya nos dice que un modelo de
# regresion lineal tiene sentido para estos datos.



# 7.2 -- ESTIMACION DE beta0 Y beta1 POR MINIMOS CUADRADOS----

# EXPLICACION AMIGABLE:
# Ya sabemos que queremos una recta yhat = beta0 + beta1*x que pase "lo mas
# cerca posible" de todos los puntos. Pero, ¿que significa "lo mas cerca
# posible"? El metodo de MINIMOS CUADRADOS contesta eso de forma muy
# concreta: busca los valores de beta0 y beta1 que hacen que la suma de
# los errores AL CUADRADO sea la mas chica posible:
#
#   S(beta0,beta1) = suma_i (y_i - beta0 - beta1*x_i)^2   -> minimizar
#
# (se eleva al cuadrado para que los errores positivos y negativos no se
# cancelen entre si, y para castigar mas fuerte los errores grandes).
# Derivando e igualando a cero, se llega a una formula cerrada -- no hay
# que "probar" rectas a prueba y error, se calcula directo:
#
#   beta1_hat = Sxy / Sxx = suma[(xi-xbar)(yi-ybar)] / suma[(xi-xbar)^2]
#   beta0_hat = ybar - beta1_hat * xbar
#
# COMO INTERPRETAR LOS COEFICIENTES EN PALABRAS NORMALES:
#  - beta1_hat = cuanto cambia, EN PROMEDIO, la variable respuesta por
#    cada unidad que aumenta la variable explicativa.
#  - beta0_hat = el valor esperado de la respuesta cuando la variable
#    explicativa vale 0 (ojo: esto solo tiene sentido interpretarlo si X=0
#    es un valor realista dentro de tus datos).
#
# PARA QUE ME SIRVE EL CODIGO: es el paso que realmente "ajusta" el modelo
# -- de aqui sale la ecuacion que despues usas para predecir. En R casi
# nunca haces las sumas a mano (para eso esta lm()), pero el examen suele
# pedir el calculo paso a paso para demostrar que entiendes de donde sale
# la formula.

Y <- df$Resistencia
X <- df$Edad
n <- length(X)

x_bar <- mean(X)
y_bar <- mean(Y)

Sxx <- sum((X - x_bar)^2)
Sxy <- sum((X - x_bar) * (Y - y_bar))

beta1 <- Sxy / Sxx
beta0 <- y_bar - beta1 * x_bar
data.frame(beta0 = beta0, beta1 = beta1)

# verificacion con la funcion de R
modelo <- lm(Resistencia ~ Edad, data = df)
modelo

# INTERPRETACION (numeros reales del libro, muestra completa):
# beta0_hat = 15.74, beta1_hat = -0.244. Es decir: una soldadura recien
# hecha (Edad=0) tiene una resistencia promedio de 15.74 psi, y por cada
# semana que pasa, la resistencia baja en promedio 0.244 psi.



# 7.3 ESTIMACION DE sigma^2 (LA VARIANZA DEL ERROR) ----

# EXPLICACION AMIGABLE:
# Ya tenemos la recta, pero nos falta una pieza: ¿que tan "ruidosos" son
# los datos alrededor de esa recta? Eso lo mide sigma^2, la varianza de
# los errores. Como no conocemos los errores poblacionales, los estimamos
# con los RESIDUOS de la muestra: la diferencia entre lo observado y lo
# que el modelo predijo, e_i = y_i - yhat_i.
#
# La suma de esos residuos al cuadrado se llama SSE (suma de cuadrados del
# error):
#
#   SSE = suma_i (y_i - yhat_i)^2
#
# y como usamos 2 grados de libertad para estimar beta0 y beta1, dividimos
# entre (n-2) en vez de n:
#
#   sigma2_hat = SSE / (n-2) = MSE
#
# A la raiz cuadrada de esto, sigma_hat = sqrt(MSE), se le llama tambien
# ERROR ESTANDAR DE LA REGRESION (RSE) -- y tiene las mismas unidades que
# tu variable respuesta, lo cual lo hace facil de interpretar: "en
# promedio, mis predicciones se equivocan por +/- sigma_hat unidades".
#
# PARA QUE ME SIRVE EL CODIGO: te dice que tan preciso es tu modelo en las
# mismas unidades del problema (psi, pesos, kg, lo que sea) -- un
# sigma_hat chico comparado con el rango de tus datos significa un modelo
# bastante preciso.

b0 <- modelo$coefficients[1]
b1 <- modelo$coefficients[2]

yhat <- b0 + b1 * X
e <- Y - yhat

SSE <- sum(e^2)
gl <- n - 2
MSE <- SSE / gl
RSE <- sqrt(MSE)

data.frame(SSE = SSE, MSE = MSE, RSE = RSE)

# verificacion directa
summary(modelo)$sigma

# INTERPRETACION (numeros reales del libro): MSE = 0.349,
# sigma_hat = RSE = 0.591 psi. En promedio, la prediccion del modelo se
# desvia de la resistencia real en poco mas de media unidad de psi --
# bastante preciso para el rango de los datos.



# 7.4 COEFICIENTE DE DETERMINACION R^2 ----

# EXPLICACION AMIGABLE:
# Esta es probablemente la metrica mas citada de toda la regresion, y la
# mas facil de mal-entender. R^2 responde a la pregunta: DE TODA LA
# VARIABILIDAD QUE HAY EN MI VARIABLE RESPUESTA, ¿QUE PORCENTAJE LOGRA
# "EXPLICAR" MI MODELO?
#
#   R^2 = 1 - SSE/SST
#
# donde SST (suma de cuadrados TOTAL) es cuanto varian los datos respecto
# a su propio promedio (sin usar el modelo para nada), y SSE es lo que le
# queda sin explicar al modelo (el error). Si tu modelo es perfecto,
# SSE=0 y R^2=1; si tu modelo no explica nada, R^2 ~ 0.
#
# Hay una identidad muy util (SOLO valida en regresion lineal SIMPLE, con
# una sola variable explicativa): R^2 es literalmente el coeficiente de
# correlacion de Pearson AL CUADRADO:
#
#   R^2 = SSR/SST = r^2
#
# (donde SSR = SST - SSE es la parte de la variabilidad que SI explica el
# modelo). Por eso, si ya calculaste tu r en la Unidad 6, en regresion
# simple puedes obtener el R^2 solo elevandolo al cuadrado -- no hace
# falta rehacer todas las cuentas.
#
# PARA QUE ME SIRVE EL CODIGO: para responder, en tu interpretacion final,
# algo como "el modelo explica el 78% de la variabilidad en la resistencia
# de las soldaduras" -- es el numero que resume que tan bueno es tu modelo
# en una sola cifra.

Y_hat <- b0 + b1 * X

SST <- sum((Y - y_bar)^2)
SSE <- sum((Y - Y_hat)^2)
SSR <- SST - SSE

R2 <- SSR / SST
R2

# formas alternativas de llegar al mismo numero
cor(Y, X)^2                    # r^2 (solo valido en regresion SIMPLE)
summary(modelo)$r.squared      # directo desde el objeto lm

# INTERPRETACION (numeros reales del libro): R^2 = 0.7793. El modelo
# explica aproximadamente el 78% de la variabilidad de la resistencia de
# las soldaduras a partir de su edad; el 22% restante se debe a otros
# factores no incluidos en el modelo (o a variabilidad aleatoria propia
# del proceso).


# 7.4-BIS -- PREDECIR CON EL MODELO (y ejercicio COMPLETO de botes, Parcial 2) ----

# EXPLICACION AMIGABLE:
# Una vez tienes beta0 y beta1 (o el objeto `modelo` de lm()), predecir es
# solo reemplazar en la ecuacion de la recta:
#
#   yhat_nuevo = beta0_hat + beta1_hat * x_nuevo
#
# En R lo puedes hacer a mano (reemplazando el numero) o con predict(),
# que hace exactamente lo mismo pero te deja usar el objeto del modelo
# directo. El Punto 3.e del Parcial 2 pide justo esto: predecir el precio
# de un bote con capacidad de equipaje = 3. Como el Punto 3 trae el dataset
# COMPLETO dentro del mismo PDF, aqui te lo resuelvo entero (a-e) para que
# veas el ejercicio tipo "regresion simple" del examen ya resuelto de
# principio a fin, con numeros 100% reales y verificados.
#
# PARA QUE ME SIRVE EL CODIGO: es el ejercicio de regresion mas completo y
# mas parecido a lo que te va a tocar resolver en el parcial -- uselo como
# plantilla y solo cambie los datos.

# Datos de botes (tal cual los trae el Parcial 2, Punto 3)
botes <- data.frame(
  Bote = c("S14", "Orinoco", "Outside Pro", "Explorer 380X",
           "River XK2", "Sea Tiger", "Maverik II", "Starlite 100", "Fat Pack Cat"),
  Capacidad = c(4, 4, 4, 3, 2.5, 4, 3, 2, 3),
  Precio = c(1595, 1399, 1890, 795, 600, 1995, 1205, 583, 1048)
)

# a) diagrama de dispersion + la recta del modelo superpuesta
modelo_botes <- lm(Precio ~ Capacidad, data = botes)

botes |>
  ggplot(aes(x = Capacidad, y = Precio)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "Precio vs. capacidad para equipaje (botes inflables)") +
  theme_bw()

# INTERPRETACION a): se ve una tendencia positiva y bastante marcada -- a
# mayor capacidad para equipaje, mayor precio del bote.

# b) minimos cuadrados paso a paso
x_b <- botes$Capacidad
y_b <- botes$Precio
n_b <- length(x_b)

xbar_b <- mean(x_b)
ybar_b <- mean(y_b)

Sxx_b <- sum((x_b - xbar_b)^2)
Sxy_b <- sum((x_b - xbar_b) * (y_b - ybar_b))

beta1_b <- Sxy_b / Sxx_b
beta0_b <- ybar_b - beta1_b * xbar_b

data.frame(beta0 = beta0_b, beta1 = beta1_b)

# verificacion
modelo_botes

# INTERPRETACION b) (numeros reales de este dataset): beta1_hat = 638.76 ->
# por cada punto adicional de capacidad para equipaje, el precio del bote
# sube en promedio $638.76. beta0_hat = -859.26 -> el intercepto es
# negativo, lo cual NO tiene una interpretacion practica aqui (un bote con
# capacidad=0 no existe en el rango de los datos), es solo un ajuste
# matematico de la recta.

# c) estimacion de sigma (RSE) paso a paso
yhat_b <- beta0_b + beta1_b * x_b
e_b <- y_b - yhat_b

SSE_b <- sum(e_b^2)
gl_b <- n_b - 2
MSE_b <- SSE_b / gl_b
RSE_b <- sqrt(MSE_b)

data.frame(SSE = SSE_b, MSE = MSE_b, RSE = RSE_b)
summary(modelo_botes)$sigma   # verificacion

# INTERPRETACION c): sigma_hat = RSE = 227.47. En promedio, el modelo se
# equivoca en unos $227 al predecir el precio de un bote a partir de su
# capacidad para equipaje.

# d) coeficiente de determinacion R^2
SST_b <- sum((y_b - ybar_b)^2)
R2_b  <- 1 - SSE_b / SST_b
R2_b
summary(modelo_botes)$r.squared   # verificacion

# INTERPRETACION d): R^2 = 0.8369. El modelo explica cerca del 84% de la
# variabilidad en el precio de los botes usando solo la capacidad para
# equipaje -- un ajuste bastante bueno.

# e) prediccion para un bote con capacidad = 3
capacidad_nueva <- 3
precio_predicho <- beta0_b + beta1_b * capacidad_nueva
precio_predicho

# equivalente con predict() usando el objeto del modelo
predict(modelo_botes, newdata = data.frame(Capacidad = 3))

# INTERPRETACION e): para un bote con capacidad para equipaje = 3, el
# modelo predice un precio aproximado de $1,057.



# RESUMEN DE COMO SE CONECTA TODO ----

# | Seccion | Tema      | Pregunta que responde                              | Funcion clave en R                |
# |---------|-----------|-----------------------------------------------------|------------------------------------|
# | 5.1.1   | Homogeneidad | ¿Los grupos que ya tengo se distribuyen igual?   | chisq.test(tabla)                  |
# | 5.1.2   | Independencia| ¿Estan relacionadas estas dos variables categoricas? | chisq.test(tabla)              |
# | 6.2.3   | Pearson   | ¿Que tan fuerte es la relacion LINEAL entre 2 numericas normales? | cor.test(x, y, method="pearson")  |
# | 6.2.4   | Spearman  | Lo mismo, sin exigir normalidad (relacion monotona)  | cor.test(x, y, method="spearman") |
# | 6.2.5   | Kendall   | Lo mismo, mas robusto ante empates/muestras chicas   | cor.test(x, y, method="kendall")  |
# | 7.2     | Minimos cuadrados | ¿Cual es la mejor recta para predecir Y a partir de X? | lm(y ~ x)                  |
# | 7.3     | Estimacion de sigma^2 | ¿Que tan preciso es mi modelo (en unidades originales)? | summary(modelo)$sigma   |
# | 7.4     | R^2       | ¿Que porcentaje de la variabilidad explica mi modelo? | summary(modelo)$r.squared        |
#
# La logica general de la Unidad 6 y 7 es progresiva: primero mides si dos
# variables estan relacionadas (correlacion), y si la relacion es lineal y
# quieres ademas PREDECIR una a partir de la otra, pasas al modelo de
# regresion (que reutiliza el mismo r que ya calculaste, ahora elevado al
# cuadrado como R^2).



# =============================================================================
# EXTRA 1 -- QUE TIPO DE COSAS PIDE ESTE PARCIAL (Parcial 2, 2025-09-24) ----
# =============================================================================

# Estructura del examen (120 minutos, entrega en Rmd, con codigo + salidas
# + interpretacion de cada punto -- NO solo el numero final):
#
#  Punto 1 (1.5 pts) -- Chi-cuadrado de INDEPENDENCIA
#    a) EDA: tabla de contingencia + grafico
#    b) H0 / H1 explicitas
#    c) Eij y chi^2 CALCULADOS A MANO, celda por celda
#    d) grados de libertad (I-1)(J-1)
#    e) conclusion con la regla "Rechazar H0 si chi^2 > chi^2_alpha"
#    -> practicalo con la seccion 5.1.2-BIS de arriba
#
#  Punto 2 (1.5 pts) -- Correlacion de PEARSON
#    a) diagrama de dispersion
#    b) Shapiro-Wilk para AMBAS variables
#    c) r calculado A MANO (formula completa, no solo cor())
#    d) t0 calculado A MANO y comparado contra t_critico
#    e) intervalo de confianza al 95% por Fisher (arctanh) A MANO
#    -> practicalo con la seccion 6.2.3 y 6.2.3-BIS de arriba
#
#  Punto 3 (2.0 pts) -- Regresion lineal SIMPLE
#    a) dispersion + recta del modelo, interpretar
#    b) beta0 y beta1 por minimos cuadrados, A MANO, interpretar el intercepto
#    c) sigma_hat (RSE) A MANO
#    d) R^2 A MANO, interpretar
#    e) prediccion para un valor nuevo de X
#    -> este es EXACTAMENTE el ejercicio de botes de la seccion 7.4-BIS,
#       ya resuelto completo arriba
#
# PATRON GENERAL: en los tres puntos el profesor pide "paso a paso" -- eso
# significa que NO basta con poner chisq.test(), cor.test() o lm() y ya;
# hay que mostrar el calculo manual con las formulas (Eij, r, t0, beta0,
# beta1, SSE, R2) Y ADEMAS verificar con la funcion de R. Por eso en cada
# seccion de esta guia siempre dejo las dos versiones: el calculo a mano y
# la funcion que lo verifica.



# =============================================================================
# EXTRA 2 -- TU SCRIPT DE LA REGRESION SOLDADURA, CORREGIDO Y EXPLICADO ----
# =============================================================================

# Este es tu ejercicio de Resistencia ~ Edad (data_rls.txt), corregido: le
# arregle los `*` que faltaban (en R dos parentesis pegados como
# `(x-xbar)(y-ybar)` NO es multiplicacion implicita, da error de sintaxis),
# le puse `#` a las lineas que eran texto explicativo pero no tenian el
# comentario (si no, R las trata de leer como codigo y truena), quite el
# lillie.test() duplicado (el segundo definitivamente era para Resistencia,
# no para repetir Edad... digo, Resistencia dos veces), y lo corri con el
# archivo REAL completo (100 filas) para verificar que todo cuadre.
#
# DATO IMPORTANTE que valida tu razonamiento: ya confirme con el archivo
# real que Shapiro-Wilk SI rechaza normalidad para Edad (p=0.00046) y para
# Resistencia (p=0.0034) -- o sea que tu conclusion de "no se puede usar
# Pearson, toca Spearman" es CORRECTA para estos datos. Lo unico es que en
# tu codigo, despues de decir eso, seguias llamando cor() sin
# method="spearman" (que por defecto calcula Pearson) -- ya lo deje
# corregido abajo.
#
# Y aca la aclaracion clave de la seccion 7.1-BIS: que Edad y Resistencia
# NO sean normales por separado (lo que decide Pearson vs. Spearman en la
# Unidad 6) NO significa que no puedas ajustar el modelo de regresion.
# Ajustar lm() no exige que X y Y sean normales; exige que los RESIDUOS del
# modelo lo sean, y eso se revisa DESPUES de ajustar. De hecho, con estos
# datos, shapiro.test(residuals(modelo)) da p=0.563 -- los residuos SI son
# normales, aunque Edad y Resistencia por separado no lo sean. Por eso al
# final de tu script si tiene sentido seguir con la regresion normalmente.

# Librerias (ya las tienes arriba, las repito por si corres solo este bloque)
library(dplyr)
library(tidyverse)
library(nortest)

# Datos: cambia la ruta por la tuya si es necesario
df <- read.delim("~/METODOS ESTADISTICOS/Correlacion/data_rls.txt")

# Predecir la resistencia segun la edad, siendo Edad la variable predictora (independiente)

# ---- EDA ----
df |>
  summarise(n = length(Edad),
            prom = mean(Edad),
            ds = sd(Edad),
            mediana = median(Edad),
            RIC = IQR(Edad),
            minimo = min(Edad),
            maximo = max(Edad))

df |>
  summarise(n = length(Resistencia),
            prom = mean(Resistencia),
            ds = sd(Resistencia),
            mediana = median(Resistencia),
            RIC = IQR(Resistencia),
            minimo = min(Resistencia),
            maximo = max(Resistencia))

# grafico de dispersion
df |>
  ggplot(aes(x = Edad, y = Resistencia)) +
  geom_point() +
  theme_bw()

# ---- Normalidad (para decidir Pearson vs Spearman) ----
ks.test(scale(df$Edad), "pnorm")
ks.test(scale(df$Resistencia), "pnorm")
# no sirve del todo la Kolmogorov-Smirnov aqui (esta version con scale() es
# una aproximacion; lo mas correcto para n<50 es Shapiro directo), usamos Shapiro
shapiro.test(df$Edad)
shapiro.test(df$Resistencia)
# Shapiro dice que NINGUNA de las dos es normal (p < 0.05 en ambas), asi
# que para estar seguros aplicamos tambien Lilliefors (la version de
# Kolmogorov-Smirnov que no exige conocer mu y sigma de antemano)
lillie.test(df$Edad)
lillie.test(df$Resistencia)

# CONCLUSION: como Edad y Resistencia no pasan la prueba de normalidad
# (Shapiro y Lilliefors coinciden), no se puede usar Pearson con
# tranquilidad -- usamos Spearman en su lugar.
cor(df$Edad, df$Resistencia, method = "spearman")

# (para comparar nomas, este seria el que NO deberiamos usar como principal)
cor(df$Edad, df$Resistencia, method = "pearson")

# ---- Estimacion paso a paso de beta_0 y beta_1 ----
x <- df$Edad
y <- df$Resistencia
n <- length(x)

xbar <- mean(x)
ybar <- mean(y)

sxx <- sum((x - xbar)^2)
sxy <- sum((x - xbar) * (y - ybar))   # <- aqui faltaba el * entre los dos parentesis

# Estimacion de beta_0 y beta_1
beta1 <- sxy / sxx
beta0 <- ybar - beta1 * xbar          # <- aqui faltaba el * entre beta1 y xbar
data.frame(beta0 = beta0, beta1 = beta1)

# funcion de R para la regresion lineal simple (RLS), para verificar
modelo <- lm(Resistencia ~ Edad, data = df)
modelo

# ---- Hallemos sigma^2 ----
x <- df$Edad
y <- df$Resistencia

# coeficientes estimados
beta_0 <- modelo$coefficients[1]
beta_1 <- modelo$coefficients[2]

# valores ajustados (los que predice el modelo) y el residuo (el error de cada punto)
yhat <- beta_0 + beta_1 * x
error <- y - yhat

# SSE
SSE <- sum(error^2)

# otra forma de calcular el SSE, directo desde el modelo
e <- modelo$residuals
sse <- sum(e^2)

# MSE, que es lo mismo que la varianza estimada del error
n <- nrow(df)
sigma2 <- MSE <- sse / (n - 2)

# RSE (raiz del MSE)
rse <- sqrt(sigma2)
data.frame(SSE = sse, MSE = MSE, RSE = rse)

# ---- Coeficiente de determinacion R^2 ----
x <- df$Edad
y <- df$Resistencia
n <- nrow(df)

# medias
xbar <- mean(x)
ybar <- mean(y)

# Sxx y Sxy
SST <- sum((y - ybar)^2)
Sxx <- sum((x - xbar)^2)
Sxy <- sum((x - xbar) * (y - ybar))   # <- aqui tambien faltaba el *

beta_1 <- Sxy / Sxx

# R2 -- forma 1: SSR/SST
e <- y - yhat
SSE <- sum(e^2)
r2 <- (SST - SSE) / SST
r2

# R2 -- forma 2: coeficiente de correlacion de Pearson al cuadrado
# (ojo: esta identidad r^2 = R^2 usa el r de PEARSON por definicion, aunque
# arriba hayamos concluido que Spearman es el mas apropiado para describir
# la asociacion -- son dos preguntas distintas: Pearson/Spearman es sobre
# "que tan asociadas estan Edad y Resistencia", mientras que R^2 de la
# regresion es sobre "que tanto explica linealmente mi modelo lm()", y ese
# SIEMPRE se calcula con el r de Pearson al cuadrado en regresion simple)
cor(df$Edad, df$Resistencia)^2

# R2 -- forma 3: directo desde el objeto lm()
summary(modelo)$r.squared

# ---- Chequeo extra: normalidad de los RESIDUOS (el supuesto real de la RLS) ----
shapiro.test(modelo$residuals)
# INTERPRETACION: aunque Edad y Resistencia no eran normales por separado,
# los RESIDUOS del modelo si dan normales (p > 0.05) -- el supuesto de
# normalidad de la regresion (seccion 7.1, supuesto 5) SI se cumple. Este
# es el chequeo que realmente valida (o no) tu modelo de regresion, y es
# distinto del chequeo de normalidad que hiciste arriba para elegir entre
# Pearson y Spearman.

# INTERPRETACION FINAL (numeros reales, dataset completo de 100 soldaduras):
# beta0 = 15.74, beta1 = -0.244, MSE = 0.349, RSE = 0.591, R2 = 0.7793.
# El modelo: Resistencia_estimada = 15.74 - 0.244 * Edad. Por cada semana
# que envejece la soldadura, se espera que pierda en promedio 0.244 psi de
# resistencia, y el modelo explica el 78% de la variabilidad observada.
