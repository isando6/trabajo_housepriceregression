# Modelos de regresión lineal simple y múltiple para la predicción del precio de vivienda: conjunto "House Price Regression"

**Autoras:** Karina Núñez Sarmiento y Isabella Sandoval Escorcia
**Asignatura:** Métodos Estadísticos

**Libro en línea:** [isando6.github.io/trabajo_housepriceregression](https://isando6.github.io/trabajo_housepriceregression/)

---

## Presentación

Este repositorio contiene el código fuente del libro en el que se desarrolla un análisis de regresión lineal orientado a explicar y predecir el precio de venta de viviendas residenciales. El punto de partida es una pregunta que suele darse por resuelta en el sentido común inmobiliario: si el tamaño del terreno determina el valor de una propiedad y si ese efecto depende de la calidad del vecindario en el que se ubica. A partir de ella, el trabajo avanza desde un modelo simple, con el tamaño del terreno como único predictor, hasta un modelo múltiple que incorpora las características estructurales y temporales de la vivienda.

## Enfoque teórico

El análisis se apoya en la teoría de los precios hedónicos (Rosen, 1974), según la cual la vivienda se transa como un conjunto de atributos estructurales, temporales y de entorno, cada uno con un precio implícito, de modo que el precio final resulta de la suma de esas valoraciones marginales. Desde el modelo clásico del uso del suelo (Alonso, 1964; DiPasquale y Wheaton, 1996), se plantea además que el valor marginal del terreno no es homogéneo, sino que tiende a ser mayor en entornos más atractivos. De ahí se deriva la hipótesis central del trabajo: la capacidad predictiva del tamaño del terreno podría variar según la calidad del vecindario. La regresión lineal (Montgomery, Peck y Vining, 2012) ofrece el marco para estimar estos efectos manteniendo constantes las demás condiciones y para distinguir, mediante pruebas de significancia, un efecto real de una variación debida al azar.

## Fuente de datos

Se utiliza el *House Price Regression Dataset* (Kaggle, s.f.), un conjunto de acceso público con 1.000 observaciones y 8 variables: superficie construida, número de habitaciones, número de baños, año de construcción, tamaño del terreno, capacidad del garaje, calidad del vecindario y precio de venta.

## Principales hallazgos

Los resultados matizan varias de las ideas de partida. En el modelo simple, cada acre adicional de terreno se asocia con un aumento promedio cercano a USD 31.300 en el precio; sin embargo, cuando se comparan viviendas con la misma superficie, número de habitaciones y baños, año de construcción y garaje, ese efecto se reduce a unos USD 14.900, lo que indica que en el modelo simple el terreno recogía parte del efecto de otras variables. La diferencia de pendientes entre vecindarios, que se observa al estimar el modelo por separado en cada grupo, desaparece en el modelo múltiple, y la calidad del vecindario no resulta significativa, por lo que los datos no respaldan la hipótesis planteada en el marco teórico.

El contraste entre ambos modelos es igualmente revelador. Con el terreno como único predictor, el intervalo de predicción para una vivienda individual alcanza un ancho de casi USD 1 millón, prácticamente todo el rango de precios observado; con el modelo múltiple, ese ancho se reduce a unos USD 38.500, es decir, unas 25 veces menos. El modelo final, con seis predictoras, explica el 99,85 % de la variabilidad del precio y cumple los supuestos sobre los errores, incluida la normalidad, que no se satisfacía en el modelo simple.

## Estructura del libro

El documento se organiza en doce capítulos que siguen el orden natural de un análisis estadístico:

1. Introducción
2. Extracción, transformación y carga de datos (ETL)
3. Análisis de la variable objetivo `House_Price`
4. Análisis de las variables características (independientes)
5. Análisis exploratorio bivariado
6. Correlación entre variables independientes
7. Modelo de regresión lineal con `Lot_Size` como predictor de `House_Price`
8. Comparación de la relación `Lot_Size`–`House_Price` según la calidad del vecindario
9. Validación de supuestos y predicción con el modelo de regresión lineal simple
10. Modelo de regresión lineal múltiple
11. Conclusiones
12. Referencias

Al inicio del libro se incluye un video de presentación que recorre el trabajo de principio a fin y ofrece una visión general antes de la lectura detallada de cada capítulo.

## Herramientas

El análisis se desarrolló en **R**, y el libro se construyó con **bookdown** (formato GitBook) y se publicó mediante **GitHub Pages**. Para generarlo localmente basta con ejecutar:

```r
install.packages("bookdown")
bookdown::render_book("index.Rmd")
```

## Consideraciones sobre los datos

El ajuste casi perfecto del modelo múltiple es poco habitual en datos reales del mercado inmobiliario. Junto con las distribuciones casi uniformes de las variables y la ausencia de correlación entre predictoras, este resultado sugiere que el conjunto fue generado de forma sintética. Por ello, el trabajo resulta útil para ilustrar el proceso completo de modelación, pero sus conclusiones deben trasladarse con cautela a mercados reales, donde la ubicación y la calidad del entorno suelen tener un peso considerablemente mayor.
