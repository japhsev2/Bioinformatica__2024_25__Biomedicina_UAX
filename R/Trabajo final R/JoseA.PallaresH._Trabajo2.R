#################################################
#################################################
#####							  #####
#####		   José A. Pallarés H. 		  #####
#####							  #####
#####			 Trabajo2.R			  #####
#####			 				  #####
#################################################
#################################################



### 1 ### Carga los datos y examínalos en R. Emplea las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos?
# Leer los datos (suponiendo valores separados por tabuladores)
datos <- read.table("datos-trabajoR.txt", header=TRUE, sep="\t")

# Examen de datos
# Mostrar las primeras filas
cat("Primeras filas del conjunto de datos:\n")
print(head(datos))

# Mostrar las últimas filas
cat("Últimas filas del conjunto de datos:\n")
print(tail(datos))

# Mostrar estadísticas resumidas
cat("Resumen estadístico:\n")
print(summary(datos))

# Dimensiones de la pantalla
cat("Dimensiones del conjunto de datos:\n")
print(dim(datos))

# Número de tratamientos únicos
cat("Número de tratamientos únicos:\n")
length(unique(datos$Tratamiento))

cat("Número de observaciones por tratamiento:\n")
table(datos$Tratamiento)

# Mostrar estructura
cat("Estructura del conjunto de datos:\n")
str(datos)



### 2 ### Haz un boxplot para nuestros datos. Uno para cada condición. Elige un color para cada condición y guárdalo para las siguientes gráficas.
# Crear boxplots con colores personalizados
# Definir colores para cada condición
colors <- c("darkblue", "darkred", "darkgreen")

# Create boxplots
par(mfrow=c(1,3))  # Arrange plots in 1 row, 3 columns

# Boxplot for Wildtype
boxplot(datos$Wildtype, 
        main="Wildtype", 
        col=colors[1], 
        ylab="Value")

# Boxplot for Sequia
boxplot(datos$Sequia, 
        main="Sequia", 
        col=colors[2], 
        ylab="Value")

# Boxplot for ExcesoRiego
boxplot(datos$ExcesoRiego, 
        main="Exceso Riego", 
        col=colors[3], 
        ylab="Value")

# Reset the plotting parameters
par(mfrow=c(1,1))


###*COMO NO ENTENDÍ SI EL ENUNCIADO LO PEDÍA ERA QUE GRAFICARA CADA CONDICIÓN POR SEPARADA Y ADEMAS, DIFERENCIARA EL TRATAMIENTO CON COLOR DIFERENTE Y BOXPLOT PARA PODER INDENTIFICA CADA UNO, HE HECHO LAS SIGUIENTES GRÁFICAS SI ESO ES LO QUE VERDADERAMENTE PEDÍA:
# Definir colores para cada tratamiento
colores <- c("black", "red", "green", "lightblue", "blue", "pink")
nombres_tratamientos <- c("Tto. 1", "Tto. 2", "Tto. 3", "Tto. 4", "Tto. 5", "Tto. 6")

# Configurar layout para gráficos y leyenda
layout(matrix(c(1, 2, 3, 4), nrow = 1, byrow = TRUE), widths = c(3, 3, 3, 1)) # Tres gráficos + espacio para leyenda

# Ajustar márgenes para los gráficos
par(mar = c(5, 4, 4, 2))  # Márgenes para los boxplots

# Crear boxplots
boxplot(datos$Wildtype ~ datos$Tratamiento, 
        col = colores,
        main = "Wildtype por Tratamiento",
        xlab = "Tratamiento",
        ylab = "Valor")

boxplot(datos$Sequia ~ datos$Tratamiento,
        col = colores,
        main = "Sequía por Tratamiento",
        xlab = "Tratamiento",
        ylab = "Valor")

boxplot(datos$ExcesoRiego ~ datos$Tratamiento,
        col = colores,
        main = "Exceso Riego por Tratamiento",
        xlab = "Tratamiento",
        ylab = "Valor")

# Crear leyenda en el último panel
par(mar = c(0, 0, 0, 0))  # Márgenes reducidos para la leyenda
plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")  # Crear un espacio vacío para la leyenda
legend("center", legend = nombres_tratamientos, fill = colores, bty = "n", cex = 1.2)  # Dibujar la leyenda



### 3 ### Haz dos gráficos de dispersión. El primero debe comparar Sequía con Wildtype, y el segundo ExcesoRiego con Wildtype. Cada tratamiento debe de ir de un color distinto. Pista: usa col=datos$Tratamiento.
# Restaurar configuración gráfica antes de los gráficos de dispersión
par(mfrow = c(1, 1))  # Restaurar layout estándar
par(mar = c(5, 4, 4, 2) + 0.1)  # Restaurar márgenes estándar

# Sequía vs Wildtype
plot(datos$Wildtype, datos$Sequia,
     col=colores[datos$Tratamiento],
     pch=16,
     main="Sequía vs Wildtype",
     xlab="Wildtype",
     ylab="Sequía")

# Exceso Riego vs Wildtype
plot(datos$Wildtype, datos$ExcesoRiego,
     col=colores[datos$Tratamiento],
     pch=16,
     main="Exceso Riego vs Wildtype",
     xlab="Wildtype",
     ylab="Exceso Riego")




### 4 ### Ponle leyenda al gráfico del apartado anterior. En el margen inferior derecho. Pista: investiga sobre legend().
# Definir los colores para los tratamientos
colores <- c("black", "red", "green", "lightblue", "blue", "pink")

# Gráfico de dispersión Sequía vs Wildtype con leyenda en la esquina inferior derecha
plot(datos$Wildtype, datos$Sequia,
     col=colores[datos$Tratamiento],
     pch=1,  # Usar círculos vacíos como en la imagen
     main = "Sequía vs Wildtype",
     xlab="Wildtype",
     ylab="Sequia")

# Añadimos la leyenda con las mismas especificaciones
legend("bottomright",                         # Posición de la leyenda 
       legend = paste("Tto", 1:6),           # Etiquetas de "Tto. 1" a "Tto. 6"
       fill = c("black", "red", "green", "lightblue", "blue", "pink"),   # Colores de fondo
       border = "black",                      # Bordes de los rectángulos
       cex = 1.2,                            # Tamaño del texto
       bty = "o")                            # Tipo de caja (o = caja)

# Gráfico de dispersión ExcesoRiego vs Wildtype (manteniendo el mismo formato)
plot(datos$Wildtype, datos$ExcesoRiego,
     col=colores[datos$Tratamiento],
     pch=1,
     main = "Exceso de Riego vs Wildtype",
     xlab="Wildtype",
     ylab="ExcesoRiego")

# Añadimos la leyenda con las mismas especificaciones
legend("bottomright",                         # Posición de la leyenda 
       legend = paste("Tto", 1:6),           # Etiquetas de "Tto. 1" a "Tto. 6"
       fill = c("black", "red", "green", "lightblue", "blue", "pink"),   # Colores de fondo
       border = "black",                      # Bordes de los rectángulos
       cex = 1.2,                            # Tamaño del texto
       bty = "o")                            # Tipo de caja (o = caja)



### 5 ### Haz un histograma para cada variable. Recuerda mantener los colores.
# Configuramos el layout para mostrar los tres histogramas en una fila
par(mfrow = c(1, 3))

# Histograma para Wildtype
hist(datos$Wildtype,
     main = "Histograma de Wildtype",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "forestgreen",    # Mantenemos el color verde forestal usado anteriormente
     border = "black",
     breaks = 15)           # Número de intervalos

# Histograma para Sequía
hist(datos$Sequia,
     main = "Histograma de Sequía",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "red",        # Mantenemos el color naranja usado anteriormente
     border = "black",
     breaks = 15)

# Histograma para Exceso de Riego
hist(datos$ExcesoRiego,
     main = "Histograma de Exceso de Riego",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "dodgerblue",    # Mantenemos el color azul usado anteriormente
     border = "black",
     breaks = 15)

# Restauramos la configuración original de los gráficos
par(mfrow = c(1, 1))


######*ALIGUAL QUE EL EJERCICIO Nº 2, COMO NO ENTENDÍ SI EL ENUNCIADO LO PEDÍA ERA QUE GRAFICARA CADA CONDICIÓN POR SEPARADA Y ADEMAS, DIFERENCIARA EL TRATAMIENTO CON COLOR DIFERENTE EN EL HISTOGRAMA PARA PODER INDENTIFICA CADA UNO, SI ESO ES LO QUE VERDADERAMENTE PEDÍA, HE HECHO LAS SIGUIENTES GRÁFICAS:
# Instalar y cargar las librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(ggplot2)
library(patchwork)

# Cargar datos
data <- read.table(file = "datos-trabajoR.txt", header = TRUE)

# Definir la paleta de colores
colores_tratamientos <- c("1" = "black", 
                          "2" = "red", 
                          "3" = "green", 
                          "4" = "lightblue", 
                          "5" = "blue", 
                          "6" = "pink")

# Crear histograma para Wildtype
hist_wildtype <- ggplot(data, aes(x = Wildtype, fill = factor(Tratamiento), color = factor(Tratamiento))) +
  geom_histogram(position = "dodge", linewidth = 0.5) +  # Cambiar size por linewidth
  scale_fill_manual(values = colores_tratamientos, labels = paste("Tto.", 1:6)) +
  scale_color_manual(values = colores_tratamientos, guide = "none") +  # Ocultar leyenda de color
  labs(title = "Histograma de Wildtype", 
       x = "Valor", 
       y = "Frecuencia", 
       fill = "Tratamientos") +
  theme_minimal() +
  theme(legend.position = "none") # Ocultar la leyenda individual

# Crear histograma para Sequía
hist_sequia <- ggplot(data, aes(x = Sequia, fill = factor(Tratamiento), color = factor(Tratamiento))) +
  geom_histogram(position = "dodge", linewidth = 0.5) +  # Cambiar size por linewidth
  scale_fill_manual(values = colores_tratamientos, labels = paste("Tto.", 1:6)) +
  scale_color_manual(values = colores_tratamientos, guide = "none") +  # Ocultar leyenda de color
  labs(title = "Histograma de Sequía", 
       x = "Valor", 
       y = "Frecuencia", 
       fill = "Tratamientos") +
  theme_minimal() +
  theme(legend.position = "none") # Ocultar la leyenda individual

# Crear histograma para ExcesoRiego
hist_excesoriego <- ggplot(data, aes(x = ExcesoRiego, fill = factor(Tratamiento), color = factor(Tratamiento))) +
  geom_histogram(position = "dodge", linewidth = 0.5) +  # Cambiar size por linewidth
  scale_fill_manual(values = colores_tratamientos, labels = paste("Tto.", 1:6)) +
  scale_color_manual(values = colores_tratamientos, guide = "none") +  # Ocultar leyenda de color
  labs(title = "Histograma de Exceso de Riego", 
       x = "Valor", 
       y = "Frecuencia", 
       fill = "Tratamientos") +
  theme_minimal() +
  theme(legend.position = "none") # Ocultar la leyenda individual

# Extraer la leyenda de una de las gráficas
leyenda <- ggplot(data, aes(x = Wildtype, fill = factor(Tratamiento))) +
  geom_histogram(position = "dodge", color = "black") +  # Eliminar binwidth para control automático
  scale_fill_manual(values = colores_tratamientos, labels = paste("Tto.", 1:6)) +
  labs(fill = "Tratamientos") +
  theme_void() +
  theme(legend.position = "right") # Mantener sólo la leyenda

# Combinar las gráficas en una fila
layout <- (hist_wildtype | hist_sequia | hist_excesoriego) +  # Cambiar la disposición para una fila
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

# Mostrar el diseño
print(layout)



### 6 ### Haz un factor en la columna tratamiento y guárdalo en una variable. Pista: factor(factor$Tratamiento).
# Creamos el factor a partir de la columna Tratamiento y lo guardamos en una nueva variable
tratamiento_factor <- factor(datos$Tratamiento)

# Mostramos la estructura del factor creado
print("Estructura del factor creado:")
str(tratamiento_factor)

# Mostramos los niveles del factor
print("Niveles del factor:")
levels(tratamiento_factor)

# Mostramos un resumen del factor
print("Resumen del factor:")
summary(tratamiento_factor)



### 7 ### Calcula la media y la desviación estándar para cada tratamiento. Recomendación: es más fácil si usas aggregate() o tapply(). 
	#• aggregate(Variable~factor,datos,función)
	#• tapply(datos$Variable,factor,función)
## Método 1: Usando aggregate() ##
print("Resultados usando aggregate():")

# Medias para cada variable por tratamiento
print("Medias:")
aggregate(Wildtype ~ Tratamiento, datos, mean)
aggregate(Sequia ~ Tratamiento, datos, mean)
aggregate(ExcesoRiego ~ Tratamiento, datos, mean)

# Desviaciones estándar para cada variable por tratamiento
print("Desviaciones estándar:")
aggregate(Wildtype ~ Tratamiento, datos, sd)
aggregate(Sequia ~ Tratamiento, datos, sd)
aggregate(ExcesoRiego ~ Tratamiento, datos, sd)

## Método 2: Usando tapply() ##
print("Resultados usando tapply():")

# Medias para cada variable por tratamiento
print("Medias:")
print("Wildtype:")
tapply(datos$Wildtype, datos$Tratamiento, mean)
print("Sequía:")
tapply(datos$Sequia, datos$Tratamiento, mean)
print("Exceso de Riego:")
tapply(datos$ExcesoRiego, datos$Tratamiento, mean)

# Desviaciones estándar para cada variable por tratamiento
print("Desviaciones estándar:")
print("Wildtype:")
tapply(datos$Wildtype, datos$Tratamiento, sd)
print("Sequía:")
tapply(datos$Sequia, datos$Tratamiento, sd)
print("Exceso de Riego:")
tapply(datos$ExcesoRiego, datos$Tratamiento, sd)

# Creamos una tabla resumen más organizada
print("Tabla resumen con medias y desviaciones estándar:")

# Función para formatear media ± desviación estándar
format_mean_sd <- function(x) {
    sprintf("%.2f ± %.2f", mean(x), sd(x))
}

# Creamos un data frame con los resultados
resultados <- data.frame(
    Tratamiento = 1:6,
    Wildtype = tapply(datos$Wildtype, datos$Tratamiento, format_mean_sd),
    Sequia = tapply(datos$Sequia, datos$Tratamiento, format_mean_sd),
    ExcesoRiego = tapply(datos$ExcesoRiego, datos$Tratamiento, format_mean_sd)
)

# Mostramos la tabla final
print(resultados)


### 8 ### Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si usas table() con el factor.
# Usando table() con la columna Tratamiento
print("Número de elementos por tratamiento:")
table(datos$Tratamiento)

# También podemos usar el factor que creamos anteriormente
print("Número de elementos por tratamiento (usando el factor):")
table(tratamiento_factor)

# Podemos obtener un resumen más detallado
print("Resumen del factor tratamiento:")
summary(tratamiento_factor)

# Calculamos el porcentaje de cada tratamiento
print("Porcentaje de elementos por tratamiento:")
prop.table(table(datos$Tratamiento)) * 100

# Creamos una tabla resumen con conteos y porcentajes
conteos <- table(datos$Tratamiento)
porcentajes <- prop.table(conteos) * 100
resumen <- data.frame(
    Tratamiento = 1:6,
    Conteo = as.vector(conteos),
    Porcentaje = sprintf("%.1f%%", as.vector(porcentajes))
)

print("Tabla resumen de conteos y porcentajes:")
print(resumen)



### 9 ### Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente.
# Extraemos los datos del tratamiento 1
tratamiento1 <- datos[datos$Tratamiento == 1, ]

# Extraemos los datos del tratamiento 4
tratamiento4 <- datos[datos$Tratamiento == 4, ]

# Mostramos los datos extraídos para verificar
print("Datos del tratamiento 1:")
print(tratamiento1)

print("Datos del tratamiento 4:")
print(tratamiento4)

# Verificamos las dimensiones de cada subconjunto
print("Dimensiones del tratamiento 1:")
dim(tratamiento1)

print("Dimensiones del tratamiento 4:")
dim(tratamiento4)

# Mostramos un resumen estadístico de cada subconjunto
print("Resumen estadístico del tratamiento 1:")
summary(tratamiento1)

print("Resumen estadístico del tratamiento 4:")
summary(tratamiento4)



### 10 ### Queremos comprobar que hay diferencias significativas para el tratamiento 1 y el tratamiento 5 entre Wildtype y Sequia, y entre Wildtype y ExcesoRiego. Primero, necesitaríamos comprobar si los datos se distribuyen de forma normal. En función de los resultados de la prueba de normalidad, ¿qué test usarías para cada comparativa? ¿Puedes comparar también Sequia con ExcesoRiego en ambos tratamientos? ** En general, asumimos que las muestras son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo con tus resultados.
# 1. Extraemos los datos de los tratamientos 1 y 5
tratamiento1 <- datos[datos$Tratamiento == 1, ]
tratamiento5 <- datos[datos$Tratamiento == 5, ]

# 2. Test de normalidad (Shapiro-Wilk) para cada grupo y variable
print("Tests de normalidad (Shapiro-Wilk):")
print("Tratamiento 1:")
print("Wildtype:")
shapiro.test(tratamiento1$Wildtype)
print("Sequía:")
shapiro.test(tratamiento1$Sequia)
print("Exceso Riego:")
shapiro.test(tratamiento1$ExcesoRiego)

print("Tratamiento 5:")
print("Wildtype:")
shapiro.test(tratamiento5$Wildtype)
print("Sequía:")
shapiro.test(tratamiento5$Sequia)
print("Exceso Riego:")
shapiro.test(tratamiento5$ExcesoRiego)

# 3. Test de igualdad de varianzas (F test) para cada par de comparaciones
print("Tests de igualdad de varianzas:")

# Tratamiento 1
print("Tratamiento 1:")
print("Wildtype vs Sequía:")
var.test(tratamiento1$Wildtype, tratamiento1$Sequia)
print("Wildtype vs Exceso Riego:")
var.test(tratamiento1$Wildtype, tratamiento1$ExcesoRiego)
print("Sequía vs Exceso Riego:")
var.test(tratamiento1$Sequia, tratamiento1$ExcesoRiego)

# Tratamiento 5
print("Tratamiento 5:")
print("Wildtype vs Sequía:")
var.test(tratamiento5$Wildtype, tratamiento5$Sequia)
print("Wildtype vs Exceso Riego:")
var.test(tratamiento5$Wildtype, tratamiento5$ExcesoRiego)
print("Sequía vs Exceso Riego:")
var.test(tratamiento5$Sequia, tratamiento5$ExcesoRiego)

# 4. Realizamos las comparaciones según los resultados anteriores

# Para Tratamiento 1
print("Comparaciones para Tratamiento 1:")

# Wildtype vs Sequía
print("Wildtype vs Sequía (t.test con varianzas desiguales):")
t.test(tratamiento1$Wildtype, tratamiento1$Sequia, var.equal = FALSE)

# Wildtype vs Exceso Riego
print("Wildtype vs Exceso Riego (t.test con varianzas desiguales):")
t.test(tratamiento1$Wildtype, tratamiento1$ExcesoRiego, var.equal = FALSE)

# Sequía vs Exceso Riego
print("Sequía vs Exceso Riego (t.test con varianzas desiguales):")
t.test(tratamiento1$Sequia, tratamiento1$ExcesoRiego, var.equal = FALSE)

# Para Tratamiento 5
print("Comparaciones para Tratamiento 5:")

# Wildtype vs Sequía
print("Wildtype vs Sequía (t.test con varianzas desiguales):")
t.test(tratamiento5$Wildtype, tratamiento5$Sequia, var.equal = FALSE)

# Wildtype vs Exceso Riego
print("Wildtype vs Exceso Riego (t.test con varianzas desiguales):")
t.test(tratamiento5$Wildtype, tratamiento5$ExcesoRiego, var.equal = FALSE)

# Sequía vs Exceso Riego
print("Sequía vs Exceso Riego (t.test con varianzas desiguales):")
t.test(tratamiento5$Sequia, tratamiento5$ExcesoRiego, var.equal = FALSE)



### 11 ### Realiza un ANOVA para comparar el tratamiento 1 en las tres condiciones. Pista: primero separa los valores de tratamiento1 en Wildtype, Sequia y ExcesoRiego en variables separadas. Luego fíjate en el archivo “datos-anova.txt” y trata de colocar los datos de esa forma en una tabla. Por último, ejecuta el test.
# 1. Primero extraemos los valores del tratamiento 1 en variables separadas
wildtype_t1 <- tratamiento1$Wildtype
sequia_t1 <- tratamiento1$Sequia
exceso_t1 <- tratamiento1$ExcesoRiego

# 2. Creamos un vector con todos los valores
valores <- c(wildtype_t1, sequia_t1, exceso_t1)

# 3. Creamos un factor que indique a qué grupo pertenece cada valor
condiciones <- rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = length(wildtype_t1))
condiciones <- factor(condiciones)

# 4. Creamos el data frame en formato largo (como en datos-anova.txt)
datos_anova <- data.frame(
    Condicion = condiciones,
    Valor = valores
)

# Mostramos cómo quedaron organizados los datos
print("Estructura de los datos para ANOVA:")
head(datos_anova, n = 15)

# 5. Realizamos el test ANOVA
modelo_anova <- aov(Valor ~ Condicion, data = datos_anova)

# 6. Mostramos los resultados
print("Resultados del ANOVA:")
print(summary(modelo_anova))

# 7. Si el ANOVA es significativo, realizamos un test post-hoc de Tukey
print("Test post-hoc de Tukey:")
print(TukeyHSD(modelo_anova))

# 8. Verificamos los supuestos del ANOVA

# Normalidad de los residuos
print("Test de normalidad de los residuos (Shapiro-Wilk):")
print(shapiro.test(residuals(modelo_anova)))

# Homogeneidad de varianzas
print("Test de homogeneidad de varianzas (Bartlett):")
print(bartlett.test(Valor ~ Condicion, data = datos_anova))