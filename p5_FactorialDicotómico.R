# ==================================================================#
# CASO: ANÁLISIS FACTORIAL DICOTÓMICO (COMPORTAMIENTO E-COMMERCE)   #
# ==================================================================#

# ── 1. GENERACIÓN DE LA BASE DE DATOS (DATASET CORREGIDO) ─────────
set.seed(2026) # Semilla para reproducibilidad (Ciclo 2026-I)
n <- 400       # Aumentamos la muestra a 400 para asegurar variabilidad

# Simulamos 2 variables latentes (Seguridad y Uso) independientes
latente_seguridad <- rnorm(n)
latente_uso <- rnorm(n)

# Generamos 10 ítems dicotómicos.
# Usamos cargas equilibradas (aprox 0.65) y error (aprox 0.75) 
# El corte "> 0" asegura un ~50% de respuestas Sí/No, evitando varianzas cero.
bd_ecommerce <- data.frame(
  P1 = as.numeric(0.65 * latente_seguridad + 0.75 * rnorm(n) > 0),
  P2 = as.numeric(0.70 * latente_seguridad + 0.71 * rnorm(n) > 0),
  P3 = as.numeric(0.60 * latente_seguridad + 0.80 * rnorm(n) > 0),
  P4 = as.numeric(0.65 * latente_seguridad + 0.75 * rnorm(n) > 0),
  P5 = as.numeric(0.75 * latente_seguridad + 0.66 * rnorm(n) > 0),
  
  P6 = as.numeric(0.65 * latente_uso + 0.75 * rnorm(n) > 0),
  P7 = as.numeric(0.70 * latente_uso + 0.71 * rnorm(n) > 0),
  P8 = as.numeric(0.60 * latente_uso + 0.80 * rnorm(n) > 0),
  P9 = as.numeric(0.65 * latente_uso + 0.75 * rnorm(n) > 0),
  P10= as.numeric(0.75 * latente_uso + 0.66 * rnorm(n) > 0)
)

# Guardar la base de datos en un archivo CSV para presentar en el trabajo
write.csv(bd_ecommerce, "Datos_ECommerce.csv", row.names = FALSE)

test <- bd_ecommerce
head(test)

# ── 2. PAQUETES Y SUPUESTOS ───────────────────────────────────────
if (!require("psych")) install.packages("psych")
if (!require("performance")) install.packages("performance")
if (!require("tidyverse")) install.packages("tidyverse")

library(psych)
library(performance)
library(tidyverse)

# Matriz de Correlaciones Tetracóricas
r.tetra = tetrachoric(test)
R = r.tetra$rho    # Extrayendo la matriz
print(round(R, 2))

# Esfericidad de Bartlett
# H0: La matriz de correlaciones es una matriz identidad
cortest.bartlett(R, n = nrow(test)) 

# KMO (Kaiser-Meyer-Olkin)
# Evalúa la adecuación muestral (>0.7 es aceptable)
KMO(R)
check_factorstructure(R, n = nrow(test))

# ── 3. IDENTIFICACIÓN DEL N° DE FACTORES ──────────────────────────
# Análisis Paralelo para matriz tetracórica
fap <- fa.parallel(R, n.obs = n, fa = "fa", fm = "wls", 
                   main = "Análisis Paralelo - E-Commerce")

# ── 4. CORRIENDO EL MODELO (MÉTODO WLS Y ROTACIÓN VARIMAX) ────────
# Usamos fm="wls" y cor="tet" ideal para datos dicotómicos.
factorial_rotado = fa(test, nfactors = 2, n.obs = n, rotate = "varimax", 
                      fm = "wls", cor = "tet")
print(factorial_rotado)

# ── 5. GRÁFICA DE SEGMENTACIÓN Y CARGAS FACTORIALES ───────────────
# Gráfico
e1 = fa.diagram(factorial_rotado, e.size = .05, rsize = 3.5, 
                digits = 2, col = "blue", main="Diagrama Factorial")

# Extraer y ordenar las cargas factoriales mayores
dat = data.frame(ifelse(abs(factorial_rotado$loadings) == apply(abs(factorial_rotado$loadings), 1, max), 
                        apply(abs(factorial_rotado$loadings), 1, max), NA))

dat1 = data.frame(Item = rownames(dat), dat)
fact = gather(dat1, Factor, cargas, -Item) %>% drop_na()
Cargas = split(fact, fact$Factor) 
print(Cargas)

# ==================================================================#
# ── 6. GRÁFICOS COMPLEMENTARIOS PARA LA EXPOSICIÓN ────────────────
# ==================================================================#

# 1. GRÁFICO DE BARRAS: Distribución de respuestas (Exploratorio)
# Muestra visualmente qué porcentaje de usuarios respondió Sí/No a cada pregunta.
test_long <- pivot_longer(test, cols = everything(), 
                          names_to = "Item", values_to = "Respuesta")
test_long$Respuesta <- factor(test_long$Respuesta, levels = c(0,1), labels = c("No", "Sí"))

ggplot(test_long, aes(x = Item, fill = Respuesta)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribución de Respuestas por Ítem (Sí / No)",
       x = "Ítems del Cuestionario", y = "Porcentaje",
       fill = "Respuesta") +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("tomato", "steelblue"))

# 2. MAPA DE CALOR (HEATMAP): Matriz de Correlaciones Tetracóricas
# Exportado directamente a PNG para evitar problemas de márgenes

png("Mapa_Calor.png", width = 800, height = 800, res = 100) # Abre el archivo
cor.plot(R, 
         numbers = TRUE, 
         main = "Mapa de Calor - Correlaciones Tetracóricas",
         las = 2,       # Gira las etiquetas de los ejes
         cex = 0.8,     # Tamaño de los números
         colors = TRUE) # Usa colores azul (positiva) y rojo (negativa)
dev.off() # Cierra y guarda el archivo

# 3. GRÁFICO DE CARGAS FACTORIALES EN 2D (Factor Loadings Plot)
# Exportado directamente a PNG para evitar problemas de márgenes y choque de argumentos

png("Grafico_Cargas_2D.png", width = 800, height = 600, res = 100) 
plot(factorial_rotado, 
     labels = rownames(factorial_rotado$loadings),
     main = "Dispersión de Ítems en el Plano Factorial (2D)",
     cex = 1.2)  # Eliminamos 'col' para que no choque
dev.off()