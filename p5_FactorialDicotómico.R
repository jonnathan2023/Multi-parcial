# Análisis Factorial Dicotómico - Caso E-Commerce
# Integrante: Jose Luis Garay Ramos

# Contexto de la base de datos:
# Encuesta a 400 usuarios sobre hábitos y seguridad al comprar por internet.
# Todas las respuestas son dicotómicas: 1 = Sí, 0 = No.

# Dimensión 1: Seguridad y Prevención
# P1: Verifica que la página tenga HTTPS 
# P2: Evita guardar los datos de su tarjeta 
# P3: Lee las políticas de devoluciones 
# P4: Usa pasarelas intermediarias como PayPal 
# P5: Evita comprar en redes Wi-Fi públicas 

# Dimensión 2: Hábitos de Consumo Digital
# P6: Compra por internet al menos una vez al mes 
# P7: Prefiere usar las Apps de las tiendas 
# P8: Está suscrito a newsletters para ofertas 
# P9: Suele dejar reseñas de productos 
# P10: Planifica sus compras para días de descuento 

# 1. Generación de datos 
set.seed(2026) 
n <- 400       
latente_seguridad <- rnorm(n)
latente_uso <- rnorm(n)

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

write.csv(bd_ecommerce, "Datos_ECommerce.csv", row.names = FALSE)

test <- bd_ecommerce
test

# 2. Carga de paquetes y exploración inicial
if (!require("psych")) install.packages("psych")
if (!require("performance")) install.packages("performance")
if (!require("tidyverse")) install.packages("tidyverse")

library(psych)
library(performance)
library(tidyverse)

str(test)
summary(test)

# 3. Supuestos del Análisis Factorial Dicotómico
# Matriz de correlaciones tetracóricas
r.tetra = tetrachoric(test)
R = r.tetra$rho    
print(round(R, 2))

# Prueba de esfericidad de Bartlett
cortest.bartlett(R, n) 

# Índice KMO
KMO(R)
check_factorstructure(R, n)

# 4. Identificación del número de factores
fap <- fa.parallel(R, n.obs = n, fa = "fa", fm = "wls", 
                   main = "Análisis Paralelo - E-Commerce")

# 5. Modelamiento y Comparación (Sin Rotar vs Rotado)

# Modelo 1: Sin rotación
factorial_sin_rotar = fa(test, nfactors = 2, n.obs = n, rotate = "none", fm = "wls", cor = "tet")
print(factorial_sin_rotar)

# Gráfico del Modelo 1 (Sin rotar) - Exportado para exposición
png("Diagrama_SinRotar.png", width = 800, height = 600, res = 100)
fa.diagram(factorial_sin_rotar, e.size = .05, rsize = 3.5, digits = 2, col = "red", main = "Diagrama Factorial (Sin Rotación)")
dev.off()

# Modelo 2: Con rotación Varimax
factorial_rotado = fa(test, nfactors = 2, n.obs = n, rotate = "varimax", fm = "wls", cor = "tet")
print(factorial_rotado)

# Gráfico del Modelo 2 (Rotado) - Exportado para exposición
png("Diagrama_Rotado.png", width = 800, height = 600, res = 100)
fa.diagram(factorial_rotado, e.size = .05, rsize = 3.5, digits = 2, col = "blue", main = "Diagrama Factorial (Rotación Varimax)")
dev.off()

# 6. Extracción de Cargas Factoriales (Tabla Final)
dat = data.frame(ifelse(abs(factorial_rotado$loadings) == apply(abs(factorial_rotado$loadings), 1, max), 
                        apply(abs(factorial_rotado$loadings), 1, max), NA))

dat1 = data.frame(Item = rownames(dat), dat)
fact = gather(dat1, Factor, cargas, -Item) %>% drop_na()
Cargas = split(fact, fact$Factor) 
print(Cargas)

# 7. Gráficos visuales de impacto para la exposición

# Mapa de calor de correlaciones
png("Mapa_Calor.png", width = 800, height = 800, res = 100) 
cor.plot(R, numbers = TRUE, main = "Matriz de Correlaciones Tetracóricas", las = 2, cex = 0.8, colors = TRUE) 
dev.off() 

# Gráfico de dispersión 2D
png("Grafico_Cargas_2D.png", width = 800, height = 600, res = 100) 
plot(factorial_rotado, labels = rownames(factorial_rotado$loadings), main = "Dispersión Espacial de Ítems (2D)", cex = 1.2)  
dev.off()