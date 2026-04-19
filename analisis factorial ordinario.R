# ============================================================
# ANÁLISIS FACTORIAL ORDINAL - SATISFACCIÓN LABORAL
# ============================================================

# --- Instalar y cargar librerías necesarias ---
# install.packages(c("psych", "polycor", "GPArotation",
#                    "lavaan", "semPlot", "MVN"))

library(psych)       # Funciones factoriales y psicométricas
#library(polycor)     # Correlaciones policóricas
library(GPArotation) # Rotaciones factoriales
library(lavaan)      # Modelos de ecuaciones estructurales
#library(semPlot)     # Diagramas de paths
library(MVN)         # Normalidad multivariante

# ============================================================
# GENERACIÓN DE DATOS ORDINALES SIMULADOS
# ============================================================

set.seed(2024)
n <- 200  # tamaño muestral

# Definir 3 factores latentes correlacionados
Phi <- matrix(c(
  1.00, 0.45, 0.30,
  0.45, 1.00, 0.35,
  0.30, 0.35, 1.00
), nrow = 3)

# Cargas factoriales (cada factor → 5 ítems)
Lambda <- matrix(c(
  # F1   F2   F3
  0.75, 0.10, 0.05,  # item1
  0.80, 0.08, 0.10,  # item2
  0.70, 0.12, 0.08,  # item3
  0.72, 0.06, 0.12,  # item4
  0.68, 0.15, 0.05,  # item5
  0.08, 0.78, 0.10,  # item6
  0.10, 0.82, 0.08,  # item7
  0.12, 0.75, 0.06,  # item8
  0.05, 0.70, 0.12,  # item9
  0.09, 0.68, 0.15,  # item10
  0.06, 0.08, 0.80,  # item11
  0.10, 0.10, 0.76,  # item12
  0.12, 0.06, 0.72,  # item13
  0.05, 0.12, 0.70,  # item14
  0.08, 0.09, 0.74   # item15
), nrow = 15, byrow = TRUE)

# Generar datos continuos latentes
Sigma <- Lambda %*% Phi %*% t(Lambda) +
  diag(1 - rowSums(Lambda^2))
datos_cont <- MASS::mvrnorm(n, mu = rep(0, 15), Sigma = Sigma)

# Discretizar a escala Likert 1-5 (umbrales ligeramente asimétricos)
umbrales <- c(-1.2, -0.4, 0.4, 1.2)
datos_ord <- apply(datos_cont, 2, function(x) {
  cut(x, breaks = c(-Inf, umbrales, Inf), labels = 1:5) |>
    as.integer()
})

colnames(datos_ord) <- paste0("item", 1:15)
datos_ord <- as.data.frame(datos_ord)

# Vista previa
head(datos_ord, 5)
summary(datos_ord)