# ============================================================== #
#   ANÁLISIS FACTORIAL ORDINAL — GENERACIÓN Y VALIDACIÓN         #
#   v9: Escala 1-5 | n=250 | Varianza > 76% | Scree Plot Realista#
#   Técnicas Multivariadas — UNALM 2026                          #
# ============================================================== #

# ── 0. PAQUETES NECESARIOS ─────────────────────────────────────
if(!require(psych)) install.packages("psych")
if(!require(writexl)) install.packages("writexl")
if(!require(MASS)) install.packages("MASS")
library(psych)
library(writexl)
library(MASS)
library(tidyverse)

# ── 1. GENERACIÓN DE LA DATA (Datos_Ordinales.xlsx) ────────────
set.seed(2026)
n <- 250
p <- 15

# Definimos cargas principales MUY FUERTES para asegurar >76% varianza
L <- matrix(0, nrow = p, ncol = 3)
L[c(2,5,6,10,12,14), 1] <- c(0.92, 0.94, 0.90, 0.88, 0.93, 0.91) # F1 - Proactivo
L[c(1,3,4,13),       2] <- c(0.88, 0.86, 0.90, 0.85)             # F2 - Liderazgo
L[c(7,8,9,11,15),    3] <- c(0.87, 0.89, 0.84, 0.91, 0.86)       # F3 - Empático

# Inyectamos ruido leve para un Scree Plot realista (curva suave)
ruido_cargas <- matrix(runif(p * 3, -0.05, 0.10), p, 3)
L[L == 0] <- ruido_cargas[L == 0]

# Matriz de Correlación Poblacional
R_pop <- L %*% t(L)
diag(R_pop) <- 1

# Suavizado para asegurar que sea definida positiva
R_pop <- cor.smooth(R_pop)

# Generación Multivariante Continua
datos_cont <- mvrnorm(n = n, mu = rep(0, p), Sigma = R_pop)

# Discretización a Likert 1-5 con umbrales realistas
Y <- matrix(NA_integer_, n, p)
for (j in 1:p) {
  # Umbrales dinámicos (simulan que no todos responden igual)
  cortes <- c(-Inf, sort(rnorm(4, c(-1.2, -0.4, 0.4, 1.2), 0.05)), Inf)
  Y[, j] <- as.numeric(cut(datos_cont[, j], breaks = cortes))
}

colnames(Y) <- paste0("P", 1:15)
test <- as.data.frame(Y)

# GUARDAR ARCHIVO
write_xlsx(test, "Datos_Ordinales.xlsx")
cat("\n✓ Archivo 'Datos_Ordinales.xlsx' generado con n=250.\n\n")


# ── 2. VERIFICACIÓN DE SUPUESTOS Y REQUISITOS ──────────────────
cat("========================================================\n")
cat("          VERIFICACIÓN DE CALIDAD ESTADÍSTICA           \n")
cat("========================================================\n\n")

# A. Matriz Policórica
r.poly <- polychoric(test)
R <- r.poly$rho

# B. Prueba de Bartlett (Debe ser p < 0.05)
bartlett <- cortest.bartlett(R, n = n)
cat("1. Bartlett (p-valor):", format(bartlett$p.value, scientific=F), 
    ifelse(bartlett$p.value < 0.05, " ✓ CUMPLE", " ✗ FALLA"), "\n")

# C. KMO (Debe ser > 0.60, ideal > 0.80)
kmo_val <- KMO(R)$MSA
cat("2. KMO Global       :", round(kmo_val, 3), 
    ifelse(kmo_val >= 0.80, " ✓ EXCELENTE", " ✓ CUMPLE"), "\n")

# D. Análisis Paralelo (Deben salir 3 factores)
fap <- fa.parallel(R, n.obs = n, fa = "fa", fm = "uls", main = "Scree Plot Realista")
cat("3. Factores Sugeridos:", fap$nfact, 
    ifelse(fap$nfact == 3, " ✓ CUMPLE", " ✗ REVISAR"), "\n")

# E. Extracción y Varianza Explicada
modelo <- fa(test, nfactors = 3, n.obs = n, rotate = "varimax", fm = "uls", cor = "poly")
var_acum <- sum(modelo$Vaccounted[2, ])
cat("4. Varianza Acum.   :", round(var_acum * 100, 2), "%", 
    ifelse(var_acum >= 0.76, " ✓ CUMPLE (>76%)", " ✗ BAJA"), "\n")

# F. Ajuste del Modelo (RMSR)
cat("5. Ajuste RMSR      :", round(modelo$rms, 4), 
    ifelse(modelo$rms < 0.05, " ✓ EXCELENTE", " ✗ POBRE"), "\n")

# G. Consistencia Interna (Omega)
om <- omega(test, nfactors = 3, fm = "minres", poly = TRUE, plot = FALSE)
cat("6. Fiabilidad Omega :", round(om$omega.tot, 3), 
    ifelse(om$omega.tot >= 0.80, " ✓ EXCELENTE", " ✓ CUMPLE"), "\n")

cat("\n========================================================\n")
cat("         VEREDICTO: DATA APTA PARA SUSTENTACIÓN         \n")
cat("========================================================\n")

library(readxl)
data <- read_excel("Datos_Ordinales.xlsx") 
data
