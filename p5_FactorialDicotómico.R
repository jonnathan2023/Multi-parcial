#==============================================================================#
# ANÁLISIS FACTORIAL DICOTÓMICO: SEGURIDAD Y HÁBITOS EN E-COMMERCE
# Especialidad: Estadística e Informática - UNALM
#==============================================================================#

# 1. Carga de Paquetes y Configuración
# ------------------------------------------------------------------------------
packages <- c("psych", "performance", "tidyverse", "corrplot", "kableExtra", "polycor", "GPArotation")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))

# 2. Carga y Auditoría de Datos
# ------------------------------------------------------------------------------
# Solo carga de la data generada (sin mostrar simulación)
test <- read.csv("Datos_ECommerce.csv")
str(test)
summary(test)

# 3. Supuestos: Matriz de Correlaciones Tetracóricas
# ------------------------------------------------------------------------------
# Obligatorio para datos binarios (Aldás & Uriel, Cap. 11)
r_tetra_list <- tetrachoric(test)
R <- r_tetra_list$rho
print(round(R, 2))

# 4. Pruebas de Factorabilidad (Bartlett y KMO)
# ------------------------------------------------------------------------------
# Esfericidad de Bartlett
bartlett_res <- cortest.bartlett(R, n = nrow(test))
print(bartlett_res)

# Índice KMO
kmo_res <- KMO(R)
print(kmo_res)
check_factorstructure(R, n = nrow(test))

# 5. Identificación del Número de Factores
# ------------------------------------------------------------------------------
fap <- fa.parallel(R, n.obs = nrow(test), fa = "fa", fm = "wls", 
                   main = "Análisis Paralelo - E-Commerce")

# 6. Modelamiento y Comparación (Sin Rotar vs Rotado)
# ------------------------------------------------------------------------------
# Modelo 1: Sin rotación
fact_sin <- fa(test, nfactors = 2, n.obs = nrow(test), rotate = "none", fm = "wls", cor = "tet")
print(fact_sin)

# Modelo 2: Con rotación Varimax (Validada si Phi < 0.32)
fact_rotado <- fa(test, nfactors = 2, n.obs = nrow(test), rotate = "varimax", fm = "wls", cor = "tet")
print(fact_rotado)

# 7. Extracción de Cargas Factoriales (Tabla Final)
# ------------------------------------------------------------------------------
dat <- data.frame(ifelse(abs(fact_rotado$loadings) == apply(abs(fact_rotado$loadings), 1, max), 
                         apply(abs(fact_rotado$loadings), 1, max), NA))
dat1 <- data.frame(Item = rownames(dat), dat)
fact_long <- gather(dat1, Factor, cargas, -Item) %>% drop_na()
Cargas <- split(fact_long, fact_long$Factor)
print(Cargas)

# 8. Evaluación de la Calidad del Modelo (Bondad de Ajuste)
# ------------------------------------------------------------------------------
# Índices críticos para la sustentación
fit_indices <- data.frame(
  RMSR = fact_rotado$rms,
  TLI = fact_rotado$TLI,
  RMSEA = fact_rotado$RMSEA[1],
  Varianza_Acumulada = sum(fact_rotado$Vaccounted[2,])
)
print(fit_indices)

# 9. Análisis de Fiabilidad (Coeficiente Omega)
# ------------------------------------------------------------------------------
# Superior al Alfa de Cronbach para datos dicotómicos
omega_res <- omega(R, nfactors = 2, fm = "wls", plot = FALSE, n.iter = 1)
cat("Fiabilidad Omega Total:", round(omega_res$omega.tot, 3), "\n")

# 10. Exportación de Gráficos de Impacto (Corrección de error 'col')
# ------------------------------------------------------------------------------

# A. Mapa de Calor
png("Mapa_Calor.png", width = 800, height = 800, res = 120)
corrplot(R, method = "color", type = "upper", addCoef.col = "black", 
         number.cex = 0.7, tl.col = "black", title = "Matriz Tetracórica", mar=c(0,0,1,0))
dev.off()

# B. Dispersión 2D (Corregido: usando factor.plot para evitar conflicto de argumentos)
png("Grafico_Cargas_2D.png", width = 1000, height = 800, res = 150)
factor.plot(fact_rotado, labels = rownames(fact_rotado$loadings), 
            main = "Dispersión de Ítems (2D)", pos = 3)
abline(h = 0, v = 0, lty = 2, col = "gray")
dev.off()

# C. Diagrama Factorial
png("Diagrama_Rotado.png", width = 1000, height = 800, res = 150)
fa.diagram(fact_rotado, e.size = .05, rsize = 3.5, digits = 2, 
           col = "blue", main = "Estructura Factorial (Varimax)")
dev.off()
