# ============================================================
#   ANÁLISIS FACTORIAL ORDINAL — EXPLICACIÓN PASO A PASO
#   Satisfacción Laboral · Datos Simulados
# ============================================================
#
#   ESTRUCTURA DEL SCRIPT:
#   1. Librerías
#   2. Matriz Phi  — correlaciones entre factores
#   3. Matriz Lambda — cargas factoriales
#   4. Matriz Sigma  — covarianza poblacional
#   5. MASS::mvrnorm — datos continuos latentes
#   6. Discretización — convertir a escala Likert 1-5
#   7. Vista previa de los datos generados
#
# ============================================================




# ============================================================
# BLOQUE 1 — LIBRERÍAS
# ============================================================
#
#   Cada paquete cumple un rol específico en el análisis:
#
#   psych       → análisis factorial exploratorio, KMO, omega
#   polycor     → correlaciones policóricas (clave para ordinales)
#   GPArotation → métodos de rotación (oblimin, varimax, etc.)
#   lavaan      → modelos confirmatorios con estimador WLSMV
#   semPlot     → diagramas de paths del modelo
#   MVN         → pruebas de normalidad multivariante
#
#   ¿Por qué no basta con R base?
#   Porque el AFO requiere correlaciones POLICÓRICAS (no Pearson)
#   y estimadores robustos (WLSMV) que solo están en estos paquetes.
#
# ------------------------------------------------------------

# install.packages(c("psych", "polycor", "GPArotation",
#                    "lavaan", "semPlot", "MVN", "MASS"))

library(psych)        # análisis factorial, KMO, omega
library(polycor)      # correlaciones policóricas
library(GPArotation)  # rotaciones oblimin / varimax
library(lavaan)       # AFC con estimador WLSMV
library(semPlot)      # diagrama de paths
library(MVN)          # normalidad multivariante




# ============================================================
# BLOQUE 2 — PARÁMETROS INICIALES
# ============================================================
#
#   set.seed() garantiza que los resultados sean REPRODUCIBLES:
#   cualquier persona que ejecute este script con el mismo seed
#   obtendrá exactamente los mismos datos generados.
#
#   n = 200 observaciones (tamaño muestral suficiente para AFO).
#
# ------------------------------------------------------------

set.seed(2024)   # semilla para reproducibilidad
n <- 200         # número de observaciones (encuestados)




# ============================================================
# BLOQUE 3 — MATRIZ Phi (Φ): CORRELACIONES ENTRE FACTORES
# ============================================================
#
#   Phi es una matriz 3×3 simétrica que define QUÉ TAN
#   RELACIONADOS están los 3 factores latentes entre sí.
#
#   Estructura:
#              F1     F2     F3
#   F1       [1.00   0.45   0.30]
#   F2       [0.45   1.00   0.35]
#   F3       [0.30   0.35   1.00]
#
#   La diagonal SIEMPRE es 1.00:
#   → Cada factor correlaciona perfecto consigo mismo.
#
#   Los valores fuera de la diagonal (off-diagonal):
#   → r(F1, F2) = 0.45 → Clima y Desarrollo: correlación moderada
#   → r(F1, F3) = 0.30 → Clima y Bienestar:  correlación baja-moderada
#   → r(F2, F3) = 0.35 → Desarrollo y Bienestar: correlación moderada
#
#   IMPLICACIÓN CLAVE:
#   Como los factores ESTÁN correlacionados (r ≠ 0), debemos
#   usar rotación OBLICUA (oblimin) en el análisis factorial,
#   NO rotación ortogonal (varimax), que asumiría r = 0.
#
# ------------------------------------------------------------

Phi <- matrix(c(
  1.00, 0.45, 0.30,   # F1: correlaciones con F1, F2, F3
  0.45, 1.00, 0.35,   # F2: correlaciones con F1, F2, F3
  0.30, 0.35, 1.00    # F3: correlaciones con F1, F2, F3
), nrow = 3)

# Etiquetar filas y columnas para mayor claridad
rownames(Phi) <- colnames(Phi) <- c("F1_Clima", "F2_Desarrollo", "F3_Bienestar")

cat("=== Matriz Phi: correlaciones entre factores latentes ===\n")
print(Phi)




# ============================================================
# BLOQUE 4 — MATRIZ Lambda (Λ): CARGAS FACTORIALES
# ============================================================
#
#   Lambda es una matriz 15×3 que define CUÁNTO carga cada
#   ítem en cada factor. Es el corazón de la estructura factorial.
#
#   Cada FILA  = un ítem del cuestionario (15 en total)
#   Cada COLUMNA = un factor latente (3 en total)
#
#   Lectura de una fila:
#     item1 → [0.75,  0.10,  0.05]
#              ↑        ↑      ↑
#           F1=0.75  F2=0.10  F3=0.05
#
#   → 0.75: carga ALTA en Factor 1 (Clima organizacional)
#   → 0.10: carga cruzada pequeña en Factor 2 (casi ruido)
#   → 0.05: carga cruzada mínima en Factor 3 (casi ruido)
#
#   Esto simula una ESTRUCTURA SIMPLE: cada ítem pertenece
#   claramente a UN solo factor, con cargas cruzadas despreciables.
#   En datos reales esto rara vez es tan limpio, pero es ideal
#   para validar el método de recuperación factorial.
#
#   Asignación de ítems a factores:
#   F1 Clima organizacional  → item1  a item5   (cargas 0.68–0.80)
#   F2 Desarrollo profesional → item6  a item10  (cargas 0.68–0.82)
#   F3 Bienestar/compensación → item11 a item15  (cargas 0.70–0.80)
#
# ------------------------------------------------------------

Lambda <- matrix(c(
  #  F1     F2     F3
  0.75,  0.10,  0.05,   # item1  → Clima
  0.80,  0.08,  0.10,   # item2  → Clima
  0.70,  0.12,  0.08,   # item3  → Clima
  0.72,  0.06,  0.12,   # item4  → Clima
  0.68,  0.15,  0.05,   # item5  → Clima
  0.08,  0.78,  0.10,   # item6  → Desarrollo
  0.10,  0.82,  0.08,   # item7  → Desarrollo
  0.12,  0.75,  0.06,   # item8  → Desarrollo
  0.05,  0.70,  0.12,   # item9  → Desarrollo
  0.09,  0.68,  0.15,   # item10 → Desarrollo
  0.06,  0.08,  0.80,   # item11 → Bienestar
  0.10,  0.10,  0.76,   # item12 → Bienestar
  0.12,  0.06,  0.72,   # item13 → Bienestar
  0.05,  0.12,  0.70,   # item14 → Bienestar
  0.08,  0.09,  0.74    # item15 → Bienestar
), nrow = 15, byrow = TRUE)

rownames(Lambda) <- paste0("item", 1:15)
colnames(Lambda) <- c("F1_Clima", "F2_Desarrollo", "F3_Bienestar")

cat("\n=== Matriz Lambda: cargas factoriales (15 ítems × 3 factores) ===\n")
print(Lambda)




# ============================================================
# BLOQUE 5 — MATRIZ Sigma (Σ): COVARIANZA POBLACIONAL
# ============================================================
#
#   Sigma es la MATRIZ DE COVARIANZA 15×15 de los ítems.
#   Se construye a partir de Lambda y Phi usando la
#   FÓRMULA DEL MODELO FACTORIAL:
#
#         Σ = Λ · Φ · Λᵀ  +  Ψ
#
#   Donde cada parte significa:
#
#   ┌─────────────────────────────────────────────────────┐
#   │  Λ · Φ · Λᵀ  →  varianza COMÚN (factores explican) │
#   │  Ψ           →  varianza ÚNICA (error / unicidad)   │
#   └─────────────────────────────────────────────────────┘
#
#   Parte 1: Lambda %*% Phi %*% t(Lambda)
#   ----------------------------------------
#   Multiplica las cargas factoriales por las correlaciones
#   entre factores y las cargas transpuestas.
#   → Produce la varianza compartida entre ítems GRACIAS
#     a los factores comunes.
#
#   Parte 2: diag(1 - rowSums(Lambda^2))
#   ----------------------------------------
#   rowSums(Lambda^2) = comunalidad de cada ítem
#     → cuánta varianza total explican los factores en ese ítem
#   1 - comunalidad = UNICIDAD (varianza específica del ítem)
#     → lo que el modelo factorial NO explica
#   diag(...) = convierte el vector en una matriz diagonal
#     → cada ítem tiene su propio término de error independiente
#
#   Ejemplo para item1:
#   Comunalidad = 0.75² + 0.10² + 0.05² = 0.5625 + 0.01 + 0.0025 = 0.575
#   Unicidad    = 1 - 0.575 = 0.425
#   → El 42.5% de la varianza del item1 no la explican los factores.
#
# ------------------------------------------------------------

# Varianza común (estructura factorial)
var_comun <- Lambda %*% Phi %*% t(Lambda)

# Unicidades (varianza específica por ítem)
unicidades <- diag(1 - rowSums(Lambda^2))

# Matriz de covarianza poblacional completa
Sigma <- var_comun + unicidades

cat("\n=== Comunalidades por ítem (varianza explicada por factores) ===\n")
comunalidades <- rowSums(Lambda^2)
print(round(comunalidades, 3))

cat("\n=== Unicidades por ítem (varianza NO explicada) ===\n")
print(round(1 - comunalidades, 3))

cat("\n=== Sigma: primeras 5 filas/columnas de la covarianza ===\n")
print(round(Sigma[1:5, 1:5], 3))




# ============================================================
# BLOQUE 6 — MASS::mvrnorm: DATOS CONTINUOS LATENTES
# ============================================================
#
#   mvrnorm() genera datos de una DISTRIBUCIÓN NORMAL
#   MULTIVARIANTE. Los argumentos son:
#
#   n     = número de observaciones (200)
#   mu    = vector de medias → rep(0, 15) = todos los ítems
#           tienen media 0 (variables estandarizadas)
#   Sigma = la matriz de covarianza que calculamos arriba
#
#   ¿Por qué "latentes"?
#   En este punto los datos son CONTINUOS: cada "respuesta"
#   es un número real como -0.83, 1.42, 0.07...
#   Representan la disposición REAL del encuestado antes
#   de convertirla a categorías discretas.
#
#   El modelo asume que detrás de cada respuesta ordinal
#   existe esta variable continua subyacente (latente).
#   El AFO trata de RECUPERAR esa estructura.
#
# ------------------------------------------------------------

datos_cont <- MASS::mvrnorm(
  n   = n,
  mu  = rep(0, 15),   # media 0 para todos los ítems
  Sigma = Sigma        # estructura de covarianza definida arriba
)

colnames(datos_cont) <- paste0("item", 1:15)

cat("\n=== Primeras 3 filas de datos CONTINUOS latentes ===\n")
print(round(head(datos_cont, 3), 3))

cat("\n=== Rango de valores (antes de discretizar) ===\n")
cat("Mínimo:", round(min(datos_cont), 2),
    "| Máximo:", round(max(datos_cont), 2), "\n")




# ============================================================
# BLOQUE 7 — DISCRETIZACIÓN: CONTINUO → LIKERT 1-5
# ============================================================
#
#   Este es el paso CRÍTICO que convierte los datos continuos
#   latentes en respuestas ordinales de escala Likert.
#
#   LÓGICA:
#   Se definen 4 umbrales que dividen la recta real en 5 zonas,
#   y cada zona corresponde a una categoría de respuesta:
#
#      −∞ ←──────|────────|────────|────────|──────→ +∞
#                -1.2    -0.4     0.4      1.2
#         [  1  ] [  2  ] [  3  ] [  4  ] [  5  ]
#
#   ┌────────────────┬──────────────────────────────┐
#   │ Rango continuo │ Categoría Likert              │
#   ├────────────────┼──────────────────────────────┤
#   │   x < -1.2     │ 1 — Muy en desacuerdo        │
#   │ -1.2 ≤ x < -0.4│ 2 — En desacuerdo            │
#   │ -0.4 ≤ x < 0.4 │ 3 — Ni de acuerdo/desacuerdo │
#   │  0.4 ≤ x < 1.2 │ 4 — De acuerdo               │
#   │   x ≥  1.2     │ 5 — Muy de acuerdo            │
#   └────────────────┴──────────────────────────────┘
#
#   ¿Por qué umbrales NO equidistantes?
#   Los umbrales [-1.2, -0.4, 0.4, 1.2] NO están igualmente
#   espaciados. Esto genera que las categorías 1 y 5 sean
#   menos frecuentes que la 3 (distribución asimétrica),
#   lo cual es MÁS REALISTA que asumir frecuencias iguales
#   en todas las categorías de una encuesta real.
#
#   Función apply():
#   Aplica la transformación a CADA COLUMNA (ítem) del
#   dataframe de datos continuos de forma vectorizada.
#
#   cut() + as.integer():
#   cut() asigna etiquetas "1","2","3","4","5" según umbrales.
#   as.integer() convierte esas etiquetas a números enteros.
#
# ------------------------------------------------------------

umbrales <- c(-1.2, -0.4, 0.4, 1.2)

datos_ord <- apply(datos_cont, 2, function(x) {
  cut(x,
      breaks = c(-Inf, umbrales, Inf),  # 5 intervalos
      labels = 1:5                       # etiquetas 1 a 5
  ) |>
    as.integer()                         # convertir a entero
})

colnames(datos_ord) <- paste0("item", 1:15)
datos_ord <- as.data.frame(datos_ord)

cat("\n=== Distribución de categorías (item1 como ejemplo) ===\n")
print(table(datos_ord$item1))
cat("Proporción:", round(prop.table(table(datos_ord$item1)) * 100, 1), "%\n")




# ============================================================
# BLOQUE 8 — VISTA PREVIA Y VERIFICACIÓN
# ============================================================
#
#   Verificamos que los datos generados tienen la forma
#   esperada: enteros del 1 al 5, sin valores faltantes,
#   con una distribución razonable en cada ítem.
#
# ------------------------------------------------------------

cat("\n=== Primeras 5 filas de datos ORDINALES (Likert 1-5) ===\n")
head(datos_ord, 5)

cat("\n=== Estadísticos descriptivos ===\n")
summary(datos_ord)

cat("\n=== Verificación: ¿solo valores 1-5? ===\n")
valores_unicos <- unique(unlist(datos_ord))
cat("Valores únicos encontrados:", sort(valores_unicos), "\n")
cat("Rango correcto:", ifelse(all(valores_unicos %in% 1:5), "SÍ ✓", "NO ✗"), "\n")

cat("\n=== Frecuencias globales (todos los ítems) ===\n")
freq_global <- table(unlist(datos_ord))
print(freq_global)
barplot(freq_global,
        main  = "Distribución global de respuestas (todos los ítems)",
        xlab  = "Categoría Likert",
        ylab  = "Frecuencia",
        col   = c("#3b4fd4","#6c8ef5","#a8b8ff","#6c8ef5","#3b4fd4"),
        names.arg = c("1\nMuy en\ndesacuerdo",
                      "2\nEn\ndesacuerdo",
                      "3\nNeutro",
                      "4\nDe\nacuerdo",
                      "5\nMuy de\nacuerdo"))

# ============================================================
# ¿POR QUÉ ESTE ENFOQUE ES VÁLIDO PARA AFO?
# ============================================================
#
#   El Análisis Factorial Ordinal se basa en el modelo de
#   VARIABLE LATENTE SUBYACENTE (Underlying Variable Model):
#
#   SUPUESTO CLAVE:
#   Detrás de cada respuesta ordinal (1,2,3,4,5) existe
#   una variable continua latente (y*) que el encuestado
#   "corta" al responder según sus umbrales personales.
#
#   Este script simula exactamente ese proceso:
#   1. Generamos y* continua con MASS::mvrnorm()     ← variable latente
#   2. Aplicamos umbrales de corte con cut()         ← proceso de respuesta
#   3. Obtenemos categorías ordinales 1-5             ← respuesta observada
#
#   El AFO RECUPERA la estructura de y* usando:
#   • Correlaciones POLICÓRICAS (no Pearson)
#     → estiman correlaciones entre las y* subyacentes
#   • Estimador WLSMV (no ML)
#     → robusto a no-normalidad y datos discretos
#
#   ERROR COMÚN: usar Pearson + ML con datos Likert
#   → subestima cargas factoriales
#   → índices de ajuste artificialmente malos
#   → conclusiones incorrectas sobre la estructura
#
# ============================================================

cat("\n")
cat("============================================================\n")
cat("  Datos listos. Dimensiones:", nrow(datos_ord), "filas ×",
    ncol(datos_ord), "columnas\n")
cat("  Escala: Likert 1-5 | Ítems:", ncol(datos_ord),
    "| Observaciones:", nrow(datos_ord), "\n")
cat("  Siguiente paso: verificar supuestos (Bartlett + KMO)\n")
cat("  Luego: fa() con fm='wls', cor='poly' o cfa() con WLSMV\n")
cat("============================================================\n")
