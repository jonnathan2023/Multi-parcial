###############################################################
#                                                             #
#   ANÁLISIS DE REGRESIÓN LINEAL MULTIVARIADA                 #
#   Calidad del Café Arábica — Selva Alta del Perú            #
#   Archivo de datos: p1_regre_cafe.csv                       #
#                                                             #
###############################################################

# ── PAQUETES ──────────────────────────────────────────────────
paquetes <- c("psych", "knitr", "GGally", "ggplot2", "gridExtra",
              "mvnormtest", "car", "lmtest", "kableExtra")
for (p in paquetes) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, quiet = TRUE)
  library(p, character.only = TRUE, quietly = TRUE)
}

# ══════════════════════════════════════════════════════════════
#   CARGA DE DATOS
# ══════════════════════════════════════════════════════════════
# Datos simulados bajo condiciones controladas (variedad Typica,
# método lavado, campaña principal). No requieren limpieza.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bd <- read.csv("p1_regre_cafe.csv")
head(bd)
dim(bd)   # 199 filas  x  8 columnas

# ══════════════════════════════════════════════════════════════
#   1. ANÁLISIS EXPLORATORIO
# ══════════════════════════════════════════════════════════════

# ── Estadísticos descriptivos con kable(describe()) ──────────
# describe() reporta: n, media, SD, mediana, media recortada, MAD,
# mín, máx, rango, asimetría, curtosis y error estándar.
kable(
  describe(bd),
  caption = "Tabla 1. Estadísticos descriptivos del conjunto de datos de café arábica",
  align   = "c",
  digits  = 3
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    latex_options = c("scale_down", "HOLD_position")
  ) %>%
  scroll_box(width = "100%")

# ── Matriz de dispersión y correlaciones (ggpairs) ────────────
# Diagonal:     densidad de cada variable
# Triáng. inf.: dispersión con línea de tendencia
# Triáng. sup.: coeficiente de correlación de Pearson
# Se aprecia ya la alta correlación negativa altitud–temperatura.
ggpairs(bd,
        title = "Café Arábica — Selva Alta del Perú: Correlaciones y Dispersión",
        aes(alpha = 0.4))

# ══════════════════════════════════════════════════════════════
#   2. MODELO COMPLETO (5 X)
# ══════════════════════════════════════════════════════════════
# Se estima el modelo con las cinco variables predictoras para
# detectar posibles problemas antes de evaluar supuestos.
mod_completo <- lm(cbind(VOCs, acidez, TDS) ~
                     altitud + temperatura + precipitacion +
                     horas_sol + mat_organica,
                   data = bd)

# ══════════════════════════════════════════════════════════════
#   3. DIAGNÓSTICO DE MULTICOLINEALIDAD
# ══════════════════════════════════════════════════════════════

# ── alias(): detecta dependencias lineales exactas ────────────
# Si una variable es combinación lineal exacta de otra, alias() la lista.
# En este caso no hay dependencia exacta, pero la correlación
# altitud–temperatura (r ≈ -0.95) produce multicolinealidad severa
# que alias() no alcanza a detectar por sí solo.
alias_completo <- alias(mod_completo)
alias_completo

# ── VIF (Variance Inflation Factor) ──────────────────────────
# Cuantifica cuánto se infla la varianza de cada coeficiente
# por la colinealidad con los demás predictores.
# Interpretación:
#   VIF < 5      → sin problema
#   5 ≤ VIF < 10 → colinealidad moderada, vigilar
#   VIF ≥ 10     → colinealidad severa → candidato a eliminar
vif_completo <- vif(lm(VOCs ~ altitud + temperatura + precipitacion +
                          horas_sol + mat_organica, data = bd))
round(vif_completo, 3)

# → altitud y temperatura presentan VIF > 10 (colinealidad severa).
# → temperatura tiene el VIF más alto: es la variable más redundante,
#   ya que físicamente es consecuencia de la altitud (gradiente térmico
#   de -0.6 °C por cada 100 m de ascenso en la selva alta peruana).
# → DECISIÓN: se elimina TEMPERATURA del modelo.

# ══════════════════════════════════════════════════════════════
#   4. MODELO SIN TEMPERATURA (4 X)
# ══════════════════════════════════════════════════════════════
mod_4x <- lm(cbind(VOCs, acidez, TDS) ~
               altitud + precipitacion + horas_sol + mat_organica,
             data = bd)

# ── VIF después de eliminar temperatura ──────────────────────
# Se verifica que la multicolinealidad haya quedado resuelta.
vif_4x <- vif(lm(VOCs ~ altitud + precipitacion +
                   horas_sol + mat_organica, data = bd))
round(vif_4x, 3)
# Todos los VIF < 5 → multicolinealidad resuelta. Modelo estimable.

# ══════════════════════════════════════════════════════════════
#   5. SUPUESTOS DEL MODELO — NORMALIDAD MULTIVARIADA
# ══════════════════════════════════════════════════════════════
# H0: Las variables dependientes (VOCs, acidez, TDS) siguen
#     conjuntamente una distribución normal multivariada.
# Ha: No siguen distribución normal multivariada.
# Se rechaza H0 si p-value < 0.05.
y_vars <- bd[, c("VOCs", "acidez", "TDS")]

mshapiro.test(t(as.matrix(y_vars)))
# p > 0.05 → No se rechaza H0. Supuesto de normalidad SATISFECHO.

# ══════════════════════════════════════════════════════════════
#   6. SUPUESTOS — HOMOCEDASTICIDAD POR CADA Y (Breusch-Pagan)
# ══════════════════════════════════════════════════════════════
# H0: La varianza de los errores es constante (homocedasticidad).
# Ha: La varianza de los errores no es constante.
# Se rechaza H0 si p-value < 0.05.

# ── VOCs ──────────────────────────────────────────────────────
bptest(lm(VOCs ~ altitud + precipitacion + horas_sol + mat_organica,
          data = bd))
# p > 0.05 → Varianza constante. Supuesto SATISFECHO.

# ── Acidez ────────────────────────────────────────────────────
bptest(lm(acidez ~ altitud + precipitacion + horas_sol + mat_organica,
          data = bd))
# p > 0.05 → Varianza constante. Supuesto SATISFECHO.

# ── TDS ───────────────────────────────────────────────────────
bptest(lm(TDS ~ altitud + precipitacion + horas_sol + mat_organica,
          data = bd))
# p > 0.05 → Varianza constante. Supuesto SATISFECHO.

# ══════════════════════════════════════════════════════════════
#   7. PRUEBA DE ESFERICIDAD DE BARTLETT
#      Justifica el uso de regresión multivariada
# ══════════════════════════════════════════════════════════════
# H0: La matriz de correlación entre las Y es la identidad
#     (VOCs, acidez y TDS son independientes entre sí).
# Ha: Las Y están correlacionadas.
# Se rechaza H0 si p-value < 0.05.
# Rechazar H0 justifica modelarlas conjuntamente en vez de hacer
# tres regresiones univariadas independientes.
options(scipen = 0)
cortest.bartlett(cor(y_vars), n = nrow(y_vars))
# p < 0.05 → Se rechaza H0 → las Y están correlacionadas.
# El enfoque de regresión multivariada queda JUSTIFICADO.

# ══════════════════════════════════════════════════════════════
#   8. HIPÓTESIS PRINCIPAL DEL MODELO (mod_4x)
# ══════════════════════════════════════════════════════════════
# H0: β_altitud = β_precipitacion = β_horas_sol = β_mat_organica = 0
#     (ninguna variable predictora influye sobre las dependientes)
# H1: Al menos un βj ≠ 0
#
# Se evalúa con linearHypothesis y los cuatro estadísticos clásicos
# (Pillai, Wilks, Hotelling-Lawley, Roy), robustos ante violaciones
# leves de normalidad.

ev_mod_4x <- linearHypothesis(mod_4x,
                               hypothesis.matrix = c("altitud = 0",
                                                     "precipitacion = 0",
                                                     "horas_sol = 0",
                                                     "mat_organica = 0"))
ev_mod_4x
# Todos los criterios p < 0.05 → Se rechaza H0.
# Al menos una variable predictora influye sobre las propiedades
# químicas del café de forma conjunta.

# ══════════════════════════════════════════════════════════════
#   9. HIPÓTESIS INDIVIDUALES POR VARIABLE (mod_4x)
# ══════════════════════════════════════════════════════════════
# Para cada variable Xj:
# H0: βj,VOCs = βj,acidez = βj,TDS = 0
#     (Xj no influye sobre ninguna de las tres variables respuesta)
# H1: Al menos un βj,k ≠ 0  para k = VOCs, acidez, TDS
#
# Se reportan los cuatro criterios para identificar cuáles variables
# son significativas y cuáles no.

print(summary(manova(mod_4x), test = "Pillai"))

print(summary(manova(mod_4x), test = "Wilks"))

print(summary(manova(mod_4x), test = "Hotelling-Lawley"))

print(summary(manova(mod_4x), test = "Roy"))

# → altitud, horas_sol y mat_organica: significativas (p < 0.05)
#   en los cuatro criterios.
# → precipitacion: NO significativa (p > 0.05) en los cuatro criterios.
# → DECISIÓN: se elimina PRECIPITACIÓN del modelo.

# ══════════════════════════════════════════════════════════════
#   10. MODELO FINAL (3 X): altitud + horas_sol + mat_organica
# ══════════════════════════════════════════════════════════════
mod_final <- lm(cbind(VOCs, acidez, TDS) ~
                  altitud + horas_sol + mat_organica,
                data = bd)

# ── Normalidad multivariada (modelo final) ────────────────────
# Las Y no cambian; se confirma que el supuesto sigue vigente.
mshapiro.test(t(as.matrix(y_vars)))
# p > 0.05 → Normalidad SATISFECHA.

# ── Homocedasticidad (modelo final) ──────────────────────────
bptest(lm(VOCs   ~ altitud + horas_sol + mat_organica, data = bd))
# p > 0.05 → Supuesto SATISFECHO en VOCs.

bptest(lm(acidez ~ altitud + horas_sol + mat_organica, data = bd))
# p > 0.05 → Supuesto SATISFECHO en Acidez.

bptest(lm(TDS    ~ altitud + horas_sol + mat_organica, data = bd))
# p > 0.05 → Supuesto SATISFECHO en TDS.

# ── Hipótesis global — modelo final ───────────────────────────
ev_mod_final <- linearHypothesis(mod_final,
                                  hypothesis.matrix = c("altitud = 0",
                                                        "horas_sol = 0",
                                                        "mat_organica = 0"))
ev_mod_final

print(summary(manova(mod_final), test = "Pillai"))

# ══════════════════════════════════════════════════════════════
#   11. MÉTRICAS DE AJUSTE
# ══════════════════════════════════════════════════════════════

# ── Función MAPE ──────────────────────────────────────────────
# Mean Absolute Percentage Error: error promedio como porcentaje
# del valor real. Criterio: MAPE cercano a 0 indica buen ajuste.
mape <- function(y_pred, y_real) {
  if (any(y_real <= 0)) warning("Valores reales no positivos detectados")
  mean(abs((y_pred - y_real) / y_real)) * 100
}

# ── Valores ajustados (fitted) sobre toda la muestra ─────────
# Se usa fitted() para obtener los valores predichos por el modelo
# sobre las mismas 199 observaciones con las que fue entrenado.
# Esto mide el error de ajuste en muestra completa.
ajustados <- fitted(mod_final)   # matriz 199 x 3

# ── Tabla predichos vs reales (toda la muestra) ───────────────
comparacion <- data.frame(
  VOCs_REAL       = bd$VOCs,
  VOCs_PREDICHO   = round(ajustados[, "VOCs"],   2),
  ACIDEZ_REAL     = bd$acidez,
  ACIDEZ_PREDICHA = round(ajustados[, "acidez"], 3),
  TDS_REAL        = bd$TDS,
  TDS_PREDICHO    = round(ajustados[, "TDS"],    4)
)

# Mostrar primeras 20 filas a modo ilustrativo
cat("── Predichos vs Reales (primeras 20 observaciones) ─────\n")
print(head(comparacion, 20), row.names = FALSE)

# ── MAPE sobre toda la muestra ────────────────────────────────
# Al usar los 199 lotes se obtiene una medida global del error
# de ajuste del modelo, más representativa que un subconjunto.
mape_vocs   <- mape(comparacion$VOCs_PREDICHO,   comparacion$VOCs_REAL)
mape_acidez <- mape(comparacion$ACIDEZ_PREDICHA, comparacion$ACIDEZ_REAL)
mape_tds    <- mape(comparacion$TDS_PREDICHO,    comparacion$TDS_REAL)

cat("\nMAPE VOCs  :", round(mape_vocs,   3), "%\n")
cat("MAPE Acidez:", round(mape_acidez, 3), "%\n")
cat("MAPE TDS   :", round(mape_tds,    3), "%\n")

# ── R² ajustado por variable respuesta ───────────────────────
# Se usa el coeficiente ajustado, más robusto al número de
# predictores y mejor indicador para combatir el sobreajuste.
resultados  <- summary(mod_final)
r2aj_vocs   <- resultados$`Response VOCs`$adj.r.squared   * 100
r2aj_acidez <- resultados$`Response acidez`$adj.r.squared * 100
r2aj_tds    <- resultados$`Response TDS`$adj.r.squared    * 100

cat("\nR²aj VOCs  :", round(r2aj_vocs,   3), "%\n")
cat("R²aj Acidez:", round(r2aj_acidez, 3), "%\n")
cat("R²aj TDS   :", round(r2aj_tds,    3), "%\n")

# ── Gráficos de barras: MAPE y R²aj ──────────────────────────
metricas <- data.frame(
  Variable = c("VOCs", "Acidez", "TDS"),
  MAPE     = c(mape_vocs, mape_acidez, mape_tds),
  R2aj     = c(r2aj_vocs, r2aj_acidez, r2aj_tds)
)

p_mape <- ggplot(metricas, aes(x = Variable, y = MAPE, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(MAPE, 2), " %")), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c("steelblue", "darkorange", "forestgreen")) +
  labs(title = "MAPE por Variable Respuesta",
       x = "Variable", y = "MAPE (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_r2 <- ggplot(metricas, aes(x = Variable, y = R2aj, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(R2aj, 2), " %")), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c("steelblue", "darkorange", "forestgreen")) +
  labs(title = "R² Ajustado por Variable Respuesta (%)",
       x = "Variable", y = "R² ajustado (%)") +
  ylim(0, 100) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

grid.arrange(p_mape, p_r2, ncol = 2)

# ══════════════════════════════════════════════════════════════
#   12. VALIDACIÓN CRUZADA — 100 SEMILLAS (80 / 20)
# ══════════════════════════════════════════════════════════════
# Implementación manual (sin paquetes externos):
#   1. Se generan 100 semillas aleatorias a partir de la semilla madre 2026.
#   2. En cada iteración se divide la muestra 80/20 con esa semilla.
#   3. Se entrena el modelo en el 80% (train) y se predice en el 20% (test).
#   4. MAPE se calcula sobre el conjunto de prueba (datos no vistos).
#   5. R²aj se calcula manualmente sobre el conjunto de prueba:
#        SS_res = sum((y_real - y_pred)^2)
#        SS_tot = sum((y_real - mean(y_real))^2)
#        R²     = 1 - SS_res / SS_tot
#        R²aj   = 1 - (1 - R²) * (n - 1) / (n - p - 1)
#      donde n = tamaño del test y p = número de predictores (3).
# El objetivo es verificar que el modelo mantiene su capacidad
# predictiva en datos no vistos, independientemente de la partición.

set.seed(2026)                              # semilla madre → reproducibilidad
semillas  <- sample(1:10000000, size = 100) # 100 semillas de prueba
p_pred    <- 3                              # número de predictores del modelo final
tabla_cv  <- data.frame()

for (i in seq_along(semillas)) {

  set.seed(semillas[i])

  # ── Partición 80 / 20 ────────────────────────────────────────
  idx_train <- sample(seq_len(nrow(bd)), size = floor(0.80 * nrow(bd)))
  train_cv  <- bd[ idx_train, ]
  test_cv   <- bd[-idx_train, ]
  n_test    <- nrow(test_cv)

  # ── Entrenar modelo en el 80% ────────────────────────────────
  mod_cv <- lm(cbind(VOCs, acidez, TDS) ~
                 altitud + horas_sol + mat_organica,
               data = train_cv)

  # ── Predecir en el 20% (datos no vistos) ─────────────────────
  preds_cv <- predict(mod_cv,
                      newdata = test_cv[, c("altitud","horas_sol","mat_organica")])

  # ── MAPE en el conjunto de prueba ─────────────────────────────
  mv <- mape(preds_cv[, "VOCs"],   test_cv$VOCs)
  ma <- mape(preds_cv[, "acidez"], test_cv$acidez)
  mt <- mape(preds_cv[, "TDS"],    test_cv$TDS)

  # ── R²aj manual sobre el conjunto de prueba ──────────────────
  # Se calcula para cada Y por separado, usando los residuos del test.
  r2aj_manual <- function(y_real, y_pred, n, p) {
    ss_res <- sum((y_real - y_pred)^2)
    ss_tot <- sum((y_real - mean(y_real))^2)
    r2     <- 1 - ss_res / ss_tot
    r2aj   <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
    return(r2aj * 100)   # en porcentaje, igual que la sección 11
  }

  r2v <- r2aj_manual(test_cv$VOCs,   preds_cv[, "VOCs"],   n_test, p_pred)
  r2a <- r2aj_manual(test_cv$acidez, preds_cv[, "acidez"], n_test, p_pred)
  r2t <- r2aj_manual(test_cv$TDS,    preds_cv[, "TDS"],    n_test, p_pred)

  tabla_cv <- rbind(tabla_cv, data.frame(
    Semilla     = semillas[i],
    R2aj_VOCs   = round(r2v, 3),
    R2aj_Acidez = round(r2a, 3),
    R2aj_TDS    = round(r2t, 3),
    MAPE_VOCs   = round(mv,  3),
    MAPE_Acidez = round(ma,  3),
    MAPE_TDS    = round(mt,  3)
  ))
}

# ── Primeras 20 semillas ──────────────────────────────────────
cat("\n── Validación Cruzada (80/20) — 20 primeras de las 100 semillas ─\n")
print(head(tabla_cv, 20), row.names = FALSE)

# ── Resumen estadístico de las 100 semillas ───────────────────
cat("\n── Resumen de MAPE y R²aj en validación cruzada ─────────\n")
resumen_cv <- data.frame(
  Metrica = c("MAPE VOCs (%)", "MAPE Acidez (%)", "MAPE TDS (%)",
              "R²aj VOCs (%)", "R²aj Acidez (%)", "R²aj TDS (%)"),
  Media   = round(c(mean(tabla_cv$MAPE_VOCs),   mean(tabla_cv$MAPE_Acidez),
                    mean(tabla_cv$MAPE_TDS),
                    mean(tabla_cv$R2aj_VOCs),    mean(tabla_cv$R2aj_Acidez),
                    mean(tabla_cv$R2aj_TDS)), 3),
  SD      = round(c(sd(tabla_cv$MAPE_VOCs),     sd(tabla_cv$MAPE_Acidez),
                    sd(tabla_cv$MAPE_TDS),
                    sd(tabla_cv$R2aj_VOCs),      sd(tabla_cv$R2aj_Acidez),
                    sd(tabla_cv$R2aj_TDS)), 3),
  Min     = round(c(min(tabla_cv$MAPE_VOCs),    min(tabla_cv$MAPE_Acidez),
                    min(tabla_cv$MAPE_TDS),
                    min(tabla_cv$R2aj_VOCs),     min(tabla_cv$R2aj_Acidez),
                    min(tabla_cv$R2aj_TDS)), 3),
  Max     = round(c(max(tabla_cv$MAPE_VOCs),    max(tabla_cv$MAPE_Acidez),
                    max(tabla_cv$MAPE_TDS),
                    max(tabla_cv$R2aj_VOCs),     max(tabla_cv$R2aj_Acidez),
                    max(tabla_cv$R2aj_TDS)), 3)
)
print(resumen_cv, row.names = FALSE)
# Se busca que los R²aj se mantengan próximos a los obtenidos en muestra
# completa (sección 11) y que los MAPE sean estables y bajos entre semillas.
# Alta variabilidad entre semillas indicaría sobreajuste; baja variabilidad
# confirma que el modelo generaliza bien a datos no vistos.
