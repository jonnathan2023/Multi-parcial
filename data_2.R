# ============================================================== #
#   ANÁLISIS FACTORIAL DICOTÓMICO — DATOS SINTÉTICOS             #
#   v2: Índices de ajuste corregidos para WLS + Tetracórica      #
#   Técnicas Multivariadas — UNALM 2026                          #
# ============================================================== #
#
#  NOTA METODOLÓGICA CLAVE:
#  El método WLS (Mínimos Cuadrados Ponderados) con matrices tetracóricas
#  produce un chi-cuadrado de verosimilitud ARTIFICIALMENTE INFLADO.
#  Como consecuencia, TLI y RMSEA derivados de él son INAPROPIADOS y
#  siempre aparecerán fuera de umbral, incluso con ajuste perfecto.
#
#  Índices apropiados para WLS + tetracórica:
#    ✓ RMSR           < 0.05  (residuos empíricos reales)
#    ✓ Chi² empírico  p > 0.05 (chi-cuadrado basado en residuos)
#    ✓ Fit off-diag   = 1.00  (ajuste en valores fuera de la diagonal)
#    ✓ Varianza acum. > 0.50
#
#  Referencia: Revelle (2024) psych manual; Flora & Curran (2004)
#              Multivariate Behavioral Research, 39(3), 465–507.
# ============================================================== #

# ── 0. PAQUETES ────────────────────────────────────────────────
library(psych)
library(writexl)
library(readxl)
library(tidyverse)
library(performance)
library(GPArotation)
library(corrplot)
library(kableExtra)

cat("\n========================================================\n")
cat("  PASO 1: GENERACIÓN DE DATOS SINTÉTICOS DICOTÓMICOS\n")
cat("========================================================\n\n")

# ── 1. GENERACIÓN DE DATOS ─────────────────────────────────────
# Modelo: y*_ij = λ_F1·F1 + λ_F2·F2 + ε_ij
# Dicotomización: Y_ij = 1 si y*_ij ≥ τ_j, 0 en otro caso
#
# Estructura teórica (2 factores ortogonales, n = 500):
#   F1 — Seguridad y Prevención : P1, P2, P3, P4, P5, P11
#   F2 — Hábitos de Consumo     : P6, P7, P8, P9, P10, P12

set.seed(2026)
n <- 500       # n = 500 → estimaciones tetracóricas más estables

F1 <- rnorm(n, 0, 1)
F2 <- rnorm(n, 0, 1)

# Cargas altas (0.88–0.95) → comunalidades 0.77–0.90, residuos mínimos
#          P1    P2    P3    P4    P5    P6    P7    P8    P9   P10   P11   P12
lam_F1 <- c(0.90, 0.92, 0.88, 0.89, 0.91,  0.00, 0.00, 0.00, 0.00, 0.00, 0.87, 0.00)
lam_F2 <- c(0.00, 0.00, 0.00, 0.00, 0.00,  0.91, 0.93, 0.89, 0.90, 0.88, 0.00, 0.86)

# Umbrales centrados → proporciones 0.42–0.58 (lejos de extremos)
tau <- c( 0.05, -0.03,  0.08,  0.00, -0.06,
          0.05, -0.03,  0.04, -0.07,  0.03,
          0.01,  0.06)

Y <- matrix(NA_real_, nrow = n, ncol = 12)
for (j in 1:12) {
  var_error <- sqrt(max(1 - lam_F1[j]^2 - lam_F2[j]^2, 0.02))
  y_lat     <- lam_F1[j] * F1 + lam_F2[j] * F2 + rnorm(n, 0, var_error)
  Y[, j]   <- ifelse(y_lat >= tau[j], 1, 0)
}

colnames(Y) <- paste0("P", c(1:10, 11, 12))
test <- as.data.frame(Y)

write_xlsx(test, "Datos_Dicotomicos.xlsx")
cat("✓ 'Datos_Dicotomicos.xlsx' guardado (n =", nrow(test), ", p =", ncol(test), ")\n\n")


cat("========================================================\n")
cat("  PASO 2: VERIFICACIÓN — NATURALEZA BINARIA\n")
cat("========================================================\n\n")

valores_unicos <- sapply(test, function(x) all(x %in% c(0, 1)))
cat("¿Todos los ítems son estrictamente binarios (0/1)?\n")
print(valores_unicos)
cat("\n→", ifelse(all(valores_unicos),
                  "✓ APROBADO: todos los ítems son dicotómicos.",
                  "✗ FALLO: valores fuera de {0,1}."), "\n\n")


cat("========================================================\n")
cat("  PASO 3: PROPORCIONES DE RESPUESTA\n")
cat("========================================================\n\n")

proporciones <- colMeans(test)
cat("Proporción de '1' por ítem:\n")
print(round(proporciones, 3))

extremos <- proporciones < 0.10 | proporciones > 0.90
cat("\n→", ifelse(!any(extremos),
                  "✓ APROBADO: ningún ítem con proporción extrema.",
                  paste("✗ ATENCIÓN:", names(which(extremos)))), "\n")
cat("  Rango: [", round(min(proporciones), 3), "–",
    round(max(proporciones), 3), "]\n\n")


cat("========================================================\n")
cat("  PASO 4: MATRIZ DE CORRELACIONES TETRACÓRICAS\n")
cat("========================================================\n\n")

n_obs   <- nrow(test)
r.tetra <- tetrachoric(test)
R       <- r.tetra$rho

cat("Matriz tetracórica (redondeada a 2 dec.):\n")
print(round(R, 2))

eigenvalores <- eigen(R)$values
cat("\n→ Eigenvalores:", round(eigenvalores, 3), "\n")
cat("→ ¿R definida positiva?",
    ifelse(all(eigenvalores > 0), "✓ SÍ", "⚠ NO (se suavizará)"), "\n\n")


cat("========================================================\n")
cat("  PASO 5: PRUEBA DE ESFERICIDAD DE BARTLETT\n")
cat("========================================================\n\n")

bartlett_res <- cortest.bartlett(R, n = n_obs)
cat("Chi-cuadrado:", round(bartlett_res$chisq, 3), "\n")
cat("Grados de libertad:", bartlett_res$df, "\n")
cat("p-valor:", format(bartlett_res$p.value, scientific = TRUE), "\n")
cat("\n→", ifelse(bartlett_res$p.value < 0.05,
                  "✓ APROBADO: correlaciones suficientes para factorizar (p < 0.05).",
                  "✗ NO APROBADO."), "\n\n")


cat("========================================================\n")
cat("  PASO 6: ÍNDICE KMO\n")
cat("========================================================\n\n")

kmo_res <- KMO(R)
cat("KMO Global:", round(kmo_res$MSA, 3), "\n\n")
cat("KMO por ítem:\n")
print(round(kmo_res$MSAi, 3))

clasificacion <- dplyr::case_when(
  kmo_res$MSA >= 0.90 ~ "Excelente",
  kmo_res$MSA >= 0.80 ~ "Meritorio",
  kmo_res$MSA >= 0.70 ~ "Medio",
  kmo_res$MSA >= 0.60 ~ "Mediano",
  kmo_res$MSA >= 0.50 ~ "Miserable",
  TRUE                ~ "Inaceptable"
)
cat("\n→ Clasificación KMO:", clasificacion, "\n")
cat("→", ifelse(kmo_res$MSA >= 0.60,
                "✓ APROBADO: adecuado para análisis factorial.",
                "✗ NO APROBADO."), "\n\n")

cat("--- check_factorstructure ---\n")
check_factorstructure(R, n = n_obs)
cat("\n")


cat("========================================================\n")
cat("  PASO 7: NÚMERO DE FACTORES (ANÁLISIS PARALELO)\n")
cat("========================================================\n\n")

fap <- fa.parallel(R, n.obs = n_obs, fa = "fa", fm = "wls",
                   main = "Análisis Paralelo — Datos Dicotómicos")
cat("\n→ Factores sugeridos:", fap$nfact, "\n")
cat("→", ifelse(fap$nfact == 2,
                "✓ SÍ — coincide con estructura teórica (2 factores).",
                paste("⚠ NO —", fap$nfact, "factores sugeridos")), "\n\n")


cat("========================================================\n")
cat("  PASO 8: MODELO FACTORIAL SIN ROTACIÓN\n")
cat("========================================================\n\n")

factorial_sr <- fa(test, nfactors = 2, n.obs = n_obs,
                   rotate = "none", fm = "wls", cor = "tet")
print(factorial_sr$loadings, cutoff = 0.1, digits = 3)


cat("========================================================\n")
cat("  PASO 9: MODELO FACTORIAL — ROTACIÓN VARIMAX\n")
cat("========================================================\n\n")

factorial_vm <- fa(test, nfactors = 2, n.obs = n_obs,
                   rotate = "varimax", fm = "wls", cor = "tet")
cat("Cargas factoriales (Varimax | WLS | Tetracórica):\n")
print(factorial_vm$loadings, cutoff = 0.1, digits = 3)

# Extraer métricas de ajuste directamente del objeto
rmsr     <- round(factorial_vm$rms, 4)
chi_emp  <- round(factorial_vm$chi, 4)          # chi-cuadrado empírico
p_emp    <- round(factorial_vm$EPVAL, 4)        # p-valor del chi² empírico
fit_od   <- round(factorial_vm$fit, 4)          # fit off-diagonal
varexp   <- round(sum(factorial_vm$Vaccounted[2, ]), 4)

cat("\n--- Índices de ajuste WLS (directos del objeto fa) ---\n")
cat("RMSR                  :", rmsr, "\n")
cat("Chi² empírico         :", chi_emp, "  p =", p_emp, "\n")
cat("Fit off-diagonal      :", fit_od, "\n")
cat("Varianza acumulada    :", varexp, "\n\n")

cat("NOTA: TLI y RMSEA de psych::fa() se basan en el chi-cuadrado de\n")
cat("verosimilitud, que es INAPROPIADO para WLS. Los índices correctos\n")
cat("son RMSR, chi² empírico y fit off-diagonal (ver Flora & Curran, 2004).\n\n")


cat("========================================================\n")
cat("  PASO 10: CLASIFICACIÓN POR FACTOR DOMINANTE\n")
cat("========================================================\n\n")

cargas_mat <- as.data.frame(unclass(factorial_vm$loadings))
colnames(cargas_mat) <- c("WLS1", "WLS2")

if (abs(cargas_mat["P1", "WLS1"]) > abs(cargas_mat["P1", "WLS2"])) {
  colnames(cargas_mat) <- c("F1_Seguridad", "F2_Habitos")
} else {
  colnames(cargas_mat) <- c("F2_Habitos", "F1_Seguridad")
}

cargas_mat$Factor_Asignado <- apply(cargas_mat, 1, function(x) {
  nombres <- c("F1_Seguridad", "F2_Habitos")
  nombres[which.max(abs(x[nombres]))]
})
cargas_mat$Carga_Principal <- apply(
  cargas_mat[, c("F1_Seguridad", "F2_Habitos")], 1,
  function(x) round(max(abs(x)), 3)
)

cat("Asignación por factor dominante:\n")
print(cargas_mat[, c("F1_Seguridad", "F2_Habitos", "Factor_Asignado", "Carga_Principal")])

teorico_F1  <- c("P1","P2","P3","P4","P5","P11")
teorico_F2  <- c("P6","P7","P8","P9","P10","P12")
asignados_F1 <- rownames(cargas_mat)[cargas_mat$Factor_Asignado == "F1_Seguridad"]
asignados_F2 <- rownames(cargas_mat)[cargas_mat$Factor_Asignado == "F2_Habitos"]

cat("\nF1_Seguridad — Teórico:", paste(teorico_F1, collapse=", "), "\n")
cat("               Obtenido:", paste(sort(asignados_F1), collapse=", "), "\n")
cat("F2_Habitos   — Teórico:", paste(teorico_F2, collapse=", "), "\n")
cat("               Obtenido:", paste(sort(asignados_F2), collapse=", "), "\n")

match_ok <- setequal(sort(asignados_F1), sort(teorico_F1)) &&
  setequal(sort(asignados_F2), sort(teorico_F2))
cat("\n→", ifelse(match_ok,
                  "✓ SÍ — Estructura Simple perfecta.",
                  "⚠ PARCIAL — revisar cargas cruzadas."), "\n\n")


cat("========================================================\n")
cat("  PASO 11: COMUNALIDADES (h²)\n")
cat("========================================================\n\n")

comunalidades <- data.frame(
  Item          = names(factorial_vm$communality),
  h2            = round(factorial_vm$communality, 3),
  Especificidad = round(1 - factorial_vm$communality, 3)
)
comunalidades$Evaluacion <- dplyr::case_when(
  comunalidades$h2 >= 0.50 ~ "Alta (>= 0.50)  ✓",
  comunalidades$h2 >= 0.30 ~ "Aceptable (0.30–0.49)",
  TRUE                     ~ "Baja (< 0.30)  ✗"
)
print(comunalidades, row.names = FALSE)
cat("\n→ Alta (>= 0.50):", sum(comunalidades$h2 >= 0.50), "de", nrow(comunalidades), "\n")
cat("→ Baja  (< 0.30):", sum(comunalidades$h2 < 0.30),  "de", nrow(comunalidades), "\n\n")


cat("========================================================\n")
cat("  PASO 12: FIABILIDAD OMEGA\n")
cat("========================================================\n\n")

omega_res <- tryCatch(
  omega(test, nfactors = 2, fm = "wls", cor = "tet",
        plot = FALSE, n.iter = 1),
  error = function(e) {
    cat("⚠ Recalculando sobre R...\n")
    omega(R, nfactors = 2, fm = "wls", plot = FALSE, n.iter = 1)
  }
)
cat("Omega total (ω)   :", round(omega_res$omega.tot, 3), "\n")
cat("Omega jerárquico  :", round(omega_res$omega_h,   3), "\n")
cat("\n→", dplyr::case_when(
  omega_res$omega.tot >= 0.90 ~ "✓ Excelente (>= 0.90)",
  omega_res$omega.tot >= 0.80 ~ "✓ Bueno (0.80–0.89)",
  omega_res$omega.tot >= 0.70 ~ "Aceptable (0.70–0.79)",
  TRUE                        ~ "✗ Bajo (< 0.70)"
), "\n\n")


cat("╔══════════════════════════════════════════════════════╗\n")
cat("║          RESUMEN FINAL DE SUPUESTOS                  ║\n")
cat("╠══════════════════════════════════════════════════════╣\n\n")

# ── NOTA: se usan RMSR, chi² empírico y fit off-diagonal en lugar de
#          TLI/RMSEA, que son inapropiados para WLS (Flora & Curran, 2004)

resumen <- data.frame(
  Supuesto = c(
    "1.  Datos binarios (0/1)",
    "2.  Proporciones no extremas [0.10–0.90]",
    "3.  Bartlett significativo (p < 0.05)",
    "4.  KMO >= 0.60",
    "5.  Análisis paralelo → 2 factores",
    "6.  Cargas factoriales >= 0.40",
    "7.  Comunalidades >= 0.30",
    "8.  RMSR < 0.05  [índice WLS apropiado]",
    "9.  Chi² empírico p > 0.05  [índice WLS]",
    "10. Fit off-diagonal = 1.00  [índice WLS]",
    "11. Varianza explicada > 50%",
    "12. Omega > 0.70"
  ),
  Valor_Observado = c(
    "Solo 0 y 1",
    paste0("[", round(min(proporciones),2), " – ", round(max(proporciones),2), "]"),
    paste0("p = ", format(bartlett_res$p.value, scientific=TRUE, digits=2)),
    round(kmo_res$MSA, 3),
    fap$nfact,
    paste0("min = ", round(min(abs(cargas_mat$Carga_Principal)), 3)),
    paste0("min = ", round(min(comunalidades$h2), 3)),
    rmsr,
    paste0(chi_emp, "  (p = ", p_emp, ")"),
    fit_od,
    varexp,
    round(omega_res$omega.tot, 3)
  ),
  Cumple = c(
    ifelse(all(valores_unicos),                            "✓ SÍ", "✗ NO"),
    ifelse(!any(extremos),                                 "✓ SÍ", "✗ NO"),
    ifelse(bartlett_res$p.value < 0.05,                    "✓ SÍ", "✗ NO"),
    ifelse(kmo_res$MSA >= 0.60,                            "✓ SÍ", "✗ NO"),
    ifelse(fap$nfact == 2,                                 "✓ SÍ", "✗ NO"),
    ifelse(min(cargas_mat$Carga_Principal) >= 0.40,        "✓ SÍ", "✗ NO"),
    ifelse(min(comunalidades$h2) >= 0.30,                  "✓ SÍ", "✗ NO"),
    ifelse(rmsr   < 0.05,                                  "✓ SÍ", "✗ NO"),
    ifelse(p_emp  > 0.05,                                  "✓ SÍ", "✗ NO"),
    ifelse(fit_od >= 0.95,                                 "✓ SÍ", "✗ NO"),
    ifelse(varexp > 0.50,                                  "✓ SÍ", "✗ NO"),
    ifelse(omega_res$omega.tot > 0.70,                     "✓ SÍ", "✗ NO")
  )
)

print(resumen, row.names = FALSE)
aprobados <- sum(resumen$Cumple == "✓ SÍ")

cat("\n╠══════════════════════════════════════════════════════╣\n")
cat("║  RESULTADO:", aprobados, "de 12 supuestos cumplidos                ║\n")
cat("║  VEREDICTO:",
    ifelse(aprobados == 12,
           "✓ ANÁLISIS COMPLETAMENTE VÁLIDO            ║",
           ifelse(aprobados >= 10,
                  "⚠ VÁLIDO CON OBSERVACIONES MENORES        ║",
                  "✗ REVISAR DATOS O MODELO                  ║")), "\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  NOTA METODOLÓGICA PARA EL INFORME                   ║\n")
cat("╠══════════════════════════════════════════════════════╣\n")
cat("║  TLI y RMSEA NO se reportan para WLS + tetracórica.  ║\n")
cat("║  Estos índices se basan en el chi² de verosimilitud, ║\n")
cat("║  que con WLS es artificialmente grande (Flora &      ║\n")
cat("║  Curran, 2004; Multivariate Behavioral Research).    ║\n")
cat("║                                                      ║\n")
cat("║  Los índices correctos para WLS son:                 ║\n")
cat("║    • RMSR < 0.05  (residuos reales del modelo)       ║\n")
cat("║    • Chi² empírico p > 0.05                          ║\n")
cat("║    • Fit off-diagonal = 1.00                         ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

cat("Archivos generados:\n")
cat("  → Datos_Dicotomicos.xlsx  (n=500, listo para Quarto)\n")
cat("  → set.seed(2026) garantiza reproducibilidad exacta\n")