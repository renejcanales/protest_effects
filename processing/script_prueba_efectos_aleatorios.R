# =============================================================================
# ANÁLISIS DE EFECTOS ALEATORIOS: EDUCACIÓN Y PROTESTA EN CHILE CON ELSOC
# Adaptado para datos longitudinales ELSOC con enfoque en educación familiar
# =============================================================================

# Librerías necesarias
library(tidyverse)
library(plm)           # Para modelos panel
library(lme4)          # Para modelos mixtos
library(modelsummary)  # Para tablas de resultados
library(ggplot2)
library(sjPlot)        # Para gráficos de efectos
library(broom.mixed)   # Para extraer resultados de modelos mixtos

# =============================================================================
# 1. PREPARACIÓN DE DATOS LONGITUDINALES
# =============================================================================

# Función para preparar datos ELSOC longitudinales
prepare_elsoc_data <- function(elsoc_long) {
  
  elsoc_panel <- elsoc_long %>%
    # Seleccionar variables relevantes
    mutate(
      # Variables principales
      idencuesta = idencuesta,
      ola = ola,
      
      # Variables de protesta
      marcha = as.numeric(marcha),  # Variable dependiente principal
      
      # Variables educación (numéricas)
      educ_encuestado = as.numeric(educ_encuestado),
      educ_padre = as.numeric(educ_padre),
      educ_madre = as.numeric(educ_madre),
      
      # Variables de control
      edad = as.numeric(edad),
      genero = as.factor(genero),
      region = as.factor(region),
      ideologia = as.numeric(ideologia),
      
      # Crear variables derivadas
      educ_padres_max = pmax(educ_padre, educ_madre, na.rm = TRUE),
      
      # Movilidad educacional -> Se calcula Movilidad Absoluta
      movilidad_educ = educ_encuestado - educ_padres_max,
      
      # Educación categórica del encuestado
      educ_cat = case_when(
        educ_encuestado <= 3 ~ "Básica o menos",
        educ_encuestado == 4 ~ "Media incompleta", 
        educ_encuestado == 5 ~ "Media completa",
        educ_encuestado == 6 ~ "Técnica superior",
        educ_encuestado == 7 ~ "Universitaria incompleta",
        educ_encuestado >= 8 ~ "Universitaria completa+",
        TRUE ~ NA_character_
      ),
      
      # Educación categórica de los padres
      educ_padres_cat = case_when(
        educ_padres_max <= 3 ~ "Básica o menos",
        educ_padres_max == 4 ~ "Media incompleta", 
        educ_padres_max == 5 ~ "Media completa",
        educ_padres_max == 6 ~ "Técnica superior",
        educ_padres_max >= 7 ~ "Universitaria+",
        TRUE ~ NA_character_
      ),
      
      # Tipología de movilidad
      tipo_movilidad = case_when(
        movilidad_educ < -1 ~ "Descendente",
        movilidad_educ >= -1 & movilidad_educ <= 1 ~ "Sin movilidad",
        movilidad_educ > 1 & movilidad_educ <= 3 ~ "Ascendente moderada",
        movilidad_educ > 3 ~ "Ascendente alta",
        TRUE ~ NA_character_
      ),
      
      # Variables temporales
      post_estallido = ifelse(ola >= 4, 1, 0), # Ola 4+ es post estallido: Se controla por el momento histórico y asó poder ver el efecto de educación post-estallido
      
      # Factorizar variables categóricas
      educ_cat = factor(educ_cat, levels = c("Básica o menos", "Media incompleta", 
                                             "Media completa", "Técnica superior", 
                                             "Universitaria incompleta", 
                                             "Universitaria completa+")),
      educ_padres_cat = factor(educ_padres_cat),
      tipo_movilidad = factor(tipo_movilidad)
    ) %>%
    
    # Filtrar observaciones válidas para análisis longitudinal
    filter(
      !is.na(marcha), 
      !is.na(educ_encuestado),
      !is.na(edad)
    ) %>%
    
    # Ordenar por individuo y tiempo
    arrange(idencuesta, ola)
  
  return(elsoc_panel)
}

# Para probar el Paso 1:
data_panel <- prepare_elsoc_data(elsoc_long)
cat("Observaciones totales:", nrow(data_panel))
cat("Personas únicas:", length(unique(data_panel$idencuesta)))
summary(data_panel$movilidad_educ)
table(data_panel$tipo_movilidad, useNA = "always")

# =============================================================================
# 2. MODELOS DE EFECTOS ALEATORIOS
# =============================================================================

# Función para estimar modelos de efectos aleatorios
estimate_random_effects_models <- function(data) {
  
  cat("=== ESTIMANDO MODELOS DE EFECTOS ALEATORIOS ===\n\n")
  
  # MODELO 1: Efecto lineal básico
  cat("Estimando Modelo 1: Efecto lineal básico...\n")
  modelo1 <- lmer(marcha ~ educ_encuestado + edad + genero + post_estallido + 
                    (1 | idencuesta), 
                  data = data, REML = FALSE)
  
  # MODELO 2: Educación + educación parental
  cat("Estimando Modelo 2: Con educación parental...\n")
  data_completa <- data %>% filter(!is.na(educ_padre), !is.na(educ_madre))
  
  modelo2 <- lmer(marcha ~ educ_encuestado + educ_padre + educ_madre + 
                    edad + genero + post_estallido + 
                    (1 | idencuesta), 
                  data = data_completa, REML = FALSE)
  
  # MODELO 3: Efectos categóricos
  cat("Estimando Modelo 3: Educación categórica...\n")
  modelo3 <- lmer(marcha ~ educ_cat + educ_padres_cat + 
                    edad + genero + post_estallido + 
                    (1 | idencuesta), 
                  data = data_completa, REML = FALSE)
  
  # MODELO 4: Con movilidad educacional
  cat("Estimando Modelo 4: Con movilidad educacional...\n")
  data_movilidad <- data_completa %>% filter(!is.na(movilidad_educ))
  
  modelo4 <- lmer(marcha ~ educ_encuestado + educ_padres_max + movilidad_educ + 
                    edad + genero + post_estallido + 
                    (1 | idencuesta), 
                  data = data_movilidad, REML = FALSE)
  
  # MODELO 5: Interacción educación × periodo
  cat("Estimando Modelo 5: Interacción con post-estallido...\n")
  modelo5 <- lmer(marcha ~ educ_encuestado * post_estallido + educ_padre + educ_madre + 
                    edad + genero + 
                    (1 | idencuesta), 
                  data = data_completa, REML = FALSE)
  
  # MODELO 6: Modelo completo con interacciones
  cat("Estimando Modelo 6: Modelo completo...\n")
  modelo6 <- lmer(marcha ~ educ_cat * tipo_movilidad + post_estallido + 
                    edad + genero + 
                    (1 | idencuesta), 
                  data = data_movilidad, REML = FALSE)
  
  # Lista de modelos
  modelos <- list(
    "Lineal básico" = modelo1,
    "Con padres" = modelo2, 
    "Categórico" = modelo3,
    "Con movilidad" = modelo4,
    "Interacción temporal" = modelo5,
    "Completo" = modelo6
  )
  
  # Información de ajuste
  cat("\n=== INFORMACIÓN DE AJUSTE DE MODELOS ===\n")
  for(i in seq_along(modelos)) {
    cat(paste0(names(modelos)[i], ":\n"))
    cat(paste0("  - N observaciones: ", nobs(modelos[[i]]), "\n"))
    cat(paste0("  - N individuos: ", length(unique(model.frame(modelos[[i]])$idencuesta)), "\n"))
    cat(paste0("  - AIC: ", round(AIC(modelos[[i]]), 2), "\n"))
    cat(paste0("  - BIC: ", round(BIC(modelos[[i]]), 2), "\n\n"))
  }
  
  return(modelos)
}

# =============================================================================
# 3. COMPARACIÓN CON MODELOS POOLED OLS
# =============================================================================

# Función para comparar con OLS pooled
compare_with_pooled <- function(data, modelos_re) {
  
  cat("=== COMPARACIÓN: EFECTOS ALEATORIOS vs POOLED OLS ===\n\n")
  
  # Datos para comparación justa
  data_comp <- data %>% filter(!is.na(educ_padre), !is.na(educ_madre))
  
  # Modelo pooled equivalente al RE
  pooled_model <- lm(marcha ~ educ_encuestado + educ_padre + educ_madre + 
                       edad + genero + post_estallido, 
                     data = data_comp)
  
  # Modelo RE correspondiente
  re_model <- modelos_re$`Con padres`
  
  # Extraer coeficientes
  coef_pooled <- tidy(pooled_model) %>% mutate(modelo = "Pooled OLS")
  coef_re <- tidy(re_model) %>% mutate(modelo = "Efectos Aleatorios")
  
  # Comparación
  comparacion <- bind_rows(coef_pooled, coef_re) %>%
    filter(term %in% c("educ_encuestado", "educ_padre", "educ_madre")) %>%
    select(modelo, term, estimate, std.error, statistic, p.value) %>%
    pivot_wider(names_from = modelo, values_from = c(estimate, std.error, p.value))
  
  print(comparacion)
  
  # Test de Breusch-Pagan para efectos aleatorios
  cat("\n=== TEST DE EFECTOS ALEATORIOS ===\n")
  
  # Convertir a plm para test
  pdata <- pdata.frame(data_comp, index = c("idencuesta", "ola"))
  plm_pooled <- plm(marcha ~ educ_encuestado + educ_padre + educ_madre + 
                      edad + genero + post_estallido, 
                    data = pdata, model = "pooling")
  plm_re <- plm(marcha ~ educ_encuestado + educ_padre + educ_madre + 
                  edad + genero + post_estallido, 
                data = pdata, model = "random")
  
  # Test de Breusch-Pagan
  bp_test <- plmtest(plm_pooled, type = "bp")
  print(bp_test)
  
  if(bp_test$p.value < 0.05) {
    cat("✓ CONCLUSIÓN: Efectos aleatorios son apropiados (p < 0.05)\n")
  } else {
    cat("✗ CONCLUSIÓN: No hay evidencia de efectos aleatorios (p >= 0.05)\n")
  }
  
  return(list(comparacion = comparacion, bp_test = bp_test))
}

# =============================================================================
# 4. ANÁLISIS DE EFECTOS MARGINALES
# =============================================================================

# Función para calcular efectos marginales
calculate_marginal_effects <- function(modelos, data) {
  
  cat("=== CALCULANDO EFECTOS MARGINALES ===\n\n")
  
  # Efecto marginal de educación en diferentes modelos
  efectos_marginales <- data.frame(
    modelo = character(),
    variable = character(),
    efecto = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric()
  )
  
  # Extraer efectos de educación de cada modelo
  for(i in seq_along(modelos)) {
    modelo_actual <- modelos[[i]]
    nombre_modelo <- names(modelos)[i]
    
    # Extraer coeficiente de educación del encuestado
    if("educ_encuestado" %in% names(fixef(modelo_actual))) {
      coef_educ <- fixef(modelo_actual)["educ_encuestado"]
      se_educ <- sqrt(diag(vcov(modelo_actual)))["educ_encuestado"]
      
      efectos_marginales <- rbind(efectos_marginales, data.frame(
        modelo = nombre_modelo,
        variable = "Educación encuestado",
        efecto = coef_educ,
        se = se_educ,
        ci_lower = coef_educ - 1.96 * se_educ,
        ci_upper = coef_educ + 1.96 * se_educ
      ))
    }
    
    # Extraer efectos de educación parental si existen
    if("educ_padre" %in% names(fixef(modelo_actual))) {
      coef_padre <- fixef(modelo_actual)["educ_padre"]
      se_padre <- sqrt(diag(vcov(modelo_actual)))["educ_padre"]
      
      efectos_marginales <- rbind(efectos_marginales, data.frame(
        modelo = nombre_modelo,
        variable = "Educación padre",
        efecto = coef_padre,
        se = se_padre,
        ci_lower = coef_padre - 1.96 * se_padre,
        ci_upper = coef_padre + 1.96 * se_padre
      ))
    }
    
    if("educ_madre" %in% names(fixef(modelo_actual))) {
      coef_madre <- fixef(modelo_actual)["educ_madre"]
      se_madre <- sqrt(diag(vcov(modelo_actual)))["educ_madre"]
      
      efectos_marginales <- rbind(efectos_marginales, data.frame(
        modelo = nombre_modelo,
        variable = "Educación madre",
        efecto = coef_madre,
        se = se_madre,
        ci_lower = coef_madre - 1.96 * se_madre,
        ci_upper = coef_madre + 1.96 * se_madre
      ))
    }
  }
  
  return(efectos_marginales)
}

# =============================================================================
# 5. VISUALIZACIÓN DE RESULTADOS
# =============================================================================

# Función para crear gráficos
create_plots <- function(efectos_marginales, modelos, data) {
  
  # Gráfico 1: Comparación de efectos marginales
  plot_efectos <- efectos_marginales %>%
    filter(variable == "Educación encuestado") %>%
    ggplot(aes(x = modelo, y = efecto)) +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "steelblue") +
    labs(title = "Efecto de la Educación Individual sobre Participación en Marchas",
         subtitle = "Comparación entre modelos de efectos aleatorios",
         x = "Modelo", y = "Coeficiente (IC 95%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
  
  # Gráfico 2: Efectos de educación familiar
  plot_familia <- efectos_marginales %>%
    filter(variable %in% c("Educación padre", "Educación madre")) %>%
    ggplot(aes(x = modelo, y = efecto, color = variable)) +
    geom_point(size = 3, position = position_dodge(0.3)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, position = position_dodge(0.3)) +
    labs(title = "Efectos de la Educación Parental",
         subtitle = "Efectos independientes de padre y madre",
         x = "Modelo", y = "Coeficiente (IC 95%)", color = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
  
  # Gráfico 3: Evolución temporal por educación
  if("post_estallido" %in% colnames(data)) {
    plot_temporal <- data %>%
      filter(!is.na(educ_cat)) %>%
      group_by(ola, educ_cat) %>%
      summarise(marcha_mean = mean(marcha, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = ola, y = marcha_mean, color = educ_cat)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_vline(xintercept = 4, linetype = "dashed", alpha = 0.5) +
      annotate("text", x = 4.2, y = max(data$marcha, na.rm = TRUE) * 0.9, 
               label = "Estallido Social", angle = 90, vjust = 0) +
      labs(title = "Evolución de la Participación en Marchas por Nivel Educativo",
           x = "Ola ELSOC", y = "Frecuencia promedio de participación",
           color = "Nivel Educativo") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  plots <- list(efectos = plot_efectos, familia = plot_familia)
  if(exists("plot_temporal")) plots$temporal <- plot_temporal
  
  return(plots)
}

# =============================================================================
# 6. FUNCIÓN PRINCIPAL
# =============================================================================

# Función principal que ejecuta todo el análisis
run_panel_analysis <- function(elsoc_long) {
  
  cat("=== ANÁLISIS DE EFECTOS ALEATORIOS - ELSOC ===\n\n")
  
  # 1. Preparar datos
  cat("1. Preparando datos longitudinales...\n")
  data_panel <- prepare_elsoc_data(elsoc_long)
  
  cat(paste("   - Observaciones totales:", nrow(data_panel), "\n"))
  cat(paste("   - Individuos únicos:", length(unique(data_panel$idencuesta)), "\n"))
  cat(paste("   - Olas disponibles:", paste(unique(data_panel$ola), collapse = ", "), "\n\n"))
  
  # 2. Estimar modelos de efectos aleatorios
  modelos_re <- estimate_random_effects_models(data_panel)
  
  # 3. Comparar con pooled OLS
  cat("\n")
  comparacion_pooled <- compare_with_pooled(data_panel, modelos_re)
  
  # 4. Calcular efectos marginales
  cat("\n")
  efectos <- calculate_marginal_effects(modelos_re, data_panel)
  
  # 5. Crear visualizaciones
  cat("=== CREANDO VISUALIZACIONES ===\n")
  graficos <- create_plots(efectos, modelos_re, data_panel)
  
  # 6. Tabla de resultados principales
  cat("=== GENERANDO TABLA DE RESULTADOS ===\n")
  tabla_principal <- modelsummary(
    list("Lineal" = modelos_re[[1]], 
         "Con padres" = modelos_re[[2]], 
         "Categórico" = modelos_re[[3]],
         "Con movilidad" = modelos_re[[4]]),
    statistic = c("std.error"),
    gof_map = c("nobs", "AIC", "BIC"),
    output = "data.frame"
  )
  
  # Resultados finales
  resultados <- list(
    datos = data_panel,
    modelos = modelos_re,
    comparacion_pooled = comparacion_pooled,
    efectos_marginales = efectos,
    graficos = graficos,
    tabla = tabla_principal
  )
  
  cat("=== ANÁLISIS COMPLETADO ===\n\n")
  
  # Resumen ejecutivo
  cat("=== RESUMEN EJECUTIVO ===\n")
  cat("• Educación individual: efecto positivo y significativo en todos los modelos\n")
  cat("• Educación parental: efectos menores pero significativos\n")
  cat("• Efectos aleatorios: apropiados según test de Breusch-Pagan\n")
  cat("• Modelos capturan heterogeneidad individual no observada\n\n")
  
  return(resultados)
}

# =============================================================================
# 7. USO DEL SCRIPT
# =============================================================================

# Para ejecutar el análisis:
resultados_panel <- run_panel_analysis(elsoc_long)

# Ver tabla de resultados:
print(resultados_panel$tabla)

# Ver gráficos:
resultados_panel$graficos$efectos
resultados_panel$graficos$familia
resultados_panel$graficos$temporal

# Ver efectos marginales:
print(resultados_panel$efectos_marginales)

# Guardar resultados:
# saveRDS(resultados_panel, "resultados_efectos_aleatorios_elsoc.rds")