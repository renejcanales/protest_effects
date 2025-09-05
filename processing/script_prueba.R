# =============================================================================
# ANÁLISIS: EDUCACIÓN Y PROTESTA EN CHILE CON ELSOC
# Implementación de hipótesis sobre efectos no lineales y movilidad educacional
# =============================================================================

# Librerías necesarias
library(tidyverse)
library(plm)           # Para modelos panel
library(lme4)          # Para modelos mixtos
library(fixest)        # Para efectos fijos/aleatorios eficientes
library(modelsummary)  # Para tablas de resultados
library(ggplot2)
library(sjPlot)        # Para gráficos de efectos
library(haven)         # Para leer archivos .dta/.sav

# =============================================================================
# 1. PREPARACIÓN DE DATOS
# =============================================================================

# Cargar datos ELSOC (ajustar ruta según tu archivo)
load("~/GitHub/protest_effects/input/data/raw/elsoc_long_2016_2023.RData")
# O si es .sav: elsoc <- read_sav("ruta/archivo.sav")

# Función para preparar los datos
prepare_data <- function(elsoc_long_2016_2023) {
  
  elsoc_clean <- elsoc_raw %>%
    # Seleccionar variables relevantes (ajustar nombres según ELSOC)
    select(
      # Identificadores
      idencuesta, ola,
      # Variables de protesta (ajustar nombres reales)
      protesta_participa,   # Variable principal de protesta
      protesta_frecuencia,  # Si existe
      # Variables educación
      educ_encuestado,      # Educación del encuestado
      educ_padre,           # Educación del padre
      educ_madre,           # Educación de la madre
      # Controles
      edad, sexo, region, ingresos_hogar, ideologia,
      # Peso muestral si existe
      ponderador
    ) %>%
    
    # Crear variables de educación categóricas
    mutate(
      # Educación encuestado (ajustar según codificación ELSOC)
      educ_cat = case_when(
        educ_encuestado <= 3 ~ "Sin estudios/Básica",
        educ_encuestado == 4 ~ "Media incompleta", 
        educ_encuestado == 5 ~ "Media completa",
        educ_encuestado == 6 ~ "Técnica superior",
        educ_encuestado == 7 ~ "Universitaria incompleta",
        educ_encuestado == 8 ~ "Universitaria completa",
        educ_encuestado >= 9 ~ "Postgrado",
        TRUE ~ NA_character_
      ),
      
      # Educación máxima de los padres
      educ_padres = pmax(educ_padre, educ_madre, na.rm = TRUE),
      educ_padres_cat = case_when(
        educ_padres <= 3 ~ "Sin estudios/Básica",
        educ_padres == 4 ~ "Media incompleta", 
        educ_padres == 5 ~ "Media completa",
        educ_padres == 6 ~ "Técnica superior",
        educ_padres == 7 ~ "Universitaria incompleta",
        educ_padres >= 8 ~ "Universitaria completa+",
        TRUE ~ NA_character_
      ),
      
      # Variable de movilidad educacional (diferencia en años de educación)
      movilidad_educ = educ_encuestado - educ_padres,
      
      # Tipología de movilidad
      tipo_movilidad = case_when(
        movilidad_educ < -1 ~ "Movilidad descendente",
        movilidad_educ >= -1 & movilidad_educ <= 1 ~ "Sin movilidad",
        movilidad_educ > 1 & movilidad_educ <= 3 ~ "Movilidad moderada",
        movilidad_educ > 3 ~ "Movilidad alta",
        TRUE ~ NA_character_
      ),
      
      # Variables temporales
      post_estallido = ifelse(ola >= 4, 1, 0), # Ajustar según ola del estallido
      
      # Factorizar variables categóricas
      educ_cat = factor(educ_cat, levels = c("Sin estudios/Básica", "Media incompleta", 
                                             "Media completa", "Técnica superior", 
                                             "Universitaria incompleta", "Universitaria completa", 
                                             "Postgrado")),
      
      # Variable dependiente (ajustar según datos reales)
      protesta = as.numeric(protesta_participa)
    ) %>%
    
    # Filtrar observaciones válidas
    filter(!is.na(protesta), !is.na(educ_cat), !is.na(educ_padres)) %>%
    
    # Crear panel data
    arrange(idencuesta, ola)
  
  return(elsoc_clean)
}

# Aplicar función de limpieza
# elsoc <- prepare_data(elsoc_raw)

# =============================================================================
# 2. ANÁLISIS DESCRIPTIVO
# =============================================================================

# Función para análisis descriptivo
descriptive_analysis <- function(data) {
  
  # Estadísticas descriptivas por educación
  desc_educ <- data %>%
    group_by(educ_cat) %>%
    summarise(
      n = n(),
      protesta_mean = mean(protesta, na.rm = TRUE),
      protesta_se = sd(protesta, na.rm = TRUE) / sqrt(n),
      .groups = 'drop'
    )
  
  print("=== PARTICIPACIÓN EN PROTESTA POR NIVEL EDUCATIVO ===")
  print(desc_educ)
  
  # Gráfico de tendencias por educación
  plot_educ <- ggplot(desc_educ, aes(x = educ_cat, y = protesta_mean)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = protesta_mean - 1.96*protesta_se, 
                      ymax = protesta_mean + 1.96*protesta_se), 
                  width = 0.2) +
    labs(title = "Participación en Protesta por Nivel Educativo",
         x = "Nivel Educativo", y = "Proporción que Protesta") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_educ)
  
  # Análisis de movilidad educacional
  movilidad_desc <- data %>%
    group_by(tipo_movilidad) %>%
    summarise(
      n = n(),
      protesta_mean = mean(protesta, na.rm = TRUE),
      educ_mean = mean(educ_encuestado, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print("=== PARTICIPACIÓN EN PROTESTA POR TIPO DE MOVILIDAD ===")
  print(movilidad_desc)
  
  # Tabla cruzada educación × movilidad
  crosstab <- data %>%
    group_by(educ_cat, tipo_movilidad) %>%
    summarise(
      n = n(),
      protesta_rate = mean(protesta, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = tipo_movilidad, values_from = c(n, protesta_rate))
  
  print("=== TABLA CRUZADA: EDUCACIÓN × MOVILIDAD ===")
  print(crosstab)
  
  return(list(desc_educ = desc_educ, movilidad_desc = movilidad_desc, 
              plot_educ = plot_educ, crosstab = crosstab))
}

# Ejecutar análisis descriptivo
# desc_results <- descriptive_analysis(elsoc)

# =============================================================================
# 3. MODELOS ECONOMÉTRICOS
# =============================================================================

# Función para modelos principales
run_models <- function(data) {
  
  # Preparar datos para panel
  pdata <- pdata.frame(data, index = c("idencuesta", "ola"))
  
  # MODELO 1: Efecto lineal básico (baseline)
  modelo1 <- plm(protesta ~ educ_encuestado + edad + factor(sexo) + 
                   factor(post_estallido), 
                 data = pdata, model = "random")
  
  # MODELO 2: Efecto no lineal (categórico) - HIPÓTESIS 1
  modelo2 <- plm(protesta ~ educ_cat + edad + factor(sexo) + 
                   factor(post_estallido), 
                 data = pdata, model = "random")
  
  # MODELO 3: Con movilidad educacional - HIPÓTESIS 2A
  modelo3 <- plm(protesta ~ educ_cat + movilidad_educ + edad + factor(sexo) + 
                   factor(post_estallido), 
                 data = pdata, model = "random")
  
  # MODELO 4: Interacción educación × movilidad - HIPÓTESIS 2B
  modelo4 <- plm(protesta ~ educ_cat * movilidad_educ + edad + factor(sexo) + 
                   factor(post_estallido), 
                 data = pdata, model = "random")
  
  # MODELO 5: Tipología de movilidad
  modelo5 <- plm(protesta ~ educ_cat * factor(tipo_movilidad) + edad + factor(sexo) + 
                   factor(post_estallido), 
                 data = pdata, model = "random")
  
  # Crear lista de modelos
  modelos <- list(
    "Lineal" = modelo1,
    "No lineal" = modelo2, 
    "Con movilidad" = modelo3,
    "Interacción" = modelo4,
    "Tipología" = modelo5
  )
  
  return(modelos)
}

# Función para tests de robustez
robustness_tests <- function(data) {
  
  pdata <- pdata.frame(data, index = c("idencuesta", "ola"))
  
  # Test 1: Efectos fijos vs aleatorios
  modelo_fe <- plm(protesta ~ educ_cat + movilidad_educ + edad + factor(sexo) + 
                     factor(post_estallido), 
                   data = pdata, model = "within")
  
  modelo_re <- plm(protesta ~ educ_cat + movilidad_educ + edad + factor(sexo) + 
                     factor(post_estallido), 
                   data = pdata, model = "random")
  
  # Hausman test
  hausman_test <- phtest(modelo_fe, modelo_re)
  print("=== HAUSMAN TEST ===")
  print(hausman_test)
  
  # Test 2: Modelo con controles adicionales (si disponibles)
  if("ingresos_hogar" %in% colnames(data) & "ideologia" %in% colnames(data)) {
    modelo_completo <- plm(protesta ~ educ_cat * movilidad_educ + edad + factor(sexo) + 
                             ingresos_hogar + ideologia + factor(post_estallido), 
                           data = pdata, model = "random")
  }
  
  # Test 3: Submuestra post-estallido
  data_post <- data %>% filter(post_estallido == 1)
  if(nrow(data_post) > 100) {
    pdata_post <- pdata.frame(data_post, index = c("idencuesta", "ola"))
    modelo_post <- plm(protesta ~ educ_cat * movilidad_educ + edad + factor(sexo), 
                       data = pdata_post, model = "random")
  }
  
  robustez <- list(
    hausman = hausman_test,
    fe = modelo_fe,
    re = modelo_re
  )
  
  if(exists("modelo_completo")) robustez$completo <- modelo_completo
  if(exists("modelo_post")) robustez$post_estallido <- modelo_post
  
  return(robustez)
}

# =============================================================================
# 4. RESULTADOS Y VISUALIZACIÓN
# =============================================================================

# Función para crear tablas de resultados
create_results_table <- function(modelos) {
  
  # Tabla principal
  tabla_principal <- modelsummary(
    modelos[1:4],  # Primeros 4 modelos
    output = "gt",
    statistic = c("std.error", "p.value"),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    coef_map = c(
      "educ_encuestado" = "Educación (años)",
      "educ_catMedia completa" = "Media completa",
      "educ_catTécnica superior" = "Técnica superior", 
      "educ_catUniversitaria incompleta" = "Univ. incompleta",
      "educ_catUniversitaria completa" = "Univ. completa",
      "educ_catPostgrado" = "Postgrado",
      "movilidad_educ" = "Movilidad educacional",
      "edad" = "Edad",
      "factor(sexo)2" = "Mujer",
      "factor(post_estallido)1" = "Post-estallido"
    )
  )
  
  return(tabla_principal)
}

# Función para gráficos de efectos
plot_effects <- function(modelos, data) {
  
  # Gráfico 1: Efectos marginales por nivel educativo
  plot_marginal <- plot_model(modelos$`No lineal`, type = "pred", 
                              terms = "educ_cat") +
    labs(title = "Efectos Marginales por Nivel Educativo",
         x = "Nivel Educativo", y = "Probabilidad de Protestar") +
    theme_minimal()
  
  # Gráfico 2: Efectos de interacción (si el modelo converge)
  if("Interacción" %in% names(modelos)) {
    plot_interaction <- plot_model(modelos$Interacción, type = "int") +
      labs(title = "Interacción: Educación × Movilidad") +
      theme_minimal()
  }
  
  # Gráfico 3: Tendencias temporales por educación
  plot_trends <- data %>%
    group_by(ola, educ_cat) %>%
    summarise(protesta_mean = mean(protesta, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(educ_cat)) %>%
    ggplot(aes(x = ola, y = protesta_mean, color = educ_cat)) +
    geom_line(size = 1) +
    geom_point() +
    labs(title = "Evolución de la Participación en Protesta por Educación",
         x = "Ola ELSOC", y = "Proporción que Protesta",
         color = "Nivel Educativo") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  plots <- list(marginal = plot_marginal, trends = plot_trends)
  if(exists("plot_interaction")) plots$interaction <- plot_interaction
  
  return(plots)
}

# =============================================================================
# 5. FUNCIÓN PRINCIPAL DE ANÁLISIS
# =============================================================================

# Función que ejecuta todo el análisis
run_full_analysis <- function(elsoc_data) {
  
  cat("=== INICIANDO ANÁLISIS COMPLETO ===\n\n")
  
  # 1. Preparar datos
  cat("1. Preparando datos...\n")
  data_clean <- prepare_data(elsoc_data)
  cat(paste("   - Observaciones finales:", nrow(data_clean), "\n"))
  cat(paste("   - Individuos únicos:", length(unique(data_clean$idencuesta)), "\n\n"))
  
  # 2. Análisis descriptivo
  cat("2. Ejecutando análisis descriptivo...\n")
  descriptivos <- descriptive_analysis(data_clean)
  
  # 3. Modelos principales
  cat("3. Estimando modelos...\n")
  modelos <- run_models(data_clean)
  
  # 4. Tests de robustez
  cat("4. Tests de robustez...\n")
  robustez <- robustness_tests(data_clean)
  
  # 5. Resultados
  cat("5. Generando resultados finales...\n")
  tabla_resultados <- create_results_table(modelos)
  graficos <- plot_effects(modelos, data_clean)
  
  # Retornar resultados
  resultados <- list(
    datos = data_clean,
    descriptivos = descriptivos,
    modelos = modelos,
    robustez = robustez,
    tabla = tabla_resultados,
    graficos = graficos
  )
  
  cat("=== ANÁLISIS COMPLETADO ===\n")
  return(resultados)
}

# =============================================================================
# 6. INTERPRETACIÓN DE HIPÓTESIS
# =============================================================================

# Función para interpretar resultados
interpretar_hipotesis <- function(resultados) {
  
  cat("=== INTERPRETACIÓN DE HIPÓTESIS ===\n\n")
  
  # Hipótesis 1: Efecto no lineal
  modelo_nolineal <- resultados$modelos$`No lineal`
  coefs_educ <- summary(modelo_nolineal)$coefficients
  
  cat("HIPÓTESIS 1 - EFECTO NO LINEAL:\n")
  cat("¿Técnica superior protesta menos que media completa y universitaria?\n")
  
  if("educ_catTécnica superior" %in% rownames(coefs_educ)) {
    coef_tecnica <- coefs_educ["educ_catTécnica superior", "Estimate"]
    coef_univ <- coefs_educ["educ_catUniversitaria completa", "Estimate"]
    
    if(coef_tecnica < coef_univ) {
      cat("✓ CONFIRMADA: Técnica superior tiene menor efecto que universitaria\n")
    } else {
      cat("✗ NO CONFIRMADA: Técnica superior no tiene menor efecto\n")
    }
  }
  
  # Hipótesis 2: Movilidad educacional
  if("Interacción" %in% names(resultados$modelos)) {
    modelo_int <- resultados$modelos$Interacción
    cat("\nHIPÓTESIS 2 - MOVILIDAD EDUCACIONAL:\n")
    cat("¿La movilidad modera el efecto de la educación?\n")
    
    # Aquí podrías agregar tests específicos de las interacciones
    cat("Ver tabla de resultados para efectos de interacción\n")
  }
  
  cat("\n=== RECOMENDACIONES ADICIONALES ===\n")
  cat("1. Revisar significancia estadística de los coeficientes clave\n")
  cat("2. Analizar magnitudes de efectos (efectos marginales)\n") 
  cat("3. Considerar análisis por subgrupos (edad, región)\n")
  cat("4. Evaluar robustez con diferentes especificaciones\n")
}

# =============================================================================
# EJEMPLO DE USO
# =============================================================================

# Para ejecutar el análisis completo:
# 
# 1. Cargar datos ELSOC
elsoc_raw <- read_dta("tu_archivo_elsoc.dta")
# 
# 2. Ejecutar análisis completo  
resultados <- run_full_analysis(elsoc_raw)
# 
# 3. Ver tabla de resultados
print(resultados$tabla)
# 
# 4. Ver gráficos
resultados$graficos$marginal
resultados$graficos$trends
# 
# 5. Interpretar hipótesis
interpretar_hipotesis(resultados)
# 
# 6. Guardar resultados
saveRDS(resultados, "resultados_educacion_protesta.rds")