#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for research thesis
# Author: René Canales          
# Overview: Tesis Data       
# Date: 18-08-2025            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(plm, # Modelos Panel
               lme4,
               ggplot2,
               sjlabelled,
               interactions, # Gráficos de Interacción
               marginaleffects, # Effectos Marginales
               lmtest, # Test de DIagnósticos
               sandwich, # Errores Robustos
               multcomp, # Contrastes post-hoc
               stargazer, # Tablas
               sjmisc,
               psych, # Descriptivos
               texreg, # Tablas de Regresión
               sjPlot,
               skimr,
               here,
               tidyr,
               naniar,
               labelled,
               dplyr,
               broom,
               broom.mixed,
               remotes)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------


load(here("input/data/raw/elsoc_long_2016_2023.RData"))

elsoc_long <- elsoc_long_2016_2023

rm(elsoc_long_2016_2023)

glimpse(elsoc_long)

# 2.2. Data Structure ---------------------------------------------------

str(elsoc_long)

head(elsoc_long)

names(elsoc_long)

# 3. Processing -----------------------------------------------------------

# mutate ----

elsoc_long[elsoc_long ==-999] <- NA
elsoc_long[elsoc_long ==-888] <- NA
elsoc_long[elsoc_long ==-777] <- NA
elsoc_long[elsoc_long ==-666] <- NA

elsoc_long_select <- elsoc_long %>%
  dplyr::select(idencuesta, 
         tipo_atricion,
         estrato,
         segmento,
         ola,
         genero = m0_sexo,
         edad = m0_edad,
         educ_encuestado = m01,
         educ_padre = m27,
         educ_madre = m28,
         conf_gob = c05_01,
         conf_part = c05_02,
         conf_cong = c05_07,
         efic_voto = c10_01,
         efic_result = c10_02,
         efic_expr = c10_03,
         voto_deber = c10_01, 
         voto_influye = c10_02, 
         voto_expresion = c10_03, 
         interes_politica = c13, 
         hablar_politica = c14_01,
         satisf_democracia = c01,
         firma_peti = c08_01,
         asist_marcha = c08_02,
         part_huelga = c08_03,
         part_cacerol = c08_05,
         miembro_pp = c12_03,
         perc_sub_clase = c33,
         tipo_empleo = m07,
         cantidad_trabajadores = m05,
         satis_ingreso = m16,
         ideologia = c15,
         recomp_esfuer = c18_09,
         recomp_talent = c18_10,
         justicia_pensiones = d02_01,
         justicia_educacion = d02_02, 
         justicia_salud = d02_03,
         violencia_carabineros_marchas = f05_03,
         violencia_carabineros_tomas = f05_04,
         violencia_trabajadores = f05_06,
         violencia_estudiantes = f05_07,
         violencia_inmobiliario = f05_09,
         violencia_transporte = f05_10,
         violencia_locales = f05_11,
         ciuo08_m03,
         ciuo88_m03) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# ==============================================================================
# LIMPIEZA Y CONSTRUCCIÓN DE VARIABLES - ELSOC
# Proyecto: Educación, Movilidad Social y Protesta
# ==============================================================================

# ==============================================================================
# 1. VERIFICACIÓN INICIAL
# ==============================================================================


cat("Dimensiones originales:", dim(elsoc_long_select), "\n")
cat("Número de individuos únicos:", n_distinct(elsoc_long_select$idencuesta), "\n")
cat("Olas disponibles:", sort(unique(elsoc_long_select$ola)), "\n")

# ==============================================================================
# 2. EXPLORACIÓN INICIAL
# ==============================================================================

cat("\n=== VERIFICAR CODIFICACIÓN ANTES DE CONTINUAR ===\n")
cat("Educación encuestado:\n")
print(table(elsoc_long_select$educ_encuestado, useNA = "always"))
cat("\nAsistencia a marchas:\n")
print(table(elsoc_long_select$asist_marcha, useNA = "always"))
cat("\nParticipación en huelgas:\n")
print(table(elsoc_long_select$part_huelga, useNA = "always"))

# ==============================================================================
# 3. LIMPIEZA Y RECODIFICACIÓN
# ==============================================================================

elsoc_clean <- elsoc_long_select

# ----- PASO 1: RECODIFICACIÓN DE VALORES PERDIDOS -----
# Se reemplazan los códigos de valores perdidos (-999, -888, -777) por NA
elsoc_clean <- elsoc_clean %>%
  mutate(across(everything(), ~na_if(., -999))) %>% # No sabe
  mutate(across(everything(), ~na_if(., -888))) %>% # No responde
  mutate(across(everything(), ~na_if(., -777)))     # No aplica

# ----- PASO 2: VARIABLE DEPENDIENTE - ÍNDICE DE PROTESTA -----

# Parte A: Crear las variables base
elsoc_clean <- elsoc_clean %>%
  mutate(
    # Variables de frecuencia (escala original 1-5) - PARA EL ÍNDICE
    firma_peticion_freq = firma_peti,
    asist_marcha_freq = asist_marcha,
    part_huelga_freq = part_huelga,
    part_cacerolazo_freq = part_cacerol,
    
    # Variable binaria solo para marcha (usada en protesta_dummy)
    asist_marcha_bin = if_else(asist_marcha >= 2, 1, 0, missing = 0),
    
    # Flag para saber si la información de cacerolazos está disponible
    cacerolazo_disponible = !is.na(part_cacerol)
  )

# Parte B: Usar las variables recién creadas para construir los índices
elsoc_clean <- elsoc_clean %>%
  mutate(
    # --- ÍNDICE DE PROTESTA (PROMEDIO DE FRECUENCIAS 1-5) ---
    # Índice principal: promedio de marcha, huelga y petición
    protesta_index = rowMeans(cbind(firma_peticion_freq, asist_marcha_freq, part_huelga_freq), na.rm = FALSE),
    
    # Índice completo: incluye también cacerolazos
    protesta_index_completo = rowMeans(cbind(firma_peticion_freq, asist_marcha_freq, part_huelga_freq, part_cacerolazo_freq), na.rm = FALSE),
    
    # --- ÍNDICES DE INTENSIDAD (SUMA DE FRECUENCIAS) ---
    protesta_intensidad_suma = rowSums(cbind(firma_peticion_freq, asist_marcha_freq, part_huelga_freq), na.rm = FALSE),
    protesta_intensidad_suma_completa = rowSums(cbind(firma_peticion_freq, asist_marcha_freq, part_huelga_freq, part_cacerolazo_freq), na.rm = FALSE),
    
    # --- VARIABLE DICOTÓMICA (DUMMY) ---
    # Dummy: participó en marcha (solo asistencia a marchas)
    protesta_dummy = asist_marcha_bin,
    
    # --- FLAG DE DATOS FALTANTES PARA EL ÍNDICE PRINCIPAL ---
    protesta_missing = is.na(protesta_index)
  )

# ----- PASO 3: EDUCACIÓN DEL ENCUESTADO -----
# Se crean categorías, factores y años de estudio para el encuestado

# PASO 3.1: Imputar la máxima educación alcanzada por el encuestado a lo largo de las olas
elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%
  mutate(educ_encuestado = max(educ_encuestado, na.rm = TRUE)) %>%
  mutate(educ_encuestado = if_else(is.infinite(educ_encuestado), NA_real_, educ_encuestado)) %>%
  ungroup()

# PASO 3.2: Crear categorías educacionales colapsadas
elsoc_clean <- elsoc_clean %>%
  mutate(
    educ_cat = case_when(
      educ_encuestado %in% c(1:5) ~ "Media completa o menos",  # Colapso: media incompleta + media completa (Se colapsan para efectos analíticos)
      educ_encuestado == 6 ~ "Técnica superior incompleta",
      educ_encuestado == 7 ~ "Técnica superior completa",
      educ_encuestado == 8 ~ "Universitaria incompleta",
      educ_encuestado %in% c(9, 10) ~ "Universitaria completa",  # Colapso: univ completa + postgrado (N muy pequeño)
      TRUE ~ NA_character_
    ),
    educ_cat_factor = factor(
      educ_cat,
      levels = c("Media completa o menos", "Técnica superior incompleta",
                 "Técnica superior completa", "Universitaria incompleta", "Universitaria completa"), 
      ordered = TRUE
    ),
    # Años de estudio aproximados según la codificación CASEN
    educ_years = case_when(
      educ_encuestado == 1 ~ 0, educ_encuestado == 2 ~ 4, educ_encuestado == 3 ~ 8,
      educ_encuestado == 4 ~ 10, educ_encuestado == 5 ~ 12, educ_encuestado == 6 ~ 13,
      educ_encuestado == 7 ~ 14, educ_encuestado == 8 ~ 14, educ_encuestado == 9 ~ 17,
      educ_encuestado == 10 ~ 19, TRUE ~ NA_real_
    ),
    univ_dummy = if_else(educ_encuestado %in% c(8, 9, 10), 1, 0),
    tecnica_dummy = if_else(educ_encuestado %in% c(6, 7), 1, 0),
    superior_dummy = if_else(educ_encuestado >= 6, 1, 0)
  )

# ----- PASO 4: IMPUTACIÓN Y EDUCACIÓN PARENTAL -----
# Como la educación parental solo se pregunta en ola 1, 
# se "arrastra" ese valor a todas las olas del mismo individuo

elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%
  # Llenar los valores de educación parental con el valor de la primera ola (imputación simple)
  fill(educ_padre, educ_madre, .direction = "downup") %>%
  ungroup()

# Calcular años de estudio y categorías de los padres
elsoc_clean <- elsoc_clean %>%
  mutate(
    # Años de estudio del padre
    educ_padre_years = case_when(
      educ_padre == 1 ~ 0, educ_padre == 2 ~ 4, educ_padre == 3 ~ 8, educ_padre == 4 ~ 10,
      educ_padre == 5 ~ 12, educ_padre == 6 ~ 13, educ_padre == 7 ~ 14, educ_padre == 8 ~ 14,
      educ_padre == 9 ~ 17, educ_padre == 10 ~ 19, TRUE ~ NA_real_
    ),
    # Categoría educacional del padre (colapsada como la del encuestado)
    educ_padre_cat = case_when(
      educ_padre %in% c(1:5) ~ "Media completa o menos",
      educ_padre == 6 ~ "Técnica superior incompleta",
      educ_padre == 7 ~ "Técnica superior completa",
      educ_padre == 8 ~ "Universitaria incompleta",
      educ_padre %in% c(9, 10) ~ "Universitaria completa",
      TRUE ~ NA_character_
    ),
    # Años de estudio de la madre
    educ_madre_years = case_when(
      educ_madre == 1 ~ 0, educ_madre == 2 ~ 4, educ_madre == 3 ~ 8, educ_madre == 4 ~ 10,
      educ_madre == 5 ~ 12, educ_madre == 6 ~ 13, educ_madre == 7 ~ 14, educ_madre == 8 ~ 14,
      educ_madre == 9 ~ 17, educ_madre == 10 ~ 19, TRUE ~ NA_real_
    ),
    # Categoría educacional de la madre (colapsada como la del encuestado)
    educ_madre_cat = case_when(
      educ_madre %in% c(1:5) ~ "Media completa o menos",
      educ_madre == 6 ~ "Técnica superior incompleta",
      educ_madre == 7 ~ "Técnica superior completa",
      educ_madre == 8 ~ "Universitaria incompleta",
      educ_madre %in% c(9, 10) ~ "Universitaria completa",
      TRUE ~ NA_character_
    ),
    # Máximo de años de educación parental
    educ_parental_max = pmax(educ_padre_years, educ_madre_years, na.rm = TRUE),
    educ_parental_max = if_else(is.na(educ_padre_years) & is.na(educ_madre_years), NA_real_, educ_parental_max),
    
    # Categoría del padre/madre con mayor educación (para análisis)
    educ_parental_cat_detallada = case_when(
      educ_parental_max <= 10 ~ "Media completa o menos",
      educ_parental_max %in% c(13, 14) ~ "Técnica superior",
      educ_parental_max >= 17 ~ "Universitaria completa",
      TRUE ~ NA_character_
    ),
    educ_parental_cat_detallada = factor(
      educ_parental_cat_detallada,
      levels = c("Media completa o menos", "Técnica superior", "Universitaria completa"),
      ordered = TRUE
    ),
    
    # Categoría simplificada en 4 niveles (mantener la original para continuidad)
    educ_parental_cat = factor(case_when(
      educ_parental_max < 12 ~ "Bajo",
      educ_parental_max == 12 ~ "Medio",
      educ_parental_max %in% 13:15 ~ "Medio-Alto",
      educ_parental_max >= 16 ~ "Alto",
      TRUE ~ NA_character_
    ), levels = c("Bajo", "Medio", "Medio-Alto", "Alto"), ordered = TRUE)
  )

# ----- PASO 5: MOVILIDAD EDUCACIONAL -----
# Se mide la diferencia entre la educación del encuestado y la de sus padres
elsoc_clean <- elsoc_clean %>%
  mutate(
    movilidad_years = educ_years - educ_parental_max,
    movilidad_cat = case_when(
      movilidad_years < -2 ~ "Descendente",
      movilidad_years >= -2 & movilidad_years <= 2 ~ "Sin movilidad",
      movilidad_years > 2 & movilidad_years <= 5 ~ "Ascendente moderada",
      movilidad_years > 5 ~ "Ascendente alta",
      TRUE ~ NA_character_
    ),
    movilidad_cat_factor = factor(
      movilidad_cat,
      levels = c("Descendente", "Sin movilidad", "Ascendente moderada", "Ascendente alta"),
      ordered = TRUE
    ),
    movilidad_asc_dummy = if_else(movilidad_years > 2, 1, 0),
    movilidad_asc_alta = if_else(movilidad_years > 5, 1, 0)
  )

# ----- PASO 6: VARIABLES DE CONTROL -----
# Se preparan variables sociodemográficas y de actitudes políticas
elsoc_clean <- elsoc_clean %>%
  mutate(
    edad_cuadratica = edad^2,
    mujer = if_else(genero == 2, 1, 0),
    ideologia_std = if_else(ideologia >= 0 & ideologia <= 10, ideologia, NA_real_),
    ideologia_cat = case_when(
      ideologia_std <= 3 ~ "Izquierda",
      ideologia_std <= 6 ~ "Centro",
      ideologia_std >= 7 ~ "Derecha",
      TRUE ~ NA_character_
    ),
    conf_instituciones = rowMeans(cbind(conf_gob, conf_part, conf_cong), na.rm = TRUE),
    eficacia_politica = rowMeans(cbind(efic_voto, efic_result, efic_expr), na.rm = TRUE)
  )

  # Imputar ideología hacia adelante y atrás
elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%
  fill(ideologia_std, .direction = "downup") %>%  # Imputar hacia adelante y atrás
  ungroup()

# ----- PASO 7: VARIABLES TEMPORALES -----
# Se crean variables relacionadas con el tiempo y contexto (Estallido Social)
elsoc_clean <- elsoc_clean %>%
  mutate(
    year = case_when(
      ola == 1 ~ 2016, ola == 2 ~ 2017, ola == 3 ~ 2018,
      ola == 4 ~ 2019, ola == 5 ~ 2021, ola == 6 ~ 2022,
      ola == 7 ~ 2023, TRUE ~ NA_real_
    ),
    post_estallido = if_else(year >= 2019, 1, 0),
    periodo = case_when(
      year <= 2018 ~ "Pre-estallido",
      year == 2019 ~ "Estallido",
      year >= 2021 ~ "Post-estallido",
      TRUE ~ NA_character_
    )
  )

# ----- PASO 8: VARIABLES MEDIADORAS -----
# Se crean índices de percepciones sobre justicia, meritocracia y estatus
elsoc_clean <- elsoc_clean %>%
  mutate(
    justicia_distributiva = rowMeans(cbind(justicia_pensiones, justicia_educacion, justicia_salud), na.rm = TRUE),
    meritocracia = rowMeans(cbind(recomp_esfuer, recomp_talent), na.rm = TRUE),
    clase_subjetiva = perc_sub_clase,
    satis_ingreso_std = satis_ingreso
  )

# ----- PASO 9: JUSTIFICACIÓN DE LA VIOLENCIA -----
# Se crean dos índices separados para distinguir tipos de violencia

elsoc_clean <- elsoc_clean %>%
  mutate(
    # ÍNDICE 1: Justificación de violencia ESTATAL (Carabineros)
    # Refleja actitudes hacia la represión policial en contextos de protesta
    justif_violencia_estatal = rowMeans(
      dplyr::select(., violencia_carabineros_marchas, violencia_carabineros_tomas), 
      na.rm = TRUE
    ),
    
    # ÍNDICE 2: Justificación de violencia en PROTESTAS (manifestantes)
    # Refleja actitudes hacia diferentes formas de violencia en movilizaciones
    justif_violencia_protesta = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes, 
                    violencia_inmobiliario, violencia_transporte, violencia_locales), 
      na.rm = TRUE
    ),
    
    # Variables dummy para análisis binarios
    justifica_violencia_estatal_dummy = if_else(justif_violencia_estatal > 1, 1, 0),
    justifica_violencia_protesta_dummy = if_else(justif_violencia_protesta > 1, 1, 0),
    
    # Flag de disponibilidad (estas variables no están en todas las olas)
    violencia_disponible = !is.na(violencia_carabineros_marchas) | !is.na(violencia_trabajadores)
  )

#----10. Recode Ocupación---------

remotes::install_github("DiogoFerrari/occupar")

elsoc_clean <- elsoc_clean %>%
  # 1. Aseguramos el orden cronológico
  arrange(idencuesta, ola) %>%
  
  # 2. Creamos la variable combinada (aún con NAs en olas 2, 4, 6)
  mutate(
    isco88 = case_when(
      ola == 1 ~ as.numeric(ciuo88_m03),
      ola %in% c(3, 5, 7) ~ occupar::isco08to88(ciuo08_m03),
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 3. ¡La solución! Rellenamos "hacia abajo"
  # Toma el último valor válido y lo arrastra a los NAs siguientes
  group_by(idencuesta) %>%
  fill(isco88, .direction = "down") %>%
  ungroup()

# 4. Verificación (Opcional pero recomendado)
# Comprueba si ahora sí hay datos en todas las olas
elsoc_clean %>%
  group_by(ola) %>%
  summarise(
    total_NAs = sum(is.na(isco88)),
    total_validos = sum(!is.na(isco88))
  )

# Tipo de empleo

library(sjPlot)
library(sjmisc) # Para set_labels y frq

# 1. Definir etiquetas (tu código)
labs_sj <- c(
  `1` = "Empleado u obrero en empresa privada",
  `2` = "Empleado u obrero del sector publico",
  `3` = "Miembro de las Fuerzas Armadas y de Orden", # Se filtrará
  `4` = "Patron/a o empleador/a",
  `5` = "Trabaja solo, no tiene empleados",
  `6` = "Familiar no remunerado", # Se filtrará
  `7` = "Servicio domestico"
)

# 2. Limpiar, IMPUTAR CON FILL, y crear selfemp_egp
elsoc_clean <- elsoc_clean %>%
  # Aseguramos orden cronológico (clave para fill)
  arrange(idencuesta, ola) %>%
  
  # Limpiamos la variable original (tu código)
  mutate(
    tipo_empleo = suppressWarnings(as.integer(tipo_empleo)),
    tipo_empleo = if_else(tipo_empleo %in% c(1, 2, 4, 5, 7), tipo_empleo, NA_integer_)
  ) %>%
  
  # *** LA SOLUCIÓN: Imputar con fill() ***
  # Reemplaza tu 'lag()' y 'coalesce()'
  group_by(idencuesta) %>%
  fill(tipo_empleo, .direction = "down") %>%
  ungroup() %>%
  
  # 3. Aplicar etiquetas y crear selfemp_egp (tu código, combinado)
  mutate(
    tipo_empleo = set_labels(tipo_empleo, labels = labs_sj),
    tipo_empleo = set_label(tipo_empleo, label = "Relación de empleo (1–7)"),
    
    selfemp_egp = case_when(
      tipo_empleo %in% c(4, 5) ~ 1,     # Patrón o cuenta propia
      tipo_empleo %in% c(1, 2, 7) ~ 0, # Empleado (priv/pub) o serv. dom.
      TRUE ~ NA_real_
    )
  )

# 4. Verificación (tu código)
# Revisa que 'selfemp_egp' y 'tipo_empleo' tengan datos en todas las olas
frq(elsoc_clean$selfemp_egp)

sjt.xtab(elsoc_clean$tipo_empleo, elsoc_clean$ola,
         show.col.prc = TRUE,
         var.labels = c("Relacion de empleo (Imputada)", "Ola"),
         show.summary = FALSE, 
         title = NULL)

elsoc_clean <- elsoc_clean %>%
  # 1. Aseguramos el orden (clave para fill)
  arrange(idencuesta, ola) %>%
  
  # 2. Creamos las variables (tu código)
  mutate(
    cantidad_trabajadores_rec = case_when(
      cantidad_trabajadores == 1 ~ 1,
      cantidad_trabajadores == 2 ~ 4,
      cantidad_trabajadores == 3 ~ 9,
      cantidad_trabajadores == 4 ~ 49,
      cantidad_trabajadores == 5 ~ 199,
      cantidad_trabajadores == 6 ~ 201,
      TRUE ~ NA_real_
    ),
    
    # Creamos nemploy_egp
    # OJO: selfemp_egp debe estar creado e imputado (Paso 2)
    nemploy_egp = if_else(selfemp_egp == 1 & cantidad_trabajadores_rec > 1, 
                          cantidad_trabajadores_rec, 0)
  ) %>%
  
  # 3. *** LA SOLUCIÓN: Imputar con fill() ***
  # Reemplaza tu bloque de 'lag()'
  group_by(idencuesta) %>%
  fill(nemploy_egp, .direction = "down") %>%
  ungroup()

# 4. Verificación (tu código)
# Revisa que 'nemploy_egp' tenga datos en todas las olas
frq(elsoc_clean$nemploy_egp)

sjt.xtab(elsoc_clean$nemploy_egp, elsoc_clean$ola,
         show.col.prc = TRUE,
         var.labels = c("N employees (Imputado)", "Ola"),
         show.summary = FALSE, 
         title = NULL)

# 5. Convertir ISCO-88 a EGP

elsoc_clean <- elsoc_clean %>% 
  mutate(egp = occupar::isco88toEGP(isco88 = isco88, 
                                    n.employees = nemploy_egp, 
                                    self.employed = selfemp_egp,
                                    n.classes = 7))

frq(elsoc_clean$egp)

library(stringr)

elsoc_clean <- elsoc_clean %>% 
  mutate(egp = factor(str_squish(as.character(egp))),
         egp3 = case_when(egp %in% c("I Service class I", 
                                     "II Service class II") ~ "Service class (I+II)",
                          egp %in% c("III Routine non-manual",
                                     "IV Self-employed") ~ "Intermediate class (III+IV)",
                          egp %in% c("V Manual supervisors/Lower grade technicians",
                                     "VI Skilled workers",
                                     "VII Unskilled workers/Farm labours") ~ "Working class (V+VI+VII)"),
         egp3 = factor(egp3, levels = c("Service class (I+II)",
                                        "Intermediate class (III+IV)",
                                        "Working class (V+VI+VII)")))


frq(elsoc_clean$egp3)

sjt.xtab(elsoc_clean$egp3,elsoc_clean$ola,
         show.col.prc=TRUE,
         var.labels=c("EGP","Ola"),
         show.summary=FALSE,         title=NULL)


# ==================================================================================
# 4. FILTROS Y RESTRICCIONES
# ==============================================================================

# PASO 4.1: Identificar individuos con al menos 3 olas
individuos_min_3_olas <- elsoc_clean %>%
  group_by(idencuesta) %>%
  summarise(n_olas = n()) %>%
  filter(n_olas >= 3) %>%
  pull(idencuesta)

cat("\n=== BALANCEO DEL PANEL ===\n")
cat("Individuos con al menos 3 olas:", length(individuos_min_3_olas), "\n")

# PASO 4.2: Aplicar filtros
elsoc_analisis <- elsoc_clean %>%
  filter(
    # Balanceo del panel: mínimo 3 olas por individuo
    idencuesta %in% individuos_min_3_olas,
    
    # Adultos en edad relevante
    edad >= 18 & edad <= 75,
    
    # Sin missing en variables clave
    !is.na(educ_encuestado),
    !is.na(protesta_index)
    
    # OPCIONAL: requerir info de padres
    # !is.na(educ_parental_max)
  )


cat("\n=== RESUMEN DE FILTROS ===\n")
cat("Casos originales:", nrow(elsoc_clean), "\n")
cat("Casos finales (panel balanceado):", nrow(elsoc_analisis), "\n")
cat("Porcentaje retenido:", 
    round(nrow(elsoc_analisis)/nrow(elsoc_clean)*100, 2), "%\n")
cat("Individuos únicos finales:", n_distinct(elsoc_analisis$idencuesta), "\n")

# ==============================================================================
# 4.3. VERIFICACIÓN DE BALANCEO DEL PANEL
# ==============================================================================

cat("\n=== VERIFICACIÓN DE BALANCEO ===\n")

# Casos por ola
casos_por_ola <- elsoc_analisis %>%
  group_by(ola) %>%
  summarise(n_casos = n()) %>%
  arrange(ola)

print(casos_por_ola)

# Distribución de olas por individuo
olas_por_individuo <- elsoc_analisis %>%
  group_by(idencuesta) %>%
  summarise(n_olas = n()) %>%
  count(n_olas, name = "n_individuos")

cat("\nDistribución de observaciones por individuo:\n")
print(olas_por_individuo)

# Verificar si está perfectamente balanceado
n_individuos <- n_distinct(elsoc_analisis$idencuesta)
n_olas_total <- n_distinct(elsoc_analisis$ola)
esperado_si_balanceado <- n_individuos * n_olas_total

cat("\nDiagnóstico de balanceo:\n")
cat("- Individuos únicos:", n_individuos, "\n")
cat("- Olas únicas:", n_olas_total, "\n")
cat("- Casos esperados si panel balanceado:", esperado_si_balanceado, "\n")
cat("- Casos observados:", nrow(elsoc_analisis), "\n")
cat("- ¿Panel balanceado?:", 
    ifelse(nrow(elsoc_analisis) == esperado_si_balanceado, "SÍ ✓", "NO ✗"), "\n")

# ==============================================================================
# 5. ANÁLISIS DE VALORES PERDIDOS
# ==============================================================================

missing_summary <- elsoc_analisis %>%
  dplyr::select(protesta_index, educ_years, educ_parental_max, movilidad_years,
         edad, ideologia_std) %>%
  miss_var_summary()
print(missing_summary)

# ==============================================================================
# 6. ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================

cat("\n=== DISTRIBUCIÓN PROTESTA ===\n")
print(table(elsoc_analisis$protesta_index, useNA = "always"))
cat("\n% Participación:", 
    round(mean(elsoc_analisis$protesta_dummy, na.rm = TRUE) * 100, 2), "%\n")

cat("\n=== DISTRIBUCIÓN EDUCACIÓN ===\n")
print(table(elsoc_analisis$educ_cat, useNA = "always"))

cat("\n=== DISTRIBUCIÓN MOVILIDAD ===\n")
print(table(elsoc_analisis$movilidad_cat, useNA = "always"))

cat("\n=== ESTADÍSTICAS NUMÉRICAS ===\n")
print(summary(elsoc_analisis %>% 
                dplyr::select(protesta_index, educ_years, movilidad_years, edad, protesta_dummy)))

# ==============================================================================
# 7. TABLAS CRUZADAS
# ==============================================================================

cat("\n=== EDUCACIÓN × PROTESTA ===\n")
tabla_educ <- elsoc_analisis %>%
  group_by(educ_cat) %>%
  summarise(
    n = n(),
    pct_protesta = mean(protesta_dummy, na.rm = TRUE) * 100,
    index_medio = mean(protesta_index, na.rm = TRUE)
  )
print(tabla_educ)

cat("\n=== MOVILIDAD × PROTESTA ===\n")
tabla_mov <- elsoc_analisis %>%
  group_by(movilidad_cat) %>%
  summarise(
    n = n(),
    pct_protesta = mean(protesta_dummy, na.rm = TRUE) * 100,
    index_medio = mean(protesta_index, na.rm = TRUE)
  )
print(tabla_mov)

# ==============================================================================
# 8. ESTRUCTURA DEL PANEL
# ==============================================================================

cat("\n=== ESTRUCTURA PANEL ===\n")
panel_structure <- elsoc_analisis %>%
  group_by(idencuesta) %>%
  summarise(n_olas = n()) %>%
  count(n_olas)
print(panel_structure)



# ==============================================================================
# 9. SELECCIONAR VARIABLES FINALES
# ==============================================================================

elsoc_final_2 <- elsoc_analisis %>%
  dplyr::select(
    # IDs
    idencuesta, ola, year, tipo_atricion,
    
    # VD - Formas individuales (frecuencia 1-5) - USADAS PARA ÍNDICE
    firma_peticion_freq, asist_marcha_freq,
    part_huelga_freq, part_cacerolazo_freq,
    
    # VD - Variable binaria auxiliar
    asist_marcha_bin,  # Solo para crear protesta_dummy
    
    # VD - Índices principales
    protesta_index,                      # PRINCIPAL: promedio marcha + huelga + petición
    protesta_index_completo,             # Incluye cacerolazos
    protesta_intensidad_suma,            # Suma de frecuencias (intensidad)
    protesta_intensidad_suma_completa,   # Suma incluyendo cacerolazos
    
    # VD - Dicotómicas
    protesta_dummy,              # PRINCIPAL DUMMY (basado en marcha)
    
    # Flags
    cacerolazo_disponible,
    protesta_missing,
    
    # VI principales
    educ_cat, educ_cat_factor, educ_years,
    univ_dummy, tecnica_dummy, superior_dummy,
    educ_parental_max, educ_parental_cat, educ_parental_cat_detallada,
    educ_padre_years, educ_padre_cat,
    educ_madre_years, educ_madre_cat,
    movilidad_years, movilidad_cat, movilidad_cat_factor,
    movilidad_asc_dummy, movilidad_asc_alta,
    
    # Controles
    edad, edad_cuadratica, mujer,
    ideologia_std, ideologia_cat,
    interes_politica,
    conf_instituciones, eficacia_politica, 
    egp, egp3,
    
    # Temporales
    post_estallido, periodo,
    
    # Mediadoras
    justicia_distributiva, meritocracia,
    clase_subjetiva, satis_ingreso_std,
    
    # Justificación de violencia
    violencia_trabajadores, violencia_estudiantes, 
    violencia_inmobiliario, violencia_transporte, violencia_locales, 
    violencia_carabineros_marchas, violencia_carabineros_tomas,
    justif_violencia_estatal, justif_violencia_protesta,
    justifica_violencia_estatal_dummy, justifica_violencia_protesta_dummy,
    violencia_disponible
  )

sjPlot::view_df(elsoc_final,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)

# ==============================================================================
# 10. MATRIZ ORIGEN-DESTINO Y GUARDAR BASE
# ==============================================================================

# Crear matriz de movilidad educacional origen-destino
elsoc_final_2 <- elsoc_final %>%
  mutate(
    # Clasificar educación del encuestado en 3 niveles
    educ_destino = case_when(
      educ_years < 12 ~ "Bajo",
      educ_years >= 12 & educ_years < 16 ~ "Medio",
      educ_years >= 16 ~ "Alto",
      TRUE ~ NA_character_
    ),
    educ_destino = factor(educ_destino, levels = c("Bajo", "Medio", "Alto"), ordered = TRUE),
    
    # Clasificar educación parental en 3 niveles
    educ_origen = case_when(
      educ_parental_max < 12 ~ "Bajo",
      educ_parental_max >= 12 & educ_parental_max < 16 ~ "Medio",
      educ_parental_max >= 16 ~ "Alto",
      TRUE ~ NA_character_
    ),
    educ_origen = factor(educ_origen, levels = c("Bajo", "Medio", "Alto"), ordered = TRUE),
    
    # Crear matriz de movilidad (9 categorías posibles)
    matriz_movilidad = interaction(educ_origen, educ_destino, sep = " → ")
  )

cat("\n=== DISTRIBUCIÓN MATRIZ ORIGEN-DESTINO ===\n")
print(table(elsoc_final$matriz_movilidad, useNA = "always"))

# Guardar base final
save(elsoc_final_2, file = here("input/data/proc/elsoc_final_2.RData"))

cat("\n=== BASE GUARDADA ===\n")
cat("Dimensiones finales:", dim(elsoc_final), "\n")
cat("Variables totales:", ncol(elsoc_final), "\n")
