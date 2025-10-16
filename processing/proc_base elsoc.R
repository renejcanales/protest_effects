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

# Parte A: Crear las variables base (binarias y de frecuencia)
elsoc_clean <- elsoc_clean %>%
  mutate(
    # Variables individuales binarias (1 = participó, 0 = no participó)
    firma_peticion_bin = if_else(firma_peti >= 2, 1, 0, missing = 0),
    asist_marcha_bin = if_else(asist_marcha >= 2, 1, 0, missing = 0),
    part_huelga_bin = if_else(part_huelga >= 2, 1, 0, missing = 0),
    part_cacerolazo_bin = if_else(part_cacerol >= 2, 1, 0, missing = 0),
    
    # Variables de frecuencia (escala original 1-5)
    firma_peticion_freq = firma_peti,
    asist_marcha_freq = asist_marcha,
    part_huelga_freq = part_huelga,
    part_cacerolazo_freq = part_cacerol,
    
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
elsoc_clean <- elsoc_clean %>%
  mutate(
    educ_cat = case_when(
      educ_encuestado %in% c(1:4) ~ "Media incompleta o menos",
      educ_encuestado == 5 ~ "Media completa",
      educ_encuestado == 6 ~ "Técnica superior incompleta",
      educ_encuestado == 7 ~ "Técnica superior completa",
      educ_encuestado == 8 ~ "Universitaria incompleta",
      educ_encuestado == 9 ~ "Universitaria completa",
      educ_encuestado == 10 ~ "Postgrado",
      TRUE ~ NA_character_
    ),
    educ_cat_factor = factor(
      educ_cat,
      levels = c("Media incompleta o menos", "Media completa", "Técnica superior incompleta",
                 "Técnica superior completa", "Universitaria incompleta", "Universitaria completa", "Postgrado"),
      ordered = TRUE
    ),
    # Años de estudio aproximados según la codificación de ELSOC
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

# Calcular años de estudio de los padres y variables resumen
elsoc_clean <- elsoc_clean %>%
  mutate(
    educ_padre_years = case_when(
      educ_padre == 1 ~ 0, educ_padre == 2 ~ 4, educ_padre == 3 ~ 8, educ_padre == 4 ~ 10,
      educ_padre == 5 ~ 12, educ_padre == 6 ~ 13, educ_padre == 7 ~ 14, educ_padre == 8 ~ 14,
      educ_padre == 9 ~ 17, educ_padre == 10 ~ 19, TRUE ~ NA_real_
    ),
    educ_madre_years = case_when(
      educ_madre == 1 ~ 0, educ_madre == 2 ~ 4, educ_madre == 3 ~ 8, educ_madre == 4 ~ 10,
      educ_madre == 5 ~ 12, educ_madre == 6 ~ 13, educ_madre == 7 ~ 14, educ_madre == 8 ~ 14,
      educ_madre == 9 ~ 17, educ_madre == 10 ~ 19, TRUE ~ NA_real_
    ),
    educ_parental_max = pmax(educ_padre_years, educ_madre_years, na.rm = TRUE),
    educ_parental_max = if_else(is.na(educ_padre_years) & is.na(educ_madre_years), NA_real_, educ_parental_max),
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

# Recode Ocupación

remotes::install_github("DiogoFerrari/occupar")

# ISCO 

frq(elsoc_clean$ciuo08_m03)

elsoc_clean %>%
  group_by(ola) %>%
  summarise(
    solo_NA = all(is.na(ciuo88_m03))
  ) # only not in wave 2016

frq(elsoc_clean$ciuo08_m03)

elsoc_clean %>%
  group_by(ola) %>%
  summarise(
    solo_NA = all(is.na(ciuo08_m03))
  ) # only wave 2018, 2021 and 2023

elsoc_clean$isco88 <- NA
elsoc_clean$isco88[elsoc_clean$ola %in% c(1)] <- elsoc_clean$ciuo88_m03[elsoc_clean$ola %in% c(1)]

elsoc_clean$isco88[elsoc_clean$ola %in% c(3,5,7)] <- occupar::isco08to88(elsoc_clean$ciuo08_m03[elsoc_clean$ola %in% c(3,5,7)]
)

elsoc_clean %>%
  group_by(ola) %>%
  summarise(
    solo_NA = all(is.na(isco88))
  )

# Crear una columna con la variable "isco08" adelantada una ola
elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%        # Agrupa por id para trabajar en cada individuo
  mutate(isco88_lagged=lag(isco88,n=1)) %>%  # Desplaza isco08 a la siguiente ola
  ungroup()

# Rellenar los valores NA en la variable original
elsoc_clean <- elsoc_clean %>%
  mutate(isco88=ifelse(!is.na(isco88),isco88,isco88_lagged)) %>%  # Si isco08 es NA, sustituir con el valor de la ola anterior 
  dplyr::select(-isco88_lagged)                 # Elimina la columna temporal

elsoc_clean %>%
  group_by(ola) %>%
  summarise(
    solo_NA = all(is.na(isco88))
  )

# tipo empleo
library(sjPlot)

frq(elsoc_clean$tipo_empleo)

labs_sj <- c(
  `1` = "Empleado u obrero en empresa privada",
  `2` = "Empleado u obrero del sector publico",
  `3` = "Miembro de las Fuerzas Armadas y de Orden",
  `4` = "Patron/a o empleador/a",
  `5` = "Trabaja solo, no tiene empleados",
  `6` = "Familiar no remunerado",
  `7` = "Servicio domestico"
)

elsoc_clean <- elsoc_clean %>%
  mutate(tipo_empleo = suppressWarnings(as.integer(tipo_empleo)),
         tipo_empleo = if_else(tipo_empleo %in% c(1,2,4,5,7), tipo_empleo, NA_integer_)) %>%
  group_by(idencuesta) %>%
  arrange(idencuesta, ola, .by_group = TRUE) %>%
  mutate(tipo_empleo = coalesce(tipo_empleo, lag(tipo_empleo))) %>%
  ungroup() %>%
  mutate(
    tipo_empleo = set_labels(tipo_empleo, labels = labs_sj),
    tipo_empleo = set_label(tipo_empleo, label = "Relación de empleo (1–7)")
  )

frq(elsoc_clean$tipo_empleo)

sjt.xtab(elsoc_clean$tipo_empleo,elsoc_clean$ola,
         show.col.prc=TRUE,
         var.labels=c("Relacion de empleo","Ola"),
         show.summary=FALSE,         title=NULL)

elsoc_clean <- elsoc_clean %>% 
  rowwise() %>% 
  mutate(selfemp_egp = case_when(tipo_empleo %in% c(4:5) ~ 1,
                                 tipo_empleo %in% c(1,2,7) ~ 0,
                                 TRUE ~ NA)) %>% 
  ungroup()

frq(elsoc_clean$selfemp_egp)

elsoc_clean <- elsoc_clean %>% 
  mutate(cantidad_trabajadores_rec = case_when(
                            cantidad_trabajadores == 1 ~ 1,
                            cantidad_trabajadores == 2 ~ 4,
                            cantidad_trabajadores == 3 ~ 9,
                            cantidad_trabajadores == 4 ~ 49,
                            cantidad_trabajadores == 5 ~ 199,
                            cantidad_trabajadores == 6 ~ 201,
                             TRUE ~ NA_real_))

elsoc_clean <- elsoc_clean %>% 
  mutate(nemploy_egp = if_else(selfemp_egp == 1 & cantidad_trabajadores_rec > 1, cantidad_trabajadores_rec, 0))

frq(elsoc_clean$nemploy_egp)

elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%        # Agrupa por id para trabajar en cada individuo
  mutate(nemploy_egp_lagged=lag(nemploy_egp,n=1)) %>%  # Desplaza isco08 a la siguiente ola
  ungroup()

# Rellenar los valores NA en la variable original
elsoc_clean <- elsoc_clean %>%
  mutate(nemploy_egp=ifelse(!is.na(nemploy_egp),nemploy_egp,nemploy_egp_lagged)) %>%  # Si isco08 es NA, sustituir con el valor de la ola anterior 
  dplyr::select(-nemploy_egp_lagged)                 # Elimina la columna temporal


sjt.xtab(elsoc_clean$nemploy_egp,elsoc_clean$ola,
         show.col.prc=TRUE,
         var.labels=c("N employees","Ola"),
         show.summary=FALSE,         title=NULL)

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

elsoc_analisis <- elsoc_clean %>%
  filter(
    # Adultos en edad relevante
    edad >= 18 & edad <= 75,
    
    # Sin missing en variables clave
    !is.na(educ_encuestado),
    !is.na(protesta_index)
    
    # OPCIONAL: requerir info de padres
    # !is.na(educ_parental_max)
  )


cat("Casos originales:", nrow(elsoc_clean), "\n")
cat("Casos finales:", nrow(elsoc_analisis), "\n")
cat("Porcentaje retenido:", 
    round(nrow(elsoc_analisis)/nrow(elsoc_clean)*100, 2), "%\n")

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
                dplyr::select(protesta_index, educ_years, movilidad_years, edad)))

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

elsoc_final <- elsoc_analisis %>%
  dplyr::select(
    # IDs
    idencuesta, ola, year, tipo_atricion,
    
    # VD - Formas individuales (binarias)
    firma_peticion_bin, asist_marcha_bin, 
    part_huelga_bin, part_cacerolazo_bin,
    
    # VD - Formas individuales (frecuencia)
    firma_peticion_freq, asist_marcha_freq,
    part_huelga_freq, part_cacerolazo_freq,
    
    # VD - Índices principales
    protesta_index,              # PRINCIPAL: marchas + huelgas + peticiones
    protesta_index_completo,     # Incluye cacerolazos
    
    # VD - Intensidad
    protesta_intensidad_suma,
    protesta_intensidad_suma_completa,
    
    # VD - Dicotómicas
    protesta_dummy,              # PRINCIPAL DUMMY (solo asist_marcha)
    
    # Flags
    cacerolazo_disponible,
    protesta_missing,
    
    # VI principales
    educ_cat, educ_cat_factor, educ_years,
    univ_dummy, tecnica_dummy, superior_dummy,
    educ_parental_max, educ_parental_cat,
    movilidad_years, movilidad_cat, movilidad_cat_factor,
    movilidad_asc_dummy, movilidad_asc_alta,
    
    # Controles
    edad, edad_cuadratica, mujer,
    ideologia_std, ideologia_cat,
    interes_politica,
    conf_instituciones, eficacia_politica,
    
    # Temporales
    post_estallido, periodo,
    
    # Mediadoras
    justicia_distributiva, meritocracia,
    clase_subjetiva, satis_ingreso_std, egp, egp3
  )

sjPlot::view_df(elsoc_final,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)
# ==============================================================================
# 10. GUARDAR BASE
# ==============================================================================

save(elsoc_final, file = here ("input/data/proc/elsoc_final.RData"))

cat("Dimensiones finales:", dim(elsoc_final), "\n")