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
               here,
               tidyr,
               naniar,
               dplyr,
               broom,
               broom.mixed)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------


load("~/GitHub/protest_effects/input/data/raw/elsoc_long_2016_2023.RData")

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

elsoc_long <- elsoc_long %>% 
  mutate(idencuesta, 
         tipo_atricion,
         estrato,
         segmento,
         ola,
         comuna,
         comuna_cod,
         region, region_cod,
         genero = m0_sexo,
         edad = m0_edad,
         educ_encuestado = m01,
         educ_padre = m27,
         educ_madre = m28,
         ocupacion_encuestado = m03,
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
         asist_marcha = c08_02,
         part_huelga = c08_03,
         miembro_pp = c12_03,
         perc_sub_clase = c33,
         tipo_empleo = m07,
         cantidad_trabajadores = m05,
         satis_ingreso = m12,
         ideologia = c15,
         recomp_esfuer = c18_09,
         recomp_talent = c18_10,
         justicia_pensiones = d02_01,
         justicia_educacion = d02_02, 
         justicia_salud = d02_03
         ) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# ============================================
# IMPUTAR EDUCACIÓN DEL PADRE (CONSTANTE)
# ============================================

# Nombre de tu variable
educ_padre_imp <- "educ_padre"  # ajusta según tu variable real

# Método 1: Usar el primer valor disponible por persona
elsoc_long <- elsoc_long %>%
  group_by(idencuesta) %>%
  arrange(ola) %>%
  fill(!!sym(educ_padre_imp), .direction = "downup") %>%
  ungroup()

# Verificar resultado
elsoc_long %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_disponible = sum(!is.na(.data[[educ_padre_imp]])),
    pct_disponible = round(100 * n_disponible / n_total, 1)
  )

# ============================================
# IMPUTAR EDUCACIÓN DE LA MADRE (CONSTANTE)
# ============================================

# Nombre de tu variable
educ_madre_imp <- "educ_madre"  # ajusta según tu variable real

# Método 1: Usar el primer valor disponible por persona
elsoc_long <- elsoc_long %>%
  group_by(idencuesta) %>%
  arrange(ola) %>%
  fill(!!sym(educ_madre_imp), .direction = "downup") %>%
  ungroup()

# Verificar resultado
elsoc_long %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_disponible = sum(!is.na(.data[[educ_madre_imp]])),
    pct_disponible = round(100 * n_disponible / n_total, 1)
  )

# ============================================================================
# RECODIFICACIÓN DE EDUCACIÓN A AÑOS DE ESCOLARIDAD
# ============================================================================

elsoc_long <- elsoc_long %>%
  mutate(
    # Años educación encuestado
    anos_educ_encuestado = case_when(
      educ_encuestado == 1 ~ 0,   # Sin estudios
      educ_encuestado == 2 ~ 4,   # Básica/Preparatoria incompleta
      educ_encuestado == 3 ~ 8,   # Básica/Preparatoria completa
      educ_encuestado == 4 ~ 10,  # Media/Humanidades incompleta
      educ_encuestado == 5 ~ 12,  # Media/Humanidades completa
      educ_encuestado == 6 ~ 13,  # Técnica Superior incompleta
      educ_encuestado == 7 ~ 14,  # Técnica Superior completa
      educ_encuestado == 8 ~ 15,  # Universitaria incompleta
      educ_encuestado == 9 ~ 17,  # Universitaria completa
      educ_encuestado == 10 ~ 19, # Posgrado
      TRUE ~ NA_real_
    ),
    
    # Años educación padre
    anos_educ_padre = case_when(
      educ_padre == 1 ~ 0,
      educ_padre == 2 ~ 4,
      educ_padre == 3 ~ 8,
      educ_padre == 4 ~ 10,
      educ_padre == 5 ~ 12,
      educ_padre == 6 ~ 13,
      educ_padre == 7 ~ 14,
      educ_padre == 8 ~ 15,
      educ_padre == 9 ~ 17,
      educ_padre == 10 ~ 19,
      TRUE ~ NA_real_
    ),
    
    # Años educación madre
    anos_educ_madre = case_when(
      educ_madre == 1 ~ 0,
      educ_madre == 2 ~ 4,
      educ_madre == 3 ~ 8,
      educ_madre == 4 ~ 10,
      educ_madre == 5 ~ 12,
      educ_madre == 6 ~ 13,
      educ_madre == 7 ~ 14,
      educ_madre == 8 ~ 15,
      educ_madre == 9 ~ 17,
      educ_madre == 10 ~ 19,
      TRUE ~ NA_real_
    )
  )

# ============================================================================
# CÁLCULO DE MOVILIDAD EDUCACIONAL
# ============================================================================

elsoc_long <- elsoc_long %>%
  mutate(
    # Máximo educacional parental
    anos_educ_padres_max = pmax(anos_educ_padre, anos_educ_madre, na.rm = TRUE),
    
    # Movilidad absoluta (continua)
    movilidad = anos_educ_encuestado - anos_educ_padres_max,
    
    # Movilidad categórica
    movilidad_cat = case_when(
      movilidad < 0 ~ "Descendente",
      movilidad == 0 ~ "Sin movilidad",
      movilidad >= 1 & movilidad <= 4 ~ "Baja ascendente",
      movilidad >= 5 & movilidad <= 8 ~ "Moderada ascendente",
      movilidad > 8 ~ "Alta ascendente",
      TRUE ~ NA_character_
    ),
    
    # Convertir a factor ordenado
    movilidad_cat = factor(movilidad_cat, 
                           levels = c("Descendente", "Sin movilidad", 
                                      "Baja ascendente", "Moderada ascendente", 
                                      "Alta ascendente"),
                           ordered = TRUE),
    
    # Dummy alta educación (universitaria completa o más)
    alta_educacion = if_else(educ_encuestado >= 9, 1, 0),
    
    # Grupos para hipótesis principal
    grupo_hipotesis = case_when(
      alta_educacion == 1 & movilidad > 4 ~ "A: Alta educ + Alta movilidad",
      alta_educacion == 1 & movilidad <= 2 ~ "B: Alta educ + Sin/Baja movilidad",
      alta_educacion == 0 ~ "C: Baja/Media educación",
      TRUE ~ "Otro"
    ),
    
    grupo_hipotesis = factor(grupo_hipotesis,
                             levels = c("C: Baja/Media educación",
                                        "B: Alta educ + Sin/Baja movilidad",
                                        "A: Alta educ + Alta movilidad",
                                        "Otro")),
    
    # Educación simplificada (para análisis adicionales)
    educ_simplificada = case_when(
      educ_encuestado <= 3 ~ "Básica o menos",
      educ_encuestado %in% c(4, 5) ~ "Media completa",
      educ_encuestado %in% c(6, 7) ~ "Técnica/CFT",
      educ_encuestado == 8 ~ "Universitaria incompleta",
      educ_encuestado >= 9 ~ "Universitaria completa o más",
      TRUE ~ NA_character_
    ),
    
    educ_simplificada = factor(educ_simplificada,
                               levels = c("Básica o menos", "Media completa", 
                                          "Técnica/CFT", "Universitaria incompleta",
                                          "Universitaria completa o más"))
  )


# 3. INDEX CREATION AND RECODE

#-----Confianza Institucional-------

elsoc_long %>% 
  group_by(ola) %>% 
  select(conf_gob, conf_part, conf_cong) %>% 
  frq()

elsoc_long$conf_inst<- rowMeans(elsoc_long[, c("conf_gob", "conf_part", "conf_cong")], na.rm = TRUE)

#----Autoeficacia-----

elsoc_long %>% 
  group_by(ola) %>% 
  select(efic_voto, efic_result, efic_expr) %>% 
  frq()

elsoc_long$eficacia<- rowMeans(elsoc_long[, c("efic_voto", "efic_result", "efic_expr")], na.rm = TRUE)


#---Crear variable dummy de protesta----

elsoc_long <- elsoc_long %>%
  mutate(protesta_dummy = case_when(
    asist_marcha >= 3 ~ 1,  # Participa (A veces, Frecuentemente, Muy frecuentemente)
    asist_marcha <= 2 ~ 0,  # No participa (Nunca, Casi nunca)
    TRUE ~ NA_real_          # Para valores perdidos
  ))

# Verificar la creación de la variable
table(elsoc_long$asist_marcha, elsoc_long$protesta_dummy, useNA = "always")

# Ver frecuencias
table(elsoc_long$protesta_dummy, useNA = "always")

# Guardar

save(elsoc_long, file = here ("input/data/proc/elsoc_long.RData"))


############################################################################################################################################################