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

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               sjPlot,
               here,
               tidyr,
               naniar,
               dplyr,
               broom,
               broom.mixed,
               texreg)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------


load("~/GitHub/protest_effects/input/data/raw/elsoc_long_2016_2023.RData")

elsoc_long <- elsoc_long_2016_2023

rm(elsoc_long_2016_2023)

glimpse(elsoc_long)

# 3. Processing -----------------------------------------------------------

# mutate ----

elsoc_long[elsoc_long ==-999] <- NA
elsoc_long[elsoc_long ==-888] <- NA
elsoc_long[elsoc_long ==-777] <- NA
elsoc_long[elsoc_long ==-666] <- NA

elsoc_long <- elsoc_long %>% 
  mutate(idencuesta, tipo_caso,
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
         perc_sub_clase = c33,
         just_violencia_carab = f05_03,
         just_violencia_trab = f05_06,
         just_violencia_est = f05_07,
         dannio_inmobilia = f05_09,
         dannio_transporte = f05_10,
         dannio_comercio = f05_11,
         ideologia = c15,
         justicia_pensiones = d02_01,
         justicia_educacion = d02_02, 
         justicia_salud = d02_03
         ) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)


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


#----Índice de movilidad-----#

elsoc_long <- elsoc_long %>%
  mutate(
    educ_padres_max = pmax(educ_padre, educ_madre, na.rm = TRUE),
    movilidad_educ = educ_encuestado - educ_padres_max,
    movilidad_cat = case_when(
      movilidad_educ <= -2 ~ "Movilidad descendente",
      movilidad_educ %in% c(-1, 0, 1) ~ "Sin movilidad",
      movilidad_educ >= 2 ~ "Movilidad ascendente"
    )
  )

# Guardar

save(elsoc_long, file = here ("input/data/proc/elsoc_long.RData"))

#--------------------------------------------------------------
# Analysis General
#--------------------------------------------------------------

# Analisis transversal

# Filtrar solo ola 4 (año 2019)
elsoc_2019 <- elsoc_long %>%
  filter(ola == 4)

# Verificar que solo tienes datos de 2019
table(elsoc_2019$ola)
nrow(elsoc_2019)

# Explorar variables principales
summary(elsoc_2019$educ_encuestado)  # Educación (variable independiente)
frq(elsoc_2019$educ_encuestado)

summary(elsoc_2019$asist_marcha) # Asistencia a marchas
frq(elsoc_2019$asist_marcha)

summary(elsoc_2019$protesta_dummy) # Asistencia a marchas
frq(elsoc_2019$protesta_dummy)

frq(elsoc_2019$ideologia)

# Verificar valores perdidos
elsoc_2019 %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Crear dataset limpio para regresiones
datos_2019 <- elsoc_2019 %>%
  filter(!is.na(educ_encuestado),
         !is.na(asist_marcha)
         ) %>%  # Filtrar casos con datos completos
  mutate(
    # Convertir a numérico si es necesario
    educ_encuestado = as.numeric(educ_encuestado),
    marcha = as.numeric(asist_marcha),
    just_violencia_carab = as.numeric(just_violencia_carab),
    just_violencia_trab = as.numeric(just_violencia_trab),
    just_violencia_est = as.numeric(just_violencia_est),
    dannio_inmobilia = as.numeric(dannio_inmobilia),
    dannio_transporte = as.numeric(dannio_transporte),
    dannio_comercio = as.numeric(dannio_comercio),
  )

# Verificar casos finales
nrow(datos_2019)

#------ Modelos Iniciales Transversales ------

# Asiste Marcha/Educación - Continua
modelo_marcha <- lm(asist_marcha ~ educ_encuestado, data = elsoc_2019)
screenreg(modelo_marcha)

# Asiste Marcha/Educación - Dummy
modelo_logit <- glm(protesta_dummy ~ educ_encuestado, 
                    data = elsoc_long, 
                    family = binomial(link = "logit"))
screenreg(modelo_logit)

# Calcular Odds Ratios
exp(coef(modelo_logit))
# O con intervalos de confianza
exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))

#----- Modelo Comparativo por Educación ------
elsoc_2019 <- elsoc_2019 %>%
  mutate(educ_cat = case_when(
    educ_encuestado <= 3 ~ "Básica o menos",
    educ_encuestado == 4 ~ "Media incompleta", 
    educ_encuestado == 5 ~ "Media completa",
    educ_encuestado == 6 ~ "Técnica incompleta",
    educ_encuestado == 7 ~ "Técnica completa",
    educ_encuestado == 8 ~ "Universitaria incompleta",
    educ_encuestado == 9 ~ "Universitaria completa",
    educ_encuestado == 10 ~ "Posgrado",
    TRUE ~ NA_character_
  ))

# Convertir a factor con Media completa como referencia
elsoc_2019 <- elsoc_2019 %>%
  mutate(educ_cat = factor(educ_cat, 
                           levels = c("Media completa",  # Referencia
                                      "Básica o menos",
                                      "Media incompleta",
                                      "Técnica incompleta",
                                      "Técnica completa",
                                      "Universitaria incompleta",
                                      "Universitaria completa",
                                      "Posgrado")))


# Modelo con variable continua
# Filtrar solo desde Media completa en adelante
elsoc_2019_filtrado <- elsoc_2019 %>%
  filter(educ_encuestado >= 5)

# Modelo sin controles
modelo_1 <- lm(asist_marcha ~ educ_cat, 
               data = elsoc_2019_filtrado)

# Crear sexo como factor
elsoc_2019_filtrado <- elsoc_2019_filtrado %>%
  mutate(sexo_factor = factor(genero, 
                              levels = c(1, 2),
                              labels = c("Hombre", "Mujer")))

# Modelo con controles
modelo_2 <- lm(asist_marcha ~ educ_cat + sexo_factor + ideologia + interes_politica, 
               data = elsoc_2019_filtrado)

# Comparar modelos
screenreg(list(modelo_1, modelo_2),
          custom.model.names = c("Modelo 1: Solo Educación", 
                                 "Modelo 2: Con Controles"))

# Modelo 3: Con interacción educación * interés político
modelo_3 <- lm(asist_marcha ~ educ_cat * interes_politica + sexo_factor + ideologia, 
               data = elsoc_2019_filtrado)

# Comparar los tres modelos
screenreg(list(modelo_1, modelo_2, modelo_3),
          custom.model.names = c("Modelo 1: Solo Educación", 
                                 "Modelo 2: Con Controles",
                                 "Modelo 3: Con Interacción"))

# Ver summary detallado del modelo 3
summary(modelo_3)

# Test de significancia de la interacción
anova(modelo_2, modelo_3)

# Modelo con Variable Dummy

# Filtrar solo desde Media completa (educacion >= 5)
elsoc_2019 <- elsoc_2019 %>%
  filter(educ_encuestado >= 5)

# Verificar
table(elsoc_2019$educ_cat, useNA = "always")

# Modelo logístico con Media completa como referencia
modelo_cat_filtrado <- glm(protesta_dummy ~ educ_cat, 
                           data = elsoc_2019, 
                           family = binomial(link = "logit"))

summary(modelo_cat_filtrado)

# Odds Ratios con IC
exp(cbind(OR = coef(modelo_cat_filtrado), confint(modelo_cat_filtrado)))

# Tabla bonita
screenreg(modelo_cat_filtrado)

#--------------------------------------------------------------------------------------------
  
#------ Modelos Iniciales longitudinales ------

######### HIPOTESIS 1 ###########


library(plm)
library(lme4)

# Opción A: Modelo de efectos aleatorios (plm)
modelo_re <- plm(asist_marcha ~ educ_encuestado + genero + ideologia + interes_politica + factor(ola),
                 data = elsoc_long,
                 index = c("idencuesta", "ola"),
                 model = "random")

# Opción B: Modelo mixto (lme4) - más flexible
modelo_mixto <- lmer(asist_marcha ~ educ_cat + genero + ideologia + interes_politica + 
                       (1 | idencuesta) + factor(ola),
                     data = elsoc_long)

######### HIPOTESIS 2 ###########

# ¿ELSOC tiene datos de educación de los padres?
# Buscar variables como educ_padre, educ_madre

# Crear variable de movilidad
elsoc_long <- elsoc_long %>%
  mutate(
    educ_padres_max = pmax(educ_padre, educ_madre, na.rm = TRUE),
    movilidad_educ = educ_encuestado - educ_padres_max,
    movilidad_cat = case_when(
      movilidad_educ <= -2 ~ "Movilidad descendente",
      movilidad_educ %in% c(-1, 0, 1) ~ "Sin movilidad",
      movilidad_educ >= 2 ~ "Movilidad ascendente"
    )
  )

# Modelo con interacción educación × movilidad
modelo_movilidad <- lmer(asist_marcha ~ educ_cat * movilidad_cat + 
                           genero + ideologia + interes_politica + 
                           (1 | idencuesta) + factor(ola),
                         data = elsoc_long)

screenreg(modelo_movilidad)


### Interacciones significativas (LO INTERESANTE):

#Técnica incompleta × Movilidad descendente: +0.46*
  
###Personas con técnica incompleta que experimentaron movilidad descendente protestan MÁS

#Universitaria incompleta × Sin movilidad: +0.36*
  
###Personas con universitaria incompleta sin movilidad educacional protestan MÁS


############################################################################################################################################################


# missings ----

colSums(is.na(db_long))

na.omit(db_long) 

db_long <- naniar::add_n_miss(db_long)

any_na(db_long)

n_miss(db_long)

prop_miss(db_long[c(4:11)])

naniar::gg_miss_var(db_long)

miss_var_summary(db_long)

miss_var_table(db_long)

miss_case_summary(db_long)

miss_case_table(db_long)

vis_miss(db_long) + theme(axis.text.x = element_text(angle=80))

#db_long <- na.omit(db_long)