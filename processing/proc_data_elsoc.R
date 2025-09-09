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
               naniar)

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
         marcha = c08_02,
         just_vio_carab = f05_03,
         just_vio_trab = f05_06,
         just_vio_est = f05_07,
         vio_inmob = f05_09,
         vio_transp = f05_10,
         vio_comer = f05_11,
         ideologia = c15) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# Justificación de la violencia
frq(elsoc_long$just_vio_carab)
frq(elsoc_long$just_vio_est)
frq(elsoc_long$just_vio_trab)

elsoc_long <- elsoc_long %>% 
  mutate(
    across(
      .cols = c(just_vio_carab, just_vio_est, just_vio_trab),
      .fns = ~ car::recode(., recodes = c("1='Nunca'; 2='Pocas veces';
                                          3='Algunas'; 4='Muchas veces';
                                          5='Siempre se justifica'"), 
                           levels = c("Nunca", "Pocas veces", "Algunas veces", "Muchas veces", "Siempre"),
                           as.factor = T)
    )
  )

elsoc_long$just_vio_carab <- sjlabelled::set_label(elsoc_long$just_vio_carab,
                                                          label = "Justificacion carabineros repriman marcha pacifica")

elsoc_long$just_vio_est <- sjlabelled::set_label(elsoc_long$just_vio_est,
                                                           label = "Justificacion estudiantes pedreen carabineros")

elsoc_long$just_vio_trab <- sjlabelled::set_label(elsoc_long$just_vio_trab,
                                                        label = "Justificacion trabajadores cortacalle")

# Protesta
frq(elsoc_long$marcha)

elsoc_long <- elsoc_long %>% 
  mutate(
    across(
      .cols = c(marcha),
      .fns = ~ car::recode(., recodes = c("1='Nunca'; 2='Casi nunca';
                                          3='A veces'; 4='Frecuentemente';
                                          5='Muy frecuentemente'"), 
                           levels = c("Nunca", "Casi nunca", "A veces", "Frecuentemente", "Muy frecuentemente"),
                           as.factor = T)
    )
  )

elsoc_long$marcha <- sjlabelled::set_label(elsoc_long$marcha,
                                                   label = "Asistencia a Marchas en el último año")

# Educacion
frq(elsoc_long$educ_encuestado)
frq(elsoc_long$educ_padre)

# Confianza
frq(elsoc_long$conf_cong)
frq(elsoc_long$conf_part)
frq(elsoc_long$conf_pres)
frq(elsoc_long$conf_gob)



# Analisis transversal

# Filtrar solo ola 4 (año 2019)
elsoc_2019 <- elsoc_long %>%
  filter(ola == 4) %>%  # Mantener solo ola 4
  select(idencuesta, tipo_caso, ola, comuna, region,
         genero, edad, educ_encuestado, marcha,
         just_vio_carab, just_vio_trab, just_vio_est,
         vio_inmob, vio_transp, vio_comer)

# Verificar que solo tienes datos de 2019
table(elsoc_2019$ola)
nrow(elsoc_2019)

# Explorar variables principales
summary(elsoc_2019$educ_encuestado)  # Educación (variable independiente)
summary(elsoc_2019$marcha)           # Asistencia a marchas
summary(elsoc_2019$just_vio_carab)   # Justificación violencia carabineros
summary(elsoc_2019$just_vio_trab)    # Justificación violencia trabajadores
summary(elsoc_2019$just_vio_est)     # Justificación violencia estudiantes

# Verificar valores perdidos
library(dplyr)
elsoc_2019 %>% 
  select(educ_encuestado, marcha, just_vio_carab, just_vio_trab, just_vio_est) %>%
  summarise_all(~sum(is.na(.)))

# Crear dataset limpio para regresiones
datos_2019 <- elsoc_2019 %>%
  filter(!is.na(educ_encuestado),
         !is.na(marcha)) %>%  # Filtrar casos con datos completos
  mutate(
    # Convertir a numérico si es necesario
    educ_encuestado = as.numeric(educ_encuestado),
    marcha = as.numeric(marcha),
    just_vio_carab = as.numeric(just_vio_carab),
    just_vio_trab = as.numeric(just_vio_trab),
    just_vio_est = as.numeric(just_vio_est)
  )

# Verificar casos finales
nrow(datos_2019)


modelo_marcha <- lm(marcha ~ educ_encuestado, data = datos_2019)
summary(modelo_marcha)

# Filtrar casos sin NA en justificación violencia carabineros
datos_vio_carab <- datos_2019 %>% filter(!is.na(just_vio_carab))

modelo_vio_carab <- lm(just_vio_carab ~ educ_encuestado, data = datos_vio_carab)
summary(modelo_vio_carab)

# Filtrar casos sin NA en justificación violencia trabajadores
datos_vio_trab <- datos_2019 %>% filter(!is.na(just_vio_trab))

modelo_vio_trab <- lm(just_vio_trab ~ educ_encuestado, data = datos_vio_trab)
summary(modelo_vio_trab)


# Filtrar casos sin NA en justificación violencia estudiantes
datos_vio_est <- datos_2019 %>% filter(!is.na(just_vio_est))

modelo_vio_est <- lm(just_vio_est ~ educ_encuestado, data = datos_vio_est)
summary(modelo_vio_est)

library(broom)

# Crear tabla con todos los modelos
resultados <- bind_rows(
  tidy(modelo_marcha) %>% mutate(modelo = "Asistencia a marchas"),
  tidy(modelo_vio_carab) %>% mutate(modelo = "Just. violencia carabineros"),
  tidy(modelo_vio_trab) %>% mutate(modelo = "Just. violencia trabajadores"),
  tidy(modelo_vio_est) %>% mutate(modelo = "Just. violencia estudiantes")
) %>%
  filter(term == "educ_encuestado") %>%  # Solo coeficiente de educación
  select(modelo, estimate, std.error, statistic, p.value)

print(resultados)

library(ggplot2)
library(gridExtra)

# Gráfico 1: Educación vs Asistencia a marchas
p1 <- ggplot(datos_2019, aes(x = educ_encuestado, y = marcha)) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Educación → Asistencia a Marchas (2019)",
       x = "Nivel Educativo", y = "Frecuencia Asistencia a Marchas") +
  theme_minimal()

# Gráfico 2: Educación vs Justificación violencia carabineros
p2 <- ggplot(datos_vio_carab, aes(x = educ_encuestado, y = just_vio_carab)) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Educación → Just. Violencia Carabineros (2019)",
       x = "Nivel Educativo", y = "Justificación Violencia") +
  theme_minimal()

# Gráfico 3: Educación vs Justificacion violencia estudiantes
p3 <- ggplot(datos_2019, aes(x = educ_encuestado, y = just_vio_est)) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Educación → Justificacion Violencia estudiantes (2019)",
       x = "Nivel Educativo", y = "Justificacion Violencia") +
  theme_minimal()

# Gráfico 4: Educación vs Justificación violencia carabineros
p4 <- ggplot(datos_2019, aes(x = educ_encuestado, y = just_vio_trab)) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Educación → Just. Violencia Trabajadores (2019)",
       x = "Nivel Educativo", y = "Justificación Violencia") +
  theme_minimal()

# Mostrar gráficos
grid.arrange(p1, p2, ncol = 2)

grid.arrange(p3, p4, ncol = 2)

# Interpreta:
####Gráfico 1: Educación → Asistencia a Marchas (2019)###
#Relación positiva significativa:
  
#La línea roja ascendente indica que a mayor nivel educativo, mayor frecuencia de asistencia a marchas
#El efecto es moderado pero consistente: las personas con educación universitaria (niveles 8-10) tienden a participar más en marchas que aquellas con educación básica (niveles 1-3)
#La concentración de puntos en el valor 1 (nunca asiste) muestra que la mayoría de la población no participa en marchas, independiente de su nivel educativo

###Gráfico 2: Educación → Justificación a que  Carabineros (2019)
#Educación → Justificación de represión policial a marchas pacíficas
#Relación negativa (aunque leve):

#La línea azul descendente indica que a mayor nivel educativo, menor justificación de que carabineros repriman marchas pacíficas
#Las personas más educadas tienden a ser menos tolerantes con la represión policial de manifestaciones pacíficas
#El efecto es pequeño pero consistente con la teoría


### LONGITUDINAL ####

# recode and transform ----
mutate(
  # recodificación ola a años
  año = dplyr::recode(ola,
                       `1` = 2016,
                       `2` = 2017,
                       `3` = 2018,
                       `4` = 2019,
                       `5` = 2021,  # no hubo ola 2020
                       `6` = 2023),
  
  año = factor(anio, levels = c(2016, 2017, 2018, 2019, 2021, 2023)),
  
  # recodificación asistencia a marchas (0/1)
  protesta_bin = dplyr::case_when(
    frq_marcha %in% c(1, 2) ~ 0,
    frq_marcha %in% c(3, 4, 5) ~ 1,
    TRUE ~ NA_real_
  ),
  
  # ejemplo: recodificación sexo
  sexo = dplyr::recode(sexo,
                       `1` = "Hombre",
                       `2` = "Mujer"))
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