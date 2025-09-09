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


##Comparativos##
# 1. Preparar datos con educación de padres
elsoc_2019 <- elsoc_long %>%
  filter(!is.na(educ_encuestado),
         !is.na(marcha)) %>%
  mutate(
    educ_encuestado = as.numeric(educ_encuestado),
    educ_padre = as.numeric(educ_padre),
    educ_madre = as.numeric(educ_madre),
    marcha = as.numeric(marcha)
  )

# Verificar casos disponibles
cat("Casos con educación encuestado y marcha:", nrow(elsoc_2019), "\n")
cat("Casos con educación padre:", sum(!is.na(elsoc_2019$educ_padre)), "\n")
cat("Casos con educación madre:", sum(!is.na(elsoc_2019$educ_madre)), "\n")

# 2. MODELO 1: Solo educación del encuestado
modelo1_simple <- lm(marcha ~ educ_encuestado, data = elsoc_2019)

# 3. MODELO 2: Educación encuestado + educación padres
# Filtrar casos con datos completos de educación familiar
datos_familia <- elsoc_2019 %>%
  filter(!is.na(educ_padre), !is.na(educ_madre))

cat("Casos finales con datos completos de familia:", nrow(datos_familia), "\n")

# Modelo con educación familiar
modelo2_familia <- lm(marcha ~ educ_encuestado + educ_padre + educ_madre, 
                      data = datos_familia)

# Modelo Solo educación de los padres
modelo2_simple <- lm(marcha ~ educ_padre + educ_madre, data = datos_familia)

# También ejecutar modelo simple en la misma muestra para comparación justa
modelo1_restringido <- lm(marcha ~ educ_encuestado, data = datos_familia)

library(broom)
library(knitr)

# Resumen de modelos
summary(modelo1_simple)
summary(modelo1_restringido) 
summary(modelo2_familia)
summary(modelo2_simple)

# Tabla comparativa
comparacion <- data.frame(
  Modelo = c("Solo Educación Encuestado (muestra completa)",
             "Solo Educación Encuestado (muestra restringida)", 
             "Con Educación Padres"),
  N = c(nrow(elsoc_2019), nrow(datos_familia), nrow(datos_familia)),
  R_cuadrado = c(summary(modelo1_simple)$r.squared,
                 summary(modelo1_restringido)$r.squared,
                 summary(modelo2_familia)$r.squared),
  R_cuadrado_adj = c(summary(modelo1_simple)$adj.r.squared,
                     summary(modelo1_restringido)$adj.r.squared,
                     summary(modelo2_familia)$adj.r.squared)
)

library(kableExtra)
kable(comparacion, digits = 4)

# Test F para comparar modelos anidados
anova(modelo1_restringido, modelo2_familia)
#El test ANOVA confirma que los dos modelos son estadísticamente diferentes (p < 0.001), 
#lo que significa que incluir la educación parental mejora significativamente la predicción de asistencia a marchas.

# AIC para comparar ajuste
AIC(modelo1_restringido, modelo2_familia)

# Coeficientes modelo simple
coef_simple <- tidy(modelo1_restringido) %>%
  mutate(modelo = "Simple")

# Coeficientes modelo con padres
coef_familia <- tidy(modelo2_familia) %>%
  mutate(modelo = "Con padres")

# Tabla combinada
tabla_coef <- bind_rows(coef_simple, coef_familia) %>%
  select(modelo, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    p.value = round(p.value, 4)
  )

print(tabla_coef)

library(ggplot2)

# Coeficientes de educación en ambos modelos
efectos <- data.frame(
  Modelo = c("Solo Encuestado", "Con Padres"),
  Coeficiente = c(coef(modelo1_restringido)["educ_encuestado"],
                  coef(modelo2_familia)["educ_encuestado"]),
  Error_std = c(summary(modelo1_restringido)$coefficients["educ_encuestado", "Std. Error"],
                summary(modelo2_familia)$coefficients["educ_encuestado", "Std. Error"])
)

# Crear datos con información de significancia
efectos_sig <- data.frame(
  Modelo = c("Solo Encuestado", "Con Padres"),
  Coeficiente = c(coef(modelo1_restringido)["educ_encuestado"],
                  coef(modelo2_familia)["educ_encuestado"]),
  Error_std = c(summary(modelo1_restringido)$coefficients["educ_encuestado", "Std. Error"],
                summary(modelo2_familia)$coefficients["educ_encuestado", "Std. Error"]),
  p_value = c(summary(modelo1_restringido)$coefficients["educ_encuestado", "Pr(>|t|)"],
              summary(modelo2_familia)$coefficients["educ_encuestado", "Pr(>|t|)"]),
  significancia = c("p < 0.001", "p < 0.001")
)

ggplot(efectos_sig, aes(x = Modelo, y = Coeficiente)) +
  geom_point(size = 4, aes(color = Modelo)) +
  geom_errorbar(aes(ymin = Coeficiente - 1.96*Error_std, 
                    ymax = Coeficiente + 1.96*Error_std,
                    color = Modelo), 
                width = 0.1, linewidth = 1) +
  geom_text(aes(label = significancia), 
            vjust = -0.5, hjust = 0.5, size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Solo Encuestado" = "red", "Con Padres" = "blue")) +
  labs(title = "Efecto de Educación del Encuestado sobre Asistencia a Marchas",
       subtitle = "Comparación con y sin controles de educación parental (IC 95%)",
       y = "Coeficiente (IC 95%)",
       caption = "Ambos coeficientes son altamente significativos (p < 0.001)") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico de coeficientes con intervalos de confianza
ggplot(efectos, aes(x = Modelo, y = Coeficiente)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coeficiente - 1.645*Error_std,  # 90% IC en lugar de 95%
                    ymax = Coeficiente + 1.645*Error_std), 
                width = 0.1) +
  labs(title = "Efecto de Educación del Encuestado sobre Asistencia a Marchas",
       subtitle = "Comparación con y sin controles de educación parental (IC 90%)",
       y = "Coeficiente (IC 90%)") +
  theme_minimal()

ggplot(efectos, aes(x = Modelo, y = Coeficiente)) +
  geom_col(aes(fill = Modelo), alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = Coeficiente - 1.96*Error_std, 
                    ymax = Coeficiente + 1.96*Error_std), 
                width = 0.2, linewidth = 1) +
  geom_text(aes(label = paste("β =", round(Coeficiente, 3))), 
            vjust = -2.5, fontface = "bold") +
  geom_text(aes(label = "p < 0.001"), 
            vjust = -1.2, size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Solo Encuestado" = "#E74C3C", "Con Padres" = "#3498DB")) +
  labs(title = "Efecto de Educación del Encuestado sobre Asistencia a Marchas",
       subtitle = "Ambos efectos son estadísticamente significativos",
       y = "Coeficiente (IC 95%)") +
  theme_minimal() +
  theme(legend.position = "none")


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