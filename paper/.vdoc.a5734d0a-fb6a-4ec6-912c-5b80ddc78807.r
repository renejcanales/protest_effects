#
#| include: false
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE
)

library(dplyr)
library(ggplot2)
library(glmmTMB)
library(texreg)
library(ggeffects)
library(patchwork)
library(gt)
library(kableExtra)
library(here)
library(margins)
library(broom)
library(modelsummary)
library(stargazer)
library(tidyr)

# Cargar datos
load(here("input/data/proc/elsoc_final_2.RData"))
#
#
#
#
#
#
#
#| warning: false
#| message: false
#| echo: false
#| results: false


# Paso 1: Convertir a factor no ordenado
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    educ_cat_unordered = factor(educ_cat_factor, ordered = FALSE),
    movilidad_cat_unordered = factor(movilidad_cat_factor, ordered = FALSE)
  )

# Paso 2: Crear índice consistente de violencia en protestas (solo variables en todas las olas)
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    # Índice consistente temporal (solo estudiantes y trabajadores)
    justif_violencia_protesta_consistente = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes), 
      na.rm = TRUE
    ),
    # Índice amplio (todas las variables disponibles por ola)
    justif_violencia_protesta_amplio = justif_violencia_protesta
  )

# Verificar disponibilidad por ola
elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    n = n(),
    estudiantes = sum(!is.na(violencia_estudiantes)),
    trabajadores = sum(!is.na(violencia_trabajadores)),
    inmobiliario = sum(!is.na(violencia_inmobiliario)),
    transporte = sum(!is.na(violencia_transporte)),
    locales = sum(!is.na(violencia_locales)),
    carab_marchas = sum(!is.na(violencia_carabineros_marchas)),
    carab_tomas = sum(!is.na(violencia_carabineros_tomas))
  ) %>%
  print()

#
#
#
#
#
#
#
#
#
#| label: tbl-justificacion-educ
#| tbl-cap: "Justificación de violencia según nivel educativo y participación en protestas"
#| echo: false
#| warning: false
#| results: asis

# Redefinir los labels del factor:
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    educ_cat_unordered = factor(
      educ_cat_unordered,
      labels = c(
        "Media completa o menos",
        "Téc. sup.incompleta",
        "Téc. sup.completa",
        "Univ. incompleta",
        "Univ. completa"
      )
    )
  )

# Crear y mostrar tabla
tabla_1 <- elsoc_final_2 %>%
  filter(!is.na(educ_cat_unordered) & !is.na(protesta_dummy)) %>%
  group_by(educ_cat_unordered, Participacion = factor(protesta_dummy, 
                                                       labels = c("No participó", "Participó"))) %>%
  summarise(
    N = n(),
    `Viol. Protestas` = round(mean(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `DE` = round(sd(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `Viol. Estatal` = round(mean(justif_violencia_estatal, na.rm = TRUE), 2),
    `DE ` = round(sd(justif_violencia_estatal, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  knitr::kable(
    col.names = c("Educación", "Participación", "N", "Media", "DE", "Media", "DE"),
    align = c("l", "l", "c", "c", "c", "c", "c"),
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    font_size = 9,
    position = "center"
  ) %>%
  add_header_above(c(" " = 2, " " = 1, "Viol. Protestas" = 2, "Viol. Estatal" = 2)) %>%
  footnote(
    general = "Elaboración propia a partir de ELSOC (2016-2023). Escala 1-5 donde 1 = Nunca se justifica y 5 = Siempre se justifica",
    threeparttable = TRUE
  )

tabla_1
#
#
#
#
#
#
#
#| label: tbl-justificacion-clase
#| tbl-cap: "Justificación de violencia según clase social y participación en protestas"
#| echo: false
#| warning: false
#| results: asis

# Crear y mostrar tabla
tabla_2 <- elsoc_final_2 %>%
  filter(!is.na(egp3) & !is.na(protesta_dummy)) %>%
  group_by(egp3, Participacion = factor(protesta_dummy, 
                                         labels = c("No participó", "Participó"))) %>%
  summarise(
    N = n(),
    `Viol. Protestas` = round(mean(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `DE` = round(sd(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `Viol. Estatal` = round(mean(justif_violencia_estatal, na.rm = TRUE), 2),
    `DE ` = round(sd(justif_violencia_estatal, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  knitr::kable(
    col.names = c("Clase Social", "Participación", "N", "Media", "DE", "Media", "DE"),
    align = c("l", "l", "c", "c", "c", "c", "c"),
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    font_size = 9,
    position = "center"
  ) %>%
  add_header_above(c(" " = 2, " " = 1, "Viol. Protestas" = 2, "Viol. Estatal" = 2)) %>%
  footnote(
    general = "Escala 1-5 donde 1 = Nunca se justifica y 5 = Siempre se justifica",
    threeparttable = TRUE
  )

tabla_2
#
#
#
#
#
#
#| label: fig-evolucion-temporal
#| fig-cap: "Evolución de la participación en protestas 2016-2023"
#| fig-width: 12
#| fig-height: 7
#| echo: false

# Panel Participación
plot_evol_particip <- elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    Participacion = 100 * mean(protesta_dummy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = Participacion)) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_line(linewidth = 1.2, color = "#619CFF") +
  geom_point(size = 2.5, color = "#619CFF") +
  labs(
    title = "Evolución de Participación en Protestas",
    x = "Año",
    y = "% que Participó") +
  theme_minimal(base_size = 16)

# Combinar
grafico_evolucion <- plot_evol_particip +
  plot_annotation(
    tag_levels = 'A',
    caption = "Elaboración propia con ELSOC 2016-2023. Nota: 2019 = Estallido social",
    theme = theme(plot.caption = element_text(size = 11))
  )

# Mostrar gráfico
grafico_evolucion
#
#
#
#
#
#| label: fig-participacion-estratificada
#| fig-cap: "Composición sociodemográfica de participantes en protestas por año: cambios estructurales post-estallido. Elaboración propia a partir de ELSOC."
#| fig-width: 12
#| fig-height: 18
#| echo: false
#| warning: false

# Panel: Comparación directa Protestas vs Estatal por año
comparacion_data <- elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    `Viol. en Protestas` = mean(justif_violencia_protesta_consistente, na.rm = TRUE),
    `Viol. Estatal` = mean(justif_violencia_estatal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("Viol"), 
                      names_to = "Tipo", 
                      values_to = "Justificacion")

p3 <- ggplot(comparacion_data, aes(x = year, y = Justificacion, color = Tipo, group = Tipo)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3.5) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.6) +
  scale_color_manual(values = c("Viol. en Protestas" = "#EFC000FF", 
                                  "Viol. Estatal" = "#0073C2FF"),
                     name = "Tipo de violencia") +
  labs(
    title = "Divergencia entre Justificación de Violencia en Protestas y Violencia Estatal",
    subtitle = "Líneas cruzadas post-2019 revelan inversión de jerarquías morales: protestas más legitimadas que represión",
    x = "Año",
    y = "Justificación (escala 1-5)",
    caption = "Línea roja: estallido 2019. Elaboración propia con ELSOC 2016-2023."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 10, hjust = 0)
  ) +
  ylim(1.4, 2.1)

# Mostrar gráfico
p3
#
#
#
#
#
#
#
#| label: fig-prepost-estallido
#| fig-cap: "Comparación de justificación de violencia antes (2016-2018) y después (2022-2023) del estallido social por educación, clase y participación"
#| fig-width: 12
#| fig-height: 20
#| echo: false
#| warning: false

# Función para calcular medias e IC por grupo y año
calc_means_ci <- function(data, group_var, tipo_violencia) {
  data %>%
    filter(!is.na(year) & year != 2019 & !is.na(!!sym(group_var)) & !is.na(protesta_dummy)) %>%
    group_by(year, !!sym(group_var), protesta_dummy) %>%
    summarise(
      mean = mean(!!sym(tipo_violencia), na.rm = TRUE),
      se = sd(!!sym(tipo_violencia), na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ci_low = mean - 1.96 * se,
      ci_high = mean + 1.96 * se,
      participacion = factor(protesta_dummy, 
                            levels = c(0, 1),
                            labels = c("No participó", "Participó"))
    )
}

# Panel Educación × Violencia en Protestas
data_educ_prot <- calc_means_ci(elsoc_final_2, "educ_cat_unordered", "justif_violencia_protesta_consistente")

p1 <- ggplot(data_educ_prot, aes(x = educ_cat_unordered, y = mean, color = participacion, group = participacion)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  labs(title = "Educación × Violencia en Protestas",
       x = "Nivel Educativo",
       y = "Justificación (escala 1-5)",
       color = "") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", size = 12)) +
  ylim(1, 3)

# Panel Educación × Violencia Estatal
data_educ_est <- calc_means_ci(elsoc_final_2, "educ_cat_unordered", "justif_violencia_estatal")

p2 <- ggplot(data_educ_est, aes(x = educ_cat_unordered, y = mean, color = participacion, group = participacion)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  labs(title = "Educación × Violencia Estatal",
       x = "Nivel Educativo",
       y = "Justificación (escala 1-5)",
       color = "") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", size = 12)) +
  ylim(1, 3)

# Panel Clase × Violencia en Protestas
data_clase_prot <- calc_means_ci(elsoc_final_2, "egp3", "justif_violencia_protesta_consistente")

p3 <- ggplot(data_clase_prot, aes(x = egp3, y = mean, color = participacion, group = participacion)) +
  geom_point(position = position_dodge(width = 0.4), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.8) +
  geom_line(position = position_dodge(width = 0.4), linewidth = 1) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  labs(title = "Clase Social × Violencia en Protestas",
       x = "Clase Social (EGP)",
       y = "Justificación (escala 1-5)",
       color = "") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", size = 12)) +
  ylim(1, 3)

# Panel Clase × Violencia Estatal
data_clase_est <- calc_means_ci(elsoc_final_2, "egp3", "justif_violencia_estatal")

p4 <- ggplot(data_clase_est, aes(x = egp3, y = mean, color = participacion, group = participacion)) +
  geom_point(position = position_dodge(width = 0.4), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.8) +
  geom_line(position = position_dodge(width = 0.4), linewidth = 1) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  labs(title = "Clase Social × Violencia Estatal",
       x = "Clase Social (EGP)",
       y = "Justificación (escala 1-5)",
       color = "") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", size = 9)) +
  ylim(1, 3)

# Combinar los 2 paneles según clase educación
educ_plot <- (p1 / p2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = 'A',
    caption = "Puntos muestran medias; barras muestran IC 95%. Se excluye 2019 (año del estallido). Elaboración propia con ELSOC."
  ) &
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 7)
  )

class_plot <- (p3 / p4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = 'A',
    caption = "Puntos muestran medias; barras muestran IC 95%. Se excluye 2019 (año del estallido). Elaboración propia con ELSOC."
  ) &
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 7)
  )

# Mostrar el gráfico
educ_plot
class_plot
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| include: false

# ============================================
# MODELOS DE EDUCACIÓN
# ============================================

# Modelo 1a: Educación - Efectos principales para Violencia en Protestas
mod_educ_protesta_main <- glmmTMB(
  justif_violencia_protesta_consistente ~ educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 1b: Educación × Protesta para Violencia en Protestas (con interacción)
mod_educ_protesta_int <- glmmTMB(
  justif_violencia_protesta_consistente ~ educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 2a: Educación - Efectos principales para Violencia Estatal
mod_educ_estatal_main <- glmmTMB(
  justif_violencia_estatal ~ educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 2b: Educación × Protesta para Violencia Estatal (con interacción)
mod_educ_estatal_int <- glmmTMB(
  justif_violencia_estatal ~ educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# ============================================
# MODELOS DE CLASE SOCIAL
# ============================================

# Modelo 3a: Clase - Efectos principales para Violencia en Protestas
mod_clase_protesta_main <- glmmTMB(
  justif_violencia_protesta_consistente ~ egp3 + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 3b: Clase × Protesta para Violencia en Protestas (con interacción)
mod_clase_protesta_int <- glmmTMB(
  justif_violencia_protesta_consistente ~ egp3 * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 4a: Clase - Efectos principales para Violencia Estatal
mod_clase_estatal_main <- glmmTMB(
  justif_violencia_estatal ~ egp3 + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 4b: Clase × Protesta para Violencia Estatal (con interacción)
mod_clase_estatal_int <- glmmTMB(
  justif_violencia_estatal ~ egp3 * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)
#
#
#
#| label: tbl-modelos-principales
#| tbl-cap: "Modelos principales de justificación de violencia"
#| echo: false
#| warning: false
#| results: asis

# Custom coefficient map
ccoef <- list(
  "educ_cat_unorderedTéc. sup.incompleta" = "Tecnica Incompleta",
  "educ_cat_unorderedTéc. sup.completa" = "Tecnica completa",
  "educ_cat_unorderedUniv. incompleta" = "Universitaria Incompleta",
  "educ_cat_unorderedUniv. completa" = "Universitaria completa",
  "egp3Intermediate class (III+IV)" = "Clase Media (III+IV)",
  "egp3Service class (I+II)" = "Clase Servicios (I+II)",
  "protesta_dummy" = "Participación en Protestas",
  "edad" = "Edad",
  "mujer" = "Mujer",
  "ideologia_std" = "Ideología",
  "factor(year)2017" = "2017",
  "factor(year)2018" = "2018",
  "factor(year)2019" = "2019",
  "factor(year)2022" = "2022",
  "factor(year)2023" = "2023"
)

texreg(list(mod_educ_protesta_main, mod_educ_estatal_main, 
            mod_clase_protesta_main, mod_clase_estatal_main),
       custom.model.names = c("M1", "M2", "M3", "M4"),
       caption.above = TRUE,
       caption = "Main effects models for justification of violence",
       stars = c(0.05, 0.01, 0.001),
       omit.coef = "(Intercept)",
       custom.coef.map = ccoef,
       digits = 3,
       groups = list("Wave (Ref.= 2016)" = 1:5),
       custom.note = "Note: Cells contain regression coefficients with standard errors in parentheses. %stars.",
       leading.zero = TRUE,
       use.packages = FALSE,
       booktabs = TRUE,
       scalebox = 0.80,
       include.loglik = FALSE,
       include.aic = FALSE,
       center = TRUE,
       custom.gof.names = c("BIC", "Num. obs.", "Num. groups: individuals"),
       float.pos = "h")
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-modelos-interaccion
#| tbl-cap: "Modelos de interacción para justificación de violencia"
#| echo: false
#| warning: false
#| results: asis

# Custom coefficient map for interaction models
ccoef_int <- list(
  "educ_cat_unorderedTéc. sup.incompleta" = "Tecnica Incompleta",
  "educ_cat_unorderedTéc. sup.completa" = "Tecnica Incompleta",
  "educ_cat_unorderedUniv. incompleta" = "Universitaria Incompleta",
  "educ_cat_unorderedUniv. completa" = "Universitaria Incompleta",
  "egp3Intermediate class (III+IV)" = "Intermediate class (III+IV)",
  "egp3Service class (I+II)" = "Service class (I+II)",
  "protesta_dummy" = "Protest participation",
  "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy" = "Tecnica Incompleta × Protest",
  "educ_cat_unorderedTéc. sup.completa:protesta_dummy" = "Tecnica Incompleta × Protest",
  "educ_cat_unorderedUniv. incompleta:protesta_dummy" = "Universitaria Incompleta × Protest",
  "educ_cat_unorderedUniv. completa:protesta_dummy" = "Universitaria Incompleta × Protest",
  "egp3Intermediate class (III+IV):protesta_dummy" = "Intermediate class × Protest",
  "egp3Service class (I+II):protesta_dummy" = "Service class × Protest",
  "edad" = "Age",
  "mujer" = "Woman",
  "ideologia_std" = "Ideology (standardized)",
  "factor(year)2017" = "Wave 2017",
  "factor(year)2018" = "Wave 2018",
  "factor(year)2019" = "Wave 2019",
  "factor(year)2022" = "Wave 2022",
  "factor(year)2023" = "Wave 2023"
)

texreg(list(mod_educ_protesta_int, mod_educ_estatal_int, 
            mod_clase_protesta_int, mod_clase_estatal_int),
       custom.model.names = c("M1b", "M2b", "M3b", "M4b"),
       caption.above = TRUE,
       caption = "Interaction models for justification of violence",
       stars = c(0.05, 0.01, 0.001),
       omit.coef = "(Intercept)",
       custom.coef.map = ccoef_int,
       digits = 3,
       groups = list("Wave (Ref.= 2016)" = 1:5),
       custom.note = "Note: Cells contain regression coefficients with standard errors in parentheses. %stars.",
       leading.zero = TRUE,
       use.packages = FALSE,
       booktabs = TRUE,
       scalebox = 0.80,
       include.loglik = FALSE,
       include.aic = FALSE,
       center = TRUE,
       custom.gof.names = c("BIC", "Num. obs.", "Num. groups: individuals"),
       float.pos = "h")
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-efectos-marginales-interaccion
#| fig-cap: "Efectos marginales de la participación en protestas sobre justificación de violencia según educación y clase social"
#| fig-width: 14
#| fig-height: 10
#| echo: false
#| warning: false

# ===================================================================
# PANEL A: Efecto marginal de participación según EDUCACIÓN 
# para Violencia en PROTESTAS
# ===================================================================

# Calcular efectos marginales manualmente
# Efecto marginal = β_protesta + β_interacción * nivel_educación

# Extraer coeficientes del modelo
coef_educ_prot <- fixef(mod_educ_protesta_int)$cond

# Efecto marginal base (educación media = categoría de referencia)
me_base <- coef_educ_prot["protesta_dummy"]

# Efectos marginales para cada nivel educativo
me_data_educ_prot <- data.frame(
  educacion = c("Media o menos", "Téc. sup.\nincompleta", "Téc. sup.\ncompleta", 
                "Univ.\nincompleta", "Univ.\ncompleta"),
  me = c(
    me_base,  # Media (referencia)
    me_base + coef_educ_prot["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"],  # Téc inc
    me_base + coef_educ_prot["educ_cat_unorderedTéc. sup.completa:protesta_dummy"],     # Téc comp
    me_base + coef_educ_prot["educ_cat_unorderedUniv. incompleta:protesta_dummy"],      # Univ inc
    me_base + coef_educ_prot["educ_cat_unorderedUniv. completa:protesta_dummy"]         # Univ comp
  )
)

# Calcular errores estándar aproximados usando vcov
vcov_mat <- vcov(mod_educ_protesta_int)$cond

# SE para categoría de referencia (solo varianza de protesta_dummy)
se_base <- sqrt(vcov_mat["protesta_dummy", "protesta_dummy"])

# Para interacciones: SE = sqrt(Var(β1) + Var(β2) + 2*Cov(β1,β2))
se_tec_inc <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy", 
           "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"]
)

se_tec_comp <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedTéc. sup.completa:protesta_dummy", 
           "educ_cat_unorderedTéc. sup.completa:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedTéc. sup.completa:protesta_dummy"]
)

se_univ_inc <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedUniv. incompleta:protesta_dummy", 
           "educ_cat_unorderedUniv. incompleta:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedUniv. incompleta:protesta_dummy"]
)

se_univ_comp <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedUniv. completa:protesta_dummy", 
           "educ_cat_unorderedUniv. completa:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedUniv. completa:protesta_dummy"]
)

me_data_educ_prot$se <- c(se_base, se_tec_inc, se_tec_comp, se_univ_inc, se_univ_comp)
me_data_educ_prot$ci_low <- me_data_educ_prot$me - 1.96 * me_data_educ_prot$se
me_data_educ_prot$ci_high <- me_data_educ_prot$me + 1.96 * me_data_educ_prot$se
me_data_educ_prot$significativo <- !(me_data_educ_prot$ci_low < 0 & me_data_educ_prot$ci_high > 0)

# Reordenar niveles
me_data_educ_prot$educacion <- factor(me_data_educ_prot$educacion, 
                                       levels = c("Media o menos", "Téc. sup.\nincompleta", 
                                                  "Téc. sup.\ncompleta", "Univ.\nincompleta", 
                                                  "Univ.\ncompleta"))

# Gráfico Panel A
panel_a <- ggplot(me_data_educ_prot, aes(x = educacion, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significativo), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo), 
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#00BFC4"), 
                     guide = "none") +
  labs(title = "A) Violencia en Protestas: Efecto Marginal de Participación\n por Educación",
       x = "Nivel Educativo",
       y = "Efecto Marginal de Participación\n(cambio en justificación)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

# ===================================================================
# PANEL B: Efecto marginal de participación según EDUCACIÓN 
# para Violencia ESTATAL
# ===================================================================

coef_educ_est <- fixef(mod_educ_estatal_int)$cond
me_base_est <- coef_educ_est["protesta_dummy"]

me_data_educ_est <- data.frame(
  educacion = c("Media o menos", "Téc. sup.\nincompleta", "Téc. sup.\ncompleta", 
                "Univ.\nincompleta", "Univ.\ncompleta"),
  me = c(
    me_base_est,
    me_base_est + coef_educ_est["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedTéc. sup.completa:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedUniv. incompleta:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedUniv. completa:protesta_dummy"]
  )
)

vcov_mat_est <- vcov(mod_educ_estatal_int)$cond
se_base_est <- sqrt(vcov_mat_est["protesta_dummy", "protesta_dummy"])

se_tec_inc_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy", 
               "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"]
)

se_tec_comp_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedTéc. sup.completa:protesta_dummy", 
               "educ_cat_unorderedTéc. sup.completa:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedTéc. sup.completa:protesta_dummy"]
)

se_univ_inc_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedUniv. incompleta:protesta_dummy", 
               "educ_cat_unorderedUniv. incompleta:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedUniv. incompleta:protesta_dummy"]
)

se_univ_comp_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedUniv. completa:protesta_dummy", 
               "educ_cat_unorderedUniv. completa:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedUniv. completa:protesta_dummy"]
)

me_data_educ_est$se <- c(se_base_est, se_tec_inc_est, se_tec_comp_est, se_univ_inc_est, se_univ_comp_est)
me_data_educ_est$ci_low <- me_data_educ_est$me - 1.96 * me_data_educ_est$se
me_data_educ_est$ci_high <- me_data_educ_est$me + 1.96 * me_data_educ_est$se
me_data_educ_est$significativo <- !(me_data_educ_est$ci_low < 0 & me_data_educ_est$ci_high > 0)

me_data_educ_est$educacion <- factor(me_data_educ_est$educacion, 
                                      levels = c("Media o menos", "Téc. sup.\nincompleta", 
                                                 "Téc. sup.\ncompleta", "Univ.\nincompleta", 
                                                 "Univ.\ncompleta"))

panel_b <- ggplot(me_data_educ_est, aes(x = educacion, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significativo), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo), 
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#619CFF"), 
                     guide = "none") +
  labs(title = "B) Violencia Estatal: Efecto Marginal de Participación\n por Educación",
       x = "Nivel Educativo",
       y = "Efecto Marginal de Participación\n(cambio en justificación)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

# ===================================================================
# PANEL C: Efecto marginal de participación según CLASE SOCIAL
# para Violencia en PROTESTAS
# ===================================================================

coef_clase_prot <- fixef(mod_clase_protesta_int)$cond
me_base_clase_prot <- coef_clase_prot["protesta_dummy"]

me_data_clase_prot <- data.frame(
  clase = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", "Working\nclass (V+VI+VII)"),
  me = c(
    me_base_clase_prot,
    me_base_clase_prot + coef_clase_prot["egp3Intermediate class (III+IV):protesta_dummy"],
    me_base_clase_prot + coef_clase_prot["egp3Working class (V+VI+VII):protesta_dummy"]
  )
)

vcov_mat_clase_prot <- vcov(mod_clase_protesta_int)$cond
se_base_clase_prot <- sqrt(vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"])

se_int_clase <- sqrt(
  vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_prot["egp3Intermediate class (III+IV):protesta_dummy", 
                      "egp3Intermediate class (III+IV):protesta_dummy"] +
  2 * vcov_mat_clase_prot["protesta_dummy", "egp3Intermediate class (III+IV):protesta_dummy"]
)

se_work_clase <- sqrt(
  vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_prot["egp3Working class (V+VI+VII):protesta_dummy", 
                      "egp3Working class (V+VI+VII):protesta_dummy"] +
  2 * vcov_mat_clase_prot["protesta_dummy", "egp3Working class (V+VI+VII):protesta_dummy"]
)

me_data_clase_prot$se <- c(se_base_clase_prot, se_int_clase, se_work_clase)
me_data_clase_prot$ci_low <- me_data_clase_prot$me - 1.96 * me_data_clase_prot$se
me_data_clase_prot$ci_high <- me_data_clase_prot$me + 1.96 * me_data_clase_prot$se
me_data_clase_prot$significativo <- !(me_data_clase_prot$ci_low < 0 & me_data_clase_prot$ci_high > 0)

me_data_clase_prot$clase <- factor(me_data_clase_prot$clase, 
                                    levels = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", 
                                               "Working\nclass (V+VI+VII)"))

panel_c <- ggplot(me_data_clase_prot, aes(x = clase, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significativo), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo), 
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#00BFC4"), 
                     guide = "none") +
  labs(title = "C) Violencia en Protestas: Efecto Marginal de Participación\n por Clase Social",
       x = "Clase Social (EGP)",
       y = "Efecto Marginal de Participación\n(cambio en justificación)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

# ===================================================================
# PANEL D: Efecto marginal de participación según CLASE SOCIAL
# para Violencia ESTATAL
# ===================================================================

coef_clase_est <- fixef(mod_clase_estatal_int)$cond
me_base_clase_est <- coef_clase_est["protesta_dummy"]

me_data_clase_est <- data.frame(
  clase = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", "Working\nclass (V+VI+VII)"),
  me = c(
    me_base_clase_est,
    me_base_clase_est + coef_clase_est["egp3Intermediate class (III+IV):protesta_dummy"],
    me_base_clase_est + coef_clase_est["egp3Working class (V+VI+VII):protesta_dummy"]
  )
)

vcov_mat_clase_est <- vcov(mod_clase_estatal_int)$cond
se_base_clase_est <- sqrt(vcov_mat_clase_est["protesta_dummy", "protesta_dummy"])

se_int_clase_est <- sqrt(
  vcov_mat_clase_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_est["egp3Intermediate class (III+IV):protesta_dummy", 
                     "egp3Intermediate class (III+IV):protesta_dummy"] +
  2 * vcov_mat_clase_est["protesta_dummy", "egp3Intermediate class (III+IV):protesta_dummy"]
)

se_work_clase_est <- sqrt(
  vcov_mat_clase_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_est["egp3Working class (V+VI+VII):protesta_dummy", 
                     "egp3Working class (V+VI+VII):protesta_dummy"] +
  2 * vcov_mat_clase_est["protesta_dummy", "egp3Working class (V+VI+VII):protesta_dummy"]
)

me_data_clase_est$se <- c(se_base_clase_est, se_int_clase_est, se_work_clase_est)
me_data_clase_est$ci_low <- me_data_clase_est$me - 1.96 * me_data_clase_est$se
me_data_clase_est$ci_high <- me_data_clase_est$me + 1.96 * me_data_clase_est$se
me_data_clase_est$significativo <- !(me_data_clase_est$ci_low < 0 & me_data_clase_est$ci_high > 0)

me_data_clase_est$clase <- factor(me_data_clase_est$clase, 
                                   levels = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", 
                                              "Working\nclass (V+VI+VII)"))

panel_d <- ggplot(me_data_clase_est, aes(x = clase, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significativo), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo), 
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#619CFF"), 
                     guide = "none") +
  labs(title = "D) Violencia Estatal: Efecto Marginal de Participación\n por Clase Social",
       x = "Clase Social (EGP)",
       y = "Efecto Marginal de Participación\n(cambio en justificación)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

# Combinar paneles
combined_marginal_effects <- (panel_a | panel_b) / (panel_c | panel_d) +
  plot_annotation(
    caption = "Efectos marginales con IC 95%. Línea roja indica efecto nulo. Puntos grises: no significativos. \nEfecto marginal = cambio en justificación al participar vs no participar, condicionado por educación/clase. Elaboración propia con ELSOC.",
    theme = theme(plot.caption = element_text(size = 9, hjust = 0))
  )

combined_marginal_effects
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-paradoja-educacion
#| fig-cap: "Análisis de interacción: Educación × Participación en Protestas"
#| fig-width: 12
#| fig-height: 9
#| echo: false
#| warning: false

# Generar predicciones del modelo de educación con interacciones
pred_df <- ggpredict(mod_educ_protesta_int, 
                     terms = c("educ_cat_unordered", "protesta_dummy")) %>%
  as.data.frame() %>%
  rename(participacion = group) %>%
  mutate(participacion = factor(participacion, 
                                 levels = c("0", "1"),
                                 labels = c("No participó", "Participó")))

# Panel A: Predicciones
panel_a <- ggplot(pred_df, aes(x = x, y = predicted, 
                                color = participacion, group = participacion, 
                                fill = participacion)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  scale_fill_manual(values = c("No participó" = "#F8766D", "Participó" = "#00BFC4")) +
  labs(
    title = "A) Valores Predichos",
    x = "Nivel Educativo",
    y = "Justificación (predicha)",
    color = "Participación",
    fill = "Participación"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 13, face = "bold")
  )

# Panel B: Magnitud de la interacción
interaction_df <- pred_df %>%
  select(x, participacion, predicted) %>%
  pivot_wider(names_from = participacion, values_from = predicted) %>%
  mutate(Diferencia = `Participó` - `No participó`)

panel_b <- ggplot(interaction_df, aes(x = x, y = Diferencia)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_col(fill = "#619CFF", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = round(Diferencia, 2)), 
            vjust = ifelse(interaction_df$Diferencia > 0, -0.5, 1.5),
            size = 2.5) +
  labs(
    title = "B) Magnitud de la Interacción",
    x = "Nivel Educativo",
    y = "Diferencia\n(Participó - No Participó)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 13, face = "bold")
  )

# Combinar
plot_final <- panel_a / panel_b +
  plot_annotation(
    caption = "Panel A muestra efectos por grupo; Panel B cuantifica la interacción. Elaboración propia con ELSOC 2016-2023.",
    theme = theme(plot.caption = element_text(size = 7, hjust = 0))
  )

# Mostrar gráfico
plot_final
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-bidireccional-tiempo
#| fig-cap: "Evolución temporal de la reconfiguración bidireccional según educación y participación (2016-2023)"
#| fig-width: 12
#| fig-height: 14
#| dpi: 300
#| echo: false
#| warning: false

# Obtener predicciones para violencia en protestas
pred_protestas_tiempo <- ggpredict(mod_educ_protesta_int, 
                                   terms = c("year", "protesta_dummy", "educ_cat_unordered [Media completa o menos, Téc. sup.incompleta, Téc. sup.completa, Univ. completa]"),
                                   typical = "mean")

# Obtener predicciones para violencia estatal
pred_estatal_tiempo <- ggpredict(mod_educ_estatal_int, 
                                 terms = c("year", "protesta_dummy", "educ_cat_unordered [Media completa o menos, Téc. sup.incompleta, Téc. sup.completa, Univ. completa]"),
                                 typical = "mean")

# Convertir a dataframes
pred_protestas_df <- as.data.frame(pred_protestas_tiempo)
pred_protestas_df$tipo_violencia <- "Violencia en Protestas"
pred_protestas_df$participacion <- factor(pred_protestas_df$group, 
                                          labels = c("No participó", "Participó"))
pred_protestas_df$educacion <- pred_protestas_df$facet

pred_estatal_df <- as.data.frame(pred_estatal_tiempo)
pred_estatal_df$tipo_violencia <- "Violencia Estatal"
pred_estatal_df$participacion <- factor(pred_estatal_df$group, 
                                        labels = c("No participó", "Participó"))
pred_estatal_df$educacion <- pred_estatal_df$facet

# Combinar ambos datasets
pred_combined <- rbind(pred_protestas_df, pred_estatal_df)

# Crear gráfico con facetas por educación y participación
ggplot(pred_combined, aes(x = x, y = predicted, color = tipo_violencia, group = tipo_violencia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = tipo_violencia), 
              alpha = 0.15, color = NA) +
  facet_grid(participacion ~ educacion, 
             labeller = labeller(
               participacion = c("No participó" = "No Participantes", 
                                "Participó" = "Participantes")
             )) +
  scale_color_manual(values = c("Violencia en Protestas" = "#EFC000FF", 
                                 "Violencia Estatal" = "#0073C2FF")) +
  scale_fill_manual(values = c("Violencia en Protestas" = "#EFC000FF", 
                                "Violencia Estatal" = "#0073C2FF")) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Reconfiguración Bidireccional: Evolución Temporal por Educación y Participación",
    x = "Año",
    y = "Justificación de Violencia (predicha)",
    color = "Tipo de Violencia",
    fill = "Tipo de Violencia",
    caption = "Línea roja punteada: 2019 (Estallido social). Áreas sombreadas: IC 95%. Otros predictores en valores medios.\nElaboración propia con ELSOC 2016-2023."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.caption = element_text(size = 7)
  ) +
  ylim(1.3, 2.4)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-clase-protesta
#| fig-cap: "Efecto de participar en protestas según clase social"
#| fig-width: 12
#| fig-height: 8
#| echo: false
#| warning: false

# Predicciones
pred_clase <- ggpredict(mod_clase_protesta_int, terms = c("protesta_dummy", "egp3"))

grafico_clase_protesta <- plot(pred_clase) +
  labs(
    title = "Justificación de Violencia en Protestas según Clase Social y Participación",
    subtitle = "Service class experimenta mayor cambio al participar; Working class muestra efecto techo",
    x = "Participación en Protestas",
    y = "Justificación de Violencia (predicha)",
    color = "Clase Social",
    caption = "Elaboración propia con ELSOC 2016-2023"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 10))

# Mostrar gráfico
grafico_clase_protesta
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-clase-estatal
#| fig-cap: "Efecto de participar en protestas según clase social"
#| fig-width: 12
#| fig-height: 8
#| echo: false
#| warning: false

# Predicciones
pred_clase <- ggpredict(mod_clase_estatal_int, terms = c("protesta_dummy", "egp3"))

plot(pred_clase) +
  labs(
    title = "Justificación de Violencia Estatal según Clase Social y Participación",
    subtitle = "Service class experimenta mayor cambio al participar; Working class muestra efecto techo",
    x = "Participación en Protestas",
    y = "Justificación de Violencia Estatal (predicha)",
    color = "Clase Social",
    caption = "Elaboración propia con ELSOC 2016-2023"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 10))
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-clase-tiempo
#| fig-cap: "Evolución temporal del efecto de clase según participación y tipo de violencia (2016-2023)"
#| fig-width: 12
#| fig-height: 14
#| dpi: 300
#| echo: false
#| warning: false

# Predicciones para violencia en protestas
pred_clase_protestas <- ggpredict(mod_clase_protesta_int, 
                                  terms = c("year", "protesta_dummy", "egp3"),
                                  typical = "mean")

# Predicciones para violencia estatal
pred_clase_estatal <- ggpredict(mod_clase_estatal_int, 
                                terms = c("year", "protesta_dummy", "egp3"),
                                typical = "mean")

# Convertir a dataframes
pred_clase_prot_df <- as.data.frame(pred_clase_protestas)
pred_clase_prot_df$tipo_violencia <- "Violencia en Protestas"
pred_clase_prot_df$participacion <- factor(pred_clase_prot_df$group, 
                                           labels = c("No participó", "Participó"))
pred_clase_prot_df$clase <- pred_clase_prot_df$facet

pred_clase_est_df <- as.data.frame(pred_clase_estatal)
pred_clase_est_df$tipo_violencia <- "Violencia Estatal"
pred_clase_est_df$participacion <- factor(pred_clase_est_df$group, 
                                          labels = c("No participó", "Participó"))
pred_clase_est_df$clase <- pred_clase_est_df$facet

# Combinar ambos datasets
pred_clase_combined <- rbind(pred_clase_prot_df, pred_clase_est_df)

# Crear gráfico con facetas por clase y participación
ggplot(pred_clase_combined, aes(x = x, y = predicted, color = tipo_violencia, group = tipo_violencia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = tipo_violencia), 
              alpha = 0.15, color = NA) +
  facet_grid(participacion ~ clase, 
             labeller = labeller(
               participacion = c("No participó" = "No Participantes", 
                                "Participó" = "Participantes")
             )) +
  scale_color_manual(values = c("Violencia en Protestas" = "#EFC000FF", 
                                 "Violencia Estatal" = "#0073C2FF")) +
  scale_fill_manual(values = c("Violencia en Protestas" = "#EFC000FF", 
                                "Violencia Estatal" = "#0073C2FF")) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Reconfiguración Bidireccional: Evolución Temporal por Clase Social y Participación",
    x = "Año",
    y = "Justificación de Violencia (predicha)",
    color = "Tipo de Violencia",
    fill = "Tipo de Violencia",
    caption = "Línea roja punteada: 2019 (Estallido social). Áreas sombreadas: IC 95%. Otros predictores en valores medios.\nElaboración propia con ELSOC 2016-2023."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11),
    plot.caption = element_text(size = 10)
  ) +
  ylim(1.3, 2.4)
#
#
#
#
#
#
#
#
#
#
#
#
