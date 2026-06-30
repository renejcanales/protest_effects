# ==============================================================================
# Data Processing Script
# Project: Attitudes toward political violence in Chile (ELSOC 2016–2023)
# Description: Prepares the analytical dataset from raw ELSOC panel data.
#              Produces elsoc_final_2.RData used in all regression models.
#
# Raw data source: ELSOC longitudinal survey (COES)
#   https://coes.cl/elsoc/
#   https://dataverse.harvard.edu/dataverse/elsoc
# ==============================================================================

# 1. Packages ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  tidyr,
  here,
  naniar
)

options(scipen = 999)
rm(list = ls())

# 2. Load raw data -------------------------------------------------------------
# Download 'elsoc_long_2016_2023.RData' from the COES Harvard Dataverse and
# place it in input/data/raw/ before running this script.

load(here("input/data/raw/elsoc_long_2016_2023.RData"))
elsoc_long <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# 3. Variable selection --------------------------------------------------------

elsoc_long_select <- elsoc_long %>%
  dplyr::select(
    idencuesta,
    tipo_atricion,
    ola,
    genero        = m0_sexo,
    edad          = m0_edad,
    educ_encuestado = m01,
    educ_padre    = m27,
    educ_madre    = m28,
    ideologia     = c15,
    asist_marcha  = c08_02,
    part_huelga   = c08_03,
    part_cacerol  = c08_05,
    firma_peti    = c08_01,
    violencia_carabineros_marchas = f05_03,
    violencia_carabineros_tomas   = f05_04,
    violencia_trabajadores        = f05_06,
    violencia_estudiantes         = f05_07,
    violencia_inmobiliario        = f05_09,
    violencia_transporte          = f05_10,
    violencia_locales             = f05_11
  ) %>%
  as_tibble()

# 4. Missing value codes -------------------------------------------------------
# ELSOC uses -999, -888, -777, -666 for different types of non-response

elsoc_long_select[elsoc_long_select == -999] <- NA
elsoc_long_select[elsoc_long_select == -888] <- NA
elsoc_long_select[elsoc_long_select == -777] <- NA
elsoc_long_select[elsoc_long_select == -666] <- NA

elsoc_clean <- elsoc_long_select %>%
  mutate(across(everything(), ~na_if(., -999))) %>%
  mutate(across(everything(), ~na_if(., -888))) %>%
  mutate(across(everything(), ~na_if(., -777)))

# 5. Protest participation variable -------------------------------------------

elsoc_clean <- elsoc_clean %>%
  mutate(
    # Dichotomous: participated in marches/demonstrations (past 12 months)
    protesta_dummy = if_else(asist_marcha >= 2, 1L, 0L, missing = NA_integer_)
  )

# 6. Education (respondent) ---------------------------------------------------
# PASO 6.1: Carry forward maximum education attained across waves (time-stable)

elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%
  mutate(
    educ_encuestado = max(educ_encuestado, na.rm = TRUE),
    educ_encuestado = if_else(is.infinite(educ_encuestado), NA_real_, educ_encuestado)
  ) %>%
  ungroup()

# PASO 6.2: Collapsed education categories (5 levels)
elsoc_clean <- elsoc_clean %>%
  mutate(
    educ_cat = case_when(
      educ_encuestado %in% 1:5  ~ "Media completa o menos",
      educ_encuestado == 6      ~ "Técnica superior incompleta",
      educ_encuestado == 7      ~ "Técnica superior completa",
      educ_encuestado == 8      ~ "Universitaria incompleta",
      educ_encuestado %in% 9:10 ~ "Universitaria completa",
      TRUE                      ~ NA_character_
    ),
    educ_cat_factor = factor(
      educ_cat,
      levels = c(
        "Media completa o menos",
        "Técnica superior incompleta",
        "Técnica superior completa",
        "Universitaria incompleta",
        "Universitaria completa"
      ),
      ordered = TRUE
    ),
    # Approximate years of schooling
    educ_years = case_when(
      educ_encuestado == 1  ~  0,
      educ_encuestado == 2  ~  4,
      educ_encuestado == 3  ~  8,
      educ_encuestado == 4  ~ 10,
      educ_encuestado == 5  ~ 12,
      educ_encuestado == 6  ~ 13,
      educ_encuestado == 7  ~ 14,
      educ_encuestado == 8  ~ 14,
      educ_encuestado == 9  ~ 17,
      educ_encuestado == 10 ~ 19,
      TRUE                  ~ NA_real_
    )
  )

# 7. Parental education -------------------------------------------------------
# Only collected in wave 1; propagate across all waves for each respondent

elsoc_clean <- elsoc_clean %>%
  group_by(idencuesta) %>%
  fill(educ_padre, educ_madre, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    educ_padre_years = case_when(
      educ_padre == 1  ~  0, educ_padre == 2  ~  4, educ_padre == 3  ~  8,
      educ_padre == 4  ~ 10, educ_padre == 5  ~ 12, educ_padre == 6  ~ 13,
      educ_padre == 7  ~ 14, educ_padre == 8  ~ 14, educ_padre == 9  ~ 17,
      educ_padre == 10 ~ 19, TRUE             ~ NA_real_
    ),
    educ_madre_years = case_when(
      educ_madre == 1  ~  0, educ_madre == 2  ~  4, educ_madre == 3  ~  8,
      educ_madre == 4  ~ 10, educ_madre == 5  ~ 12, educ_madre == 6  ~ 13,
      educ_madre == 7  ~ 14, educ_madre == 8  ~ 14, educ_madre == 9  ~ 17,
      educ_madre == 10 ~ 19, TRUE             ~ NA_real_
    ),
    educ_parental_max = pmax(educ_padre_years, educ_madre_years, na.rm = TRUE),
    educ_parental_max = if_else(
      is.na(educ_padre_years) & is.na(educ_madre_years),
      NA_real_,
      educ_parental_max
    )
  )

# 8. Educational mobility (intergenerational) ---------------------------------

elsoc_clean <- elsoc_clean %>%
  mutate(
    movilidad_years = educ_years - educ_parental_max,
    movilidad_cat = case_when(
      movilidad_years < -2                        ~ "Descendente",
      movilidad_years >= -2 & movilidad_years <= 2 ~ "Sin movilidad",
      movilidad_years > 2  & movilidad_years <= 5  ~ "Ascendente moderada",
      movilidad_years > 5                          ~ "Ascendente alta",
      TRUE                                         ~ NA_character_
    ),
    movilidad_cat_factor = factor(
      movilidad_cat,
      levels = c("Descendente", "Sin movilidad", "Ascendente moderada", "Ascendente alta"),
      ordered = TRUE
    )
  )

# 9. Control variables ---------------------------------------------------------

elsoc_clean <- elsoc_clean %>%
  mutate(
    mujer       = if_else(genero == 2, 1L, 0L),
    # Ideology: 0 (far left) to 10 (far right); out-of-range set to NA
    ideologia_std = if_else(ideologia >= 0 & ideologia <= 10, ideologia, NA_real_)
  ) %>%
  # Carry ideology forward/backward within individual (time-stable proxy)
  group_by(idencuesta) %>%
  fill(ideologia_std, .direction = "downup") %>%
  ungroup()

# 10. Survey wave and temporal context ----------------------------------------

elsoc_clean <- elsoc_clean %>%
  mutate(
    year = case_when(
      ola == 1 ~ 2016, ola == 2 ~ 2017, ola == 3 ~ 2018,
      ola == 4 ~ 2019, ola == 5 ~ 2021, ola == 6 ~ 2022,
      ola == 7 ~ 2023, TRUE ~ NA_real_
    ),
    post_estallido = if_else(year >= 2019, 1L, 0L),
    periodo = case_when(
      year <= 2018 ~ "Pre-estallido",
      year == 2019 ~ "Estallido",
      year >= 2021 ~ "Post-estallido",
      TRUE         ~ NA_character_
    )
  )

# 11. Violence justification indices ------------------------------------------
# NOTE: violence items were not collected in 2021 (ola 5); those observations
# will have NA outcomes and will be excluded from models via listwise deletion.

elsoc_clean <- elsoc_clean %>%
  mutate(
    # State violence: Carabineros using force against demonstrations / occupations
    justif_violencia_estatal = rowMeans(
      dplyr::select(., violencia_carabineros_marchas, violencia_carabineros_tomas),
      na.rm = TRUE
    ),
    # Protest violence (consistent 6-wave index): workers + students items only
    # (the only two protest violence items available in ALL six analytical waves)
    justif_violencia_protesta_consistente = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes),
      na.rm = TRUE
    ),
    # Broad protest violence index (additional items, 2019 and 2022-2023 only)
    justif_violencia_protesta_amplio = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes,
                    violencia_inmobiliario, violencia_transporte, violencia_locales),
      na.rm = TRUE
    )
  )

# 12. Panel inclusion filter --------------------------------------------------
# Include only individuals with at least 3 completed waves

individuos_min_3_olas <- elsoc_clean %>%
  group_by(idencuesta) %>%
  summarise(n_olas = n(), .groups = "drop") %>%
  filter(n_olas >= 3) %>%
  pull(idencuesta)

cat("Individuals meeting >=3 wave criterion:", length(individuos_min_3_olas), "\n")

elsoc_analisis <- elsoc_clean %>%
  filter(
    idencuesta %in% individuos_min_3_olas,
    edad >= 18 & edad <= 75,
    !is.na(educ_encuestado)
  )

cat("Observations in panel (pre-listwise):", nrow(elsoc_analisis), "\n")
cat("Unique individuals (pre-listwise):",    n_distinct(elsoc_analisis$idencuesta), "\n")

# 13. Within-between (REWB) participation variables ---------------------------
# Decompose protest participation into person-mean (between) and
# within-person deviation from own mean

elsoc_analisis <- elsoc_analisis %>%
  group_by(idencuesta) %>%
  mutate(
    protesta_mean   = mean(protesta_dummy, na.rm = TRUE),
    protesta_within = protesta_dummy - protesta_mean
  ) %>%
  ungroup()

# 14. Final variable selection ------------------------------------------------

elsoc_final_2 <- elsoc_analisis %>%
  dplyr::select(
    # Identifiers and wave structure
    idencuesta, ola, year, tipo_atricion,

    # Protest participation
    protesta_dummy, protesta_mean, protesta_within,

    # Education
    educ_cat, educ_cat_factor, educ_years,
    educ_parental_max, movilidad_years, movilidad_cat, movilidad_cat_factor,

    # Controls
    edad, mujer, ideologia_std,

    # Temporal context
    post_estallido, periodo,

    # Violence items (raw)
    violencia_trabajadores, violencia_estudiantes,
    violencia_inmobiliario, violencia_transporte, violencia_locales,
    violencia_carabineros_marchas, violencia_carabineros_tomas,

    # Violence indices
    justif_violencia_estatal,
    justif_violencia_protesta_consistente,
    justif_violencia_protesta_amplio
  )

# 15. Diagnostics and save ----------------------------------------------------

cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Total observations:", nrow(elsoc_final_2), "\n")
cat("Unique individuals:", n_distinct(elsoc_final_2$idencuesta), "\n")
cat("Waves:", sort(unique(elsoc_final_2$year)), "\n")

cat("\nObservations per wave:\n")
print(table(elsoc_final_2$year))

cat("\nProtesta dummy missing (2021 expected):\n")
print(elsoc_final_2 %>%
  group_by(year) %>%
  summarise(n_missing_protest_outcome = sum(is.na(justif_violencia_protesta_consistente))))

# Switcher diagnostics
switchers <- elsoc_final_2 %>%
  filter(!is.na(protesta_dummy)) %>%
  group_by(idencuesta) %>%
  summarise(
    ever_participated     = any(protesta_dummy == 1),
    ever_not_participated = any(protesta_dummy == 0),
    .groups = "drop"
  ) %>%
  filter(ever_participated & ever_not_participated)

cat("\nSwitchers (changed participation status at least once):", nrow(switchers), "\n")
cat("Switchers as % of panel:", round(100 * nrow(switchers) / n_distinct(elsoc_final_2$idencuesta), 1), "%\n")

save(elsoc_final_2, file = here("input/data/proc/elsoc_final_2.RData"))
cat("\nData saved to input/data/proc/elsoc_final_2.RData\n")
