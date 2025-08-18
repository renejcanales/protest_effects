#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for research paper
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

load("~/GitHub/protest_effects/output/data/ELSOC_Long_2016_2023 (3).RData")

elsoc_long <- elsoc_long_2016_2023

rm(elsoc_long_2016_2023)

glimpse(elsoc_long)

# 3. Processing -----------------------------------------------------------

# select ----

elsoc_long <- elsoc_long %>% 
  select(idencuesta, tipo_caso,
         ola,
         comuna,
         comuna_cod,
         region, region_cod,
         sexo = m0_sexo,
         edad = m0_edad,
         educacion = m01,
         ocupacion = m03,
         conf_gob = c05_01,
         conf_part = c05_02,
         conf_cong = c05_07,
         conf_pres = c05_08,
         efic_voto = c10_01,
         efic_result = c10_02,
         efic_expr = c10_03,
         frq_marcha = c08_02,
         ideologia = c15)

elsoc_long <- elsoc_long %>% select(-n_miss_all)

sapply(elsoc_long, class)
save(elsoc_long, file = here("protest_effects/output/data/elsoc_proc.RData"))


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