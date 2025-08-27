# 0. Identification ---------------------------------------------------

# Title: Data preparation
# Institution: OCS
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data preparation for analysis of cohesion and migration
# Date: Agust 26, 2025

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               car,
               sjmisc, 
               here,
               sjlabelled,
               SciViews,
               naniar,
               readxl,
               sjPlot)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

glimpse(elsoc_long_2016_2023)

# 3. Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

# 3.1 Select ----

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola, 
         cuestion_mig,
         ponderador_long_total, 
         segmento, 
         estrato,
         educacion = m01, 
         sexo = m0_sexo, 
         edad = m0_edad, 
         seguridad_sat = t06_01, 
         seguridad_perc = t10, 
         peleas_calle = t09_01,
         asaltos = t09_02,
         trafico_drogas = t09_03,
         barrio_ideal = t02_01,
         barrio_integracion = t02_02, 
         barrio_identidad = t02_03, 
         barrio_pertenencia = t02_04, 
         barrio_amigos = t03_01, 
         barrio_sociable = t03_02, 
         barrio_cordial = t03_03, 
         barrio_colaborador = t03_04, 
         confianza_gen = c02,
         altruismo_gen = c03,
         reunion_pub = c07_02, 
         voluntariado = c07_04,
         donar_dinero = c07_05, 
         prestar_dinero = c07_06, 
         ayuda_trabajo = c07_08,
         frecuencia_migrantes = r06,
         contacto_migrantes = r07, 
         simpatia_migrantes = r09,
         perdida_identidad = r12_03, 
         desempleo_migrantes = r12_04,
         confianza_migrantes = r16, 
         fomentar_migracion = r18_01,
         igualdad_migrantes = r18_02) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# 3.2 Filter ----
# No

# 3.3 Recode and transform ----

# Wave
db <- db %>% 
  mutate(wave = case_when(ola == 1 ~ "2016",
                          ola == 2 ~ "2017",
                          ola == 3 ~ "2018",
                          ola == 4 ~ "2019",
                          ola == 5 ~ "2022",
                          ola == 6 ~ "2023"),
         wave = factor(wave, levels = c("2016",
                                        "2017",
                                        "2018",
                                        "2019",
                                        "2022",
                                        "2023")))

# comportamiento_prosocial

db %>% 
  group_by(wave) %>% 
  select(reunion_pub, voluntariado) %>% 
  frq()

db$comportamiento_prosocial <- rowMeans(db[, c("reunion_pub", "voluntariado")], na.rm = TRUE)

frq(db$comportamiento_prosocial)
# ayuda_economica

db %>% 
  group_by(wave) %>% 
  select(prestar_dinero, ayuda_trabajo) %>% 
  frq()

db$ayuda_economica <- rowMeans(db[, c("prestar_dinero", "ayuda_trabajo")], na.rm = TRUE)

frq(db$ayuda_economica)
# confianza_inter

db %>% 
  group_by(wave) %>% 
  select(confianza_gen, altruismo_gen) %>% 
  frq()

db$confianza_inter <- rowMeans(db[, c("confianza_gen", "altruismo_gen")], na.rm = TRUE)

frq(db$confianza_inter)
# seguridad_sub

db %>% 
  group_by(wave) %>% 
  select(seguridad_sat, seguridad_perc) %>% 
  frq()

db$seguridad_sub <- rowMeans(db[, c("seguridad_sat", "seguridad_perc")], na.rm = TRUE)

frq(db$seguridad_sub)
# seguridad_obj

db %>% 
  group_by(wave) %>% 
  select(peleas_calle, asaltos, trafico_drogas) %>% 
  frq()

db$seguridad_obj <- rowMeans(db[, c("peleas_calle", "asaltos", "trafico_drogas")], na.rm = TRUE)
db$seguridad_obj <- (round(db$seguridad_obj * 2) / 2)

frq(db$seguridad_obj)
# sentido_pertenencia

db %>% 
  group_by(wave) %>% 
  select(barrio_ideal, barrio_integracion, barrio_identidad, barrio_pertenencia) %>% 
  frq()

db$sentido_pertenencia <- rowMeans(db[, c("barrio_ideal", "barrio_integracion", "barrio_identidad", "barrio_pertenencia")], na.rm = TRUE)
db$sentido_pertenencia <- (round(db$sentido_pertenencia * 2) / 2)

frq(db$sentido_pertenencia)
# satisfaccion_barrio
db %>% 
  group_by(wave) %>% 
  select(barrio_amigos, barrio_sociable, barrio_cordial, barrio_colaborador) %>% 
  frq()

db$satisfaccion_barrio <- rowMeans(db[, c("barrio_amigos", "barrio_sociable", "barrio_cordial", "barrio_colaborador")], na.rm = TRUE)
db$satisfaccion_barrio <- (round(db$satisfaccion_barrio * 2) / 2)

frq(db$satisfaccion_barrio)

# migracion

db %>% 
  select(contains("migra"), perdida_identidad) %>% 
  frq() # ok

db <- db %>% 
  mutate(
    frecuencia_migrantes = car::recode(frecuencia_migrantes, 
    recodes = c("1:2 ='Bajo'; 
                 3='Medio'; 
                 4:5 ='Alto'"), 
    levels = c("Bajo", 
               "Medio", 
               "Alto"),
    as.factor = T),
    contacto_migrantes = car::recode(contacto_migrantes, 
    recodes = c("1:2='Muy poco + poco amistosa'; 
                 3='Ni amistosa ni no amistosa'; 
                 4:5='Muy + bastante amistosa'"), 
    levels = c("Muy poco + poco amistosa", 
                "Ni amistosa ni no amistosa", 
                "Muy + bastante amistosa"),
    as.factor = T),
    simpatia_migrantes = car::recode(simpatia_migrantes,
    recodes = c("1:2='Muy poco + poco o nada'; 
                 3='Algo'; 
                 4:5='Mucho + bastante'"), 
    levels = c("Muy poco + poco o nada", 
               "Algo", 
               "Mucho + bastante"),
    as.factor = T),    
    confianza_migrantes = car::recode(confianza_migrantes,
    recodes = c("1:2='Nada + poca confianza'; 
                 3='Algo de confianza'; 
                 4:5='Mucha + bastante confianza'"),
    levels = c("Nada + poca confianza",
               "Algo de confianza", 
               "Mucha + bastante confianza"),
    as.factor = T)) %>% 
  mutate(
    across(
      .cols = c(desempleo_migrantes, fomentar_migracion, igualdad_migrantes, perdida_identidad),
      .fns = ~ car::recode(., 
      recodes = c("1:2='Muy en desacuerdo'; 
                   3='Ni en desacuerdo ni de acuerdo'; 
                   4:5='Muy de acuerdo'"), 
      levels = c("Muy en desacuerdo",
                 "Ni en desacuerdo ni de acuerdo", 
                 "Muy de acuerdo"),
      as.factor = T)
    )
  )


# 3.4 Missings values ----

colSums(is.na(db))

n_miss(db)

prop_miss(db)*100

miss_var_summary(db)

miss_var_table(db)

vis_miss(db, warn_large_data = F) + theme(axis.text.x = element_text(angle=80))


vars_clave <- c("reunion_pub", "voluntariado",
                "prestar_dinero", "ayuda_trabajo",
                "confianza_gen", "altruismo_gen",
                "seguridad_sat", "seguridad_perc",
                "peleas_calle", "asaltos", "trafico_drogas",
                "barrio_ideal", "barrio_integracion",
                "barrio_identidad", "barrio_pertenencia",
                "barrio_amigos", "barrio_sociable",
                "barrio_cordial", "barrio_colaborador",
                "simpatia_migrantes", "perdida_identidad",
                "desempleo_migrantes", "confianza_migrantes",
                "fomentar_migracion")

db %>%
  group_by(wave) %>%
  summarise(across(all_of(vars_clave),
                   ~ mean(is.na(.)) * 100,
                   .names = "pNA_{.col}"),
            .groups = "drop")

# 3.5 Labels ----

# 4. Save and export  ----------------------------------------------------------------

save(db, file = here("input/data/proc_data/db_long.RData"))
