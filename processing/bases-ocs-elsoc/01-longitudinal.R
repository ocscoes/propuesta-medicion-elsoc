# 0. Identification ---------------------------------------------------

# Title: Data preparation Longitudinal
# Institution: OCS
# Responsible: René Canales

# Executive Summary: This script contains the code to data preparation for analysis of cohesion and migration
# Date: Sep 23, 2025

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

# 3. Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola,
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
         conf_gobierno = c05_01, 
         conf_pp = c05_02, 
         conf_judicial = c05_05, 
         conf_congreso = c05_07, 
         firma_peticion = c08_01, 
         asiste_marcha = c08_02, 
         part_huelga = c08_03,
         opinion_rrss = c08_04, 
         voto_deber = c10_01, 
         voto_influye = c10_02, 
         voto_expresion = c10_03, 
         interes_politica = c13, 
         hablar_politica = c14_01, 
         infopolitica_medios = c14_02, 
         gobierno_firme = c18_04, 
         mandatario_fuerte = c18_05, 
         vida_disciplinar = c18_07, 
         justicia_pensiones = d02_01,
         justicia_educacion = d02_02, 
         justicia_salud = d02_03, 
         sat_democracia = c01) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

