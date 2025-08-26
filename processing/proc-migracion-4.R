pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

elsoc_4 <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
elsoc_4 <- elsoc_4 %>%
  filter(ola==4)

# seleccionar variables de interés
elsoc_4 <- elsoc_4 %>%
  select(idencuesta, m01, m0_sexo, m0_edad, comuna, comuna_cod, region, region_cod, 
         t06_01, t10, t09_01, t09_02, t09_03, t02_01, t02_02,
         t02_03, t02_04, t03_01, t03_02, t03_03, t03_04, r06, r07, r09,
         r12_03, r12_04, r16, r18_01, r18_02)

# renombrar variables

elsoc_4 <- elsoc_4 %>%
  rename(educacion = m01, sexo = m0_sexo, edad = m0_edad, seguridad_sat = t06_01, 
         seguridad_perc = t10, peleas_calle = t09_01,
         asaltos = t09_02, trafico_drogas = t09_03,
         barrio_ideal = t02_01,
         barrio_integracion = t02_02, barrio_identidad = t02_03, 
         barrio_pertenencia = t02_04, barrio_amigos = t03_01, 
         barrio_sociable = t03_02, barrio_cordial = t03_03, 
         barrio_colaborador = t03_04, frecuencia_migrantes = r06,
         contacto_migrantes = r07, simpatia_migrantes = r09,
         perdida_identidad = r12_03, desempleo_migrantes = r12_04,
         confianza_migrantes = r16, fomentar_migracion = r18_01,
         igualdad_migrantes = r18_02)

# revisar labels para asegurarse de que están codificados correctamente

get_labels(elsoc_4$confianza_gen, values = TRUE)
get_labels(elsoc_4$voluntariado, values = TRUE)

# recodificar NA

elsoc_4$seguridad_sat <- recode(elsoc_4$seguridad_sat, "c(-666, -777, -888, -999) = NA") 
elsoc_4$seguridad_perc <- recode(elsoc_4$seguridad_perc, "c(-666, -777, -888, -999) = NA")
elsoc_4$peleas_calle <- recode(elsoc_4$peleas_calle, "c(-666, -777, -888, -999) = NA")
elsoc_4$asaltos <- recode(elsoc_4$asaltos, "c(-666, -777, -888, -999) = NA")
elsoc_4$trafico_drogas <- recode(elsoc_4$trafico_drogas, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_ideal <- recode(elsoc_4$barrio_ideal, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_integracion <- recode(elsoc_4$barrio_integracion, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_identidad <- recode(elsoc_4$barrio_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_pertenencia <- recode(elsoc_4$barrio_pertenencia, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_amigos <- recode(elsoc_4$barrio_amigos, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_sociable <- recode(elsoc_4$barrio_sociable, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_cordial <- recode(elsoc_4$barrio_cordial, "c(-666, -777, -888, -999) = NA")
elsoc_4$barrio_colaborador <- recode(elsoc_4$barrio_colaborador, "c(-666, -777, -888, -999) = NA")
elsoc_4$frecuencia_migrantes <- recode(elsoc_4$frecuencia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_4$contacto_migrantes <- recode(elsoc_4$contacto_migrante, "c(-666, -777, -888, -999) = NA")
elsoc_4$simpatia_migrantes <- recode(elsoc_4$simpatia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_4$perdida_identidad <- recode(elsoc_4$perdida_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_4$desempleo_migrantes <- recode(elsoc_4$desempleo_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_4$confianza_migrantes <- recode(elsoc_4$confianza_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_4$fomentar_migracion <- recode(elsoc_4$fomentar_migracion, "c(-666, -777, -888, -999) = NA")
elsoc_4$igualdad_migrantes <- recode(elsoc_4$igualdad_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_4$sexo <- recode(elsoc_4$sexo, "c(1) = 0; c(2) = 1")
elsoc_4$educacion <- recode(elsoc_4$educacion, "c(-666, -777, -888, -999) = NA")

# dicotomizar nivel educacional 
elsoc_4$educacion <- car::recode(elsoc_4$educacion, "c(1, 2, 3, 4, 5, 6, 7, 8)=0; c(9, 10)=1")

# guardar base de datos
saveRDS(elsoc_4, file = "input/data/proc_data/elsoc_2019_mig.RData")
