pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

elsoc_3 <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
elsoc_3 <- elsoc_3 %>%
  filter(ola==3)

# seleccionar variables de interés
elsoc_3 <- elsoc_3 %>%
  select(idencuesta, m01, m0_sexo, m0_edad, comuna, comuna_cod,
         region, region_cod, c02, c03, c07_02,
         c07_04, c07_05, c07_06, c07_07, c07_08, r06, r07, r09,
         r12_03, r12_04, r16, r18_01, r18_02)

# renombrar variables

elsoc_3 <- elsoc_3 %>%
  rename(educacion = m01, sexo = m0_sexo, edad = m0_edad, confianza_gen = c02,
         altruismo_gen = c03,
         reunion_pub = c07_02, voluntariado = c07_04,
         donar_dinero = c07_05, prestar_dinero = c07_06, ayuda_trabajo = c07_08,
         frecuencia_migrantes = r06,
         contacto_migrantes = r07, simpatia_migrantes = r09,
         perdida_identidad = r12_03, desempleo_migrantes = r12_04,
         confianza_migrantes = r16, fomentar_migracion = r18_01,
         igualdad_migrantes = r18_02)

# revisar labels para asegurarse de que están codificados correctamente

get_labels(elsoc_3$confianza_gen, values = TRUE)
get_labels(elsoc_3$voluntariado, values = TRUE)

# recodificar NA

elsoc_3$confianza_gen <- recode(elsoc_3$confianza_gen, "c(-666, -777, -888, -999) = NA")
elsoc_3$altruismo_gen <- recode(elsoc_3$altruismo_gen, "c(-666, -777, -888, -999) = NA")
elsoc_3$reunion_pub <- recode(elsoc_3$reunion_pub, "c(-666, -777, -888, -999) = NA")
elsoc_3$voluntariado <- recode(elsoc_3$voluntariado, "c(-666, -777, -888, -999) = NA")
elsoc_3$donar_dinero <- recode(elsoc_3$donar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_3$prestar_dinero <- recode(elsoc_3$prestar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_3$ayuda_trabajo <- recode(elsoc_3$ayuda_trabajo, "c(-666, -777, -888, -999) = NA")
elsoc_3$frecuencia_migrantes <- recode(elsoc_3$frecuencia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_3$contacto_migrantes <- recode(elsoc_3$contacto_migrante, "c(-666, -777, -888, -999) = NA")
elsoc_3$simpatia_migrantes <- recode(elsoc_3$simpatia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_3$perdida_identidad <- recode(elsoc_3$perdida_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_3$desempleo_migrantes <- recode(elsoc_3$desempleo_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_3$confianza_migrantes <- recode(elsoc_3$confianza_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_3$fomentar_migracion <- recode(elsoc_3$fomentar_migracion, "c(-666, -777, -888, -999) = NA")
elsoc_3$igualdad_migrantes <- recode(elsoc_3$igualdad_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_3$sexo <- recode(elsoc_3$sexo, "c(1) = 0; c(2) = 1")
elsoc_3$educacion <- recode(elsoc_3$educacion, "c(-666, -777, -888, -999) = NA")

# dicotomizar nivel educacional 
elsoc_3$educacion <- car::recode(elsoc_3$educacion, "c(1, 2, 3, 4, 5, 6, 7, 8)=0; c(9, 10)=1")

# guardar base de datos
saveRDS(elsoc_3, file = "input/data/proc_data/elsoc_2018_mig.RData")