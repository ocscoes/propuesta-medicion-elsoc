pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

elsoc_2 <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
elsoc_2 <- elsoc_2 %>%
  filter(ola==2)

# seleccionar variables de interés
elsoc_2 <- elsoc_2 %>%
  select(idencuesta, m01, m0_sexo, m0_edad, comuna, comuna_cod, region, region_cod, 
         t06_01, t10, t09_01, t09_02, t09_03, t02_01, t02_02,
         t02_03, t02_04, t03_01, t03_02, t03_03, t03_04, 
         c02, c03, c07_02, c07_04, c07_05, c07_06, c07_08,
         r06, r07, r09, r12_03, r12_04, r16, r18_01, r18_02)

# renombrar variables
elsoc_2 <- elsoc_2 %>%
  rename(educacion = m01, sexo = m0_sexo, edad = m0_edad, 
         seguridad_sat = t06_01, seguridad_perc = t10, 
         peleas_calle = t09_01, asaltos = t09_02, trafico_drogas = t09_03,
         barrio_ideal = t02_01, barrio_integracion = t02_02, 
         barrio_identidad = t02_03, barrio_pertenencia = t02_04, 
         barrio_amigos = t03_01, barrio_sociable = t03_02, 
         barrio_cordial = t03_03, barrio_colaborador = t03_04,
         confianza_gen = c02, altruismo_gen = c03,
         reunion_pub = c07_02, voluntariado = c07_04,
         donar_dinero = c07_05, prestar_dinero = c07_06, 
         ayuda_trabajo = c07_08,
         frecuencia_migrantes = r06, contacto_migrantes = r07, 
         simpatia_migrantes = r09, perdida_identidad = r12_03, 
         desempleo_migrantes = r12_04, confianza_migrantes = r16, 
         fomentar_migracion = r18_01, igualdad_migrantes = r18_02)

# revisar labels para asegurarse de que están codificados correctamente

get_labels(elsoc_2$confianza_gen, values = TRUE)
get_labels(elsoc_2$voluntariado, values = TRUE)

# recodificar NA
elsoc_2$seguridad_sat <- recode(elsoc_2$seguridad_sat, "c(-666, -777, -888, -999) = NA") 
elsoc_2$seguridad_perc <- recode(elsoc_2$seguridad_perc, "c(-666, -777, -888, -999) = NA")
elsoc_2$peleas_calle <- recode(elsoc_2$peleas_calle, "c(-666, -777, -888, -999) = NA")
elsoc_2$asaltos <- recode(elsoc_2$asaltos, "c(-666, -777, -888, -999) = NA")
elsoc_2$trafico_drogas <- recode(elsoc_2$trafico_drogas, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_ideal <- recode(elsoc_2$barrio_ideal, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_integracion <- recode(elsoc_2$barrio_integracion, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_identidad <- recode(elsoc_2$barrio_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_pertenencia <- recode(elsoc_2$barrio_pertenencia, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_amigos <- recode(elsoc_2$barrio_amigos, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_sociable <- recode(elsoc_2$barrio_sociable, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_cordial <- recode(elsoc_2$barrio_cordial, "c(-666, -777, -888, -999) = NA")
elsoc_2$barrio_colaborador <- recode(elsoc_2$barrio_colaborador, "c(-666, -777, -888, -999) = NA")
elsoc_2$confianza_gen <- recode(elsoc_2$confianza_gen, "c(-666, -777, -888, -999) = NA")
elsoc_2$altruismo_gen <- recode(elsoc_2$altruismo_gen, "c(-666, -777, -888, -999) = NA")
elsoc_2$reunion_pub <- recode(elsoc_2$reunion_pub, "c(-666, -777, -888, -999) = NA")
elsoc_2$voluntariado <- recode(elsoc_2$voluntariado, "c(-666, -777, -888, -999) = NA")
elsoc_2$donar_dinero <- recode(elsoc_2$donar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_2$prestar_dinero <- recode(elsoc_2$prestar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_2$ayuda_trabajo <- recode(elsoc_2$ayuda_trabajo, "c(-666, -777, -888, -999) = NA")
elsoc_2$frecuencia_migrantes <- recode(elsoc_2$frecuencia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$contacto_migrantes <- recode(elsoc_2$contacto_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$simpatia_migrantes <- recode(elsoc_2$simpatia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$perdida_identidad <- recode(elsoc_2$perdida_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_2$desempleo_migrantes <- recode(elsoc_2$desempleo_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$confianza_migrantes <- recode(elsoc_2$confianza_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$fomentar_migracion <- recode(elsoc_2$fomentar_migracion, "c(-666, -777, -888, -999) = NA")
elsoc_2$igualdad_migrantes <- recode(elsoc_2$igualdad_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_2$sexo <- recode(elsoc_2$sexo, "c(1) = 0; c(2) = 1")
elsoc_2$educacion <- recode(elsoc_2$educacion, "c(-666, -777, -888, -999) = NA")

# dicotomizar nivel educacional 
elsoc_2$educacion <- car::recode(elsoc_2$educacion, "c(1, 2, 3, 4, 5, 6, 7, 8)=0; c(9, 10)=1")

# guardar base de datos
saveRDS(elsoc_2, file = "input/data/proc_data/elsoc_2017_coh_mig.RData")