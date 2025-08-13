# ---- Procesamiento base ELSOC ola 2022 ----

pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('../input/data/raw_data/elsoc.RData')

data <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
data <- data %>%
  filter(ola==6)

#seleccionar variables de interes
data <- data %>%
  select(idencuesta, m0_sexo, m0_edad, comuna, comuna_cod, region, region_cod, 
         t06_01, t10, t09_01, t09_02, t09_03,
         c02, c03, c04, c07_01, c07_02, c07_03, c07_04, c07_05, c07_06, c07_07,
         t01, t02_01, t02_02, t02_03, t02_04, t03_01, t03_02, t03_03, t03_04,
         t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08,
         t07_01, t07_02)

#cambiar nombre a variables
data <- data %>% 
  rename(sexo = m0_sexo, edad = m0_edad, seguridad_sat = t06_01, 
         seguridad_perc = t10, peleas_calle = t09_01,
         asaltos = t09_02, trafico_drogas = t09_03, confianza_gen = c02,
         altruismo_gen = c03, gente_justa = c04, visitar_vecino = c07_01,
         reunion_pub = c07_02, visita_amigos = c07_03, voluntariado = c07_04,
         donar_dinero = c07_05, prestar_dinero = c07_06,
         confianza_vecinos = t01, barrio_ideal = t02_01,
         barrio_integracion = t02_02, barrio_identidad = t02_03, 
         barrio_pertenencia = t02_04, barrio_amigos = t03_01, 
         barrio_sociable = t03_02, barrio_cordial = t03_03, 
         barrio_colaborador = t03_04, conectividad = t06_02, 
         areas_verdes = t06_03, barrio_limpio = t06_04, 
         cercania_actividad = t06_05, cercania_escuelas = t06_06,
         cercania_comercio = t06_07, cercania_familia = t06_08, 
         tamaño_vivienda = t07_01, calidad_vivienda = t07_02)

#revisar los labels y los values para segurarnos 
#de que están codificadas correctamente
get_labels(data$seguridad_sat, values = TRUE)
get_labels(data$seguridad_perc, values = TRUE)
get_labels(data$barrio_ideal, values = TRUE)
get_labels(data$calidad_vivienda, values = TRUE)
get_labels(data$confianza_gen, values = TRUE)
get_labels(data$voluntariado, values = TRUE)

#Recodificar NA
data$seguridad_sat <- recode(data$seguridad_sat, "c(-666, -777, -888, -999) = NA") 
data$seguridad_perc <- recode(data$seguridad_perc, "c(-666, -777, -888, -999) = NA")
data$peleas_calle <- recode(data$peleas_calle, "c(-666, -777, -888, -999) = NA")
data$asaltos <- recode(data$asaltos, "c(-666, -777, -888, -999) = NA")
data$trafico_drogas <- recode(data$trafico_drogas, "c(-666, -777, -888, -999) = NA")
data$confianza_gen <- recode(data$confianza_gen, "c(-666, -777, -888, -999) = NA")
data$altruismo_gen <- recode(data$altruismo_gen, "c(-666, -777, -888, -999) = NA")
data$gente_justa <- recode(data$gente_justa, "c(-666, -777, -888, -999) = NA")
data$visitar_vecino <- recode(data$visitar_vecino, "c(-666, -777, -888, -999) = NA")
data$reunion_pub <- recode(data$reunion_pub, "c(-666, -777, -888, -999) = NA")
data$visita_amigos <- recode(data$visita_amigos, "c(-666, -777, -888, -999) = NA")
data$voluntariado <- recode(data$voluntariado, "c(-666, -777, -888, -999) = NA")
data$donar_dinero <- recode(data$donar_dinero, "c(-666, -777, -888, -999) = NA")
data$prestar_dinero <- recode(data$prestar_dinero, "c(-666, -777, -888, -999) = NA")
data$confianza_vecinos <- recode(data$confianza_vecinos, "c(-666, -777, -888, -999) = NA")
data$barrio_ideal <- recode(data$barrio_ideal, "c(-666, -777, -888, -999) = NA")
data$barrio_integracion <- recode(data$barrio_integracion, "c(-666, -777, -888, -999) = NA")
data$barrio_identidad <- recode(data$barrio_identidad, "c(-666, -777, -888, -999) = NA")
data$barrio_pertenencia <- recode(data$barrio_pertenencia, "c(-666, -777, -888, -999) = NA")
data$barrio_amigos <- recode(data$barrio_amigos, "c(-666, -777, -888, -999) = NA")
data$barrio_sociable <- recode(data$barrio_sociable, "c(-666, -777, -888, -999) = NA")
data$barrio_cordial <- recode(data$barrio_cordial, "c(-666, -777, -888, -999) = NA")
data$barrio_colaborador <- recode(data$barrio_colaborador, "c(-666, -777, -888, -999) = NA")
data$barrio_limpio <- recode(data$barrio_limpio, "c(-666, -777, -888, -999) = NA")
data$conectividad <- recode(data$conectividad, "c(-666, -777, -888, -999) = NA")
data$areas_verdes <- recode(data$areas_verdes, "c(-666, -777, -888, -999) = NA")
data$cercania_actividad <- recode(data$cercania_actividad, "c(-666, -777, -888, -999) = NA")
data$cercania_comercio <- recode(data$cercania_comercio, "c(-666, -777, -888, -999) = NA")
data$cercania_escuelas <- recode(data$cercania_escuelas, "c(-666, -777, -888, -999) = NA")
data$cercania_familia <- recode(data$cercania_familia, "c(-666, -777, -888, -999) = NA")
data$tamaño_vivienda <- recode(data$tamaño_vivienda, "c(-666, -777, -888, -999) = NA")
data$calidad_vivienda <- recode(data$calidad_vivienda, "c(-666, -777, -888, -999) = NA")
data$sexo <- recode(data$sexo, "c(1) = 0; c(2) = 1")

#reformatearlo como dataframe

data <- as.data.frame(data)

saveRDS(data, file = "../input/data/proc_data/elsoc_2022.RData")
