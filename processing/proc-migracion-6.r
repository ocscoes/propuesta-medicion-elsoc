pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

data <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
data <- data %>%
  filter(ola==6)

#seleccionar variables de interes
data <- data %>%
  select(idencuesta, m01, m0_sexo, m0_edad, comuna, comuna_cod, region, region_cod, 
         t06_01, t10, t09_01, t09_02, t09_03, t02_01, t02_02,
         t02_03, t02_04, t03_01, t03_02, t03_03, t03_04, r06, r07, r09,
          r12_03, r12_04, r16, r18_01, r18_02)

#cambiar nombre a variables
data <- data %>% 
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

#Recodificar NA
data$seguridad_sat <- recode(data$seguridad_sat, "c(-666, -777, -888, -999) = NA") 
data$seguridad_perc <- recode(data$seguridad_perc, "c(-666, -777, -888, -999) = NA")
data$peleas_calle <- recode(data$peleas_calle, "c(-666, -777, -888, -999) = NA")
data$asaltos <- recode(data$asaltos, "c(-666, -777, -888, -999) = NA")
data$trafico_drogas <- recode(data$trafico_drogas, "c(-666, -777, -888, -999) = NA")
data$barrio_ideal <- recode(data$barrio_ideal, "c(-666, -777, -888, -999) = NA")
data$barrio_integracion <- recode(data$barrio_integracion, "c(-666, -777, -888, -999) = NA")
data$barrio_identidad <- recode(data$barrio_identidad, "c(-666, -777, -888, -999) = NA")
data$barrio_pertenencia <- recode(data$barrio_pertenencia, "c(-666, -777, -888, -999) = NA")
data$barrio_amigos <- recode(data$barrio_amigos, "c(-666, -777, -888, -999) = NA")
data$barrio_sociable <- recode(data$barrio_sociable, "c(-666, -777, -888, -999) = NA")
data$barrio_cordial <- recode(data$barrio_cordial, "c(-666, -777, -888, -999) = NA")
data$barrio_colaborador <- recode(data$barrio_colaborador, "c(-666, -777, -888, -999) = NA")
data$frecuencia_migrantes <- recode(data$frecuencia_migrantes, "c(-666, -777, -888, -999) = NA")
data$contacto_migrantes <- recode(data$contacto_migrante, "c(-666, -777, -888, -999) = NA")
data$simpatia_migrantes <- recode(data$simpatia_migrantes, "c(-666, -777, -888, -999) = NA")
data$perdida_identidad <- recode(data$perdida_identidad, "c(-666, -777, -888, -999) = NA")
data$desempleo_migrantes <- recode(data$desempleo_migrantes, "c(-666, -777, -888, -999) = NA")
data$confianza_migrantes <- recode(data$confianza_migrantes, "c(-666, -777, -888, -999) = NA")
data$fomentar_migracion <- recode(data$fomentar_migracion, "c(-666, -777, -888, -999) = NA")
data$igualdad_migrantes <- recode(data$igualdad_migrantes, "c(-666, -777, -888, -999) = NA")
data$sexo <- recode(data$sexo, "c(1) = 0; c(2) = 1")
data$educacion <- recode(data$educacion, "c(-666, -777, -888, -999) = NA")

# invertir escala de seguridad objetiva para que esté alineada con seguridad subjetiva
data$peleas_calle <- car::recode(data$peleas_calle, "(1)=5; (2)=4; (3)=3; (4)=2; (5)=1")
data$asaltos <- car::recode(data$asaltos, "(1)=5; (2)=4; (3)=3; (4)=2; (5)=1")
data$trafico_drogas <- car::recode(data$trafico_drogas, "(1)=5; (2)=4; (3)=3; (4)=2; (5)=1")

# dicotomizar nivel educacional 
data$educacion <- car::recode(data$educacion, "c(1, 2, 3, 4, 5, 6, 7, 8)=0; c(9, 10)=1")

# recodificar labels de migración



data$confianza_migrantes <- set_labels(data$confianza_migrantes,
            labels=c( "Nada de confianza"=1,
                      "Poca confianza"=2,
                      "Algo de confianza"=3,
                      "Bastante confianza"=4,
                      "Mucha confianza"=5))

data$igualdad_migrantes <- set_labels(data$igualdad_migrantes,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

data$fomentar_migracion <- set_labels(data$fomentar_migracion,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

data$frecuencia_migrantes <- set_labels(data$frecuencia_migrantes,
            labels=c( "Nunca"=1,
                      "Casi nunca"=2,
                      "A veces"=3,
                      "Casi siempre"=4,
                      "Siempre"=5))

data$perdida_identidad <- set_labels(data$perdida_identidad,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

data$desempleo_migrantes <- set_labels(data$desempleo_migrantes,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))


data_mig <- data
saveRDS(data_mig, file="input/data/proc_data/elsoc_2022_mig.RData")
