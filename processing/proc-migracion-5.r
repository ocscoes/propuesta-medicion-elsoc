pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

elsoc_5 <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
elsoc_5 <- elsoc_5 %>%
  filter(ola==5)

# seleccionar variables de interés
elsoc_5 <- elsoc_5 %>%
  select(idencuesta, m01, m0_sexo, m0_edad, comuna, comuna_cod,
         region, region_cod, c02, c03, c07_02,
         c07_04, c07_05, c07_06, c07_07, c07_08, r06, r07, r09,
          r12_03, r12_04, r16, r18_01, r18_02)

# renombrar variables

elsoc_5 <- elsoc_5 %>%
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

get_labels(elsoc_5$altruismo_gen, values = TRUE)
get_labels(elsoc_5$voluntariado, values = TRUE)

# recodificar NA

elsoc_5$confianza_gen <- recode(elsoc_5$confianza_gen, "c(-666, -777, -888, -999) = NA; (1)=3; (2)=1; (3)=2")
elsoc_5$altruismo_gen <- recode(elsoc_5$altruismo_gen, "c(-666, -777, -888, -999) = NA; (1)=3; (2)=1; (3)=2")
elsoc_5$reunion_pub <- recode(elsoc_5$reunion_pub, "c(-666, -777, -888, -999) = NA")
elsoc_5$voluntariado <- recode(elsoc_5$voluntariado, "c(-666, -777, -888, -999) = NA")
elsoc_5$donar_dinero <- recode(elsoc_5$donar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_5$prestar_dinero <- recode(elsoc_5$prestar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_5$ayuda_trabajo <- recode(elsoc_5$ayuda_trabajo, "c(-666, -777, -888, -999) = NA")
elsoc_5$frecuencia_migrantes <- recode(elsoc_5$frecuencia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_5$contacto_migrantes <- recode(elsoc_5$contacto_migrante, "c(-666, -777, -888, -999) = NA")
elsoc_5$simpatia_migrantes <- recode(elsoc_5$simpatia_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_5$perdida_identidad <- recode(elsoc_5$perdida_identidad, "c(-666, -777, -888, -999) = NA")
elsoc_5$desempleo_migrantes <- recode(elsoc_5$desempleo_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_5$confianza_migrantes <- recode(elsoc_5$confianza_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_5$fomentar_migracion <- recode(elsoc_5$fomentar_migracion, "c(-666, -777, -888, -999) = NA")
elsoc_5$igualdad_migrantes <- recode(elsoc_5$igualdad_migrantes, "c(-666, -777, -888, -999) = NA")
elsoc_5$sexo <- recode(elsoc_5$sexo, "(1) = 0; (2) = 1")
elsoc_5$educacion <- recode(elsoc_5$educacion, "c(-666, -777, -888, -999) = NA")

# dicotomizar nivel educacional 
elsoc_5$educacion <- car::recode(elsoc_5$educacion, "c(1, 2, 3, 4, 5, 6, 7, 8)=0; c(9, 10)=1")

# Recodificar labels

get_labels(elsoc_5$confianza_migrantes, values = TRUE)

elsoc_5$confianza_migrantes <- set_labels(elsoc_5$confianza_migrantes,
            labels=c( "Nada de confianza"=1,
                      "Poca confianza"=2,
                      "Algo de confianza"=3,
                      "Bastante confianza"=4,
                      "Mucha confianza"=5))

elsoc_5$igualdad_migrantes <- set_labels(elsoc_5$igualdad_migrantes,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

elsoc_5$fomentar_migracion <- set_labels(elsoc_5$fomentar_migracion,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

elsoc_5$frecuencia_migrantes <- set_labels(elsoc_5$frecuencia_migrantes,
            labels=c( "Nunca"=1,
                      "Casi nunca"=2,
                      "A veces"=3,
                      "Casi siempre"=4,
                      "Siempre"=5))

elsoc_5$perdida_identidad <- set_labels(elsoc_5$perdida_identidad,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

elsoc_5$desempleo_migrantes <- set_labels(elsoc_5$desempleo_migrantes,
            labels=c( "Totalmente en desacuerdo"=1,
                      "En desacuerdo"=2,
                      "Ni en desacuerdo ni de acuerdo"=3,
                      "De acuerdo"=4,
                      "Totalmente de acuerdo"=5))

elsoc_5$altruismo_gen <- set_labels(elsoc_5$altruismo_gen,
            labels=c( "La mayoria de las veces se preocupan solo de si mismas"=1,
                      "Depende"=2,
                      "La mayoria de las veces tratan de ayudar a los demas"=3))

elsoc_5$confianza_gen <- set_labels(elsoc_5$confianza_gen,
            labels=c( "Casi siempre hay que tener cuidado al tratar con las personas"=1,
                      "Depende"=2,
                      "Casi siempre se puede confiar en las personas"=3))



# guardar base de datos
saveRDS(elsoc_5, file = "input/data/proc_data/elsoc_2021_mig.RData")
