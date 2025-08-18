# ---- Procesamiento ola 5 ----

pacman::p_load(dplyr, car, summarytools, sjmisc, sjlabelled)

load('input/data/raw_data/elsoc.RData')

elsoc_5 <- elsoc_long_2016_2023
rm(elsoc_long_2016_2023)

# seleccionar solo ola 2022
elsoc_5 <- elsoc_5 %>%
  filter(ola==5)

# seleccionar variables de interés
elsoc_5 <- elsoc_5 %>%
  select(idencuesta, m0_sexo, m0_edad, comuna, comuna_cod,
         region, region_cod, c02, c03, c04, c07_01, c07_02, c07_03,
         c07_04, c07_05, c07_06, c07_07, c07_08)

# renombrar variables

elsoc_5 <- elsoc_5 %>%
  rename(sexo = m0_sexo, edad = m0_edad, confianza_gen = c02,
         altruismo_gen = c03, gente_justa = c04, visitar_vecino = c07_01,
         reunion_pub = c07_02, visita_amigos = c07_03, voluntariado = c07_04,
         donar_dinero = c07_05, prestar_dinero = c07_06, ayuda_trabajo = c07_08)

# revisar labels para asegurarse de que están codificados correctamente

get_labels(elsoc_5$confianza_gen, values = TRUE)
get_labels(elsoc_5$voluntariado, values = TRUE)

# recodificar NA

elsoc_5$confianza_gen <- recode(elsoc_5$confianza_gen, "c(-666, -777, -888, -999) = NA")
elsoc_5$altruismo_gen <- recode(elsoc_5$altruismo_gen, "c(-666, -777, -888, -999) = NA")
elsoc_5$gente_justa <- recode(elsoc_5$gente_justa, "c(-666, -777, -888, -999) = NA")
elsoc_5$visitar_vecino <- recode(elsoc_5$visitar_vecino, "c(-666, -777, -888, -999) = NA")
elsoc_5$reunion_pub <- recode(elsoc_5$reunion_pub, "c(-666, -777, -888, -999) = NA")
elsoc_5$visita_amigos <- recode(elsoc_5$visita_amigos, "c(-666, -777, -888, -999) = NA")
elsoc_5$voluntariado <- recode(elsoc_5$voluntariado, "c(-666, -777, -888, -999) = NA")
elsoc_5$donar_dinero <- recode(elsoc_5$donar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_5$prestar_dinero <- recode(elsoc_5$prestar_dinero, "c(-666, -777, -888, -999) = NA")
elsoc_5$ayuda_trabajo <- recode(elsoc_5$ayuda_trabajo, "c(-666, -777, -888, -999) = NA")
elsoc_5$sexo <- recode(elsoc_5$sexo, "c(1) = 0; c(2) = 1")

# guardar base de datos
saveRDS(elsoc_5, file = "input/data/proc_data/elsoc_2021.RData")
