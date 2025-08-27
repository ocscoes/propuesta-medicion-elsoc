# Selección de variables: (1) desempleo por migrantes, (2)pérdida de identidad
# (3)simpatía hacia migrantes, (4)acceso a salud igualitaro y (5) contacto positivo

## (1) DESEMPLEO MIGRANTES

# Dimension seguridad 
frq(db$desempleo_migrantes, weights = db$ponderador_long_total)

# Factor seguridad subjetiva
db_pond %>% 
  select(wave, desempleo_migrantes, seguridad_sub) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(seguridad_sub, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Seguridad percibida",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# Factor seguridad objetiva

db_pond %>% 
  select(wave, desempleo_migrantes, seguridad_obj) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(seguridad_obj, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Seguridad objetiva",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# Dimension redes interpersonales

# Confianza interpersonal

db_pond %>% 
  select(wave, desempleo_migrantes, confianza_inter) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(confianza_inter, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Confianza interpersonal",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# prosocial

db_pond %>% 
  select(wave, desempleo_migrantes, comportamiento_prosocial) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(comportamiento_prosocial, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Comportamiento prosocial",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 


# economica

db_pond %>% 
  select(wave, desempleo_migrantes, ayuda_economica) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(ayuda_economica, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Ayuda económica",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 


# Dimension vinculación territoral

# pertenencia
db_pond %>% 
  select(wave, desempleo_migrantes, sentido_pertenencia) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(sentido_pertenencia, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Sentido pertenencia barrio",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# satisfaccion

db_pond %>% 
  select(wave, desempleo_migrantes, satisfaccion_barrio) %>% 
  na.omit() %>% 
  group_by(wave, desempleo_migrantes) %>%
  summarise(value = survey_mean(satisfaccion_barrio, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = desempleo_migrantes)) +
  geom_point(aes(color=desempleo_migrantes), size = 3.5) +
  geom_line(aes(color = desempleo_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = desempleo_migrantes),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Desempleo percibido por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Satisfacción barrio",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

## (2) PERDIDA IDENTIDAD

# Dimension seguridad 
frq(db$perdida_identidad, weights = db$ponderador_long_total)

# Factor seguridad subjetiva
db_pond %>% 
  select(wave, perdida_identidad, seguridad_sub) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(seguridad_sub, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Seguridad percibida",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# Factor seguridad objetiva

db_pond %>% 
  select(wave, perdida_identidad, seguridad_obj) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(seguridad_obj, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Seguridad objetiva",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# Dimension redes interpersonales

# Confianza interpersonal

db_pond %>% 
  select(wave, simpatia_migrantes, confianza_inter) %>% 
  na.omit() %>% 
  group_by(wave, simpatia_migrantes) %>%
  summarise(value = survey_mean(confianza_inter, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = simpatia_migrantes)) +
  geom_point(aes(color=simpatia_migrantes), size = 3.5) +
  geom_line(aes(color = simpatia_migrantes), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Confianza interpersonal",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# prosocial

db_pond %>% 
  select(wave, perdida_identidad, comportamiento_prosocial) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(comportamiento_prosocial, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Comportamiento prosocial",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 


# economica

db_pond %>% 
  select(wave, perdida_identidad, ayuda_economica) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(ayuda_economica, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Ayuda económica",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 


# Dimension vinculación territoral

# pertenencia
db_pond %>% 
  select(wave, perdida_identidad, sentido_pertenencia) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(sentido_pertenencia, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Sentido pertenencia barrio",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 

# satisfaccion

db_pond %>% 
  select(wave, perdida_identidad, satisfaccion_barrio) %>% 
  na.omit() %>% 
  group_by(wave, perdida_identidad) %>%
  summarise(value = survey_mean(satisfaccion_barrio, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, group = perdida_identidad)) +
  geom_point(aes(color=perdida_identidad), size = 3.5) +
  geom_line(aes(color = perdida_identidad), linewidth = 0.8) +
  #geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = perdida_identidad),
  #             width = 0.1) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(
    name = "Pérdida identidad por \nmigrantes", 
    type = "qual", 
    palette = "Dark2"
  ) +
  labs(x = "Ola",
       y = "Satisfacción barrio",
       caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)") +
  theme_ggdist()+
  theme(legend.position = "bottom") 
