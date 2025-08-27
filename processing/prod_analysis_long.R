# 0. Identification ---------------------------------------------------

# Title: Data analysis
# Institution: OCS
# Responsible: Andreas Laffert

# Executive Summary: This script contains the analysis of cohesion and migration
# Date: Agust 26, 2025

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               here,
               sjlabelled,
               psych,
               rstatix,
               lme4,
               performance,
               sjPlot,
               srvyr,
               ggdist,
               purrr,
               rlang)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc_data/db_long.RData"))

glimpse(db)

# 3. Analysis -----------------------------------------------------------

db_pond <- db %>% 
  as_survey_design(ids = segmento, 
                   strata = estrato, 
                   weights = ponderador_long_total,
                   nest = T)

# Selección de variables: (1) desempleo por migrantes, (2)pérdida de identidad
# (3)simpatía hacia migrantes, (4)acceso a salud igualitaro y (5) contacto positivo

# plot function
make_plot <- function(design, predictor, outcome, ylab, legend_title) {
  pred_sym <- ensym(predictor)
  out_sym  <- ensym(outcome)
  
  design %>% 
    select(wave, !!pred_sym, !!out_sym) %>% 
    tidyr::drop_na() %>% 
    group_by(wave, !!pred_sym) %>%
    summarise(
      value = survey_mean(!!out_sym, vartype = "ci", na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    ggplot(aes(x = wave, y = value, group = !!pred_sym)) +
    geom_point(aes(color = !!pred_sym), size = 3.5) +
    geom_line(aes(color = !!pred_sym), linewidth = 0.8) +
    # geom_errorbar(aes(ymin = value_low, ymax = value_upp, color = !!pred_sym), width = 0.1) +
    scale_y_continuous(n.breaks = 7) +
    scale_color_manual(
      name = legend_title,
      values = c("#2b2f3c","#7ABA21", "#F9913D")
      ) +
    labs(
      x = "Ola",
      y = ylab,
      caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)"
    ) +
    theme_ggdist() +
    theme(
      legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
      legend.background     = element_rect(fill = "white", color = NA),
      legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
      legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
      
      legend.position       = "top",
      legend.direction      = "horizontal",
      legend.box            = "horizontal",  # útil si hubiera varias leyendas
      
      legend.title          = element_text(size = 11, face = "bold"),
      legend.text           = element_text(size = 11),
      legend.key            = element_rect(fill = "white", color = NA),
      legend.key.height     = unit(12, "pt"),
      legend.key.width      = unit(18, "pt"),
      
      axis.text  = element_text(size = 12),
      title      = element_text(size = 16)
      ) +
    guides(color = guide_legend(
      title.position = "top",   # título arriba
      label.position = "bottom", # etiquetas a la derecha del símbolo
      nrow = 1, byrow = TRUE,   # una fila (ajusta según ancho)
      keywidth = unit(14, "pt"),
      keyheight = unit(10, "pt")
    ))
}

# combinations

outcomes <- c(
  seguridad_sub           = "Seguridad percibida",
  seguridad_obj           = "Seguridad objetiva",
  confianza_inter         = "Confianza interpersonal",
  comportamiento_prosocial= "Comportamiento prosocial",
  ayuda_economica         = "Ayuda económica",
  sentido_pertenencia     = "Sentido pertenencia barrio",
  satisfaccion_barrio     = "Satisfacción barrio"
)

predictors <- list(
  desempleo_migrantes = "Desempleo percibido por \nmigrantes",
  perdida_identidad   = "Pérdida identidad por \nmigrantes",
  simpatia_migrantes = "Simpatía hacia migrantes",
  igualdad_migrantes = "Acceso a salud igualitario \npara migrantes",
  contacto_migrantes = "Contacto positivo con \nmigrantes"
)

comb <- tidyr::crossing(pred = names(predictors), out = names(outcomes))

# make plots

plots_tbl <- comb %>%
  mutate(
    plot = pmap(
      list(pred, out),
      ~ make_plot(
        design       = db_pond,
        predictor    = ..1,
        outcome      = ..2,
        ylab         = outcomes[..2],
        legend_title = predictors[[..1]]
      )
    ),
    plot_id = paste0("plot_", out, "__by__", pred)
  )

# view all
walk(plots_tbl$plot, print)

plots_tbl$plot[[1]]



# barras
# acceso salud igualitario + sentido pertenencia

db_pond %>% 
  select(wave, igualdad_migrantes, sentido_pertenencia) %>% 
  na.omit() %>% 
  group_by(wave, igualdad_migrantes) %>%
  summarise(value = survey_mean(sentido_pertenencia, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, fill = igualdad_migrantes)) +
  geom_col(position = position_dodge2(padding = 0.01), width = 0.6) +
  scale_y_continuous(limits = c(0,5),n.breaks = 10) +
  scale_fill_manual(
    name = "Acceso a salud igualitario \npara migrantes",
    values = c("#2b2f3c","#7ABA21", "#F9913D")
  ) +
  labs(
    x = "Ola",
    y = "Sentido pertenencia barrio",
    caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)"
  ) +
  theme_ggdist() +
  theme(
    legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
    legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
    
    legend.position       = "top",
    legend.direction      = "horizontal",
    legend.box            = "horizontal",  # útil si hubiera varias leyendas
    
    legend.title          = element_text(size = 11, face = "bold"),
    legend.text           = element_text(size = 11),
    legend.key            = element_rect(fill = "white", color = NA),
    legend.key.height     = unit(12, "pt"),
    legend.key.width      = unit(18, "pt"),
    
    axis.text  = element_text(size = 12),
    title      = element_text(size = 16)
  ) +
  guides(color = guide_legend(
    title.position = "top",   # título arriba
    label.position = "bottom", # etiquetas a la derecha del símbolo
    nrow = 1, byrow = TRUE,   # una fila (ajusta según ancho)
    keywidth = unit(14, "pt"),
    keyheight = unit(10, "pt")
  ))

# acceso salud igualitario + satisfaccion barrio

db_pond %>% 
  select(wave, igualdad_migrantes, satisfaccion_barrio) %>% 
  na.omit() %>% 
  group_by(wave, igualdad_migrantes) %>%
  summarise(value = survey_mean(satisfaccion_barrio, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, fill = igualdad_migrantes)) +
  geom_col(position = position_dodge2(padding = 0.01), width = 0.6) +
  scale_y_continuous(limits = c(0,5),n.breaks = 10) +
  scale_fill_manual(
    name = "Acceso a salud igualitario \npara migrantes",
    values = c("#2b2f3c","#7ABA21", "#F9913D")
  ) +
  labs(
    x = "Ola",
    y = "Satisfacción barrio",
    caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)"
  ) +
  theme_ggdist() +
  theme(
    legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
    legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
    
    legend.position       = "top",
    legend.direction      = "horizontal",
    legend.box            = "horizontal",  # útil si hubiera varias leyendas
    
    legend.title          = element_text(size = 11, face = "bold"),
    legend.text           = element_text(size = 11),
    legend.key            = element_rect(fill = "white", color = NA),
    legend.key.height     = unit(12, "pt"),
    legend.key.width      = unit(18, "pt"),
    
    axis.text  = element_text(size = 12),
    title      = element_text(size = 16)
  ) +
  guides(color = guide_legend(
    title.position = "top",   # título arriba
    label.position = "bottom", # etiquetas a la derecha del símbolo
    nrow = 1, byrow = TRUE,   # una fila (ajusta según ancho)
    keywidth = unit(14, "pt"),
    keyheight = unit(10, "pt")
  ))


# acceso salud igualitario + prosociañ
db_pond %>% 
  select(wave, igualdad_migrantes, comportamiento_prosocial) %>% 
  na.omit() %>% 
  group_by(wave, igualdad_migrantes) %>%
  summarise(value = survey_mean(comportamiento_prosocial, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, fill = igualdad_migrantes)) +
  geom_col(position = position_dodge2(padding = 0.01), width = 0.6) +
  scale_y_continuous(limits = c(0,3),n.breaks = 10) +
  scale_fill_manual(
    name = "Acceso a salud igualitario \npara migrantes",
    values = c("#2b2f3c","#7ABA21", "#F9913D")
  ) +
  labs(
    x = "Ola",
    y = "Comportamiento prosocial",
    caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)"
  ) +
  theme_ggdist() +
  theme(
    legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
    legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
    
    legend.position       = "top",
    legend.direction      = "horizontal",
    legend.box            = "horizontal",  # útil si hubiera varias leyendas
    
    legend.title          = element_text(size = 11, face = "bold"),
    legend.text           = element_text(size = 11),
    legend.key            = element_rect(fill = "white", color = NA),
    legend.key.height     = unit(12, "pt"),
    legend.key.width      = unit(18, "pt"),
    
    axis.text  = element_text(size = 12),
    title      = element_text(size = 16)
  ) +
  guides(color = guide_legend(
    title.position = "top",   # título arriba
    label.position = "bottom", # etiquetas a la derecha del símbolo
    nrow = 1, byrow = TRUE,   # una fila (ajusta según ancho)
    keywidth = unit(14, "pt"),
    keyheight = unit(10, "pt")
  ))


# acceso salud igualitario + ayuda economica
db_pond %>% 
  select(wave, igualdad_migrantes, ayuda_economica) %>% 
  na.omit() %>% 
  group_by(wave, igualdad_migrantes) %>%
  summarise(value = survey_mean(ayuda_economica, vartype = "ci")) %>% 
  ggplot(aes(x = wave, y = value, fill = igualdad_migrantes)) +
  geom_col(position = position_dodge2(padding = 0.01), width = 0.6) +
  scale_y_continuous(limits = c(0,3),n.breaks = 10) +
  scale_fill_manual(
    name = "Acceso a salud igualitario \npara migrantes",
    values = c("#2b2f3c","#7ABA21", "#F9913D")
  ) +
  labs(
    x = "Ola",
    y = "Ayuda económica",
    caption = "Fuente: elaboración propia con datos agrupados de ELSOC 2016-2023 (factores de expansión aplicados)"
  ) +
  theme_ggdist() +
  theme(
    legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
    legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
    
    legend.position       = "top",
    legend.direction      = "horizontal",
    legend.box            = "horizontal",  # útil si hubiera varias leyendas
    
    legend.title          = element_text(size = 11, face = "bold"),
    legend.text           = element_text(size = 11),
    legend.key            = element_rect(fill = "white", color = NA),
    legend.key.height     = unit(12, "pt"),
    legend.key.width      = unit(18, "pt"),
    
    axis.text  = element_text(size = 12),
    title      = element_text(size = 16)
  ) +
  guides(color = guide_legend(
    title.position = "top",   # título arriba
    label.position = "bottom", # etiquetas a la derecha del símbolo
    nrow = 1, byrow = TRUE,   # una fila (ajusta según ancho)
    keywidth = unit(14, "pt"),
    keyheight = unit(10, "pt")
  ))

# como cambian las trayectorias para personas ocn alta o baja educacion

