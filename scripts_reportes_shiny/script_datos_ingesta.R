library(pacman)
p_load(readr, tidyverse, plyr, dplyr, tidyr, reshape, scales, ggplot2, kableExtra, ggthemes, ggrepel, data.table, zoo)

setwd("/Users/ASM/Documents/Rizika/Incidencia-delictiva/")

#incidencia fuero comÃºn
incidencia <- read_csv("/Users/ASM/Documents/Rizika/Incidencia-delictiva/datos_ingesta/Municipal-Delitos-2015-2021_sep2021.csv", 
                       locale = locale(encoding = "latin1"))


# simplificar mÃ¡s de una modalidad 
robo_v_cv <- c("Robo de coche de 4 ruedas Con violencia",
               "Robo de embarcaciones pequeñas y grandes Con violencia",
               "Robo de motocicleta Con violencia")
robo_v_sv <- c("Robo de coche de 4 ruedas Sin violencia",
               "Robo de embarcaciones pequeñas y grandes Sin violencia",
               "Robo de motocicleta Sin violencia")

violacion <- c("Violación equiparada",
               "Violación simple")

subtipos <- c("Homicidio doloso", "Secuestro", "Extorsión", "Robo de vehículo automotor",
              "Feminicidio", "Robo a transportista", 
              "Robo a transeúnte en espacio abierto al público", 
              "Robo a transeúnte en vía pública", "Robo a negocio", 
              "Robo en transporte individual", "Robo en transporte público colectivo",
              "Robo en transporte público individual", "Robo de ganado", "Narcomenudeo", 
              "Violencia familiar", "Lesiones dolosas",
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", "Robo a institución bancaria", "Robo de maquinaria", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Corrupción de menores", "Trata de personas", "Rapto", "Tráfico de menores", "Violación equiparada", "Violación simple", "Robo a casa habitación", "Allanamiento de morada", "Homicidio culposo", "Lesiones culposas", "Daño a la propiedad", "Despojo", "Fraude", "Falsificación")

incidencia <- incidencia %>% 
  filter(`Subtipo de delito` %in% subtipos)

incidencia <- incidencia %>% 
  mutate(delito=ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_cv, "Robo de vehículo con violencia",
                       ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_sv, "Robo de vehículo sin violencia",
                              ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Con violencia", "Robo a transportista con violencia",
                                     ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Sin violencia", "Robo a transportista sin violencia",
                                            ifelse(`Subtipo de delito`=="Violación simple" & Modalidad %in% violacion, "Violación", 
                                                   ifelse(`Subtipo de delito`=="Violación equiparada" & Modalidad %in% violacion, "Violación", `Subtipo de delito` )))))))


incidencia <- incidencia %>% 
  mutate(delito_ab = ifelse(delito=="Homicidio doloso", "homicidio_dol",
                            ifelse(delito=="Lesiones dolosas", "lesiones_dol",
                                   ifelse(delito=="Feminicidio", "feminicidio",
                                          ifelse(delito=="Secuestro", "secuestro",
                                                 ifelse(delito=="Robo de vehículo con violencia", "robo_veh_cv",
                                                        ifelse(delito=="Robo de vehículo sin violencia", "robo_veh_sv",
                                                               ifelse(delito=="Robo a transportista con violencia", "robo_transportista_cv",
                                                                      ifelse(delito=="Robo a transportista sin violencia", "robo_transportista_sv",
                                                                             ifelse(delito=="Robo a transeúnte en vía pública", "robo_transeunte_viap",
                                                                                    ifelse(delito=="Robo a transeúnte en espacio abierto al público", "robo_transeunte_ep",
                                                                                           ifelse(delito=="Robo en transporte público individual", "robo_transppub_ind",
                                                                                                  ifelse(delito=="Robo en transporte público colectivo", "robo_transppub_col",
                                                                                                         ifelse(delito=="Robo en transporte individual", "robo_transp_ind",
                                                                                                                ifelse(delito=="Robo a negocio", "robo_negocio",
                                                                                                                       ifelse(delito=="Robo de ganado", "robo_ganado",
                                                                                                                              ifelse(delito=="Extorsión", "extorsion",
                                                                                                                                     ifelse(delito=="Violencia familiar", "viol_familiar",
                                                                                                                                            ifelse(delito=="Robo de maquinaria", "robo_maquinaria",       
                                                                                                                                                   ifelse(delito=="Violencia de género en todas sus modalidades distinta a la violencia familiar", "viol_genero",
                                                                                                                                                          ifelse(delito=="Robo a institución bancaria", "robo_banco", 
                                                                                                                                                                 ifelse( delito == "Homicidio culposo", "homicidio_cul", 
                                                                                                                                                                         ifelse(delito == "Lesiones culposas", "lesiones_cul", 
                                                                                                                                                                                ifelse( delito == "Falsificación", "falsificacion", 
                                                                                                                                                                                        ifelse(delito == "Fraude", "fraude", 
                                                                                                                                                                                               ifelse(delito == "Fraude", "fraude",
                                                                                                                                                                                                      ifelse(delito == "Daño a la propiedad", "dano_propiedad", 
                                                                                                                                                                                                             ifelse(delito == "Robo a casa habitación", "robo_casa", 
                                                                                                                                                                                                                    ifelse(delito == "Allanamiento de morada", "allanamiento", 
                                                                                                                                                                                                                           ifelse(delito == "Violación", "violacion", 
                                                                                                                                                                                                                                  ifelse(delito == "Tráfico de menores", "trafico_menores", 
                                                                                                                                                                                                                                         ifelse(delito == "Rapto", "rapto", 
                                                                                                                                                                                                                                                ifelse(delito == "Trata de personas", "trata",
                                                                                                                                                                                                                                                       ifelse(delito == "Corrupción de menores", "corr_menores", 
                                                                                                                                                                                                                                                              ifelse(delito == "Hostigamiento sexual", "hostiga_sex", 
                                                                                                                                                                                                                                                                     ifelse(delito == "Acoso sexual", "acoso_sex", 
                                                                                                                                                                                                                                                                            ifelse(delito == "Abuso sexual", "abuso_sex", 
                                                                                                                                                                                                                                                                                   ifelse(delito == "Narcomenudeo", "narcomenudeo", 
                                                                                                                                                                                                                                                                                          ifelse(delito == "Despojo", "despojo", "otros_delitos")))))))))))))))))))))))))))))))))))))))

incidencia <-  gather(incidencia, mes, value = total_mes, "Enero":"Diciembre")

incidencia_agrupada <- aggregate(total_mes ~ Año + mes + Clave_Ent + Entidad + delito + delito_ab , data= incidencia, sum)


incidencia_agrupada <- incidencia_agrupada %>% 
  mutate(mes_num = ifelse(mes == "Enero", 01,
                          ifelse(mes == "Febrero", 02, 
                                 ifelse( mes == "Marzo", 03,
                                         ifelse( mes == "Abril", 04,
                                                 ifelse( mes == "Mayo", 05, 
                                                         ifelse(mes == "Junio", 6,
                                                                ifelse(mes == "Julio", 7, 
                                                                       ifelse(mes == "Agosto", 8,
                                                                              ifelse(mes == "Septiembre", 9,
                                                                                     ifelse(mes == "Octubre", 10,
                                                                                            ifelse(mes == "Noviembre", 11, 12))))))))))))

incidencia_agrupada$dia <- "01"

incidencia_agrupada <- incidencia_agrupada %>% 
  mutate(fecha_amd = paste0(incidencia_agrupada$Año, "-", incidencia_agrupada$mes_num, "-", incidencia_agrupada$dia))

p_load(lubridate)

incidencia_agrupada$fecha_amd <- as_date(incidencia_agrupada$fecha_amd)

incidencia_agrupada <- incidencia_agrupada %>% 
  arrange(Clave_Ent, delito_ab, fecha_amd)


incidencia_agrupada <- incidencia_agrupada %>% 
  group_by(Clave_Ent, Entidad, delito, delito_ab) %>%
  dplyr::mutate(total_12_meses = rollsum(x = total_mes, 12, align = "right", fill = NA))

pob_1 <- read_csv("/Users/ASM/Documents/Rizika/pob-geo/pob_mun 2015-2030/base_municipios_final_datos_01.csv", locale = locale(encoding = "latin1"))

pob_2 <- read_csv("/Users/ASM/Documents/Rizika/pob-geo/pob_mun 2015-2030/base_municipios_final_datos_02.csv", locale = locale(encoding = "latin1"))

pobtot <- rbind(pob_1, pob_2)

pobtot <- pobtot %>% 
  select(CLAVE_ENT, NOM_ENT, AÑO, POB)

pobtot_ent <- aggregate(POB ~ CLAVE_ENT + AÑO, data= pobtot, sum)

names(pobtot_ent)[1] <- "Clave_Ent"

names(pobtot_ent)[2] <- "Año"

names(pobtot_ent)[3] <- "pob_tot"

incidencia_agrupada <- left_join(incidencia_agrupada, pobtot_ent, by = c("Clave_Ent", "Año"))

incidencia_agrupada <- incidencia_agrupada %>% 
  mutate(tasa_anual = (total_12_meses/pob_tot)*100000 )

incidencia_agrupada$tasa_anual <- round(incidencia_agrupada$tasa_anual, 2)

incidencia_agrupada_tempo <- incidencia_agrupada %>% 
  ungroup() %>% 
  select(Clave_Ent, Entidad, delito_ab, fecha_amd, tasa_anual)

names(incidencia_agrupada_tempo)[5] <- "tasa_anual_pasada"

incidencia_agrupada_tempo$fecha_amd <- incidencia_agrupada_tempo$fecha_amd + years(1)

incidencia_agrupada <- left_join(incidencia_agrupada, incidencia_agrupada_tempo, by = c("Clave_Ent", "Entidad", "delito_ab", "fecha_amd"))

incidencia_agrupada <- incidencia_agrupada %>% 
  mutate(tasa_cambio = (tasa_anual-tasa_anual_pasada/tasa_anual_pasada)*100 )


write.csv(incidencia_agrupada, "/Users/ASM/Documents/Rizika/Incidencia-delictiva/datos_ingesta/incidencia_agrupada.csv")
