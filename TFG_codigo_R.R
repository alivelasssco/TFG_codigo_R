
#CONFIGURACIÓN INICIAL
rm(list = ls(all = TRUE))   # Limpiar el entorno de trabajo

# Librerías 
library(haven)
library(dplyr)
library(janitor)
library(writexl)
library(readxl)
library(psych)
library(moments)
library(ggplot2)
library(reshape2)
library(tidyr)
library(DescTools)
library(purrr)
library(tibble)
library(corrplot)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(MASS)
library(ROCR)

# Directorio de trabajo (carpeta propia con todos los ficheros .xpt)
setwd("C:/Users/alive/OneDrive/Escritorio/uni/Cuarto curso/Segundo cuatri/TFG/Archivos datos") #cambiar ruta según convenga


#CARGA Y SELECCIÓN DE VARIABLES POR MÓDULO
#Variables demográficas (DEMO_L)
demo <- read_xpt("DEMO_L.xpt") %>% clean_names()
demo <- demo %>%
  select(seqn, ridageyr, riagendr, ridreth3, dmdeduc2, dmdmartz) %>%
  rename(
    Id              = seqn,
    Edad            = ridageyr,
    Género          = riagendr,
    Raza            = ridreth3,
    Nivel_educativo = dmdeduc2,
    Estado_civil    = dmdmartz
  )


#Presión arterial y colesterol (BPQ_L)
bpq <- read_xpt("BPQ_L.xpt") %>% clean_names()
bpq <- bpq %>%
  select(seqn, bpq020, bpq150, bpq080, bpq101d) %>%
  rename(
    Id                     = seqn,
    Hipertensión           = bpq020,
    Medicación_hipertensión = bpq150,
    Colesterol_alto        = bpq080,
    Medicación_colesterol  = bpq101d
  )

#Condiciones médicas (MCQ_L)
mcq <- read_xpt("MCQ_L.xpt") %>% clean_names()
mcq <- mcq %>%
  select(seqn, mcq160e, mcq160f, mcq220, mcq160c) %>%
  rename(
    Id                  = seqn,
    Infarto             = mcq160e,
    Ictus               = mcq160f,
    Cáncer              = mcq220,
    Enfermedad_coronaria = mcq160c
  )

#Diabetes (DIQ_L)
diq <- read_xpt("DIQ_L.xpt") %>% clean_names()
diq <- diq %>%
  select(seqn, diq010, diq050, diq070) %>%
  rename(
    Id                  = seqn,
    Diabetes            = diq010,
    Insulina            = diq050,
    Medicación_diabetes = diq070
  )


#Uso de cigarrillos (SMQ_L)
smq <- read_xpt("SMQ_L.xpt") %>% clean_names()
smq <- smq %>%
  select(seqn, smq020, smq040) %>%
  rename(
    Id            = seqn,
    Fumador_vida  = smq020,
    Fumador_actual = smq040
  )

#Actividad física (PAQ_L)
paq <- read_xpt("PAQ_L.xpt") %>% clean_names()
paq <- paq %>%
  select(seqn, pad790q, pad790u, pad800, pad810q, pad810u, pad820, pad680) %>%
  rename(
    Id                      = seqn,
    Frecuencia_act_moderada = pad790q,
    Unidad_frec_moderada    = pad790u,
    Minutos_act_moderada    = pad800,
    Frecuencia_act_vigorosa = pad810q,
    Unidad_frec_vigorosa    = pad810u,
    Minutos_act_vigorosa    = pad820,
    Tiempo_sedentario       = pad680
  )

#Ingresos (INQ_L)
inq <- read_xpt("INQ_L.xpt") %>% clean_names()
inq <- inq %>%
  select(seqn, indfmmpi) %>%
  rename(
    Id                    = seqn,
    Índice_pobreza_mensual = indfmmpi
  )

#Seguro de salud (HIQ_L)
hiq <- read_xpt("HIQ_L.xpt") %>%
  clean_names() %>%
  select(seqn, hiq011, hiq032a, hiq032b, hiq032c, hiq032d,
         hiq032e, hiq032f, hiq032h, hiq032i) %>%
  rename(
    Id                         = seqn,
    Seguro_salud               = hiq011,
    Seguro_privado             = hiq032a,
    Medicare                   = hiq032b,
    Medi_gap                   = hiq032c,
    Medicaid                   = hiq032d,
    CHIP                       = hiq032e,
    Seguro_militar             = hiq032f,
    Plan_estatal               = hiq032h,
    Otro_seguro_gubernamental  = hiq032i
  )

# Crear indicadores binarios por tipo de seguro y variable resumen
hiq <- hiq %>%
  mutate(
    Seguro_salud = case_when(
      Seguro_salud == 1           ~ "Sí",
      Seguro_salud == 2           ~ "No",
      Seguro_salud %in% c(7, 9)  ~ NA_character_,
      TRUE                        ~ NA_character_
    ),
    
    Privado_bin  = case_when(
      Seguro_privado == 1              ~ 1,
      is.na(Seguro_privado)           ~ 0,
      Seguro_privado %in% c(77, 99)  ~ NA_real_,
      TRUE                            ~ NA_real_
    ),
    Medicare_bin = case_when(
      Medicare == 2   ~ 1,
      is.na(Medicare) ~ 0,
      TRUE            ~ NA_real_
    ),
    Medi_gap_bin = case_when(
      Medi_gap == 3   ~ 1,
      is.na(Medi_gap) ~ 0,
      TRUE            ~ NA_real_
    ),
    Medicaid_bin = case_when(
      Medicaid == 4   ~ 1,
      is.na(Medicaid) ~ 0,
      TRUE            ~ NA_real_
    ),
    CHIP_bin = case_when(
      CHIP == 5   ~ 1,
      is.na(CHIP) ~ 0,
      TRUE        ~ NA_real_
    ),
    Militar_bin = case_when(
      Seguro_militar == 6   ~ 1,
      is.na(Seguro_militar) ~ 0,
      TRUE                  ~ NA_real_
    ),
    Estatal_bin = case_when(
      Plan_estatal == 8   ~ 1,
      is.na(Plan_estatal) ~ 0,
      TRUE                ~ NA_real_
    ),
    Otro_gob_bin = case_when(
      Otro_seguro_gubernamental == 9   ~ 1,
      is.na(Otro_seguro_gubernamental) ~ 0,
      TRUE                             ~ NA_real_
    )
  ) %>%
  mutate(
    n_tipos_seguro = Privado_bin + Medicare_bin + Medi_gap_bin + Medicaid_bin +
      CHIP_bin + Militar_bin + Estatal_bin + Otro_gob_bin
  ) %>%
  mutate(
    Tipo_seguro = case_when(
      Seguro_salud == "No"  ~ "Sin seguro",
      is.na(Seguro_salud)   ~ NA_character_,
      n_tipos_seguro > 1    ~ "Cobertura múltiple", 
      Privado_bin == 1      ~ "Seguro privado",
      Militar_bin == 1      ~ "Seguro militar",
      Medicare_bin == 1 | Medi_gap_bin == 1 | Medicaid_bin == 1 |
        CHIP_bin == 1 | Estatal_bin == 1 | Otro_gob_bin == 1 ~ "Seguro público",
      TRUE                  ~ NA_character_
    )
  )


#Utilización hospitalaria y acceso a la atención (HUQ_L)
huq <- read_xpt("HUQ_L.xpt") %>% clean_names()
huq <- huq %>%
  select(seqn, huq010, huq042) %>%
  rename(
    Id                = seqn,
    Salud_general     = huq010,
    Tipo_centro_salud = huq042
  )


#Historial de peso (WHQ_L)
whq <- read_xpt("WHQ_L.xpt") %>% clean_names()
whq <- whq %>%
  select(seqn, whd010, whd020) %>%
  rename(
    Id     = seqn,
    Altura = whd010,
    Peso   = whd020
  )

#Uso preventivo de aspirina (RXQASA_L)
rxqasa <- read_xpt("RXQASA_L.xpt") %>% clean_names()
rxqasa <- rxqasa %>%
  select(seqn, rxq510, rxq515, rxq520) %>%
  rename(
    Id                        = seqn,
    Aspirina                  = rxq510,
    Indicación_aspirina       = rxq515,
    Aspirina_por_cuenta_propia = rxq520
  )



#UNIÓN DE TODOS LOS MÓDULOS EN UN ÚNICO DATASET
datos <- demo %>%
  left_join(bpq,    by = "Id") %>%
  left_join(mcq,    by = "Id") %>%
  left_join(diq,    by = "Id") %>%
  left_join(smq,    by = "Id") %>%
  left_join(paq,    by = "Id") %>%
  left_join(inq,    by = "Id") %>%
  left_join(
    hiq %>% select(Id, Seguro_salud, Tipo_seguro),
    by = "Id"
  ) %>%
  left_join(huq,    by = "Id") %>%
  left_join(whq,    by = "Id") %>%
  left_join(rxqasa, by = "Id")


#RECODIFICACIÓN DE VALORES NS/NC a NA
# Variables con códigos 7/9
vars_79 <- c(
  "Nivel_educativo", "Estado_civil",
  "Hipertensión", "Medicación_hipertensión",
  "Colesterol_alto", "Medicación_colesterol",
  "Infarto", "Ictus", "Cáncer", "Enfermedad_coronaria",
  "Diabetes", "Insulina", "Medicación_diabetes",
  "Fumador_vida", "Fumador_actual",
  "Salud_general",
  "Aspirina", "Indicación_aspirina", "Aspirina_por_cuenta_propia"
)
datos[vars_79] <- lapply(datos[vars_79], function(x) ifelse(x %in% c(7, 9), NA, x))

# Variables con códigos 77/99
vars_7799 <- c("Tipo_centro_salud")
datos[vars_7799] <- lapply(datos[vars_7799], function(x) ifelse(x %in% c(77, 99), NA, x))

# Variables con códigos 7777/9999
vars_77779999 <- c(
  "Frecuencia_act_moderada", "Minutos_act_moderada",
  "Frecuencia_act_vigorosa", "Minutos_act_vigorosa",
  "Tiempo_sedentario", "Altura", "Peso"
)
datos[vars_77779999] <- lapply(datos[vars_77779999], function(x) ifelse(x %in% c(7777, 9999), NA, x))


#ETIQUETADO Y TRANSFORMACIÓN DE VARIABLES
datos <- datos %>%
  mutate(
    #Sociodemográficas
    Género = factor(Género,
                    levels = c(1, 2),
                    labels = c("Hombre", "Mujer")),
    
    Raza = factor(Raza,
                  levels = c(1, 2, 3, 4, 6, 7),
                  labels = c("Hispano mexicano", "Otro hispano",
                             "Blanco no hispano", "Negro no hispano",
                             "Asiático no hispano", "Otra raza")),
    
    Nivel_educativo = factor(Nivel_educativo,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Sin estudios",
                                        "Secundaria incompleta",
                                        "Bachillerato/Secundaria completa",
                                        "Formación universitaria parcial/FP superior",
                                        "Universitario o más")),
    
    Estado_civil = factor(Estado_civil,
                          levels = c(1, 2, 3),
                          labels = c("Casado/Pareja",
                                     "Viudo/Separado/Divorciado",
                                     "Nunca casado")),
    
    #Salud general y acceso
    Salud_general = factor(Salud_general,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Excelente", "Muy buena",
                                      "Buena", "Regular", "Mala")),
    
    Tipo_centro_salud = factor(Tipo_centro_salud,
                               levels = c(1, 2, 3, 4, 5, 6),
                               labels = c("Clínica/centro salud",
                                          "Farmacia",
                                          "Urgencias",
                                          "Médico privado",
                                          "Otro sitio",
                                          "No suele ir a un único lugar")),
    
    #Condiciones médicas (patrón Sí/No)
    across(c(Hipertensión, Medicación_hipertensión,
             Colesterol_alto, Medicación_colesterol,
             Infarto, Ictus, Cáncer, Enfermedad_coronaria,
             Fumador_vida,
             Aspirina, Aspirina_por_cuenta_propia),
           ~ factor(.x, levels = c(1, 2), labels = c("Sí", "No"))),
    
    #Diabetes (categoría extra: borderline)
    Diabetes = factor(Diabetes,
                      levels = c(1, 2, 3),
                      labels = c("Sí", "No", "Borderline")),
    
    #Insulina y medicación para la diabetes
    across(c(Insulina, Medicación_diabetes),
           ~ factor(.x, levels = c(1, 2), labels = c("Sí", "No"))),
    
    #Hábito tabáquico actual
    Fumador_actual = factor(Fumador_actual,
                            levels = c(1, 2, 3),
                            labels = c("Todos los días",
                                       "Algunos días",
                                       "No fuma")),
    
    #Indicación de aspirina
    Indicación_aspirina = factor(Indicación_aspirina,
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Sí", "No", "A veces",
                                            "Dejó de tomar aspirina por efectos secundarios")),
    
    #IMC: calculado desde altura (pulgadas) y peso (libras)
    Altura_m = Altura * 0.0254,
    Peso_kg  = Peso   * 0.453592,
    IMC      = Peso_kg / Altura_m^2
  )


#AGRUPACIÓN DE CATEGORÍAS
datos <- datos %>%
  mutate(
    Nivel_educativo = case_when(
      Nivel_educativo %in% c("Sin estudios", "Secundaria incompleta") ~ "Bajo",
      Nivel_educativo == "Bachillerato/Secundaria completa" ~ "Medio",
      Nivel_educativo %in% c("Formación universitaria parcial/FP superior",
                             "Universitario o más") ~ "Alto",
      TRUE ~ NA_character_
    ),
    Raza = case_when(
      Raza %in% c("Hispano mexicano", "Otro hispano") ~ "Hispano",
      Raza == "Blanco no hispano" ~ "Blanco no hispano",
      Raza == "Negro no hispano" ~ "Negro no hispano",
      Raza %in% c("Asiático no hispano", "Otras razas") ~ "Otras razas no hispanas",
      TRUE ~ NA_character_
    ),
    Tipo_centro_salud = case_when(
      Tipo_centro_salud == "Clínica/centro salud" ~ "Clínica/centro ambulatorio",
      Tipo_centro_salud == "Médico privado" ~ "Médico privado",
      Tipo_centro_salud == "Urgencias" ~ "Urgencias",
      Tipo_centro_salud %in% c("Farmacia", "Otro sitio") ~ "Otros recursos",
      Tipo_centro_salud == "No suele ir a un único lugar" ~ "Sin centro habitual",
      TRUE ~ NA_character_
    ),
    Salud_general = case_when(
      Salud_general %in% c("Excelente", "Muy buena") ~ "Excelente/Muy buena",
      Salud_general == "Buena" ~ "Buena",
      Salud_general %in% c("Regular", "Mala") ~ "Regular/Mala",
      TRUE ~ NA_character_
    )
  )

#CREACIÓN DE VARIABLES DE ACTIVIDAD FÍSICA
datos <- datos %>%
  mutate(
    
    # Sesiones por semana (actividad moderada)
    Sesiones_moderada = case_when(
      Frecuencia_act_moderada == 0 ~ 0,
      is.na(Frecuencia_act_moderada) | is.na(Unidad_frec_moderada) ~ NA_real_,
      Unidad_frec_moderada == "D" ~ Frecuencia_act_moderada * 7,
      Unidad_frec_moderada == "W" ~ Frecuencia_act_moderada,
      Unidad_frec_moderada == "M" ~ Frecuencia_act_moderada * (12 / 52),
      Unidad_frec_moderada == "Y" ~ Frecuencia_act_moderada * (1 / 52),
      TRUE ~ NA_real_
    ),
    
    # Minutos semanales de actividad moderada
    Actividad_moderada = case_when(
      Sesiones_moderada == 0 ~ 0,
      is.na(Sesiones_moderada) | is.na(Minutos_act_moderada) ~ NA_real_,
      TRUE ~ Sesiones_moderada * Minutos_act_moderada
    ),
    
    # Sesiones por semana (actividad vigorosa)
    Sesiones_vigorosa = case_when(
      Frecuencia_act_vigorosa == 0 ~ 0,
      is.na(Frecuencia_act_vigorosa) | is.na(Unidad_frec_vigorosa) ~ NA_real_,
      Unidad_frec_vigorosa == "D" ~ Frecuencia_act_vigorosa * 7,
      Unidad_frec_vigorosa == "W" ~ Frecuencia_act_vigorosa,
      Unidad_frec_vigorosa == "M" ~ Frecuencia_act_vigorosa * (12 / 52),
      Unidad_frec_vigorosa == "Y" ~ Frecuencia_act_vigorosa * (1 / 52),
      TRUE ~ NA_real_
    ),
    
    # Minutos semanales de actividad vigorosa
    Actividad_vigorosa = case_when(
      Sesiones_vigorosa == 0 ~ 0,
      is.na(Sesiones_vigorosa) | is.na(Minutos_act_vigorosa) ~ NA_real_,
      TRUE ~ Sesiones_vigorosa * Minutos_act_vigorosa
    )
  )



#CATEGORIZACIÓN DE ACTIVIDAD FÍSICA (TERCILES)
# Percentiles 33 y 66 calculados solo sobre valores positivos
p_mod <- quantile(
  datos$Actividad_moderada[datos$Actividad_moderada > 0],
  probs = c(0.33, 0.66),
  na.rm = TRUE
)
p_vig <- quantile(
  datos$Actividad_vigorosa[datos$Actividad_vigorosa > 0],
  probs = c(0.33, 0.66),
  na.rm = TRUE
)

# Recodificación en 4 categorías (sin actividad / baja / media / alta)
datos <- datos %>%
  mutate(
    Actividad_moderada_cat = case_when(
      is.na(Actividad_moderada) ~ NA_character_,
      Actividad_moderada == 0 ~ "Sin actividad",
      Actividad_moderada > 0 & Actividad_moderada <= p_mod[1] ~ "Actividad baja",
      Actividad_moderada > p_mod[1] & Actividad_moderada <= p_mod[2] ~ "Actividad media",
      Actividad_moderada > p_mod[2] ~ "Actividad alta"
    ),
    Actividad_vigorosa_cat = case_when(
      is.na(Actividad_vigorosa) ~ NA_character_,
      Actividad_vigorosa == 0  ~ "Sin actividad",
      Actividad_vigorosa > 0 & Actividad_vigorosa <= p_vig[1] ~ "Actividad baja",
      Actividad_vigorosa > p_vig[1] & Actividad_vigorosa <= p_vig[2] ~ "Actividad media",
      Actividad_vigorosa > p_vig[2] ~ "Actividad alta"
    )
  )

#CONVERSIÓN FINAL DE VARIABLES A FACTOR
datos <- datos %>%
  mutate(
    Género                   = factor(Género),
    Raza                     = factor(Raza),
    Nivel_educativo          = factor(Nivel_educativo, ordered = TRUE),
    Estado_civil             = factor(Estado_civil),
    Hipertensión             = factor(Hipertensión),
    Medicación_hipertensión  = factor(Medicación_hipertensión),
    Colesterol_alto          = factor(Colesterol_alto),
    Medicación_colesterol    = factor(Medicación_colesterol),
    Infarto                  = factor(Infarto),
    Ictus                    = factor(Ictus),
    Cáncer                   = factor(Cáncer),
    Enfermedad_coronaria     = factor(Enfermedad_coronaria),
    Diabetes                 = factor(Diabetes),
    Insulina                 = factor(Insulina),
    Medicación_diabetes      = factor(Medicación_diabetes),
    Fumador_vida             = factor(Fumador_vida),
    Fumador_actual           = factor(Fumador_actual),
    Unidad_frec_moderada     = factor(Unidad_frec_moderada),
    Unidad_frec_vigorosa     = factor(Unidad_frec_vigorosa),
    Seguro_salud             = factor(Seguro_salud),
    Tipo_seguro              = factor(Tipo_seguro),
    Salud_general            = factor(Salud_general, ordered = TRUE),
    Tipo_centro_salud        = factor(Tipo_centro_salud),
    Aspirina                 = factor(Aspirina),
    Indicación_aspirina      = factor(Indicación_aspirina),
    Aspirina_por_cuenta_propia = factor(Aspirina_por_cuenta_propia), 
    Actividad_moderada_cat   = factor(Actividad_moderada_cat), 
    Actividad_vigorosa_cat   = factor(Actividad_vigorosa_cat)
    
    
  )

#EXPORTACIÓN DEL DATASET FINAL
#write_xlsx(datos, "C:/Users/alive/OneDrive/Escritorio/uni/Cuarto curso/Segundo cuatri/TFG/nhanes.xlsx") #se guarda dentro de la carpeta donde yo estoy trabajando


#ANÁLISIS UNIVARIANTE Y BIVARIANTE 
# Conversión de variables a factor
datos <- datos %>%
  mutate(
    Género                     = factor(Género),
    Raza                       = factor(Raza),
    Nivel_educativo            = factor(Nivel_educativo, ordered = TRUE),
    Estado_civil               = factor(Estado_civil),
    Hipertensión               = factor(Hipertensión),
    Medicación_hipertensión    = factor(Medicación_hipertensión),
    Colesterol_alto            = factor(Colesterol_alto),
    Medicación_colesterol      = factor(Medicación_colesterol),
    Infarto                    = factor(Infarto),
    Ictus                      = factor(Ictus),
    Cáncer                     = factor(Cáncer),
    Enfermedad_coronaria       = factor(Enfermedad_coronaria),
    Diabetes                   = factor(Diabetes),
    Insulina                   = factor(Insulina),
    Medicación_diabetes        = factor(Medicación_diabetes),
    Fumador_vida               = factor(Fumador_vida),
    Fumador_actual             = factor(Fumador_actual),
    Unidad_frec_moderada       = factor(Unidad_frec_moderada),
    Unidad_frec_vigorosa       = factor(Unidad_frec_vigorosa),
    Seguro_salud               = factor(Seguro_salud),
    Tipo_seguro                = factor(Tipo_seguro),
    Salud_general              = factor(Salud_general, ordered = TRUE),
    Tipo_centro_salud          = factor(Tipo_centro_salud),
    Aspirina                   = factor(Aspirina),
    Indicación_aspirina        = factor(Indicación_aspirina),
    Aspirina_por_cuenta_propia = factor(Aspirina_por_cuenta_propia),
    Actividad_moderada_cat     = factor(Actividad_moderada_cat),
    Actividad_vigorosa_cat     = factor(Actividad_vigorosa_cat)
  )



# ANÁLISIS UNIVARIANTE
#Variables numéricas: estadísticos descriptivos

vars_numericas <- c(
  "Edad", "Altura_m", "Peso_kg", "IMC",
  "Tiempo_sedentario", "Índice_pobreza_mensual",
  "Sesiones_moderada", "Actividad_moderada",
  "Sesiones_vigorosa", "Actividad_vigorosa"
)

tabla_univariante <- data.frame(
  Variable   = vars_numericas,
  Media      = sapply(datos[vars_numericas], mean,                  na.rm = TRUE),
  Mediana    = sapply(datos[vars_numericas], median,                na.rm = TRUE),
  Desv_tipica = sapply(datos[vars_numericas], sd,                   na.rm = TRUE),
  Minimo     = sapply(datos[vars_numericas], min,                   na.rm = TRUE),
  Q1         = sapply(datos[vars_numericas], quantile, 0.25,        na.rm = TRUE),
  Q3         = sapply(datos[vars_numericas], quantile, 0.75,        na.rm = TRUE),
  Maximo     = sapply(datos[vars_numericas], max,                   na.rm = TRUE),
  Asimetria  = sapply(datos[vars_numericas], moments::skewness,     na.rm = TRUE),
  Curtosis   = sapply(datos[vars_numericas], moments::kurtosis,     na.rm = TRUE)
)

tabla_univariante
sapply(datos[vars_numericas], skewness, na.rm = TRUE)


#Detección de valores atípicos (método IQR de Tukey)
tabla_outliers <- data.frame(
  Variable        = character(),
  Q1              = numeric(),
  Q3              = numeric(),
  IQR             = numeric(),
  Limite_inferior = numeric(),
  Limite_superior = numeric(),
  N_outliers      = numeric()
)

for (v in vars_numericas) {
  x   <- datos[[v]]
  x   <- x[!is.na(x)]
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- IQR(x)
  li  <- q1 - 1.5 * iqr
  ls  <- q3 + 1.5 * iqr
  
  tabla_outliers <- rbind(tabla_outliers, data.frame(
    Variable        = v,
    Q1              = q1,
    Q3              = q3,
    IQR             = iqr,
    Limite_inferior = li,
    Limite_superior = ls,
    N_outliers      = sum(x < li | x > ls)
  ))
}

tabla_outliers

#Winsorización de variables con atípicos (IMC y tiempo sedentario)
winsorize_tukey <- function(x) {
  Q1      <- quantile(x, 0.25, na.rm = TRUE, names = FALSE)
  Q3      <- quantile(x, 0.75, na.rm = TRUE, names = FALSE)
  IQR_val <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val
  
  x_w            <- x
  x_w[x < lim_inf] <- lim_inf
  x_w[x > lim_sup] <- lim_sup
  return(x_w)
}

datos <- datos %>%
  mutate(
    IMC_wins               = winsorize_tukey(IMC),
    Tiempo_sedentario_wins = winsorize_tukey(Tiempo_sedentario)
  )


#Variables cualitativas: tablas de frecuencia
vars_cualitativas <- c(
  "Género", "Raza", "Nivel_educativo", "Estado_civil",
  "Hipertensión", "Medicación_hipertensión",
  "Colesterol_alto", "Medicación_colesterol",
  "Infarto", "Ictus", "Enfermedad_coronaria",
  "Diabetes", "Insulina", "Medicación_diabetes",
  "Fumador_vida", "Fumador_actual",
  "Seguro_salud", "Tipo_seguro", "Salud_general",
  "Tipo_centro_salud", "Aspirina",
  "Índicacion_aspirina", "Aspirina_cuenta_propia",
  "Actividad_moderada_cat", "Actividad_vigorosa_cat"
)

# Función para obtener frecuencias absolutas y relativas
tabla_frecuencias <- function(variable) {
  frec_abs <- table(datos[[variable]], useNA = "ifany")
  frec_rel <- round(prop.table(frec_abs) * 100, 2)
  
  data.frame(
    Categoria  = names(frec_abs),
    Frecuencia = as.numeric(frec_abs),
    Porcentaje = as.numeric(frec_rel)
  )
}

tabla_frecuencias("Género")
tabla_frecuencias("Salud_general")
tabla_frecuencias("Actividad_moderada_cat")


#ANÁLISIS BIVARIANTE: CUALITATIVA VS CUALITATIVA
analisis_cuali_cuali <- function(data, var1, var2) {
  df <- data %>%
    select(all_of(c(var1, var2))) %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    mutate(
      !!var1 := as.factor(.data[[var1]]),
      !!var2 := as.factor(.data[[var2]])
    )
  tabla    <- table(df[[var1]], df[[var2]])
  pct_fila <- round(prop.table(tabla, margin = 1) * 100, 2)
  pct_col  <- round(prop.table(tabla, margin = 2) * 100, 2)
  # Prueba de independencia
  chi           <- suppressWarnings(chisq.test(tabla))
  expected_low  <- any(chi$expected < 5)
  
  if (!expected_low) {
    metodo      <- "Chi-cuadrado de Pearson"
    estadistico <- unname(chi$statistic)
    p_valor     <- chi$p.value
  } else if (all(dim(tabla) == c(2, 2))) {
    fisher      <- fisher.test(tabla)
    metodo      <- "Test exacto de Fisher"
    estadistico <- NA
    p_valor     <- fisher$p.value
  } else {
    chi_sim     <- chisq.test(tabla, simulate.p.value = TRUE, B = 5000)
    metodo      <- "Chi-cuadrado con p-valor simulado"
    estadistico <- unname(chi_sim$statistic)
    p_valor     <- chi_sim$p.value
  }
  
  #V de Cramer
  v_cramer <- CramerV(tabla)
  interpretacion_v <- case_when(
    v_cramer < 0.10 ~ "Muy débil",
    v_cramer < 0.30 ~ "Débil",
    v_cramer < 0.50 ~ "Moderada",
    TRUE            ~ "Fuerte"
  )
  
  resumen <- tibble(
    Variable_1   = var1,
    Variable_2   = var2,
    N_validos    = nrow(df),
    Metodo       = metodo,
    Estadistico  = estadistico,
    p_valor      = p_valor,
    V_Cramer     = round(v_cramer, 3),
    Intensidad   = interpretacion_v
  )
  
  return(list(
    tabla_frecuencias      = tabla,
    porcentajes_fila       = pct_fila,
    porcentajes_columna    = pct_col,
    frecuencias_esperadas  = round(chi$expected, 2),
    resumen                = resumen
  ))
}


#Análisis individuales de interés
res1 <- analisis_cuali_cuali(datos, "Hipertensión","Salud_general")
res2 <- analisis_cuali_cuali(datos, "Diabetes","Salud_general")
res3 <- analisis_cuali_cuali(datos, "Fumador_actual","Salud_general")
res4 <- analisis_cuali_cuali(datos, "Tipo_seguro","Salud_general")
res5 <- analisis_cuali_cuali(datos, "Hipertensión","Colesterol_alto")
res6 <- analisis_cuali_cuali(datos, "Hipertensión","Diabetes")
res7 <- analisis_cuali_cuali(datos, "Hipertensión","Enfermedad_coronaria")
res8 <- analisis_cuali_cuali(datos, "Actividad_moderada_cat","Salud_general")
res9 <- analisis_cuali_cuali(datos, "Actividad_vigorosa_cat","Salud_general")

# Consulta de resultados (tabla de contingencia, % por fila y resumen)
res1$tabla_frecuencias; res1$porcentajes_fila; res1$resumen
res2$tabla_frecuencias; res2$porcentajes_fila; res2$resumen
res3$tabla_frecuencias; res3$porcentajes_fila; res3$resumen
res4$tabla_frecuencias; res4$porcentajes_fila; res4$resumen
res5$tabla_frecuencias; res5$porcentajes_fila; res5$resumen
res6$tabla_frecuencias; res6$porcentajes_fila; res6$resumen
res7$tabla_frecuencias; res7$porcentajes_fila; res7$resumen
res8$tabla_frecuencias; res8$porcentajes_fila; res8$resumen
res9$tabla_frecuencias; res9$porcentajes_fila; res9$resumen


#Análisis en bloque: variables asociadas a la salud general
vars_salud_general <- c(
  "Diabetes", "Hipertensión", "Colesterol_alto", "Enfermedad_coronaria",
  "Fumador_vida", "Fumador_actual",
  "Tipo_centro_salud", "Seguro_salud", "Tipo_seguro",
  "Género", "Raza", "Nivel_educativo", "Estado_civil"
)

resultados_salud_general <- map(
  vars_salud_general,
  ~ analisis_cuali_cuali(datos, .x, "Salud_general")$resumen
) %>%
  bind_rows() %>%
  arrange(p_valor)

resultados_salud_general


#Análisis en bloque: perfil cardiometabólico
pares_cardiometabolico <- list(
  c("Hipertensión","Diabetes"),
  c("Hipertensión","Colesterol_alto"),
  c("Hipertensión","Enfermedad_coronaria"),
  c("Hipertensión","Infarto"),
  c("Hipertensión","Ictus"),
  c("Diabetes","Enfermedad_coronaria"),
  c("Diabetes","Infarto"),
  c("Colesterol_alto", "Enfermedad_coronaria"),
  c("Colesterol_alto", "Infarto"),
  c("Infarto","Enfermedad_coronaria"),
  c("Ictus","Enfermedad_coronaria")
)

resultados_cardiometabolico <- map(
  pares_cardiometabolico,
  ~ analisis_cuali_cuali(datos, .x[1], .x[2])$resumen
) %>%
  bind_rows() %>%
  arrange(p_valor)

resultados_cardiometabolico

#Análisis en bloque: contexto sanitario y hábitos
pares_contexto_habitos <- list(
  # Relación con salud general
  c("Seguro_salud","Salud_general"),
  c("Tipo_seguro","Salud_general"),
  c("Tipo_centro_salud","Salud_general"),
  c("Fumador_vida","Salud_general"),
  c("Fumador_actual","Salud_general"),
  
  # Relación con hipertensión
  c("Seguro_salud","Hipertensión"),
  c("Tipo_seguro","Hipertensión"),
  c("Tipo_centro_salud","Hipertensión"),
  c("Fumador_actual","Hipertensión"),
  
  # Relación con diabetes
  c("Seguro_salud","Diabetes"),
  c("Tipo_seguro","Diabetes"),
  c("Tipo_centro_salud","Diabetes"),
  c("Fumador_actual","Diabetes"),
  
  # Relación con enfermedad coronaria
  c("Seguro_salud","Enfermedad_coronaria"),
  c("Tipo_seguro","Enfermedad_coronaria"),
  c("Tipo_centro_salud","Enfermedad_coronaria"),
  c("Fumador_actual","Enfermedad_coronaria"),
  
  # Tabaquismo e infarto / colesterol
  c("Fumador_actual","Infarto"),
  c("Fumador_vida","Infarto"),
  c("Fumador_actual","Colesterol_alto")
)

resultados_contexto_habitos <- map(
  pares_contexto_habitos,
  ~ analisis_cuali_cuali(datos, .x[1], .x[2])$resumen
) %>%
  bind_rows() %>%
  arrange(p_valor)

resultados_contexto_habitos

resumen_cuali_cuanti <- function(data, var_cuali, var_cuanti) {
  data %>%
    select(all_of(c(var_cuali, var_cuanti))) %>%
    filter(!is.na(.data[[var_cuali]]), !is.na(.data[[var_cuanti]])) %>%
    mutate(
      !!var_cuali  := as.factor(.data[[var_cuali]]),
      !!var_cuanti := as.numeric(.data[[var_cuanti]])
    ) %>%
    group_by(.data[[var_cuali]]) %>%
    summarise(
      N       = n(),
      Media   = mean(.data[[var_cuanti]],             na.rm = TRUE),
      DT      = sd(.data[[var_cuanti]],               na.rm = TRUE),
      Mediana = median(.data[[var_cuanti]],           na.rm = TRUE),
      Q1      = quantile(.data[[var_cuanti]], 0.25,   na.rm = TRUE),
      Q3      = quantile(.data[[var_cuanti]], 0.75,   na.rm = TRUE),
      IQR     = IQR(.data[[var_cuanti]],              na.rm = TRUE),
      .groups = "drop"
    )
}

#ANÁLISIS BIVARIANTE: CUANTITATIVA VS CUANTITATIVA
# Selección de variables numéricas
datos_cuant <- datos %>%
  dplyr::select(
    Edad,
    IMC_wins,
    Índice_pobreza_mensual,
    Tiempo_sedentario_wins
  )

# Renombrar variables para que se vea mejor en el gráfico
datos_cuant_plot <- datos_cuant
names(datos_cuant_plot) <- c("Edad", "IMC", "Pobreza", "Sedentario")

# Matriz de correlación de Pearson
mat_pearson <- cor(
  datos_cuant_plot,
  method = "pearson",
  use = "complete.obs"
)

cor_pearson_long <- reshape2::melt(mat_pearson)
ggplot(cor_pearson_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
  scale_fill_gradient2(
    low = "#9C2F2F",     
    mid = "#F7F7F7",     
    high = "#2D2675",    
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  coord_fixed() +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(),
    panel.grid = element_blank()
  )

# Matriz de correlación de Spearman
mat_spearman <- cor(
  datos_cuant_plot,
  method = "spearman",
  use = "complete.obs"
)

cor_spearman_long <- reshape2::melt(mat_spearman)
ggplot(cor_spearman_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
  scale_fill_gradient2(
    low = "#9C2F2F",      
    mid = "#F7F7F7",     
    high = "#2D2675",    
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  coord_fixed() +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(),
    panel.grid = element_blank()
  )

#ANÁLISIS MULTIVARIANTE: MCA
#Cargar datos
datos <- read_xlsx("C:/Users/alive/OneDrive/Escritorio/uni/Cuarto curso/Segundo cuatri/TFG/nhanes.xlsx")

datos <- datos %>%
  mutate(
    Género                 = factor(Género),
    Raza                   = factor(Raza),
    Nivel_educativo        = factor(Nivel_educativo, ordered = TRUE),
    Estado_civil           = factor(Estado_civil),
    Hipertensión           = factor(Hipertensión),
    Colesterol_alto        = factor(Colesterol_alto),
    Fumador_actual         = factor(Fumador_actual),
    Seguro_salud           = factor(Seguro_salud),
    Tipo_seguro            = factor(Tipo_seguro),
    Salud_general          = factor(Salud_general, ordered = TRUE),
    Actividad_moderada_cat = factor(Actividad_moderada_cat)
  )

#Subdataset con variables categóricas
sub_datos <- datos %>%
  dplyr::select(
    Salud_general,
    Hipertensión,
    Colesterol_alto,
    Fumador_actual,
    Nivel_educativo,
    Género,
    Raza,
    Estado_civil,
    Actividad_moderada_cat,
    Tipo_seguro
  ) %>%
  na.omit() 

#ANÁLISIS MCA 
mca_result <- MCA(sub_datos, graph = FALSE)
eig_val <- factoextra::get_eigenvalue(mca_result)
head(eig_val)

#SCREE PLOT: varianza explicada por dimensión
fviz_screeplot(mca_result,
               addlabels = TRUE,
               ylim = c(0, 20),         
               barfill  = "steelblue",
               barcolor = "steelblue") +
  labs(title    = "",
       x = "Dimensiones",
       y = "Porcentaje de varianza explicada (%)") +
  theme_minimal()

grafico_inercia <- fviz_screeplot(mca_result,
                                  addlabels = TRUE,
                                  ylim = c(0, 20),
                                  barfill  = "steelblue",
                                  barcolor = "steelblue") +
  labs(title = "",
       x = "Dimensiones",
       y = "Porcentaje de varianza explicada (%)") +
  theme_minimal()

grafico_inercia

ggsave(
  filename = "screeplot_mca.png",
  plot = grafico_inercia,
  width = 10,
  height = 6,
  dpi = 300
)

#CONTRIBUCIÓN DE VARIABLES 
# Dimensión 1
fviz_contrib(mca_result,
             choice = "var",
             axes   = 1,
             top    = 15) +
  labs(title = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Dimensión 2
fviz_contrib(mca_result,
             choice = "var",
             axes   = 2,
             top    = 15) +
  labs(title = "", x="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Contribución conjunta a Dim 1 + 2
fviz_contrib(mca_result,
             choice = "var",
             axes   = 1:2,
             top    = 15) +
  labs(title = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#BIPLOT DE CATEGORÍAS
fviz_mca_var(mca_result,
             col.var  = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel    = TRUE,
             ggtheme  = theme_minimal()) +
  labs(title = "",
       subtitle = "Color según contribución a las dimensiones")

#BIPLOT DE INDIVIDUOS: perfiles similares
#Agrupados por Salud_general
fviz_mca_ind(mca_result,
             label = "none",
             habillage = "Salud_general",
             addEllipses = TRUE,
             ellipse.type = "norm",
             ellipse.level = 0.95,
             palette = "jco",
             alpha.ind = 0.40,
             ggtheme = theme_minimal()) +
  labs(title = "",
       subtitle = "Elipses de concentración por categoría")

#Agrupado por la variable Hipertensión
fviz_mca_ind(mca_result,
             label       = "none",
             habillage   = "Hipertensión",
             addEllipses = TRUE,
             ellipse.type = "norm",
             palette     = "jco",
             alpha.ind   = 0.40,
             ggtheme     = theme_minimal()) +
  labs(title = "", 
       subtitle = "Elipses de concentración por categoría")

#BIPLOT COMBINADO
fviz_mca_biplot(mca_result,
                label     = "var",        
                habillage = "Salud_general",
                addEllipses = TRUE,
                ellipse.type = "confidence",
                palette   = "jco",
                alpha.ind = 0.3,
                repel     = TRUE,
                ggtheme   = theme_minimal()) +
  labs(title    = "",
       subtitle = "Agrupación por salud general percibida")

#CLUSTERING
# PREPARACIÓN DE VARIABLES
datos_cluster <- datos %>%
  mutate(
    Salud_general          = factor(Salud_general, ordered = TRUE),
    Hipertensión           = factor(Hipertensión),
    Colesterol_alto        = factor(Colesterol_alto),
    Fumador_actual         = factor(Fumador_actual),
    Nivel_educativo        = factor(Nivel_educativo, ordered = TRUE),
    Género                 = factor(Género),
    Raza                   = factor(Raza),
    Estado_civil           = factor(Estado_civil),
    Actividad_moderada_cat = factor(Actividad_moderada_cat),
    Tipo_seguro            = factor(Tipo_seguro),
    
    IMC_wins               = as.numeric(IMC_wins),
    Edad                   = as.numeric(Edad),
    Índice_pobreza_mensual = as.numeric(Índice_pobreza_mensual),
    Tiempo_sedentario_wins = as.numeric(Tiempo_sedentario_wins)
  ) %>%
  select(
    Salud_general,
    Hipertensión,
    Colesterol_alto,
    Fumador_actual,
    Nivel_educativo,
    Género,
    Raza,
    Estado_civil,
    Actividad_moderada_cat,
    Tipo_seguro,
    IMC_wins,
    Edad,
    Índice_pobreza_mensual,
    Tiempo_sedentario_wins
  ) %>%
  na.omit()


# FAMD
res_famd <- FAMD(datos_cluster, graph = FALSE)

# Scree plot del FAMD
grafico_inercia_famd <- fviz_screeplot(
  res_famd,
  addlabels = TRUE,
  ylim = c(0, 20)
) +
  labs(
    title = "Varianza explicada por dimensión (FAMD)",
    x = "Dimensiones",
    y = "Porcentaje de varianza explicada (%)"
  ) +
  theme_minimal()

grafico_inercia_famd


# MÉTODO DEL CODO
coord_ind <- res_famd$ind$coord[, 1:5]
set.seed(123)

grafico_codo <- fviz_nbclust(
  coord_ind,
  kmeans,
  method = "wss",
  k.max = 10,
  nstart = 25
) +
  labs(
    title = "",
    x = "Número de clusters",
    y = "Inercia"
  ) +
  theme_minimal()

grafico_codo

# MÉTODO DE SILHOUETTE
set.seed(123)

grafico_silhouette <- fviz_nbclust(
  coord_ind,
  kmeans,
  method = "silhouette",
  k.max = 10,
  nstart = 25
) +
  labs(
    title = "Coeficiente de Silhouette para determinar el número de clusters",
    x = "Número de clusters",
    y = "Coeficiente medio de Silhouette"
  ) +
  theme_minimal()

grafico_silhouette

#Valores del coeficiente medio de Silhouette
silhouette_scores <- data.frame(
  k = 2:10,
  silhouette_media = sapply(2:10, function(k) {
    modelo_kmeans <- kmeans(coord_ind, centers = k, nstart = 25)
    sil <- silhouette(modelo_kmeans$cluster, dist(coord_ind))
    mean(sil[, 3])
  })
)
silhouette_scores

# CLUSTERING FINAL CON k = 3
set.seed(123)

cluster_kmeans <- kmeans(
  coord_ind,
  centers = 3,
  nstart = 25
)

# Añadir el cluster al dataframe
datos_cluster$Cluster <- factor(cluster_kmeans$cluster)

# REPRESENTACIÓN DE INDIVIDUOS SEGÚN CLUSTER
# Calcular centroides de cada cluster en las dos primeras dimensiones
geom_point(
  data = centroides,
  aes(x = Dim1, y = Dim2),
  inherit.aes = FALSE,
  shape = 4,       
  color = "black",
  size = 2,       
  stroke = 0.8    
) 

centroides <- as.data.frame(cluster_kmeans$centers[, 1:2])
colnames(centroides) <- c("Dim1", "Dim2")

grafico_clusters <- fviz_cluster(
  cluster_kmeans,
  data = coord_ind,
  geom = "point",
  ellipse.type = "norm",
  ellipse.level = 0.68,
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "Distribución de individuos según cluster"
) +
  geom_point(
    data = centroides,
    aes(x = Dim1, y = Dim2),
    inherit.aes = FALSE,
    shape = 4,
    color = "black",
    size = 2,
    stroke = 0.8
  ) +
  labs(
    title = "",
    x = "Dimensión 1",
    y = "Dimensión 2"
  )

grafico_clusters

# INDICADORES CLAVE POR CLUSTER
datos_cluster$Cluster <- factor(cluster_kmeans$cluster)

porcentaje <- function(condicion) {
  round(mean(condicion, na.rm = TRUE) * 100, 2)
}

# Tabla resumen de indicadores clave
porcentaje <- function(condicion) {
  round(mean(condicion, na.rm = TRUE) * 100, 2)
}

datos_cluster$Cluster <- factor(cluster_kmeans$cluster)

# Tabla de indicadores clave por cluster
indicadores_cluster <- datos_cluster %>%
  group_by(Cluster) %>%
  summarise(
    `Tamaño del grupo` = n(),
    `Edad media` = round(mean(Edad, na.rm = TRUE), 2),
    `IMC medio` = round(mean(IMC_wins, na.rm = TRUE), 2),
    `Índice de pobreza medio` = round(mean(Índice_pobreza_mensual, na.rm = TRUE), 2),
    `Tiempo sedentario medio` = round(mean(Tiempo_sedentario_wins, na.rm = TRUE), 2),
    `Salud regular/mala (%)` = porcentaje(Salud_general == "Regular/Mala"),
    `Hipertensión (%)` = porcentaje(Hipertensión %in% c("Sí", "Hipertensión_Sí")),
    `Colesterol alto (%)` = porcentaje(Colesterol_alto %in% c("Sí", "Colesterol_alto_Sí")),
    `Fumador diario (%)` = porcentaje(Fumador_actual == "Todos los días"),
    `Nivel educativo bajo (%)` = porcentaje(Nivel_educativo == "Bajo"),
    `Sin actividad (%)` = porcentaje(Actividad_moderada_cat == "Sin actividad"),
    `Seguro público (%)` = porcentaje(Tipo_seguro == "Seguro público"),
    .groups = "drop"
  )


indicadores_cluster_tabla <- indicadores_cluster %>%
  pivot_longer(
    cols = -Cluster,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    names_from = Cluster,
    values_from = Valor,
    names_prefix = "Clúster "
  )

indicadores_cluster_tabla

#Formato tabla
indicadores_cluster <- indicadores_cluster %>%
  pivot_longer(
    cols = -Cluster,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    names_from = Cluster,
    values_from = Valor,
    names_prefix = "Cluster "
  )

indicadores_cluster


#IMPLEMENTACIÓN DE MODELOS
#REGRESIÓN PROBIT ORDINAL
#Modelo 1: modelo base
datos$Salud_general <- ordered(
  datos$Salud_general,
  levels = c("Excelente/Muy buena", "Buena", "Regular/Mala")
)
datos$Nivel_educativo <- factor(
  datos$Nivel_educativo,
  levels = c("Bajo", "Medio", "Alto")
)
datos$Estado_civil <- factor(datos$Estado_civil)
datos$Género <- factor(datos$Género)
datos$Raza <- factor(datos$Raza)

m1 <- polr(
  Salud_general ~ Edad + Género + Raza + Nivel_educativo + Estado_civil, 
  data = datos,
  method = "probit",
  Hess = TRUE
)

summary(m1)
tabla_coeficientes <- coef(summary(m1))
p_valores <- pnorm(abs(tabla_coeficientes[, "t value"]), lower.tail = FALSE) * 2
resultado1 <- cbind(tabla_coeficientes, "p value" = p_valores)
resultado1

#Modelo 2: interacción entre edad y género
datos$Actividad_moderada_cat <- factor(
  datos$Actividad_moderada_cat,
  levels = c("Sin actividad", "Actividad media", "Actividad alta")
)

m2 <- polr(
  Salud_general ~ Edad * Género + Raza + Nivel_educativo + Estado_civil,
  data = datos,
  method = "probit",
  Hess = TRUE
)

summary(m2)
tabla_coeficientes <- coef(summary(m2))
p_valores <- pnorm(abs(tabla_coeficientes[, "t value"]), lower.tail = FALSE) * 2
resultado2 <- cbind(tabla_coeficientes, "p value" = p_valores)
resultado2

#Modelo 3: interacción entre edad y nivel educativo
datos$Fumador_actual <- factor(
  datos$Fumador_actual,
  levels = c("No fuma", "Algunos días", "Todos los días")
)
datos$Actividad_moderada_cat <- factor(
  datos$Actividad_moderada_cat,
  levels = c("Sin actividad", "Actividad media", "Actividad alta")
)

m3 <- polr(
  Salud_general ~ Edad * Nivel_educativo + Género + Raza + Estado_civil,
  data = datos,
  method = "probit",
  Hess = TRUE
)

summary(m3)
tabla_coeficientes <- coef(summary(m3))
p_valores <- pnorm(abs(tabla_coeficientes[, "t value"]), lower.tail = FALSE) * 2
resultado3 <- cbind(tabla_coeficientes, "p value" = p_valores)
resultado3
AIC(m1, m2, m3)

#Modelo 4: Modelo ampliado con variables cardiometabólicas y de estilo de vida
m4 <- polr(
  Salud_general ~ Edad + Género + Raza + Nivel_educativo + Estado_civil +
    Índice_pobreza_mensual + Hipertensión + Colesterol_alto +
    IMC_wins + Fumador_actual + Tiempo_sedentario_wins +
    Actividad_moderada_cat,
  data = datos,
  method = "probit",
  Hess = TRUE
)

summary(m4)

summary(m4)
tabla_coeficientes <- coef(summary(m4))
p_valores <- pnorm(abs(tabla_coeficientes[, "t value"]), lower.tail = FALSE) * 2
resultado4 <- cbind(tabla_coeficientes, "p value" = p_valores)
resultado4


#REGRESIÓN LOGÍSTICA BINARIA
datos$Hipertension_bin <- ifelse(
  datos$Hipertensión == "Sí", 1,
  ifelse(datos$Hipertensión == "No", 0, NA)
)
datos$Colesterol_alto_bin <- ifelse(
  datos$Colesterol_alto == "Sí", 1,
  ifelse(datos$Colesterol_alto == "No", 0, NA)
)
datos$Género <- factor(datos$Género)
datos$Raza <- factor(datos$Raza)
datos$Nivel_educativo <- factor(
  datos$Nivel_educativo,
  levels = c("Bajo", "Medio", "Alto")
)
datos$Estado_civil <- factor(datos$Estado_civil)
datos$Fumador_actual <- factor(datos$Fumador_actual)
datos$Actividad_moderada_cat <- factor(datos$Actividad_moderada_cat)
datos$Actividad_moderada_cat <- factor(
  datos$Actividad_moderada_cat,
  levels = c("Sin actividad", "Actividad media", "Actividad alta")
)

#Modelo con Hipertensión
m5 <- glm(
  Hipertension_bin ~ Edad + Género + Raza + Nivel_educativo + Estado_civil +
    Índice_pobreza_mensual + IMC_wins + Colesterol_alto_bin +
    Fumador_actual + Tiempo_sedentario_wins + Actividad_moderada_cat,
  data = datos,
  family = binomial(link = "logit")
)

summary(m5)
OR_m5 <- exp(coef(m5))
OR_m5

#Modelo con Colesterol alto
m6 <- glm(
  Colesterol_alto_bin ~ Edad + Género + Raza + Nivel_educativo + Estado_civil +
    Índice_pobreza_mensual + IMC_wins + Hipertension_bin +
    Fumador_actual + Tiempo_sedentario_wins + Actividad_moderada_cat,
  data = datos,
  family = binomial(link = "logit")
)

summary(m6)
OR_m6 <- exp(coef(m6))
OR_m6

#CURVAS ROC PARA LOS MODELOS LOGÍSTICOS
#modelo Hipertensión
prob_m5 <- predict(m5, type = "response")
datos_m5 <- model.frame(m5)
real_m5 <- datos_m5$Hipertension_bin
pred_m5 <- prediction(prob_m5, real_m5)

# Calcular sensibilidad y 1-especificidad
perf_m5 <- performance(pred_m5, "tpr", "fpr")

# Representar curva ROC
plot(
  perf_m5,
  main = "Curva ROC del modelo logístico para Hipertensión",
  col = "blue",
  lwd = 2
)
abline(0, 1, lty = 2, col = "gray")

#AUC
auc_m5 <- performance(pred_m5, "auc")
auc_m5_valor <- auc_m5@y.values[[1]]
auc_m5_valor


#modelo colesterol alto
prob_m6 <- predict(m6, type = "response")
datos_m6 <- model.frame(m6)
real_m6 <- datos_m6$Colesterol_alto_bin
pred_m6 <- prediction(prob_m6, real_m6)
perf_m6 <- performance(pred_m6, "tpr", "fpr")

# Representar curva ROC
plot(
  perf_m6,
  main = "Curva ROC del modelo logístico para Colesterol alto",
  col = "red",
  lwd = 2
)

abline(0, 1, lty = 2, col = "gray")

#AUC
auc_m6 <- performance(pred_m6, "auc")
auc_m6_valor <- auc_m6@y.values[[1]]
auc_m6_valor


# Comparación ambas curvas ROC
plot(
  perf_m5,
  main = "",
  col = "blue",
  lwd = 2
)

plot(
  perf_m6,
  add = TRUE,
  col = "red",
  lwd = 2
)

abline(0, 1, lty = 2, col = "gray")
