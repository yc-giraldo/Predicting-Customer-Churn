getwd()
list.files()
install.packages("tidyverse")
# CASO: PREDICTING CUSTOMER CHURN AT QWE INC.
# PUNTO 1. RECONOCIMIENTO DE LA BASE DE DATOS
library(readxl)
library(tidyverse)
str(datos)


# CARGAR LOS DATOS NUEVAMENTE

library(readxl)
library(ggplot2)

# Importar la base
datos <- read_excel("DATA.xlsx", sheet = "Case Data")

# Crear variable de estado
datos$Estado <- ifelse(
  datos$`Churn (1 = Yes, 0 = No)` == 1,
  "Abandonaron",
  "Permanecieron"
)

# Comprobar que todo funciona
dim(datos)
table(datos$Estado)

# 1.1. CALIDAD DE LOS DATOS

# Valores faltantes por variable
colSums(is.na(datos))

# Total de valores faltantes
sum(is.na(datos))

# Filas duplicadas
sum(duplicated(datos))

# Clientes con ID repetido
sum(duplicated(datos$ID))
# 5. DISTRIBUCION DEL ABANDONO

# Cantidad de clientes por grupo
table(datos$`Churn (1 = Yes, 0 = No)`)

# Porcentaje de clientes por grupo
round(
  prop.table(table(datos$`Churn (1 = Yes, 0 = No)`)) * 100,
  2
)

# 1.2. GRAFICA DE DISTRIBUCION DEL ABANDONO

# Cargar libreria para graficas
library(ggplot2)

# Crear tabla con los resultados de Churn
tabla_churn <- data.frame(
  Estado = c("Permanecieron", "Abandonaron"),
  Clientes = c(6024, 323)
)

# Calcular porcentajes
tabla_churn$Porcentaje <- round(
  tabla_churn$Clientes / sum(tabla_churn$Clientes) * 100,
  2
)

# Crear grafica
grafica_churn <- ggplot(
  tabla_churn,
  aes(x = Estado, y = Clientes, fill = Estado)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(Clientes, " (", Porcentaje, "%)")),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Permanecieron" = "#237A91",
      "Abandonaron" = "#E58A62"
    )
  ) +
  labs(
    title = "Distribucion del abandono de clientes",
    x = "Estado del cliente",
    y = "Numero de clientes"
  ) +
  expand_limits(y = 6800) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar grafica
ggsave(
  "Grafica_Churn.png",
  plot = grafica_churn,
  width = 8,
  height = 5,
  dpi = 300
)
# Comprobar que se guardo
file.exists("Grafica_Churn.png")

# 1.3. ESTADISTICAS DESCRIPTIVAS
# Resumen general de las variables
summary(datos)

# Estadisticas de las variables explicativas
variables <- datos[, -c(1, 3)]

tabla_descriptiva <- data.frame(
  Variable = names(variables),
  Media = sapply(variables, mean, na.rm = TRUE),
  Mediana = sapply(variables, median, na.rm = TRUE),
  Minimo = sapply(variables, min, na.rm = TRUE),
  Maximo = sapply(variables, max, na.rm = TRUE),
  Desviacion = sapply(variables, sd, na.rm = TRUE),
  row.names = NULL
)

# Redondear resultados
tabla_descriptiva[, -1] <- round(
  tabla_descriptiva[, -1], 2
)

# GUARDAR TABLA DESCRIPTIVA COMO PNG

library(gridExtra)
library(grid)

# Crear tabla visual
tabla_visual <- tableGrob(
  tabla_descriptiva,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 10
  )
)

# Guardar como imagen PNG
png(
  filename = "Tabla_Descriptiva.png",
  width = 1800,
  height = 1000,
  res = 180
)

grid.newpage()
grid.draw(tabla_visual)

dev.off()

# Verificar que se guardo
file.exists("Tabla_Descriptiva.png")

# 1.4. ANTIGUEDAD DE LOS CLIENTES SEGUN CHURN

# Crear grupos para comparar
datos$Estado <- ifelse(
  datos$`Churn (1 = Yes, 0 = No)` == 1,
  "Abandonaron",
  "Permanecieron"
)

# Calcular antiguedad promedio por grupo
tabla_antiguedad <- aggregate(
  datos$`Customer Age (in months)`,
  by = list(Estado = datos$Estado),
  FUN = mean
)

names(tabla_antiguedad)[2] <- "Antiguedad_Promedio"

tabla_antiguedad$Antiguedad_Promedio <- round(
  tabla_antiguedad$Antiguedad_Promedio, 2
)

print(tabla_antiguedad)

# Crear grafica comparativa
grafica_antiguedad <- ggplot(
  datos,
  aes(
    x = Estado,
    y = `Customer Age (in months)`,
    fill = Estado
  )
) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Antiguedad de los clientes segun abandono",
    x = "Estado del cliente",
    y = "Antiguedad en meses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar grafica
ggsave(
  "Grafica_Antiguedad.png",
  plot = grafica_antiguedad,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Verificar archivo
file.exists("Grafica_Antiguedad.png")

# 1.5. SATISFACCION DEL CLIENTE SEGUN CHURN

# Crear variable de estado si aun no existe
datos$Estado <- ifelse(
  datos$`Churn (1 = Yes, 0 = No)` == 1,
  "Abandonaron",
  "Permanecieron"
)

# Calcular promedios por grupo
tabla_chi <- aggregate(
  datos[, c("CHI Score Month 0", "CHI Score 0-1")],
  by = list(Estado = datos$Estado),
  FUN = mean
)

# Redondear resultados
tabla_chi[, -1] <- round(tabla_chi[, -1], 2)

# Mostrar resultados
print(tabla_chi)

# GRAFICA 1. NIVEL DE SATISFACCION

grafica_chi <- ggplot(
  datos,
  aes(
    x = Estado,
    y = `CHI Score Month 0`,
    fill = Estado
  )
) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Satisfaccion del cliente segun abandono",
    x = "Estado del cliente",
    y = "CHI Score - Mes 0"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  "Grafica_CHI.png",
  plot = grafica_chi,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


# GRAFICA 2. CAMBIO EN LA SATISFACCION

grafica_cambio_chi <- ggplot(
  datos,
  aes(
    x = Estado,
    y = `CHI Score 0-1`,
    fill = Estado
  )
) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Cambio en la satisfaccion segun abandono",
    x = "Estado del cliente",
    y = "Cambio del CHI respecto al mes anterior"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  "Grafica_Cambio_CHI.png",
  plot = grafica_cambio_chi,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# VERIFICAR ARCHIVOS

file.exists("Grafica_CHI.png")
file.exists("Grafica_Cambio_CHI.png")
print(tabla_antiguedad)
print(tabla_chi)

# 1.6. ANALISIS DE SOPORTE TECNICO

# Calcular promedios por grupo
tabla_soporte <- aggregate(
  datos[, c(
    "Support Cases Month 0",
    "Support Cases 0-1",
    "SP Month 0",
    "SP 0-1"
  )],
  by = list(Estado = datos$Estado),
  FUN = mean
)

# Redondear resultados
tabla_soporte[, -1] <- round(tabla_soporte[, -1], 2)

# Mostrar tabla
print(tabla_soporte)

# Crear grafica de casos de soporte
grafica_soporte <- ggplot(
  datos,
  aes(
    x = Estado,
    y = `Support Cases Month 0`,
    fill = Estado
  )
) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Casos de soporte segun abandono",
    x = "Estado del cliente",
    y = "Casos de soporte - Mes 0"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar grafica
ggsave(
  "Grafica_Soporte.png",
  plot = grafica_soporte,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)
# Verificar archivo
file.exists("Grafica_Soporte.png")
print(tabla_soporte)


# 1.7. ACTIVIDAD DE LOS CLIENTES EN LA PLATAFORMA

# Calcular promedios por grupo
tabla_actividad <- aggregate(
  datos[, c(
    "Logins 0-1",
    "Blog Articles 0-1",
    "Views 0-1",
    "Days Since Last Login 0-1"
  )],
  by = list(Estado = datos$Estado),
  FUN = mean
)

# Redondear resultados
tabla_actividad[, -1] <- round(tabla_actividad[, -1], 2)

# Mostrar resultados
print(tabla_actividad)

# GRAFICA: CAMBIO EN LOS INICIOS DE SESION

grafica_logins <- ggplot(
  datos,
  aes(
    x = Estado,
    y = `Logins 0-1`,
    fill = Estado
  )
) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Cambio en los inicios de sesion segun abandono",
    x = "Estado del cliente",
    y = "Cambio en inicios de sesion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar grafica
ggsave(
  "Grafica_Logins.png",
  plot = grafica_logins,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Verificar archivo
file.exists("Grafica_Logins.png")
print(tabla_actividad)
# 1.9 REVISION DE VALORES EXTREMOS

# Minimos y maximos de todas las variables
resumen_extremos <- data.frame(
  Variable = names(datos)[sapply(datos, is.numeric)],
  Minimo = sapply(datos[sapply(datos, is.numeric)], min),
  Maximo = sapply(datos[sapply(datos, is.numeric)], max)
)

print(resumen_extremos)

# Seleccionar variables numericas sin ID ni Churn
variables <- datos[, !names(datos) %in% c(
  "ID",
  "Churn (1 = Yes, 0 = No)",
  "Estado"
)]

# Contar valores atipicos mediante IQR
tabla_atipicos <- data.frame(
  Variable = names(variables),
  Atipicos = sapply(variables, function(x) {
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- IQR(x)

    sum(x < q1 - 1.5 * iqr |
        x > q3 + 1.5 * iqr)
  })
)

print(tabla_atipicos)

# GUARDAR TABLA DE VALORES ATIPICOS COMO PNG

library(gridExtra)
library(grid)

# Preparar la tabla
tabla_atipicos_png <- tabla_atipicos

# Agregar porcentaje
tabla_atipicos_png$Porcentaje <- round(
  tabla_atipicos_png$Atipicos / nrow(datos) * 100, 2
)

# Quitar nombres repetidos de las filas
rownames(tabla_atipicos_png) <- NULL

# Guardar como imagen
png("Tabla_Atipicos.png", width = 1800, height = 900, res = 150)

grid.newpage()

grid.draw(
  tableGrob(
    tabla_atipicos_png,
    rows = NULL
  )
)

dev.off()

# Comprobar que se guardo
file.exists("Tabla_Atipicos.png")

# PUNTO 2. FACTORES ASOCIADOS AL ABANDONO

# 2.1. Preparacion de los datos

library(readxl)
library(ggplot2)
library(dplyr)

# Cargar la base de datos
datos <- read_excel("DATA.xlsx", sheet = "Case Data")

# Crear grupos segun el abandono
datos$Estado <- ifelse(
  datos$`Churn (1 = Yes, 0 = No)` == 1,
  "Abandonaron",
  "Permanecieron"
)

# Revisar cantidad de clientes por grupo
table(datos$Estado)

# Comprobar que la base se cargo correctamente
dim(datos)

# 2.2. RELACION ENTRE ANTIGUEDAD Y ABANDONO

# Crear grupos de antiguedad
datos <- datos %>%
  mutate(
    Grupo_Antiguedad = case_when(
      `Customer Age (in months)` < 6 ~ "Menos de 6 meses",
      `Customer Age (in months)` <= 14 ~ "6 a 14 meses",
      `Customer Age (in months)` > 14 ~ "Mas de 14 meses",
      TRUE ~ NA_character_
    )
  )

# Calcular abandono por grupo
tabla_antiguedad_churn <- datos %>%
  filter(!is.na(Grupo_Antiguedad),
         !is.na(`Churn (1 = Yes, 0 = No)`)) %>%
  group_by(Grupo_Antiguedad) %>%
  summarise(
    Total_Clientes = n(),
    Abandonaron = sum(`Churn (1 = Yes, 0 = No)` == 1),
    .groups = "drop"
  ) %>%
  mutate(
    Porcentaje_Abandono = round(
      Abandonaron / Total_Clientes * 100, 2
    )
  )

# Mostrar resultados
print(tabla_antiguedad_churn)

# 2.3. GRAFICA DE ABANDONO POR ANTIGUEDAD

# Organizar los grupos
tabla_antiguedad_churn$Grupo_Antiguedad <- factor(
  tabla_antiguedad_churn$Grupo_Antiguedad,
  levels = c(
    "Menos de 6 meses",
    "6 a 14 meses",
    "Mas de 14 meses"
  )
)

# Crear grafica
grafica_riesgo_antiguedad <- ggplot(
  tabla_antiguedad_churn,
  aes(x = Grupo_Antiguedad,
      y = Porcentaje_Abandono,
      fill = Grupo_Antiguedad)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(Porcentaje_Abandono, "%")),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Menos de 6 meses" = "#237A91",
      "6 a 14 meses" = "#E58A62",
      "Mas de 14 meses" = "#75A9B6"
    )
  ) +
  labs(
    title = "Porcentaje de abandono segun antiguedad",
    x = "Antiguedad del cliente",
    y = "Porcentaje de abandono (%)"
  ) +
  expand_limits(y = 9) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar grafica en PNG
ggsave(
  "Grafica_Riesgo_Antiguedad.png",
  plot = grafica_riesgo_antiguedad,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Mostrar grafica
print(grafica_riesgo_antiguedad)

# Comprobar que se guardo
file.exists("Grafica_Riesgo_Antiguedad.png")

# 2.4. RELACION ENTRE SATISFACCION (CHI) Y ABANDONO

tabla_chi_abandono <- datos %>%
  group_by(Estado) %>%
  summarise(
    Promedio_CHI_Mes0 = round(mean(`CHI Score Month 0`, na.rm = TRUE), 2),
    Mediana_CHI_Mes0 = round(median(`CHI Score Month 0`, na.rm = TRUE), 2),
    Promedio_Cambio_CHI = round(mean(`CHI Score 0-1`, na.rm = TRUE), 2),
    Mediana_Cambio_CHI = round(median(`CHI Score 0-1`, na.rm = TRUE), 2),
    Clientes = n(),
    .groups = "drop"
  )

print(tabla_chi_abandono)

# 2.5. GUARDAR TABLA DE SATISFACCION COMO PNG

library(gridExtra)
library(grid)

# Preparar tabla para imagen
tabla_chi_png <- as.data.frame(tabla_chi_abandono)

# Cambiar nombres para que se vean mejor
names(tabla_chi_png) <- c(
  "Estado",
  "CHI promedio",
  "Mediana CHI",
  "Cambio promedio",
  "Mediana cambio",
  "Clientes"
)

# Crear tabla visual
tabla_visual_chi <- tableGrob(
  tabla_chi_png,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    colhead = list(
      fg_params = list(fontface = "bold")
    )
  )
)

# Guardar tabla como PNG
png(
  "Tabla_CHI_Abandono.png",
  width = 1800,
  height = 500,
  res = 150
)

grid.newpage()
grid.draw(tabla_visual_chi)
dev.off()

# Verificar archivo
file.exists("Tabla_CHI_Abandono.png")

# 2.6. GRAFICA COMPARATIVA DEL CHI - CORREGIDA

library(ggplot2)

# Crear grafica
grafica_chi_promedio <- ggplot(
  tabla_chi_abandono,
  aes(x = Estado, y = Promedio_CHI_Mes0, fill = Estado)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(Promedio_CHI_Mes0, 2)),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Satisfaccion promedio de los clientes",
    x = "Estado del cliente",
    y = "CHI promedio"
  ) +
  coord_cartesian(ylim = c(0, 105)) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(25, 25, 25, 25)
  )

# Guardar con dispositivo de Windows
png(
  filename = "Grafica_CHI_Promedio.png",
  width = 2400,
  height = 1500,
  res = 300,
  type = "windows"
)

print(grafica_chi_promedio)
dev.off()

# Verificar
file.exists("Grafica_CHI_Promedio.png")

# 2.7. RELACION ENTRE SOPORTE TECNICO Y ABANDONO

# Comparar soporte tecnico segun el estado del cliente
tabla_soporte_churn <- datos %>%
  group_by(Estado) %>%
  summarise(
    Casos_Promedio = round(
      mean(`Support Cases Month 0`, na.rm = TRUE), 2
    ),
    Cambio_Casos = round(
      mean(`Support Cases 0-1`, na.rm = TRUE), 2
    ),
    Prioridad_Promedio = round(
      mean(`SP Month 0`, na.rm = TRUE), 2
    ),
    Cambio_Prioridad = round(
      mean(`SP 0-1`, na.rm = TRUE), 2
    ),
    Clientes = n(),
    .groups = "drop"
  )

# Mostrar resultados
print(tabla_soporte_churn)

# 2.8. GUARDAR TABLA DE SOPORTE COMO PNG

library(gridExtra)
library(grid)

# Preparar la tabla
tabla_soporte_png <- as.data.frame(tabla_soporte_churn)

# Cambiar nombres de columnas
names(tabla_soporte_png) <- c(
  "Estado",
  "Casos promedio",
  "Cambio casos",
  "Prioridad promedio",
  "Cambio prioridad",
  "Clientes"
)

# Crear tabla visual
tabla_visual_soporte <- tableGrob(
  tabla_soporte_png,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    colhead = list(
      fg_params = list(fontface = "bold")
    )
  )
)

# Guardar tabla como imagen
png(
  "Tabla_Soporte_Churn.png",
  width = 2000,
  height = 550,
  res = 150,
  type = "windows"
)

grid.newpage()
grid.draw(tabla_visual_soporte)
dev.off()

# Verificar archivo
file.exists("Tabla_Soporte_Churn.png")

# 2.9. GRAFICA COMPARATIVA DE SOPORTE TECNICO

# Crear grafica
grafica_soporte_promedio <- ggplot(
  tabla_soporte_churn,
  aes(x = Estado, y = Casos_Promedio, fill = Estado)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = Casos_Promedio),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Solicitudes de soporte tecnico por cliente",
    subtitle = "Comparacion segun el abandono",
    x = "Estado del cliente",
    y = "Promedio de solicitudes de soporte"
  ) +
  coord_cartesian(ylim = c(0, 0.9)) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(25, 25, 25, 25)
  )

# Guardar grafica como PNG
png(
  filename = "Grafica_Soporte_Promedio.png",
  width = 2400,
  height = 1500,
  res = 300,
  type = "windows"
)

print(grafica_soporte_promedio)
dev.off()

# Verificar que se guardo
file.exists("Grafica_Soporte_Promedio.png")

# 2.10. RELACION ENTRE ACTIVIDAD Y ABANDONO

# Comparar actividad de los clientes
tabla_actividad_churn <- datos %>%
  group_by(Estado) %>%
  summarise(
    Cambio_Logins = round(
      mean(`Logins 0-1`, na.rm = TRUE), 2
    ),
    Cambio_Blogs = round(
      mean(`Blog Articles 0-1`, na.rm = TRUE), 2
    ),
    Cambio_Views = round(
      mean(`Views 0-1`, na.rm = TRUE), 2
    ),
    Cambio_Dias_Login = round(
      mean(`Days Since Last Login 0-1`, na.rm = TRUE), 2
    ),
    Clientes = n(),
    .groups = "drop"
  )

# Mostrar resultados
print(tabla_actividad_churn)

# GUARDAR TABLA DE ACTIVIDAD COMO PNG

library(gridExtra)
library(grid)

# Preparar tabla
tabla_actividad_png <- as.data.frame(tabla_actividad_churn)

# Cambiar nombres de columnas
names(tabla_actividad_png) <- c(
  "Estado",
  "Cambio logins",
  "Cambio blogs",
  "Cambio views",
  "Cambio dias login",
  "Clientes"
)

# Crear tabla visual
tabla_visual_actividad <- tableGrob(
  tabla_actividad_png,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    colhead = list(
      fg_params = list(fontface = "bold")
    )
  )
)

# Guardar como PNG
png(
  "Tabla_Actividad_Churn.png",
  width = 2000,
  height = 550,
  res = 150,
  type = "windows"
)

grid.newpage()
grid.draw(tabla_visual_actividad)
dev.off()

# Verificar archivo
file.exists("Tabla_Actividad_Churn.png")

# 2.11. GRAFICA COMPARATIVA DE ACTIVIDAD

library(ggplot2)

# Crear grafica de inicios de sesion
grafica_actividad_churn <- ggplot(
  tabla_actividad_churn,
  aes(x = Estado, y = Cambio_Logins, fill = Estado)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = Cambio_Logins),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Abandonaron" = "#E58A62",
      "Permanecieron" = "#237A91"
    )
  ) +
  labs(
    title = "Cambio promedio en los inicios de sesion",
    subtitle = "Comparacion segun el abandono de clientes",
    x = "Estado del cliente",
    y = "Cambio promedio de logins"
  ) +
  coord_cartesian(ylim = c(0, 19)) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(25, 25, 25, 25)
  )

# Guardar grafica en PNG
png(
  filename = "Grafica_Actividad_Churn.png",
  width = 2400,
  height = 1500,
  res = 300,
  type = "windows"
)

print(grafica_actividad_churn)
dev.off()

# Verificar archivo
file.exists("Grafica_Actividad_Churn.png")

# PUNTO 3. MODELO DE REGRESION LOGISTICA

# 3.1. PREPARACION DE VARIABLES

library(dplyr)

# Seleccionar variables para el modelo
datos_modelo <- datos %>%
  select(
    ID,
    `Churn (1 = Yes, 0 = No)`,
    `Customer Age (in months)`,
    `CHI Score Month 0`,
    `CHI Score 0-1`,
    `Support Cases Month 0`,
    `Support Cases 0-1`,
    `SP Month 0`,
    `SP 0-1`,
    `Logins 0-1`,
    `Blog Articles 0-1`,
    `Views 0-1`,
    `Days Since Last Login 0-1`
  ) %>%
  rename(
    Churn = `Churn (1 = Yes, 0 = No)`,
    Antiguedad = `Customer Age (in months)`,
    CHI = `CHI Score Month 0`,
    Cambio_CHI = `CHI Score 0-1`,
    Casos_Soporte = `Support Cases Month 0`,
    Cambio_Soporte = `Support Cases 0-1`,
    Prioridad = `SP Month 0`,
    Cambio_Prioridad = `SP 0-1`,
    Logins = `Logins 0-1`,
    Blogs = `Blog Articles 0-1`,
    Views = `Views 0-1`,
    Dias_Login = `Days Since Last Login 0-1`
  )

# Verificar dimensiones
dim(datos_modelo)

# Revisar estructura de variables
str(datos_modelo)

# Revisar valores faltantes
colSums(is.na(datos_modelo))

# Comprobar la variable dependiente
table(datos_modelo$Churn, useNA = "ifany")

# 3.2. MODELO DE REGRESION LOGISTICA

# Construir modelo con las variables del caso
modelo_logistico <- glm(
  Churn ~ Antiguedad + CHI + Cambio_CHI +
    Casos_Soporte + Cambio_Soporte +
    Prioridad + Cambio_Prioridad +
    Logins + Blogs + Views + Dias_Login,
  data = datos_modelo,
  family = binomial(link = "logit")
)

# Mostrar resultados del modelo
summary(modelo_logistico)

# Verificar cantidad de observaciones utilizadas
nobs(modelo_logistico)

# 3.3. ODDS RATIOS DEL MODELO LOGISTICO

# Extraer coeficientes del modelo
resultados_modelo <- summary(modelo_logistico)$coefficients

# Crear tabla de resultados
tabla_odds <- data.frame(
  Variable = rownames(resultados_modelo),
  Coeficiente = round(resultados_modelo[, 1], 5),
  Odds_Ratio = round(exp(resultados_modelo[, 1]), 5),
  Valor_p = signif(resultados_modelo[, 4], 3),
  row.names = NULL
)

# Identificar variables significativas
tabla_odds$Significativa <- ifelse(
  tabla_odds$Valor_p < 0.05,
  "Si",
  "No"
)

# Mostrar resultados
print(tabla_odds)

# 3.4. GUARDAR TABLA DE ODDS RATIOS COMO PNG

library(gridExtra)
library(grid)

# Preparar tabla para imagen
tabla_odds_png <- tabla_odds

# Cambiar nombres para que se vean mejor
names(tabla_odds_png) <- c(
  "Variable",
  "Coeficiente",
  "Odds ratio",
  "Valor p",
  "Significativa"
)


# Mostrar valores p con maximo 3 decimales
tabla_odds_png$`Valor p` <- ifelse(
  tabla_odds$Valor_p < 0.001,
  "<0.001",
  sprintf("%.3f", tabla_odds$Valor_p)
)
)

# Crear tabla visual
tabla_visual_odds <- tableGrob(
  tabla_odds_png,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 11,
    colhead = list(
      fg_params = list(fontface = "bold")
    )
  )
)

# Guardar tabla como PNG
png(
  "Tabla_Odds_Ratios.png",
  width = 2200,
  height = 700,
  res = 150,
  type = "windows"
)

grid.newpage()
grid.draw(tabla_visual_odds)
dev.off()

# Verificar archivo
file.exists("Tabla_Odds_Ratios.png")

# GUARDAR MODELO DE REGRESION LOGISTICA

# Guardar modelo para su uso posterior
saveRDS(
  modelo_logistico,
  file = "Modelo_Logistico_Churn.rds"
)

# Guardar datos utilizados en el modelo
saveRDS(
  datos_modelo,
  file = "Datos_Modelo_Churn.rds"
)

# Verificar que los archivos se guardaron
file.exists("Modelo_Logistico_Churn.rds")
file.exists("Datos_Modelo_Churn.rds")

# Verificar numero de observaciones del modelo
nobs(modelo_logistico)

# CONCLUSIONES DE LOS PUNTOS 2 Y 3

# Al analizar los datos encontramos que los clientes
# que llevaban entre 6 y 14 meses en la empresa
# fueron los que mas abandonaron, con un 7.22%.
# En cambio, los de menos de 6 meses tuvieron
# el porcentaje mas bajo, con un 1.56%.

# Tambien vimos que la satisfaccion tiene una relacion
# importante con el abandono. Los clientes que se fueron
# tenian un CHI promedio de 63.27, mientras que los
# que se quedaron tenian 88.61. Ademas, la satisfaccion
# de los que abandonaron habia disminuido.

# En el soporte tecnico encontramos algo diferente
# a lo que esperaba el gerente. Los clientes que se
# quedaron hicieron mas solicitudes de soporte que
# los que abandonaron. Sin embargo, en la regresion
# estas variables no fueron significativas.

# Con la actividad de los clientes vimos que los que
# permanecieron aumentaron mas sus inicios de sesion.
# Tambien encontramos que las visualizaciones y los
# dias desde el ultimo ingreso fueron significativos
# en el modelo, pero los logins y blogs no.

# Finalmente, la regresion logistica mostro que
# la antiguedad, el CHI, el cambio del CHI, las
# visualizaciones y los dias desde el ultimo login
# tienen una relacion significativa con el abandono.

# En general, encontramos que la satisfaccion y
# la actividad de los clientes pueden ayudar a
# identificar quienes tienen mas riesgo de irse.
# Sin embargo, estos resultados no significan que
# esas variables sean directamente la causa.

# Con este modelo, el siguiente paso sera evaluar
# las predicciones e identificar los 100 clientes
# con mayor riesgo de abandonar la empresa.

# PUNTO 4. EVALUACION PREDICTIVA DEL MODELO LOGISTICO

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

if (!exists("modelo_logistico")) {
  modelo_logistico <- readRDS("Modelo_Logistico_Churn.rds")
}
if (!exists("datos_modelo")) {
  datos_modelo <- readRDS("Datos_Modelo_Churn.rds")
}

# Calcular probabilidades predichas de abandono para cada cliente
datos_modelo$Prob_Churn <- predict(
  modelo_logistico,
  newdata = datos_modelo,
  type = "response"
)


# 4.1. MATRIZ DE CONFUSION Y METRICAS (UMBRAL ESTANDAR 0.50 vs UMBRAL OPTIMO)
# Evaluacion con umbral estandar (0.50)
pred_05 <- ifelse(datos_modelo$Prob_Churn >= 0.50, 1, 0)
matriz_05 <- table(
  Real = factor(datos_modelo$Churn, levels = c(0, 1)),
  Predicho = factor(pred_05, levels = c(0, 1))
)
print("Matriz de Confusion - Umbral 0.50:")
print(matriz_05)

# Calculo exacto de Curva ROC, AUC e Indice de Youden (en R base, cero errores)
umbrales <- sort(unique(datos_modelo$Prob_Churn), decreasing = TRUE)
umbrales <- c(1, umbrales, 0)

roc_datos <- do.call(rbind, lapply(umbrales, function(u) {
  pred <- ifelse(datos_modelo$Prob_Churn >= u, 1, 0)
  vp <- sum(datos_modelo$Churn == 1 & pred == 1)
  vn <- sum(datos_modelo$Churn == 0 & pred == 0)
  fp <- sum(datos_modelo$Churn == 0 & pred == 1)
  fn <- sum(datos_modelo$Churn == 1 & pred == 0)
  
  tpr <- vp / (vp + fn) # Sensibilidad
  fpr <- fp / (fp + vn) # 1 - Especificidad
  esp <- vn / (vn + fp) # Especificidad
  youden <- tpr + esp - 1
  data.frame(Umbral = u, TPR = tpr, FPR = fpr, Especificidad = esp, Youden = youden)
}))

# Calcular AUC mediante integracion trapezoidal
roc_ordenado <- roc_datos[order(roc_datos$FPR, roc_datos$TPR), ]
auc_valor <- round(
  sum(diff(roc_ordenado$FPR) * (head(roc_ordenado$TPR, -1) + tail(roc_ordenado$TPR, -1)) / 2),
  4
)

# Seleccionar umbral optimo (Indice de Youden maximo)
fila_optima <- roc_datos[which.max(roc_datos$Youden), ][1, ]
umbral_optimo <- fila_optima$Umbral

# Predicciones con umbral optimo
datos_modelo$Pred_Optima <- ifelse(datos_modelo$Prob_Churn >= umbral_optimo, 1, 0)

# Componentes de la matriz de confusion optima
VP <- sum(datos_modelo$Churn == 1 & datos_modelo$Pred_Optima == 1)
VN <- sum(datos_modelo$Churn == 0 & datos_modelo$Pred_Optima == 0)
FP <- sum(datos_modelo$Churn == 0 & datos_modelo$Pred_Optima == 1)
FN <- sum(datos_modelo$Churn == 1 & datos_modelo$Pred_Optima == 0)

# Metricas de evaluacion
exactitud <- round((VP + VN) / nrow(datos_modelo) * 100, 2)
sensibilidad <- round(VP / (VP + FN) * 100, 2)
especificidad <- round(VN / (VN + FP) * 100, 2)
precision <- round(VP / (VP + FP) * 100, 2)

# Tabla resumen de metricas
tabla_metricas <- data.frame(
  Metrica = c(
    "AUC (Area bajo la curva ROC)",
    "Umbral optimo de clasificacion (Youden)",
    "Exactitud (Accuracy)",
    "Sensibilidad (Recall / Deteccion de Churn)",
    "Especificidad (Deteccion de Retencion)",
    "Precision (Valor Predictivo Positivo)"
  ),
  Valor = c(
    as.character(auc_valor),
    paste0(round(umbral_optimo * 100, 2), "%"),
    paste0(exactitud, "%"),
    paste0(sensibilidad, "%"),
    paste0(especificidad, "%"),
    paste0(precision, "%")
  )
)

print(tabla_metricas)

# Guardar Tabla de Metricas como PNG 
tabla_visual_metricas <- tableGrob(
  tabla_metricas,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    colhead = list(fg_params = list(fontface = "bold"))
  )
)

png(
  filename = "Tabla_Metricas_Modelo.png",
  width = 2000,
  height = 750,
  res = 150
)
grid.newpage()
grid.draw(tabla_visual_metricas)
dev.off()

# 4.2. GRAFICA DE MATRIZ DE CONFUSION (UMBRAL OPTIMO)
df_matriz <- data.frame(
  Real = factor(
    c("Permanecio (0)", "Permanecio (0)", "Abandono (1)", "Abandono (1)"),
    levels = c("Abandono (1)", "Permanecio (0)")
  ),
  Predicho = factor(
    c("Permanecio (0)", "Abandono (1)", "Permanecio (0)", "Abandono (1)"),
    levels = c("Permanecio (0)", "Abandono (1)")
  ),
  Cantidad = c(VN, FP, FN, VP),
  Etiqueta = c(
    paste0("VN: ", VN, "\n(", round(VN / nrow(datos_modelo) * 100, 1), "%)"),
    paste0("FP: ", FP, "\n(", round(FP / nrow(datos_modelo) * 100, 1), "%)"),
    paste0("FN: ", FN, "\n(", round(FN / nrow(datos_modelo) * 100, 1), "%)"),
    paste0("VP: ", VP, "\n(", round(VP / nrow(datos_modelo) * 100, 1), "%)")
  )
)

grafica_matriz <- ggplot(df_matriz, aes(x = Predicho, y = Real, fill = Cantidad)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Etiqueta), size = 5, fontface = "bold", color = "black") +
  scale_fill_gradient(low = "#EBF3F5", high = "#237A91") +
  labs(
    title = "Matriz de confusion del modelo logistico",
    subtitle = paste0("Evaluada con umbral optimo de Youden (", round(umbral_optimo * 100, 2), "%)"),
    x = "Prediccion del modelo",
    y = "Estado real del cliente"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(25, 25, 25, 25)
  )
ggsave(
  filename = "Grafica_Matriz_Confusion.png",
  plot = grafica_matriz,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 4.3. GRAFICA DE CURVA ROC

grafica_roc <- ggplot(roc_ordenado, aes(x = FPR, y = TPR)) +
  geom_line(color = "#237A91", linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#E58A62", linewidth = 1) +
  annotate(
    "text",
    x = 0.65,
    y = 0.25,
    label = paste0("AUC = ", auc_valor),
    size = 5.5,
    fontface = "bold",
    color = "#237A91"
  ) +
  labs(
    title = "Curva ROC del modelo de prediccion de abandono",
    subtitle = "Capacidad discriminante del modelo logistico",
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(25, 25, 25, 25)
  )

ggsave(
  filename = "Grafica_Curva_ROC.png",
  plot = grafica_roc,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# PUNTO 5. RANKING DE LOS 100 CLIENTES DE MAYOR RIESGO Y FACTORES PRINCIPALES

# 5.1. IDENTIFICAR LOS 3 FACTORES QUE MAS CONTRIBUYEN AL RIESGO DE CADA CLIENTE
coeficientes <- coef(modelo_logistico)[-1]
variables_x <- names(coeficientes)
medias_x <- colMeans(datos_modelo[, variables_x], na.rm = TRUE)

# Contribucion marginal al log-odds respecto al cliente promedio: beta_j * (X_ij - media_j)
contribuciones <- as.data.frame(
  sapply(variables_x, function(var) {
    coeficientes[var] * (datos_modelo[[var]] - medias_x[var])
  })
)

nombres_legibles <- c(
  Antiguedad = "Antiguedad",
  CHI = "Bajo CHI",
  Cambio_CHI = "Caida en CHI",
  Casos_Soporte = "Casos soporte",
  Cambio_Soporte = "Aumento soporte",
  Prioridad = "Prioridad alta",
  Cambio_Prioridad = "Cambio prioridad",
  Logins = "Cambio logins",
  Blogs = "Cambio blogs",
  Views = "Caida en views",
  Dias_Login = "Dias sin login"
)

top3_factores <- t(apply(contribuciones, 1, function(fila) {
  ordenados <- sort(fila, decreasing = TRUE)
  nombres_legibles[names(ordenados)[1:3]]
}))

datos_modelo$Factor_1 <- top3_factores[, 1]
datos_modelo$Factor_2 <- top3_factores[, 2]
datos_modelo$Factor_3 <- top3_factores[, 3]


# 5.2. RANKING TOP 100 CLIENTES CON MAYOR PROBABILIDAD DE ABANDONO
ranking_100 <- datos_modelo %>%
  arrange(desc(Prob_Churn)) %>%
  slice(1:100) %>%
  mutate(
    Ranking = row_number(),
    Probabilidad_Pct = round(Prob_Churn * 100, 2)
  ) %>%
  select(
    Ranking,
    ID,
    Probabilidad_Pct,
    Factor_1,
    Factor_2,
    Factor_3,
    Antiguedad,
    CHI,
    Cambio_CHI,
    Views,
    Dias_Login,
    Churn
  )

# Exportar lista completa de los 100 clientes a CSV
write.csv(ranking_100, "Ranking_Top100_Clientes_Riesgo.csv", row.names = FALSE)

# Mostrar los primeros 15 clientes en consola
print(head(ranking_100, 15))

# 5.3. GUARDAR TABLA DEL TOP 15 CLIENTES EN MAYOR RIESGO COMO PNG
tabla_top15_png <- ranking_100 %>%
  slice(1:15) %>%
  select(Ranking, ID, Probabilidad_Pct, Factor_1, Factor_2, Factor_3, CHI, Cambio_CHI, Dias_Login)

names(tabla_top15_png) <- c(
  "Rank",
  "ID Cliente",
  "Prob. Churn (%)",
  "Factor 1",
  "Factor 2",
  "Factor 3",
  "CHI",
  "Cambio CHI",
  "Dias sin Login"
)

tabla_visual_top15 <- tableGrob(
  tabla_top15_png,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    colhead = list(fg_params = list(fontface = "bold"))
  )
)

png(
  filename = "Tabla_Top15_Clientes_Riesgo.png",
  width = 2400,
  height = 1100,
  res = 150
)
grid.newpage()
grid.draw(tabla_visual_top15)
dev.off()

# 5.4. GRAFICA DE PRINCIPALES FACTORES DE RIESGO EN EL TOP 100
todos_factores_top100 <- c(ranking_100$Factor_1, ranking_100$Factor_2, ranking_100$Factor_3)

df_factores_top100 <- as.data.frame(table(Factor = todos_factores_top100)) %>%
  arrange(desc(Freq)) %>%
  slice(1:6)

grafica_factores_top100 <- ggplot(
  df_factores_top100,
  aes(x = reorder(Factor, Freq), y = Freq)
) +
  geom_col(width = 0.65, fill = "#237A91") +
  geom_col(data = subset(df_factores_top100, Freq >= 50), width = 0.65, fill = "#E58A62") +
  geom_text(
    aes(label = paste0(Freq, " clientes")),
    hjust = -0.15,
    size = 4.2,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 115)) +
  labs(
    title = "Principales impulsores de riesgo en el Top 100",
    subtitle = "Frecuencia de aparicion entre los 3 factores criticos de cada cliente",
    x = "Factor de riesgo",
    y = "Numero de clientes en el Top 100"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(25, 25, 25, 25)
  )

ggsave(
  filename = "Grafica_Factores_Top100.png",
  plot = grafica_factores_top100,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)
