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
