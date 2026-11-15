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
