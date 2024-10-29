# Data --------------------------------------------------------------------
{
# Librería
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)
library(tidyverse)
library(ggplot2)
library(fdth)
library(knitr)
library(dplyr)
library(fdth)
library(e1071)
library(haven)
library(corrplot)
library(reshape2)
library(modelsummary)
library(pandoc)
library(stargazer)
library(devtools)
library(usethis)
library(sjPlot)
library(zoo)
library(car)
library(lmtest)
}
{
datos <- read_excel("C:/Users/USER/Documents/REMESAS_PIB/data/amplia/CIFRAS_PROJECT_AMPLIADO.xlsx")
# Limpieza de datos
datos$TIEMPO <- as.Date(paste0(datos$TIEMPO, "-01-01"))
  
# Tratamiento para variable de exportaciones netas
datos$EXPORTA_NETAS <- datos$EXPORTACIONES-datos$IMPORTACIONES

min_exporta_neta <- min(datos$EXPORTA_NETAS)

shift_value <- abs(min_exporta_neta) + 1  # Añadir 1 para evitar log(0)

# Tratamiento para variable de inflación
min_inflacion <- min(datos$INFLACION)
shift_valueinf <- abs(min_inflacion) + 1  # Añadir 1 para evitar log(0)

# Transformar las variables de consumo y remesas a logaritmo
datos$log_CONSUMO <- log(datos$CONSUMO)
datos$log_REMESAS <- log(datos$REMESAS)
datos$log_PIB <- log(datos$PIB)
datos$log_GASTO_PUBLICO <- log(datos$GASTO_PUBLICO)
datos$log_INVERSION <- log(datos$INVERSION)
datos$log_EXPORTA_NETAS <- log(datos$EXPORTA_NETAS + shift_value)
datos$log_INFLACION <- log(datos$INFLACION+ shift_valueinf)
datos$log_INGRESO_PER_CAPITA <- log(datos$INGRESO_PER_CAPITA)
} # Transformación de valores

# Regresión en dos etapas -------------------------------------------------
# Etapa 1: Ajustar el modelo de regresión para consumo
modelo_etapa1 <- lm(log_CONSUMO ~ log_REMESAS + log_INFLACION + log_INGRESO_PER_CAPITA , data = datos)

# Extraer los valores ajustados (consumo estimado a partir de remesas)
datos$consumo_ajustado <- exp(modelo_etapa1$fitted.values)

#Independiente entre remesas y PIB

modelo_remesas_pib <- lm(REMESAS~PIB, data = datos)

# Etapa 2: Ajustar el modelo de regresión para el PIB usando el consumo ajustado

modelo_indice_pib <- lm(log_PIB ~ log(consumo_ajustado) + log_GASTO_PUBLICO + log_INVERSION + log_EXPORTA_NETAS, data = datos)

# Ver los resultados del modelo final
summary(modelo_etapa1)

summary(modelo_remesas_pib)

summary(modelo_indice_pib)

# Visualización de la relación
# Visualización de la relación entre REMESAS y CONSUMO
ggplot(datos, aes(x = CONSUMO, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Consumo", y = "PIB")

# Visualización de la relación entre CONSUMO_AJUSTADO y PIB
ggplot(datos, aes(x = consumo_ajustado, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las Consumo ajustado por remesas en el PIB", x = "Consumo ajustado", y = "PIB")
  
# PRUEBAS DE ROBUSTEZ ----------------------------------------------------------
# Prueba de colinealidad
{
# Pruea de colinealidad (Prueba VIF) para el modelo de la etapa 1
vif(modelo_etapa1)

# Pruea de colinealidad (Prueba VIF) para el modelo de la etapa 2
vif(modelo_indice_pib)
}

# Prueba de heterocedasticidad 
{
# Para el modelo de la etapa 1
bptest(modelo_etapa1)

# Prueba el omdelo de la etapa 2
bptest(modelo_indice_pib)
}

# Prueba de autocorrelación
{
# Para el modelo 1
dwtest(modelo_etapa1)

# Para el modelo 2
dwtest(modelo_indice_pib)
}

# EXPORTAR DATOS ----------------------------------------------------------
# Exportar los resultados en formato Word
{
# Exportar modelo a LaTeX, HTML o texto
stargazer(modelo_etapa1, modelo_indice_pib, 
          type = "text",
          title = "Resultados de las Regresiones en Dos Etapas",
          align = TRUE, 
          dep.var.labels = c("Consumo Ajustado", "PIB"),
          covariate.labels = c("Remesas", "Consumo Ajustado"),
          out = "resultados_regresion1.html")  # Cambiar extensión según formato
} # Opción 1
{
# Crear tabla para los modelos y exportarla a Word
tab_model(modelo_etapa1, modelo_indice_pib, 
          title = "Resultados de las Regresiones",
          dv.labels = c("Consumo", "PIB"),
          show.ci = FALSE, 
          show.se = TRUE,  # Muestra errores estándar
          file = "resultados_regresion2.doc")  # Exporta a Word
} # Opción 2


# MATRIZ DE CORRELACION ---------------------------------------------------
{
# Creación de base de variables para matriz de correlacion
Bs_correlacion<-data.frame(PIB=datos$PIB,
                           CONSUMO=datos$CONSUMO,
                           REMESAS=datos$REMESAS,
                           INVERSION=datos$INVERSION,
                           EXPORTACIONES=datos$EXPORTACIONES,
                           IMPORTACIONES=datos$IMPORTACIONES,
                           GASTO_PUBLICO=datos$GASTO_PUBLICO,
                           INFLACION=datos$INFLACION,
                           INGRESO_PER_CAPITAL=datos$INGRESO_PER_CAPITA)

Matriz_Correl<-cor(Bs_correlacion)

cor_melt <- melt(Matriz_Correl)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Matriz de Correlación", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Grafico de matriz de correlacion
corrplot(Matriz_Correl)
}


# LINEA DE TIEMPO ---------------------------------------------------------
{
ggplot(data = datos, aes(x = TIEMPO, y = log_CONSUMO)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución del Consumo", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()
}# Gráfico para el Connsumo

{
ggplot(data = datos, aes(x = TIEMPO, y = log_PIB)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución del PIB", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()
}# Gráfico para el pib

{
# Linea de tiempo para las REMESAS
ggplot(data = datos, aes(x = TIEMPO, y = log_REMESAS)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución de las remesas", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()
}# Gráfico para las remesas

# Línea de evolución
{
ggplot(datos, aes(x = TIEMPO)) +
  geom_line(aes(y = log_REMESAS, color = "REMESAS"), size = 1.2) +
  geom_line(aes(y = log_PIB, color = "PIB"), size = 1.2) +
  geom_line(aes(y = log_CONSUMO, color = "CONSUMO"), size = 1.2) +
  labs(title = "Evolución de REMESAS, PIB y CONSUMO a lo largo del tiempo",
       x = "Año",
       y = "Valor") +
  scale_color_manual(values = c("REMESAS" = "blue", "PIB" = "yellow", "CONSUMO" = "red"),
                     name = "Variable") +
  theme_minimal()
}