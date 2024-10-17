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
}
datos <- read_excel("C:/Users/USER/Documents/REMESAS_PIB/data/amplia/CIFRAS_PROJECT_AMPLIA.xlsx")

data <- na.omit(datos)

# Regresión en dos etapas -------------------------------------------------
# Etapa 1: Ajustar el modelo de regresión para consumo
modelo_etapa1 <- lm(CONSUMO ~ REMESAS, data = datos)

# Extraer los valores ajustados (consumo estimado a partir de remesas)
datos$consumo_ajustado <- modelo_etapa1$fitted.values

# Etapa 2: Ajustar el modelo de regresión para el PIB usando el consumo ajustado
modelo_etapa2 <- lm(PIB ~ consumo_ajustado, data = datos)

EXPORTA_NETAS <- datos$EXPORTACIONES-datos$IMPORTACIONES

modelo_indice_pib <- lm(PIB ~ consumo_ajustado + GASTO_PUBLICO + INVERSION + EXPORTA_NETAS, data = datos)


# Ver los resultados del modelo final
summary(modelo_etapa1)

summary(modelo_etapa2)

summary(modelo_indice_pib)

# Visualización de la relación
# Visualización de la relación entre REMESAS y CONSUMO
ggplot(datos, aes(x = CONSUMO, y = REMESAS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Remesas", y = "Consumo")

# Visualización de la relación entre CONSUMO_AJUSTADO y PIB
ggplot(datos, aes(x = consumo_ajustado, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las CONSUMO_AJUSTADO en el PIB", x = "PIB", y = "CONSUMO_AJUSTADO")


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
# Creación de base de variables para matriz de correlacion
Bs_correlacion<-data.frame(PIB=datos$PIB,
                           CONSUMO=datos$CONSUMO,
                           REMESAS=datos$REMESAS,
                           INVERSION=datos$INVERSION,
                           EXPORTACIONES=datos$EXPORTACIONES,
                           IMPORTACIONES=datos$IMPORTACIONES,
                           GASTO_PUBLICO=datos$GASTO_PUBLICO)

Matriz_Correl<-cor(Bs_correlacion)

cor_melt <- melt(Matriz_Correl)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Matriz de Correlación", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafico de matriz de correlacion
corrplot(Matriz_Correl)

# LINEA DE TIEMPO ---------------------------------------------------------
{
  # Linea de tiempo para el gasto público
ggplot(data = datos, aes(x = TIEMPO, y = GASTO_PUBLICO)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución del Gasto Público", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()
}# Gráfico para el gasto público
{
# Linea de tiempo para las REMESAS
ggplot(data = datos, aes(x = TIEMPO, y = REMESAS)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución de las remesas", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()
}# Gráfico para las remesas

