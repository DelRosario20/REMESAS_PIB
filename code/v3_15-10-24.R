# Data --------------------------------------------------------------------
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


# Gráfico de la Etapa 1: Remesas vs Consumo
plot(datos$REMESAS, datos$CONSUMO, main = "Remesas vs Consumo")
abline(modelo_etapa1, col = "red")

# Gráfico de la Etapa 2: Consumo ajustado vs PIB
plot(datos$consumo_ajustado, datos$PIB, main = "Consumo Ajustado vs PIB")
abline(modelo_etapa2, col = "blue")

# EXPORTAR DATOS ----------------------------------------------------------
# Exportar los resultados en formato Word
# Opción 1
# Exportar modelo a LaTeX, HTML o texto
stargazer(modelo_etapa1, modelo_etapa2, 
          type = "text",  # Cambiar a "html" o "latex" si es necesario
          title = "Resultados de las Regresiones en Dos Etapas",
          align = TRUE, 
          dep.var.labels = c("Consumo Ajustado", "PIB"),
          covariate.labels = c("Remesas", "Consumo Ajustado"),
          out = "resultados_regresion1.doc")  # Cambiar extensión según formato


# Opción 2
# Crear tabla para los modelos y exportarla a Word
tab_model(modelo_etapa1, modelo_etapa2, 
          title = "Resultados de las Regresiones",
          dv.labels = c("Consumo Ajustado", "PIB"),
          show.ci = FALSE, 
          show.se = TRUE,  # Muestra errores estándar
          file = "resultados_regresion2.doc")  # Exporta a Word


# Opción 2
modelsummary(list("Etapa 1: Consumo Ajustado" = modelo_etapa1, 
                  "Etapa 2: PIB" = modelo_etapa2),
             output = "resultados_regresion.docx")


# GRAFICAS POR CASOS ------------------------------------------------------
# MATRIZ DE CORRELACION ---------------------------------------------------
# Creación de base de variables para matriz de correlacion
Bs_correlacion<-data.frame(PIB=data$PIB,
                           CONSUMO=data$CONSUMO,
                           REMESAS=data$REMESAS,
                           INVERSION=data$INVERSION,
                           EXPORTACIONES=data$EXPORTACIONES,
                           IMPORTACIONES=data$IMPORTACIONES,
                           GASTO_PUBLICO=data$GASTO_PUBLICO)
Matriz_Correl<-cor(Bs_correlacion)
cor_melt <- melt(Matriz_Correl)
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Matriz de Correlación", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(Matriz_Correl)
#Grafico de matriz de correlacion
corrplot(Matriz_Correl)
plot(Matriz_Correl)
corrplot.mixed(Matriz_Correl
               


# LINEA DE TIEMPO ---------------------------------------------------------
# Linea de tiempo para el gasto público
ggplot(data = datos, aes(x = TIEMPO, y = GASTO_PUBLICO)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución del Gasto Público", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()

# Linea de tiempo para las REMESAS
ggplot(data = datos, aes(x = TIEMPO, y = REMESAS)) +
  geom_line(color = "blue", size = 1.2) +    # Línea azul
  geom_point(color = "red", size = 3) +      # Puntos rojos
  labs(title = "Evolución de las remesas", 
       x = "Año", 
       y = "Gasto Público (en millones)") +
  theme_minimal() +                          # Tema visual minimalista
  theme()


