    # Data --------------------------------------------------------------------
# Librería
library(tidyverse)

library(ggplot2)

library(modelsummary)

library(pandoc)

library(stargazer)

library(devtools)

library(usethis)

library(sjPlot)
datos <- read_excel("C:/Users/USER/Documents/REMESAS_PIB/data/modificacion/CIFRAS_PROJECT.xlsx")

data <- na.omit(datos)

# Regresión en dos etapas -------------------------------------------------
# Etapa 1: Ajustar el modelo de regresión para consumo
modelo_etapa1 <- lm(CONSUMO ~ REMESAS, data = datos)

# Extraer los valores ajustados (consumo estimado a partir de remesas)
datos$consumo_ajustado <- modelo_etapa1$fitted.values

# Etapa 2: Ajustar el modelo de regresión para el PIB usando el consumo ajustado
modelo_etapa2 <- lm(PIB ~ consumo_ajustado, data = datos)

modelo_indice_pib <- lm(PIB ~ consumo_ajustado + GASTO_PUBLICO + INVERSION + EXPORTACIONES + IMPORTACIONES, data = datos)


# Ver los resultados del modelo final
summary(modelo_etapa2)

summary(modelo_indice_pib)

# Visualización de la relación
ggplot(datos, aes(x = consumo_ajustado, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Remesas", y = "Consumo")


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
library(sjPlot)

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



