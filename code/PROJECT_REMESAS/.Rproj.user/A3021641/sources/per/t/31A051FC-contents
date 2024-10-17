# Librería
library(tidyverse)

library(ggplot2)

datos <- read_excel("C:/Users/USER/Documents/REMESAS_PIB/data/modificacion/CIFRAS_PROJECT.xlsx")

data <- na.omit(datos)

# REMESAS Y CONSUMO -------------------------------------------------------
# Crear el modelo de regresión lineal
modelo_consumo_remesas <- lm(CONSUMO ~ REMESAS, data = datos)

# Resumen del modelo
summary(modelo_consumo_remesas)

# Visualización de la relación
ggplot(datos, aes(x = REMESAS, y = CONSUMO)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Remesas", y = "Consumo")



# CONSUMO Y PIB -----------------------------------------------------------
# Crear índice de consumo según remesas
datos$indice_consumo_remesas <- datos$CONSUMO / datos$REMESAS

datos$indice_completo_consumo <-  datos$indice_consumo_remesas + datos$CONSUMO

# Crear el modelo de regresión lineal donde el índice afecta al PIB
modelo_indice_pib <- lm(PIB ~ indice_completo_consumo + GASTO_PUBLICO + INVERSION + EXPORTACIONES + IMPORTACIONES, data = datos)
modelo2 <- lm(PIB ~ indice_consumo_remesas, data = datos)
# Resumen del modelo para ver los coeficientes
summary(modelo_indice_pib)

# Visualización de la relación
ggplot(datos, aes(x = indice_completo_consumo, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Remesas", y = "Consumo")
  
# Visualización de la relación
ggplot(datos, aes(x = indice_consumo_remesas, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Impacto de las REMESAS en el CONSUMO", x = "Remesas", y = "Consumo")




