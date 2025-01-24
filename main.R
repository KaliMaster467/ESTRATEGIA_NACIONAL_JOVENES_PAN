library(readxl)
library(sf)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(grid)
library(ggmagnify) 
library(scales)
library(foreign)
library(tseries)
library(PASWR2)
library(knitr)
library(stargazer)
library(lmtest)
library(corrplot)
library(car)
 ##IMPORTAMOS EL LISTADO NOMINAL
LN_2024 <- read_excel("LN-2024.xlsx")
View(LN_2024)


##IMPORTAMOS EL MAPA DE DISTRITOS

mapa <- st_read("DISTRITO_FEDERAL.shp")




###AGRUPAMOS DATOS POR DISTRITO
##PRIMERO AGRUPAMOS DATOS DEL LISTADO NOMINAL

LN_2024 <- LN_2024 |>
  group_by(NOMBRE_ENTIDAD, CLAVE_ENTIDAD, CLAVE_DISTRITO) |>
  summarise(
    "LISTADO_NOMINAL" = sum(LISTA_NOMINAL),
    "LISTADO_HOMBRES" = sum(LISTA_HOMBRES),
    "LISTADO_MUJERES" = sum(LISTA_MUJERES),
    "LISTADO_HOMBRES_JOVENES" = sum(LISTA_18_HOMNRES) + sum(LISTA_19_HOMBRES) + 
      sum(LISTA_20_24_HOMBRES) + sum(LISTA_25_29_HOMBRES),
    "LISTADO_MUJERES_JOVENES" = sum(LISTA_18_MUJERES) + sum(LISTA_19_MUJERES) + 
      sum(LISTA_20_24_MUJERES) + sum(LISTA_25_29_MUJERES),
    "TOTAL_JOVENES" = LISTADO_HOMBRES_JOVENES + LISTADO_MUJERES_JOVENES, 
    "PORCENTAJE_JOVENES" = TOTAL_JOVENES / LISTADO_NOMINAL, 
    "PORCENTAJE_JHOMBRES" = LISTADO_HOMBRES_JOVENES / TOTAL_JOVENES, 
    "PORCENTAJE_JMUJERES" = LISTADO_MUJERES_JOVENES / TOTAL_JOVENES
  )
describe(LN_2024)
summary(LN_2024)
###HACEMOS UN SUMMARY

##AGRUPAMOS LA DATA EN EL MAPA SHP
mapa <- mapa |> 
  right_join(LN_2024, by = c("ENTIDAD" = "CLAVE_ENTIDAD", "DISTRITO_F" = "CLAVE_DISTRITO"))

ggplot(data = mapa) +
  geom_sf(aes(fill = PORCENTAJE_JOVENES*100)) +
  scale_fill_gradient(low = "blue", high = "lightblue", name = "Porcentaje de Jóvenes") +
  theme_minimal() +
  ggtitle("Porcentaje de jóvenes por Distrito Federal") + 
  labs(subtitle = "Fuente: INE",
      caption = "Elaborado por Alberto Reyes Briseño")

ggplot(LN_2024, aes(x = PORCENTAJE_JOVENES)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  # Cambiar el color del boxplot
  ggtitle("Distribución del porcentaje de jóvenes 
por Distrito Federal") +   # Añadir título
  theme_minimal() +                                     # Usar tema minimalista
  labs(caption = "Elaboración propia",
       subtitle = "Fuente: INE")       



###############################################
##AHORA AGREGAMOS LA DATA DE VOTOS POR PARTIDO#
##AGRUPAMOS DATOS POR DISTRITO
RES <-RES |> select(ID_ENTIDAD, ID_DISTRITO_FEDERAL, PAN, PRI, PRD, PVEM, PT, MC, MORENA, PAN_PRI_PRD, PAN_PRI, PAN_PRD, PRI_PRD, PVEM_PT_MORENA, 
         PVEM_PT, PVEM_MORENA, PT_MORENA)

RES <- RES |>
  group_by(ID_ENTIDAD, ID_DISTRITO_FEDERAL) |>
  summarise(
    PAN = sum(PAN, na.rm = TRUE),
    PRI = sum(PRI, na.rm = TRUE),
    PRD = sum(PRD, na.rm = TRUE),
    PVEM = sum(PVEM, na.rm = TRUE),
    PT = sum(PT, na.rm = TRUE),
    MC = sum(MC, na.rm = TRUE),
    MORENA = sum(MORENA, na.rm = TRUE),
    PAN_PRI_PRD = sum(PAN_PRI_PRD, na.rm = TRUE),
    PAN_PRI = sum(PAN_PRI, na.rm = TRUE),
    PAN_PRD = sum(PAN_PRD, na.rm = TRUE),
    PRI_PRD = sum(PRI_PRD, na.rm = TRUE),
    PVEM_PT_MORENA = sum(PVEM_PT_MORENA, na.rm = TRUE),
    PVEM_PT = sum(PVEM_PT, na.rm = TRUE),
    PVEM_MORENA = sum(PVEM_MORENA, na.rm = TRUE),
    PT_MORENA = sum(PT_MORENA, na.rm = TRUE),
    "TOTAL_VOTOS" = sum(PAN, PRI, PRD, PVEM, PT, MC, MORENA, PAN_PRI_PRD, PAN_PRI, PAN_PRD, PRI_PRD, PVEM_PT_MORENA, 
                        PVEM_PT, PVEM_MORENA, PT_MORENA, na.rm = TRUE),
    "PORCENTAJE_PAN" = PAN / TOTAL_VOTOS,
    "PORCENTAJE_MORENA" = MORENA / TOTAL_VOTOS,
    "PORCENTAJE_MC" = MC / TOTAL_VOTOS
  )

mapa <- mapa |> 
  right_join(RES, by = c("ENTIDAD" = "ID_ENTIDAD", "DISTRITO_F" = "ID_DISTRITO_FEDERAL"))
  
###MAPA CON PORCENTAJE DE VOTACION PAN DE TODOS LOS VOTOS
ggplot(data = mapa) +
  geom_sf(aes(fill = PORCENTAJE_PAN*100)) +
  scale_fill_gradient(low = "blue", high = "lightblue", name = "Porcentaje de voto PAN") +
  theme_minimal() +
  ggtitle("Porcentaje de votación PAN 
por Distrito Federal") + 
  labs(subtitle = "Fuente: INE",
       caption = "Elaborado por Alberto Reyes Briseño")
describe(RES$PORCENTAJE_PAN)
###CORRELACION JOVENES-VOTOPAN

cor_pan <- cor(mapa$PORCENTAJE_PAN, mapa$PORCENTAJE_JOVENES)
cor_morena <- cor(mapa$PORCENTAJE_MORENA, mapa$PORCENTAJE_JOVENES)
cor_mc <- cor(mapa$PORCENTAJE_MC, mapa$PORCENTAJE_JOVENES)


ggplot(data = mapa, aes(x = PORCENTAJE_PAN, y = PORCENTAJE_JOVENES)) +
  geom_point(color = "darkblue", size = 3) +                      # Añadir puntos
  geom_smooth(method = "lm", color = "lightblue", se = FALSE) +      # Añadir línea de ajuste
  ggtitle("Correlación entre el porcentaje de 
votación del PAN y el porcentaje de Jóvenes") +  # Título
  xlab("Porcentaje de Votación del PAN") +                     # Etiqueta del eje X
  ylab("Porcentaje de Jóvenes") +                             # Etiqueta del eje Y
  theme_minimal() + 
  annotate("text", x = Inf, y = Inf, label = paste("Coeficiente de correlación:", round(cor_pan, 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black",
           fontface = "italic")   
# Tema minimalista

ggplot(data = mapa, aes(x = PORCENTAJE_MORENA, y = PORCENTAJE_JOVENES)) +
  geom_point(color = "#870000", size = 3) +                      # Añadir puntos
  geom_smooth(method = "lm", color = "lightblue", se = FALSE) +      # Añadir línea de ajuste
  ggtitle("Correlación entre el porcentaje de votación 
de MORENA y el orcentaje de jóvenes") +  # Título
  xlab("Porcentaje de Votación del PAN") +                     # Etiqueta del eje X
  ylab("Porcentaje de Jóvenes") +                             # Etiqueta del eje Y
  theme_minimal() + 
  annotate("text", x = Inf, y = Inf, label = paste("Coeficiente de correlación:", round(cor_morena, 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black",
           fontface = "italic")   
# Tema minimalista                                            # Tema minimalista

ggplot(data = mapa, aes(x = PORCENTAJE_MC, y = PORCENTAJE_JOVENES)) +
  geom_point(color = "orange", size = 3) +                      # Añadir puntos
  geom_smooth(method = "lm", color = "lightblue", se = FALSE) +      # Añadir línea de ajuste
  ggtitle("Correlación entre el porcentaje de votación de 
MC y el porcentaje de jóvenes") +  # Título
  xlab("Porcentaje de Votación del PAN") +                     # Etiqueta del eje X
  ylab("Porcentaje de Jóvenes") +                             # Etiqueta del eje Y
  theme_minimal() + 
  annotate("text", x = Inf, y = Inf, label = paste("Coeficiente de correlación:", round(cor_mc, 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black",
           fontface = "italic")   
# Tema minimalista                                            # Tema minimalista


####ESTABLECER LA DIFERENCIA PROMEDIO ENTRE MORENA Y EL PAN

mapa <- mapa |>
  mutate("DIFERENCIA_PORCENTUAL" = PORCENTAJE_MORENA - PORCENTAJE_PAN)

summary(mapa$DIFERENCIA_PORCENTUAL)

sd(mapa$DIFERENCIA_PORCENTUAL)
plot(density(mapa$DIFERENCIA_PORCENTUAL*100), col = "blue", 
     main = "Distribución de la Diferencia Porcentual PAN vs MORENA", xlab = "Diferencia Porcentual %")
#######
media <- mean(mapa$DIFERENCIA_PORCENTUAL * 100)
mediana <- median(mapa$DIFERENCIA_PORCENTUAL * 100)

# Crear el gráfico de densidad
plot(density(mapa$DIFERENCIA_PORCENTUAL * 100), col = "blue", 
     main = "Distribución de la Diferencia Porcentual PAN vs MORENA", 
     xlab = "Diferencia Porcentual %")

# Añadir líneas verticales para la media y la mediana
abline(v = media, col = "red", lwd = 2, lty = 2)    # Línea roja punteada para la media
abline(v = mediana, col = "green", lwd = 2, lty = 3) # Línea verde punteada para la mediana

# Añadir una leyenda con los valores numéricos
legend("topright", legend = c(paste("Media:", round(media, 2)), 
                              paste("Mediana:", round(mediana, 2)),
                              paste("SD:", round(sd(mapa$DIFERENCIA_PORCENTUAL*100), 2))), 
       col = c("red", "green"), lwd = 2, lty = c(2, 3))
#####

summary(mapa$DIF_VOTOS)

quantile(mapa$DIFERENCIA_PORCENTUAL)
###ESTABLECEMOS UN 20% DE DIFERENCIA COMO PRIORIDAD

mapa <- mapa |>
  mutate("INTERVALO_20" = if_else(DIFERENCIA_PORCENTUAL > 0 & DIFERENCIA_PORCENTUAL <= .15, 
                                  "R", "N"), 
         "DIF_VOTOS" = MORENA - PAN)

mapa |>
  filter(INTERVALO_20 == "R") |>
  group_by(NOMBRE_ENTIDAD) |>
  tally() |>
  select(-geometry)

mapa |>
  filter(INTERVALO_20 == "R") |>
  select(DIF_VOTOS) |>
  summary()

##grafico zoomeado

# Crear el gráfico de zoom
ggplot(data = mapa) +
  geom_sf(aes(fill = INTERVALO_20)) +
  scale_fill_manual(values = c("grey", "blue"), labels = c("NR", "Rentable")) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  ggtitle("Distritos Federales con menos de 15% de diferencia 
respecto a MORENA") +
  labs(subtitle = "Fuente: INE",
       caption = "Elaborado por Alberto Reyes Briseño")

##MAPAS CDMX,MORELOS, EDOMEX

metropol <- mapa |>
  filter(ENTIDAD == 9 | ENTIDAD == 15 | ENTIDAD == 17)

ggplot(data = metropol) +
  geom_sf(aes(fill = INTERVALO_20)) +
  scale_fill_manual(values = c("grey", "blue"), labels = c("No empate", "Empate")) + 
  theme_void()

####
#### GRAFICO REPUBLICA POR DIFERENCIAL DE VOTACIÓN

ggplot(data = mapa) +
  geom_sf(aes(fill = DIFERENCIA_PORCENTUAL*100)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "#870000", 
                       midpoint = 0, name = "Diferencia Porcentual (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggtitle("Diferencia porcentual PAN vs MORENA distrital") + 
  labs(subtitle = "Fuente: INE",
       caption = "Elaborado por Alberto Reyes Briseño")

##MAPAS CDMX,MORELOS, EDOMEX



ggplot(data = metropol) +
  geom_sf(aes(fill = DIFERENCIA_PORCENTUAL*100)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "#870000", 
                       midpoint = 0, name = "Diferencia Porcentual (%)")+ 
  theme_void()
#### AÑADIR BINOMIAL DE PROBABILIADD de que el diferencial sea mas voto jov


####QUIEN GANA
corp <- cor(mapa$DIFERENCIA_PORCENTUAL, mapa$PORCENTAJE_JOVENES)
mapa <- mapa |>
  mutate(GANADOR = if_else(DIF_VOTOS > 0, "MORENA", "PAN"))####
##CORRELACION % PADRON JOVEN VS DIFERENCIAL DE VOTACIÓN

ggplot(data = mapa, aes(x = DIFERENCIA_PORCENTUAL, y = PORCENTAJE_JOVENES)) +
  geom_point(color = "darkblue", size = 3) +                      # Añadir puntos
  geom_smooth(method = "lm", color = "lightblue", se = FALSE) +      # Añadir línea de ajuste
  ggtitle("Correlación entre el la diferencia de votos porcentual
PAN vs MORENA y el porcentaje de jóvenes en el padrón electoral por distrito") +  # Título
  xlab("Diferencial PAN vs MORENA") +                     # Etiqueta del eje X
  ylab("Porcentaje de Jóvenes") +                             # Etiqueta del eje Y
  theme_minimal() + 
  annotate("text", x = Inf, y = Inf, label = paste("Coeficiente de correlación:", round(corp, 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black",
           fontface = "italic")   



pbinom(29000, 100000, .29)
qbinom(.95, 200000, .29)
mapa <- mapa |> 
  mutate(PBIN_95_VJOVEN = qbinom(.95, TOTAL_VOTOS, PORCENTAJE_JOVENES))

###TABLA PARA EXCEL
mapa <- mapa |>
  mutate(COMPENSA = if_else(INTERVALO_20 == "R", PBIN_95_VJOVEN - DIF_VOTOS, 0))


write.csv(mapa |>
            st_drop_geometry(), "def.csv", row.names = FALSE)

######PROPIEDAD JOVENES
persona <- read_csv("persona.csv")
View(persona)
####JOVENES POR HOGAR
jov <- persona |>
  select(folioviv, edad) |>
  filter(edad <= 29)|>
  group_by(folioviv) |>
  tally()

library(PASWR2)

eda(jov$n)
describe(jov)
media <- mean(jov$n)
mediana <- median(jov$n)
histogram(jov$n, main = "Histograma de jóvenes por hogar",
          xlab = "Número de personas menores a 30 años",
          ylab = "N", 
          col = "lightblue")
###PROPIETARIOS VS NO PROPIETARIOS
vivienda <- read_csv("vivienda.csv")
View(vivienda)

jov <- jov |> 
  right_join(vivienda |> 
               select(folioviv, tenencia), by = c("folioviv" = "folioviv"))

jov <- jov |>
  mutate("PROPIEDAD" = if_else(tenencia == 4, "PROPIA", "OTRO"))

barp <- jov |> 
  group_by(PROPIEDAD) |>
  summarise(
    "JOVENES" = sum(n, na.rm = TRUE)
    
  ) |>
  mutate(PORCENTAJE = JOVENES / sum(JOVENES))

###CALCULAMOS EL INTERVALO DE CONFIANZA
p_hat <- .491

# Nivel de confianza
alpha <- 0.05
z <- qnorm(1 - alpha/2)  # Valor crítico de la distribución normal

# Calcular el error estándar
se <- sqrt((p_hat*(1 - p_hat)) / 107848)

# Calcular los límites del intervalo de confianza
lower_bound <- p_hat - z * se
upper_bound <- p_hat + z * se

# Mostrar el intervalo de confianza
cat("Intervalo de confianza del 95% para la proporción p:", "\n")
cat("Límite inferior:", round(lower_bound, 4), "\n")
cat("Límite superior:", round(upper_bound, 4), "\n")

###CALCULAMOS EL INTERVALO DE CONFIANZA
p_hat <- .509

# Nivel de confianza
alpha <- 0.05
z <- qnorm(1 - alpha/2)  # Valor crítico de la distribución normal

# Calcular el error estándar
se <- sqrt((p_hat*(1 - p_hat)) / 107848)

# Calcular los límites del intervalo de confianza
lower_bound <- p_hat - z * se
upper_bound <- p_hat + z * se

# Mostrar el intervalo de confianza
cat("Intervalo de confianza del 95% para la proporción p:", "\n")
cat("Límite inferior:", round(lower_bound, 4), "\n")
cat("Límite superior:", round(upper_bound, 4), "\n")

barplot(height = barp$JOVENES, names = barp$PROPIEDAD, col = c("lightblue", "orange"), main = "Frecuencia por tipo de vivienda",
        xlab = "Tipo de propiedad", ylab = "N")

####HACINAMIENTO

hacin <- persona |>
  group_by(folioviv) |>
  tally()

hacin <- hacin |> 
  right_join(vivienda |> 
               select(folioviv, tenencia, cuart_dorm), by = c("folioviv" = "folioviv"))

hacin <- hacin |>
  mutate("PROPIEDAD" = if_else(tenencia == 4, "PROPIA", "OTRO"))

hacin <- hacin |>
  mutate(INDICE_HAC = n/cuart_dorm)


plot(density(hacin$INDICE_HAC))
boxplot(hacin$INDICE_HAC)
summary(hacin$INDICE_HAC)
ggplot(hacin) + 
  geom_density(aes(x = INDICE_HAC, color = PROPIEDAD)) + 
  scale_x_log10()+
  labs(
    title = "Densidad índice de hacinamiento 
(número de habitantes / cuartos)",
    subtitle = "Transformación logarítmica
Elaboración propia",
    caption = "Fuente: ENH 2017"
  ) + 
  xlab("Indice hacinamiento") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("lightblue", "orange"))

hacin <- hacin |>
  mutate(HACINAMIENTO = if_else(INDICE_HAC >= 2.5, 1, 0 ))

hacin |> 
  group_by(HACINAMIENTO) |>
  tally() |>
  mutate(PORCENTAJE = n /sum(n))

#ESTIMACIÓN POR INTERVALOS
p_hat <- .262

# Nivel de confianza
alpha <- 0.05
z <- qnorm(1 - alpha/2)  # Valor crítico de la distribución normal

# Calcular el error estándar
se <- sqrt((p_hat*(1 - p_hat)) / 56680)

# Calcular los límites del intervalo de confianza
lower_bound <- p_hat - z * se
upper_bound <- p_hat + z * se

# Mostrar el intervalo de confianza
cat("Intervalo de confianza del 95% para la proporción p:", "\n")
cat("Límite inferior:", round(lower_bound, 4), "\n")
cat("Límite superior:", round(upper_bound, 4), "\n")

##GRAFICA POR PROPIEADAD O NO

estathacin <- hacin |>
  group_by(PROPIEDAD) |>
  summarise(
    "ESTIMADOR_HACINAMIENTO" = mean(INDICE_HAC, na.rm = TRUE), 
    "MEDIANA" = median(INDICE_HAC, na.rm = TRUE),
    "TOT_VIVIENDAS" = n(), 
    "HACINAMIENTO" = sum(HACINAMIENTO == 1, na.rm = TRUE),
    "NO_HACINAMIENTO" = TOT_VIVIENDAS - HACINAMIENTO, 
    "ESTIMADOR_P_HACINAMIENTO" = HACINAMIENTO / TOT_VIVIENDAS, 
    "SD_HACINAMIENTO" = sd(INDICE_HAC)
  ) |>
  pivot_longer(cols = c(HACINAMIENTO, NO_HACINAMIENTO), 
               names_to = "HACINAMIENTO_STATUS", 
               values_to = "FREQUENCY")


ggplot(estathacin) + 
  geom_bar(aes(x = PROPIEDAD, y = FREQUENCY, fill = HACINAMIENTO_STATUS), stat = "identity", 
           position = position_dodge(width = 0.9), color = "black") + 
  geom_text(aes(x = PROPIEDAD, y = FREQUENCY, label = FREQUENCY), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3,
            hjust = 1,
            size = 4) + 
  scale_fill_manual(values = c("darkblue", "lightblue")) + 
  theme_minimal()+ 
  theme(legend.position = "bottom") + 
  labs(
    title = "Frecuencia por estado de vivienda Propias/Otras", 
    subtitle = "Elaboración propia", 
    caption = "Fuente = ENH 2017", 
    xlab = "Tipo de vivienda", 
    ylab = "Frecuencia", 
    fill = "Estado de vivienda"
  ) 
t.test(hacin |> 
         filter(PROPIEDAD == "PROPIA") |>
         select(INDICE_HAC), hacin |> 
         filter(PROPIEDAD == "OTRO") |>
         select(INDICE_HAC))
####GRAFICO LA PRUEBA
library(cowplot)

# Parámetros de la prueba t
t_value <- -19.87
df <- 54231
alpha <- 0.05

# Valores críticos para alfa
critical_value <- qt(1 - alpha / 2, df)

# Generar datos para la distribución t
x <- seq(-25, 25, length = 1000)
y <- dt(x, df)

# Crear el gráfico de la distribución t
plot <- ggplot(data.frame(x, y), aes(x, y)) +
  geom_line() +
  geom_area(data = subset(data.frame(x, y), x > critical_value | x < -critical_value), aes(y = y), fill = "red", alpha = 0.5) +
  geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = t_value, linetype = "solid", color = "green") +
  annotate("text", x = t_value, y = 0.02, label = paste("t =", round(t_value, 2)), color = "green", vjust = -1) +
  labs(
    title = "Distribución t con Áreas de Rechazo",
    x = "Valores t",
    y = "Densidad",
    caption = "Líneas azules: valores críticos; Línea verde: estadístico t observado"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(plot)
#### RENTA PROMEDIO

hacin <- hacin |>
  right_join(vivienda |>
               select(folioviv, pago_renta), by = c("folioviv" = "folioviv"))

plot(density(na.omit(hacin$pago_renta)), main = "Densidad pago de renta mensual", 
     xlab = "Renta mensual \n $MXN", col = "blue")

# Añadir líneas verticales para la media y la mediana
abline(v = mean(hacin$pago_renta, na.rm = TRUE), col = "red", lwd = 2, lty = 2)    # Línea roja punteada para la media
abline(v = median(hacin$pago_renta, na.rm = TRUE), col = "green", lwd = 2, lty = 3) # Línea verde punteada para la mediana

# Añadir una leyenda con los valores numéricos
legend("topright", legend = c(paste("Media:", round(mean(hacin$pago_renta, na.rm = TRUE), 2)), 
                              paste("Mediana:", round(median(hacin$pago_renta, na.rm = TRUE), 2)),
                              paste("SD:", round(sd(hacin$pago_renta, na.rm = TRUE), 2))), 
       col = c("red", "green"), lwd = 2, lty = c(2, 3))




ggplot(hacin) + 
  geom_boxplot(aes( x = pago_renta), fill = "lightblue", color = "blue") + 
  labs(
    title = "Boxplot pago de renta mensual por concepto de vivienda", 
    subtitle = "Elaboración propia", 
    caption = "Fuente: ENH 2017"
  ) + 
  theme_minimal() + 
  xlab("Pago de renta mensual \n$MXN") +  
  scale_x_continuous(labels = dollar_format(prefix = "$"))

###CALCULAMOS EL INTERVALO DE CONFIANZA
media <- mean(hacin$pago_renta, na.rm = TRUE)
sd <- sd(hacin$pago_renta, na.rm = TRUE)
n <- length(na.omit(hacin$pago_renta))

# Nivel de confianza
confianza <- 0.95
error_estandar <- sd / sqrt(n)
alpha <- 1 - confianza
t_critico <- qt(1 - alpha/2, df = n - 1)

# Intervalo de confianza
IC_inferior <- media - t_critico * error_estandar
IC_superior <- media + t_critico * error_estandar

c(IC_inferior, IC_superior)

##CALCULAR POR ENTIDAD

hacin <- hacin |>
  mutate(ENTIDAD = as.numeric(substr(folioviv, 1, 2)))

hacin <- hacin |>
  right_join(LN_2024 |> select(CLAVE_ENTIDAD, NOMBRE_ENTIDAD), by = c("ENTIDAD" = "CLAVE_ENTIDAD"))

renta_estado <- hacin |>
  group_by(NOMBRE_ENTIDAD) |>
  summarise(
    "RENTA_PROMEDIO" = mean(pago_renta, na.rm = TRUE), 
    "SD" = sd(pago_renta, na.rm = TRUE), 
    "HACINAMIENTO_PROMEDIO" = mean(INDICE_HAC, na.rm = TRUE)
  )

ggplot(renta_estado, aes(x= NOMBRE_ENTIDAD, y=RENTA_PROMEDIO)) +
  geom_bar(stat="identity", fill="skyblue", alpha=1, width=.4) +
  coord_flip() +
  ylab("Renta promedio vivienda \n$MXN") +
  xlab("")+
  labs(
    title = "Renta promedio por estado",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENH 2017"
  ) + 
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  theme_bw()
###JOVENES JEFE DE FAMILIA
jovenes_jefes <- persona |>
  select(folioviv, id_pobla, edad) |> 
  filter(id_pobla == "01") |>
  mutate("PERSONA" = if_else(edad <= 29, "JOVEN", "ADULTA")) |>
  right_join(vivienda |> 
               select(folioviv, pago_renta), by = c("folioviv" = "folioviv")) |>
  group_by(PERSONA) |>
  summarise(
    "n" = n(), 
    "RENTA_PROMEDIO" = mean(pago_renta, na.rm = TRUE), 
    "ESTIMADOR_VAR" = sd(pago_renta, na.rm = TRUE)
  ) |>
  na.omit() |>
  mutate("ESTIMADOR_P" = n / sum(n, na.rm = TRUE),
         SE_RENTA_PROMEDIO = ESTIMADOR_VAR / sqrt(n),
         t_critico = qt(0.975, df = n - 1),
         CI_LOWER_RENTA = RENTA_PROMEDIO - t_critico * SE_RENTA_PROMEDIO,
         CI_UPPER_RENTA = RENTA_PROMEDIO + t_critico * SE_RENTA_PROMEDIO)

write.csv(jovenes_jefes, "jovenes_renta.csv")
##PRUEBA MEDIAS IGUALES
t.test( persona |>
          select(folioviv, id_pobla, edad) |> 
          filter(id_pobla == "01") |>
          mutate("PERSONA" = if_else(edad <= 29, "JOVEN", "ADULTA")) |>
          right_join(vivienda |> 
                       select(folioviv, pago_renta), by = c("folioviv" = "folioviv")) |>
          filter(PERSONA == "JOVEN") |>
          select(pago_renta) |>
          na.omit(), persona |>
          select(folioviv, id_pobla, edad) |> 
          filter(id_pobla == "01") |>
          mutate("PERSONA" = if_else(edad <= 29, "JOVEN", "ADULTA")) |>
          right_join(vivienda |> 
                       select(folioviv, pago_renta), by = c("folioviv" = "folioviv")) |>
          filter(PERSONA == "ADULTA") |>
          select(pago_renta) |>
          na.omit())
##GRAFICA INTERVALO DE CONFIANZA 
# Instalar y cargar ggplot2 si no está instalado

# Parámetros de la prueba t
t_observado <- -7.9238
df <- 6057.6
p_value <- 2.721e-15
ci_lower <- -471.5563
ci_upper <- -284.5066
mean_diff <- 2197.381 - 1819.350

# Crear un vector para la distribución t
x <- seq(-10, 10, length = 1000)
y <- dt(x, df = df)

# Crear un data frame para la distribución t
df_t <- data.frame(x = x, y = y)

# Crear el gráfico de la distribución t
ggplot(df_t, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(-qt(0.975, df), qt(0.975, df)), linetype = "dashed", color = "red") +
  geom_vline(xintercept = t_observado, linetype = "solid", color = "green") +
  annotate("text", x = t_observado, y = max(y) / 2, label = paste("t =", round(t_observado, 2)), color = "green", vjust = -1) +
  annotate("text", x = -qt(0.975, df), y = max(y) / 2, label = paste("LIM INF =", round(ci_lower, 2)), color = "red", vjust = -1) +
  annotate("text", x = qt(0.975, df), y = max(y) / 2, label = paste("LIM SUP =", round(ci_upper, 2)), color = "red", vjust = -1) +
  labs(
    title = "Distribución t y Prueba de Hipótesis",
    x = "t",
    y = "Densidad"
  ) +
  theme_minimal()
##########
##########
## SALARIO Y GRADO DE EDUCACIÓN JÓVENES####

salario <- read_csv("conjunto_de_datos_coe2_enoe_2024_1t.csv")
socioc <- read_csv("conjunto_de_datos_sdem_enoe_2024_1t.csv")

salario <- salario |>
  select(cd_a, ent, con, upm, d_sem, n_pro_viv,
         v_sel, n_hog, h_mud, n_ent, per, n_ren, eda, n_inf, p6b2, p6_3, p6_7)

salario <- salario |>
  mutate("PERSONA" = if_else(eda <= 29, "JOVEN", "ADULTO"))

mean(salario$p6b2, na.rm = TRUE)
salario |>
  group_by(PERSONA) |>
  summarise(
    "ESTIMADOR_SALARIO" = mean(p6b2, na.rm = TRUE),
    "SD_SALARIO" = sd(p6b2, na.rm = TRUE)
  )

mean_mediana <- salario %>%
  group_by(PERSONA) %>%
  summarise(
    mean_value = mean(p6b2, na.rm = TRUE),
    median_value = median(p6b2, na.rm = TRUE)
  )

ggplot(salario) + 
  geom_density(aes(x = p6b2, fill = PERSONA), alpha = .4, color = "black") + 
  scale_x_continuous(labels = dollar_format(prefix = "$")) + 
  theme_bw()+ 
  labs(
    title = "Densidad salrios mensuales jóvenes y adultos",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENOE I TRIM 2024"
  ) + 
  theme(legend.position = "bottom")+
  xlab("Salario mensual $MXN")

ggplot(salario) + 
  geom_boxplot(aes(x = p6b2, fill = PERSONA)) + 
  theme_bw()+
  scale_x_continuous(labels = dollar_format(prefix = "$")) + 
  labs(
    title = "Boxplot ingreso mensual", 
    subtitle = "Elaboración propia",
    caption = "Fuente ENOE I TRIM 2024"
  )+ 
  xlab("Ingreso mensual $MXN") + 
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "bottom")

##CALCULAMOS LOS INTERVALOS DE CONFIANZA

tab_salario <- salario |>
  group_by(PERSONA) |>
  summarise(
    "ESTIMADOR_SALARIO" = mean(p6b2, na.rm = TRUE),
    "SD_SALARIO" = sd(p6b2, na.rm = TRUE),
    "n" = n()) |>
  mutate(  SE_INGRESO_PROMEDIO = SD_SALARIO / sqrt(n),
           t_critico = qt(0.975, df = n - 1),
           CI_LOWER_RENTA = ESTIMADOR_SALARIO - t_critico * SE_INGRESO_PROMEDIO,
           CI_UPPER_RENTA = ESTIMADOR_SALARIO + t_critico * SE_INGRESO_PROMEDIO)

write.csv(tab_salario, "tab_salario.csv")
##POR ESTADO 

calcular_valor_p <- function(data) {
  t_test <- t.test(p6b2 ~ PERSONA, data = data %>% filter(PERSONA %in% c("JOVEN", "ADULTO")))
  return(t_test$p.value)
}

salario_edo <- salario |> 
  group_by(ent) |>
  summarise(
    "SALARIO_PROMEDIO_JOVEN" = mean(p6b2[PERSONA == "JOVEN"], na.rm = TRUE),
    "SD_JOVEN" = sd(p6b2[PERSONA == "JOVEN"], na.rm = TRUE),
    N_JOVEN = sum(!is.na(p6b2[PERSONA == "JOVEN"])), 
    "SALARIO_PROMEDIO_ADULTO" = mean(p6b2[PERSONA == "ADULTO"], na.rm = TRUE),
    "SD_ADULTO" = sd(p6b2[PERSONA == "ADULTO"], na.rm = TRUE), 
    N_ADULTO = sum(!is.na(p6b2[PERSONA == "ADULTO"])), 
    "Prueba_F" = 2 * min(pf(SD_JOVEN**2/SD_ADULTO**2, N_JOVEN -1, N_ADULTO - 1 ), 1 - pf(SD_JOVEN**2/SD_ADULTO**2, N_JOVEN -1, N_ADULTO - 1 ))
  )

salario_edo <- salario_edo |>
  right_join(LN_2024 |> 
               select(CLAVE_ENTIDAD, NOMBRE_ENTIDAD) |>
               distinct(), by = c("ent" = "CLAVE_ENTIDAD")) 


salario_edo_long <- salario_edo %>%
  pivot_longer(cols = starts_with("SALARIO_PROMEDIO"), 
               names_to = "grupo_etario", 
               values_to = "salario_promedio") %>%
  mutate(grupo_etario = case_when(
    grupo_etario == "SALARIO_PROMEDIO_JOVEN" ~ "Joven",
    grupo_etario == "SALARIO_PROMEDIO_ADULTO" ~ "Adulto"
  ))

ggplot(salario_edo_long, aes(x = NOMBRE_ENTIDAD, y = salario_promedio, fill = grupo_etario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ingreso Promedio por Estado",
       subtitle = "Elaboración propia", 
       caption = "Fuente: ENOE I TRIM 2024",
       x = "Estado",
       y = "Ingreso Promedio $MXN",
       fill = "Grupo de edad") +
  theme_bw() +
  theme(legend.position = "bottom")+ 
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  scale_fill_manual(values = c("darkblue", "skyblue" ))
  

salario_edo <- salario_edo |>
  mutate(
    diff_means = SALARIO_PROMEDIO_JOVEN - SALARIO_PROMEDIO_ADULTO,
    se_diff = sqrt((SD_JOVEN^2 / N_JOVEN) + (SD_ADULTO^2 / N_ADULTO)),
    df = ( (SD_JOVEN^2 / N_JOVEN) + (SD_ADULTO^2 / N_ADULTO) )^2 /
      ( ( (SD_JOVEN^2 / N_JOVEN)^2 / (N_JOVEN - 1) ) + ( (SD_ADULTO^2 / N_ADULTO)^2 / (N_ADULTO - 1) ) ),
    t_critical = qt(1 - .05 / 2, df),
    lower_bound = diff_means - t_critical * se_diff,
    upper_bound = diff_means + t_critical * se_diff
    
  )

write.csv(salario_edo, "salario_edo.csv")

ggplot(salario_edo, aes(x = reorder(NOMBRE_ENTIDAD, lower_bound), y = lower_bound)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "red") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Entidad", y = "Lower Bound") + 
  theme_bw() +
  labs(
    title = "Límite inferior del intervalo de confianza al 95% que
muestra brecha salarial entre adultos y jóvenes por entidad federativa", 
    subtitle = "Elaboración propia",
    caption = "Fuente: ENOE I TRIM 2024"
  ) + 
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  ylab("Diferencia salarial $MXN") + 
  xlab("")
#####SALARIO EN RELACIÓN CON EL NIVEL DE EDUCACIÓN


socioc <- socioc |>
  select(r_def, loc, mun, est, est_d_tri, est_d_men, ageb, t_loc_tri,
         t_loc_men, cd_a, ent, con, upm, d_sem, n_pro_viv, v_sel, n_hog, eda, cs_p13_1) |>
  right_join(salario, by = c("cd_a", "ent", "con", "upm", "d_sem", "n_pro_viv", "v_sel", "n_hog", "eda"))

edu_sal <- socioc |>
  filter(PERSONA == "JOVEN" & cs_p13_1 != 99) |>
  select(eda, p6b2, cs_p13_1, PERSONA) |>
  mutate("GRADO_ESTUDIO" = if_else(cs_p13_1 == 0, "NINGUNO",
                                   if_else(cs_p13_1 == 1, "PREESCOLAR",
                                           if_else(cs_p13_1 == 2, "PRIMARIA",
                                                   if_else(cs_p13_1 == 3, "SECUNDARIA", 
                                                           if_else(cs_p13_1 == 4, "PREPARATORIA",
                                                                   if_else(cs_p13_1 == 5, "NORMAL",
                                                                          if_else(cs_p13_1 == 6, "TECNICA",
                                                                                  if_else(cs_p13_1 == 7, "PROFESIONAL",
                                                                                          if_else(cs_p13_1 == 8, "MAESTRIA",
                                                                                                  if_else(cs_p13_1 == 9, "DOCTORADO", "NO SABE")))))))))))

ggplot(edu_sal) + 
  geom_density(aes(x = p6b2, fill = GRADO_ESTUDIO), alpha = 1) + 
  facet_wrap(~GRADO_ESTUDIO) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set3")

graf_edu_sal <- edu_sal |> 
  group_by(GRADO_ESTUDIO) |>
  summarise(
    "SUELDO_PROMEDIO" = mean(p6b2, na.rm = TRUE), 
    "SD_SUELDO" = sd(p6b2, na.rm = TRUE), 
    "n" = sum(!is.na(p6b2))
  ) |>
  mutate(  SE_INGRESO_PROMEDIO = SD_SUELDO / sqrt(n),
           t_critico = qt(0.975, df = n - 1),
           CI_LOWER_RENTA = SUELDO_PROMEDIO - t_critico * SE_INGRESO_PROMEDIO,
           CI_UPPER_RENTA = SUELDO_PROMEDIO + t_critico * SE_INGRESO_PROMEDIO)

ggplot(graf_edu_sal, aes(x = GRADO_ESTUDIO, y = SUELDO_PROMEDIO)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "skyblue", color = "black") + 
  geom_errorbar(aes(ymin = CI_LOWER_RENTA, ymax = CI_UPPER_RENTA), width = 0.2, color = "red") + 
  theme_bw() + 
  labs(
    title = "Sueldo promedio mensual de jóvenes por grado de estudios en el país", 
    subtitle = "Elaboración propia",
    caption = "Fuente: ENOE I TRIM 2024"
  ) + 
  xlab("Grado de estudios") + 
  ylab("") + 
  scale_y_continuous(labels = dollar_format(prefix = "$"))

write.csv(graf_edu_sal, "edusal.csv")

##########
########## SITUACIÓN DE EDUCACIÓN
###
vivienda_edu <- read_csv("conjunto_de_datos_tvivienda_enape_2021.csv")

vivienda_edu <- vivienda_edu |>
  select(FOLIO, P4_1_1) |>
  mutate("PERCEPCION" = if_else(P4_1_1 == 1, "MUY DE ACUERDO",
                                if_else(P4_1_1 == 2, "ALGO DE ACUERDO",
                                        if_else(P4_1_1 == 3, "POCO DE ACUERDO",
                                                if_else(P4_1_1 == 4, "NADA DE ACUERDO",
                                                "NO SABE"))))) |>
  na.omit()


vivienda_edu |> 
  group_by(PERCEPCION) |>
  summarise(
    "n" = sum(!is.na(P4_1_1)),
  ) |>
  mutate("ESTIMADOR_P" = n/ sum(n),
         "LIM_INF" = ESTIMADOR_P - qnorm(.975)* 
           sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n),
         "LIM_SUP" = ESTIMADOR_P + qnorm(.975)* 
           sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n))

##IMPORTO LA TABLA DE REGISTROS PERSONALES 
persona_edu <- read_csv("conjunto_de_datos_tmodulo_enape_2021.csv")

persona_edu <- persona_edu |>
  select(FOLIO, N_REN, SEXO, EDAD, ENT, P3_2, PA3_3_NIVEL, PA3_4, PA3_5, 
         PB3_5_NIVEL, PB3_16_2, PC3_2, PC3_3_1, PC3_5, PC3_6, PC3_7, PD3_3)

#EDUCACIÓN INFANTIL, GUARDERIA

persona_edu <- persona_edu |>
  mutate("EDU_INF" = if_else(P3_2 == 1, "SI", 
                             if_else(P3_2 == 2, "NO", NA))) 

persona_edu |> 
  group_by(EDU_INF) |>
  summarise(
    "n" = sum(!is.na(P3_2))
  ) |>
  mutate(
    "ESTIMADOR_P" = n / sum(n)
  )
##### DERSERCIÓN DEL ÚTLIMO CICLO ESCOLAR

persona_edu <- persona_edu |>
  mutate(
    ULTIMO_GRADO = case_when(
      PA3_3_NIVEL == "01" ~ "EDUCACION INICIAL",
      PA3_3_NIVEL == "02" ~ "PREESCOLAR",
      PA3_3_NIVEL == "03" ~ "PRIMARIA",
      PA3_3_NIVEL == "04" ~ "SECUNDARIA",
      PA3_3_NIVEL == "05" ~ "TECNICO",
      PA3_3_NIVEL == "06" ~ "PREPARATORIA",
      PA3_3_NIVEL == "07" ~ "BACHILLERATO TECNOLOGICO",
      PA3_3_NIVEL == "08" ~ "TECNICO SUPERIOR",
      PA3_3_NIVEL == "09" ~ "LICENCIATURA",
      PA3_3_NIVEL == "10" ~ "ESPECIALIDAD",
      PA3_3_NIVEL == "11" ~ "MAESTRIA",
      PA3_3_NIVEL == "12" ~ "DOCTORADO"
    )
  )

graf_edu_pers <- persona_edu |>
  group_by(ULTIMO_GRADO) |>
  summarise(
    "n" = sum(!is.na(PA3_3_NIVEL)),
    "n_desercion" = sum(PA3_4 == 2, na.rm = TRUE),
    "PORC_DESERCION" = n_desercion/n
  ) |>
  mutate(
    "ESTIMADOR_P" = n / sum(n),
    "LIM_INF" = ESTIMADOR_P - qnorm(.975)* 
      sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n),
    "LIM_SUP" = ESTIMADOR_P + qnorm(.975)* 
      sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n),
    "ESTIMADOR_DESERCION" = n_desercion / sum(n_desercion, na.rm = TRUE),
    "LIM_INF_D" = ESTIMADOR_DESERCION - qnorm(.975)* 
      sqrt((ESTIMADOR_DESERCION*(1-ESTIMADOR_DESERCION))/n_desercion),
    "LIM_SUP_D" = ESTIMADOR_DESERCION + qnorm(.975)* 
      sqrt((ESTIMADOR_DESERCION*(1-ESTIMADOR_DESERCION))/n_desercion),
  ) 

write.csv(graf_edu_pers, "graf_edu.csv")

###CAUSAL DE DESERCIÓN ESCOLAR EN SECUNDARIA, PREPA, LIC Y PRIMARIA

ggplot(graf_edu_pers, aes(x = reorder(ULTIMO_GRADO, +ESTIMADOR_DESERCION), y = ESTIMADOR_DESERCION)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") + 
  xlab("")+
  ylab("% Deserción") + 
  labs(
    title = "% Deserción por nivel de estudios",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENAPE 2021"
  ) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

###FILTRO POR ESTADO

persona_edu <- persona_edu |>
   mutate(ent = as.numeric(ENT)) |>
   right_join(LN_2024 |> 
     select(CLAVE_ENTIDAD, NOMBRE_ENTIDAD) |>
     distinct(), by = c("ent" = "CLAVE_ENTIDAD"))
####FILTAR NIVEL DE ESTUDIOS

edo_desc <- persona_edu |>
  filter(ULTIMO_GRADO == "PREPARATORIA" | ULTIMO_GRADO == "LICENCIATURA" | ULTIMO_GRADO == "SECUNDARIA") |> 
  group_by(NOMBRE_ENTIDAD) |>
  summarise(
    "n_desercion_PREPA" = sum(ULTIMO_GRADO == "PREPARATORIA" & PA3_4 == 2, na.rm = TRUE),
    "n_p" = sum(ULTIMO_GRADO == "PREPARATORIA" & PA3_4 == 1, na.rm = TRUE) , 
    "n_desercion_LICENCIATURA" = sum(ULTIMO_GRADO == "LICENCIATURA" & PA3_4 == 2, na.rm = TRUE),
    "n_l" = sum(ULTIMO_GRADO == "LICENCIATURA" & PA3_4 == 1, na.rm = TRUE) ,
    "n_desercion_SECUNDARIA" = sum(ULTIMO_GRADO == "SECUNDARIA" & PA3_4 == 2, na.rm = TRUE),
    "n_S" = sum(ULTIMO_GRADO == "SECUNDARIA" & PA3_4 == 1, na.rm = TRUE) 
  ) |>
  mutate(
    "ESTIMADOR_P_PREPA" = n_desercion_PREPA / sum(n_desercion_PREPA),
    "ESTIMADOR_P_LIC" = n_desercion_LICENCIATURA / sum(n_desercion_LICENCIATURA),
    "ESTIMADOR_P_SEC" = n_desercion_SECUNDARIA / sum(n_desercion_SECUNDARIA)
  ) |>
  pivot_longer(cols = starts_with("ESTIMADOR_P_"), 
               names_to = "nivel_desercion", 
               values_to = "ESTIMADOR_P") 

ggplot(edo_desc, aes( reorder(NOMBRE_ENTIDAD, +ESTIMADOR_P), y = ESTIMADOR_P, fill = nivel_desercion)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()+ 
  scale_fill_manual(values = c("darkblue", "skyblue" , "green" ))+
  labs(title = "% del total de deserciones por estado y nivel académico",
       subtitle = "Elaboración propia", 
       caption = "Fuente: ENAPE 2021",
       fill = "Grupo de edad") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5)) +  
  xlab("") + 
  ylab("% promedio del total de deserciones")
  



persona_edu <- persona_edu |>
  mutate(
    "DECERCION" = case_when(
      PA3_5 == "01" ~ "FALTA DE RECURSOS",
      PA3_5 == "02" ~ "TRABAJO",
      PA3_5 == "03" ~ "FALTA DE INTERES",
      PA3_5 == "04" ~ "REPROBO",
      PA3_5 == "05" ~ "CIERRE DE ESCUELA O ESTABA LEJOS",
      PA3_5 == "06" ~ "DISCAPACIDAD FISICA O MENTAL",
      PA3_5 == "07" ~ "PANDEMIA",
      PA3_5 == "08" ~ "ENFERMO DE COVID",
      PA3_5 == "09" ~ "ENFERMEDAD O PROBLEMAS DE SALUD FISICA O EMOCIONAL",
      PA3_5 == "10" ~ "SE UNIO, CASO O EMBARAZO",
      PA3_5 == "11" ~ "QUEHACERES DEL HOGAR O CUIDAR A UN FAMILIAR",
      PA3_5 == "12" ~ "FAMILIA O PADRES NO LA DEJARON SEGUIR ESTUDIANDO",
      PA3_5 == "13" ~ "PROBLEMAS PERSONALES",
      PA3_5 == "14" ~ "OTRO"
      
    )
           )

  graf_caus_des <- persona_edu |>
    filter(ULTIMO_GRADO == "PREPARATORIA" | ULTIMO_GRADO == "LICENCIATURA" | ULTIMO_GRADO == "SECUNDARIA") |>
    group_by(DECERCION) |>
    summarise(
      "n" = sum(!is.na(PA3_5))
    ) |> 
    mutate(
      "ESTIMADOR_P" = n / sum(n, na.rm = TRUE)
    )
ggplot(graf_caus_des, aes(x = reorder(DECERCION, +ESTIMADOR_P), y= ESTIMADOR_P)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
  theme_bw() + 
  labs(
    title = "Porcentaje de deserción por causa",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENAPE 2021"
  )+
  xlab("") + 
  ylab("%")+ 
  coord_flip()
  
### ESTUDIO DE DEPRESIÓN EN EL ÁMBITO EDUCATIVO
persona_edu <- persona_edu |>
  mutate("DEPRESION" = if_else(PB3_16_2 == 1, "SI", "NO"))

graf_dep <- persona_edu |>
  group_by(DEPRESION) |>
  summarise(
    "n" = sum(!is.na(PB3_16_2))
  ) |>
  mutate(
    "ESTIMADOR_P" = n / sum(n)
  ) |> 
  na.omit() |>
  mutate(
    "LIM_INF" = ESTIMADOR_P - qnorm(.95)*sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n),
    "LIM_SUP" = ESTIMADOR_P + qnorm(.95)*sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n)
  )

ggplot(graf_dep, aes(x = DEPRESION, y = ESTIMADOR_P, fill = DEPRESION)) + 
  geom_bar(stat = "identity", position = "dodge", width = .5)+
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("Depresión") + 
  ylab("%") + 
  labs(
    title = "% de jóvenes que presentan depresión por condición académica",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENAPE 2021",
    fill = ""
  ) + 
  scale_fill_manual(values = c("skyblue", "darkblue"))
####
####
##Hasta qué grado estudiaron 
persona_edu <- persona_edu |>
  mutate(
    "ULTIMO_APROBADO" = case_when(
      PC3_3_1 == "00" ~ "NINGUNO",
      PC3_3_1 == "01" ~ "PREESCOLAR",
      PC3_3_1 == "02" ~ "PRIMARIA",
      PC3_3_1 == "03" ~ "SECUNDARIA",
      PC3_3_1 == "04" ~ "TECNICO",
      PC3_3_1 == "05" ~ "PREPARATORIA",
      PC3_3_1 == "06" ~ "BACHILLERATO TECNOLOGICO",
      PC3_3_1 == "07" ~ "TECNICO SUPERIOR",
      PC3_3_1 == "08" ~ "LICENCIATURA",
      PC3_3_1 == "09" ~ "ESPECIALIDAD",
      PC3_3_1 == "10" ~ "MAESTRIA",
      PC3_3_1 == "11" ~ "DOCTORADO"
    ), "RAZON_INT" = case_when(
      PC3_6 == "01" ~ "FALTA DE RECURSOS",
      PC3_6 == "02" ~ "TRABAJO",
      PC3_6 == "03" ~ "FALTA DE INTERES",
      PC3_6 == "04" ~ "REPROBO",
      PC3_6 == "05" ~ "CIERRE DE ESCUELA O ESTABA LEJOS",
      PC3_6 == "06" ~ "NO CUMPLE LOS REQUISITOS",
      PC3_6 == "07" ~ "DISCAPACIDAD FISICA O MENTAL",
      PC3_6 == "08" ~ "ENFERMEDAD O PROBLEMAS DE SALUD FISICA O EMOCIONAL",
      PC3_6 == "09" ~ "SE UNIO, CASO O EMBARAZO",
      PC3_6 == "10" ~ "QUEHACERES DEL HOGAR O CUIDAR A UN FAMILIAR",
      PC3_6 == "11" ~ "FAMILIA O PADRES NO LA DEJARON SEGUIR ESTUDIANDO",
      PC3_6 == "12" ~ "PROBLEMAS PERSONALES",
      PC3_6 == "13" ~ "OTRO",
      PC3_6 == "14" ~ "LOGRO SU META ACADEMICA"
      
    ), "REGRESA" = if_else(PC3_7 == 1, "SI", 
                           if_else(PC3_7 == 2, "NO", "NO SABE"))) 

###NIVEL PROMEDIO Y SI REGRESARIAN

graf_ult <- persona_edu |>
  group_by(ULTIMO_APROBADO) |>
  summarise(
    "n" = sum(!is.na(PC3_3_1)),
    "VOLVERIAN" = sum(PC3_7 == 1, na.rm = TRUE),
    "NO_VOLVERIAN" = sum(PC3_7 == 2, na.rm = TRUE), 
    "NO_SABE" = sum(PC3_7 == 9, na.rm = TRUE),
    "EDAD_PROMEDIO" = mean(as.numeric(EDAD), na.rm = TRUE),
    "EDAD_PROMEDIO_DEJO" = mean(as.numeric(PC3_5), na.rm = TRUE)
  ) |> 
  na.omit() |>
  mutate(
    "EST_VOL" = VOLVERIAN / sum(VOLVERIAN),
    "EST_NVOL" = NO_VOLVERIAN / sum(NO_VOLVERIAN),
    "EST_NS" = NO_SABE / sum(NO_SABE),
    "ESTIMADOR_P_ULTIMO" = n / sum(n)
  ) |>
  select(ULTIMO_APROBADO, n, ESTIMADOR_P_ULTIMO)

ggplot(graf_ult, aes(x = reorder(ULTIMO_APROBADO, + ESTIMADOR_P_ULTIMO), y = ESTIMADOR_P_ULTIMO)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") + 
  geom_text(aes(label = paste0(round(ESTIMADOR_P_ULTIMO*100, 1), "%")), 
            vjust = -0.5, color = "black", size = 3.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5)) +
  xlab("") + 
  ylab("%") + 
  labs(
    title = "Deserciones escolares por nivel académico 
(No inscritos en el periodo 2020-2021)",
    subtitle = "Elaboración propia",
    caption = "Fuente: ENAPE 2021"
  )
####RAZONES

razon_graf <- persona_edu |>
  group_by(RAZON_INT) |>
  summarise(
    "n" = sum(!is.na(RAZON_INT))
  ) |> 
  mutate(
    "ESTIMADOR_P" = n / sum(n), 
    "LIM_INF" = ESTIMADOR_P - qnorm(.95)*sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n),
    "LIM_SUP" = ESTIMADOR_P + qnorm(.95)*sqrt((ESTIMADOR_P*(1-ESTIMADOR_P))/n)
  ) |>
  na.omit()

write.csv(razon_graf, "razon_graf.csv")

ggplot(razon_graf, aes(x = reorder(RAZON_INT, +ESTIMADOR_P), y = ESTIMADOR_P)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") + 
  geom_text(aes(label = paste0(round(ESTIMADOR_P*100, 1), "%")), 
            vjust = -0.1, color = "black", size = 3.5) + 
  coord_flip()+
  labs(
    title = "Motivo de deserción escolar", 
    subtitle = "Elaboración propia",
    caption = "Fuente: ENAPE 2021"
  ) + 
  ylab("Porcentaje %")+
  xlab("") + 
  theme_bw()
  
####
edad_ult <- persona_edu |>
  group_by(ULTIMO_APROBADO) |>
  summarise(
    "n" = sum(!is.na(PC3_3_1)),
    "VOLVERIAN" = sum(PC3_7 == 1, na.rm = TRUE),
    "NO_VOLVERIAN" = sum(PC3_7 == 2, na.rm = TRUE), 
    "NO_SABE" = sum(PC3_7 == 9, na.rm = TRUE),
    "EDAD_PROMEDIO" = mean(as.numeric(EDAD), na.rm = TRUE),
    "SD_EDAD" = sd(as.numeric(EDAD), na.rm = TRUE),
    "EDAD_PROMEDIO_DEJO" = mean(as.numeric(PC3_5), na.rm = TRUE),
    "SD_EDAD_DEJO" = sd(as.numeric(PC3_5), na.rm = TRUE),
    "SP2" = ((n-1)*(SD_EDAD**2) + (n-1)*(SD_EDAD_DEJO**2))/(n+n-2)
  ) |> 
  na.omit() |>
  mutate(
    "EST_VOL" = VOLVERIAN / sum(VOLVERIAN),
    "EST_NVOL" = NO_VOLVERIAN / sum(NO_VOLVERIAN),
    "EST_NS" = NO_SABE / sum(NO_SABE),
    "ESTIMADOR_P_ULTIMO" = n / sum(n)
  ) 

edad_ult |> 
  select(ULTIMO_APROBADO, n, EDAD_PROMEDIO, SD_EDAD, EDAD_PROMEDIO_DEJO, SD_EDAD_DEJO, SP2) |>
  filter(ULTIMO_APROBADO != "NINGUNO") |>
  mutate(
    "DIF_PROM" = EDAD_PROMEDIO - EDAD_PROMEDIO_DEJO
  )

###VUELTA A LA ESCUELA PROMEDIO 
write.csv(persona_edu |>
            group_by(REGRESA) |>
            summarise(
              "n" = sum(!is.na(REGRESA), na.rm = TRUE)
            ) |>
            mutate(
              "PORCENTAJE" = n / sum(n),
              "LIM_INF" = PORCENTAJE - qnorm(.90)*sqrt((PORCENTAJE*(1-PORCENTAJE))/n),
              "LIM_SUP" = PORCENTAJE + qnorm(.90)*sqrt((PORCENTAJE*(1-PORCENTAJE))/n)
            ) |>
            na.omit(), "regreso.csv")


### POR GRUPO DE EDAD

persona_edu <- persona_edu |>
  mutate(
    "GRUPO_EDAD" = case_when(
      EDAD <= 12 ~ "0_12",
      EDAD > 12 & EDAD <= 15 ~ "13_15",
      EDAD > 15 & EDAD <= 19 ~ "15_19",
      EDAD > 19 & EDAD <= 25 ~ "20_25",
      EDAD > 25 & EDAD <= 29 ~ "25_29"
    )
  )

persona_edu |>
  filter(EDAD <= 12)|>
  tally()

edad_vuelta <- persona_edu |>
  group_by(GRUPO_EDAD) |>
  summarise(
    "n" = sum(!is.na(REGRESA)),
    "REGRESARIA" = sum(REGRESA == "SI", na.rm = TRUE), 
    "NO_REGRESARIA" = sum(REGRESA == "NO", na.rm = TRUE),
    "NO_SABE" = sum(REGRESA == "NO SABE", na.rm = TRUE)
  ) |>
  mutate(
    "P_REGRESARIA" = REGRESARIA / n,
    "P_NO_REGRESARIA" = NO_REGRESARIA / n,
    "P_NO_SABE" = NO_SABE / n
  )

write.csv(edad_vuelta, "edad_vuelta.csv")
###COSTO OPORTUNIDAD ESTUDIO

persona_edu <- persona_edu |>
  mutate(
    "CONSECUENCIA" = case_when(
      PD3_3 == 1 ~ "Contratar otra persona en el negocio familiar",
      PD3_3 == 2 ~ "El hogar no podría sostenerse económicamente",
      PD3_3 == 3 ~ "El ingreso del hogar se vería disminuido",
      PD3_3 == 4 ~ "La carga de trabajo sería más pesada para los demás",
      PD3_3 == 5 ~ "El hogar tendría que destinar ingresos para los gastos de ella (él)",
      PD3_3 == 6 ~ "Ella (él) ya no podría continuar estudiando",
      PD3_3 == 7 ~ "Otra consecuencia",
      PD3_3 == 8 ~ "No habría otra consecuencia"
    )
  )

write.csv(persona_edu |>
            filter(REGRESA == "SI" | REGRESA == "NO" | REGRESA == "NO SABE") |>
            group_by(GRUPO_EDAD) |>
            summarise(
              "n" = sum(!is.na(PD3_3)),
              "P1" = sum(PD3_3 == 1, na.rm = TRUE), 
              "P2" = sum(PD3_3 == 2, na.rm = TRUE),
              "P3" = sum(PD3_3 == 3, na.rm = TRUE),
              "P4" = sum(PD3_3 == 4, na.rm = TRUE), 
              "P5" = sum(PD3_3 == 5, na.rm = TRUE),
              "P6" = sum(PD3_3 == 6, na.rm = TRUE),
              "P7" = sum(PD3_3 == 7, na.rm = TRUE), 
              "P8" = sum(PD3_3 == 8, na.rm = TRUE)
            ) |>
            mutate(
              "P_P1" = P1 / n,
              "P_P2" = P2 / n,
              "P_P3" = P3 / n,
              "P_P4" = P4 / n,
              "P_P5" = P5 / n,
              "P_P6" = P6 / n,
              "P_P7" = P7 / n,
              "P_P8" = P8 / n
            ), "cons.csv")