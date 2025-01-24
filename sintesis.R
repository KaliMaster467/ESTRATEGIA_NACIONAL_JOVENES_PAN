######PERCEPCION DE SERVICIOS PUBLICOS

conf_serv <- read_csv("encig2023_01_sec1_A_3_4_5_8_9_10.csv")
resid_serv <- read_csv("encig2023_02_residentes_sec_2.csv")

conf_serv <- conf_serv |>
  right_join(resid_serv |> 
               select(ID_PER, EDAD), by = c("ID_PER"))

conf_serv <- conf_serv |>
  filter(EDAD <= 29) |>
  select(CVE_ENT, NOM_ENT, EDAD, P3_1_01, P3_1_02, P3_1_03,
         P3_1_04, P3_1_05, P3_1_06, P3_1_07, P3_1_08, P3_1_09, P3_1_10,
         P3_1_11, P3_1_99)
conf_serv_pivot <- conf_serv |>
  pivot_longer(cols = c(P3_1_01, P3_1_02, P3_1_03,
                        P3_1_04, P3_1_05, P3_1_06, P3_1_07, P3_1_08, P3_1_09, P3_1_10,
                        P3_1_11, P3_1_99), 
               names_to = "PROBLEMAS_IMP", 
               values_to = "SI_NO")

 
conf_serv |>
  group_by(P3_1_99) |>
  summarise(
    "N" = n()
  ) |>
  na.omit() |>
  mutate(
    "P" = N / sum(N)
  )

conf_serv <- conf_serv |>
  mutate("ent" = as.numeric(CVE_ENT))



###MODELO ECONOMETRICO
edos_df <- read_csv("DIP_FED_2024.csv")

edos_df <- edos_df |>
  group_by(ID_ENTIDAD, ENTIDAD) |>
  summarise(
    "VOTO_PAN" = sum(PAN, na.rm = TRUE),
    "VOTACION_CALCULADA" = sum(TOTAL_VOTOS_CALCULADOS, na.rm = TRUE),
    "LISTA_NOMINAL" = sum(LISTA_NOMINAL, na.rm = TRUE),
    ""
  ) |>
  mutate(
    "id_ent" = as.numeric(ID_ENTIDAD)
  )

regresion <- edos_df |>
  right_join(salario |>
  filter(PERSONA == "JOVEN") |>
  group_by(ent) |>
  summarise(
    "SALARIO_PROMEDIO_JOVEN" = mean(p6b2, na.rm = TRUE)
  ), by = c("id_ent" = "ent"))

##AGREGHAMOS RENTA PROMEDIO 

regresion <- regresion |>
  right_join(hacin |>
               group_by(ENTIDAD) |>
               summarise(
                 "RENTA_PROMEDIO" = mean(pago_renta, na.rm = TRUE)
               ), by = c("id_ent" ="ENTIDAD"))

###AGREGAMOS MODA ESTUDIOS ULTIMO GRADO DE ESTUDIOS

moda <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}


regresion <- regresion |>
  right_join(socioc |> 
               group_by(ent) |>
               summarise(
                 "MODA_GRADO_ESTUDIO" = moda(cs_p13_1)
               ) |>
               mutate("GRADO_ESTUDIO" = if_else(MODA_GRADO_ESTUDIO == 0, "NINGUNO",
                                                if_else(MODA_GRADO_ESTUDIO == 1, "PREESCOLAR",
                                                        if_else(MODA_GRADO_ESTUDIO == 2, "PRIMARIA",
                                                                if_else(MODA_GRADO_ESTUDIO == 3, "SECUNDARIA", 
                                                                        if_else(MODA_GRADO_ESTUDIO == 4, "PREPARATORIA",
                                                                                if_else(MODA_GRADO_ESTUDIO == 5, "NORMAL",
                                                                                        if_else(MODA_GRADO_ESTUDIO == 6, "TECNICA",
                                                                                                if_else(MODA_GRADO_ESTUDIO == 7, "PROFESIONAL",
                                                                                                        if_else(MODA_GRADO_ESTUDIO == 8, "MAESTRIA",
                                                                                                                if_else(MODA_GRADO_ESTUDIO == 9, "DOCTORADO", "NO SABE"))))))))))),
             by = c("id_ent" = "ent"))
###JOVENES

regresion <- regresion |>
  right_join(LN_2024 |>
               group_by(CLAVE_ENTIDAD) |>
               summarise(
                 "TOTAL_JOVENES" = sum(TOTAL_JOVENES)
               ), by = c("id_ent" = "CLAVE_ENTIDAD"))

regresion <- regresion |>
  mutate(
    "PORCENTAJE_JOVENES_LN" = TOTAL_JOVENES / LISTA_NOMINAL,
    "PORCENTAJE_VOTO_PAN" = VOTO_PAN / VOTACION_CALCULADA,
    "PORCENTAJE_PARTICIPACION" = VOTACION_CALCULADA / LISTA_NOMINAL
  )


###### DESERCION ESCOLAR

regresion <- regresion |>
  right_join(persona_edu |>
               filter(EDAD >= 15) |>
               group_by(ent) |>
               summarise(
                 "N_DESERCION" = sum(!is.na(PA3_5))
               ), by = c("id_ent" = "ent"))


regresion <- regresion |>
  right_join(persona_edu |>
               filter(EDAD >= 15) |>
               group_by(ent) |>
               summarise(
                 "N_DEPRESION" = sum(PB3_16_2 == 1, na.rm = TRUE)
               ), by = c("id_ent" = "ent"))

##trabajo jovenes
empleo_jov <- read_csv("ENOE_COE1T124.csv")

empleo_jov <- empleo_jov |>
  filter(eda >= 18) |>
  group_by(ent) |>
  summarise(
    "JOVENES_TRABAJANDO" = sum(p2h1 == 1 | p2h2 == 2 | p2h3 == 3, na.rm = TRUE),
    "JOVENES_NO_TRABAJANDO" = sum(p2h4 == 4, na.rm = TRUE)
  )

regresion <- regresion |>
  right_join(empleo_jov, by = c("id_ent" = "ent"))
### DEPORTE

deporte_joven <- read_csv("conjunto_de_datos_mopradef_2023_11.csv")


regresion <- regresion |>
  right_join(deporte_joven |>
               filter(edad >= 17 & edad <= 28) |>
               mutate("ent" = as.numeric(entidad)) |>
               group_by(ent) |>
               summarise(
                 "N" = n(),
                 "N_DEPORTE" = sum(p1 == 1, na.rm = TRUE)
               ) |> 
               mutate(
                 "P" = N_DEPORTE/N
               ), by = c("id_ent" = "ent"))


###GOBIERNO PROBLEMAS CORRUPCION INSEGURIDAD

regresion <- regresion |>
  right_join(conf_serv |>
               group_by(ent) |>
               summarise(
                 "CORRUPCION" = sum(P3_1_03 == 1, na.rm = TRUE),
                 "INSEGURIDAD" = sum(P3_1_05 == 1, na.rm = TRUE),
                 "MGOB" = sum(P3_1_01 == 1, na.rm = TRUE)
               ), by = c("id_ent" = "ent"))


####

regs <- lm(regresion$VOTO_PAN ~ regresion$VOTACION_CALCULADA  + 
     regresion$RENTA_PROMEDIO + regresion$N_DESERCION  + regresion$JOVENES_TRABAJANDO)



summary(regs)
stargazer(regs, type = "text")
boxplot(regs[['residuals']],main='Boxplot: Residuals',ylab='residual value')

par(mfrow=c(2,2))
plot(regs)
par(mfrow=c(1,1))

eda(regs[['residuals']])

jarque.bera.test(regs[['residuals']])
stargazer(regs, type = "text")
bptest(regs)
plot(regs[['residuals']])


fitted_values <- fitted(regs)
residuals <- rstandard(regs)

# Crear el gráfico
plot(fitted_values, residuals,
     xlab = "Valores Ajustados",
     ylab = "Residuos Estandarizados",
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lwd = 2)  # Línea horizontal en y = 0

variables <- regs$model
cor_matrix <- cor(variables)
corrplot(cor_matrix, method = "number", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Añadir coeficientes de correlación en el gráfico
         title = "Correlation Plot de las Variables del Modelo")

vif(regs)
####
