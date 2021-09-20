dados <- readRDS("dados.rds")

#################### Circunferência Abdominal ############################
modelo_ca_final <-
  bayesQR::bayesQR(
    ca ~ igaval + sexo + imc_cat + idade_cat + brancasn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )

medidas_ca_final <-
  saidas.matriz(modelo_ca_final[[1]][["betadraw"]],
                modelo_ca_final[[1]][["quantile"]],
                modelo_ca_final[[1]][["names"]])

for (i in 2:19) {
  medidas_ca_final <-
    rbind(
      medidas_ca_final,
      saidas.matriz(modelo_ca_final[[i]][["betadraw"]],
                    modelo_ca_final[[i]][["quantile"]],
                    modelo_ca_final[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_ca_final))) {
  medidas_ca_final[, i] <- as.numeric(medidas_ca_final[, i])
}

medidas_ca_final$medida <- "ca"

saveRDS(medidas_ca_final, "medidas_ca.rds")

#################### Circunferência Cefálica #############################
modelo_cc_final <-
  bayesQR::bayesQR(
    cc ~ igaval + sexo + imc_cat + brancasn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )

medidas_cc_final <-
  saidas.matriz(modelo_cc_final[[1]][["betadraw"]],
                modelo_cc_final[[1]][["quantile"]],
                modelo_cc_final[[1]][["names"]])

for (i in 2:19) {
  medidas_cc_final <-
    rbind(
      medidas_cc_final,
      saidas.matriz(modelo_cc_final[[i]][["betadraw"]],
                    modelo_cc_final[[i]][["quantile"]],
                    modelo_cc_final[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_cc_final))) {
  medidas_cc_final[, i] <- as.numeric(medidas_cc_final[, i])
}

medidas_cc_final$medida <- "cc"

saveRDS(medidas_cc_final, "medidas_cc.rds")

###################### Diâmetro Biparietal ###############################
modelo_dbp_final <-
  bayesQR::bayesQR(
    dbp ~ igaval + sexo,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )

medidas_dbp_final <-
  saidas.matriz(modelo_dbp_final[[1]][["betadraw"]],
                modelo_dbp_final[[1]][["quantile"]],
                modelo_dbp_final[[1]][["names"]])

for (i in 2:19) {
  medidas_dbp_final <-
    rbind(
      medidas_dbp_final,
      saidas.matriz(modelo_dbp_final[[i]][["betadraw"]],
                    modelo_dbp_final[[i]][["quantile"]],
                    modelo_dbp_final[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_dbp_final))) {
  medidas_dbp_final[, i] <- as.numeric(medidas_dbp_final[, i])
}

medidas_dbp_final$medida <- "dbp"

saveRDS(medidas_dbp_final, "medidas_dbp.rds")

################################# Fêmur #################################
modelo_femur_final <-
  bayesQR::bayesQR(
    femur ~ igaval,
    quantile = c(1:19 / 20),
    data = dados[is.na(dadoss$femur) == FALSE,],
    ndraw = 25000,
    keep = 20
  )

medidas_femur_final <-
  saidas.matriz(modelo_femur_final[[1]][["betadraw"]],
                modelo_femur_final[[1]][["quantile"]],
                modelo_femur_final[[1]][["names"]])

for (i in 2:19) {
  medidas_femur_final <-
    rbind(
      medidas_femur_final,
      saidas.matriz(
        modelo_femur_final[[i]][["betadraw"]],
        modelo_femur_final[[i]][["quantile"]],
        modelo_femur_final[[i]][["names"]]
      )
    )
}

for (i in c(1, 3:length(medidas_femur_final))) {
  medidas_femur_final[, i] <- as.numeric(medidas_femur_final[, i])
}

medidas_femur_final$medida <- "femur"

saveRDS(medidas_femur_final, "medidas_femur.rds")

medidas_final <- do.call(
  "rbind",
  list(
    medidas_ca_final,
    medidas_cc_final,
    medidas_dbp_final,
    medidas_femur_final
  )
)

saveRDS(medidas_final, "medidas_final.rds")