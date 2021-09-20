#################### Circunferência Abdominal ############################
modelo_ind_ca <-
  bayesQR::bayesQR(
    ca ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )


medidas_ind_ca <-
  saidas.matriz(modelo_ind_ca[[1]][["betadraw"]],
                modelo_ind_ca[[1]][["quantile"]],
                modelo_ind_ca[[1]][["names"]])

for (i in 2:19) {
  medidas_ind_ca <-
    rbind(
      medidas_ind_ca,
      saidas.matriz(modelo_ind_ca[[i]][["betadraw"]],
                    modelo_ind_ca[[i]][["quantile"]],
                    modelo_ind_ca[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_ind_ca))) {
  medidas_ind_ca[, i] <- as.numeric(medidas_ind_ca[, i])
}

#################### Circunferência Cefálica #############################
modelo_ind_cc <-
  bayesQR::bayesQR(
    cc ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )


medidas_ind_cc <-
  saidas.matriz(modelo_ind_cc[[1]][["betadraw"]],
                modelo_ind_cc[[1]][["quantile"]],
                modelo_ind_cc[[1]][["names"]])

for (i in 2:19) {
  medidas_ind_cc <-
    rbind(
      medidas_ind_cc,
      saidas.matriz(modelo_ind_cc[[i]][["betadraw"]],
                    modelo_ind_cc[[i]][["quantile"]],
                    modelo_ind_cc[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_ind_cc))) {
  medidas_ind_cc[, i] <- as.numeric(medidas_ind_cc[, i])
}
###################### Diâmetro Biparietal ###############################
modelo_ind_dbp <-
  bayesQR::bayesQR(
    dbp ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20
  )


medidas_ind_dbp <-
  saidas.matriz(modelo_ind_dbp[[1]][["betadraw"]],
                modelo_ind_dbp[[1]][["quantile"]],
                modelo_ind_dbp[[1]][["names"]])

for (i in 2:19) {
  medidas_ind_dbp <-
    rbind(
      medidas_ind_dbp,
      saidas.matriz(modelo_ind_dbp[[i]][["betadraw"]],
                    modelo_ind_dbp[[i]][["quantile"]],
                    modelo_ind_dbp[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_ind_dbp))) {
  medidas_ind_dbp[, i] <- as.numeric(medidas_ind_dbp[, i])
}

################################# Fêmur #################################
modelo_ind_femur <-
  bayesQR::bayesQR(
    femur ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados[is.na(dados$femur) == FALSE, ],
    ndraw = 25000,
    keep = 20
  )


medidas_ind_femur <-
  saidas.matriz(modelo_ind_femur[[1]][["betadraw"]],
                modelo_ind_femur[[1]][["quantile"]],
                modelo_ind_femur[[1]][["names"]])

for (i in 2:19) {
  medidas_ind_femur <-
    rbind(
      medidas_ind_femur,
      saidas.matriz(modelo_ind_femur[[i]][["betadraw"]],
                    modelo_ind_femur[[i]][["quantile"]],
                    modelo_ind_femur[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_ind_femur))) {
  medidas_ind_femur[, i] <- as.numeric(medidas_ind_femur[, i])
}
