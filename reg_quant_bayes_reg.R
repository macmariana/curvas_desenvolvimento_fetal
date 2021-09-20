#################### Circunferência Abdominal ############################
modelo_lasso_ca <-
  bayesQR::bayesQR(
    ca ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20,
    alasso = TRUE
  )


medidas_lasso_ca <-
  saidas.matriz(modelo_lasso_ca[[1]][["betadraw"]],
                modelo_lasso_ca[[1]][["quantile"]],
                modelo_lasso_ca[[1]][["names"]])

for (i in 2:19) {
  medidas_lasso_ca <-
    rbind(
      medidas_lasso_ca,
      saidas.matriz(modelo_lasso_ca[[i]][["betadraw"]],
                    modelo_lasso_ca[[i]][["quantile"]],
                    modelo_lasso_ca[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_lasso_ca))) {
  medidas_lasso_ca[, i] <- as.numeric(medidas_lasso_ca[, i])
}

#################### Circunferência Cefálica #############################
modelo_lasso_cc <-
  bayesQR::bayesQR(
    cc ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20,
    alasso = TRUE
  )


medidas_lasso_cc <-
  saidas.matriz(modelo_lasso_cc[[1]][["betadraw"]],
                modelo_lasso_cc[[1]][["quantile"]],
                modelo_lasso_cc[[1]][["names"]])

for (i in 2:19) {
  medidas_lasso_cc <-
    rbind(
      medidas_lasso_cc,
      saidas.matriz(modelo_lasso_cc[[i]][["betadraw"]],
                    modelo_lasso_cc[[i]][["quantile"]],
                    modelo_lasso_cc[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_lasso_cc))) {
  medidas_lasso_cc[, i] <- as.numeric(medidas_lasso_cc[, i])
}
###################### Diâmetro Biparietal ###############################
modelo_lasso_dbp <-
  bayesQR::bayesQR(
    dbp ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados,
    ndraw = 25000,
    keep = 20,
    alasso = TRUE
  )


medidas_lasso_dbp <-
  saidas.matriz(modelo_lasso_dbp[[1]][["betadraw"]],
                modelo_lasso_dbp[[1]][["quantile"]],
                modelo_lasso_dbp[[1]][["names"]])

for (i in 2:19) {
  medidas_lasso_dbp <-
    rbind(
      medidas_lasso_dbp,
      saidas.matriz(modelo_lasso_dbp[[i]][["betadraw"]],
                    modelo_lasso_dbp[[i]][["quantile"]],
                    modelo_lasso_dbp[[i]][["names"]])
    )
}

for (i in c(1, 3:length(medidas_lasso_dbp))) {
  medidas_lasso_dbp[, i] <- as.numeric(medidas_lasso_dbp[, i])
}

################################# Fêmur #################################
modelo_lasso_femur <-
  bayesQR::bayesQR(
    femur ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn,
    quantile = c(1:19 / 20),
    data = dados[is.na(dados$femur) == FALSE, ],
    ndraw = 25000,
    keep = 20,
    alasso = TRUE
  )


medidas_lasso_femur <-
  saidas.matriz(modelo_lasso_femur[[1]][["betadraw"]],
                modelo_lasso_femur[[1]][["quantile"]],
                modelo_lasso_femur[[1]][["names"]])

for (i in 2:19) {
  medidas_lasso_femur <-
    rbind(
      medidas_lasso_femur,
      saidas.matriz(
        modelo_lasso_femur[[i]][["betadraw"]],
        modelo_lasso_femur[[i]][["quantile"]],
        modelo_lasso_femur[[i]][["names"]]
      )
    )
}

for (i in c(1, 3:length(medidas_lasso_femur))) {
  medidas_lasso_femur[, i] <- as.numeric(medidas_lasso_femur[, i])
}
