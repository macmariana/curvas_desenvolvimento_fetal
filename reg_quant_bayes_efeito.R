#################### Circunferência Abdominal ############################
modelo_efeito_ca <- lapply(1:9, function(a) {
  R2BayesX::bayesx(
    ca ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn +
      sx(fmatricula, bs = "re"),
    data = dados,
    iter = 110000,
    burnin = 10000,
    step = 100,
    method = "MCMC",
    family = "quantreg",
    quantile = a / 10,
    outfile = paste0(
      "~/curvas_fetal/efeito_aleatorio_ca/q",
      a,
      "/"
    ),
    dir.rm = FALSE
  )
})

path_folders <-
  '~/curvas_fetal/efeito_aleatorio_ca/'

listFolders <- list.files(path_folders)

check_na() ### caso algum retorne TRUE, gerar novos valores para esse quantil

### armazenando medidas da posteriori
estimativas_ca <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects1.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})
estimativas_ca2 <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects2.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})

estimativas_todas_ca <- do.call(rbind.data.frame, estimativas_ca)
estimativas_todas_ca <- do.call(rbind.data.frame, estimativas_ca2)

numero_var <- 10
numero_var2 <- 5
quantis_estimados <- 1:9 / 10

estimativas_todas_ca <- estimativas_todas_ca %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var))
estimativas_todas_ca <- estimativas_todas_ca %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var2))

#################### Circunferência Cefálica ############################
modelo_efeito_cc <- lapply(1:9, function(a) {
  R2BayesX::bayesx(
    cc ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn +
      sx(fmatricula, bs = "re"),
    data = dados,
    iter = 110000,
    burnin = 10000,
    step = 100,
    method = "MCMC",
    family = "quantreg",
    quantile = a / 10,
    outfile = paste0(
      "~/curvas_fetal/efeito_aleatorio_cc/q",
      a,
      "/"
    ),
    dir.rm = FALSE
  )
})

path_folders <-
  '~/curvas_fetal/efeito_aleatorio_cc/'

listFolders <- list.files(path_folders)

check_na() ### caso algum retorne TRUE, gerar novos valores para esse quantil

estimativas_cc <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects1.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})
estimativas_cc2 <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects2.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})

estimativas_todas_cc <- do.call(rbind.data.frame, estimativas_cc)
estimativas_todas_cc <- do.call(rbind.data.frame, estimativas_cc2)

estimativas_todas_cc <- estimativas_todas_cc %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var))
estimativas_todas_cc <- estimativas_todas_cc %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var2))

###################### Diâmtero Biparietal ##############################
modelo_efeito_dbp <- lapply(1:9, function(a) {
  R2BayesX::bayesx(
    dbp ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn +
      sx(fmatricula, bs = "re"),
    data = dados,
    iter = 110000,
    burnin = 10000,
    step = 100,
    method = "MCMC",
    family = "quantreg",
    quantile = a / 10,
    outfile = paste0(
      "~/curvas_fetal/efeito_aleatorio_dbp/q",
      a,
      "/"
    ),
    dir.rm = FALSE
  )
})

path_folders <-
  '~/curvas_fetal/efeito_aleatorio_dbp/'

listFolders <- list.files(path_folders)

check_na() ### caso algum retorne TRUE, gerar novos valores para esse quantil

estimativas_dbp <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects1.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})
estimativas_dbp2 <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects2.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})

estimativas_todas_dbp <- do.call(rbind.data.frame, estimativas_dbp)
estimativas_todas_dbp <- do.call(rbind.data.frame, estimativas_dbp2)

estimativas_todas_dbp <- estimativas_todas_dbp %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var))
estimativas_todas_dbp <- estimativas_todas_dbp %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var2))

################################# Fêmur #################################
modelo_efeito_femur <- lapply(1:9, function(a) {
  R2BayesX::bayesx(
    femur ~ igaval + sexo + imc_cat + idade_cat + brancasn + escolaridade2 +
      ngestacoes_cat + antecedentedecesarea_cat + trabalha_sn + companheiro_sn +
      sx(fmatricula, bs = "re"),
    data = dados[is.na(dados$femur) == FALSE, ],
    iter = 110000,
    burnin = 10000,
    step = 100,
    method = "MCMC",
    family = "quantreg",
    quantile = a / 10,
    outfile = paste0(
      "~/curvas_fetal/efeito_aleatorio_dbp/q",
      a,
      "/"
    ),
    dir.rm = FALSE
  )
})

path_folders <-
  '~/curvas_fetal/efeito_aleatorio_femur/'

listFolders <- list.files(path_folders)

check_na() ### caso algum retorne TRUE, gerar novos valores para esse quantil

estimativas_femur <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects1.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})
estimativas_femur2 <- lapply(listFolders, function(a) {
  namefile1 <- paste0(path_folders, a,
                      '/bayesx.estim_FixedEffects2.res')
  
  datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                   silent = TRUE)
  
  var <- datatemp1$varname
  mediana <- datatemp1$pmed
  media <- datatemp1$pmean
  q_inferior <- datatemp1$pqu2p5
  q_superior <- datatemp1$pqu97p5
  
  data.frame(var,
             mediana,
             media,
             q_inferior,
             q_superior)
})

estimativas_todas_femur <-
  do.call(rbind.data.frame, estimativas_femur)
estimativas_todas_femur <-
  do.call(rbind.data.frame, estimativas_femur2)

estimativas_todas_femur <- estimativas_todas_femur %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var))
estimativas_todas_femur <- estimativas_todas_femur %>%
  mutate(quantis = rep(quantis_estimados, each = numero_var2))
