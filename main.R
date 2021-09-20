####################### carregando pacotes e dados #######################
loadlibrary <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = T)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}
packages <-
  c(
    "readxl",
    "janitor",
    "dplyr",
    "tidyr",
    "skimr",
    "forcats",
    "stringr",
    "lubridate",
    "readr",
    "ggplot2",
    "summarytools",
    "TeachingDemos",
    "bayesQR",
    "R2BayesX",
    "LaplacesDemon"
  )
lapply(packages, loadlibrary)

dados_completos <- read_csv("dados_completos.csv",
                            locale = locale(decimal_mark = ","))

##### retirando observações com idade gestacional maior que no parto ######
erros <- read_csv("erros.csv")
dados <-
  anti_join(dados_completos, erros, c("Matricula", "Num_avals"))
dados <- clean_names(dados)

colnames(dados)[4] <- "igaval"
colnames(dados)[35] <- "sexo"
dados$fmatricula <- as.numeric(as.factor(dados$matricula))
############################## categorização ##############################
dados$imc_cat <- ifelse(dados$imc < 18.5,
                        "subpeso",
                        ifelse(
                          dados$imc < 25,
                          "normal",
                          ifelse(dados$imc < 30, "sobrepeso",
                                 "obesidade")
                        ))

dados$idade_cat <- ifelse(dados$idadeanos < 20,
                          "-19",
                          ifelse(dados$idadeanos < 35, "20-34",
                                 "35+"))

dados$brancasn <- ifelse(dados$cor == "b", "1", "0")

dados$escolaridade2 <- ifelse(dados$escolaridade != "pos",
                              dados$escolaridade, "s")

dados$ngestacoes_cat <- ifelse(dados$nºgestacoes > 1 , "1+", "1")

dados$antecedentedecesarea_cat <-
  ifelse(dados$antecedentedecesareaanterior > 0, "1+", "0")

###########################################################################
saveRDS(dados, "dados.rds")

############### função para armazenar medidas da posteriori ##############
saidas.matriz <- function(cadeia, quantil, nome) {
  out <- cbind(quantil,
               nome,
               t(apply(cadeia, 2, summary)),
               as.matrix(apply(cadeia, 2, sd)),
               t(apply(cadeia, 2, function(x)
                 quantile(x, probs = c(0.025, 0.975)))),
               t(apply(cadeia, 2, function(x)
                 emp.hpd(x, conf = 0.95))))
  out <- as.data.frame(out)
  colnames(out) <- c(
    "quantil",
    "var",
    "min",
    "1qt",
    "med",
    "media",
    "3qt",
    "max",
    "dp",
    "emp_2.5",
    "emp_97.5",
    "HPD_2.5",
    "HPD_97.5"
  )
  return(out)
}

##### checando se existe NA na distribuição gerada para cada quantil #####
check_na <- function() {
  sapply(listFolders, function(a) {
    print(paste0("Pasta = ", a))
    
    namefile1 <- paste0(path_folders, a,
                        '/bayesx.estim_FixedEffects1.res')
    namefile2 <- paste0(path_folders, a,
                        '/bayesx.estim_f_fmatricula_random.res')
    
    datatemp1 <- try(utils::read.table(namefile1, head = TRUE),
                     silent = TRUE)
    datatemp2 <- try(utils::read.table(namefile2, head = TRUE),
                     silent = TRUE)
    
    if (class(datatemp1) != 'try-error') {
      check1 <- any(is.na(datatemp1$pmean))
    }
    else
      check1 <- TRUE
    
    if (class(datatemp2) != 'try-error') {
      check2 <- any(is.na(datatemp2$pmean))
    }
    else
      check2 <- TRUE
    
    print(paste0("check1 = ", check1))
    
    print(paste0("check2 = ", check1))
    
    check1 | check2
    
  }) %>% as.logical()
}