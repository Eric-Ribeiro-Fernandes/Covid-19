## Blibliotecas
library(tidyverse)
library(lubridate)
library(ROSE)
library(caret)
library(pROC)


#________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

              ## LENDO AS BASES DE DADOS
#________________________________________________________________________________________________________________________________

          ## CSV
            {
            # Casos Suspeitos

              
            df_suspeitos_2020 <- read.delim("./00_data/02_csv/07_Suspeita_de_gripe_2020.csv", sep =   ";")
            df_suspeitos_2021 <- read.delim("./00_data/02_csv/08_Suspeita_de_gripe_2021.csv", sep =   ";")
            df_suspeitos <- rbind(df_suspeitos_2020, df_suspeitos_2021)
            

            # Informações dos hospitais
            rm(df_suspeitos_2020, df_suspeitos_2021)
            gc()

}

#________________________________________________________________________________________________________________________________

          ## Tratando as bases para modelagem preditiva
#________________________________________________________________________________________________________________________________

## Análise dos Casos suspeitos
  {
## Regressão Logística para avaliar quais são os sintomas mais relevantes para a COVID-19
{

  ## Vetor de Sintomas
  sintomas <-  c("FEBRE","TOSSE","GARGANTA","DISPNEIA","DESC_RESP","SATURACAO","DIARREIA","VOMITO","DOR_ABD",
  "FADIGA","PERD_OLFT","PERD_PALA","OUTRO_SIN")
  
  ## Criando tabela de sintomas com codificação 0 e 1
    x <- na_if(df_suspeitos[sintomas], 9)
    for (i in names(x)){
    x[[i]] <-  x[[i]] %>% replace_na(2)}
    
    x[x == 2] <- 0
    
  ## Variável Target
      y <-  df_suspeitos[["CLASSI_FIN"]]
      y <- replace_na(y, 0)
      y[y==1] <- 0
      y[y==2] <- 0
      y[y==3] <- 0
      y[y==4] <- 0
      y[y==6] <- 0
      y[y==7] <- 0
      y[y == 5] <- 1
      x["Target"] <- y
      rm(y)
      
    ## Modelando regressão Logística
        model <- glm(as.factor(Target) ~.
        ,
        data = x, family = binomial(link = "logit") )

      ## Tabela de sintomas
    df_sintomas <-  as.data.frame(sort(exp(model$coefficients)))
    names(df_sintomas) <- "Exponencial do Coeficiente"
    }
##
}


## Modelando regressão logistica para prever infectados por sintomas
{
  ## Data spliting
  set.seed(42)  
  
  r <- createDataPartition(y=x$Target,list = F)
  
  x.train <- x[r,]
  x.test <- x[-r,]
  
  ## Balanceando a Target
  df_over <- ovun.sample(as.factor(Target)~., data = x.train, method = "over", N = 452315*2)$data
  
  model <- glm(Target ~ . , data = df_over , family = "binomial")
  summary(model)
  # Tabela com os coeficientes do modelos
  df_sintomas <-  data.frame(sort(model$coefficients, decreasing = T))
  
  # Estipulando melhor ponto de corte
  preds <- predict(model, newdata = x.test[, -which(names(x)=="Target")])
  

  preds.values <- ifelse(preds >= .05, 1, 0)
  
  # Matriz de confução e parâmetros
  confusion <- table(previsto=preds.values, real=x.test$Target)
  print(confusionMatrix(confusion))
  
  ## Curva ROIC
  plot.roc(roc(x.test$Target,preds.values), legacy.axes = TRUE, print.auc = T, col= "blue")
}


## Evolução dos casos diagnosticados com COVID-19
  {
    df_confirmados_covid <- df_suspeitos %>%
    filter(df_suspeitos$CLASSI_FIN == 5, df_suspeitos$NU_IDADE_N < 110, df_suspeitos$NU_IDADE_N >= 0)
    
    
    df_confirmados_covid["SURTO_SG"]
    ## Target Mortes
    y <- df_confirmados_covid["EVOLUCAO"]
    y[ y == 1] <- 0
    y[ y == 9] <- 0
    y$EVOLUCAO <- y$EVOLUCAO %>% replace_na(0)
    y[ y == 2] <- 1
    y[ y == 3 ] <- 1
    prop.table(table(y))
    

    
    
## Variáveis explicativas
  
    ## Fatores de risco
  fatores_risco <- c("PUERPERA","CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC",
  "PNEUMOPATI","IMUNODEPRE","RENAL","OBESIDADE","OUT_MORBI")
    df_risco <- data.frame(df_confirmados_covid[fatores_risco])
    df_risco[ df_risco == 2] <- 0
    df_risco[ df_risco == 9] <- 0
    for (i in names(df_risco)){
    df_risco[i] <-  df_risco[[i]] %>% replace_na(0)
    }
    
    ## Condição hospitalar
    hospital <- c("HOSPITAL","UTI", "SUPORT_VEN", "RAIOX_RES")
    
    df_hospital <- df_confirmados_covid[hospital]
    df_hospital[ df_hospital == 9] <- 0
    df_hospital[ df_hospital == 6] <- 0
     
    for (i in names(df_hospital)){
      df_hospital[i] <-  df_hospital[[i]] %>% replace_na(0)
    }
    
     ## Transforamndo em factor
    df_hospital$SUPORT_VEN <- as.factor(df_hospital$SUPORT_VEN )
    df_hospital$RAIOX_RES <- as.factor(df_hospital$RAIOX_RES )
    
    
## Condição social
  socio_economico <- c("CS_ESCOL_N", "CS_ZONA" , "HISTO_VGM" , "CS_RACA" , "NU_IDADE_N" )
    df_social <- df_confirmados_covid[socio_economico]

    ## Escolaridade
      df_social$CS_ESCOL_N <-  df_social$CS_ESCOL_N +1
      df_social$CS_ESCOL_N[df_social$CS_ESCOL_N == 10] <- 0
      df_social$CS_ESCOL_N[df_social$CS_ESCOL_N == 6] <- 0
      df_social$CS_ESCOL_N <- df_social$CS_ESCOL_N %>% replace_na(0)
      
    ## Transformando em factor
      df_social$CS_ESCOL_N <- as.factor(df_social$CS_ESCOL_N)

    ## Zona de moradia
      df_social$CS_ZONA[df_social$CS_ZONA == 9] <- 0
      df_social$CS_ZONA <- df_social$CS_ZONA %>% replace_na(0)
      df_social$CS_ZONA <- as.factor(df_social$CS_ZONA)
      
    ## Histórico de viagem pro exterior há 14 dias da consulta
      df_social$HISTO_VGM[df_social$HISTO_VGM == 2] <- 0
      df_social$HISTO_VGM[df_social$HISTO_VGM == 9] <- 0
      df_social$HISTO_VGM <- df_social$HISTO_VGM %>% replace_na(0)
    
  ## Etnia
      df_social$CS_RACA[df_social$CS_RACA == 9] <- 0
      df_social$CS_RACA <- df_social$CS_RACA %>% replace_na(0)
      df_social$CS_RACA <- as.factor(df_social$CS_RACA )
      


  ## Transformando Idade em categoria
    df_social$NU_IDADE_N <- cut(df_confirmados_covid$NU_IDADE_N, breaks = c(0, 15, 30, 45, 60, 75, 120),
                                labels = c("Até 15 anos", "16 até 30 anos", "31 até 45 anos", "46 até 60 anos","61 até 75 anos", "Acima de 76 anos"))
    x <- cbind(df_risco, df_social, df_hospital)
    x["Target"] <- y
}    
     
  
     ## Modelando regressão logistica para prever mortes
    {
   ## Data spliting
    set.seed(42)  
  
    r <- createDataPartition(y=x$Target,list = F)
    
      x.train <- x[r,]
      x.test <- x[-r,]
      
     ## Balanceando a Target
      df_over <- ovun.sample(Target~., data = x.train, method = "over", N = 311707*2)$data

    model <- glm(Target ~ . , data = df_over , family = "binomial")
    summary(model)
    # Tabela com os coeficientes do modelos
    df_causa_morte <-  data.frame(sort(exp(model$coefficients), decreasing = T))
    names(df_causa_morte) <- "Exponencial do Coeficiente"
    # Estipulando melhor ponto de corte
    preds <- predict(model, newdata = x.test[, -which(names(x)=="Target")])
    
 
    preds.values <- ifelse(preds >= .15, 1, 0)
    
    # Matriz de confução e parâmetros
    confusion <- table(previsto=preds.values, real=x.test$Target)
    print(confusionMatrix(confusion))
    
    ## Curva ROIC
    plot.roc(roc(x.test$Target,preds.values), legacy.axes = TRUE, print.auc = T, col= "blue")
    }

#________________________________________________________________________________________________________________________________

## Criando Dataset para o PowerBi
#________________________________________________________________________________________________________________________________
    {

  ## Intervalos de tempo
      
    ## Convertendo Datas
      primeiros_sintomas <- as.Date(strptime(df_confirmados_covid$DT_SIN_PRI, "%d/%m/%Y"))
      inicio_tratamento <- as.Date(strptime(df_confirmados_covid$DT_ANTIVIR, "%d/%m/%Y"))
      internacao <-  as.Date(strptime(df_confirmados_covid$DT_INTERNA, "%d/%m/%Y"))
      entrada_uti <- as.Date(strptime(df_confirmados_covid$DT_ENTUTI, "%d/%m/%Y"))
      saida_uti <- as.Date(strptime(df_confirmados_covid$DT_SAIDUTI, "%d/%m/%Y"))
      alta_obito <- as.Date(strptime(df_confirmados_covid$DT_EVOLUCA, "%d/%m/%Y"))
      encerramento <- as.Date(strptime(df_confirmados_covid$DT_ENCERRA, "%d/%m/%Y"))

  ## Calculando intervalos em dias
      
      # Dias internado na Uti
      dias <- as.integer(difftime(saida_uti, entrada_uti, units = "d" ))
      x["Dias na UTI"] <- dias

      # Dias dos primeiros sintomas até o início do tratamento
      dias <- as.integer(difftime( inicio_tratamento,primeiros_sintomas, units = "d" ))
      x["Dias dos sintomas ao tratamento"] <- dias
      

      # Dias internado
      dias <- as.integer(difftime(alta_obito, internacao, units = "d"))
      x["Dias internado"] <- dias
      
      # Dias em observação
      dias <- as.integer(difftime( encerramento,  inicio_tratamento, units = "d"))
      x["Dias em observação"] <- dias
      
      x["Sexo"]<- df_confirmados_covid$CS_SEXO

   x <-   x %>% filter((`Dias na UTI` >=0 |`Dias na UTI` == is.na(`Dias na UTI`)) & 
                         (`Dias dos sintomas ao tratamento` >= 0 | is.na(`Dias dos sintomas ao tratamento`))  &
                    (`Dias internado` >=0 | is.na(`Dias internado`)) & 
                      (`Dias em observação` >=0 | is.na(`Dias em observação`)))
   


   }