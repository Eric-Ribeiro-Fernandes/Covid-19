## Blibliotecas
library(tidyverse)
library(lubridate)
library(coronabr)
library(rnaturalearth)
library(rnaturalearthhires)
library(geobr)
library(animation)


# _________________________________________________________________________________________________________  

          ## COLETANDO DADOS ##
# _________________________________________________________________________________________________________    


reticulate::use_condaenv("ds_basic")
setwd("/home/eric/Documentos/Data Science/06_Projetos/Vacinação da COVID no Brasil")

## Lendo Dados
{
   ## Lendo csv's
   # df_vacinas <- read_csv("02_vacinação_clean.csv")
  latitude_e_longitude_dos_municípios <- read_csv("00_data/02_csv/latitude e longitude dos municípios.csv")

  ## Lendo pickles
    df_primeira_municipios <- reticulate::py_load_object("00_data/00_raw/df_vacinacao_1.pickle", pickle ='pickle 5')
    df_segunda_municipios <- reticulate::py_load_object("00_data/00_raw/df_vacinacao_2.pickle")

  ## Lendo API coronabr última Data
    df_coronabr <- get_corona_br() %>% filter(date == Sys.Date()-1)
    df_coronabr_por_dia <- get_corona_br() %>% filter(date > '2020-04-30')
    
}    

## Fazendo Merge nas bases
  ## Primeira Dose
  df_merge_primeira_dose  <- merge.data.frame(df_primeira_municipios, df_coronabr, by.x = "id", by.y = "city_ibge_code" )
  df_merge_primeira_dose <- df_merge_primeira_dose[df_merge_primeira_dose$lat < 20,]
  
  ## Segunda Dose
  df_merge_segunda_dose  <- merge.data.frame(df_segunda_municipios, df_coronabr, by.x = "id", by.y = "city_ibge_code" )
  df_merge_segunda_dose <- df_merge_segunda_dose[df_merge_segunda_dose$lat < 20,]  
  

## Calculando estatísticas pertinentes
  
  ## Criando colunas
  df_merge_primeira_dose["Porcentagem da pop. Vacinada"] <- round((df_merge_primeira_dose$`Vacinas Aplicadas` / df_merge_primeira_dose$estimated_population) *100)
  df_merge_primeira_dose["Grupo de porcentagem"] <- cut(df_merge_primeira_dose[["Porcentagem da pop. Vacinada"]],4, labels = c("0% até 10%", "11% até 20%", "21% até 30%", "Acima de 31%"),breaks = c(0,10,20,30,100) )

  ## Tabelas de frequência
  table(df_merge_primeira_dose$`Grupo de porcentagem`)
  
## Retirando anomalias
  df_merge_primeira_dose <- df_merge_primeira_dose %>% filter(`Porcentagem da pop. Vacinada` >0)
  
  ## Baixar arquivos de Mapa
  
  ## Brasil
  BR <- ne_states(
    country = "Brazil",
    returnclass = "sf"
  )
  municipios <- geobr::read_municipality(year = 2018)
  regiao <- read_region()
  ## Regiões 
  ## sul (42,41,43)
  sul <- rbind(read_municipality(code_muni = 41, year = 2018, simplified = T),
               read_municipality(code_muni = 42, year = 2018, simplified = T),
               read_municipality(code_muni = 43, year = 2018, simplified = T))
    
  ## centro-oeste (53, 52, 50, 51)
  centro_oeste <- rbind(read_municipality(code_muni = 50, year = 2018, simplified = F),
                        read_municipality(code_muni = 51, year = 2018, simplified = F),
                        read_municipality(code_muni = 52, year = 2018, simplified = F),
                        read_municipality(code_muni = 53, year = 2018, simplified = F))
  
  ## Sudeste (31, 32, 33, 35)
  sudeste <- rbind(read_municipality(code_muni = 31, year = 2010, simplified = F),
                   read_municipality(code_muni = 32, year = 2010, simplified = F),
                   read_municipality(code_muni = 33, year = 2010, simplified = F),
                   read_municipality(code_muni = 35, year = 2010, simplified = F))
  
  ## nordeste (27, 29, 23, 21, 25, 26, 22, 24, 28)
  nordeste <- rbind(read_municipality(code_muni = 27, year = 2010, simplified = F),
                    read_municipality(code_muni = 29, year = 2010, simplified = F),
                    read_municipality(code_muni = 23, year = 2010, simplified = F),
                    read_municipality(code_muni = 21, year = 2010, simplified = F),
                    read_municipality(code_muni = 25, year = 2010, simplified = F),
                    read_municipality(code_muni = 26, year = 2010, simplified = F),
                    read_municipality(code_muni = 22, year = 2010, simplified = F),
                    read_municipality(code_muni = 24, year = 2010, simplified = F),
                    read_municipality(code_muni = 28, year = 2010, simplified = F))
 
  ## norte (12, 16, 13, 15, 11, 14, 17)
  norte <- rbind(read_municipality(code_muni = 12, year = 2010, simplified = F),
                 read_municipality(code_muni = 16, year = 2010, simplified = F),
                 read_municipality(code_muni = 13, year = 2010, simplified = F),
                 read_municipality(code_muni = 15, year = 2010, simplified = F),
                 read_municipality(code_muni = 11, year = 2010, simplified = F),
                 read_municipality(code_muni = 14, year = 2010, simplified = F),
                 read_municipality(code_muni = 17, year = 2010, simplified = F))
  
   
  
  ## Dados geográficos dos municípios com as estatísticas
  
    df_merge_municipios_primeira <- right_join(df_merge_primeira_dose, municipios, by= c("id" = "code_muni"))
    df_merge_municipios_segunda <- right_join(df_merge_segunda_dose, municipios, by= c("id" = "code_muni"))
    
  ## Intervalo em semanas
  semanas <- sort(unique(df_coronabr_por_dia$epidemiological_week))
  
    
    ## Garbage collector
    rm(df_merge_primeira_dose,df_merge_segunda_dose, df_primeira_municipios, df_segunda_municipios)
    gc()
  

  # _________________________________________________________________________________________________________    
            
            ## PLOTANDO MAPAS ITERATIVOS ##
  # _________________________________________________________________________________________________________    
    
    
    
    
  ## CRIANDO GIFS
    
    ## País
    
      # Contágios
        saveGIF( movie.name = "./02_plot/contágio.gif",
                 interval =.6,
                 convert="convert",
                 {
            for (i in 1:length(semanas)){
              
              print(i)
              
              df_1 <- df_coronabr_por_dia %>% 
                select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                group_by(city_ibge_code) %>% 
                summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
              
              df <- left_join(municipios, df_1, by= c("code_muni" = "city_ibge_code" ))
              df$date <- df_1$date[1]
              
              df[is.na(df)] <- 1
              
              plot.casos  <- ggplot()+
                geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                geom_sf(data = BR, fill = NA, color = "gray") +
                scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange"))+
                theme_void() +
                labs(subtitle = "Andamento do contágio", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
              
              print(plot.casos)
            }       
    }
                )
      
      # Mortes
        saveGIF( movie.name = "./02_plot/mortes.gif",
                 interval =.6,
                 convert="convert",
                 {
                   
                   for (i in 1:length(semanas)){
                     
                     print(i)
                     
                     df_1 <- df_coronabr_por_dia %>% 
                       select(c(city_ibge_code, last_available_death_rate, date))%>% 
                       filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                       group_by(city_ibge_code) %>% 
                       summarise(last_available_death_rate = (mean(last_available_death_rate)*1000)+1, date = max(date))
                     
                     df <- left_join(municipios, df_1, by= c("code_muni" = "city_ibge_code" ))
                     df$date <- df_1$date[1]
                     
                     df[is.na(df)] <- 1
                     
                     plot.mortes  <- ggplot()+
                       geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate))) +
                       geom_sf(data = BR, fill = NA, color = "gray") +
                       scale_fill_gradientn(limits = c(0,6),colours = c("white", "red"))+
                       theme_void() +
                       labs(subtitle = "Andamento das mortes", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                     
                     print(plot.mortes)
                     
                   }       
           }
               )

    ## Regiões
        
      # Nordeste 
        # Contágio 
        saveGIF( movie.name = "./02_plot/contágio_nordeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                     print(i)
                    df_1 <- df_coronabr_por_dia %>% 
                            select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                            filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                            group_by(city_ibge_code) %>% 
                            summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
                    
                    df <- left_join(nordeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                      df$date <- df_1$date[1]
                    
                    df[is.na(df)] <- 1
                    
                    capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 2111300 | id_cidade == 2211001 | id_cidade == 2304400 |
                                                                                 id_cidade == 2408102 | id_cidade == 2507507 | id_cidade == 2611606 |
                                                                                 id_cidade == 2704302 | id_cidade == 2800308 | id_cidade == 2927408)
                    capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                    capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_confirmed_per_100k_inhabitants, lat, lon)
                     
                   plot.nordeste <-  ggplot()+
                                       geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                                      geom_sf(data = BR, fill = NA, color = "black") +
                                      geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_confirmed_per_100k_inhabitants))) +
                                      geom_text(data = capitais,aes(label = name_muni, x = lon+ .75, y= lat +.6)) +
                                      xlim(-48, -32.5) + ylim(-18,-1) + 
                                      scale_size( limits  = c(0,10), name = "") +
                                      scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange")) + 
                                      theme_void() +
                                      labs(subtitle = "CONTÁGIO NO NORDESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                                      
                   print(plot.nordeste)}
        }
        )
        
        # Mortes
        saveGIF( movie.name = "./02_plot/mortes_nordeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_death_rate, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_death_rate = mean(last_available_death_rate)*1000+1, date = max(date))
                   
                   df <- left_join(nordeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 2111300 | id_cidade == 2211001 | id_cidade == 2304400 |
                                                                                id_cidade == 2408102 | id_cidade == 2507507 | id_cidade == 2611606 |
                                                                                id_cidade == 2704302 | id_cidade == 2800308 | id_cidade == 2927408)
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_death_rate, lat, lon)
                   
                   plot.nordeste <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_death_rate))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon+ .75, y= lat +.6)) +
                     xlim(-48, -32.5) + ylim(-18,-1) + 
                     scale_size( limits  = c(0,6), name = "") +
                     scale_fill_gradientn(limits = c(0,6),colours = c("white", "red")) + 
                     theme_void() +
                     labs(subtitle = "MORTES NO NORDESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.nordeste)}
                 }
        )
        
        
        # Norte
        # Contágio 
        saveGIF( movie.name = "./02_plot/contágio_norte.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
                   
                   df <- left_join(norte, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 1200401 | id_cidade == 1100205 | id_cidade == 1302603 |
                                                                                id_cidade == 1400100 | id_cidade == 1501402 | id_cidade == 1600303 |
                                                                                id_cidade == 1721000 )
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_confirmed_per_100k_inhabitants, lat, lon)
                   
                   plot.norte <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_confirmed_per_100k_inhabitants))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon+0.2, y= lat +1.9 )) +
                     xlim(-75, -44) + ylim(-15,5.5) + 
                     scale_size( limits  = c(0,10), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange")) + 
                     labs(subtitle = "CONTÁGIO NO NORTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.norte)}
                   
                 }
        )
        
        
        # Mortes 
        saveGIF( movie.name = "./02_plot/mortes_norte.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_death_rate, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_death_rate = mean(last_available_death_rate)*1000+1, date = max(date))
                   
                   df <- left_join(norte, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 1200401 | id_cidade == 1100205 | id_cidade == 1302603 |
                                                                                id_cidade == 1400100 | id_cidade == 1501402 | id_cidade == 1600303 |
                                                                                id_cidade == 1721000 )
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_death_rate, lat, lon)
                   
                   plot.norte <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_death_rate))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon+0.2, y= lat +1.9 )) +
                     xlim(-75, -44) + ylim(-15,5.5) + 
                     scale_size( limits  = c(0,6), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,6),colours = c("white", "red")) + 
                     labs(subtitle = "CONTÁGIO NO NORTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.norte)}
                   
                 }
        )
        
        
  ## Centro-Oeste
        # Contágio 
        saveGIF( movie.name = "./02_plot/contágio_centro_oeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
                   
                   df <- left_join(centro_oeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 5103403 | id_cidade == 5002704 | id_cidade == 5208707 |
                                                                                id_cidade == 5300108)
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_confirmed_per_100k_inhabitants, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_confirmed_per_100k_inhabitants))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon, y= lat +1 )) +
                     xlim(-63,-45)+ ylim(-24,-8) +
                     scale_size( limits  = c(0,10), name = "") +
                      theme_void()+
                     scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange")) + 
                     labs(subtitle = "CONTÁGIO NO CENTRO-OESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )
        
        # Mortes 
        saveGIF( movie.name = "./02_plot/mortes_centro_oeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_death_rate, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_death_rate = mean(last_available_death_rate)*1000+1, date = max(date))
                   
                   df <- left_join(centro_oeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 5103403 | id_cidade == 5002704 | id_cidade == 5208707 |
                                                                                id_cidade == 5300108)
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_death_rate, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_death_rate))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon+0.2, y= lat +1 )) +
                     xlim(-63,-45)+ ylim(-24,-8) +
                     scale_size( limits  = c(0,6), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,6),colours = c("white", "red")) + 
                     labs(subtitle = "MORTES NO CENTRO-OESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )

  ## Sudeste    
        # Contágio 
        saveGIF( movie.name = "./02_plot/contágio_sudeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
                   
                   df <- left_join(sudeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 3106200 | id_cidade == 3205309 | id_cidade == 3304557 |
                                                                                id_cidade == 3550308)
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_confirmed_per_100k_inhabitants, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_confirmed_per_100k_inhabitants))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon, y= lat +0.5 )) +
                     xlim(-55,-38)+ ylim(-26,-14) +
                     scale_size( limits  = c(0,10), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange")) + 
                     labs(subtitle = "CONTÁGIO NO SUDESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )
        
        # Mortes 
        saveGIF( movie.name = "./02_plot/mortes_sudeste.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_death_rate, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_death_rate = mean(last_available_death_rate)*1000 +1, date = max(date))
                   
                   df <- left_join(sudeste, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 3106200 | id_cidade == 3205309 | id_cidade == 3304557 |
                                                                                id_cidade == 3550308)
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_death_rate, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_death_rate))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon, y= lat +0.5 )) +
                     xlim(-55,-38)+ ylim(-26,-14) +
                     scale_size( limits  = c(0,6), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,6),colours = c("white", "red")) + 
                     labs(subtitle = "MORTES NO SUDESTE", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )
        
        
  ## Sul
      # Contágio
        saveGIF( movie.name = "./02_plot/contágio_sul.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_confirmed_per_100k_inhabitants, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_confirmed_per_100k_inhabitants = mean(last_available_confirmed_per_100k_inhabitants), date = max(date))
                   
                   df <- left_join(sul, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 4106902 | id_cidade == 4205407 | id_cidade == 4314902 )
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_confirmed_per_100k_inhabitants, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_confirmed_per_100k_inhabitants) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_confirmed_per_100k_inhabitants))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon, y= lat +0.5 )) +
                     xlim(-58,-47)+ ylim(-34,-22) +
                     scale_size( limits  = c(0,10), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,11),colours = c("white", "orange")) + 
                     labs(subtitle = "CONTÁGIO NO SUL", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )
        
        # Mortes
        saveGIF( movie.name = "./02_plot/mortes_sul.gif",
                 interval =.6,
                 convert="convert",
                 {for (i in 1:length(semanas)){
                   print(i)
                   df_1 <- df_coronabr_por_dia %>% 
                     select(c(city_ibge_code, last_available_death_rate, date))%>% 
                     filter(df_coronabr_por_dia$epidemiological_week == semanas[i]) %>% 
                     group_by(city_ibge_code) %>% 
                     summarise(last_available_death_rate = mean(last_available_death_rate)*1000+1, date = max(date))
                   
                   df <- left_join(sul, df_1, by= c("code_muni" = "city_ibge_code" ))
                   df$date <- df_1$date[1]
                   
                   df[is.na(df)] <- 1
                   
                   capitais <- latitude_e_longitude_dos_municípios %>% filter(id_cidade == 4106902 | id_cidade == 4205407 | id_cidade == 4314902 )
                   capitais <- capitais %>%select(id_cidade, lat, lon) %>%  group_by(id_cidade) %>% summarise(lat = median(lat), lon = median(lon))
                   
                   capitais <- merge.data.frame(capitais, df, by.x = "id_cidade", by.y ="code_muni" ) %>% select(name_muni,last_available_death_rate, lat, lon)
                   
                   plot.centro <-  ggplot()+
                     geom_sf(data = df$geom, color =NA, aes(fill = log(df$last_available_death_rate) )) +
                     geom_sf(data = BR, fill = NA, color = "black") +
                     geom_point(data = capitais, aes(x = lon, y = lat, size = log(last_available_death_rate))) +
                     geom_text(data = capitais,aes(label = name_muni, x = lon, y= lat +0.5 )) +
                     xlim(-58,-47)+ ylim(-34,-22) +
                     scale_size( limits  = c(0,6), name = "") +
                     theme_void()+
                     scale_fill_gradientn(limits = c(0,6),colours = c("white", "red")) + 
                     labs(subtitle = "MORTES NO SUL", title = paste(months.Date(df$date[1]), year(df$date[1])) ,  fill = "Escala logarítmica") 
                   
                   print(plot.centro)}
                 } 
                 
        )
        
        
             
