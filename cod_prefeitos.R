library(tidyverse)
library(ggplot2)
library(brazilmaps)

#Informações sobre o pacote brazilmaps em https://cran.r-project.org/web/packages/brazilmaps/brazilmaps.pdf
#Dados obtidos em http://inter04.tse.jus.br/ords/dwtse/f?p=176:25:2774073294115::NO::: acesso em 12/09/2019

prefeitos <- read_csv2("G:/Meu Drive/InstitutoGuimaraes/Prefeitos/prefeitos_eleitos_2016_1.csv") %>% rbind(read_csv2("G:/Meu Drive/InstitutoGuimaraes/Prefeitos/prefeitos_eleitos_2016_2.csv") )
prefeitos$`Votos nominais` <- as.integer(prefeitos$`Votos nominais`)

mapa_estado <- function(UF, dados){

estados_brasileiros = tibble(UF = c("AC", "AL", "AM" , "AP","BA" ,"CE" , "DF","ES", "GO", "MA" ,"MG" ,"MS" ,"MT", "PA" ,"PB" ,"PE", "PI" ,"PR", "RJ", "RN" ,"RO", "RR","RS" ,"SC" ,"SE" ,"SP","TO"),
                             id  = c(12, 27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17),
                             estado = c("Acre" , "Alagoas" , "Amazonas" , "Amapá", "Bahia" , "Ceará" ,"Distrito Federal", "Espírito Santo" , "Goiás" , "Maranhão" , "Minas Gerais" , "Mato Grosso do Sul" , "Mato Grosso" , "Pará" , "Paraíba" , "Pernambuco" , "Piauí" , "Paraná" , "Rio de Janeiro" , "Rio Grande do Norte" , "Rondônia", "Roraima" , "Rio Grande do Sul" , "Santa Catarina" , "Sergipe" ,  "São Paulo" , "Tocantins"))

id <- estados_brasileiros[(estados_brasileiros$UF == UF),2]
estado <- estados_brasileiros[(estados_brasileiros$UF == UF),3]

#Pega informações de coordenadas do IBGE para monstar o mapa do estado
map <- get_brmap(geo = "City",
                  geo.filter = list(State = id),
                  class = "SpatialPolygonsDataFrame")

#une o mapa do IBGE com os dados
plot<- plot_brmap(map, data_to_join = dados,
                    join_by = c("nome" = "Município"),
                    var = "Votos nominais") 


return(plot+ labs(title = estado,caption = "Fonte: tse.jus.br") )

}



UF = c("AC", "AL", "AM" , "AP","BA" ,"CE" , "DF","ES", "GO", "MA" ,"MG" ,"MS" ,"MT", "PA" ,"PB" ,"PE", "PI" ,"PR", "RJ", "RN" ,"RO", "RR","RS" ,"SC" ,"SE" ,"SP","TO")

dados <- filter(prefeitos, UF == "AC", Partido == "PSB" )

mapa_estado(UF = "AC", dados = dados)




