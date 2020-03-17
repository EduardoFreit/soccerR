library(dplyr)

#time1 <- toupper("santa cruz")
#time2 <- toupper("flamengo")

time1VStime2 <- function(time1, time2){ 
  tableTime <- readRDS("data/tables/TabelaDeTimesBR.rds")
  
  idTime1 <- tableTime %>% 
    select(NomeTime, ID) %>% 
    filter(NomeTime == toupper(time1)) %>% 
    select(ID) %>% 
    as.numeric()
  
  idTime2 <- tableTime %>% 
    select(NomeTime, ID) %>% 
    filter(NomeTime == toupper(time2)) %>% 
    select(ID) %>% 
    as.numeric()
  
  paste("https://www.ogol.com.br/team_adversario.php?id=",
               idTime1,
               "&fk_adv=",
               idTime2, 
               sep = "")
}