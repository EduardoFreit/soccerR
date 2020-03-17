library(rvest)
library(xml2)
library(dplyr)
aux <- "https://www.ogol.com.br/search_team.php?search_string=&sta=&nac=6&posi=&peq=&ida=&cap=&ord=i&op=all&page=1"
vs <- "https://www.ogol.com.br/team_adversario.php?id=2244&fk_adv=2240"

teamDst <- function(link){
  Sys.sleep(runif(1, 5.3, 7.8))
  teamDestHTML <- link %>% 
    read_html() %>% 
    html_nodes('.zztable') %>% 
    .[1] %>% html_nodes("div a")
  
  teamDestNM <- teamDestHTML %>%
    html_text() %>% 
    toupper()
  
  teamDestID <- teamDestHTML %>% 
    html_attr("href") %>% 
    .[nchar(teamDestID) <= 23] %>% 
    gsub(".*=", "", .) %>% as.numeric()
  
  teamDestq <- c(teamDestNM,teamDestID)
}

crawlerTeam <- function(link){
  
  teamsHTML<- link %>% read_html() %>%  
    html_nodes(".zztable.stats tr div div a")
  
  teamsNM <- teamsHTML %>% 
    html_nodes("b") %>% 
    html_text() %>% 
    toupper()
  
  teamsID <- teamsHTML %>% 
    html_attr("href")
  teamsID <- teamsID[nchar(teamsID) <= 23]
  teamsID <- gsub(".*=", "", teamsID) %>% 
    as.numeric()

  if (stringr::str_sub(link, -2 ,-1) == "=1"){
    cat("...")
    teamDst <- teamDst(link)
    teamsNM <- c(teamsNM, teamDst[1])
    teamsID <- c(teamsID, (teamDst[2] %>% as.numeric()))
  }
  
  data.frame(NomeTime = teamsNM, ID = teamsID)
}

tabelaTeams <- function(link){
  source("data/funcR/2_tbLinkBrTeam.R")
  x <- 0
  tbLinkBrTeam(link)
  Sys.sleep(runif(1, 3.3, 7.8))
  linksTeams <- readRDS("data/tables/teamLinkTable.rds")
  tbTeams <- data.frame() # tabela em branco
  
  for(i in linksTeams){
    x <- x %>% 
      sum((100/length(linksTeams))) %>% 
      round(2)
    
    cat("Loading...", x,'%\n')
    tbTeams <- tbTeams %>%
      rbind(crawlerTeam(i))
      Sys.sleep(runif(1, 3.3, 7.8))
  }
  saveRDS(tbTeams, "data/tables/TabelaDeTimesBR.rds")
  cat("Done 100",'%\n')
}