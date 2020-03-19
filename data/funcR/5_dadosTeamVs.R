ultimasPartidasDados <- function(classHTML, ultimJogos){
  ultViEmDe <- ultimJogos %>% 
    html_nodes(classHTML) %>% 
    html_text()
}  #funções auxiliares 
funcUltCampeonato <- function(link, ultimoCampeonatoAUX){
  ultimosJogoss <- (link %>% 
    read_html())
  
  time_1 <- ultimosJogoss %>% 
    html_nodes("h1 span") %>% 
    html_text()
  time_2 <- ultimosJogoss %>% 
    html_node("#team_games div.text a") %>% 
    html_text()
  
  x <- ultimoCampeonatoAUX[ultimoCampeonatoAUX != time_2] %>% #problema
    .[ultimoCampeonatoAUX != time_1]
  
  x <- x[!(x %in% "")]
  x <- x[!(x %in% NA)]
} #funções auxiliares
saldoGol <- function(teamHTML, nodes){
  dataSaldJog <- teamHTML %>% 
    html_nodes(nodes) %>% 
    html_text() %>% .[2] %>% 
    gsub("* .*", "", .) %>% gsub(",", ".", .) %>% 
    as.double()
} #funções auxiliares


dadosTeamVS <- function(link){
  teamVsHTML <- link %>% 
    read_html() %>% 
    html_nodes(".curiosity")
  if(length(teamVsHTML) == 0)
    listDadosTeamVs <- 0
  else{
    dataViEmDe_aux <- teamVsHTML %>% 
      html_nodes(".graph_games .bars span") %>% 
      html_text() 
    dataViEmDe <- dataViEmDe_aux[nchar(dataViEmDe_aux) >= 5] %>% 
      gsub("* .*", "", .) %>% as.numeric()
    
    totalJogos <- sum(dataViEmDe)                         #total de jogos entre as duas equipes
    SaldJogNg <- saldoGol(teamVsHTML, ".percent.red")     #golsSofridos por partida
    SaldJogPs <- saldoGol(teamVsHTML, ".percent.green")   #golsFeitos por partida
    GolMarcd <- as.integer(totalJogos * SaldJogPs)        #total de gols marcados
    GolSofri <- as.integer(totalJogos * SaldJogNg)        #total de gols sofridos
  
    listDadosTeamVs <- c(totalJogos, SaldJogNg, SaldJogPs, SaldJogPs, GolMarcd, GolSofri)
  }
}  #Retorna uma lista com os dados de equipe vs equipe
tabelaUltimasPartidas <- function(link){
  ultimosJogos <- link %>% 
    read_html() %>% 
    html_nodes("#team_games .zztable.stats tbody .parent")
  
    ultViEmDe <- ultimasPartidasDados(".form", ultimosJogos)          #lista das ultimas vitórias e derrotas
      ultData_aux <- ultimasPartidasDados(".double", ultimosJogos)     
    ultData <- ultData_aux[nchar(ultData_aux) >= 10]                #lista das ultimas Datas de confrotos
    ultTeamCasa <- ultimasPartidasDados(".text.home", ultimosJogos)   #lista do time da casa 
    ultResult <- ultimasPartidasDados(".result a", ultimosJogos)        #lista do placar
    ultTeamVisit <- ultimasPartidasDados(".text.away", ultimosJogos)  #lista do time visitante
      ultimoCampeonato_aux <- ultimasPartidasDados(".micrologo_and_text a", ultimosJogos)
    ultCampeonato <- funcUltCampeonato(link, ultimoCampeonato_aux)  #lista do campeonato onde as partidas foram realizadas
    
    TableLastGames <- data.frame(Resultado=ultViEmDe, Data = ultData, TimeDaCasa=ultTeamCasa,
                        Placar=ultResult, TimeVisitante=ultTeamVisit,Campeonato=ultCampeonato)
}  #Retorna uma tabela
