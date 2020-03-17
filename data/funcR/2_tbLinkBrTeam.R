library(rvest)
library(xml2)

tbLinkBrTeam <- function(link){
  source("data/funcR/1_numbPageBrTeam.R")
  for(i in 1:numberTeamPages(link)){
    if(i == 1)
      teamLinkTable <- link
    else{
      auxLink <- gsub("1", toString(i), link)
      teamLinkTable <- teamLinkTable %>% 
        rbind(auxLink)
    }
  }
  saveRDS(teamLinkTable, "data/tables/teamLinkTable.rds")
}