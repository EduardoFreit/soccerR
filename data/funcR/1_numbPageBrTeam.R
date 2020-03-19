numberTeamPages <- function(link){
  download.file(link, destfile = "data/html/scrapedpage.html", quiet=TRUE)
  numPag <- read_html("data/html/scrapedpage.html") %>% 
    html_nodes(".numbers a") %>% 
    html_text() %>% 
    tail(n=1) %>% 
    as.numeric()
}