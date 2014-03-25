
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# read html code from website
year <-  seq(1991, 2014, 1)
urls <- paste("http://www.danskfodbold.com/kampe.php?seasonid=",
             year, sep = "")

urls <- c(urls, paste("http://www.danskfodbold.com/kampe.php?seasonid=",
             seq(1992, 1995, 1), 
             "&prioritet=1", 
             sep = ""))

remove <- c("Dato", "Hjemmehold",
            "Resultat", "Udehold",
            "Tilsk.", "Dommer",
            "Stadion",
            "Kampen sluttede oprindeligt 1-2, men DBU omstødte resultatet til 0-3, da Esbjerg fB havde benyttet en spiller ulovligt.")

get_data <- function(url){
  require(XML)
  url.data <- readLines(url)
  doc <- htmlTreeParse(url.data,  useInternalNodes = TRUE, encoding = "UTF-8")
  
  data <- xpathSApply(doc, "//table//tr//td//table//tr//td//table//tr//td//table//tr//td//table//tr//td", xmlValue)
  
  data <- trim(data[153:length(data)])
  data <- data[data != ""]
  
  data <- data[! data %in% remove]
  #if(year>2007){
  #  data <- split(data, ceiling(seq_along(data)/8))
  #}
  #else{
  data <- split(data, ceiling(seq_along(data)/7))
  #}
  return(do.call(rbind, data))
}

soc_data <- list()
for(i in seq_along(urls)){
  soc_data[[i]] <- get_data(urls[i])
}

data <- data.frame(do.call(rbind, soc_data), stringsAsFactors=F)

data$X1 <- unlist(strsplit(data$X1, " "))[1]
library(lubridate)
data$X1 <- dmy(data$X1)

colnames(data) <- c("date",
                    "home_team",
                    "result",
                    "away_team",
                    "attendance",
                    "referee",
                    "place")

data$attendance <- gsub("\\.","", data$attendance)

write.csv(data,
          "~/git/superliga/data/superliga.csv", row.names=FALSE)

