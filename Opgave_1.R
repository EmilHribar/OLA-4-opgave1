# Video fra Wulf Uge 46

# Opgave 1.1 – Hente data fra Bilbasen
{
# min første kode
{

  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  
  # Første side        
  startlink <- "https://www.bilbasen.dk/brugt/bil/renault?fuel=3&includeengroscvr=true&includeleasing=false"
  rawres <- GET(url=startlink)
  rawres$status_code 
  rawcontent <- httr::content(rawres, as="text")
  
  # Transformer text til html-nodes
  page <- read_html(rawcontent)
  
  # Hent bil-elementer fra startside
  carlist <- page %>% html_elements("article")
  
  # tag-liste
  ptag <- ".Listing_price__6B3kE"
  proptag <- ".Listing_properties___ptWv"
  mmtag <- ".Listing_makeModel__7yqgs"
  dettag <- "[class^='Listing-details']"
  dettagitem <- "[class^='ListingDetails_listItem']"
  desctag <- "[class^='Listing_description']"
  loctag <- ".Listing_location__nKGQz"
  Imgtag <- "img.Listing_dealerLogoCard__wHl9H"
  
  # Dataframe til opsamling
  cn <- c("price", "properties", "model", "detailitems", "description", "location", "dealerLogo", "link", "carid", "scrapedate")
  colldf <- as.data.frame(matrix(data=NA, nrow = 0, ncol = 10))
  colnames(colldf) = cn
  
  # Løkke gennem bil-elementer og udtræk data
  for (car in carlist) {
    price <- tryCatch(car %>% html_element(ptag) %>% html_text(trim = TRUE), error = function(e) NA)
    props <- tryCatch(car %>% html_element(proptag) %>% html_text(trim = TRUE), error = function(e) NA)
    makemodel <- tryCatch(car %>% html_element(mmtag) %>% html_text(trim = TRUE), error = function(e) NA)
    details <- tryCatch(car %>% html_elements(dettagitem) %>% html_text(trim = TRUE) %>% paste0(collapse = "_"), error = function(e) NA)
    description <- tryCatch(car %>% html_element(desctag) %>% html_text(trim = TRUE), error = function(e) NA)
    location <- tryCatch(car %>% html_element(loctag) %>% html_text(trim = TRUE), error = function(e) NA)
    dealerLogo <- tryCatch(car %>% html_element(Imgtag) %>% html_attr("src"), error = function(e) NA)
    link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
    carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
    scrapedate <- Sys.time()
    
    # Midlertidig data frame til indsamlede data
    tmpdf <- data.frame(price, props, makemodel, details, description, location, dealerLogo, link, carid, scrapedate, stringsAsFactors = FALSE)
    
    # Tilføj til hoved dataframen
    colldf <- rbind(colldf, tmpdf)
  }
  
  # Udskriv dataframe
  print(colldf)
}

# Loop for at hente de resterende sider
{
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Dataframe til opsamling
cn <- c("price", "properties", "model", "detailitems", "description", "location", "dealerLogo", "link", "carid", "scrapedate")
colldf <- as.data.frame(matrix(data=NA, nrow = 0, ncol = 10))
colnames(colldf) = cn

# Løkke gennem siderne (fra side 2 til 13)
for (page_num in 2:13) {
  # Dynamisk URL for hver side
  url <- paste0("https://www.bilbasen.dk/brugt/bil/renault?fuel=3&includeengroscvr=true&includeleasing=false&page=", page_num)
  
  # Hent sideindhold
  rawres <- GET(url = url)
  rawcontent <- httr::content(rawres, as = "text")
  
  # Transformer tekst til HTML-nodes
  page <- read_html(rawcontent)
  
  # Hent bil-elementer fra nuværende side
  carlist <- page %>% html_elements("article")
  
  # tag-liste
  ptag <- ".Listing_price__6B3kE"
  proptag <- ".Listing_properties___ptWv"
  mmtag <- ".Listing_makeModel__7yqgs"
  dettagitem <- "[class^='ListingDetails_listItem']"
  desctag <- "[class^='Listing_description']"
  loctag <- ".Listing_location__nKGQz"
  Imgtag <- "img.Listing_dealerLogoCard__wHl9H"
  
  # Løkke gennem bil-elementer og udtræk data
  for (car in carlist) {
    price <- tryCatch(car %>% html_element(ptag) %>% html_text(trim = TRUE), error = function(e) NA)
    props <- tryCatch(car %>% html_element(proptag) %>% html_text(trim = TRUE), error = function(e) NA)
    makemodel <- tryCatch(car %>% html_element(mmtag) %>% html_text(trim = TRUE), error = function(e) NA)
    details <- tryCatch(car %>% html_elements(dettagitem) %>% html_text(trim = TRUE) %>% paste0(collapse = "_"), error = function(e) NA)
    description <- tryCatch(car %>% html_element(desctag) %>% html_text(trim = TRUE), error = function(e) NA)
    location <- tryCatch(car %>% html_element(loctag) %>% html_text(trim = TRUE), error = function(e) NA)
    dealerLogo <- tryCatch(car %>% html_element(Imgtag) %>% html_attr("src"), error = function(e) NA)
    link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
    carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
    scrapedate <- Sys.time()
    
    # Midlertidig data frame til indsamlede data
    tmpdf <- data.frame(price, props, makemodel, details, description, location, dealerLogo, link, carid, scrapedate, stringsAsFactors = FALSE)
    
    # Tilføj til hoved dataframen
    colldf <- rbind(colldf, tmpdf)
  }
  
  # Vent 3 sekunder for ikke at overbelaste serveren
  Sys.sleep(3)
}

# Udskriv dataframe
print(colldf)
}

# Lav separat df
Opgave_1.1 <- colldf

# Fjern alle NA-vædier
Opgave_1.1 <- na.omit(Opgave_1.1)

# Dan fohandlerID
# Opret en unik identifikator baseret på location og dealerLogo
Opgave_1.1$dealerIdentifier <- paste(Opgave_1.1$location, Opgave_1.1$dealerLogo)

# Generer et unikt forhandlerID startende fra 1
Opgave_1.1$dealerID <- as.numeric(factor(Opgave_1.1$dealerIdentifier))

# Fjern eventuelle midlertidige kolonner, hvis de ikke er nødvendige
Opgave_1.1$dealerIdentifier <- NULL

# Tjek resultaterne
head(Opgave_1.1[, c("location", "dealerLogo", "dealerID")])
}

# Opgave 1.2 – Rense data
{
# Skateboard
  
# Forsøg1
{
# Hent teksten fra colldf[1,5]
text <- colldf[1,5]

# Fjern specialtegn, behold bogstaver (inkl. æ,ø,å), tal, punktum og komma
text <- gsub("[^a-zA-Z0-9æøåÆØÅ., \\n]", "", text)

# Erstat newline med ". "
text <- gsub("\\n+", ". ", text)

# Fjern ekstra mellemrum
text <- gsub("\\s+", " ", text)

# Fjern mellemrum før punktummer
text <- gsub("\\s+\\.", ".", text)

# Sørg for korrekt mellemrum efter punktummer
text <- gsub("\\.\\s+", ". ", text)

# Print den færdige tekst
cat(text)
}
cat(text)

# Forsøg 2 // Bedst i test
{
  # Hent teksten fra colldf[1,5]
  text2 <- colldf[1,5]
  
  # Fjern specialtegn, behold bogstaver (inkl. æ,ø,å), tal, punktum og komma
  text2 <- gsub("[^a-zA-Z0-9æøåÆØÅ., \\n]", "", text2)
  
  # Erstat newline med ". "
  text2 <- gsub("\\n+", ". ", text2)
  
  # Fjern ekstra mellemrum
  text2 <- gsub("\\s+", " ", text2)
  
  # Fjern mellemrum før punktummer
  text2 <- gsub("\\s+\\.", ".", text2)
  
  # Sørg for korrekt mellemrum efter punktummer
  text2 <- gsub("\\.\\s+", ". ", text2)
  
  # Fjern sekvenser af flere punktummer og mellemrum som ".. ." -> ". "
  text2 <- gsub("\\.\\.+\\s*\\.?", ". ", text2)
}
# Print den færdige tekst
cat(text2)

# Forsøg 3
{
  # Hent tekst fra colldf[1,5]
  tekst <- colldf[1,5]
  
  # Fjern tegn som ikke er punktum eller komma og erstat linjeskift med ". "
  renset_tekst <- gsub("[^a-zA-Z0-9.,æøåÆØÅ ]", "", tekst)  # Fjern alle tegn undtagen bogstaver, tal, komma, punktum og mellemrum
  renset_tekst <- gsub("\n", ". ", renset_tekst)  # Erstat linjeskift med ". "
  renset_tekst <- gsub(" +", " ", renset_tekst)  # Erstat flere mellemrum med ét mellemrum
  renset_tekst <- trimws(renset_tekst)  # Fjern ledende og efterfølgende mellemrum

}
# Vis den rensede tekst
print(renset_tekst)
}

# Opgave 1.3 - Hente nye data - simuleret
{
# Ny df til 1.3
Opgave_1.3 <- colldf

# Fjerner de 5 første rækker
Opgave_1.3 <- Opgave_1.3[-(1:5),]

# Ændre datoen for webscraping
Opgave_1.3[,10] <- Opgave_1.3[,10]+86400

# Ændre prisen i 3 første rækker
Opgave_1.3[1:3,1] <- NA
# 226900*1.02 = 231.438 kr.
Opgave_1.3[1,1] <- "231.438 kr."
# 229900*1.02 = 234.498 kr.
Opgave_1.3[2,1] <- "234.498 kr."
# 219800*1.02 = 224.196 kr.
Opgave_1.3[3,1] <- "224.196 kr."

# Oprettelse af 2 nye rækker
Opgave_1.3 <- rbind(Opgave_1.3, tail(Opgave_1.3, 2))
Opgave_1.3[347,1] <- "199.995 kr"
Opgave_1.3[347,9] <- "7000001"

Opgave_1.3[348,1] <- "149.995 kr"
Opgave_1.3[348,9] <- "7000002"
}

# Opgave 1.4 - Hente tyske data
{
{
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Autoscout24.de
# Første side        
startlink2 <- "https://www.autoscout24.de/lst/renault/kiel?atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E&lat=54.32276&lon=10.1359&ocs_listing=include&powertype=kw&search_id=1t76jc6m3jx&sort=standard&source=detailsearch&zip=kiel&zipr=100"
rawres2 <- GET(url=startlink2)
rawres2$status_code 
rawcontent2 <- httr::content(rawres2,as="text")

# Transformer text til html-nodes
page2 <- read_html(rawcontent2)

# Hent bil-elementer fra startside
Carlisttag <- ".ListItem_article__qyYw7"
carlist2 <- page2 %>% html_nodes(Carlisttag)

# tag-liste
Modeltag <- "a.ListItem_title__ndA4s h2"
Pricetag <- ".Price_price__APlgs.PriceAndSeals_current_price__ykUpx"
Producedtag <- 'span[data-testid="VehicleDetails-calendar"]'
KMdriventag <- 'span[data-testid="VehicleDetails-mileage_road"]'
Milagetag <- 'span[data-testid="VehicleDetails-speedometer"]'
Consumptiontag <- 'span[data-testid="VehicleDetails-lightning_bolt"]'
Locationtag <- 'span[data-testid="sellerinfo-address"]'
Dealertag <- 'span[data-testid="sellerinfo-company-name"]'


# Dataframe til opsamling
cn2 <- c("price","Produced","model", "KMdriven","Milage","Consumption","location", "Dealer", "link","carid","scrapedate")
colldf2 <- as.data.frame(matrix(data=NA,nrow = 0,ncol = 10))
colnames(colldf2)=cn2

# Løkke gennem bil-elementer og udtræk data
for (car in carlist2) { # Iterér over alle biler i carlist2
  model <- tryCatch(car %>% html_element(Modeltag) %>% html_text(trim = TRUE), error = function(e) NA)
  price <- tryCatch(car %>% html_element(Pricetag) %>% html_text(trim = TRUE), error = function(e) NA)
  Produced <- tryCatch(car %>% html_element(Producedtag) %>% html_text(trim = TRUE), error = function(e) NA)
  KMdriven <- tryCatch(car %>% html_element(KMdriventag) %>% html_text(trim = TRUE), error = function(e) NA)
  Milage <- tryCatch(car %>% html_elements(Milagetag) %>% html_text(trim = TRUE), error = function(e) NA)
  Consumption <- tryCatch(car %>% html_element(Consumptiontag) %>% html_text(trim = TRUE), error = function(e) NA)
  location <- tryCatch(car %>% html_element(Locationtag) %>% html_text(trim = TRUE), error = function(e) NA)
  Dealer <- tryCatch(car %>% html_element(Dealertag) %>% html_text(trim = TRUE), error = function(e) NA)
  link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
  full_link <- tryCatch(paste0("https://www.autoscout24.de", link), error = function(e) NA)
  carid <- tryCatch(str_extract(link, "[a-f0-9-]{36}"), error = function(e) NA)
  scrapedate <- Sys.time()
  
  # Midlertidig data frame til indsamlede data
  tmpdf2 <- data.frame(model, price, Produced, KMdriven, Milage, Consumption, location, Dealer, link, carid, scrapedate, stringsAsFactors = FALSE)
  
  # Tilføj til hoved dataframen
  colldf2 <- rbind(colldf2, tmpdf2)
}
print(colldf2)
}

# Gem dataframen colldf2 som en CSV-fil
write.csv(colldf2, "bil_data.csv", row.names = FALSE)

# Script for at hente alle sider
{
  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  
  # Dataframe til opsamling
  cn2 <- c("price", "Produced", "model", "KMdriven", "Milage", "Consumption", "location", "Dealer", "link", "carid", "scrapedate")
  colldf2 <- as.data.frame(matrix(data = NA, nrow = 0, ncol = length(cn2)))
  colnames(colldf2) <- cn2
  
  # Løkke gennem siderne
  for (page_num in 1:15) {
    # Dynamisk URL for hver side
    startlink2 <- paste0(
      "https://www.autoscout24.de/lst/renault/kiel?",
      "atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E",
      "&lat=54.32276&lon=10.1359&ocs_listing=include&powertype=kw",
      "&search_id=1t76jc6m3jx&sort=standard&source=detailsearch",
      "&zip=kiel&zipr=100&size=20&page=", page_num
    )
    
    # Send HTTP-anmodning
    rawres2 <- GET(url = startlink2)
    if (rawres2$status_code != 200) {
      message(paste("Fejl ved indlæsning af side:", page_num))
      next
    }
    
    rawcontent2 <- httr::content(rawres2, as = "text", encoding = "UTF-8")
    page2 <- read_html(rawcontent2)
    
    # Hent bil-elementer fra siden
    Carlisttag <- ".ListItem_article__qyYw7"
    carlist2 <- page2 %>% html_nodes(Carlisttag)
    
    # tag-liste
    Modeltag <- "a.ListItem_title__ndA4s h2"
    Pricetag <- ".Price_price__APlgs.PriceAndSeals_current_price__ykUpx"
    Producedtag <- 'span[data-testid="VehicleDetails-calendar"]'
    KMdriventag <- 'span[data-testid="VehicleDetails-mileage_road"]'
    Milagetag <- 'span[data-testid="VehicleDetails-speedometer"]'
    Consumptiontag <- 'span[data-testid="VehicleDetails-lightning_bolt"]'
    Locationtag <- 'span[data-testid="sellerinfo-address"]'
    Dealertag <- 'span[data-testid="sellerinfo-company-name"]'
    
    # Løkke gennem bil-elementer og udtræk data
    for (car in carlist2) {
      model <- tryCatch(car %>% html_element(Modeltag) %>% html_text(trim = TRUE), error = function(e) NA)
      price <- tryCatch(car %>% html_element(Pricetag) %>% html_text(trim = TRUE), error = function(e) NA)
      Produced <- tryCatch(car %>% html_element(Producedtag) %>% html_text(trim = TRUE), error = function(e) NA)
      KMdriven <- tryCatch(car %>% html_element(KMdriventag) %>% html_text(trim = TRUE), error = function(e) NA)
      Milage <- tryCatch(car %>% html_element(Milagetag) %>% html_text(trim = TRUE), error = function(e) NA)
      Consumption <- tryCatch(car %>% html_element(Consumptiontag) %>% html_text(trim = TRUE), error = function(e) NA)
      location <- tryCatch(car %>% html_element(Locationtag) %>% html_text(trim = TRUE), error = function(e) NA)
      Dealer <- tryCatch(car %>% html_element(Dealertag) %>% html_text(trim = TRUE), error = function(e) NA)
      link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
      full_link <- tryCatch(paste0("https://www.autoscout24.de", link), error = function(e) NA)
      carid <- tryCatch(str_extract(link, "[a-f0-9-]{36}"), error = function(e) NA)
      
      # Tilføj til hoved dataframen
      scrapedate <- Sys.Date()
      tmpdf <- data.frame(price, Produced, model, KMdriven, Milage, Consumption, location, Dealer, link, carid, scrapedate, stringsAsFactors = FALSE)
      colldf2 <- rbind(colldf2, tmpdf)
    }
    
    # Vent 3 sekunder for ikke at overbelaste serveren
    Sys.sleep(3)
  }
}
  # Udskriv dataframe
  print(colldf2)
}

# 12gebrauchwagen.de
{
# Første side        
startlink2 <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmk%5D=63&s%5Bmd%5D=&s%5By_min%5D=&s%5By_max%5D=&s%5Bm_min%5D=&s%5Bm_max%5D=&s%5Bprice_or_rate%5D=price&s%5Bpr_min%5D=500&s%5Bpr_max%5D=500000&s%5Brate_from%5D=&s%5Brate_to%5D=&s%5Bzip%5D=24103&s%5Brad%5D=100&s%5Bt%5D=&s%5Bg%5D=a&s%5Bpw_min%5D=&s%5Bpw_max%5D=&s%5Bsince%5D=&s%5Bfuel%5D%5B%5D=7&s%5Bcu%5D=&s%5Bsort%5D=6&button="
rawres2 <- GET(url=startlink2)
rawres2$status_code 
rawcontent2 <- httr::content(rawres2,as="text")

# Transformer text til html-nodes
page2 <- read_html(rawcontent2)

# Hent bil-elementer fra startside
# Carlisttag <- "columns car-ad offers-gap  offer-4803616015 car-make-renault car-model-zoe"
Carlisttag <- "[class*='car-ad']"
carlist2 <- page2 %>% html_nodes(Carlisttag)

# tag-liste
Modeltag <- "a.provider-link.click-out"
Pricetag <- ".purchase-price.ml-3.h1"
Producedtag <- "[class*='reg_year']"
KMdriventag <- "[class*='mileage']"
Topspeedtag <- "[class*='power']"
Consumptiontag <-"[class*='consumption']"
Locationtag <- "[class*='location']"
Dealertag <- "[class*='hover-underline']"


# Dataframe til opsamling
cn2 <- c("price","Produced","model", "KMdriven","Topspeed","Consumption","location","link","carid","scrapedate")
colldf2 <- as.data.frame(matrix(data=NA,nrow = 0,ncol = 10))
colnames(colldf2)=cn2

# Løkke gennem bil-elementer og udtræk data
for (car in carlist2) { # Iterér over alle biler i carlist2
  car=car2[[1]]
  Modeltag <- tryCatch(car %>% html_element(Modeltag) %>% html_text(trim = TRUE), error = function(e) NA)
  price <- tryCatch(car %>% html_element(Pricetag) %>% html_text(trim = TRUE), error = function(e) NA)
  Produced <- tryCatch(car %>% html_element(Producedtag) %>% html_text(trim = TRUE), error = function(e) NA)
  KMdriven <- tryCatch(car %>% html_element(KMdriventag) %>% html_text(trim = TRUE), error = function(e) NA)
  Topspeed <- tryCatch(car %>% html_elements(Topspeedtag) %>% html_text(trim = TRUE), error = function(e) NA)
  Consumption <- tryCatch(car %>% html_element(Consumptiontag) %>% html_text(trim = TRUE), error = function(e) NA)
  location <- tryCatch(car %>% html_element(Locationtag) %>% html_text(trim = TRUE), error = function(e) NA)
  Dealer <- tryCatch(car %>% html_element(Dealertag) %>% html_text(trim = TRUE), error = function(e) NA)
  link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
  carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
  scrapedate <- Sys.time()
  
  # Midlertidig data frame til indsamlede data
  tmpdf2 <- data.frame(Modeltag, price, Produced, KMdriven, Topspeed, Consumption, location, Dealer, link, carid, scrapedate, stringsAsFactors = FALSE)
  
  # Tilføj til hoved dataframen
  colldf2 <- rbind(colldf2, tmpdf2)
}
print(colldf2)
}






