
library(stringr)
library(rvest)
library(dplyr)
library(rworldmap)
library(XML)

url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_tax_revenue_as_percentage_of_GDP'
xpath_wiki <- '//*[@id="mw-content-text"]/table[2]'

table <- url %>% html() %>% html_nodes(xpath = xpath_wiki) %>% html_table()
table <- table[[1]]
table$Country <- str_sub(table$Country,start = 2,end = str_length(table$Country))

names(table)[] <- c('country','tax_as_GDP')
table <- table %>% arrange(-tax_as_GDP)


#Plotting in a world map
gc <- geocode(table$country)
geo_longitude <- gc$lon
geo_latitude <- gc$lat
map('world',fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))

# points(geo_longitude,geo_latitude, col="red", pch=16)

buckets <- c(5,10,15,20,25,30,Inf)
table$tax_cat <- findInterval(table$tax_as_GDP,buckets,rightmost.closed = T)


table$tax_category[table$tax_cat == 0] <- '<5'
table$tax_category[table$tax_cat == 1] <- '5 - 10'
table$tax_category[table$tax_cat == 2] <- '10 - 15'
table$tax_category[table$tax_cat == 3] <- '15 - 20'
table$tax_category[table$tax_cat == 4] <- '20 - 25'
table$tax_category[table$tax_cat == 5] <- '25 - 30'
table$tax_category[table$tax_cat == 6] <- '30+'

sPDF <- joinCountryData2Map(table
                            ,joinCode="NAME"
                            ,nameJoinColumn="country")
# color <- c('#1f1f1f','#f32828','#0b4759','#ebebeb','#a49b62',)
colourPalette <- c('palegreen'
                   ,'yellow'
                   ,'orange'
                   ,'orangered'
                   ,'darkred','blue','cyan')

# colourPalette <- brewer.pal(7,'RdYlGn')


par(mar=c(0,0,1,0))
mapParams <- mapCountryData(sPDF
                            ,nameColumnToPlot='tax_cat'
                            ,colourPalette = colourPalette
                            ,addLegend=FALSE
                            ,catMethod='fixedWidth'
                            ,mapTitle='Tax percentage as GDP'
                            )

mapParams$legendText <-
  c('<5'
    ,'5-10'
    ,'10-15'
    ,'15-20'
    ,'20-25',
    '25-30',
    '>30')


do.call( addMapLegendBoxes
         , c(mapParams
             ,x='bottomleft'
             ,title="Tax Category"))
