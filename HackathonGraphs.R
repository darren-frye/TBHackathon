library(plyr)
library(dplyr)
library(plotly)
library(lubridate)
library(leaflet)

##!!!!!!! Also has two dependencies: geojsonio, and htmltools. If you don't have them remember you can always install a package
## using install.packages("NAME_OF_PACKAGE")

## Grab data from cdc website
cdcData <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                    na.strings = "", 
                    fileEncoding = "UTF-8-BOM",
                    stringsAsFactors = FALSE) %>%
  mutate(Date = parse_date_time(dateRep, c("%d/%m/%Y")))

##Notice that the graph that we were supposed to recreate begins on a specific data. So we set a start date.
startDate <- as.Date("01-22-2020", "%m-%d-%Y")

##Aggregate the data to the world level
worldData <- cdcData %>%
  ##Make a data set for each Date
  group_by(Date) %>%
  ##For each date we sum the number of cases and sum all countries populations
  summarise(cases = sum(as.numeric(cases)), popData2018 = sum(as.numeric(popData2018), na.rm = TRUE)) %>%
  ##Remove the groupings
  ungroup() %>%
  ##Order the data by Date
  dplyr::arrange(Date) %>%
  ##Create a new column that is a cummulative sum of cases
  mutate(TotalCases = cumsum(cases)) %>%
  ##Create a column that is population in millions
  mutate(Millions = popData2018/1000000) %>%
  ##Create columns that are total cases per million, new cases per day per million, and tag this as world
  mutate(PerMillion = TotalCases/Millions, PerMillionDay = cases/Millions, countriesAndTerritories = "World") %>%
  ##Limit the data to on or after the start date
  filter(Date >= startDate) %>%
  ##Grab only the columns that are of interest
  dplyr::select(Date, countriesAndTerritories, cases, popData2018, TotalCases, Millions, PerMillion, PerMillionDay)

transformData <- cdcData %>% 
  ##Only grab the countries we are interested in.
  filter(countriesAndTerritories %in% c("United_States_of_America", "United_Kingdom", "China", "South_Korea")) %>%
  ##Group by country
  group_by(countriesAndTerritories) %>%
  ##Order the data by Date
  dplyr::arrange(Date) %>%
  ##Create a new column that is a cummulative sum of cases
  mutate(TotalCases = cumsum(cases)) %>%
  ##Remove the groupings
  ungroup() %>%
  ##Create a column that is population in millions
  mutate(Millions = popData2018/1000000) %>%
  ##Create columns that are total cases per million, new cases per day per million
  mutate(PerMillion = TotalCases/Millions, PerMillionDay = cases/Millions) %>%
  ##Limit the data to on or after the start date
  filter(Date >= startDate) %>%
  ##Grab only the columns that are of interest
  dplyr::select(Date, countriesAndTerritories, cases, popData2018, TotalCases, Millions, PerMillion, PerMillionDay)

##Combine world and country data
fullData <- rbind(transformData, worldData)

## Line Graph
p <- plot_ly(fullData,
        type='scatter',
        x=~Date,
        y=~PerMillion,
        connectgaps = TRUE,
        mode = "lines",
        transforms = list(
          list(
            type = 'groupby',
            groups = fullData$countriesAndTerritories,
            styles = list(
              list(target = "United_States_of_America", value = list(line=list("#bc506c"))),
              list(target = "United_Kingdom", value = list(line=list("#325ea9"))),
              list(target = "China", value = list(line=list("#703378"))),
              list(target = "South_Korea", value = list(line=list("#c5d3ff"))),
              list(target = "World", value = list(line=list("#76cab8")))
            )
          )
        ))
p <-  p %>%
  layout(yaxis = list(type = "log",
                      autotick = F, tickvals = c(.001,.01,.1,1,10,100,1000)), 
         showlegend=TRUE) %>%
  config(displayModeBar = FALSE)
p

## Lat Date of Interest for the Map
endDate <- as.Date("05-08-2020", "%m-%d-%Y")


mapData <- cdcData %>%
  ##Group by country
  group_by(countriesAndTerritories) %>%
  ##Order each group by date
  dplyr::arrange(Date) %>%
  ## Cummulative sum by country
  mutate(TotalCases = cumsum(cases)) %>%
  ## Remove Grouping
  ungroup() %>%
  ##Add column that is population in millions
  mutate(Millions = popData2018/1000000) %>%
  ##Create columns that are total cases per million, new cases per day per million
  mutate(PerMillion = TotalCases/Millions, PerMillionDay = cases/Millions) %>%
  ##Subset to only the date we set above
  filter(Date == endDate) %>%
  ##Select the columns of interest
  dplyr::select(Date, countriesAndTerritories, ISO_A3 = countryterritoryCode, cases, popData2018, TotalCases, Millions, 
                PerMillion, PerMillionDay)

#Leaflet/GEOJSON

##Country json Load
countries <- geojsonio::geojson_read("Data/countries.geojson", what = "sp")

## Join our country data with the json data.
countries@data <- join_all(list(countries@data, mapData), match = "first")

## Create a base graph to work from
Map <- leaflet(countries) %>% addTiles() %>% addPolygons()

##Setup color bins
bins <- c(0,5,10,50,100,500,1000,5000,Inf)

## Choose your color pallette
pal <- colorBin("YlOrRd", domain = countries@data$PerMillion, bins = bins)

## The info for when you hover over a country. Uses html format.
labels <- sprintf(
  "<strong>%s</strong><br/>%g Cases <sup></sup>",
  countries@data$countriesAndTerritories, countries@data$PerMillion) %>% lapply(htmltools::HTML)


##BOOM. A little chick-fil-a will take you a long way. The map below takes a while to render because it is drawing a ton of
##spatial polygons and assign information to them. Give it a minute or two before you throw your computer.

Map %>% addPolygons(
  fillColor = ~pal(PerMillion),
  weight = 1,
  opacity = 1,
  color = 'white',
  dashArray = '3',
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
)
