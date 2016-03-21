library(acs) #Helper Function to use Census API
library(dplyr) #Manipulate Data
library(ggplot2) #Make static charts
library(scales) #Make pretty numbers
library(htmlwidgets) #enable html writing
library("haven") 
library("viridisLite") #A good color scheme for heatmaps
library(highcharter) #Wrapper for the Javascript highcharts package
library(xtable)
library(tidyr)
data("uscountygeojson")

setwd("C:/Users/Greg/Documents/R/R Markdown Presentation 3-15-2015")

#Get ACS Data from the API -----

#Load ACS API key, you only need to do this once
#api.key.install(key = "3f9611464c1c967ce320f43ef54ce90a5f60b65f")

#geographies determine what areas you are pulling in data from
#Here I am pulling North Carolina data by county
NC_Area = geo.make(state = "NC", county = "*", check = T)
geo.list(NC_Area)


#how to figure what what variables to pull
Lookup = results(acs.lookup(table.name = "B25026"))

library(knitr)
kable( head(geo.lookup(state = "NC", county = "A"), 10))
  xtable()%>%
  print()

# 
# #Variables to pull:
# Slide1: B01003_001: Total Population
# 
# Slide2: By Sex
# B98012_002: Males
# B98012_003: Females
# 
# Slide3: How Long you have owned your house that you own
# B25026_003: Moved in After 2005
# B25026_004: Between 2000-2004
# B25026_005: 1990-1999
# B25026_006: 1980-1989
# B25026_007:  1970-1979
# B25026_008: before 1969


#Actually call the API and return the data as an acs object
NC_Data = acs.fetch(geography = NC_Area, 
                           variable = c("B01003_001", "B01001_002", "B01001_026", "B25026_003",
                                        "B25026_004", "B25026_005", "B25026_006", "B25026_007", "B25026_008"),
                           col.names = c("Total", "Males", "Females", "Moved in After 2005", "Between 2000 and 04",
                                         "Between 90 and 99", "Between 80 and 89",  "Between 70 and 79", 
                                         "Before 69") )


#Turn acs into a data frame for easier manipulation
NC_DF = data.frame(geography(NC_Data), estimate(NC_Data))%>%
  left_join(fips.state, by = c("state" = "STATE"))

row.names(NC_DF) = NULL

gsub(".", " ", colnames(NC_DF), fixed = TRUE)

#Create static Bar Charts ----

Top_Counties = NC_DF%>%
  top_n(n = 15, wt = Total)%>%
  mutate(County = sapply(regmatches(NAME, regexec("^(.*) County,", NAME)), function(x) x[2]))%>%
  arrange(desc(Total))%>%
  select(County, Total)%>%
  mutate(County = factor(County, levels = County, ordered = TRUE))%>%
  ggplot()+
  geom_bar(aes(x = County, y = Total),stat = "identity", fill = "#D40010")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(label = comma)

Top_Counties








#Create a heatmap by County ----

Heatmap_data <-NC_DF%>%
  mutate(CODE = paste("us",
                      tolower(STUSAB) ,
                      str_pad(county, width = 3, pad = "0"),
                      sep = "-"))

n <- 32
dstops <- data.frame(q = 0:n/n, c = substring(viridis(n + 1, option = "D"), 0, 7))
dstops <- list.parse2(dstops)

highchart() %>% 
  hc_title(text = "Total Religious Adherents by County") %>% 
  hc_add_series_map(map = test, df = Heatmap_data, 
                    value = "Total", joinBy = c("hc-key", "CODE"),
                    name = "Pop Under Poverty Line", borderWidth = 0.1) %>% 
  hc_colorAxis(stops = dstops, min = 0, max = 800000) %>% 
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_tooltip(valueDecimals = 0)

### Make a Sankey for College Enrollmenta----


College_geo = geo.make(state = "NC", county = "Mecklenburg", check = T)

College_Data = acs.fetch(geography = College_geo, table.number = "B14004", col.names = "pretty")

College_DF = data.frame(geography(College_Data), estimate(College_Data))

row.names(College_DF) = NULL

CDF2 = gather(College_DF, Variable, Population, -NAME, -state, -county)

CDF2$VariableSimp = sapply(regmatches(CDF2$Variable, regexec(".*Yrs...(.*)", CDF2$Variable)), function(x) x[2])

CDF2$V1 = sapply(regmatches(CDF2$VariableSimp, regexec("(^.*?)\\.\\..*", CDF2$VariableSimp)), function(x) x[2])
CDF2$V2 = sapply(regmatches(CDF2$VariableSimp, regexec("^.*?\\.\\.(.*?)\\.\\.", CDF2$VariableSimp)), function(x) x[2])
CDF2$V3 = sapply(regmatches(CDF2$VariableSimp, regexec("^.*?\\.\\..*?\\.\\.(.*)", CDF2$VariableSimp)), function(x) x[2])
CDF2$V3 = ifelse(CDF2$V3=="", NA, CDF2$V3)

CDF2$Source = NA
CDF2$Target = NA
CDF2$Value = NA

for(i in 1:nrow(CDF2)){
  
  if(is.na(CDF2[i,"V3"]) & !is.na(CDF2[i,"V2"]) ){
    CDF2[i, "Source"] =  CDF2[i,"V1"]
    CDF2[i, "Target"] =  CDF2[i,"V2"]
    CDF2[i, "Value"] =  CDF2[i,"Population"]
    
  }
  
  else if(!is.na(CDF2[i,"V3"]) & !is.na(CDF2[i,"V2"]) ){
    CDF2[i, "Source"] =  CDF2[i,"V2"]
    CDF2[i, "Target"] =  CDF2[i,"V3"]
    CDF2[i, "Value"] =  CDF2[i,"Population"]
  }
  else if(is.na(CDF2[i,"V3"]) & is.na(CDF2[i,"V2"]) ){
    CDF2[i, "Source"] =  "Total"
    CDF2[i, "Target"] =  CDF2[i,"V1"]
    CDF2[i, "Value"] =  CDF2[i,"Population"]
    
  }
}

library(networkD3)

Sankey_Data = select(CDF2, Source, Target, Value)[-1,]

nodes = as.data.frame(unique(c(Sankey_Data$Source, Sankey_Data$Target)))
names(nodes) = "name"
nodes$nodevalue = as.numeric(row.names(nodes))-1

Sankey_Data = merge(Sankey_Data, nodes, by.x = "Target", by.y = "name")
names(Sankey_Data) = c("SRC", "Targ", "value", "source")

Sankey_Data = merge(Sankey_Data, nodes, by.x = "Targ", by.y = "name")
names(Sankey_Data) = c("Targ", "SRC", "value", "source", "target")

sankeyNetwork(Links = Sankey_Data, Nodes = nodes,
              Source = "source", Target = "target", Value = "value", NodeID = "name")


Sankey_Data = Sankey_Data%>%
  mutate(Source_Num = sapply(Source, function(x){ nodes[match(x, nodes$name), "nodevalue"]}),
         Targ_Num = sapply(Target, function(x){ nodes[match(x, nodes$name), "nodevalue"]}))


sankeyNetwork(Links = SankeyTest, Nodes = nodes, 
              Source = "Source_Num", Target = "Targ_Num", Value = "Value", NodeID = "name")

colnames(nodes)
# Make a Bar Chart

Own_Rent_Data = NC_Data = acs.fetch(geography = geo.make(state = "NC", county = "Mecklenburg"), 
                                    variable = c("B25026_003","B25026_004", "B25026_005", "B25026_006", "B25026_007", "B25026_008",
                                                 "B25026_010","B25026_011", "B25026_012", "B25026_013", "B25026_014", "B25026_015"),
                                    col.names = c("Owned Moved in After 2005", "Owned Between 2000 and 2004",
                                                  "Owned Between 1990 and 1999", "Owned Between 1980 and 1989",  "Owned Between 1970 and 1979", 
                                                  "Owned Before 1969","Rent Moved in After 2005", "Rent Between 2000 and 2004",
                                                  "Rent Between 1990 and 1999", "Rent Between 1980 and 1989",  "Rent Between 1970 and 1979", 
                                                  "Rent Before 1969") )



Own_Rent_DF = data.frame(geography(Own_Rent_Data), estimate(Own_Rent_Data))%>%
  select(-NAME, -state, -county)%>%
  gather(key = Variable,value = Population)%>%
  mutate(Own_Rent = sapply(regmatches(Variable, regexec("^(.*?)\\.", Variable)), function(x) x[2]),
         VAR = sapply(regmatches(Variable, regexec("^.*?\\.(.*)", Variable)), function(x) x[2]) )%>%
  select(-Variable)%>%
  spread(Own_Rent, Population, drop = TRUE)
 

Ownership_Bar = highchart()%>%
  hc_chart(type = "column")%>%
  hc_title(text = "Own vs Rent") %>% 
  hc_xAxis(categories = gsub(".", replacement = " ", Own_Rent_DF$VAR, fixed = TRUE)) %>% 
  hc_add_series(data = Own_Rent_DF$Owned,
                name = "Ownership Population")%>%
  hc_add_series(data = Own_Rent_DF$Rent,
                name = "Renter Population")
  
  
Ownership_Bar

match("Male", nodes$name)
