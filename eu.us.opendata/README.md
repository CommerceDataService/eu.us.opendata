# Project EU-US
Joint EU-US R Library focused on extracting data from Eurostat API and BEA API

## Installation
Because this is currently a private repository, installation has an extra step or two:

1. [Generate a GitHub token](https://github.com/settings/tokens)
2. Run the code:
```{r install} 
#Install packages if needed
install.packages(c('devtools', 'httr'));

library(devtools);
library(httr);

httr::set_config( config( ssl_verifypeer = 0L ));

devtools::install_github(
	'CommerceDataService/eu.us.opendata/eu.us.opendata', 
	auth_user = '[your github username]', 
	auth_token = '[the token you just made]'
) 

library(eu.us.opendata)

```

## getRel
Using my BEA API key, assigned to the variable beaKey (which may not be necessary in final version), get the data as a relationship table:
```{r getRel}
getRel('gross domestic product', lucky = T, beaKey = beaKey)
getRel('gdp', lucky = T, beaKey = beaKey)
```

## searchRel
Return search results as a table:
```{r searchRel}
searchRel('gdp')

```

## describeRel
Using a relationship ID, return a description of that relationship as a table:
```{r describeRel}
describeRel('<JOINT#GDP_A_2>', asHtml = TRUE)
```
 
## listRel
 List the relationships available using a direct SPARQL query of the (online) metadata store (should work so long as the query is very small, as it is in this case). 
```{r listRel}
listRel(asHtml = FALSE)
```

 
## geoMap
 Using the retrieved dataset from getRel(), returns either (1) a harmonized shapefile of EU and US geographies with the dataset joined for a selected year, (2) an interactive web-enabled leaflet map.
```{r geoMap}
## Look at state/NUTS2 level data:
dataset <- getRel('<JOINT#GDP_A_2>', lucky = F, beaKey = beaKey)

geoMap(dataset, 2014) ## As leaflet map

geoMap(dataset, 2014, asSHP = TRUE) ## As shapefile

## Look at metro level data:
dataset = getRel('gdp', lucky = T, beaKey = beaKey)

geoMap(dataset, 2012) ## As leaflet map

geoMap(dataset, "all") ##As a shapefile with all years in the data

```

##timeSync
Provides user easier access to extracts the latest overlapping year of data or all overlapping years.
```{r timeSync}
timeSync(dataset, 1) #latest
timeSync(dataset, 2) #all overlapping years
```
