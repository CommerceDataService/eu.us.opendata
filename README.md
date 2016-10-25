# Project EU-US
Joint EU-US R Library focused on extracting data from Eurostat API and BEA API

## Installation
Because this is currently a private repository, installation has an extra step or two:

1. [Generate a GitHub token](https://github.com/settings/tokens)
2. Run the code:
```r 
#Install packages if needed
install.packages(c('devtools', 'httr'));

library(devtools);
library(httr);

httr::set_config( config( ssl_verifypeer = 0L ));

devtools::install_github('CommerceDataService/project-eu-us/eu.us.openR', auth_user='DreaJulca', auth_token=ghToken) 

library(eu.us.openR)

```

## getRel
Using my BEA API key, assigned to the variable beaKey (which should not be necessary in final version), I am able to get the data as a relationship table:
```r
getRel('gdp', lucky = T, beaKey = beaKey)
```

## searchRel
Using my BEA API key, which again should not be necessary in final version (albeit for a different, easier-to-change reason), I am able to return search results as a table:
```{r searchRel}
searchRel('gdp', asHtml = F, beaKey = beaKey)

```

## describeRel
Using a relationship ID, return a description of that relationship as a table (again, beaKey parameter required only for now):
```{r describeRel}
describeRel('<JOINT#GDP_A_2>', asHtml = TRUE, beaKey = beaKey)
```
 