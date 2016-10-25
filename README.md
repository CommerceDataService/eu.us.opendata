# Project EU-US
Joint EU-US R Library focused on extracting data from Eurostat API and BEA API

## Installation
Because this is currently a private repository, installation has an extra step or two:

1. [Generate a GitHub token](https://github.com/settings/tokens)
2. Run the code:
```r 
#Install packages if needed
install.packages(c('devtools', 'httr'));

library(httr);
library(devtools);
httr::set_config( config( ssl_verifypeer = 0L ));
devtools::install_github('CommerceDataService/project-eu-us/eu.us.openR', auth_user='[your github user ID]', auth_token='[the token you just got]');
```
