# Project EU-US
Joint EU-US R Library focused on extracting data from Eurostat API and BEA API


## Example Usage of SPARQL/rsdmx to retrieve dataflow and DSD
```r
SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#Declare dataset name

dataset.name<-"nama_10r_2gdp"

#Retrieve dataflow and DSD

dataflow <- readSDMX(providerId = "ESTAT", agencyId="ESTAT", resource = "dataflow", resourceId=dataset.name)@dataflows[[1]]

dsd.name<- dataflow@dsdRef
dsd <- readSDMX(providerId = "ESTAT", agencyId="ESTAT", resource = "datastructure", resourceId=dsd.name)
data.structure<-dsd@datastructures@datastructures[[1]]

code.lists<-dsd@codelists@codelists

print("Handling Dataflow...")
Dataflow_To_RDF(SPARQL.endpoint, dataflow)

print("Handling DSD...")
DSD_To_RDF(SPARQL.endpoint, data.structure)

print("Handling Codelists...")
CodeList_To_RDF(SPARQL.endpoint, code.lists)

```