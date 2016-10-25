# Function to join readSDMX() response and beaGet() (regional) response 

#sdmx <- readSDMX(providerId = "ESTAT", resource = "data", flowRef="cdh_e_fos", key = list("A", "", "PC", "FOS1", ""))
#beaDT <- beaGet(list('userid' = apiKey, 'Method' = 'GetData', 'DatasetName' = 'RegionalIncome', 'LineCode' = '1200', 'GeoFIPS' = 'STATE', 'frequency' = 'A', 'year' = 'all','TableName' = 'CA25N'), asWide = FALSE)

statMerge <- function(sdmx, beaDT) {

	requireNamespace('data.table', quietly = TRUE)
	euDT <- as.data.table(sdmx)
	euDT[, Code := sdmx@dsdRef]
	usEUDT <- beaDT[ GeoName != 'United States',.(
		obsValue = DataValue,
		obsTime = TimePeriod,
		UNIT = CL_UNIT, 
		GEO = GeoName,
		Code
	)]
	
	outDT <- rbindlist(
		list(
			euDT[, .(
				Code, GEO, obsValue, UNIT, obsTime
			)],
			usEUDT
		), use.names = TRUE, fill = TRUE
	)
	
	return(outDT)
	
}