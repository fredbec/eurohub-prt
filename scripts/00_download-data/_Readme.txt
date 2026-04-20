get-data: 			   	main downloader and munger for forecast data and anomalies
get-* with *\in(truth, population, anomalies): 	simply downloads the respective data
geh-hub-forecasts:			only contains functions, doesn't execute
get-metadata:				builds upon output from 


-------
after downloading all these (running master-get-data), run R/load-data.R to merge all data into single data.table