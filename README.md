# Using Consumer Wearables to Measure Physical Activity of Nursing Home Residents with Dementia

This is the repository with the code needed to rerun our analysis of the manuscript: "Using Consumer Wearables to Measure Physical Activity of Nursing Home Residents with Dementia", available as preprint here: https://osf.io/preprints/psyarxiv/mqg86


## Data

The data necessary to run this script and to recreate the plots is currently not available. 
There are three types needed
a) aggregated behavioral observations (Medlo Data)
b) raw accelerometer data (Acc Data)
c) aggregated 24hrs log data from the Samsung activity recognition application (24hrs Data)

From the raw accelerometer data one can calculate the MAD derivatives.
With the two aggregated datasets the correlation matrix can be recreated


__Data Contact__

You can contact Daniela Gawehns, the owner of the repro and first author of the manuscript regarding the data: <gawehnsd@liacs.leidenuniv.nl> or <d.gawehns@umcg.nl> 


## Getting Started

The .r file named AnalysisPipelineSTART_HERE.R is the easiest entry point to rerun the analysis. This is where the data gets imported, cleaned and normalized into the format used for the plots. 
The file PlottingScripts.R contains all code used to plot the figures in the manuscript. 


### Requirements

The scrips have the following dependencies: ggplot2, lubridate, haven and eeptools
(eeptools is only used in a script to extract demographic data from participant files, which will not be shared out of privacy reasons)

## Publications

Gawehns, D., Portegijs, S., van Beek, A. P. A., & van Leeuwen, M. (2024, January 26). Using Consumer Wearables to Measure Physical Activity of Nursing Home Residents with Dementia. Retrieved from osf.io/preprints/psyarxiv/mqg86


van Dijk, R.M.K., Gawehns, D. and van Leeuwen, M., 2023. WEARDA: Recording Wearable Sensor Data for Human Activity Monitoring.  Journal of Open Research Software,  11(1), p.13.DOI: https://doi.org/10.5334/jors.454



## License

This project is licensed under CC-BY-SA-4.0. 


