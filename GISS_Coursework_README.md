## CASA0005 Coursework: A spatial analysis of well-being in London, UK

This spatial analysis and report are part of an assignment for UCL CASA0005 “Geographic Information Systems and Science”.

This project explores the spatial distribution and measurement of well-being in London, UK. 
It examines the relationships between London wards' well-being scores (as measured by Greater London Authority), and their median household income and population density.
The analysis utilises linear and Geographically Weighted Regressions to assess these variables’ potential in enhancing the GLA well-being index.

The affiliated webpage: https://petitsushi.github.io/publishexample/ showing the analysis outputs within the narrative of the report 

The code: https://github.com/PetitSushi/publishexample/blob/main/Assignment_final/GISSAssignment_script.R with all the steps taken to get the output and some comments

The raw and pre-processed data: https://github.com/PetitSushi/publishexample/tree/main/Assignment_final/data 

I had to remove some headers, titles, "£" characters, empty rows and colums from the .xlsx files, transformed into .csv before I moved onto RStudio to continue the cleaning ( it's included in the .R file above). Both raw and manually cleaned datasets are available in the data folder. 

Data sources:

1. London ward well being score: https://data.london.gov.uk/dataset/london-ward-well-being-scores. I used the latest release, 2013. 

2. London geographic boundaries: https://data.london.gov.uk/dataset/super-output-area-population-lsoa-msoa-london Ward level

3. Household income estimates: https://data.london.gov.uk/dataset/household-income-estimates-small-areas. Ward level data, for the year 2012/2013. 

4. Population Density: https://data.london.gov.uk/dataset/land-area-and-population-density-ward-and-borough From the 2011 national census

