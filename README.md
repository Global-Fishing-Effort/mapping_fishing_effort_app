[![image](https://github.com/user-attachments/assets/7890abd7-61ea-4d13-ab69-a53ed1770d11)](https://doi.org/10.5281/zenodo.15110746)


# [Global Fishing Effort Explorer and Model Data](https://data.mapping-global-fishing.cloud.edu.au/shiny/mapping_fishing_effort_app/)

## Background
Industrial fishing exerts significant pressure on marine ecosystems, influencing fish stock dynamics, biodiversity, and food security on a global scale. Effective fisheries management and marine conservation efforts rely on accurate assessments of fishing effort—the amount of time and resources expended in fishing activities—at appropriate spatial and temporal scales. However, existing datasets on global fishing effort are fraught with limitations, including a lack of spatial resolution in country-level statistics and incomplete coverage in vessel tracking systems. This study aims to address these gaps by developing a novel approach to remapping global industrial fishing effort, integrating FAO country-level fishing effort data, national logbooks, and AIS-derived fishing patterns from the Global Fishing Watch project into a statistically driven spatial allocation framework. 
  
We employ a statistical remapping approach similar to that of [McDonald et al. 2024](https://doi.org/10.1073/pnas.2400592121) that spatially allocates industrial fishing effort based on multiple data sources, including country-level effort data from [Rousseau et al. (2019)](https://doi.org/10.1073/pnas.1820344116), AIS data from [Global Fishing Watch](https://globalfishingwatch.org/map-and-data/), and environmental and geographical factors (e.g., sea surface temperature, chlorophyll-a concentration, bathymetry, distance to productive fishing grounds) as well as governance variables (e.g., EEZ boundaries, FAO Major Fishing Areas, etc.) from different sources. Our approach ensures that the spatial allocation of fishing effort reflects real-world constraints and ecological drivers and enables us to allocate country-level fishing effort data across the global ocean in a way that aligns with observed vessel behavior, known ecological preferences, and regulatory constraints. The resulting dataset offers a 1 degree resolution, making it well-suited for applications in fisheries management, global climate assessments, and marine spatial planning. 


## Shiny app
This repository contains all code developed to produce the shiny app supporting this future publication. The shiny app is designed to allow users to explore fishing effort across space and time. 

The [Global Fishing Effort Explorer Shiny app](https://data.mapping-global-fishing.cloud.edu.au/shiny/mapping_fishing_effort_app/) is under continuous development, but it is ready for public use.

This app provides an interactive platform for exploring and downloading global industrial fishing effort data, allowing users to filter by year, country, gear type, vessel length category, Exclusive Economic Zone (EEZ) and FAO statistical area. Built on a refined dataset integrating country-level fishing effort data, AIS-derived fishing patterns, and environmental and governance variables, the app offers spatially explicit insights into industrial fishing activity. Effort estimates are provided as hours fished, with a spatial resolution of 1° cell, and span the years 1950-2017. Users can visualize maps and trends, compare regional differences, and access data to support research, policy-making, and marine conservation efforts.
  
## How should I use this tool?
This app has two tabs that allow you to visualise and download fishing effort data in different ways:

 - The 'Map' tab allows you to explore spatially explicit industrial effort data globally and for a selected region (EEZ or FAO statistical area). You can also specify the year (between 1950 and 2017), flag country (e.g. Angola, Albania, Argentina), gear type (e.g. bottom trawling, longline), and vessel length category (6-12m, 12-24m, 24-50m, over 50m) you are interested in exploring.
 
 - The ‘Time series’ tab gives you the same options but allows you to explore trends in fishing effort. 
 
## How should I cite data from this site?
You can download the data used to create the plots shown in this interactive tool using the 'Download' button included under each tab. As a condition of this tool to access data, you must cite its use: Clawson, S.G., Novaglio, C., & Blanchard J.L. (2025). Global Fishing Effort Model Data and Shiny App: [10.5281/zenodo.15110746](https://zenodo.org/records/15110746)

## How can I contact you?
If you have any ideas on how to improve this app or if you found any issues, you can “create an issue” in our GitHub repository.
For general enquiry contact Julia Blanchard at julia.blanchard@utas.edu.au 


## Acknowledgments
The development of this app was funded by the Food and Agriculture Organization of the United Nation (FAO). We would also like to acknowledge the use of computing facilities provided by Digital Research Services, IT Services at the University of Tasmania.


