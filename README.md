# Shiny R application

Project prepared as part of the ['Advanced Visualisation in R'](https://usosweb.wne.uw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&kod=2400-DS2AV) subject on my Master Studies.

The goal of the project was to prepare a web application based on the use of various data sources.

After opening the application, current data is automatically downloaded from the WHO website. In the project, I presented a visualization using the following libraries:
- shiny,
- ggplot2,
- sf,
- rnaturalearth,
- ggmosaic,
- shinydashboard,
- shinyjs,
- bubbles,
- rgdal,
- leaflet,
- ggalluvial.
And other important libraries: dplyr, tidyverse.
We can see an interactive map. We can also choose the date for which we want to see the data, select the variable from among "Cumulative cases", 'Cumulative deaths', 'New deaths', 'New cases' for which we want to see the graphs, highlight the most important statistics, select the country for which we want to do the analysis. The symptom analysis is also interesting. We can see which symptoms are specific to each age group or gender.
