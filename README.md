## PROTOTYPE WARNING


**This is the almost-release version of the ForecastR package**


The [main project repository](https://github.com/SalmonForecastR), 
has all the latest information and links to other project components like the *Shiny App*, and a wiki
with detailed information (e.g. statistical methods).



## What's New?

Since the last major release in the spring of 2020,
the following major updates have been implemented:

* Return Rate (Mechanistic) Model added to the package code
and to the app interface.
* Explore tab options dynamically respond to data set and model selection (e.g. only show sibling regression model options
if data has age classes, menu options for model settings adapt to model selection)
*  Compare tab model selection re-design: have tabs for each model type now, with model-specific options.




## How To Install

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("SalmonForecastR/ForecastR-Package")
```



