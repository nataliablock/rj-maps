# rj-maps
Collecting, cleaning and structuring data for 6 legislative elections in the state of Rio de Janeiro


Objective: create automated, functional R scripts to collect, clean and structure data for legislative elections per party in the state of Rio de Janeiro. Positions: Chamber of Deputies, Senate and State Assembly; electoral years: 1998,2002,2006,2010,2014,2018.  

In this repository you will find the following R scripts:

_coleta_dados.R:_ collects electoral data using the package electionsBR, structures datasets as panels, calculates the percentage of voting per municipality and saves datasets as csv files.

_funcoes.R:_ functions created to automate the transformation of the datasets in order to plot maps (transform panel to spread dataset, merge shapefile to spread dataset, etc). This script should be called as source in the mapas.R script

_mapas.R:_ *In progress!* Merge shapefiles to datasets and plots the final maps. The script funcoes.R should be called as source to appropriately run the functions.

**Scripts documented in Portuguese.**
