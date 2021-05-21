# Marine App Demo

This Shiny app is a short demo project for Appsilon, and helps users to find out the largest distance traveled by a ship between two observation points.

## Getting Started

Go to view the live demo in [ShinyApps](https://cselefendi.shinyapps.io/marineapptested/)

### How to use

It's very easy to use the app: simply select a vessel type and a vessel by name below that, and the map will automatically refresh and show you the largest distance traveled.

![First look](screenshot_empty.png "First look")

Feel overwhelmed by the number of ships? Click on I'm feeling lucky to randomly select a ship from the given type and view the map. It's the easy way to do a random walk through the ships in the data.

![Type already selected](screenshot_typeselected.png "Name to be chosen look")

Once you selected the vessel by name, the map will refresh and show you the route that belongs to the largest distance traveled.

![Name already selected](screenshot_nameselected.png "Full look")


### Bugs and testing

There are no known bugs at this time, and the project is lightly tested.
In case you ran into a problem, contact the developer: bszuoperation at gmail.com

## Built with

R package requirements:

general
- shiny 1.6.0
- shiny.semantic 0.4.2
- shinyjs 2.0.0
- shinycssloaders 1.0.0
- tidyverse 1.3.1
data preprocessing
- geodist 0.0.7
map
- leaflet 2.0.4.1
testing
- testthat 3.0.2

## Authors

* **Endre Szolnoki** - 2021, Budapest

The demo project designed to match Appsilon's test assignment.

## License

No licence

## Acknowledgments

* I'm feeling lucky: First time that I used the thing I developed much more than the Google original.

