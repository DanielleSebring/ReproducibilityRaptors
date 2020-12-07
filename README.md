Note: This application is also available to use on shinyapps.io: https://andyc.shinyapps.io/ReproducibilityRaptors/.

# ReproducibilityRaptors
ReproducibilityRaptors Project Team for STAT 5014 Fall 2020 (Danielle Sebring, Ankur Patel, Andy Cooper) The main goal of our project will be to create a real-time dashboard of COVID-19 case data using RShiny. We will create an interactive time series figure with the ability for the user to filter through features including but not limited to patient race, patient age, and patient location across Virginia. Finally, we will report summary statistics including but not limited to means, medians, variances, infection rate, and cumulative number of cases and/or deaths. We will write our code in RStudio, save it in GitHub, and make use of version control. We will use Slack as our primary method of communication.

Description of files:
1) data_preparation.R is our final data cleaning script that loads, cleans, merges, and prepares the data ready for the Shiny app.

2) shiny_functions.R is the code for the functions for which the Shiny app will call to display the output.

3) shiny_app.R is the code for the actual creation of the Shiny app.

4) data/VDH-Population_By_Health_District.csv contains the latitude, longitude, and population recorded in 2015 of Virginia's health districts.

This shiny application uses data collected and made publically available by the Virginia Department of Health (https://www.vdh.virginia.gov/coronavirus/)
