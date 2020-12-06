# ReproducibilityRaptors
ReproducibilityRaptors Project Team for STAT 5014 Fall 2020 (Danielle Sebring, Ankur Patel, Andy Cooper) The main goal of our project will be to create a real-time dashboard of COVIDcase data using RShiny. We will create an interactive time series figure with the ability for the user to filter through features including but not limited to patient race, patient age, and patient location across Virginia. Finally, we will report summary statistics including but not limited to means, medians, variances, infection rate, and cumulative number of cases and/or deaths. We will write our code in RStudio, save it in GitHub, and make use of version control. We will use Slack as our primary method of communication.

Description of files:
.gitignore is a text file where we could tell git to ignore any files we delete.
COVID Project.Rmd is our initial Rmd file where we collected and cleaned data.
ReproducibilityRaptors.proj is the R project that houses all of our R files.
VDH-COVID-19-PublicUseDataset-Cases_By-Race-Ethnicity.csv is a csv of Virginia Department of Health covid data with information on patient race.
VDH-COVID-19-PublicUseDataset-Cases_By-Sex.csv is a csv of Virginia Department of Health covid data with information on patient sex.
VDH-Population_By_Health_District.csv is a csv of Virginia Department of Health population data per health district as of 2015.
data_preparation.R is our final data cleaning script that gets our data ready for the Shiny app.
shiny_app.R is the code for the actual creation of the Shiny app.
shiny_functions.R is the code for the functions for which the Shiny app will display the output.
va_covid is our cleaned data set saved as an R object.
va_tables is the script for one of the functions (table) that will appear in the Shiny app.
