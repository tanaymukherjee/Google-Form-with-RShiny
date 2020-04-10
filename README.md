# Google-Form-with-RShiny
Here we will walk through the steps required to build a shiny app that mimicks a Google Form. It will allow users to submit responses to some input fields, save their data, and allow admins to view the submitted responses.

## Overview:
The app we will build will be a form collecting data for Graduate students with some mandatory fields and some additional fields to gather more info about students.

The main idea is simple: create a UI with some inputs that users need to fill out, add a submit button, and save the response. Sounds simple, and it is! In this tutorial each response will be saved to a .csv file along with the timestamp of submission. To see all submissions that were made, we simply read all csv files and join them together. There will also be an “admin panel” that will show admin users all previous responses and allow them to download this data.

## How to get your own RStudio Server?
[Follow this detailed link to initiate a copy for yourself] (https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/#shiny-user-perms)

## References:
[I learnt this cool app technique based on Dean Attali's work [here] (https://deanattali.com/2015/06/14/mimicking-google-form-shiny/)
