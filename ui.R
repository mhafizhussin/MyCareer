# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Developed with R version 3.3.2 (64-bit)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(shinythemes)
library(DT)
library(visNetwork)
library(rintrojs)
library(shinyWidgets)

source("appPanel.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = "MYCareer",
                   theme = shinytheme("flatly"),
                   # theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,

                   tabPanel("HOME", value = "home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                                tags$link(rel = "icon", 
                                          type = "image/png", 
                                          href = "images/logo_icon.png")
                            )
                   ),
                   tabPanel( "PERSONALITI-KERJAYA", value = "careerList",
                             jobListing(),
                             
                   ),
                   tabPanel("PERANCANGAN-KERJAYA", value = "careerPF",
                            
                            careerPath(),
                   )
                   
                )
        
            )