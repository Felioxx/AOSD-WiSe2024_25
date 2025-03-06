library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(sp)
library(spdep)
library(shinyBS)

ui <- fluidPage(
  titlePanel("Distribution of Votes by Groups in the last Elections"),
  tabsetPanel(
    tabPanel("Past Election", 
             # infotext
             actionButton("infoBtn", label = icon("info-circle"), style="background-color: transparent; border-color: transparent;"),
             bsModal("infoModal", "Information", "Close",# size = "small",
                     htmltools::tags$div(class="modal-body", style="max-height: 600px; overflow-y: auto;", 
                                         p("With this application you can analyze the election results of past elections in Münster."),
                                         p("Simply set the “Election Filters” according to your interests and click on “Update Map”."),
                                         p("The map on the right shows the election results by electoral district. Below the map you can also see the corresponding analysis results of Global Moran I. You can find an explanation of how to interpret these here: https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html "),
                                         p("If you wish, you can also set the “Personal Filters”, which consist of social population indicators. If you then click on the corresponding electoral distric on the map, you will see these results. You can then use them to identify correlations between the election results and the social factors by yourself or use the tab 'Influence of the Variables'."),
                                         p("If you want to get an overview about the election results in whole Münster, use the Link below.")
                     )),
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Election Filters"),
                 selectInput(
                   inputId = "selected_election",
                   label = "Choose an Election:",
                   choices = c("European Election", "Bundestag Election"),
                   selected = "European Election"
                 ),
                 selectInput(
                   inputId = "party_votes",
                   label = "Choose a Party:",
                   choices = list("Union", "SPD", "Grüne", "FDP", "AFD", "Volt", "Linke", "Die PARTEI", "Tierschutzpartei", "Familienpartei", "Freie Wähler", "ÖDP", "BSW", "PdF"),
                   selected = "Volt"
                 ),
                 div(style="margin-bottom:40px"),
                 h3("Personal Filters"),
                 selectInput(
                   inputId = "gender",
                   label = "Select Gender:",
                   choices = list("total", "female", "male"),
                   selected = "total"
                 ),
                 selectInput(
                   inputId = "age",
                   label = "Select Age:",
                   choices = list("Children (0-14 years)", "Adults (15-64 years)"),
                   selected = "Adults (15-64 years)"
                 ),
                 checkboxInput(inputId = "unemployed", label = "unemployed (SGBII)"),
                 conditionalPanel(
                   condition = "input.age === 'Children (0-14 years)'",
                   checkboxInput(inputId = "migration", label = "migration background"),
                   conditionalPanel(
                    condition = "input.gender === 'total'",
                    checkboxInput(inputId = "single_parent", label = "in a single-parent household")
                   )
                 ),
                 div(style="margin-bottom:40px"),
                 actionButton("update_map", "Update Map"),
                 div(style="margin-bottom:60px"),
                 tags$div(style = "text-align: left;",
                          conditionalPanel(
                            condition = "input.selected_election === 'Bundestag Election'",
                            tags$a(href = "https://wahlen.citeq.de/20210926/05515000/praesentation/ergebnisgrafik.html?wahl_id=39&stimmentyp=1&id=ebene_3_id_72", "Link to Bundestag election statistic of Münster")
                          ),
                          conditionalPanel(
                            condition = "input.selected_election === 'European Election'",
                            tags$a(href = "https://wahlen.citeq.de/20240609/05515000/praesentation/ergebnisgrafik.html?wahl_id=1000039&stimmentyp=0&id=ebene_3_id_72", "Link to European election statistic of Münster")
                          )
                 )
               ),
               mainPanel(
                 leafletOutput("map", height = 500),
                 htmlOutput("MoranI")
               ),
             ),
    ),
    tabPanel("Influence of the variables",
             # infotext
             actionButton("infoBtn2", label = icon("info-circle"), style="background-color: transparent; border-color: transparent; margin-left: 120px;"),
             bsModal("infoModal2", "Information", "Close",
                     htmltools::tags$div(class="modal-body", style="max-height: 600px; overflow-y: auto;", 
                                         p("With this application you can analyze the election results of past elections in Münster."),
                                         p("Here you can see Scatterplots of the data. The regression line shows whether there could be a correlation between the variables and the party's election results. You can find more information on how to interpret these plots here: https://statisticsbyjim.com/graphs/scatterplots/"),
                                         p("You can then use them to identify correlations between the election results and the social factors.")
                     )),
             div(style="margin-bottom:40px"),
             mainPanel(
               plotOutput("scatterPlot", height = "600px", width = "800px"),
               plotOutput("scatterPlot2", height = "600px", width = "800px"),
               plotOutput("scatterPlot3", height = "600px", width = "800px")
             )
    )
  )
)