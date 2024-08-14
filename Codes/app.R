# Define UI for application 
# Service Planning App

source("rshiny_tidy_data.R")

# data.frame with credentials info
credentials <- data.frame(
  user = c("sdcbhs"),
  password = c("plan101!"),
  stringsAsFactors = FALSE
)
#### Generate UI #####
ui <- secure_app(tags_top = tags$h4("Community Experience Partnership: Service Planning Tool"),
                 
                 fluidPage(
                   useShinyjs(),
                   tags$head(
                     tags$style(HTML("
      .scroll-box {48774
        max-height: 300px;
        overflow-y: auto;
      }
        .custom-h2 {
        font-family: 'Open Sans', sans-serif;
        font-size: 24px;
        color: #4D4D4D;
        font-weight: bold; 
        text-align: center;
        }
      
      .custom-h3 {
        font-family: 'Open Sans', sans-serif;
        font-size: 20px;
        color: #4D4D4D;
        font-weight: bold; 
      }
      .custom-filter {
        font-family: 'Open Sans', sans-serif;
        font-size: 14px;
        color: #4D4D4D;
      }
      .custom-p {
        font-family: 'Open Sans', sans-serif;
        font-size: 16px;
        color: #1A1A1A;
      }
    "))
                   ),
                   
                   tags$script(
                     HTML(
                       "
      function showTab(tabName) {
        $('.nav li a').filter(function() {
          return this.text == tabName;
        })[0].click();
      }
      "
                     )
                   ),
                   navbarPage("Community Experience Partnership: Service Planning Tool", collapsible = TRUE, inverse = TRUE, 
                              theme = shinytheme("yeti"),
                              tags$head(
                                tags$style(HTML("
            .filter-panel {
            display: flex;
            align-items: center;
          }
            .info-icon {
            margin-left: 5px;
            }
              "))),
                              
                              id = "tabs",
                              #navbarMenu("About",
                              navbarMenu("More Information",
                                         
                                         tabPanel("Overview",
                                                  
                                                  div(
                                                    style = "text-align:center;",  # This will center all the elements inside the div
                                                    h2(class = "custom-h2", "Welcome to the San Diego County", 
                                                       br(),
                                                       "Behavioral Health Service Planning Tool"),
                                                    img(
                                                      src = "cep_logo.png", 
                                                      height = "150px", 
                                                      width = "150px", 
                                                      alt = "Logo"
                                                    )  # Place your logo here
                                                  ),  
                                                  p(class = "custom-p", 'The ',
                                                    a(
                                                      "Community Experience Partnership", 
                                                      href = "#", 
                                                      onclick = "showTab('About the CEP');"
                                                    ),
                                                    
                                                    'Service Planning Tool is a custom application designed by UC San Diego in collaboration with County of San Diego Behavioral Health Services. The goals of the tool are to help ensure service provision is informed by data, based in cultural and regional considerations, and focused on communities that may be at greatest risk for unmet behavioral health need. The application uses data to help planners at BHS identify areas in San Diego County where target populations are likely to be highly concentrated. Once areas are identified, users may explore community profiles and download custom reports that summarize the social, economic, and demographic profiles of the selected regions.'),
                                                  br(),
                                                  p(class = "custom-p", 'The tool also showcases the ',
                                                    a(
                                                      "Behavioral Health Equity Index (BHEI)", 
                                                      href = "#", 
                                                      onclick = "showTab('About the BHEI');"
                                                    ),
                                                    ', a composite index designed to identify areas at risk for unmet behavioral health needs. Areas with higher BHEI scores may not have access to the resources and services that promote behavioral health. These areas may serve as priority zones for behavioral health equity work and service enhancements.'
                                                  ),
                                                  div(style = "text-align:center;",  # This will center the button divs
                                                      div(
                                                        style = "display:inline-block; padding: 0 10px;",  # This will place the buttons next to each other with some space
                                                        actionButton("visit_tool", "View the Tool")
                                                      ),
                                                      div(
                                                        style = "display:inline-block; padding: 0 10px;",  # This will place the buttons next to each other with some space
                                                        actionButton("learn_tool", "How to Use the Tool")
                                                      )
                                                      
                                                  ),
                                                  br(),
                                                  
                                                  # Add the data note here
                                                  p(class = "custom-p;",'Updated June 17, 2024')
                                                  )
                                         
                                         ,
                                         
                                         
                                         
                                         tabPanel("About the CEP",
                                                  br(), 
                                                  div(
                                                    style = "text-align:center;", 
                                                    img(
                                                      src = "cep_logo.png", 
                                                      height = "200px", 
                                                      width = "200px", 
                                                      alt = "Logo"
                                                    ) , # Place your logo here
                                                    
                                                    br(),
                                                    
                                                    h2(class= "custom-h2", "About the Community Experience Partnership (CEP)")),
                                                  br(),
                                                  
                                                  p(class= "custom-p", "The Community Experience Partnership (CEP) is a joint initiative between County of San Diego Behavioral Health Services (BHS) and UC San Diego.  The goal of the CEP is the integration of data and community engagement to promote behavioral health equity in San Diego County. 
                        The ", tags$a(href="https://www.communityexperiencepartnership.com", "CEP Dashboards"), " allow users to explore, monitor, and visualize behavioral health equity data through a series of interactive dashboards. The Service Planning Tool is another product of the CEP."),
                                         ),
                                         tabPanel("About the BHEI",
                                                  br(),
                                                  h2(class= "custom-h2","About the Behavioral Health Equity Index (BHEI)"),
                                                  br(),
                                                  h3(class= "custom-h3","What is the BHEI?"),
                                                  
                                                  p(class= "custom-p", "The BHEI is a data-driven tool that allows users to explore differences in the root causes (also known as social determinants) of behavioral health across neighborhoods in San Diego County. Because the social determinants of behavioral health are multifaceted and complex, the BHEI is a composite index which combines information from multiple sources into a single score. This is a valuable tool to summarize data in a way that is interpretable and can help build community consensus for action. Understanding where inequities exist in our community is a first step towards identifying and addressing the policies, laws, and services that may contribute to behavioral health disparities."),
                                                  br(),
                                                  h3(class= "custom-h3","How is the BHEI constructed?"),
                                                  p(class= "custom-p", "The BHEI is constructed from over 30 individual variables (also known as indicators), which are organized into 8 domains that map to the social determinants of health. Indicators are drawn from over ten different data sources including the US Census Bureau’s American Community Survey, CDC’s PLACES, and the Opportunity Atlas. After normalizing, weighting, and aggregating the variables, an equity score is calculated for each of the census tracts, zip codes (ZCTAs), Subregional Areas (SRAs), and HHSAs in San Diego County. Each neighborhood is then assigned a rank based on its equity score. The indicators, domains, and weights were developed in partnership with local Subject Matter Experts, including community representatives."),
                                                  br(),
                                                  h3(class= "custom-h3","What does the BHEI score mean?"),
                                                  p(class= "custom-p", "Areas with higher BHEI scores may not have access to the resources and services that promote behavioral health. These areas may serve as priority zones for equity work and service enhancements."),
                                                  br(),
                                                  h3(class= "custom-h3","What are the limitations of the BHEI?"),
                                                  p(class= "custom-p", "The BHEI is not intended to be applied or interpreted without context. The ranks do not reflect the strengths, values, or priorities of neighborhoods or regions and the individuals who live there. While the BHEI can help users identify neighborhoods that may benefit from service enhancements and quality improvement efforts, final decisions about needs, policy, and resourcing would require community outreach and local understanding of communities.  For additional limitations, please review the Technical Report."),
                                                  br(),
                                                  h3(class= "custom-h3","Where can I get more information about the BHEI"),
                                                  p(class= "custom-p", "For more information on the construction and interpretation of the BHEI, please review the Technical Report."),
                                                  
                                                  div(
                                                    style = "text-align: center;", 
                                                    downloadButton("downloadReport", "Technical Report")
                                                    
                                                  ),
                                                  br(),
                                                  br(),
                                                  br()
                                         ),
                                         tabPanel("Learning Resources and Data Notes",
                                                  
                                                  h2(class= "custom-h2", "Learning Resources"),
                                                  br(), 
                                                  p(class= "custom-p", "For more information about how to use the Service Planning Tool, please download the User's Manual."),
                                                  downloadButton("downloadManual", "Download SPT User's Manual"),
                                                  br(),
                                                  br(),
                                                  p(class= "custom-p", "For technical details about the data, sample, and methods, please download the Technical Notes."),
                                                  downloadButton("downloadNotes", "Download SPT Technical Notes"),
                                                  
                                                  
                                                  br(),
                                                  br(),
                                                  p(class= "custom-p", "For data notes about each of the target populations included in the Service Planning Tool, view or download the table below."),
                                                  DT::dataTableOutput('tech_notes_render')
                                         ),
                                         tabPanel(
                                           "Contact Us",
                                           
                                           # Center the content using tags$div and CSS
                                           tags$div(
                                             style = "text-align: center;",
                                             br(),
                                             h2(class = "custom-h2", "Contact Us"),
                                             br(),
                                             p(class = "custom-p", "Please submit questions and suggestions to the BHS Clinical Design Team at: "),
                                             a(href = "mailto:BHSClinicalDesign.HHSA@sdcounty.ca.gov?subject=BHS Clinical Design Inquiry", target = "_blank", "BHSClinicalDesign.HHSA@sdcounty.ca.gov")
                                           )
                                         )
                                         
                                         
                              ),
                              
                              
                              #### How to use the Tool ####
                              tabPanel("View the Tool",
                                       fluidPage(
                                         
                                         ##### Sidebar Display ####
                                         sidebarPanel(
                                           h3(class= "custom-h3", "Make your Selections"),
                                           
                                           ###### Data Types #####
                                           
                                           # Your existing div for 'select_data'
                                           shiny::div(
                                             pickerInput("select_data", 
                                                         label = div(class = "custom-filter", "Select a data type"),
                                                         choices = sort(unique(map_app$source_filter)), 
                                                         options = list(
                                                           "actions-box" = FALSE,
                                                           "live-search" = TRUE, 
                                                           "max-options" = 1, 
                                                           "selectAll" = FALSE
                                                         ), 
                                                         multiple = FALSE,
                                                         selected = "MH Service Data: All Clients, FY 2022-2023"),
                                             title = "Select a data type to filter the options for target populations.",
                                             class = "my-tooltip"
                                           ),
                                           
                                           # ConditionalPanel for 'select_pop'
                                           
                                           shiny::div(
                                             pickerInput("select_pop",
                                                         label = div(class = "custom-filter", "Select a target population"),
                                                         choices = list(), # empty initially
                                                         options = list(
                                                           "actions-box" = FALSE,
                                                           "live-search" = TRUE,
                                                           "max-options" = 1,
                                                           "selectAll" = FALSE
                                                         ),
                                                         multiple = TRUE
                                                         # 'selected' will be set dynamically in server
                                             ),
                                             title = "Select a target population based on the chosen data type.",
                                             class = "my-tooltip"
                                           ),
                                           
                                           
                                           
                                           ###### Equity Lens #####
                                           uiOutput("risk_input"),
                                           
                                           ###### HHSA Regions #####
                                           shiny::div(pickerInput("select_region",
                                                                  label = div(class = "custom-filter", "Limit to HHSA region(s) of interest"),
                                                                  choices = sort(unique(map_app$hhsa_nm)), 
                                                                  options = list(
                                                                    "actions-box" = TRUE,
                                                                    "Selected-text-format" = 'count > 1'), 
                                                                  multiple = TRUE,
                                                                  selected = c("Central", "East", "North Central", "North Coastal", "North Inland", "South")),
                                                      title = "About: BHS services often focus on a subset of the 6 HHSA regions. Adjust selections to limit to HHSA regions of interest.",
                                                      class = "my-tooltip"),
                                           
                                           ###### Geography type #####
                                           shiny::div(pickerInput("select_type",
                                                                  label = div(class = "custom-filter", "Select a geography"),
                                                                  choices = sort(unique(map_app$type)), 
                                                                  options = list("actions-box" = FALSE), 
                                                                  multiple = FALSE,
                                                                  selected = "Zip Code (ZCTA)"), 
                                                      title = "About: Select from four different geographies (HHSA, SRA, Zip Code, census tract). For more information about these geographies see the How to Use the Tool page",
                                                      class = "my-tooltip"),
                                           
                                           
                                           
                                           ###### Slider #####
                                           uiOutput("dynamicLabel"), 
                                           
                                           shiny::div(uiOutput("slider"),
                                                      title = "About: Adjust the slider to limit the number of geographies displayed.",
                                                      class = "my-tooltip"),
                                           
                                           ###### Locations #####
                                           conditionalPanel(
                                             condition = "input.select_type !== 'HHSA Region'", 
                                             shiny::div(pickerInput(inputId = "selected_locations",
                                                                    label = div(class = "custom-filter", "Select location(s) by name:"),
                                                                    choices = NULL,
                                                                    selected = NULL,
                                                                    multiple = TRUE,
                                                                    options = list(
                                                                      "actions-box" = TRUE,
                                                                      "live-search" = TRUE)),  
                                                        title = "About: Select geographies by name using this dropdown menu and search box. Geographies can also be (de)selected by clicking on the map.",
                                                        class = "my-tooltip")),
                                           
                                           
                                           # create an action button with the label "Reset"
                                           actionButton("reset", "Reset")
                                           
                                           
                                           
                                           
                                         ),
                                         
                                         ##### Main Panel Display ####
                                         mainPanel(
                                           
                                           ###### Map #####
                                           tabsetPanel(
                                             tabPanel("Map",
                                                      ###### List of Selections #####         
                                                      br(), 
                                                      uiOutput("selected_outcome_desc"),
                                                      br(),
                                                      box(leafletOutput("map", height = 600), width = 800),
                                                      # Divider and Selected Areas Box
                                                      
                                                      div(class = "filter-box",
                                                          div(class = "scroll-box",
                                                              class = "custom-filter",
                                                              p(strong("The following areas are currently selected:"),
                                                                htmlOutput(class = "custom-filter","list_locations_map")
                                                              )
                                                          )
                                                      ),
                                                      
                                                      
                                                      
                                             ),
                                             
                                             
                                             ###### Data Table #####
                                             tabPanel("Data Table",
                                                      br(), br(), br(),
                                                      DT::dataTableOutput('servtable'),
                                                      
                                                      # Display the sentence from map_app$sprs_name under the map
                                                      htmlOutput(class = "custom-filter", "sprs_name_table"), # Add this line
                                                      
                                             ),
                                             
                                             ### Community Profiles #####
                                             tabPanel("Community Profiles",
                                                      br(),
                                                      
                                                      h3(class= "custom-h3", "Learn more about the communities you selected"),
                                                      
                                                      p(class= "custom-p","Use the dropdown menu to select a community or client indicator. The chart compares the areas you have selected to clients or populations in all of San Diego County. This can help highlight the regional and cultural characteristics of different areas."), 
                                                      
                                                      br(),
                                                      fluidRow(
                                                        
                                                        
                                                        ###### Source_community Data #####
                                                        column(6, shiny::div(
                                                          pickerInput("select_data_2", 
                                                                      label = div(class = "custom-filter", "Select a data type"),
                                                                      choices = sort(setdiff(unique(map_app$source_filter), "Behavioral Health Equity Index (BHEI)")), 
                                                                      options = list(
                                                                        "actions-box" = FALSE,
                                                                        "live-search" = TRUE, 
                                                                        "max-options" = 1, 
                                                                        "selectAll" = FALSE
                                                                      ), 
                                                                      multiple = FALSE,
                                                                      selected = "MH Service Data: All Clients, FY 2022-2023"),
                                                          title = "Select a data type to filter the options for target populations.",
                                                          class = "my-tooltip"
                                                        )
                                                        ),
                                                        
                                                        ###### pop_1 community data #####
                                                        column(6, 
                                                               shiny::div(
                                                                 pickerInput("select_pop_2",
                                                                             label = div(class = "custom-filter", "Select a target population"),
                                                                             choices = list(), # empty initially
                                                                             options = list(
                                                                               "actions-box" = FALSE,
                                                                               "live-search" = TRUE,
                                                                               "max-options" = 1,
                                                                               "selectAll" = FALSE
                                                                             ),
                                                                             multiple = TRUE
                                                                             # 'selected' will be set dynamically in server
                                                                 ),
                                                                 title = "Select a target population based on the chosen data type.",
                                                                 class = "my-tooltip"
                                                               ))
                                                      ),
                                                      
                                                      br(),
                                                      
                                                      fluidRow(  # start a new fluidRow
                                                        column(12,
                                                               plotlyOutput("bar_chart", height = "500px")  # set a specific height
                                                        )
                                                      ),
                                                      fluidRow(  # start a new fluidRow for the list_locations_map_2 box
                                                        column(12,
                                                               div(class = "filter-box",
                                                                   div(class = "scroll-box custom-filter",
                                                                       p(strong("The following areas are currently selected:"),
                                                                         htmlOutput(outputId = "list_locations_map_2")
                                                                       )
                                                                   )
                                                               )
                                                        )
                                                      )
                                                      
                                             ), 
                                             ###### Report #####
                                             tabPanel("Generate Report",
                                                      br(), br(),
                                                      tags$div(class="custom-h3", strong("Generate Reports")),
                                                      tags$p(class="custom-p", "Your report will include data for the following area(s) of focus. You may modify your selections using the filters (left)."),
                                                      fluidRow(id = "border",
                                                               htmlOutput("list_locations_report"),
                                                               tags$head(tags$style("#list_locations_report{color: #1A1A1A;; font-size: 16px; font-family: 'Open Sans', sans-serif; }"))
                                                      ),
                                                      
                                                      tags$style(HTML(" #border { border: 2px double lightgrey;}")),
                                                      
                                                      br(), br(), 
                                                      tags$div(class="custom-h3", strong("Generate a Key Findings Report")),
                                                      tags$p(class="custom-p", "The Key Findings Report is a brief summary report providing key statistics for the selected areas. If more than one area is selected, the data are aggregated across all selections. Selected areas are compared to San Diego County."),
                                                      downloadButton("report_short", "Download Key Findings Report"),
                                                      br(), br(), br(),
                                                      tags$div(class="custom-h3", strong("Generate a Detailed Report")),
                                                      tags$p(class="custom-p", "The Detailed Report is a comprehensive summary of outcomes for the selected areas and comparison areas. Use the options below to define aggregation rules and select one or more custom comparison area."),
                                                      downloadButton("report", "Download Detailed Report"),
                                                      br(), br(),
                                                      tags$p(class="custom-p", strong("Customize your Detailed Report")),
                                                      
                                                      fluidRow(id = "border",
                                                               useShinyjs(),
                                                               shinyjs::hidden(
                                                                 tags$head(tags$style(HTML(".checkbox {margin-left:15px}"))),
                                                                 checkboxInput("aggregate_selections",
                                                                               "Aggregate data across all selected regions", 
                                                                               TRUE)),
                                                               
                                                               shinyjs::hidden(
                                                                 checkboxInput("separate_selections",
                                                                               "Show data for each region separately", 
                                                                               TRUE)),
                                                               
                                                               
                                                               checkboxInput("cnty_total", 
                                                                             "Compare to San Diego County", 
                                                                             TRUE)
                                                      ),
                                                      
                                                      fluidRow(id = "border",
                                                               h6("Add additional comparators"),
                                                               column(3,
                                                                      
                                                                      pickerInput("select_hhsa_rmd",
                                                                                  label = div("Compare to one or more HHSA region", style = "color:#1A1A1A;"),
                                                                                  choices = sort(unique(map_app$hhsa_nm)), 
                                                                                  #choices = NULL,
                                                                                  options = list(
                                                                                    "actions-box" = TRUE,
                                                                                    "Selected-text-format" = 'count > 1'), 
                                                                                  multiple = TRUE,
                                                                                  selected = NULL),
                                                                      
                                                                      
                                                                      useShinyjs(),
                                                                      shinyjs::hidden(
                                                                        tags$head(tags$style(HTML(".checkbox {margin-left:15px}"))),
                                                                        checkboxInput("aggregate_hhsa",
                                                                                      "Aggregate HHSA regions", 
                                                                                      TRUE)),
                                                                      
                                                                      shinyjs::hidden(
                                                                        checkboxInput("separate_hhsa",
                                                                                      "Show data for each region separately", 
                                                                                      TRUE)),
                                                                      
                                                               ),
                                                               column(3,
                                                                      pickerInput("select_sra_rmd",
                                                                                  label = div("Compare to one or more SRA", style = "color:#1A1A1A;"),
                                                                                  choices = map_app_type[[2]][2], 
                                                                                  options = list(
                                                                                    "actions-box" = TRUE,
                                                                                    "live-search" = TRUE,
                                                                                    "Selected-text-format" = 'count > 1'), 
                                                                                  multiple = TRUE,
                                                                                  selected = NULL),
                                                                      
                                                                      useShinyjs(),
                                                                      shinyjs::hidden(
                                                                        tags$head(tags$style(HTML(".checkbox {margin-left:15px}"))),
                                                                        checkboxInput("aggregate_sra",
                                                                                      "Aggregate SRAs", 
                                                                                      TRUE)),
                                                                      
                                                                      shinyjs::hidden(
                                                                        checkboxInput("separate_sra",
                                                                                      "Show data for each SRA separately", 
                                                                                      TRUE)),
                                                               ),
                                                               column(3,
                                                                      pickerInput("select_zcta_rmd",
                                                                                  label = div("Compare to one or more zip code", style = "color:#1A1A1A;"),
                                                                                  choices = map_app_type[[3]][2], 
                                                                                  options = list(
                                                                                    "actions-box" = TRUE,
                                                                                    "live-search" = TRUE,
                                                                                    "Selected-text-format" = 'count > 1'), 
                                                                                  multiple = TRUE,
                                                                                  selected = NULL),
                                                                      useShinyjs(),
                                                                      shinyjs::hidden(
                                                                        tags$head(tags$style(HTML(".checkbox {margin-left:15px}"))),
                                                                        checkboxInput("aggregate_zcta",
                                                                                      "Aggregate zips", 
                                                                                      TRUE)),
                                                                      
                                                                      shinyjs::hidden(
                                                                        checkboxInput("separate_zcta",
                                                                                      "Show data for each zip separately", 
                                                                                      TRUE)),
                                                               ),
                                                               
                                                      ))
                                             
                                           )
                                         ),
                                         
                                         
                                       )),
                              
                              
                   )
                 )
)

#### Server ######


server <- function(input, output, session){
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  observeEvent(input$visit_tool, {
    runjs("showTab('View the Tool');")
  })
  
  
  observeEvent(input$learn_tool, {
    runjs("showTab('Learning Resources and Data Notes');")
  })
  
  
  # Download button handler for USer's Manual
  output$downloadManual <- downloadHandler(
    filename = "SPT User's Manual.pdf",
    content = function(file) {
      # Copy the PDF file to the temporary directory for download
      file.copy("SPT User's Manual.pdf", file)
    }
  )
  
  output$downloadNotes <- downloadHandler(
    filename = "SPT Technical Notes.pdf",
    content = function(file) {
      # Copy the PDF file to the temporary directory for download
      file.copy("SPT Technical Notes.pdf", file)
    }
  )
  
  ##### Add text for input list ######
  
  output$list_locations_map <- renderText({ 
    vals_3 <- combine_words(input$selected_locations)
    
  })
  
  
  
  
  output$list_locations_map_2 <- renderText({ 
    vals_3 <- combine_words(input$selected_locations)
    
  })
  
  output$list_locations_report <- renderText({ 
    vals_3b <- combine_words(input$selected_locations)
  })
  
  
  output$min_max <- renderText( {
    vals <- paste(input$select_pop, collapse = " ")
  })
  
  
  #### Enable Reset button ####
  observeEvent(input$reset, {
    updatePickerInput(session, "select_data", choices = c(), selected = "MH Service Data: All Clients, FY 2022-2023")
    updatePickerInput(session, "select_pop", choices = c(), selected = "All Clients Served")
    updatePickerInput(session, "select_risk", choices = c(), selected = "Percent of the population (%)")
    updatePickerInput(session, "select_type", choices = c(), selected = "Zip Code (ZCTA)")
    updatePickerInput(session, "select_region", choices = c(), selected = c("Central", "East", "North Central", "North Coastal", "North Inland", "South"))
    updateSliderInput(session, "slider", value = c(min, max))
    updatePickerInput(session, "selected_locations", choices = c(), selected = sort(map_app_final()$GEOID))
    updateCheckboxInput(session, "aggregate_selections", value = TRUE)
    updateCheckboxInput(session, "separate_selections", value = TRUE)
    updateCheckboxInput(session, "aggregate_sra", value = TRUE)
    updateCheckboxInput(session, "separate_sra", value = TRUE)
    updateCheckboxInput(session, "aggregate_hhsa", value = TRUE)
    updateCheckboxInput(session, "separate_hhsa", value = TRUE)
    updateCheckboxInput(session, "aggregate_zcta", value = TRUE)
    updateCheckboxInput(session, "separate_zcta", value = TRUE)
    updateCheckboxInput(session, "aggregate_tract", value = TRUE)
    updateCheckboxInput(session, "separate_tract", value = TRUE)
    updateCheckboxInput(session, "cnty_total", value = TRUE)
    updatePickerInput(session, "select_hhsa_rmd", choices = c(), selected = NULL)
    updatePickerInput(session, "select_sra_rmd", choices = c(), selected = NULL)
    updatePickerInput(session, "select_zcta_rmd", choices = c(), selected = NULL)
    
  })
  
  
  
  
  ## Update Select pop choices
  observeEvent(input$select_data, {
    
    req(input$select_data)  # Ensure this runs only when select_data has a value
    
    filtered_data <- no_geo %>%
      filter(source_filter %in% input$select_data) %>%
      select(pop_1, pop_1a) %>%
      arrange(pop_1, pop_1a) %>%
      unique() 
    
    choices <- split(filtered_data$pop_1a, filtered_data$pop_1)
    
    # Flatten the list to get all pop_1a values
    all_pop_1a_choices <- unlist(choices)
    if (length(all_pop_1a_choices) > 0) {
      # Initialize first_pop_1a_choice based on the condition of input$select_data
      if (grepl("Service", input$select_data) && "All Clients Served" %in% all_pop_1a_choices) {
        first_pop_1a_choice <- "All Clients Served"
      } else {
        first_pop_1a_choice <- all_pop_1a_choices[1]
      }
      
      # Update select_pop with the new choices and select the first_pop_1a_choice
      updatePickerInput(session, "select_pop", choices = choices, selected = first_pop_1a_choice)
    } else {
      # Update with empty choices if no choices are available
      updatePickerInput(session, "select_pop", choices = list(), selected = NULL)
    }
    
  }, ignoreNULL = FALSE)
  
  
  
  
  # Reactive expression for filtering data
  pop_select <- reactive({
    req(input$select_data, input$select_pop, input$select_type)
    
    filtered_data <- map_app %>%
      filter(source_filter %in% input$select_data,
             pop_1a %in% input$select_pop, 
             type %in% input$select_type)
    
    total_area <-  nrow(filtered_data)
    filtered_data <- filtered_data %>%
      mutate(total_area = total_area)
    
    return(filtered_data)
  })
  
  # Update the name of suppressed areas
  output$sprs_name_table <- renderText({ 
    unique_values <- unique(filtered_data()$sprs_name)
  })
  
  
  #### Update the choices of equity lens so size of the population is not available based on BHEI ####
  output$risk_input <- renderUI({
    pattern <- paste(input$select_pop, collapse = "|")
    label_text <- if (grepl("Rank|Domain", pattern)) {
      "The map is ordered by BHEI Ranking"
    } else {
      "Select to rank areas by the percentage or the size of the population"
    }
    
    pickerInput("select_risk", 
                label = div(label_text, class = "custom-filter"),
                choices = if (grepl("Rank|Domain", pattern)) " BHEI Ranking" else c("Percent of the population (%)", "Size of the population (n)"), 
                multiple = FALSE,
                selected = if (grepl("Rank|Domain", pattern)) " BHEI Ranking" else "Percent of the population (%)")
  })
  
  
  # Reactive expression to create a variable based on the selection of 'select_risk'
  selectedVariable <- reactive({
    req(input$select_risk)
    
    selectedRisk <- input$select_risk
    if (selectedRisk == "Percent of the population (%)") {
      return("percentage")
    } else if (selectedRisk == "Size of the population (n)") {
      return("count")
    } else {
      return("BHEI rank")
    }
  })
  
  
  # Add select risk 
  observeEvent(input$select_risk, {
    req(input$select_risk)
    current_selection <- input$select_pop
  })
  
  risk_select <- reactive({
    req(input$select_pop, input$select_data) 
    req(input$select_type, input$select_risk) 
    
    risk_select <- NULL  # Initialize risk_select to NULL or some default value
    
    if (input$select_risk == "Size of the population (n)") {
      risk_select <- pop_select() %>%
        mutate(rank_asc = rank(smst_nm, ties.method = "max")) %>%
        mutate(rank_asc = ifelse(smst_nm == 0, 0, rank_asc)) 
    } 
    else {
      # Define risk_select for other cases or handle it properly
      risk_select <- pop_select()
    }
    return(risk_select)
  })
  
  
  
  
  output$selected_outcome_desc <- renderUI({
    # Check if the selectedVariable contains "BHEI"
    if (grepl("BHEI", selectedVariable())) {
      # Assign label for BHEI
      labels <- sprintf("The map shows Behavioral Health Equity Index (BHEI) scores by %s. Areas with higher scores (darker colors) may not have access to the resources and supports that promote behavioral health. These areas are at higher risk for behavioral health inequity and may serve as priority zones for equity work and service enhancements.",
                        unique(filtered_data()$type))
      
    } else if (grepl("Service", unique(filtered_data()$source_filter))) {
      # Define the link text
link_text <- "See the Technical Notes for more information"

# Generate the formatted string with a link
labels <- sprintf("The map shows the %s of %s. A darker color means the %s is relatively higher. %s. Clients without a valid San Diego County address and those in institutional settings (e.g., SNF, hospital, etc.) are excluded. Please refer to the Technical Notes for more information.",
                 
                  unique(selectedVariable()),
                        unique(filtered_data()$defintn_description),
                        unique(selectedVariable()),
                        unique(filtered_data()$sprs_name)) # Link to the tab with ID "learning-resources-and-data-notes"
    } else {
      labels <- sprintf("The map shows the %s of %s by %s. A higher rank (darker colors) means the %s is relatively higher.", 
                        unique(selectedVariable()),
                        unique(filtered_data()$defintn_description),
                        unique(filtered_data()$type),
                        unique(selectedVariable()))
    }
    
    # Creating the HTML content with CSS style
    tags$div(
      tags$style(type = "text/css", 
                 ".custom-label {
                 font-family: 'Open Sans', sans-serif;
                 font-size: 14px;
                 color: #1A1A1A;
               }"
      ),
      tags$div(class = "custom-label", HTML(labels))
    )
  })
  
  
  ##### Filter regions ####
  region_select <- reactive({
    req(input$select_region)
    region_select <- filter(risk_select(),
                            hhsa_nm %in% input$select_region) 
    
    
  })
  
  
  
  ##### Slider ####
  output$dynamicLabel <- renderUI({
    total_area_unique <- unique(filtered_data()$total_area)
    type_unique <- unique(filtered_data()$type_desc)
    
    # if length is greater than 1, you may want to handle differently
    total_area_text <- if (length(total_area_unique) > 1) "multiple values" else total_area_unique
    type_text <- if (length(type_unique) > 1) "multiple types" else type_unique
    
    tagList(
      tags$style(type="text/css", "
               @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
               .custom-label {
                 font-family: 'Open Sans', sans-serif;
                 font-size: 14px;
                 color: #1A1A1A;
               }
               "),
      tags$div(class="custom-label", 
               HTML(sprintf(
                 "Filter %s by %s",
                 
                 type_text,
                 selectedVariable()
               ))
      )
    )
  })
  
  
  
  output$slider <- renderUI({
    sliderInput("slider",
                label = div(""),
                value = c(1, nrow(region_select())), min = 1, max = nrow(region_select()), step = 1 )
  })
  
  ##### Choose top n ####
  map_app_final<- reactive({
    req(input$select_pop, input$select_region, input$select_type, input$slider)
    
    map_app_final <- region_select() %>%
      arrange(rank_asc) %>%
      filter(row_number() >= input$slider[1], row_number()<= input$slider[2]) 
    
    
  })
  
  # Reactive variable to store aggregated data
  reactive_aggregated_data <- reactiveVal()
  
  # Perform aggregation automatically when selected locations change
  observeEvent(input$selected_locations, {
    # Remove compare variable
    service_rmd <- service_rmd %>%
      dplyr::select(-compare)
    
    # Filter service_rmd based on selected areas
    selected_data <- service_rmd %>%
      filter(GEOID %in% input$selected_locations & type != "San Diego County") %>%
      unique()
    
    # Perform aggregation
    aggregated_data <- selected_data %>% 
      group_by(type, source_filter, outcome) %>% 
      summarise(
        smst_nm = sum(smst_nm, na.rm = TRUE),
        smst_dn = sum(smst_dn, na.rm = TRUE),
        .groups = "drop"
      )
    
    selected_data <- selected_data %>%
      dplyr::select(outcome, pop_1, table_label_year, tech_label, acronyms_defined, definition, bar_label_small_n, source, source_filter) %>%
      unique()
    
    cnty_data <- service_rmd %>%
      filter(type == "San Diego County") %>%
      dplyr::select(outcome, pop_1, table_label_year, tech_label, acronyms_defined, definition, bar_label_small_n, source, smst_nm, smst_dn, type, source_filter) %>%
      unique()
    
    
    aggregated_data <- aggregated_data %>%
      left_join(selected_data, by = c("outcome", "source_filter")) 
    
    aggregated_data <- bind_rows(aggregated_data, cnty_data) %>%
      mutate(percent = smst_nm / smst_dn * 100) %>%
      rename(new_smst_nm = smst_nm, new_smst_dn = smst_dn, new_percent = percent) %>%
      mutate(new_percent = round(new_percent,1)) %>%
      mutate(new_percent_pretty = sprintf("%.1f", new_percent)) %>%
      mutate(new_percent_pretty = paste0(new_percent_pretty, "%")) %>%
      mutate(smst_nm_pretty  = prettyNum(new_smst_nm, big.mark = ",")) %>%
      mutate(smst_dn_pretty  = prettyNum(new_smst_dn, big.mark = ",")) %>%
      filter(!(pop_1 %in% c("Behavioral Health Equity Index (BHEI)", "Total Occupied Housing Units", "Total Population" ,  "Lived in Group Quarters"))) 
    
    num_selected_ids <- length(input$selected_locations)
    
    
    
    
    
    # Update the reactive variable
    reactive_aggregated_data(aggregated_data)
  }, ignoreNULL = FALSE)
  
  ##### Map and location selectInput ####
  # Creating a new reactive expression for the filtered data
  filtered_data <- reactive({
    data <- map_app_final() %>%
      filter(!is.na(smst_nm))
    data
  })
  
  # Reactive expression to store the selected option for count, rank,
  selected_risk <- reactive({
    input$select_risk
    
  })
  
  
  observeEvent(filtered_data(), {
    updatePickerInput(session,
                      inputId = "selected_locations",
                      choices = sort(filtered_data()$GEOID),
                      
                      selected = sort(filtered_data()$GEOID)) # input$selected_locations
  })
  
  #create empty vector to hold all click ids
  selected_ids <- reactiveValues(ids = vector())
  
  
  
  #initial map output
  output$map <- renderLeaflet({
    
    
    # Define your new color palette
    new_colors <- c("#FFFFD1", "#C3E4CB", "#64B4C2", "#4F89B0", "#28348E")
    
    # Get filtered data
    filtered_rank_asc <- pop_select()$rank_asc 
    
    # Use the new color palette for colorFactor
    color <- colorFactor(new_colors, filtered_rank_asc)
    
    # Determine the number of bins based on the number of unique values
    num_unique_values <- length(unique(filtered_rank_asc))
    num_bins <- min(num_unique_values, length(new_colors))
    
    # Calculate quantile breaks for the bins
    quantile_breaks <- quantile(filtered_rank_asc, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
    
    # Round the breaks and ensure unique values
    quantile_breaks <- round(quantile_breaks)
    quantile_breaks <- unique(quantile_breaks)
    
    # Use the quantile breaks for colorBin
    pal <- colorBin(palette = new_colors, bins = quantile_breaks, domain = filtered_rank_asc)
    
    
    wrap_text <- function(text) {
      wrapped_text <- strwrap(text, width = 75)
      final_text <- paste(wrapped_text, collapse = "<br>")
      return(final_text)
    }
    
    if (grepl("BHEI", selectedVariable())) {
      # Assign label for BHEI
      labels <- sprintf(
        "<strong>%s </strong> has a BHEI ranking of <strong> %s out of %g </strong> %s . A higher rank means an area is more likely to experience behavioral health inequity.",
        filtered_data()$GEOID,
        filtered_data()$ordinal_column,
        filtered_data()$total_area,
        filtered_data()$type_desc
      )
      
    } else if (unique(filtered_data()$source_filter) == "Penetration Estimates") {
      labels <- sprintf(
        "<strong>%s </strong> is ranked <strong> %s out of %g </strong> %s. A higher rank means a higher percent of the target population received outpatient treatment (%s of %s = %s). %s",
        filtered_data()$GEOID,
        filtered_data()$ordinal_column,
        filtered_data()$total_area,
        filtered_data()$type_desc,
        
        filtered_data()$smst_nm_pretty,
        filtered_data()$smst_dn_pretty,
        filtered_data()$percent_pretty,
        filtered_data()$reliablity_flag
      )
    } else if (unique(filtered_data()$source_filter) == "MH Service Data: All Clients, FY 2022-2023") {
      labels <- sprintf(
        "<strong>%s </strong>: %s",
        filtered_data()$GEOID,
        filtered_data()$map_label
        
      )
      
    } else {
      labels <- sprintf(
        "<strong>%s </strong> is ranked <strong> %s out of %g </strong> %s; %s of %s = %s. %s",
        filtered_data()$GEOID,
        filtered_data()$ordinal_column,
        filtered_data()$total_area,
        filtered_data()$type_desc,
        filtered_data()$smst_nm_pretty,
        filtered_data()$smst_dn_pretty,
        filtered_data()$percent_pretty,
        filtered_data()$reliablity_flag
      )
    }
    
    # Now apply wrap_text() to the entire label
    labels <- lapply(labels, wrap_text)
    
    labels <- lapply(labels, htmltools::HTML)
    
    
    req({nrow(filtered_data()) > 0})
    
    leaflet(options = leafletOptions(minZoom = 8.0, maxZoom = 17.5)) %>%  # Limit zoom levels
      addTiles() %>%
      setView(lng = "-116.87", lat="33.0", zoom = 9.0) %>%
      setMaxBounds(lng1 = -117.6, lat1 = 32.5, lng2 = -116.1, lat2 = 33.5) %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      #addProviderTiles(provider = "OpenStreetMap") %>%
      addPolygons(data = filtered_data(),
                  fillColor = "white",
                  fillOpacity = 0.0,
                  color = "grey",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~GEOID,
                  group = "regions",
                  label = labels) %>%
      addPolygons(data = filtered_data(),
                  fillColor = ~pal(rank_asc),
                  fillOpacity = 1.0,
                  weight = 1,
                  color = "grey",
                  stroke = TRUE,
                  layerId = ~NAME,
                  group = ~GEOID,
                  label = labels) %>%
      
      addLegend("topright",
                #pal = pal,
                #values = map_app_final()$smst_nm,
                labels = c("Low", "Fair", "Moderate", "Elevated", "High"),  # Your categories
                colors = new_colors,
                title = selected_risk(),
                opacity = 1,
                layerId = "map-legend")
    
    
    
  }) #END RENDER LEAFLET
  
  
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  observeEvent(input$map_shape_click, {
    if(input$map_shape_click$group == "regions"){
      selected$groups <- c(selected$groups, input$map_shape_click$id)
      proxy %>% showGroup(group = input$map_shape_click$id)
    } else {
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    updatePickerInput(session,
                      inputId = "selected_locations",
                      choices = sort(filtered_data()$GEOID),
                      selected = sort(selected$groups))
  })
  
  observeEvent(input$selected_locations, {
    removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
    added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
    
    if(length(removed_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% hideGroup(group = removed_via_selectInput)
    }
    
    if(length(added_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% showGroup(group = added_via_selectInput)
    }
  }, ignoreNULL = FALSE)
  
  
  #### Community Profiles Plotly #####
  # Update choices for selectInput based on available outcomes in aggregated data
  observeEvent(input$select_data_2, {
    
    req(input$select_data_2)  # Ensure this runs only when select_data_2 has a value
    
    
    filtered_data <- no_geo %>%
      filter(source_filter %in% input$select_data_2) %>%
      #filter(source_filter != "Behavioral Health Equity Index (BHEI)") %>%
      select(pop_1) %>%
      arrange(pop_1) %>%
      unique() %>%
      filter(!grepl("Clients", pop_1))
    
    choices <-  filtered_data$pop_1
    
    # Flatten the list to get all pop_1a values and select the first
    all_pop_1_choices <- choices
    if (length(all_pop_1_choices) > 0) {
      first_pop_1_choice <- all_pop_1_choices[1]
      
      # Update select_pop_2 with the new choices and select the first pop_1a choice
      updatePickerInput(session, "select_pop_2", choices = choices, selected = first_pop_1_choice)
    } else {
      # Update with empty choices if no choices are available
      updatePickerInput(session, "select_pop_2", choices = list(), selected = NULL)
    }
    
  }, ignoreNULL = FALSE)
  
  
  # Generate the bar chart
  output$bar_chart <- renderPlotly({
    req(input$select_pop_2) 
    req(input$select_data_2) 
    req(reactive_aggregated_data())
    selected_outcome <- input$select_pop_2
    selected_data_2 <- input$select_data_2
    
    num_selected_ids <- length(input$selected_locations)
    
    
    data_to_plot <- reactive_aggregated_data() %>% 
      filter(source_filter == selected_data_2) %>%
      filter(pop_1 == selected_outcome) 
    
    
    
    num_outcomes <- length(unique(data_to_plot$bar_label_small_n))
    
    data_to_plot <- data_to_plot %>%
      mutate(type = ifelse(type != "San Diego County" & num_selected_ids == 1, paste("Selected Area:", input$selected_locations), type)) %>%
      mutate(type = ifelse(type != "San Diego County" & num_selected_ids > 1,  paste0("Selected ", type, "s"), type)) 
    
    data_to_plot <- data_to_plot %>%
      mutate(type = ifelse(grepl("ful", outcome) & type != "San Diego County", paste0("MH Clients in ", type, " (excludes suppressed data) "), type)) %>%
      mutate(type = ifelse(grepl("ful", outcome) & type == "San Diego County", paste0("MH Clients in ", type, " (includes suppressed data) "), type)) 
    
    
    # Calculate the bargap based on the number of outcomes
    if (num_outcomes == 1) {
      set_width <- 500  # Narrower bars for two outcomes
    }
    else if (num_outcomes == 2) {
      set_width <- 600  # Narrower bars for two outcomes
    } else if (num_outcomes == 3) {
      set_width <- 650 # Slightly wider bars for three outcomes
    } else {
      set_width <- 800  # Default bargap for more than three outcomes
    }
    
    plot <- plot_ly(
      data_to_plot, 
      x = ~bar_label_small_n, 
      y = ~new_percent, 
      color = ~type, 
      colors = c("#00a9c5", "#Faa663"), 
      type = "bar", 
      text = ~new_percent_pretty, 
      textposition = "outside",
      hoverinfo = "text",  
      hovertext = ~paste0(new_percent_pretty, " (", smst_nm_pretty, " of ", smst_dn_pretty, ")"), 
      height = 500, 
      width = set_width
    ) %>%
      layout(
        title = list(text = paste0(unique(data_to_plot$table_label_year), '<br>'), x = 0.0, y = -0.2),
        margin = list(t = 100),  # Increase top margin to create more space for legend
        yaxis = list(
          title = 'Percent (%)',
          range = c(0, max(data_to_plot$new_percent) * 1.2)  # extend the y-axis to 120% of the max value
        ),
        xaxis = list(title = ''), 
        legend = list(
          title = list(), 
          itemclick = "toggleothers", 
          orientation = "h", 
          x = 0.5,     # center the legend
          y = 1.02,    # position the legend slightly above the top of the plot area
          xanchor = "center",  # anchor the center of the legend to the 'x' coordinate
          yanchor = "bottom"   # anchor the bottom of the legend to the 'y' coordinate
        )
      ) %>%
      config(
        modeBarButtonsToRemove = c("lasso2d", "pan2d",  "select2d", 
                                   "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "zoomIn2d", "zoomOut2d"),
        displaylogo = FALSE,
        displayModeBar = TRUE
      )
    
    return(plot)
    
  })
  
  
  ##### Data Table  ####
  output$tech_notes_render <- DT::renderDataTable(tech_notes)
  output$servtable  <-  DT::renderDataTable(extensions = c('Buttons', 
                                                           #'Autofill', 
                                                           'ColReorder',
                                                           'Responsive',
                                                           'Scroller'),
                                            style = 'bootstrap',
                                            class = 'table-bordered stripe table-condensed',
                                            filter = list(position = 'top', clear = FALSE),
                                            rownames = FALSE,
                                            options = list( lengthMenu = list(c(41,75,100,-1),
                                                                              c(41,75,100,"All")),
                                                            dom = 'frtipB',
                                                            buttons = list(list(extend = 'csv',
                                                                                buttons = c('csv'),
                                                                                exportOptions = list(modifiers = list(page = "current"))
                                                            )
                                                            ),
                                                            search = list(regex = TRUE, caseInsensitive = FALSE),
                                                            #autofill = TRUE,
                                                            colReorder = TRUE,
                                                            deferRender = TRUE,
                                                            scrollY = 500,
                                                            scroller = TRUE,
                                                            columnDefs = list(list(className = 'dt-left', targets = '_all'))
                                            ), {
                                              
                                              table_all <-  map_app %>%
                                                st_drop_geometry() %>%
                                                ungroup() %>%
                                                arrange(GEOID) %>%
                                                filter(source_filter %in% input$select_data) %>%
                                                filter(pop_1a %in% input$select_pop) %>%
                                                filter(GEOID %in% input$selected_locations) %>%
                                                dplyr::select(tbl_l_1, smst_nm, percent, GEOID, n_of_n, reliablity_note) %>%
                                                unique()
                                              
                                              table_cnt <- table_all %>%
                                                dplyr::select(-percent) %>%
                                                pivot_wider(names_from = tbl_l_1, values_from = smst_nm) 
                                              colnames(table_cnt) <- paste(colnames(table_cnt), "(n)")
                                              
                                              table_cnt <- table_cnt %>%
                                                rename(Area = `GEOID (n)`) %>%
                                                rename(`Reliability Note` = `reliablity_note (n)`) %>%
                                                rename(`Description` = `n_of_n (n)`) 
                                              
                                              
                                              table_percent <- table_all %>%
                                                dplyr::select(-smst_nm, -n_of_n, -reliablity_note) %>%
                                                pivot_wider(names_from = tbl_l_1, values_from = percent) 
                                              colnames(table_percent) <- ifelse(grepl("Rank",colnames(table_percent)), 
                                                                                colnames(table_percent),
                                                                                paste(colnames(table_percent), "(%)"))
                                              table_percent <- table_percent %>%
                                                rename(Area = `GEOID (%)`) 
                                              
                                              table_n_percent <- left_join(table_percent, table_cnt) 
                                              
                                              
                                              
                                              # Add rank order 
                                              rank_only <- map_app_final() %>%
                                                st_drop_geometry() %>%
                                                dplyr::select(GEOID, rank_asc) %>%
                                                rename(Area = GEOID, Ranking = rank_asc) 
                                              
                                              
                                              servtable <- left_join(table_n_percent, rank_only) %>%
                                                arrange(desc(Ranking)) 
                                              
                                              
                                              # Get the column names, excluding 'Area' and 'Reliability Flag'
                                              col_names <- setdiff(names(servtable), c("Area", "Ranking" , "Reliability Note", "Description"))
                                              
                                              # Reorder the columns
                                              servtable  <- servtable [, c("Ranking", "Area", col_names, "Description", "Reliability Note")] 
                                              
                                              
                                              if(input$select_risk ==  "Percent of the population (%)"){
                                                servtable  <-  servtable    %>%
                                                  rename("Ranking Based on %" = Ranking)  
                                              }
                                              else if(input$select_risk == "Size of the population (n)"){
                                                servtable  <-  servtable    %>%
                                                  rename("Ranking Based on n" = Ranking) 
                                              }
                                              
                                              # Remove n for BHEI elements
                                              if (any(grepl("BHEI|Domain", colnames(servtable)))) {
                                                servtable <- servtable[, 2:3]
                                                
                                              }else{
                                                servtable 
                                              }
                                              
                                              # Remove "reliability" for BHEI elements
                                              if (grepl("Clients", input$select_data)) {
                                                servtable <- servtable[, 1:5]
                                                
                                              }else{
                                                servtable 
                                              }
                                              
                                              
                                              
                                            })
  
  {
    
  }
  
  
  
  ##### Report Summary  ####
  
  # Assign parameters for RMD YAML
  output$report_short <- downloadHandler(
    filename <-  "Service Planning Key Findings Report.html",
    content = function(file) {
      showModal(modalDialog("The report is loading. This is a large file. For most users the report should download in under a minute.", footer=NULL))
      on.exit(removeModal())
      
      params <- list(geos = input$selected_locations)
      rmarkdown::render('service_planning_summary.Rmd', 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    }
    
    
  )
  
  
  ##### Report Detailed  ####
  
  # This hides/reveals selections to customize the report 
  observe({
    toggle("aggregate_selections", condition = length(input$selected_locations) > 1)
    toggle("separate_selections", condition = length(input$selected_locations) > 1)
    toggle("aggregate_hhsa", condition = length(input$select_hhsa_rmd) > 1)
    toggle("separate_hhsa", condition = length(input$select_hhsa_rmd) > 1)
    toggle("aggregate_sra", condition = length(input$select_sra_rmd) > 1)
    toggle("separate_sra", condition = length(input$select_sra_rmd) > 1)
    toggle("aggregate_zcta", condition = length(input$select_zcta_rmd) > 1)
    toggle("separate_zcta", condition = length(input$select_zcta_rmd) > 1)  
    
  })
  
  
  
  # Assign parameters for RMD YAML
  
  output$report <- downloadHandler(
    filename <-  "Service Planning Detailed Report.html",
    content = function(file) {
      showModal(modalDialog("The report is loading. This is a large file. For most users the report should download in under a minute.", footer=NULL))
      on.exit(removeModal())
      
      params <- list(geos = input$selected_locations,
                     targ = input$select_pop,
                     type = input$select_type,
                     aggregate_all = input$aggregate_selections,
                     separate_all = input$separate_selections,
                     compare_cnty = input$cnty_total,
                     compare_hhsa = input$select_hhsa_rmd,
                     aggregate_hhsa = input$aggregate_hhsa,
                     separate_hhsa = input$separate_hhsa,
                     compare_sra = input$select_sra_rmd,
                     aggregate_sra = input$aggregate_sra,
                     separate_sra = input$separate_sra,
                     compare_zcta = input$select_zcta_rmd,
                     aggregate_zcta = input$aggregate_zcta,
                     separate_zcta = input$separate_zcta
                     
      )
      rmarkdown::render('service_planning_report.Rmd', 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    }
    
  )
  
  ### Download technical Report Download
  # Function to generate and save the HTML file
  generateHTMLReport <- function() {
    # Code to generate the HTML report
    
    # Save the HTML report to the "www" folder
    file.copy("www/BHEI Technical Report.html", "BHEI Technical Report.html", overwrite = TRUE)
  }
  
  # Generate and save the HTML report when the app starts
  generateHTMLReport()
  
  # Download button or link handler
  output$downloadReport <- downloadHandler(
    filename = "BHEI Technical Report.html",
    content = function(file) {
      # Copy the HTML file to the temporary directory for download
      file.copy("www/BHEI Technical Report.html", file)
    }
  )
  
}

shinyApp(ui = ui, server = server)

