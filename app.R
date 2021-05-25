#
# GA Medicaid services for Autism
# Created by Freyja Brandel-Tanis (freyjabt.me) for Dr. Jennifer Singh and funded through DILAC.

library(shiny)
library(leaflet)
library(readr)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(shades)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(stringr)
library(ggmap)
library(readxl)
library(RMySQL)
library(tidyr)
library(dplyr)
library(DT)

##

# Connect to database
# # Freyja Local
# source("db_connection1.R")
# dbname <- "dilac_project"
# Server
source("db_connection.R")
dbname <- "mapdb"

# Function to load data from database
loadData <- function(databaseName, tableName) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", tableName)
  # Submit the fetch query and disconnect
  on.exit(dbDisconnect(db))
  return(dbGetQuery(db, query))
}

# Load data from database
p2p_prov <- loadData(dbname,"p2p_providers")%>% # p2p provider data
  drop_na(lat,lon)%>%
  filter(
    row_names != 399,
    !provider_id %in% c(25, 387, 315) # these providers geocoded to Michigan, so I'm manually removing them
    )%>%
  select(-row_names)

p2p_pd <- loadData(dbname,"p2p_provider_details")%>% # p2p details specific to the providers, like hours of operation
  mutate(
    provider_id = as.numeric(provider_id)
  )%>%
  select(-row_names)%>%
  filter(pfx!="disability")

p2p_sd <- loadData(dbname,"p2p_service_details")%>% # p2p which services are offered by each provider
  select(-row_names)



# CDC SVI 2018
svi_ga_tracts <- st_read("svi/SVI2018_GEORGIA_tract.shp")%>%
  st_transform(crs = 3518)%>%
  st_simplify(500, preserveTopology = TRUE)%>%
  st_transform(4326)
  # st_transform('+proj=longlat +datum=WGS84')


svi_ga_county <- st_read("svi/SVI2018_GEORGIA_county.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')


# svi_ga_county_tidy <- svi_ga_county%>%
#   select(LOCATION, FIPS, starts_with("EPL_"), starts_with("SPL_"))%>%
#   filter(
#     across(where(is.numeric),
#            ~.!=(-999)
#     )
#   )%>%
#   pivot_longer(
#     cols = c(starts_with("EPL_"), starts_with("SPL_")),
#     names_to = "variable",
#     values_to = "value"
#   )


  
# Read in the transit stops. To add more stops or update existing ones, see "transit/transit_write.R"
stops_all <- st_read("transit/stops.shp")

# Create a vector of providers within 1/4 mi of a transit stop. 
# # This intersection is done here instead of before adding data to the database so that it is more dynamic when more transit providers are added.
p2p_prov_transit <- p2p_prov%>%
  st_as_sf(coords = c("lon","lat"))%>%
  st_set_crs(4326)%>%
  st_intersection(stops_all)%>%
  st_drop_geometry()%>%
  select(provider_id)%>%
  distinct()%>%
  pull


# Edit the p2p_pd file to account for providers we've shown to be transit accessible but were not marked that way.
p2p_pd <- p2p_pd%>%
  mutate(
    values = ifelse(pfx == "transit_access" & provider_id %in% p2p_prov_transit, 1, values)
  )

purple_ramp <- scales::colour_ramp(c('#D8BFD8','#7851A9'))

bins = c(0,1,5,10,50,100,Inf)

# Bringing in the county data for GA from Census TIGER
ga_counties <- counties(state = "GA")
# basemap for both maps

plot_map <- ga_counties # Use ga counties to make the basemap

basemap = leaflet(plot_map)%>%
  addTiles(options = providerTileOptions(minZoom = 7))%>% # Limits the users ability to zoom out too far.
  setView(lat = 32.84069, lng = -83.6324, zoom = 7) # Centers the map on GA. Technically on Macon, GA, which seemed close enough to center.
  

# Variable declaration for Provider details
age_var <- list(
  "Under 3 Years"="Under 3 Years",
  "3-5 Years"="3-5 Years",
  "6-11 Years"="6-11 Years",
  "12-18 Years"="12-18 Years",
  "19-21 Years"="19-21 Years",
  "Over 21 Years"="21+ Years"
)

hours_var <- list(
  "Daytime Hours",
  "By Appointment Only",
  "Weekend Hours",
  "Evening Hours",
  "Afternoons Only",
  "24 Hours On Call",
  "Mornings Only"
)

pay_var <- list(
  "Medicaid"="medicaid",
  "PeachCare"="peachcare",
  "Tri-Care" = "tricare",
  "Private Insurance"="private_insurance",
  "Financial Assistance"="assistance"
)

# Variable declaration for services, etc
adap_var <- list("Accomodations / Modifications" = "accomodations_modifications",
                 "Adaptive Equipment" = "adaptive_equipment",
                 "Assistive Technology" = "assistive_technology",
                 "Augmentative Communication" = "augmentative_communication",
                 "Service Animals" = "service_animals")
adult_var <- list("Day Services" = "day_services",
                  "Supported Employment" = "supported_employment",
                  "Supported Independent Living" = "supported_independent_living",
                  "Job Training" = "job_training",
                  "Residential Facility" = "residential_facility",
                  "Personal Care Assitance" = "personal_care_assistance")
advoc_var <- list("Education" = "advocate_education",
                  "Heathcare" = "advocate_healthcare",
                  "Disability" = "advocate_disability",
                  "Education Resources " = "resources_advocate_education",
                  "IDEA Resources" = "resources_idea",
                  "Mediation" = "mediation",
                  "ADA Resources" = "resources_ada",
                  "Attorney: Wills / Trusts" = "attorney_wills_trusts",
                  "Attorney" =  "attorney")
care_var <- list("Day Camp" = "camp_day", "After School Caregivers" = "caregivers_after_school", "Overnight Camp" = "camp_overnight", "Respite Care" = "respite_care", "Finacial Assistance" = "financial_assistance", "Caregivers (Adult)" = "caregivers_adult", "Caregivers (Child)" = "caregivers_child", "Community Caregiving" = "community_caregiving", "Adult Camp" = "camp_adults")
child_var <- list("High-Risk Infant Assessment" = "high_risk_infant_assessment",
                  "Preschool (Specialized)" = "preschool_specialized",
                  "BCW Offices" = "bcw_offices",
                  "Preschool (Private)" = "preschool_private",
                  "Childcare Programs" = "child_care",
                  "Preschool (Public)" = "preschool_public")
health_var <- list("Psychologists (Child)" = "psychologists_child",
                   "Children's Medical Services" = "cms",
                   "Developmental Pediatricians" = "dev_pediatricians",
                   "Diet / Nutrition" = "diet_nutrition",
                   "Mental Health" = "mental_health",
                   "Medicaid Waiver" = "medicaid_waiver",
                   "Georgia Pediatric Program (GAPP)" = "gapp_providers",
                   "Home Healthcare" = "home_healthcare",
                   "Nursing" = "nursing_services",
                   "Public Health" = "public_health",
                   "Psychiatrist" = "psychiatrist",
                   "Rehabilitation Centers" = "rehabilitation_centers")
eval_var <- list("Evaluation Developmental Pediatricians" = "eval_dev_pediatricians",
                 "Developmental Evaluation and Assessment" = "developmental_eval_assessment")
mental_var <- list("Psychologist" = "psychologist",
                   "Behavior Specialist" = "behavior_specialist",
                   "Child Abuse Counseling" = "child_abuse",
                   "Sexual Abuse Support / Counseling" = "sexual_abuse",
                   "Mental Health Support" = "mental_health",
                   "Bullying Support / Prevention" = "bullying",
                   "Psychiatrist" = "psychiatrist",
                   "Sucide Support / Prevention" = "suicide_support") 
pfam_var <- list("Adpotion" = "adoption",
                 "Crisis Intervention" =  "crisis",
                 "Family Support" =  "family_support",
                 "Foster Care" =  "foster_care",
                 "Parent Training / Education" =  "parent_education",
                 "Residential Children" =  "residential_childrens",
                 "Social Work" =  "social_work",
                 "Support (Bereavement)" =  "support_bereavement",
                 "Support (Parent)" =  "support_parent",
                 "Support (Peer)" =  "support_peer",
                 "Personal Care" =  "personal_care",
                 "Support (Sibling)" =  "support_sibling",
                 "Case Management" =  "case_management",
                 "Play Groups" =  "play_groups",
                 "Support Groups" =  "support_group") 
plan_var <- list("Person-Centered Planning" = "person_centered_planning",
                 "Financial Planning" = "financial_planning",
                 "Trust / Will Prep" = "trust_will_prep",
                 "Guardianship" = "guardianship")
therp_var <- list("Counseling" = "counseling",
                  "Applied Behavior Therapy" = "aba",
                  "Feeding Therapy" = "feeding_therapy",
                  "Occupational Therapy" = "occupational_therapy",         
                  "Social Skills Training" = "social_skills_training",       
                  "Speech and Language Therapy" = "speech_language",
                  "Sensory Integration" = "sensory_integration",          
                  "Special Instruction" = "special_instruction",
                  "Alternative Therapies" = "alternative_therapies",
                  "Music Therapy" = "music_therapy",                
                  "Physical Therapy" = "physical_therapy",
                  "Art Therapy" = "art_therapy",
                  "Play Therapy" = "play_therapy",
                  "Aquatic Therapy" = "aquatic_therapy",
                  "Floor Therapy" = "floor_therapy",
                  "Hippo Therapy" = "hippotherapy",
                  "Vision Therapy" = "vision_therapy",
                  "Massage Therapy" = "massage_therapy",
                  "Sign Language Training" = "sign_language",
                  "Pet Therapy" = "pet_therapy"
                  )
edu_var <- list("Financial Assistance" = "financial_assistance", 
                "Day School (Private)" = "day_private", 
                "Tutoring" = "tutoring", 
                "Transition Services" = "transition_services", 
                "Home Schooling" = "home_schooling", 
                "Residential School (Private)" = "residential_private", 
                "Charter School (Public)" = "charter_public", 
                "Education Consultant" = "education_consultants", 
                "Inclusive Services" = "inclusive_services", 
                "Post-Secondary Education" = "post_secondary_ed", 
                "Special Education Oversight" = "special_ed_oversight"
                )

svi_descs <- tibble(
  desc = str_to_title(c("Persons below poverty",
                        "Unemployed civilians",
                        "Per capita income",
                        "Persons 25+ with no high school diploma",
                        "Persons aged 65 and older",
                        "Persons aged 17 and younger",
                        "Civilian with a disability",
                        "Single parent households with children under 18",
                        "Minority",
                        "Persons who speak English 'less than well'",
                        "Multi-family housing (10+)",
                        "Mobile homes",
                        "Crowded households",
                        "Households with no vehicle available",
                        "Persons in group quarters",
                        "Series for Socioeconomic theme",
                        "Series for Household Composition theme",
                        "Series for Minority Status/Language theme",
                        "Series for Housing Type/Transportation theme",
                        "Overall Series")),
  var =  svi_ga_county%>%st_drop_geometry()%>%select(starts_with("EP_"), starts_with("SPL_"), -EP_UNINSUR)%>%names
  )%>%
  pivot_wider(names_from = desc, values_from = var)%>%
  as.list



# Define UI
ui <- dashboardPage(
  # tags$style(type = "text/css", "html, body {width:100%;height:100%}",
  #              ".leaflet .legend i{
  #     border-radius: 50%;
  #     width: 10px;
  #     height: 10px;
  #     margin-top: 4px;
  #     }
  #   "),
  
  # Create Dashboard Header
  dashboardHeader(title = "Autism Services in Georgia", titleWidth = 280,
                  tags$li(a(onclick = "openTab('home')", # Link to take you to the homepage
                            href = '../',
                            icon("home"),
                            title = "Home",
                            style = "cursor: pointer;"),
                          class = "dropdown"#,
                          # tags$script(HTML("
                          #              var openTab = function(tabName){
                          #              $('a', $('.sidebar')).each(function() {
                          #              if(this.getAttribute('data-value') == tabName) {
                          #              this.click()
                          #              };
                          #              });
                          #              }"))
                          ),
                  tags$li(a(onclick = "openTab('home')", # Link to take you to the Service Cliff Visualization
                            href = '../adultcliff',
                            icon("clipboard"),
                            title = "Service Cliff Visualization",
                            style = "cursor: pointer;"),
                          class = "dropdown"
                          )
                  ),
  # Create Dashboard Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem( "Provider Search", tabName = 'search_tab', icon = icon('search')),#,
                #badgeLabel = maxYear_lb, badgeColor = "green" ),
      div( id = 'sidebar_cr',
           conditionalPanel("input.sidebar === 'search_tab'",
           tags$style(
             "#sidebar_cr {
                   overflow: auto;
                   max-height: 70vh;
               }"
           ),
           tags$style("h4 {margin:  0 0 -5px 10px;}"),
           # Attempt to make a search function for counties and service providers
                            # selectizeInput("select_country",
                            #                "Select or search for one or multiple markets",
                            #                choices =  list_country,
                            #                selected = NULL,  width = "200px",
                            #                multiple = T), #,
                            # #actionButton('btn_country','Submit')
                            # 
                            # ## action button to build report
                            # actionButton('btn_build_country_report',
                            #              paste0('Build Report'),
                            #              icon = icon('wrench')),
                            # 
                            # ## reset side bar selectoin
                            # actionButton('btn_reset_cr',
                            #              'Reset',
                            #              icon = icon('refresh') ),
           h4("Provider Details"),
           # selectizeInput(
           #   inputId = "name_search", # Search for providers by name. Currently not functional.
           #   label = "Search by name",
           #   choices = p2p_prov$name%>%unique,
           #   multiple = TRUE
           # ),
           # actionButton("search_go", label = "Search"),
           
           div(style="display:inline-block;width:32%;text-align: center;",actionButton("selectall1", label = "Select All")),
           div(style="display:inline-block;width:32%;text-align: center;",actionButton("selectnone1", label = "Select None")),
                         
           
           pickerInput("age_range","Age",choices=age_var,selected=unlist(age_var),
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)),
           pickerInput("hours","Operating Hours",choices=hours_var,selected=unlist(hours_var),
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)),
           pickerInput("pay_type","Payment Options",choices=pay_var,selected=unlist(pay_var),
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)),
           
                            pickerInput(
                              inputId = "trwh",
                              label = "Accessibly Requirements",
                              choices = c("Transit" = "transit_access",
                                          "Wheelchair" = "wheelchair_access"
                              ),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            h4("Services"),
           
           div(style="display:inline-block;width:32%;text-align: center;",actionButton("selectall", label = "Select All")),
           div(style="display:inline-block;width:32%;text-align: center;",actionButton("selectnone", label = "Select None")),
                            pickerInput(
                              inputId = "adap",
                              label = "Adaptations / Accessibiity",
                              choices = adap_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "adult",
                              label = "Adult-Specific",
                              choices = adult_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "advoc",
                              label = "Advocacy",
                              choices = advoc_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "care",
                              label = "Camps / Caregivers",
                              choices = care_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "child",
                              label = "Early Childhood-Specific",
                              choices = child_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "edu",
                              label = "Education",
                              choices = edu_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "health",
                              label = "Healthcare",
                              choices = health_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "eval",
                              label = "Evaluation / Diagnosis",
                              choices = eval_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "mental",
                              label = "Mental Health / Behavior",
                              choices = mental_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "pfam",
                              label = "Parent / Family Support",
                              choices = pfam_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "plan",
                              label = "Planning for the Future",
                              choices = plan_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            ),
                            pickerInput(
                              inputId = "therp",
                              label = "Therapies and Interventions",
                              choices = therp_var,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                            )
                            )),
      menuItem("Provider List", tabName = 'prov_list', icon = icon('table')),# icon = icon('globe') ),
      div( id = 'sidebar_cn',
           tags$style(
             "#sidebar_cn {
                   overflow: auto;
                   max-height: 100vh
                   
               }"
           ),
           
           conditionalPanel("input.sidebar === 'prov_list'",
                            downloadButton("report","Generate CSV", class = 'butt'),
                            tags$head(tags$style(".butt{background-color:#3c8dbc;} .butt{margin-left: 25px;}"))
                            )
           ),
           
      menuItem("Demographic Visualization", tabName = 'demo_vis', icon = icon('house-user')),# icon = icon('globe') ),
      div( id = 'sidebar_cn',
           conditionalPanel("input.sidebar === 'demo_vis'",
                            h4("CDC SVI Visualization Settings"),
                            radioButtons(
                              inputId = "geolevel",
                              label = "Geographic Level",
                              choices = c(
                                "County" = "county",
                                "Census Tract" = "tract"
                              ),
                              selected = "county"
                            ),
                            pickerInput(
                              inputId = "sviChoice",
                              label = "SVI Variable",
                              choices = svi_descs, #svi_ga_county%>%st_drop_geometry()%>%select(starts_with("EP_"), starts_with("SPL_"))%>%names,
                              selected = "SPL_THEMES",
                              multiple = FALSE
                            ),
                            radioButtons(
                              inputId = "showProv",
                              label = "Show Selected Providers?",
                              choices = c("Yes" = 0.7,"No" = 0),
                              selected = 0
                              
                            ))
                            
           ),
      menuItem("About", tabName = "about",icon = icon('info-circle')),
      div( id = 'sidebar_cn',
           conditionalPanel("input.sidebar === 'about'"
           )
           ),
      menuItem("Feedback", tabName = 'feedback', icon = icon('comments')),
      div( #id = 'sidebar_cn',
           conditionalPanel("input.sidebar==='feedback'",
                            uiOutput("feedback"))
      )
    )
    # tags$style(
    #   "#sidebarItemExpanded {
    #         overflow: auto;
    #         max-height: 80vh;
    #     }"
    # ),
    
    # ,
    #
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'search_tab',
        tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
        leafletOutput("mymap")
        
      ),
      tabItem(
        tabName = "prov_list",
        DTOutput(
          "printed_table", height = "calc(100vh - 100px)"
        )
        #)
      ),
      tabItem(
        tabName = "demo_vis",
        tags$style(type = "text/css", "#demogvis {height: calc(100vh - 80px) !important;}"),
        leafletOutput("demogvis")
      ),
      tabItem(
        tabName = "about",
        div( id = "longtext",
             tags$style(
               "#longtext {
               overflow = auto;
               max-width: 750px;
               }"
               
             ),
        h2("About the Project"),
        p(
          "This map was conceived and designed to help people in Georgia navigate and coordinate autism services. Additionally, it provides an educational tool for those learning about the relationship between structural inequalties and location of autism services across the state of Georgia."
        ),
        p(
          "We are unable to vet each service type or service provider in the database, so their presence in this database is not an endorsement of their efficacy, ethics, or existence. If you have feedback regarding a specific service or provider, please tell us through the feedback link. We will use this information to  continue updating the app."
        ),
        h3("Using the Program"),
        p("The Autism Services Map is composed of three sections that all interact with each other: the provider search, the provider list, and the demographic information."),
        h4("Provider Search", style = 'margin-left: 0px;'),
        p(
          "The Provider Search tab lets you select the characteristics of providers that are valuable to you as well as the specific services you need.",br(),"Your selection will carry through to the other sections, and you can return here to make a new selection."
        ),
        strong("Transit Access"),
        p("This map considers service providers within 1/4 mile of a bus or rail stop to be transit accessible. Counties throughout Georgia provide transit services on request through paratransit, which is not considered here because there are no fixed stops or routes. Currently, the map accounts for the following transit providers:"),
        p("Metropolitan Atlanta Rapid Transit Authority (MARTA)",br(),"Chatham Area Transit (CAT)",br(),"Gwinnett County Transit (GCT)",br(),"CobbLinc",br(),"XPRESS", style = 'margin-left: 40px;' ),
        h4("Provider List", style = 'margin-left: 0px;'),
        p("The Provider List shows providers matching your search in table form, which can be searched, sorted, and filtered. Clicking \"Generate CSV\" in the sidebar will download a .csv file with all the providers matching your search, even if you use the filters on the table."),
        h4("Demographic Information", style = 'margin-left: 0px;'),
        p("The Demographic Information tab displays CDC/ATSDR Social Vulnerability Index data at the county and census tract level. You can also overlay the selected provider search data points."),
        strong("Variable Definitions"),
        p("Persons below poverty",br(),
        "Unemployed civilians",br(),
        "Per capita income",br(),
        "Persons 25+ with no high school diploma",br(),
        "Persons aged 65 and older",br(),
        "Persons aged 17 and younger",br(),
        "Civilian with a disability",br(),
        "Single parent households with children under 18",br(),
        "Minority",br(),
        "Persons who speak English 'less than well'",br(),
        "Multi-family housing (10+)",br(),
        "Mobile homes",br(),
        "Crowded households",br(),
        "Households with no vehicle available",br(),
        "Persons in group quarters",br(),
        "Series for Socioeconomic theme",br(),
        "Series for Household Composition theme",br(),
        "Series for Minority Status/Language theme",br(),
        "Series for Housing Type/Transportation theme",br(),
        "Overall Series", style = 'margin-left: 40px;'),
        h3("Credits"),
        p(
          "Professor: Dr. Jennifer Singh"
        ),
        p("
          Programming: Freyja Brandel-Tanis"
        ),
        p("Data Gathering: Freyja Brandel-Tanis, Shristi ., Elise Zheng, Dr. Jennifer Singh
          "
        ),
        p("Grant funding provided through the ",a("Digital Integrative Liberal Arts Center",href='https://dilac.iac.gatech.edu/', target="_blank", rel="noopener noreferrer")," at the Georgia Tech Ivan Allen College of Liberal Arts"
        ),
        p(
          "This program was made using Shiny in R v4.0.5, along with RStudio and MySQL, and is hosted using Shiny Server. The code and full list of packages will be made available on github."
        ),
        h3("Sources"),
        p("Citation tags included in parentheses for service data providers."),
        p(a("Parent to Parent of Georgia", href='https://www.p2pga.org/', target="_blank", rel="noopener noreferrer")," (p2p)",
          br(),
        a("Autism Speaks*", href='https://www.autismspeaks.org/', target="_blank", rel="noopener noreferrer")," (as)",
        br(),
        a("Autism Society of America*", href='https://source.autism-society.org/autismsource', target="_blank", rel="noopener noreferrer")," (asa)",
      br(),
      a("Georgia Department of Public Health*", href='https://sendss.state.ga.us/sendss/!mch.coord_search', target="_blank", rel="noopener noreferrer")," (dph)"),
      p(a("CDC/ATSDR Social Vulnerability Index (SVI) Data", href='https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html', target="_blank", rel="noopener noreferrer")),
      p("Transit stop data from ",a("OpenMobilityData", href = 'https://transitfeeds.com/', target="_blank", rel="noopener noreferrer"), " in General Transit Feed Specification (GTFS) format."),
      
        p(em("* Starred providers are not currently integrated into the live version of the app."))
        
        )
      )
    )
    
  )
)


server <- function(input, output, session) {
  jsc <- '
  $(document).ready(function () {
    $(".sidebar-menu").children("li").on("click", function() {
      $("#mult, #single").toggle();
      });
    });
    '

  observe({
    if(input$selectall == 0) return(NULL)
    else if (input$selectall > 0)
    {
      updatePickerInput(session,"adult","Adult-Specific",choices=adult_var,selected=unlist(adult_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"advoc","Advocacy",choices=advoc_var,selected=unlist(advoc_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"adap","Adaptations / Accessibility",choices=adap_var,selected=unlist(adap_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"care","Camps / Caregiving",choices=care_var,selected=unlist(care_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"child","Early Childhood-Specific",choices=child_var,selected=unlist(child_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"edu","Education",choices=edu_var,selected=unlist(edu_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"health","Healthcare",choices=health_var,selected=unlist(health_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"eval","Evaluation / Diagnosis",choices=eval_var,selected=unlist(eval_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"mental","Mental Health / Behavior",choices=mental_var,selected=unlist(mental_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"pfam","Parent / Family Support",choices=pfam_var,selected=unlist(pfam_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"plan","Planning for the Future",choices=plan_var,selected=unlist(plan_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"therp","Therapies and Interventions",choices=therp_var,selected=unlist(therp_var),
                        options = list(`actions-box` = TRUE))
    }
  })
  
  observe({
    if(input$selectnone == 0) return(NULL)
    else if (input$selectnone > 0)
    {
      updatePickerInput(session,"adult","Adult-Specific",choices=adult_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"advoc","Advocacy",choices=advoc_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"adap","Adaptations / Accessibility",choices=adap_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"care","Camps / Caregiving",choices=care_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"child","Early Childhood-Specific",choices=child_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"edu","Education",choices=edu_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"health","Healthcare",choices=health_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"eval","Evaluation / Diagnosis",choices=eval_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"mental","Mental Health / Behavior",choices=mental_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"pfam","Parent / Family Support",choices=pfam_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"plan","Planning for the Future",choices=plan_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"therp","Therapies and Interventions",choices=therp_var,selected=NULL,
                        options = list(`actions-box` = TRUE))
    }
  })
  
  observe({
    if(input$selectall1 == 0) return(NULL)
    else if (input$selectall1 > 0)
    {
      updatePickerInput(session,"age_range","Age",choices=age_var,selected=unlist(age_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"hours","Operating Hours",choices=hours_var,selected=unlist(hours_var),
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"pay_type","Payment Options",choices=pay_var,selected=unlist(pay_var),
                        options = list(`actions-box` = TRUE))
    }
  })
  
  observe({
    if(input$selectnone1 == 0) return(NULL)
    else if (input$selectnone1 > 0)
    {
      updatePickerInput(session,"age_range","Age",choices=age_var,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"hours","Operating Hours",choices=hours_var,
                        options = list(`actions-box` = TRUE))
      updatePickerInput(session,"pay_type","Payment Options",choices=pay_var,
                        options = list(`actions-box` = TRUE))
    }
  })
  
  # observe({
  #   updateSelectizeInput()
  # })
  # 
  

    observe({
  cat(input$trwh)
      })
    
  # Creation of Reactive Dataframe for map and provider list
    reactive_db  <-  reactive({
      
      p_list_s <- p2p_sd%>%
          # select(-row_names)%>%
        filter(
          service %in% c(as.character(input$therp), as.character(input$plan), as.character(input$pfam), as.character(input$mental), as.character(input$eval), as.character(input$health), as.character(input$edu), as.character(input$child), as.character(input$care), as.character(input$advoc), as.character(input$adult), as.character(input$adap)),
          offered==1
        )%>%
        pull(1)
      
      p_list_pay <- p2p_sd%>%
        # select(-row_names)%>%
        filter(
          category == "a_payment",
          service %in% as.character(input$pay_type),
          offered==1
        )%>%
        pull(1)
      
      
      # p_list_therp <- p2p_sd%>%
      #   select(-row_names)%>%
      #   filter(
      #     service %in% as.character(input$therp),
      #     offered==1
      #   )%>%
      #   pull(1)

      p_list_age <- p2p_pd%>%
        # select(-row_names)%>%
        filter(
          pfx == "age",
          values %in% as.character(input$age_range)
          #"21+ Years Old"#
        )%>%
        pull(1)
      
      
      
      p_list_hour <- p2p_pd%>%
        # select(-row_names)%>%
        filter(
          pfx == "hour",
          values %in% as.character(input$hours)
        )%>%
        pull(1)
      
      # p_list_wheel <- p2p_pd%>%
      #   # select(-row_names)%>%
      #   filter(
      #     pfx == "wheelchair_access",
      #     values == 1
      #   )%>%
      #   pull(1)
      # p_list_transit <- p2p_pd%>%
      #   # select(-row_names)%>%
      #   filter(
      #     pfx == "transit_access",
      #     values == 1
      #   )%>%
      #   pull(1)
      
      # p2p_pd2 <- p2p_pd%>% 
      #   mutate(
      #     values = ifelse(pfx == "transit_access" & provider_id %in% p2p_prov_transit, 1, values)
      #   )%>%
      #   filter(
      #     pfx == "transit_access",
      #     provider_id %in% p2p_prov_transit
      #   )
      
      if(is.null(c(input$trwh))==TRUE){
        p_list_trwh <- p2p_prov$provider_id%>%unique
      }else if (length(input$trwh)==1){
        p_list_trwh <- p2p_pd%>%
          # select(-row_names)%>%
          filter(
            pfx %in% as.character(input$trwh),
            values == 1
          )%>%
          pull(1)
      }else{
        p_list_trwh <- intersect(
          p2p_pd%>%
          # select(-row_names)%>%
          filter(
            pfx %in% as.character(input$trwh)[1],
            values == 1
          )%>%
          pull(1),
          p2p_pd%>%
            # select(-row_names)%>%
            filter(
              pfx %in% as.character(input$trwh)[2],
              values == 1
            )%>%
            pull(1)
        )
      }
      
      # p_list_prov <- p2p_pd%>%
      #     select(-row_names)%>%
      #   filter(
      #     pfx == "age",
      #     values %in% as.character(input$age_range)#"21+ Years Old"#
      #   )%>%
      #   pull(1)

      p2p_prov%>%
          # select(-row_names)%>%
        filter(
          provider_id %in% p_list_pay,
          provider_id %in% p_list_age,
          provider_id %in% p_list_s,
          provider_id %in% p_list_trwh,
          provider_id %in% p_list_hour
          
          # provider_id %in% p_list_therp,
          # provider_id %in% p_list_prov
        )%>%
        # select(
        #   -lat, -lon
        # )%>%
        left_join(
          .,
          p2p_pd, by = "provider_id"
        )%>%
        pivot_wider(
          # id_cols = provider_id,
          names_from = "pfx",
          values_from = "values",
          values_fn = function(x)paste(unlist(x), collapse = "; "),
          values_fill = NA_character_
        )
      
  # attempt for search function    
      # if (is.null(input$name_search)) {
      #   p2p_prov%>%
      #     # select(-row_names)%>%
      #     filter(
      #       provider_id %in% p_list_pay,
      #       provider_id %in% p_list_age,
      #       provider_id %in% p_list_s,
      #       provider_id %in% p_list_trwh,
      #       provider_id %in% p_list_hour
      #       
      #       # provider_id %in% p_list_therp,
      #       # provider_id %in% p_list_prov
      #     )
      # }else{
      #   p2p_prov%>%
      #     # select(-row_names)%>%
      #     filter(
      #       provider_id %in% p_list_pay,
      #       provider_id %in% p_list_age,
      #       provider_id %in% p_list_s,
      #       provider_id %in% p_list_trwh,
      #       provider_id %in% p_list_hour#,
      #       # name %in% as.character(input$name_search)
      #       # provider_id %in% p_list_therp,
      #       # provider_id %in% p_list_prov
      #     )
      # }
      
      #
     # p2p_prov%>%
     #   select(-row_names)%>%
     #    right_join(.,p2p_sd%>%
     #                 select(-row_names)%>%
     #    filter(
     #      category == "a_payment",
     #      service %in% "medicaid",#as.character(input$pay_type), #
     #      offered==1
     #    ),by = "provider_id")%>%
     #    inner_join(.,p2p_pd%>%
     #                 select(-row_names)%>%
     #                 filter(
     #                   pfx == "age",
     #                   values %in% "21+ Years"#as.character(input$age_range) #
     #                 ),by = "provider_id")

      
      
      
      
        # mutate(
        #   medicaid1 = if_else(medicaid==1,TRUE,FALSE)
        # )%>%
        # filter(
        #    medicaid1 == input$medicaid
        #   )%>%
        #     filter(
        #       over_21 %in% input$over21
        #     )
    })
    observe({
      print(as.character(input$name_search))
    })
    observe({
      print(names(reactive_db()))
    })
    
    
    observeEvent(input$search_go,{
      # reactive_db() <- reactive_db()%>%
      #   filter(
      #     name %in% as.character(input$name_search)
      #     )
      
      test4 <<- as.character(input$name_search)
      cat(test4)
    })
    
    # reactive_db <- eventReactive(input$search_go,{
    #   if(length(input$name_search)>0){
    #   reactive_db()%>%
    #       filter(
    #         name %in% as.character(input$name_search)
    #         )
    #   }else{
    #     reactive_db()%>%
    #       filter(
    #         name %in% as.character(input$name_search)
    #       )
    #   }
    # })
    

      printed_data <- reactive({
        reactive_db()%>%
          select(.,
            -lat, -lon, -zip#, -city, -state
          )
          
        # reactive_db()%>%
        #   select(
        #     -lat, -lon
        #   )%>%
        #   left_join(
        #     .,
        #     p2p_pd, by = "provider_id"
        #   )%>%
        #   pivot_wider(
        #     # id_cols = provider_id,
        #     names_from = "pfx",
        #     values_from = "values",
        #     values_fn = function(x)paste(unlist(x), collapse = "; "),
        #     values_fill = NA_character_
        #   )%>%
        #   select(
        #     -disability, -zip#, -city, -state
        #   )#%>%
          # left_join(
          #   .,
          #   p2p_sd, by = "provider_id"
          #   )%>%
          # filter(
          #   offered == 1
          # )%>%
          # pivot_wider(
          #   id_cols = provider_id,
          #   names_from = "category",
          #   values_fn = paste("service",collapse = "; ")
          # )
        })
      #%>%
        # %>%
        # left_join(
        #   p2p_pd, by = "provider_id"
        # )
      
      
    
    
    output$printed_table <- renderDT({
      datatable(printed_data(), rownames = FALSE, fillContainer = TRUE)
      # options = list(scrollX = TRUE)
    })
    
    observe({
      cat(paste0(nrow(reactive_db()),", "))
    })
    # 
    # output$results <- renderTable({
    #     evictions%>%
    #     filter(
    #       Date >= input$plot_dates[1] & Date <= input$plot_dates[2]
    #     )%>%
    #     group_by(Address, Lat, Long)%>%
    #     summarise(
    #       count_tot = sum(Counts)
    #     )%>%
    #     filter(
    #       count_tot >= input$counts[1] & count_tot <= input$counts[2]
    #     ) %>%
    #     ungroup%>%
    #       select(Address)
    #     })
    
    demVis <- reactive({
      if(as.character(input$geolevel)=="county"){
        svi_ga_county%>%
          select(
            LOCATION, "varC" = input$sviChoice
          )%>%
          filter(
            varC != -999.0000
          )
      }else{
        svi_ga_tracts%>%
          select(
            LOCATION, "varC" = input$sviChoice
          )%>%
          filter(
            varC != -999.0000
          )
      }
    })
    
    svi_unit <- reactive({
      if(input$sviChoice == "EP_PCI"){
        "Income"
      }else if(str_detect(input$sviChoice, "EP_")==TRUE){
        "Percent"
      }else {
        "Vulnerability"
      }
    })
    
    output$mymap <- renderLeaflet({
        basemap
    })
    
    output$demogvis <- renderLeaflet({
      basemap
    })
    
    # toListen <- reactive({
    #     list(input$plot_dates,input$counts)
    # })
    
    observe({
      validate(
        need(
          nrow(reactive_db())!=0,
          'No service providers match')
      )
      
      # req(input$therp | input$plan | input$pfam | input$mental | input$eval | input$health | input$edu | input$child | input$care | input$advoc | input$adult | input$adap)
      # req(c(input$therp, input$plan, input$pfam, input$mental, input$eval, input$health, input$edu, input$child, input$care, input$advoc, input$adult, input$adap))
      # req(input$adult)
      # req(input$therp)

      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearShapes() %>%
          # # Marta stops
          # addCircleMarkers(data = marta_stops_sql, lat = ~ stop_lat, lng = ~ stop_lon, weight = 1, radius = 3,
          #                  fillOpacity = 0.2, color = 'red', fillColor = 'red', opacity = 0.2,
          #                  label = sprintf("%s", str_to_title(marta_stops_sql$stop_name)) %>% lapply(htmltools::HTML),
          #                  labelOptions = labelOptions(
          #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'black'),
          #                    textsize = "15px", direction = "auto"))%>%
            addCircleMarkers(data = reactive_db(), lat = ~ lat, lng = ~ lon, weight = 1, radius = 5,
                             fillOpacity = 0.7, color = 'blue', fillColor = 'black',
                             label = sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>%s<br/>%s", reactive_db()$name, paste("Address:",str_to_title(reactive_db()$address)), paste("Website:",reactive_db()$website), paste("Phone:",reactive_db()$phone), paste("Source:", reactive_db()$source)) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'black'),
                                 textsize = "15px", direction = "auto"),
                             options = providerTileOptions(maxZoom = 10)
                             
                             # Show transit stop 1/4 mi buffers
                             #                      )%>%
                             # addPolygons(
                             #   data = stops_all
        )
        #
        #         # addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deathsper100k))
    }#, autoDestroy = FALSE
    )
    
    
    
    # observeEvent(input$showProv,{
    # observeEvent(input$sidebar,{
    
    
    
    observe({
      
      validate(
        need(
          nrow(reactive_db())!=0,
          'No service providers match'),
        need(input$sidebar == 'demo_vis', "o")
      )
      # max_count <- max(demVis()$varC)
      
      # cv_pal <- reactive({
      #   if(input$sviChoice == "EP_PCI"){
      cv_pal <-  colorNumeric("Purples", domain = demVis()$varC)#, reverse = TRUE)
        #   }else{
        #     colorNumeric("Purples", domain = demVis()$varC)
        #     }
        # })
      c_pal <- colorNumeric(
        palette = purple_ramp(seq(0,1,length=100)),
        domain = demVis()$varC
      )

      leafletProxy("demogvis") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = demVis(), fillOpacity = 0.5, opacity = 0.5, weight = 0.2, color="black", label = sprintf("%s<br/>%f", demVis()$LOCATION, round(as.numeric(demVis()$varC),2))%>% lapply(htmltools::HTML), fillColor = ~cv_pal(demVis()$varC)
          )%>%
        addCircleMarkers(data = reactive_db(), lat = ~ lat, lng = ~ lon, weight = 1, radius = 5, opacity = input$showProv,
                         fillOpacity = input$showProv, color = 'blue', fillColor = 'black',
                         label = sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>%s<br/>%s", reactive_db()$name, paste("Address:", str_to_title(reactive_db()$address)), paste("Website:",reactive_db()$website), paste("Phone:",reactive_db()$phone), paste("Source:", reactive_db()$source)) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'black'),
                           textsize = "15px", direction = "auto"))%>%
        addLegend("bottomright", pal = c_pal, values = demVis()$varC, title = sprintf("<small>%s</small>", svi_unit())
                  )
      # }else{
      #   cv_pal <- colorNumeric("Purples", domain = demVis()$varC)
      #   
      #   c_pal <- colorNumeric(
      #     palette = purple_ramp(seq(0,1,length=100)),
      #     domain = demVis()$varC
      #   )
      #   
      #   leafletProxy("demogvis") %>%
      #     clearMarkers() %>%
      #     clearShapes() %>%
      #     clearControls() %>%
      #     addPolygons(
      #       data = demVis(), fillOpacity = 0.5, opacity = 0.5, color="black", label = sprintf("%s<br/>%f", demVis()$LOCATION, round(as.numeric(demVis()$varC),2))%>% lapply(htmltools::HTML), fillColor = ~cv_pal(demVis()$varC)
      #     )%>%
      #     addLegend("bottomright", pal = c_pal, values = demVis()$varC, title = "<small>Percentile</small>"
      #     )
      # }
      #%>%
        # addLegendCustom(colors = c(c_pal(0.1), c_pal(25), c_pal(50), c_pal(75), c_pal(100)), labels = c("0", "25", "50", "75","100"))
      # addLegendCustom(colors = c(c_pal(10), c_pal(50), c_pal(100), c_pal(150), c_pal(192)), labels = c("3", "50", "100", "150","200"), sizes = c(10^(1/2), 50^(1/2), 100^(1/2), 150^(1/2), 200^(1/2)))
      
        # addCircleMarkers(data = reactive_db(), lat = ~ lat, lng = ~ lon, weight = 1, radius = 5,
        #                  fillOpacity = 0.7, color = 'blue', fillColor = 'black',
        #                  label = sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>%s", reactive_db()$name, str_to_title(reactive_db()$address), reactive_db()$website, paste0("Phone: ",reactive_db()$phone)) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'black'),
        #                    textsize = "15px", direction = "auto"))# %>%
      #
      #         # addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deathsper100k))
    }#, autoDestroy = FALSE
    )
    
    # output$result <- renderUI(
    #          downloadButton("report","Generate CSV", class = 'butt'),
    #          tags$style(".butt{color: black}")
    #   
    # )
    
    # observeEvent(input$sidebar,{
    #   if(input$sidebar == "demo_vis"){
    #     singleton(tags$body(tags$script("$function() {
    #     map.invalidateSize();
    #                                     };"
    #                                     )))
    #   }
    # })
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){
        paste(str_replace_all(today(),"-","_"),"providers.csv",sep = "")
      },
      content = function(file) {
        write_csv(printed_data(), file)
      }
    )
    
    url_feedback <- "https://gatech.co1.qualtrics.com/jfe/form/SV_06Bf5ooVas9OJ7w"
    
    output$feedback <- renderUI({
      tagList(h4(a("Submit Feedback",href = url_feedback, target="_blank", rel="noopener noreferrer"),
                 tags$style("h4 {margin:  0 0 0 15px;}")
                 )) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
