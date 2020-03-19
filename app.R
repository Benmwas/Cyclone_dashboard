library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(scales)
library(leaflet)

data <- readxl::read_excel("./Data/zimbabwe_data.xlsx")

data[is.na(data)] = 0
data$`End_date
` <-  as.Date(data$`End_date
`, format = "%m/%d/%Y")

hdx_location <- readxl::read_excel("./Data/HDX_location_zimbabwe.xlsx")

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(
        title = "HUMANITARIAN    RESPONSE   TO   CYCLONE    IDAI  IN  ZIMBABWE", titleWidth = 800,
        tags$li(actionLink("LinkedIn", 
                           label = "", 
                           icon = icon("linkedin"),
                           onclick = "window.open('https://www.linkedin.com/in/benjamin-mwasambo-957090a5/')"),
                class = "dropdown")
        
    ),
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Map", tabName = "map", icon = icon("globe")),
            menuItem("Data Explorer", tabName = "data", icon = icon("table"),
                     menuSubItem("unicef data", tabName = "unicef"),
                     menuSubItem("hdx data", tabName = "hdx")),
            menuItem("About", tabName = "me", icon = icon("info"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Papyrus", serif;
        font-weight: bold;
        font-size: 18px;
      }
    '))),
        tabItems(
            tabItem(tabName = "dashboard",
                    
                    fluidRow(
                        valueBoxOutput("hygiene", width = 2
                                       ),
                        valueBoxOutput("water", width = 2
                                       ),
                        valueBoxOutput("sanitation", width = 2
                                       ),
                        valueBoxOutput("qty_hygiene", width = 2
                                       ),
                        valueBoxOutput("qty_water", width = 2
                                       ),
                        valueBoxOutput("qty_sanitation", width = 2)
                    ),
                    fluidRow(
                        tabBox(
                            width = 12,
                            
                            
                            tabPanel(
                                title = "Individuals targeted Vs individuals reached",
                                plotlyOutput(
                                    "comparison", height = "450px"
                                )
                                
                            ),
                            
                            tabPanel( 
                              title = "Percentage of Individuals covered by lead organizations",
                              plotlyOutput(
                                "organization", height = "500px"
                              )
                            ),
                            
                            
                            tabPanel(
                              title = "A map of individuals reached per district",
                              box(
                                width = "6 col-lg-12",
                                
                                tags$img(
                                  src = "Districts.jpg", height = 500, width = 700,
                                   style="display: block; margin-left: auto; margin-right: auto;"
                                )
                              )
                              
                            ),
                            tabPanel(
                              title = "Quantities of activity units delivered per month",
                              plotlyOutput("months", height = "400px"
                                           
                              )
                            )
                            
                            
                        )
                    ),
    
                    fluidRow(
                      column(
                        width = 3,
                  
                      ),
                      box( status = "primary",
                           class = "text-center",
                        h5("Categories of individuals reached as at October 2019"),
                        br(),
                        width = 5,
                        tableOutput("categories")
                      )
                  
                   )
                ),
            tabItem(
                tabName = "map",
                leafletOutput("districts", width="100%",height="1000px")
            ),
            tabItem(
                tabName = "unicef",
                div(
                    h1(strong("Unicef WASH data"))
                ),
                dataTableOutput("unicef_data")
            ),
            tabItem(
                tabName = "hdx",
                div(
                    h1(strong("HDX data"))
                ),
                dataTableOutput("hdx_data")
            ),
            
            tabItem(
              tabName = "me",
              fluidRow(
                box(
                  title = "About me",
                  status = "danger",
                  width = "6 col-lg-4",
                  tags$p(
                    class = "text -center",
                    tags$img(class = "img-responsive img-rounded center-block", src = "image.jpg", style = "max-width: 150px;")
                  ),
                    tags$p(
                    class = "text-center",
                    h5(tags$p(strong("Hi! I'm Benjamin."),
                           HTML(paste0(tags$a(href = "https://www.linkedin.com/in/benjamin-mwasambo-957090a5/", icon("linkedin"), target = "_blank")))))),
                           h4(tags$p(
                             "Welcome to my world of story telling,"
                           )),
                           
                    h4(tags$p(
                      "I'm a data scientist at",
                      HTML(paste0(tags$a(href = "https://farmdrive.co.ke/",
                                         "FarmDrive Limited",target = "_blank"), ",")), "Nairobi-Kenya; where I focus on data analytics and visualization,
                      interactive reporting, credit risk modelling and machine learning, geospatial and remote sensing analytics.")),
                    
                    h4(tags$p("I handle both spatial and non spatial (quantitative and qualitative) datasets. I enjoy deriving insights from data, writing technical blogs and building user friendly data tools,
                      like this dashboard.")),
                      
                      h4(tags$p("Get in touch with me through email: mwasambob@gmail.com."))
                    
                  
                
              ),
              box(
                title = "About this dashboard",
                status = "primary",
                width = "6 col-lg-4",
                tags$p(
                  class = "text-center",
                  tags$a(
                  href = "https://shiny.rstudio.com/",
                  target = "_blank",
                  tags$img(
                    class = "image-responsive",
                    src = "shiny.jpg",
                    style = "max-width: 150px;"
                    )
                  ),
                    tags$a(
                      href = "https://rstudio.com",
                      target = "_blank",
                      tags$img(class = "image-responsive",
                               src = "r-studio.jpg",
                               style = "max-width: 150px; margin-left: 2em;"
                      )
                    ),
                  tags$a(
                    href = "https://www.r-project.org",
                    target = "_blank",
                    tags$img(
                      class = "image-responsive",
                      src = "R_logo.svg.png",
                      style = "max-width: 150px; margin-left: 2em;"
                    )
                  )
                ),
                br(),
                h4(tags$p(
                  "This dashboard was built in", 
                  tags$strong("R"), "using packages;",
                  tags$strong("shiny,"),
                  tags$strong("shinydashboard,"),
                  tags$strong("leaflet"), "(for interactive web mapping,)",
                  tags$strong("plotly,"),
                  tags$strong("lubridate,"), "and many more packages"
                  
                )
              )
            ),
            box(
              title = "About this map",
              status = "warning",
              width = "6 col-lg-4",
              tags$p(
                class = "text center",
                tags$img(
                  src =  "Districts.jpg",
                  style = "max-width: 400px; margin-center: 2em;",
                  
                  br(),
                  h4(tags$p(
                    "This is a static color ramp map created using arcGIS/ArcMap 10.4, a product of",
                    HTML(paste0(tags$a(href = "https://www.esri.com/en-us/arcgis/about-arcgis/overview",
                                       "ESRI",target = "_blank"), ".")), "The map shows 5 classes variation of individuals reached
                    by lead organizations at district level. The shapefiles for Zimbabwe administrative bounderies were 
                    downloaded from", HTML(paste0(tags$a(href = "https://data.humdata.org/dataset/zimbabwe-administrative-levels-0-3-boundaries",
                                                         "HDX website",target = "_blank"), "."))
                  )
                  )
                )
              )
            )
          )
        ),
          
            
            tabItem(
                tabName = "intro",
                fluidRow(h2(HTML("<strong>Cyclone idai<strong>")),
                         h3(tags$p("In March 2019, one of the worst intense tropical cyclone idai was recorded in
                                   Africa and Southern Hemisphere which rendered more than 1,300 people dead in  Zimbabwe,
                                   Mozambique and Malawi. As a result of the strong winds brought by idai, severe flooding 
                                   was experienced affecting more than 3 million people. Several organizations played major 
                                   roles in humanitarian response to the affected areas.")),
                         br(),
                         
                         tags$p( column(
                           width = 4,
                           tags$img(
                             src = "zimbabwe.jpg", height =300, width =400,
                             style="display: block;  margin-right: auto;"
                           ),
                           "Image source:",
                                  class ="text-center",
                                  tags$a( "psmag.com",
                                          
                                          href = "https://psmag.com/news/viewfinder-cyclone-idais-devastating-damage-in-zimbabwe",
                                          
                            )
                         ),
                         column(
                           width = 8,
                           h3("This analysis brings to you the insights from humanitarian response to cyclone idai
                                   in Zimbabwe. The data used was obtained from the humanitarian data exchange (HDX) website and is
                                   only available from March 2019 to October 2019. However, this dashboard will be updated
                                   based on data availability."),
                           
                           h3(HTML("<strong>4W<strong>")),
                           
                           h3("A key tool to the coordination of humanitarian responses is coordinating between 
                                   humanitarian actors: Who is doing What, Where and When (4W). This dashboard shows such response
                                   as addressed by 4W. The interactive map developed using leaflet package in R shows affected areas in  
                                   different districts at wards level. Additional information can be viewed on the markers pop up")
                           
                         )
                     )
                )
           )
        )
    )
)

server <- function(input, output, session){
  output$hygiene <- renderValueBox({
      {hygiene <- data %>% 
          filter(data$`Sub Cluster` == "Hygiene")} 
      
          valueBox(format(sum(hygiene$`Total No. of Individuals Reached`, na.rm = TRUE), big.mark = ","), "HYGIENE - Total people reached", 
                   icon = icon("users"), color = "maroon", width = 2)
  })  
  
  output$water <- renderValueBox({
      {water <- data %>% 
          filter(data$`Sub Cluster` == "Water")}
      
     valueBox(format(sum(water$`Total No. of Individuals Reached`, na.rm = TRUE), big.mark = ","), "WATER - Total people reached",
              icon = icon("users"), color = "teal", width = 2)
                     
  })
  
  output$sanitation <- renderValueBox({
      {sanitation <- data %>% 
          filter(data$`Sub Cluster` == "Sanitation")}
      
      valueBox(format(sum(sanitation$`Total No. of Individuals Reached`, na.rm = TRUE), big.mark = ","), "SANITATION- Total people reached",
               icon = icon("users"), color = "purple", width = 2)
  })
  
  output$qty_hygiene <- renderValueBox({
      {qty_hygiene <- data %>% 
          filter(data$`Sub Cluster` == "Hygiene")}
      
      valueBox(format(sum(qty_hygiene$`Quantity Delivered`, na.rm = TRUE),big.mark = ","), "HYGIENE- Quantities delivered",
               icon = icon("medkit"), color = "green" , width = 2)
  })
  
  output$qty_water <- renderValueBox({
      {qty_water <- data %>% 
          filter(data$`Sub Cluster` == "Water")}
      
      valueBox(format(sum(qty_water$`Quantity Delivered`, na.rm = TRUE), big.mark = ","), "WATER- Quantities delivered",
               icon = icon("tint"), color = "orange", width = 2)
  })
  
  output$qty_sanitation <- renderValueBox({
      {qty_sanitation <- data %>% 
          filter(data$`Sub Cluster` == "Sanitation")}
      
      valueBox(format(sum(qty_sanitation$`Quantity Delivered`, na.rm = TRUE), big.mark = ","), "SANITATION- Quantities delivered",
               icon = icon("bath"), color = "blue", width = 2)
  })
  
  output$comparison <- renderPlotly({
    comparison <- plotdata %>% 
      select(Lead.Organization, Total_targeted, Total_delivered) %>% 
      gather(key = "variable", value = "value", -Lead.Organization)
    
    ggplot(comparison, aes(x = Lead.Organization, y = value))+
      geom_line(aes(color = variable,linetype = variable, group = 1))+
      scale_color_manual(values = c("darkred", "steelblue"))+
      theme(axis.text.x = element_text( size=8, angle=60, color="black"))+
      scale_y_continuous(labels = label_comma(big.mark = ","))+
      theme(panel.background = element_blank()) +
      theme(panel.grid.major = element_line(colour = "light grey"))+
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
      
    
  })
  
  output$organization <- renderPlotly({
      organization <- data %>% 
          group_by(data$`Lead Organization`) %>% 
          summarize(sum(`No. of Individual Targeted`),
                    sum(`Total No. of Individuals Reached`),
          ) 
      names(organization) <- c("Lead.Organization", "Total_targeted","Total_delivered")
      
      organization <- mutate(organization, Percentage_covered = (Total_delivered/Total_targeted)*100)
      
      plotdata <- organization 
         ggplot(plotdata, aes(x =Lead.Organization, y = Percentage_covered)) +
                 geom_bar(stat = "identity", position = "dodge") + 
                 geom_col(fill = '#FF9999', color = '#add8e6')+ ylab("Percentage covered")+
                 theme(plot.title = element_text(hjust = 0.5, colour = "dark green"))+
                 theme(axis.text.x = element_text(face = "bold", size=8, angle=60, color="black"))+
                  theme(panel.grid.minor.x=element_blank(),
                  panel.grid.major.x=element_blank()) +theme(panel.background = element_blank()) +
                  theme(panel.grid.major = element_line(colour = "light grey"))
                   
           
  })
  
  output$months <- renderPlotly({
    {
     months_data <- data %>% 
        group_by(Month) %>% 
        summarize(sum(`Quantity Delivered`))
      
      names(months_data) <- c("Months", "Quantity_delivered") 
      months_data$Months <- factor(months_data$Months,
                                  levels = c("March","April","May","June","July","August","September","October","November","December"))
    }
    
    ggplot(months_data, aes(x = Months, y = Quantity_delivered)) +
      geom_bar(stat = "identity", position = "dodge") + geom_col(fill = "#4682b4" , width = 0.5)+ ylab("Quantities delivered")+
       theme(plot.title = element_text(hjust = 0.5, colour = "purple"))+
      theme(axis.text.x = element_text(face = "bold", size=8, angle=60, color="black"))+
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
      
  })

  
 
  output$categories <- renderTable({
    
    summary <- data[, c("End_date
","No. of
 Men 
Reached", "No. of 
Boys 
Reached", "No. of 
Girls 
Reached", "No. of Women Reached")] 
    
    names(summary) <- c("End_date","Men_reached", "Boys_reached","Girls_reached", "Women_reached")
    summary$End_date <-  as.Date(summary$End_date, format = "%m/%d/%Y")
    
    
    
    summary %>% 
      summarize(
        Men_reached = format(sum(Men_reached), big.mark = ","),
        Boys_reached = format(sum(Boys_reached),big.mark = ","),
        Girls_reached = format(sum(Girls_reached),big.mark = ","),
        Women_reached = format(sum(Women_reached),big.mark = ",")
      )
    
  })
  
  
  
 
  
  output$districts <- renderLeaflet({
      hdx_location <- hdx_location %>% 
          mutate(
              pop_info = paste("District:",District, "<br/>","Ward name:",Ward,"<br/>","Affected:",`Affected Population`, "<br/>",
                               "Homes destroyed:",`Totally Destroyed Homes`, "<br/>", "Homes partially destroyed:",`Partially Destroyed Homes`,
                               "<br/>","Total IDPs:",`Total IDPs_in_Ward`, "<br/>")
          )
      hdx_location$magrange <- cut(hdx_location$`Affected Population`,
                                   breaks = c(0,500,2000,5000,11700),right = FALSE,
                                   labels = c("low[0-500]", "moderate[500-2000]", "high[2000-5000]", "very high[5000-11700]"))
      
      pal = colorFactor(palette = c("orange","blue", "green", "red"), domain = hdx_location$magrange)
      
      leaflet(width = "100%", height = "1000px") %>% 
        addTiles(group = "Esri World Imagery") %>% 
          addProviderTiles("CartoDB.Positron") %>% 
          addCircleMarkers(data = hdx_location, lat = ~ Latitude, lng = ~ Longitude, popup = ~pop_info,
                           color = ~pal(magrange),
                           label = paste("Affected pop. =", hdx_location$`Affected Population`, "Type=", hdx_location$magrange)) %>% 
      addLegend(pal = pal, values = hdx_location$magrange, title = "Population affected range" ) %>% 
        addLayersControl(baseGroups = c("CartoDB.Positron", "Esri World Imagery"), 
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        addMeasure() %>%
        addScaleBar()
  })
  
  output$unicef_data <- renderDataTable({
      datatable(
          data,
          class = 'cell-border stripe',
          filter = "top",
          editable = TRUE,
          options = list(pageLength = 5, scrollx = T)
      )
  })
  
  output$hdx_data <- renderDataTable({
      datatable(
          hdx_location,
          class = 'cell-border stripe',
          filter = "top",
          editable = TRUE,
          options = list(pageLength = 10, scrollx = T)
      )
  })
  
} 


shinyApp(ui = ui, server = server)









  