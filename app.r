library(shiny)
library(here) # To call data w/out parsing a local path - ogr/dsn error
library(bslib) # App theme
library(readxl)
library(leaflet) # Map plot
library(rgdal) # Real .shp file
library(rsconnect)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1) Import Data
### << TERRITORY >> ###
GB_terr <- readOGR("data\\Poly_wTerrs_wCntPrc_FIN.shp"
                   )

# 2) Prep data
# 2a) Create labels
GB_terr$LabelText <- paste0(
                            "<b>Territory: </b>", GB_terr$Territory,"<br>",                  
                            "<b>Segment 1: </b>", GB_terr$X1._M, "<b> | </b>", GB_terr$X1._M_perc,"%<br>",
                            "<b>Segment 2: </b>", GB_terr$X2._P, "<b> | </b>", GB_terr$X2._P_perc,"%<br>",
                            "<b>Segment 3: </b>", GB_terr$X3._G, "<b> | </b>", GB_terr$X3._G_perc,"%<br>",
                            "<b>Segment 4: </b>", GB_terr$X4._HG, "<b> | </b>", GB_terr$X4._HG_perc,"%<br>",
                            "<b>Total: </b>", GB_terr$Total,"<br>"
                            )

# 2b) Create  color palette - legend / markers
my_colors_TERR <-  c("#7D5DFD", "#2F3F49", "#11CEBD", "#0900FF") 

# Setting the colors based on a col, e.g. MaxCov_seg
color_factor_TERR <- colorFactor(
                                 palette = my_colors_TERR, 
                                 domain = GB_terr$MaxCov_seg
                                 )

### << HCO >> ###
# 1) Import Data                              
GB_hco <- read_excel("data\\HCO_local_HCO_n_Retailer_segmented.xlsx"
                     )
                    
GB_hco = GB_hco[order(GB_hco$Segment), ] # Sort df - for accurate col referencing

# 2) Prep data
# 2a) Create labels
GB_hco$LabelText <- paste0(
                           "<b>Root Post Code: </b>", GB_hco$Postcode,"<br>", 
                           "<b>Territory: </b>", GB_hco$Territory,"<br>",                  
                           "<b>Segment: </b>", GB_hco$Segment, "<b>"
                           )

# 2b) Create  colour palette - legend / markers
my_colors_HCO <- c("#7D5DFD", "#2F3F49", "#11CEBD", "#0900FF") 

color_factor_HCO <- colorFactor(
                                palette = my_colors_HCO,
                                domain = GB_hco$Segment
                                )


# 3) Create app UI
ui <- navbarPage(
                 ### << GENERAL >> ### 
                 title = "GB Segmentation",
                 
                 absolutePanel(top = 5, right = 5, 
                               style = "font-size: small, 
                                        color: grey", 
                               "Alpha"),
    
                 theme = bs_theme(version = 4, bootswatch = 'minty'),
                 
                 # Create Tabs for each plot
                 ### << TERRITORY >> ###
                 tabPanel(
                          title = "GB Territory View",
                          
                          # Plot to show - TERR
                          leafletOutput("GB_Terr_plot", height=1000) # Plot to show - TERRITORY
                          ),
                          
                   ### << HCO >> ###
                   tabPanel(
                            title = "GB Detailed View",
                            
                            # Creating row to store filters
                            fluidRow(
                                    column(width = 1), # spacer
                                    
                                    # Creating filter - TERRITORY
                                    column(width = 3,
                                           selectInput(
                                                       inputId = "HCO_Territory", # Ref name of slicer
                                                       label = "Select a Territory:", 
                                                       choices = c("All", unique(GB_hco$Territory)), # *All = Select all slicers
                                                       selected = "All", # Default val
                                                       width = "75%"
                                                       ),
                                           
                                           # Creating reset button
                                           actionButton(inputId = "HCO_Territory_RESET", 
                                                        label = "Reset", 
                                                        icon = icon("times"), 
                                                        width = "60%"),
                                           
                                           # Creating zoom button
                                           actionButton(inputId = "HCO_Territory_ZOOM", 
                                                        label = "🔎",
                                                        align = "centre",
                                                        width = "15%"),
                                           ),
                                    
                                     column(width = 1), # spacer
                                    
                                     # Creating filter - SEGMENT
                                     column(width = 3,
                                            selectInput(
                                                        inputId = "HCO_True_Segment", # Ref name of slicer
                                                        label = "Select a Segment:", 
                                                        choices = c("All", unique(GB_hco$Segment)),
                                                        selected = "All", # Default val
                                                        width = "75%"
                                                        ),  
                                            
                                            # Creating reset button
                                            actionButton(inputId = "HCO_True_Segment_RESET", 
                                                         label ="Reset",
                                                         icon = icon("times"), 
                                                         width = "75%"),
                                            ),
                                    ),
                            
                            # Plot to show - HCO
                            leafletOutput("GB_HCO_plot", height=1000)
                            ),
                 
                 # User guide page
                 navbarMenu("More",
                            tabPanel(
                                     title = "User Guide"
                                     ),
                            
                            tabPanel(
                                     title = "Contact Us"
                                     )
                            ),
                 )
          

server <- function(input, output, session){
  
                                  ### << TERRITORY >> ###
                                
                                  # Plot gen
                                  output$GB_Terr_plot <- renderLeaflet({
                                                                        leaflet() %>% 
                                                                          
                                                                          addTiles() %>% 
                                                                          
                                                                          # View set on UK, by default
                                                                          setView(
                                                                                  lat = 55.378051, 
                                                                                  lng = -3.435973, 
                                                                                  zoom = 6
                                                                                   ) %>% 
                                                                          
                                                                          # Add polygons - territories
                                                                          addPolygons(
                                                                                      data = GB_terr, 
                                                                                      color = ~color_factor_TERR(MaxCov_seg), # Setting colours based on a column
                                                                                      weight = 2, 
                                                                                      popup = ~LabelText,
                                                                                      highlightOptions = highlightOptions(
                                                                                                                          color = "black",
                                                                                                                          weight = 2,
                                                                                                                          bringToFront = TRUE
                                                                                                                        )
                                                                                    ) %>%
                                                                          
                                                                          # Add hovering labels (on top of the polygon)
                                                                          addLabelOnlyMarkers(
                                                                                              data = GB_terr,
                                                                                              lng = ~Cent_long, 
                                                                                              lat = ~Cent_lat,
                                                                                              label = paste0(GB_terr$MaxCov_per),
                                                                                              labelOptions = labelOptions(
                                                                                                                          noHide = TRUE, # Always active
                                                                                                                          textOnly = TRUE, # No marker
                                                                                                                          textsize = "14px"
                                                                                                                        ) 
                                                                                            ) %>%
                                                                                            
                                                                          
                                                                          addLegend(
                                                                                    "topright", 
                                                                                    pal = color_factor_TERR, 
                                                                                    values = GB_terr$MaxCov_seg,
                                                                                    title = "Hightest % Segment"
                                                                                  )
                                                                        
                                                                        })
                                  
                                  ### << HCO >> ###
                                  
                                  # React to drop-down slicer input
                                  ## If *All not selected, filter based on selection - DUAL FILTER {terr/seg}
                                  HCO_filt_GB <- reactive({
                                                           filt_data <- GB_hco
                                                           if (input$HCO_Territory != "All"){
                                                                                             filt_data <- filt_data[filt_data$Territory == input$HCO_Territory, ]
                                                                                            }
                            
                                                           if (input$HCO_True_Segment != "All"){
                                                                                                filt_data <- filt_data[filt_data$Segment == input$HCO_True_Segment, ]
                                                           }
                                                           
                                                           filt_data # Return filtered data
                                                           })
                                  
                                  # Observe interaction w/ the reset buttons - when clicked, reset input w/ "All"
                                  observeEvent(input$HCO_Territory_RESET,{
                                                                          updateSelectInput(session, "HCO_Territory", selected = "All")
                                                                         }
                                               )              
                                  observeEvent(input$HCO_True_Segment_RESET,{
                                                                             updateSelectInput(session, "HCO_True_Segment", selected = "All")
                                                                            }
                                               )                                  
                                                       
                                  
                                  # Plot gen
                                  output$GB_HCO_plot <- renderLeaflet({
                                                                        leaflet() %>% 
                                                                          
                                                                          addTiles() %>% 
                                                                          
                                                                          # View set on UK, by default
                                                                          setView(lat = 55.378051,
                                                                                  lng = -3.435973,
                                                                                  zoom = 6
                                                                                  ) %>% 
                                      
                                                                          # << Can add light polygons - to highlight territories >>
                                                                          
                                                                          # HCO specific   
                                                                          addCircleMarkers(data = HCO_filt_GB(), # Call reactive expression
                                                                                           lng = ~HCO_longitude,
                                                                                           lat = ~HCO_latitude,
                                                                                           color = ~color_factor_HCO(HCO_filt_GB()$Segment),
                                                                                           radius = 2,
                                                                                           popup = ~LabelText,
                                                                                           fillOpacity = 0.7
                                                                                           ) %>%
                                      
                                                                          
                                                                          
                                                                          addLegend("topright", 
                                                                                    pal = color_factor_HCO,
                                                                                    values = c(GB_hco$Segment),
                                                                                    title = "Segment")
                                                                       })
                                  
                                  # Zoom into the map upon "🔎" click 
                                  ## Updates the setView func
                                  observeEvent(input$HCO_Territory_ZOOM, {
                                                                           proxy <- leafletProxy("GB_HCO_plot")
                                                                           HCO_filt_GB_c <- isolate(HCO_filt_GB())
                                                                           proxy %>% setView(lng = HCO_filt_GB_c$Cent_long[1], # Index to get a sinlge val
                                                                                             lat = HCO_filt_GB_c$Cent_lat[1], # Index to get a sinlge val
                                                                                             zoom = 9)
                                                                          }
                                               )
                                  
                                  # Reset map view upon reset click
                                  ## Updates the setView func
                                  observeEvent(input$HCO_Territory_RESET, {
                                                                            proxy <- leafletProxy("GB_HCO_plot")
                                                                            proxy %>% setView(lat = 55.378051,
                                                                                              lng = -3.435973,
                                                                                              zoom = 6
                                                                                              )
                                                                            }
                                               )
                                  
                                  }
            


shinyApp(ui=ui, server=server)
