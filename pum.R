library(shiny)
library(bs4Dash)

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    controlbar_overlay = TRUE,
    title = "PUM",
    
    ## NAVBAR ----------------------------------------------------------------------
    
    navbar = bs4DashNavbar(
      skin = "dark",
      status = "gray-light",
      border = TRUE,
      sidebarIcon = "bars",
      controlbarIcon = "th",
      fixed = FALSE
    ),
    
    ## SIDEBAR ----------------------------------------------------------------------
    
    sidebar = bs4DashSidebar(
      skin = "dark",
      status = "warning",
      title = "PUM",
      brandColor = "warning",
      url = "https://github.com/pcastellanoescuder/PUM",
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg/1024px-Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarMenuItem(
          "Florence Nightingale",
          tabName = "florence",
          icon = "user-nurse"
        ),
        bs4SidebarMenuItem(
          "The Data",
          tabName = "data",
          icon = "database"
        ),
        bs4SidebarMenuItem(
          "Visualization",
          tabName = "viz",
          icon = "chart-bar"
        ),
        bs4SidebarMenuItem(
          "Analysis",
          tabName = "analysis",
          icon = "sliders"
        ),
        bs4SidebarMenuItem(
          "About Us",
          tabName = "about",
          icon = "id-card"
        )
      )
    ),
    
    ## CONTROLBAR ----------------------------------------------------------------------
    
    # controlbar = bs4DashControlbar(),
    
    ## FOOTER ----------------------------------------------------------------------
    
    footer = bs4DashFooter(
      copyrights = a(
        href = "https://twitter.com/martabofillr",
        target = "_blank", "@martabofillr"
      ),
      a(
        href = "https://twitter.com/polcastellano_",
        target = "_blank", "@polcastellano_"
      ),
      a(
        href = "https://twitter.com/G_Villacampa",
        target = "_blank", "@G_Villacampa"
      ),
      right_text = "2020, MIT License"
    ),
    
    ## BODY ----------------------------------------------------------------------
    
    body = bs4DashBody(
      bs4TabItems(
        
        ## CONTEXT ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "florence",
          fluidRow(
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "florence_card",
                     title = "About Florence",
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/florence.md")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "crimean_card",
                     title = "Crimean War",
                     status = "danger",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/crimean_war.md")
                   )
            ),
            
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "contributions_card",
                     title = "Contributions",
                     status = "success",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/contributions.md")
                   )
            )
          )
        ),
        
        ## DATA ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "data",
          fluidRow(
            bs4Dash::bs4Card(
              width = 12,
              inputId = "data_card",
              title = "Florence Nightingale Data",
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              DT::dataTableOutput("florence_data")
            )
          )
        ),
        
        ## VIZ ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "viz",
          
          ## FLORENCE PLOTS -----------------------------------------------------
          
          fluidRow(
            bs4Dash::bs4Card(
              width = 12,
              inputId = "florence_plots_card",
              title = "Florence Nightingale Plots",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              plotOutput("florence_plots")
            ) # here
          )
        )
        
      ) # bs4TabItems
    ) # bs4DashBody
  ),
  
  server = function(input, output, session) {
    
    ## DATA ----------------------------------------------------------------------
    
    output$florence_data <- DT::renderDataTable({
      
      data <- readxl::read_xlsx("data/datos_florence.xlsx") %>%
        janitor::row_to_names(row_number = 1) %>%
        janitor::clean_names() %>%
        rename_at(vars(zymotic_diseases:all_other_causes), ~ paste0(., "_deaths")) %>%
        rename_at(vars(ends_with("_2")), ~ paste0(., "_MR1000")) %>%
        rename_at(vars(ends_with("_MR1000")), ~ stringr::str_remove(., "_2")) %>%
        mutate_at(vars(average_size_of_army:all_other_causes_MR1000), as.numeric) %>%
        mutate(month = stringr::str_replace(month, "_", " ")) %>%
        separate(month, into = c("month", "year"), sep = " ")
        
      DT::datatable(
        data,
        class = 'cell-border stripe',
        rownames = FALSE, 
        filter = 'top',
        options = list(
          scrollX = TRUE,
          pageLength = 25
        )
      )
      
      # save(data, file = "data/proc_data.RData")
      
    })
    
    ## VIZ - FLORENCE PLOTS ----------------------------------------------------------------------
    
    output$florence_plots <- renderPlot({
      
      source("florence_plots.R")
      p
    })
    
  }
  
)

