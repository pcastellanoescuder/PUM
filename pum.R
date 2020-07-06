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
    
    navbar = bs4DashNavbar(),
    
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
          "Contact",
          tabName = "contact",
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
      right_text = "2020"
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
                     inputId = "history_card",
                     title = "History",
                     status = "danger",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE
                     # DT::dataTableOutput("overview_annotation")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "florence_card",
                     title = "About Florence",
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE
                     # DT::dataTableOutput("overview_annotation")
                   )
            ),
            
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "x_card",
                     title = "X",
                     status = "success",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE
                     # DT::dataTableOutput("overview_annotation")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "y_card",
                     title = "Y",
                     status = "light",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE
                     # DT::dataTableOutput("overview_annotation")
                   )
            )
          )
        ),
        
        ## DATA ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "data",
          bs4Card(
            title = "Card with messages",
            width = 9
            
          )
        )
        
        ## VIZ ----------------------------------------------------------------------
        
      ) # bs4TabItems
    ) # bs4DashBody
  ),
  
  server = function(input, output) {}
  
)

