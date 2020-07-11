
source("helpers.R")

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    controlbar_overlay = TRUE,
    title = "pum!",
    
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
      title = "pum!",
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
          "Inference",
          tabName = "inference",
          icon = "not-equal"
        ),
        bs4SidebarMenuItem(
          "Prediction",
          tabName = "prediction",
          icon = "chart-line"
        ),
        bs4SidebarMenuItem(
          "About Us",
          tabName = "about",
          icon = "id-card"
        ),
        bs4SidebarMenuItem(
          "R-Ladies Contest",
          tabName = "contest",
          icon = "thumbs-up"
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
              inputId = "datainfo_card",
              title = "Data Information",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              includeMarkdown("mds/dataset.md")
            ),
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
          
          fluidRow(
            
            ## FLORENCE PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "florence_plots_card",
              title = "Florence Nightingale Plots",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              plotOutput("florence_plots")
            ),
            
            ## CUMULATIVE PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "cum_plots_card",
              title = "Cumulative Causes of Death",
              status = "danger",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val", "Number of deaths", fill = TRUE, status = "danger"),
              plotOutput("cum_plots")
            ),
            
            ## CORRELATION PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "cor_plots_card",
              title = "Correlations",
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val2", "Number of deaths", fill = TRUE, status = "warning"),
              plotOutput("cor_plot")
            ),
            
            ## BARPLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "bar_plots_card",
              title = "Barplots",
              status = "success",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val3", "Number of deaths", fill = TRUE, status = "success"),
              plotOutput("bar_plot")
            ) # here
          )
        ),
        
        ## ABOUT ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "about",
          fluidRow(
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "marta_card",
                     title = "Marta Bofill Roig",
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/marta.md")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "pol_card",
                     title = "Pol Castellano Escuder",
                     status = "danger",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/pol.md")
                   )
            ),
            
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "guille_card",
                     title = "Guillermo Villacampa Javierre",
                     status = "success",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/guille.md")
                   )
            )
          ),
          fluidRow(
            includeMarkdown("mds/pum_picture.md")
          )
        ),
        
        ## CONTEST ---------------------------------------------------------
        
        bs4TabItem(
          tabName = "contest",
          fluidRow(
            bs4Dash::bs4Card(
              width = 12,
              inputId = "contest_card",
              title = "R-Ladies!",
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/contest.md")
            )
          )
        )
        
      ) # bs4TabItems
    ) # bs4DashBody
  ),
  
  server = function(input, output, session) {
    
    ## DATA ----------------------------------------------------------------------
    
    output$florence_data <- DT::renderDataTable({
      
      # data <- readxl::read_xlsx("data/datos_florence.xlsx") %>%
      #   janitor::row_to_names(row_number = 1) %>%
      #   janitor::clean_names() %>%
      #   rename_at(vars(zymotic_diseases:all_other_causes), ~ paste0(., "_deaths")) %>%
      #   rename_at(vars(ends_with("_2")), ~ paste0(., "_MR1000")) %>%
      #   rename_at(vars(ends_with("_MR1000")), ~ stringr::str_remove(., "_2")) %>%
      #   mutate_at(vars(average_size_of_army:all_other_causes_MR1000), as.numeric) %>%
      #   mutate(month = stringr::str_replace(month, "_", " ")) %>%
      #   separate(month, into = c("month", "year"), sep = " ")
      
      data("Nightingale")
      
      DT::datatable(
        Nightingale,
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
    
    ## VIZ - CUMULATIVE PLOTS ----------------------------------------------------------------------
    
    output$cum_plots <- renderPlot({
      
      if (input$val){
        aux <- Nightingale %>%
          select(Date, Month, Year, Wounds, Disease, Other)
        ylab_name <- "Cumulative number of deaths"
        
        } else {
          aux <- Nightingale %>%
            select(Date, Month, Year, Wounds.rate, Disease.rate, Other.rate)
        ylab_name <- "Cumulative mortality rate per 1000"  
      }
      
      aux <- aux %>%
        pivot_longer(cols = -c(Date, Month, Year)) %>%
        mutate(date = paste0(Month, " ", Year))
      
      ggplot(aux, aes(x = Date, y = value, fill = name)) +
        geom_area() +
        ylab(ylab_name) +
        xlab("") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis(discrete = TRUE)

    })
    
    ## VIZ - CORRELATION PLOTS ----------------------------------------------------------------------
    
    output$cor_plot <- renderPlot({
      
      if (input$val2){
        aux <- Nightingale %>%
          select(Wounds, Disease, Other)
        
      } else {
        aux <- Nightingale %>%
          select(Wounds.rate, Disease.rate, Other.rate)
      }

      ggpairs(aux) +
        theme_bw()

    })
    
    ## VIZ - BARPLOTS ----------------------------------------------------------------------
    
    output$bar_plot <- renderPlot({
      
      if (input$val3){
        aux <- Nightingale %>%
          mutate(date = paste0(Month, " ", Year)) %>%
          select(Date, date, Wounds, Disease, Other)
        ylab_name <- "Number of deaths"
        
      } else {
        aux <- Nightingale %>%
          mutate(date = paste0(Month, " ", Year)) %>%
          select(Date, date, Wounds.rate, Disease.rate, Other.rate) 
        ylab_name <- "Mortality rate per 1000"  
      }
      
      aux <- aux %>%
        pivot_longer(cols = -c(Date, date)) %>%
        arrange(sort(date))
      
      ggplot(aux, aes(reorder(date, Date), value, fill = name)) +
        geom_col() +
        theme_bw() +
        xlab("") +
        ylab(ylab_name) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis(discrete = TRUE)

    })
    
  }
  
)

