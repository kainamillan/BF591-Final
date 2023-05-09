#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

' cerulean, cosmo, cyborg, 
darkly, flatly, journal, 
lumen, paper, readable, 
sandstone, simplex, slate, 
spacelab, superhero, united, yeti'

library(shiny)
library(ggplot2)
library(gplots)
library(dplyr)
library(tidyr)
library(DT)
library(shinythemes)
library(colourpicker)
library(patchwork)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme('flatly'),
                
                # Application title
                titlePanel("BF591 Final Project"),
                p('Kaina Millan'),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel('Samples', 'This tab uses the samples.csv file',
                             tabsetPanel(
                               tabPanel('Summary', 'Means were calculated for numeric/integer data types', 
                                        fileInput('file1', 
                                                  'Please download the samples.csv file', 
                                                  accept = '.csv'
                                        ),
                                        dataTableOutput("sumtable")
                               ), 
                               tabPanel('Sample Info', dataTableOutput("sampletable")
                               ), 
                               tabPanel('Histogram', selectInput('hist_drop', 
                                                                 'Select x-axis for histogram', 
                                                                 choices = NULL),
                                        plotOutput("histogram")
                               )
                             )
                    ), 
                    tabPanel('DE', 
                             tabsetPanel(
                               tabPanel('DE Table',
                                        mainPanel(
                                          fileInput('file2', 
                                                    'Please download the de.csv file', 
                                                    accept = '.csv'
                                          ),
                                          dataTableOutput(outputId = 'detable')
                                        )
                                        
                               ), 
                               tabPanel('Plot', 
                                        sidebarLayout(
                                          sidebarPanel(
                                            radioButtons('x_name', 'Choose an x-axis variable',
                                                         choices = c(
                                                           'baseMean', 
                                                           'HD.mean', 
                                                           'Control.mean', 
                                                           'log2FoldChange', 
                                                           'lfcSE', 
                                                           'stat', 
                                                           'pvalue', 
                                                           'padj'
                                                         ), selected = 'stat'),
                                            
                                            radioButtons('y_name', 'Choose a y-axis variable',
                                                         choices = c(
                                                           'baseMean', 
                                                           'HD.mean', 
                                                           'Control.mean', 
                                                           'log2FoldChange', 
                                                           'lfcSE', 
                                                           'stat', 
                                                           'pvalue', 
                                                           'padj'
                                                         ), selected = 'padj'),
                                            
                                            colourInput('color1', 'Base point color', value = 'aquamarine3'),
                                            colourInput('color2', 'Highlight point color', value = 'navy'),
                                            sliderInput('slider', 'Select the magnitude of the p-adjusted coloring:',
                                                        min = -32,
                                                        max = 0,
                                                        value = -16),
                                            actionButton('Submit', 'Create Volcano')
                                          ),
                                          mainPanel(plotOutput(outputId = 'volcplot')))
                               )) 
                             
                    ), 
                    tabPanel('Counts', 
                             mainPanel(
                               fileInput('file3','Please download the counts.csv file', accept= '.csv'),
                               
                               sliderInput('slider4', 'Filter by percent variance',
                                           min = 0,
                                           max = 36,
                                           value = 0),
                               sliderInput('slider5', 'Filter by number of zeros per sample',
                                           min = 0,
                                           max = 60,
                                           value = 0),
                               tabsetPanel(
                                 tabPanel('Table', 
                                          mainPanel(tableOutput(outputId = 'countstable'))),
                                 tabPanel('Scatter', 
                                          p('This page takes 15 seconds to load'),
                                          plotOutput(outputId = 'countsplot'), 
                                          plotOutput(outputId = 'varplot')),
                                 tabPanel('Heatmap', 
                                          p('This page takes 15 seconds to load'),
                                          plotOutput(outputId = 'heatmap')),
                                 tabPanel('PCA Scatter', 
                                          selectInput('x_pc', 
                                                      'Select x-axis', 
                                                      choices = paste0('PC', 1:10)),
                                          selectInput('y_pc', 
                                                      'Select y-axis', 
                                                      choices = paste0('PC', 1:10)),
                                          mainPanel(plotOutput(outputId = 'pcaplot')))
                             )
                    )
                    ),
                    tabPanel('GSEA', 
                             tabsetPanel(
                               tabPanel('NES Barplot', 
                                        # Sidebar with a slider to filter by padj 
                                        sidebarLayout(
                                          sidebarPanel(
                                            fileInput('file4', 
                                                      'Please download the fgsea.csv file', 
                                                      accept = '.csv'
                                            ),
                                            sliderInput("slider1",
                                                        "Adjusted p-value:",
                                                        min = -22,
                                                        max = 0,
                                                        value = 0)
                                          ),
                                          mainPanel(plotOutput('barplot'))
                                        )
                               ), 
                               tabPanel('Table', 
                                        # Sidebar with a slider to filter by padj 
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("slider2",
                                                        "Adjusted p-value:",
                                                        min = -22,
                                                        max = 0,
                                                        value = 11), 
                                            radioButtons('radios', 
                                                         'Select to display positive or negative NES pathways',
                                                         choices = c(
                                                           'Positive', 
                                                           'Negative', 
                                                           'All'
                                                         ), 
                                                         selected = 'All')
                                          ),
                                          mainPanel(dataTableOutput("NEStable"))
                                        )
                               ),
                               tabPanel('Scatterplot', 
                                        # Sidebar with a slider to filter by padj 
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("slider3",
                                                        "Adjusted p-value:",
                                                        min = -22,
                                                        max = 0,
                                                        value = 11)
                                          ),
                                          mainPanel(plotOutput('scatterplot'))
                                          )
                                        )
                               )
                             )
                  )
                )
                )




#################################################################

# Define server logic required to draw a histogram and create data tables for 
##sample and summary information

server <- function(session, input, output) {
  
  #increasing max 
  options(shiny.maxRequestSize = 100*1024^2)
  
  
  #reactively loading data1
  load_data1 <- reactive({
    
    my_file <- input$file1
    if(is.null(my_file))
      return(NULL)
    
    dataf <- read.csv(my_file$datapath, 
                      sep = ',', 
                      header = TRUE)
    
    return(dataf)
  })
  
  #reactively loading data2
  load_data2 <- reactive({
    
    my_file <- input$file2
    if(is.null(my_file))
      return(NULL)
    
    dataf <- read.csv(my_file$datapath, 
                      sep = ',', 
                      header = TRUE)
    
    return(dataf)
  })
  
  #reactively loading data3
  load_data3 <- reactive({
    
    my_file <- input$file3
    if(is.null(my_file))
      return(NULL)
    
    dataf <- read.csv(my_file$datapath, 
                      sep = ',', 
                      header = TRUE)
    
    return(dataf)
  })
  
  #reactively loading data4
  load_data4 <- reactive({
    
    my_file <- input$file4
    if(is.null(my_file))
      return(NULL)
    
    dataf <- read.csv(my_file$datapath, 
                      sep = ',', 
                      header = TRUE)
    
    return(dataf)
  })
  
  #this function updates histogram dropdown menu options once file is uploaded
  observe({
    #input file required
    req(input$file1)
    
    #selecting choices for drop down menu... only numeric/integer data
    choices1 = load_data1() %>% select_if(~!any(is.character(.))) %>% names()
    
    #update dropwdown menu options upon file upload
    updateSelectInput(session, 'hist_drop', choices = choices1)
  })
  
  
  #function to create summary table 
  create_summary <- function(dataf){
    
    #loading data as df
    df <-load_data1() %>% data_frame()
    
    #calculating means/ total number of samples and storing as variables
    rows <- '-'
    death <- mean(df$age_of_death)
    bases <- mean(df$Bases)
    exp <- '-'
    mrna <- mean(df$mrna.seq_reads)
    pmi <- mean(df$pmi, na.rm = TRUE)
    rin <- mean(df$RIN)
    samps <- '-'
    age <- mean(df$age_of_onset, na.rm = TRUE)
    cag <- mean(df$cag, na.rm = TRUE)
    dur <- mean(df$Duration, na.rm = TRUE)
    
    #saving calculated values into variable to create distinct values column
    distinct <- c(rows, death, bases, exp, mrna, pmi, rin, samps, age, cag, dur)
    
    #storing column names
    colm_names <- colnames(df)
    
    #storing data types
    types <- sapply(df, class)
    
    #indicating number of decimals 
    options(digits = 5)
    
    #creating data frame 
    sum_df <- data.frame(Column_Names = colm_names, Type = types, Mean = distinct, row.names = NULL)
    
    DT_sum_df <- datatable(sum_df, 
                           class = 'cell-border stripe', 
                           options= list(paging = FALSE))
    
    return(DT_sum_df)
    
  }
  
  #function to create table with sample information
  create_sample <- function(dataf){
    
    #loading data as df and renaming first column
    df <-load_data1() %>% data_frame()
    
    DT_df<- datatable(df, 
                      class = 'cell-border stripe')
    
    return(DT_df)
    
  }
  
  #function to create histogram of continuous variables
  create_hist <- function(dtaf){
    
    #loading data as df and renaming first column
    df <-load_data1() %>% data_frame()
    
    #creating histogram with ggplot
    histo_gram <- ggplot(df, aes_string(x= input$hist_drop)) +
      geom_histogram(color= 'black', fill = 'aquamarine3') + 
      xlab(toString(input$hist_drop)) +
      ggtitle(paste('Counts for ', input$hist_drop))
    
    return(histo_gram)
  }
  
  ###########SECOND SECTION#############
  
  #drawing and filtering data table based on slider input
  create_detable <- function(dataf, slider) {
    
    my_tbl <- load_data2() %>% 
      data_frame()
    
    DT_tbl <- datatable(my_tbl, 
                        class = 'cell_stripe border')
    
    return(DT_tbl)
  }
  
  #creating dataframe for reactive volcano plot
  volc_data <- reactive({
    
    volc_df <- data_frame(x = load_data2()[[input$x_name]], 
                          y = load_data2()[[input$y_name]])
    
    volc_df$color <- ifelse(-log10(volc_df$y) < 10^input$slider, 'aquamarine3', 'navy')
    
    return(volc_df)
    
  })

  
  #creating volcano plot
  create_volcano <- function(volc_df){
    
    volcano <- ggplot(volc_data(), 
                      aes(
                        x= x, 
                        y = -log10(y), 
                        color = color)) +
      geom_point() + 
      scale_color_manual(values = c(input$color1, input$color2)) +
      labs(title = paste('Volcano plot of ', input$y_name, 'vs. ', input$x_names),
           x= input$x_name, 
           y = input$y_name)
    
    return(volcano)
    
    
  }
  
  
  ################THIRD SECTION#######################
  #Adding columns to original df 
  create_counts <- reactive({
    
    #loading data as df and renaming first column
    c_tbl <-load_data3() %>% data_frame()
    
    #counting number of 0s per row (how many samples have zeros per gene)
    c_tbl$zeros <- rowSums(c_tbl == 0)
    
    #determining variance per gene and variance
    c_tbl$variance <- apply(c_tbl[2:70], 1, var)
    
    #calculating percent variance per gene
    c_tbl$percent_var <- (c_tbl$variance/sum(c_tbl$variance)*100)
    
    #calculating median count per row 
    c_tbl$median_c <- apply(c_tbl, 1 , median)
    
    return(c_tbl)
  })
  
  #filtering original df with sliders 
  filter_counts <- reactive({
    
    #table filtered by number of zeros and variance
    z_tbl <- filter(create_counts(), zeros > input$slider5 | percent_var < input$slider4)
    
    return(z_tbl) 
  })
  
  #creating a table providing summary information after filtering 
  info_sum <- function(z_tbl){
    #determining total num of genes, samples
    samples <- create_counts()[, -c(1, 71)] %>% ncol()
    genes <- nrow(create_counts())
    
    #determining number of genes passing filter and percent out of total genes
    pass_genes <- nrow(filter_counts())
    pass_genes_per <- ((pass_genes/genes)*100)
    #determining number of genes not passing filter and percent out of total genes
    dec_genes <- (genes - pass_genes)
    dec_genes_per <- ((dec_genes/genes)*100)
    
    #creating dataframe with calculated values 
    info <- data_frame(Total_Samples = samples, 
                       Total_genes = genes, 
                       Genes_Pass_Filter = pass_genes, 
                       Percent_Pass = pass_genes_per,
                       Genes_Not_Pass = dec_genes, 
                       Percent_Not_Pass = dec_genes_per)
    
    
    return(info)
    
  }
  
  #plotting mean count vs. log10(variance) as scatter plot
  create_varplot <- function(c_tbl){

    #determining which genes passed filter 
    countsplot_data <- create_counts() %>% 
      mutate(passed_filter = X %in% filter_counts()$X, 'Passed', 'Not Passed')
    
    var_plot <- countsplot_data %>%
      ggplot(aes(x= median_c, 
                 y = log10(variance), 
                 color = passed_filter)) +
      geom_point()
    
    return(var_plot)
    
  }
  
  #plotting mean count vs. number of zeros as scatter plot 
  create_zeroplot <- function(info){
   
    #determining which genes passed filter
    zeroplot_data <- create_counts() %>% 
      mutate(passed_filter = X %in% filter_counts()$X, 'Passed', 'Not Passed')
    
    zero_plot <- zeroplot_data %>%
      ggplot(aes(x= median_c, 
                 y = zeros, 
                 color = passed_filter)) +
      geom_point() 
    
    
    return(zero_plot)
    
  }
  
  #function that creates heatmap from filtered table 
  create_heatmap <- function(z_tbl) {
    
    h_data <- as.matrix(filter_counts()[2:70]) 
    
    heat <- heatmap.2(h_data, 
                      scale = 'row', 
                      key = TRUE, 
                      cexCol = 0.8,
                      cexRow = 0.8)
    
    return(heat)
    
  }
  
  
  
  create_pca <- function(input) {
    #transpose expression values so samples are rows
    expr_mat <- create_counts()[, 1:70] %>%
      pivot_longer(-c(X), names_to = "sample") %>%
      pivot_wider(names_from = X)
    
    #mean-centered, convert to df
    center_expr_mat <- as.data.frame(
      lapply(select(expr_mat, -c(sample)), function(x) x-mean(x)))
    
    rownames(center_expr_mat) <- expr_mat$sample
    
    #prcomps performs PCA
    pca <- prcomp(
      center_expr_mat, 
      center = FALSE, 
      scale = TRUE)
      
    #table with PC, variance, %variance, cum. % variance
    pca_var <- tibble(
      PC=factor(str_c("PC",1:69),str_c("PC",1:69)),
      Variance=pca$sdev**2,
      `% Explained Variance`=Variance/sum(Variance)*100,
      `Cumulative % Explained Variance`=cumsum(`% Explained Variance`))
    
    #plotting PCA projections 
    pca_data <- as_tibble(pca$x) %>%
      mutate(type=stringr::str_sub(rownames(pca$x),1,1)) 
    
    pca_proj <- ggplot(pca_data, aes(x= input$x_pc, y= input$y_pc, color=type)) +
      geom_point() +
      ggtitle('PCA Projection Plot') +
      xlab(paste0('PC'), input$x_pc) +
      ylab(paste0('PC'), input$y_pc)
    
    return(pca_proj)
    
  }
  
  #############FOURTH SECTION######################
  #creating dataframe for reactive barplot
  bar_data <- reactive({
    
    bars <- load_data4() %>% data.frame()
    
    fltr_data <- filter(bars, padj < 10^input$slider1)
    
    return(fltr_data)
    
  })
  
  
  ###function to create bar plot of fgsea NES for top pathways 
  ###by adjusted p-value
  
  create_barplot <- function(fltr_data){
    
    my_bars <- ggplot(bar_data(), aes(reorder(pathway, NES), NES)) +
      geom_col(aes(fill=padj<0.05)) +
      scale_fill_manual(values = c('aquamarine3', 'navy')) +
      coord_flip() +
      labs(x="Pathway", y="Normalized Enrichment Score (NES)",
           title="HALLMARK pathways NES from GSEA") + 
      theme(axis.text.y = element_text(size = 6.5))
    
    return(my_bars)
    
  }
  
  
  #function to create filterable NES datable 
  draw_table <- reactive({
    
    my_tbl <- load_data4() %>% data.frame()
    
    
    fltr_tbl <- filter(my_tbl, padj < 10^input$slider2) %>% DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        class = 'cell-border stripe',
        buttons = c('csv', 'pdf')))
    
    
    if (input$radios == "All"){
      fltr_tbl <- fltr_tbl
    }
    
    else{
      if(input$radios == "Positive"){
        fltr_tbl <- filter(my_tbl, NES > 0) %>% DT::datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            class = 'cell-border stripe',
            buttons = c('csv', 'pdf')))
      }
      
      else{
        fltr_tbl <- filter(my_tbl, NES < 0) %>% DT::datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            class = 'cell-border stripe',
            buttons = c('csv', 'pdf')))
      }
    }
    return(fltr_tbl)
  })
  
  
  #function to plot scatter plot based on filtered dataframe
  create_scatter <- reactive({
    
    scatter_data <- load_data4() %>% data.frame()
    
    fltr_scatter <- filter(scatter_data, padj < 10^input$slider3)
    
    scatter <- ggplot(fltr_scatter, aes(x= NES, y= -log10(padj), color = padj < 0.05)) +
      geom_point() + scale_color_manual(values = c('aquamarine3','navy'))
    ggtitle('Scatterplot of -log10(padj) vs. NES')
    
    return(scatter)
    
  })
  
  #############################OUTPUTS############################
  #summary table output
  output$sumtable <- renderDataTable(create_summary())
  
  #sample table output
  output$sampletable <- renderDataTable(create_sample())
  
  #histogram output
  output$histogram <- renderPlot(create_hist())
  
  #de table output
  output$detable <- renderDataTable(create_detable())
  
  #volcplot output
  output$volcplot <- renderPlot(create_volcano(), height = 400, width = 600)
  
  #counts table output
  output$countstable <- renderTable(info_sum())
  
  #counts plot output
  output$countsplot <- renderPlot(create_varplot())
  
  #varplot output
  output$varplot <- renderPlot(create_zeroplot())
  
  #heatmap output
  output$heatmap <- renderPlot(create_heatmap())
  
  #pcaplot output
  output$pcaplot <- renderPlot(create_pca())

  #barplot output
  output$barplot <- renderPlot(create_barplot(), height = 400, width = 600)
  
  #sortable table output
  output$NEStable <- renderDataTable(draw_table())
  
  #scatterplot output
  output$scatterplot <- renderPlot(create_scatter(), height = 400, width = 600)
  
}
# Run the application 
shinyApp(ui = ui, server = server)
