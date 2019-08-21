library(shiny)
library(plotly)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(width = 2,
      fluidRow(
        
        wellPanel(
          tags$h4("3D Scatter Plots", style = "color:green"), br(),
          fileInput("input_file_3d", label = "Select a file", multiple = FALSE),
          numericInput("raw_counts_prefilter_3d", label = "filter raw counts less than:", value = 0),
          actionButton("plot_3d", label = "3D Plot"),
          downloadButton("maplot_3d_html", label = "3D Html download"),
          downloadButton("maplot_3d_data", label = "Download Data")
        ),
        
        wellPanel(
          # tags$head(
          #   tags$style(HTML('#input_file1{label:orange}', '#maplot{color:blue}'))
          # ),
          tags$h4("MA Plot", style = "color:blue"),
          fileInput("input_file1", label = "Select 1st file", multiple = FALSE),
          fileInput("input_file2", label = "Select 2nd file", multiple = FALSE),
          checkboxInput("ma_scale_check", label = "Select it for log10 scale", value = FALSE),
          actionButton("maplot", label = "MA Plot"),
          actionButton("maplot_no_heatmap", label = "MA Plot without heatmap"),
          actionButton("maplot_normal", label = "MA Plot normalized data"), # distinguished from the maplot_norm in the MA Plot Norm app
          downloadButton("ma_plot_data_download", label = "Download Data")
          
        ),
        
        wellPanel(
          tags$h4("MA Plot Norm", style = "color:green"), br(),
          fileInput("input_file1_norm", label = "Select 1st Anc library file", multiple = FALSE),
          fileInput("input_file2_norm", label = "Select 2nd Anc library file", multiple = FALSE),
          fileInput("input_control1_norm", label = "Select 1st Control file", multiple = FALSE),
          fileInput("input_control2_norm", label = "Select 2nd Control file", multiple = FALSE),
          checkboxInput("ma_scale_check_norm", label = "Select it for x-axis log10 scale", value = FALSE),
          actionButton("maplot_norm", label = "MA Plot"),
          downloadButton("ma_plot_data_download_norm", label = "Download Data")
          
        ),
        
        
        wellPanel(
          tags$h4("MA Plot Norm V2", style = "color:green"), br(),
          fileInput("input_file1_norm_v2", label = "Select 1st Anc library file", multiple = FALSE),
          fileInput("input_file2_norm_v2", label = "Select 2nd Anc library file", multiple = FALSE),
          fileInput("input_control1_norm_v2", label = "Select 1st Control file", multiple = FALSE),
          fileInput("input_control2_norm_v2", label = "Select 2nd Control file", multiple = FALSE), 
          numericInput("maplot_norm_raw_prefilter_v2", label = "Filter raw counts below:", value = 0),
          uiOutput("maplot_cv_filter_v2"),
          uiOutput("maplot_cv_filter_checkbox_v2"),
          checkboxInput("ma_scale_check_norm_v2", label = "Select it for x-axis log10 scale", value = FALSE),
          checkboxInput("control_check_norm_v2", label = "With control?", value = FALSE),
          actionButton("maplot_norm_v2", label = "MA Plot"),
          downloadButton("ma_plot_data_download_norm_v2", label = "Download Data")
          
        ),
        
        wellPanel(
          tags$head(
            tags$style(HTML("hr {border-top: 1px solid #000000; border-color: blue}"))
          ),
          tags$h4("MA Multi-plots", style = "color:blue"),
          selectInput("num_of_lib_input", label = "Select the number of libraries that you want to plot",
                      list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", 
                           "6" = "6", "7" = "7", "8" = "8", "9" = "9")),
          uiOutput("multi_ma_plots_input"),
          fileInput("library_control", label = "Select a control file"),
          hr(),
          uiOutput("multi_ma_plots_input_pool"),
          fileInput("virus_pool_control", label = "Select virus pool control file"),
          actionButton("multi_maplot", label = "Multiple MA Plot"),
          actionButton("multi_maplot_log", label = "Multi-MAplots log scale"),
          downloadButton("multi_ma_plot_data_download", label = "Download Data"),
          downloadButton("multi_ma_plot_img_download", label = "Download Hi-res IMG"),
          downloadButton("multi_ma_plot_html_download", label = "Download html")
        ),

        wellPanel(
          tags$h4("Generateing binaray percentage table", style = "color:blue"), br(),
          fileInput("input_file_binary", label = "Select one file", multiple = FALSE),
          actionButton("generate_percent", label = "Table Generator")
        ),
        wellPanel(
          tags$head(
            tags$style(HTML('#h4{color:blue}'))
          ),
          div(fileInput("input_files", label = "Select multiple files: Applies for Scatter Plot, Histogram & Data Merging",
                    multiple = TRUE), style = "color:blue"),
          actionButton("scatterplot", label = "Scatter Plot"),
          actionButton("histogram", label = "Histogram"),
          textInput("hist_low", label = "histogram lower bound", width = 200),
          textInput("hist_high", label = "histogram higher bound", width = 200),
          actionButton("select_merge_range", label = "merge"),
          downloadButton("save_merged_data", label = "save"),
          
          hr(), tags$h4("CSV FILES MERGE APP", style = "color:green"), br(),
          textInput("mean_column_name", label = "Enter the mean column name", width = 300),
          textInput("sd_column_name", label = "enter the SD column name", width = 300),
          actionButton("column_merge", label = "Column Merge"),
          actionButton("row_merge", label = "Row Merge"),
          actionButton("merge_normalize", label = "Merge&Normalize"),
          actionButton("multi_lib_normalize", label = "Multi Library Normalization"),
          downloadButton("csv_merge_download", label = "save"),
          # dynamic download button for multiple libraries normalization files
          uiOutput("multi_lib_normalization_download"),
          checkboxInput("csv_file_option", label = "Check the box to save an csv with all combined the columns. Otherwise, save 
                        the csv with only mean values", value = FALSE)
          
        ),
        wellPanel(
          tags$h4("Gating APP", style = "color:green"), br(),
          selectInput("num_of_gating_input", label = "Choose the number of groups that you want to compare",
                      list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")),
          uiOutput("gating_group_inputs"),
          actionButton("gating", label = "Gating")
        ),
        wellPanel(
          tags$h4("Gating APP V2", style = "color:green"), br(),
          selectInput("num_of_gating_first_input_v2", label = "Choose the number of files in first input",
                      list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")),
          uiOutput("gating_group_first_inputs_v2"),
          selectInput("num_of_gating_second_input_v2", label = "Choose the number of files in second input",
                      list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")),
          uiOutput("gating_group_second_inputs_v2"),
          actionButton("gating_v2", label = "Gating")
        ),
        wellPanel(
          tags$h4("OGI APP", style = "color:green"), br(),
          fileInput("ogi_input_file1", label = "Select 1st file", multiple = FALSE),
          fileInput("ogi_input_file2", label = "Select 2nd file", multiple = FALSE),
          actionButton("ogi_maplot", label = "MA Plot")
        )
        
     )
      
    ),
    
    
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      tabsetPanel(
        tabPanel("MA Plot", plotlyOutput("maplot_out", width = 1100, height = 800),
                            plotlyOutput("bin_heatmap_out", width = 1100, height = 800),
                            uiOutput("ma_sd_slider")),
        
        tabPanel("MA Plot Norm", plotlyOutput("maplot_out_norm", width = 1100, height = 800),
                 plotlyOutput("bin_heatmap_out_norm", width = 1100, height = 800)),
        
        tabPanel("MA Plot Norm V2", plotlyOutput("maplot_out_norm_v2", width = 1100, height = 800), br(),
                                 uiOutput("ma_norm_radio_buttons_v2"), 
                                 uiOutput("maplot_norm_heatmap_button_v2"),
                                 uiOutput("maplot_norm_corr_button_v2"),
                                 plotlyOutput("bin_heatmap_out_norm_v2", width = 1100, height = 800),
                                 plotlyOutput("ma_norm_v2_corr", width = 1100, height = 800)),
        
        tabPanel("MA Multi-Plots", plotlyOutput("multi_maplot_out", width = 1200, height = 1000)),
                            
        tabPanel("Scatter Plot", plotlyOutput("scatterplot_out", width = 1100, height = 800)),
        tabPanel("Histogram", plotlyOutput("histogram_out"),
                              dataTableOutput("select_merged_out")),
        
        tabPanel("Gating App", fluidRow(column(6, plotlyOutput("gating_in1", width = 800, height = 600)), 
                                        column(6, plotlyOutput("gating_out1", width = 800, height = 600))),
                               fluidRow(column(6, plotlyOutput("gating_in2", width = 800, height = 600)),
                                        column(6, plotlyOutput("gating_out2", width = 800, height = 600))),
                               fluidRow(column(6, plotlyOutput("gating_in3", width = 800, height = 600)),
                                        column(6, plotlyOutput("gating_out3", width = 800, height = 600))),
                               fluidRow(column(6, plotlyOutput("gating_in4", width = 800, height = 600)),
                                        column(6, plotlyOutput("gating_out4", width = 800, height = 600))),
                               fluidRow(column(6, plotlyOutput("gating_in5", width = 800, height = 600)),
                                        column(6, plotlyOutput("gating_out5", width = 800, height = 600)))
                 ),
        tabPanel("Gating App V2", uiOutput("gating_out_v2")
                 ),
        tabPanel("Stats", dataTableOutput("binary_table_out"),
                          uiOutput("generate_percent_data_button")),
          # downloadButton("generate_percent_data_download", label = "Download Binary Table")
        tabPanel("Data Merging", dataTableOutput("merged_data_view")),
        tabPanel("Experiment Form", uiOutput("exp_form")),
        tabPanel("3D Plots", fluidRow(column(width = 3, uiOutput("raw_counts_filter_ma_plot_3d")), 
                                      column(width = 3, uiOutput("sd_filter_ma_plot_3d"))),
                             fluidRow(column(width = 3, uiOutput("x_filter_ma_plot_3d")),
                                      column(width = 3, uiOutput("y_filter_ma_plot_3d")),
                                      column(width = 3, uiOutput("z_filter_ma_plot_3d"))), 
                             uiOutput("ma_plot_gating_3d_actionButton"),
                             uiOutput("ma_plot_3d_search"),
                             uiOutput("radio_buttons_3d"),
                             plotlyOutput("plot_out_3d", width = 1600, height = 1200))
      )
    )
  )
))