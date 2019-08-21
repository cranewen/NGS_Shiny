library(shiny)
library(plotly)
library(GGally)
library(ggplot2)
library(htmlwidgets)


shinyServer(function(input,output,session){
  
  df_name_list <- reactive({
    req(input$input_files)
    input_list <- input$input_files
    input_list
    }) 
  
 
  df1_file_name <- reactive({
    req(input$input_file1)
    file_obj <- input$input_file1
    file_name <- file_obj$datapath
    file_name
  })
  
  df2_file_name <- reactive({
    req(input$input_file2)
    file_obj <- input$input_file2
    file_name <- file_obj$datapath
    file_name
  })
  
  
  df_bin_file_name <- reactive({
    req(input$input_file_binary)
    file_obj <- input$input_file_binary
    file_name <- file_obj$datapath
    file_name
  })
  
  # It's the checkbox reactive obj for MA Plot to change the x-axis scale.
  ma_scale_sel <- reactive({
    ma_scale <- input$ma_scale_check
    ma_scale
  })
  
  
  #MA Plot Norm members
  
  df1_file_name_norm <- reactive({
    req(input$input_file1_norm)
    file_obj <- input$input_file1_norm
    file_name <- file_obj$datapath
    file_name
  })
  
  df2_file_name_norm <- reactive({
    req(input$input_file2_norm)
    file_obj <- input$input_file2_norm
    file_name <- file_obj$datapath
    file_name
  })
  
  control1_file_name_norm <- reactive({
    req(input$input_control1_norm)
    file_obj <- input$input_control1_norm
    file_name <- file_obj$datapath
    file_name
  })
  
  control2_file_name_norm <- reactive({
    req(input$input_control2_norm)
    file_obj <- input$input_control2_norm
    file_name <- file_obj$datapath
    file_name
  })
  
  ma_scale_sel_norm <- reactive({
    ma_scale <- input$ma_scale_check_norm
    ma_scale
  })
  
  # end of MA Plot Norm members
 
  
  #OGI APP Members#
  ogi_df1_file_name <- reactive({
    req(input$ogi_input_file1)
    file_obj <- input$ogi_input_file1
    file_name <- file_obj$datapath
    file_name
  })
  
  ogi_df2_file_name <- reactive({
    req(input$ogi_input_file2)
    file_obj <- input$ogi_input_file2
    file_name <- file_obj$datapath
    file_name
  })
  
  
  #end of - OGI APP Members#
  
  # 3D Scatter Plot members #
  
  file_name_3d_norm <- reactive({
    req(input$input_file_3d)
    file_obj <- input$input_file_3d
    file_name <- file_obj$datapath
    file_name
  })
  
  #end of - 3D Scatter Plot members #
  
  # 3D plot app #
  
  observeEvent(input$plot_3d, {
    df <- read.csv(file_name_3d_norm(), header = TRUE, sep = ",")
    if (is.null(input$raw_counts_prefilter_3d) == FALSE) {
      df <- subset(df, df$raw_counts >= input$raw_counts_prefilter_3d) 
    }
    
    output$raw_counts_filter_ma_plot_3d <- renderUI(sliderInput("raw_slider_3d", 
                                                                label = "raw counts filter", min = min(df$raw_counts), max = max(df$raw_counts), 
                                                                value = c(min(df$raw_counts), max(df$raw_counts)), step = 10))
    output$sd_filter_ma_plot_3d <- renderUI(sliderInput("sd_slider_3d", label = "SD filter", min = min(df$sd), max = max(df$sd), 
                                                        value = c(min(df$sd), max(df$sd)) ))
    
    output$x_filter_ma_plot_3d <- renderUI(sliderInput("x_slider_3d", label = "X value filter", min = min(df[[2]]), max = max(df[[2]]), 
                                                       value = c(min(df[[2]]), max(df[[2]]))))
    
    output$y_filter_ma_plot_3d <- renderUI(sliderInput("y_slider_3d", label = "Y value filter", min = min(df[[3]]), max = max(df[[3]]), 
                                                       value = c(min(df[[3]]), max(df[[3]]))))
    
    output$z_filter_ma_plot_3d <- renderUI(sliderInput("z_slider_3d", label = "Z value filter", min = min(df[[4]]), max = max(df[[4]]), 
                                                       value = c(min(df[[4]]), max(df[[4]]))))
    
    output$ma_plot_3d_search <- renderUI(selectizeInput("search_anc_3d", label = "Search", 
                                                        choices = df[[1]], selected = NULL, multiple = TRUE, options = list(create = FALSE)))
    
    output$ma_plot_gating_3d_actionButton <- renderUI(actionButton("gating_3d", label = "Gating 3D"))
    
    output$radio_buttons_3d <- renderUI(
      fluidRow(
        lapply(c(1:11), function(i){
          column(1, radioButtons(anc80_pos[i], label = anc80_pos[i], choices = c(0,1, "reset"), selected = FALSE))
        })
      )
    ) 
    
    
    output$plot_out_3d <- renderPlotly({
      #p <- plot_ly(df, x = ~x, y = ~y, z = ~z, text = df[[1]], color = ~samples)
      #p <- plot_ly(df, x = log10(df[[2]]), y = ~y, z = ~SD, text = df[[1]], color = ~ma_comparison)
      #p <- plot_ly(df, x = df[[2]], y = ~y, z = ~SD, text = df[[1]], color = ~ma_comparison)"
      
      # filter data/varibles
      df_plot <- subset(df, df$raw_counts>=input$raw_slider_3d[1] & df$raw_counts<=input$raw_slider_3d[2])
      df_plot <- subset(df_plot, df_plot$sd>input$sd_slider_3d[1] & df_plot$sd<input$sd_slider_3d[2])
      df_plot <- subset(df_plot, df_plot[[2]]>=input$x_slider_3d[1] & df_plot[[2]]<=input$x_slider_3d[2])
      df_plot <- subset(df_plot, df_plot[[3]]>=input$y_slider_3d[1] & df_plot[[3]]<=input$y_slider_3d[2])
      df_plot <- subset(df_plot, df_plot[[4]]>=input$z_slider_3d[1] & df_plot[[4]]<=input$z_slider_3d[2])
      # end of filter data varibles
      
      # Looping through all the radio buttons, if find any NOT NULL values, then re-assign df_plot, coloring by the positions start.
      selected_radios <- c()
      
      
      for (x in anc80_pos) {
        if(is.null(input[[x]]) == FALSE) {
          if (input[[x]] != "reset") {
            selected_radios <- c(selected_radios, x)
          }
        }
      }
      
      #print(input[[str(selected_radios[1])]]) #get radio button value
      # assign a string for preparing the conditions in subset() function
      
      sub_df_condition_stat_str_list <- lapply(selected_radios, function(x){
        paste(paste("df_plot$", x, sep = ""), input[[x]], sep = "==")
      })
      
      sub_df_condition_stat <- paste("subset(df_plot, ", paste(sub_df_condition_stat_str_list, collapse = " & "), ")", sep = "")
      print(sub_df_condition_stat)
      df_plot <- eval(parse(text = sub_df_condition_stat))
      #print(df_plot)
      
      
      p <- plot_ly(df_plot, x = df_plot[[2]], y = df_plot[[3]], z = df_plot[[4]], text = df_plot[[1]], color = df_plot$sd, size = 1/df_plot$sd, 
                   marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 30), type = "scatter3d") %>%
        layout(title = "Combined 2 MA Plots (Liver - Virus) & (Virus - Plasmid)",
               scene = list(xaxis = list(title = paste("X: ", colnames(df)[2])),
                            yaxis = list(title = paste("Y: ", colnames(df)[3])),
                            zaxis = list(title = paste("Z: ", colnames(df)[4])))
        )
      
      output$maplot_3d_html <- downloadHandler(filename = ".html",
                                                          content = function(file) {
                                                            htmlwidgets::saveWidget(p, file = file)
                                                          })
      
      output$maplot_3d_data <- downloadHandler(filename = ".csv", 
                                               content = function(file) {
                                                 write.csv(df_plot, file, row.names = FALSE)
                                               })
      p
    })
  })
  
  
  
  # End of 3D plot app #
  
  # MA Plot app #
  observeEvent(input$maplot, {
    df1 <- read.csv(df1_file_name(), header = TRUE, sep = ",")
    df2 <- read.csv(df2_file_name(), header = TRUE, sep = ",")
    df1[2] <- normalization(df1[2])
    df2[2] <- normalization(df2[2])
    
    # View(rbind(df1, df2))
    
    x_dat <- c()
    if(ma_scale_sel() == FALSE) {
      x_dat <- (2**df1[[2]] + 2**df2[[2]]) / 2
    }
    else {
      x_dat <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
    }
    y_dat <- df1[[2]] - df2[[2]]
    
    
    df_download <- data.frame("barcode_index" = df1[[1]], "x" = x_dat, "y" = y_dat, "index" = as.integer(row.names(df1)) - 1)
    lib_len <- library_list[[strsplit(as.character(df_download$barcode_index[1]), "BC")[[1]][1]]]
    df_bin_matrix_download <- cbind(df_download, bin_matrix(lib_len))
    output$maplot_out <- renderPlotly({
      plot_ly(type = "scatter", mode = "markers", x = x_dat, y = y_dat, text = df1[[1]], hoverinfo = text, source = "ma") %>%
        layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                    yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    })
    
    df_heatmap <- df_bin_matrix_download[order(df_bin_matrix_download$y, decreasing = TRUE),]
    df_bin_heatmap <- as.matrix(df_heatmap[, -which(names(df_heatmap) %in% c("barcode_index", "x", "y", "index"))])

    output$bin_heatmap_out <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "ma")
      selected_index <- selected_data$pointNumber

      if (is.null(selected_data) == TRUE){
        plot_ly(x = colnames(df_bin_heatmap), y = df_heatmap$barcode_index, z = df_bin_heatmap, type = "heatmap", colors = c("green", "red")) %>%
          # the heatmap was upside down, so using autorange='reversed' will filp it around
          layout(
            yaxis = list(autorange='reversed'))
      }
      else {
        # df_heatmap is a data frame, so when select range from rows, it should be df[x, ] where x is on the left of ","
        df_heatmap_sub <- df_heatmap[which(df_heatmap$index %in% selected_index), ]
        df_bin_heatmap_sub <- as.matrix(df_heatmap_sub[, -which(names(df_heatmap) %in% c("barcode_index", "x", "y", "index"))])
        plot_ly(x = colnames(df_bin_heatmap_sub), y = df_heatmap_sub$barcode_index, z = df_bin_heatmap_sub, type = "heatmap", colors = c("green", "red")) %>%
          layout(
            yaxis = list(autorange='reversed'))
      }
    })

    output$ma_plot_data_download <- downloadHandler(filename = ".csv",
                                                    content = function(file) {
                                                      write.csv(df_bin_matrix_download, file, row.names = FALSE)
                                                    })
  })

  
# MA Plot without heatmap  
  observeEvent(input$maplot_no_heatmap, {
    df1 <- read.csv(df1_file_name(), header = TRUE, sep = ",")
    df2 <- read.csv(df2_file_name(), header = TRUE, sep = ",")
    df1[2] <- normalization(df1[2])
    df2[2] <- normalization(df2[2])
    
    # View(rbind(df1, df2))
    
    x_dat <- c()
    if(ma_scale_sel() == FALSE) {
      x_dat <- (2**df1[[2]] + 2**df2[[2]]) / 2
    }
    else {
      x_dat <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
    }
    y_dat <- df1[[2]] - df2[[2]]
    
    df_download <- data.frame("barcode_index" = df1[[1]], "x" = x_dat, "y" = y_dat, "index" = as.integer(row.names(df1)) - 1)
    
    output$maplot_out <- renderPlotly({
      plot_ly(type = "scatter", mode = "markers", x = x_dat, y = y_dat, text = df1[[1]], hoverinfo = text, source = "ma") %>%
        layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                    yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    })
    

    output$ma_plot_data_download <- downloadHandler(filename = ".csv",
                                                    content = function(file) {
                                                      write.csv(df_download, file, row.names = FALSE)
                                                    })
  })
  
# MA Plot with normalized data
  observeEvent(input$maplot_normal, {
    df1 <- read.csv(df1_file_name(), header = TRUE, sep = ",")
    df2 <- read.csv(df2_file_name(), header = TRUE, sep = ",")
    
    x_dat <- c()
    if(ma_scale_sel() == FALSE) {
      x_dat <- (2**df1[[2]] + 2**df2[[2]]) / 2
    }
    else {
      x_dat <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
    }
    y_dat <- df1[[2]] - df2[[2]]
    
    df_download <- data.frame("barcode_index" = df1[[1]], "x" = x_dat, "y" = y_dat, "index" = as.integer(row.names(df1)) - 1)
    
    output$maplot_out <- renderPlotly({
      plot_ly(type = "scatter", mode = "markers", x = x_dat, y = y_dat, text = df1[[1]], hoverinfo = text, source = "ma") %>%
        layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                    yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    })
    

    output$ma_plot_data_download <- downloadHandler(filename = ".csv",
                                                    content = function(file) {
                                                      write.csv(df_download, file, row.names = FALSE)
                                                    }) 
  })
  
  # End of MA Plot # 
  
  # MA Plot normalized data (V1)
  
  observeEvent(input$maplot_norm, {
    df1 <- read.csv(df1_file_name_norm(), header = TRUE, sep = ",")
    df2 <- read.csv(df2_file_name_norm(), header = TRUE, sep = ",")
    control1 <- read.csv(control1_file_name_norm(), header = TRUE, sep = ",")
    control2 <- read.csv(control2_file_name_norm(), header = TRUE, sep = ",")
    
    x_dat <- c()
    if(ma_scale_sel_norm() == FALSE) {
      x_dat <- (2**df1[[2]] + 2**df2[[2]]) / 2
    }
    else {
      x_dat <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
    }
    y_dat <- df1[[2]] - df2[[2]]
    
    control_x_dat <- c()
    if(ma_scale_sel_norm() == FALSE) {
      control_x_dat <- (2**control1[[2]] + 2**control2[[2]]) / 2
    }
    else {
      control_x_dat <- log10((2**control1[[2]] + 2**control2[[2]]) / 2)
    }
    control_y_dat <- control1[[2]] - control2[[2]]
    
    
    
    df_download <- data.frame("barcode_index" = df1[[1]], "x" = x_dat, "y" = y_dat, "index" = as.integer(row.names(df1)) - 1)
    lib_len <- library_list[[strsplit(as.character(df_download$barcode_index[1]), "BC")[[1]][1]]]
    df_bin_matrix_download <- cbind(df_download, bin_matrix(lib_len))
    
    
    # Making a SD column if it doesn't exist, in order to prevent out of index boundray problem.
    if (length(names(df1)) < 3) {
      df1$SD <- 0 
    }
    
    if (length(names(df2)) < 3) {
      df2$SD <- 0
    }
    
    # Using standard deviation as the measurment for color scale, smaller value has a bigger size.
    color_scale <- sqrt(df1[[3]]**2 + df2[[3]]**2) 
    
    if (mean(df1[[3]]) == 0 && mean(df2[[3]]) == 0) {
      output$maplot_out_norm <- renderPlotly({
        p <- plot_ly(type = "scatter", mode = "markers", x = x_dat, y = y_dat, text = paste(df1[[1]], "<br>SD: ", df1[[3]]), hoverinfo = text, source = "ma",
                     name = strsplit(as.character(df1[[1]][1]), split = "BC")[[1]][1]) %>%
          layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                 yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
        add_trace(p, x = control_x_dat, y = control_y_dat, type = "scatter", mode = "markers", name = 'Control',
                  text = control1[[1]], hoverinfo = text, inherit = FALSE)
        
      }) 
    }
    else {
      output$maplot_out_norm <- renderPlotly({
        p <- plot_ly(type = "scatter", mode = "markers", x = x_dat, y = y_dat, text = paste(df1[[1]], "<br>SD: ", df1[[3]]), hoverinfo = text, source = "ma",
                     name = strsplit(as.character(df1[[1]][1]), split = "BC")[[1]][1], color = color_scale, size = 1/color_scale) %>%
          layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                 yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
        
        add_trace(p, x = control_x_dat, y = control_y_dat, type = "scatter", mode = "markers", name = 'Control',
                  text = control1[[1]], hoverinfo = text, inherit = FALSE)
      })
      
    } 
    
    
    
    df_heatmap <- df_bin_matrix_download[order(df_bin_matrix_download$y, decreasing = TRUE),]
    df_bin_heatmap <- as.matrix(df_heatmap[, -which(names(df_heatmap) %in% c("barcode_index", "x", "y", "index"))])
    
    
    output$bin_heatmap_out_norm <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "ma")
      selected_index <- selected_data$pointNumber
      
      if (is.null(selected_data) == TRUE){
        plot_ly(x = colnames(df_bin_heatmap), y = df_heatmap$barcode_index, z = df_bin_heatmap, type = "heatmap", colors = c("green", "red")) %>%
          # the heatmap was upside down, so using autorange='reversed' will filp it around
          layout(
            yaxis = list(autorange='reversed'))
      }
      else {
        # df_heatmap is a data frame, so when select range from rows, it should be df[x, ] where x is on the left of ","
        df_heatmap_sub <- df_heatmap[which(df_heatmap$index %in% selected_index), ]
        df_bin_heatmap_sub <- as.matrix(df_heatmap_sub[, -which(names(df_heatmap) %in% c("barcode_index", "x", "y", "index"))])
        plot_ly(x = colnames(df_bin_heatmap_sub), y = df_heatmap_sub$barcode_index, z = df_bin_heatmap_sub, type = "heatmap", colors = c("green", "red")) %>%
          layout(
            yaxis = list(autorange='reversed'))
      }
    })
    
    output$ma_plot_data_download_norm <- downloadHandler(filename = ".csv", 
                                                         content = function(file) {
                                                           selected_dat <- event_data("plotly_selected", source = "ma")
                                                           df_download_with_lasso <- df_bin_matrix_download[which(df_bin_matrix_download$index %in% selected_dat$pointNumber), ]
                                                           if (is.null(selected_dat)) {
                                                             df_download_with_lasso <- df_bin_matrix_download  
                                                           }
                                                           write.csv(df_download_with_lasso, file, row.names = FALSE) 
                                                           #write.csv(df_bin_matrix_download, file, row.names = FALSE) 
                                                         })
  })
  
  # End of MA Plot normalized data (V1)
  
  
  # MA Plot normalized data V2
  # Members 
  df1_file_name_norm_v2 <- reactive({
    req(input$input_file1_norm_v2)
    file_obj <- input$input_file1_norm_v2
    file_name <- file_obj$datapath
    file_name
  })
  
  df2_file_name_norm_v2 <- reactive({
    req(input$input_file2_norm_v2)
    file_obj <- input$input_file2_norm_v2
    file_name <- file_obj$datapath
    file_name
  })
  
  control1_file_name_norm_v2 <- reactive({
    req(input$input_control1_norm_v2)
    file_obj <- input$input_control1_norm_v2
    file_name <- file_obj$datapath
    file_name
  })
  
  control2_file_name_norm_v2 <- reactive({
    req(input$input_control2_norm_v2)
    file_obj <- input$input_control2_norm_v2
    file_name <- file_obj$datapath
    file_name
  })
  
  ma_scale_sel_norm_v2 <- reactive({
    ma_scale <- input$ma_scale_check_norm_v2
    ma_scale
  })
  
  raw_counts_prefilter_v2 <- reactive({
    prefilter <- input$maplot_norm_raw_prefilter_v2
    prefilter
  })
  
  # End of Members
  observeEvent(input$maplot_norm_v2, {
    df1 <- read.csv(df1_file_name_norm_v2(), header = TRUE, sep = ",")
    df2 <- read.csv(df2_file_name_norm_v2(), header = TRUE, sep = ",")
    control1_df <- c()
    control2_df <- c()
    df1$cv <- NULL
    df2$cv <- NULL
    
    if(is.null(df1$raw_counts)) {
      df1$raw_counts = 0
    }
    if(is.null(df2$raw_counts)) {
      df2$raw_counts = 0
    }
    
    if(is.null(df1$sd)) {
      df1$sd = 0
    }else {
      df1$cv = (df1$sd / df1[[2]]) * 100
    }
    
    if(is.null(df2$sd)) {
      df2$sd = 0
    }else {
      df2$cv = (df2$sd / df2[[2]]) * 100
    }
    
    df1$raw_counts_corr <- df1$raw_counts
    df2$raw_counts_corr <- df2$raw_counts
    
    df1$raw_counts <- (df1$raw_counts + df2$raw_counts) / 2
    df2$raw_counts <- df1$raw_counts
    
    x_dat_raw <- df1$raw_counts
    
    # df1 <- subset(df1, df1$raw_counts >= input$maplot_norm_raw_prefilter_v2)
    # df2 <- subset(df2, df2$raw_counts >= input$maplot_norm_raw_prefilter_v2)
    
    
    
    x_dat <- c()
    if(ma_scale_sel_norm_v2() == FALSE) {
      x_dat <- (2**df1[[2]] + 2**df2[[2]]) / 2
    }
    else {
      x_dat <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
    }
    
    y_dat <- df1[[2]] - df2[[2]]
    
    control_x_dat <- c()
    control_y_dat <- c()
    
    if (input$control_check_norm_v2 == TRUE) {
      control1_df <- read.csv(control1_file_name_norm_v2(), header = TRUE, sep = ",")
      control2_df <- read.csv(control2_file_name_norm_v2(), header = TRUE, sep = ",")
      if (ma_scale_sel_norm_v2() == FALSE) {
        control_x_dat <- (2**control1_df[[2]] + 2**control2_df[[2]]) / 2
      }
      else {
        control_x_dat <- log10((2**control1_df[[2]] + 2**control2_df[[2]]) / 2)
      }
      control_y_dat <- control1_df[[2]] - control2_df[[2]]
    }
    
    
    
    # This section is to merge anc matrix to the ma plot result dataframe
    # determine which anc library it it, assign the var to a string
    which_anc <- strsplit(as.character(df1$barcode_index[1]), "BC")[[1]][1]
    anc_bin_matrix_df <- eval(parse(text = tolower(paste(which_anc, "_bin_matrix_df", sep = ""))))
    anc_bin_matrix_df <- anc_bin_matrix_df[which(anc_bin_matrix_df$barcode_index_matrix %in% df1$barcode_index),]
    anc_bin_matrix_df <- subset(anc_bin_matrix_df, select = -barcode_index_matrix)
    # anc_bin_matrix_df <- anc_bin_matrix_df[, !(colnames(anc_bin_matrix_df) == "barcode_index_matrix")]
    # anc_bin_matrix_df <- anc_bin_matrix_df[, -barcode_index_matrix]
    # End of anc matrix preparation
    
    # Structuring a ploting dataframe with only anc libraries, no control
    
    # variables for correlation
    df1_col_name <- colnames(df1)[2]
    df2_col_name <- colnames(df2)[2]
    # end of variables of correlation
    
    df_anc_plot <- cbind(data.frame("barcode_index" = df1[[1]], "x" = x_dat, "y" = y_dat,
                                    df1_col_name = df1[[2]], df2_col_name = df2[[2]]), anc_bin_matrix_df)
    colnames(df_anc_plot)[which(colnames(df_anc_plot) == "df1_col_name")] <- df1_col_name
    colnames(df_anc_plot)[which(colnames(df_anc_plot) == "df2_col_name")] <- df2_col_name
    # In order to insert ranking column, it needs to be order by y value and then re-order back by index column.
    df_anc_plot$raw_counts <- x_dat_raw
    df_anc_plot <- df_anc_plot[order(df_anc_plot$y, decreasing = TRUE),]
    df_anc_plot$ranking <- 1:nrow(df_anc_plot)
    df_anc_plot <- df_anc_plot[order(df_anc_plot$index),]
    
    
    # Adding sd column
    df_anc_plot$sd <- sqrt(df1$sd**2 + df2$sd**2)
    # Adding cv column
    df_anc_plot$cv <- df1$cv
    print(df_anc_plot)
    
    # CV filter ui on sidebar
    output$maplot_cv_filter_v2 <- renderUI(sliderInput("cv_slider", label = "CV Filter", min = min(df_anc_plot$cv), max = max(df_anc_plot$cv), 
                                                       value = c(min(df_anc_plot$cv), max(df_anc_plot$cv))))
    output$maplot_cv_filter_checkbox_v2 <- renderUI(checkboxInput("cv_checkbox", label = "check for cv", value = FALSE))
    # End of CV filter ui
 
    df_anc_plot <- subset(df_anc_plot, df_anc_plot$cv >= input$cv_slider[1] & df_anc_plot$cv <= input$cv_slider[2])
    # Filtering by raw_counts
    df_anc_plot <- subset(df_anc_plot, df_anc_plot$raw_count >= input$maplot_norm_raw_prefilter_v2)
    df_control_plot <- data.frame("barcode_index" = control1_df[[1]], "x" = control_x_dat, "y" = control_y_dat)
    # get anc library positions from the lists in global.r
    anc_pos <- eval(parse(text = paste(tolower(which_anc), "pos", sep = "_")))
    
    
      
    selected_radios <- c()
    
    # Rendering all the components(uiOutput) inside the tab area
    output$ma_norm_radio_buttons_v2 <- renderUI(
        fluidRow(
          lapply(c(1: position_number_list[[which_anc]]), function(i) {
            column(1, radioButtons(anc_pos[i], label = anc_pos[i], choices = c(0, 1, "reset"), selected = FALSE))
          })
        )
      )
    
   
    output$maplot_norm_heatmap_button_v2 <- renderUI(actionButton("maplot_norm_heatmap_v2", label = "Heatmap"))
    
    output$maplot_norm_corr_button_v2 <- renderUI(actionButton("maplot_norm_corr_v2", label = "Corr"))
    
    
    # End of uiOutput rendering
    
    df_heatmap <- c()
    
    output$maplot_out_norm_v2 <- renderPlotly({
      for (x in anc_pos) {
        if(is.null(input[[x]]) == FALSE) {
          if (input[[x]] != "reset") {
            selected_radios <- c(selected_radios, x)
          }
        }
      }

      sub_df_anc_condition_stat_str_list <- lapply(selected_radios, function(x){
        paste(paste("df_anc_plot$", x, sep = ""), input[[x]], sep = "==")
      })

      sub_df_anc_condition_stat <- paste("subset(df_anc_plot, ", paste(sub_df_anc_condition_stat_str_list, collapse = " & "), ")", sep = "")
      df_anc_plot <- eval(parse(text = sub_df_anc_condition_stat))
      # mean of sd equals to 0 is only one situation which is none of the data has sd column, that's why use mean()==0 to evaluate the situation
      # Linear Regression members
      ma_fit <- lm(y ~ x, data = df_anc_plot)
      # ma_fit <- df_anc_plot %>% lm(y ~ x,.) %>% fitted.values()
      
      # End of Linear members
      if (mean(df_anc_plot$sd)==0) {
        p <- plot_ly(type = "scatter", mode = "markers", x = df_anc_plot$x, y = df_anc_plot$y,
                     text = df_anc_plot$barcode_index, hoverinfo = text, source = "ma", name = which_anc) %>%
             add_lines(x = ~df_anc_plot$x, y = fitted(ma_fit)) %>%
             layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                    yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
        # if (is.null(control_x_dat) == FALSE & is.null(control_y_dat) == FALSE) {
        if (input$control_check_norm_v2 == TRUE) {
          p <- add_trace(p, x = df_control_plot$x, y = df_control_plot$y, type = "scatter", mode = "markers+text", name = 'Control',
                    text = df_control_plot$barcode_index, hoverinfo = text, inherit = FALSE)
        }
        
        df_heatmap <<- df_anc_plot[order(df_anc_plot$y, decreasing = TRUE),]
        
        p
     
      }
      else {
        p <- plot_ly(type = "scatter", mode = "markers", x = df_anc_plot$x, y = df_anc_plot$y,
                     text = df_anc_plot$barcode_index, hoverinfo = text, source = "ma", name = which_anc, color = df_anc_plot$sd, 
                     size = 1/df_anc_plot$sd) %>%
             add_lines(x = ~df_anc_plot$x, y = fitted(ma_fit)) %>%
             layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                    yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
        # if (is.null(control_x_dat) == FALSE & is.null(control_y_dat) == FALSE) {
        if (input$control_check_norm_v2 == TRUE) {
          p <- add_trace(p, x = df_control_plot$x, y = df_control_plot$y, type = "scatter", mode = "markers+text", name = 'Control',
                    text = df_control_plot$barcode_index, hoverinfo = text, inherit = FALSE)
        }
        
        df_heatmap <<- df_anc_plot[order(df_anc_plot$y, decreasing = TRUE),]
        
        p
      }
      
    })
    
    
    # heatmap section
    # event_data's pointNumber isn't a reliable solution for subset of the original data from a plot. 
    
    download_dat_ma_plot_norm_v2 <- c()
    dat_ma_plot_norm_corr_v2 <- c()
    
    observeEvent(input$maplot_norm_heatmap_v2, {
      output$bin_heatmap_out_norm_v2 <- renderPlotly({
        selected_data <- event_data("plotly_selected", source = "ma")
        
        df_bin_heatmap <- as.matrix(df_heatmap[, which(names(df_heatmap) %in% anc_pos)])
        
        if (is.null(selected_data)) {
          # using <<- to assign the download data globally
          download_dat_ma_plot_norm_v2 <<- df_heatmap
          dat_ma_plot_norm_corr_v2 <<- download_dat_ma_plot_norm_v2
          plot_ly(x = colnames(df_bin_heatmap), y = df_heatmap$barcode_index, z = df_bin_heatmap, type = "heatmap", colors = c("green", "red")) %>%
            # the heatmap was upside down, so using autorange='reversed' will filp it around
            layout(yaxis = list(autorange='reversed'))
        }  
        else {
          
          df_anc_plot_lasso <- df_heatmap[order(df_heatmap$index, decreasing = FALSE),]
          df_anc_plot_lasso$lasso_index <- seq.int(row.names(df_heatmap))
          df_anc_plot_lasso$lasso_index <- df_anc_plot_lasso$lasso_index - 1
          selected_index <- selected_data$pointNumber
          df_anc_plot_lasso<- df_anc_plot_lasso[which(df_anc_plot_lasso$lasso_index %in% selected_index),]
          df_anc_plot_lasso <- df_anc_plot_lasso[order(df_anc_plot_lasso$y, decreasing = TRUE), ]
          df_anc_plot_lasso_bin <- as.matrix(df_anc_plot_lasso[, which(names(df_anc_plot_lasso) %in% anc_pos)])
          
          # using <<- to assign the download data globally
          download_dat_ma_plot_norm_v2 <<- df_anc_plot_lasso
          dat_ma_plot_norm_corr_v2 <<- download_dat_ma_plot_norm_v2
          plot_ly(x = colnames(df_anc_plot_lasso_bin), y = df_anc_plot_lasso$barcode_index,
                  z = df_anc_plot_lasso_bin, type = "heatmap", colors = c("green", "red")) %>%
            # the heatmap was upside down, so using autorange='reversed' will filp it around
            layout(yaxis = list(autorange='reversed'))
        }
      
      })
        
    }) 
     
    # End of heatmap section
    
    # correlation scatter plot
    observeEvent(input$maplot_norm_corr_v2, {
      output$ma_norm_v2_corr <- renderPlotly({
        p <- GGally::ggpairs(dat_ma_plot_norm_corr_v2[, c(df1_col_name, df2_col_name)],
                             higher = list(continuous="cor", corMethod="spearman", combo="dot", discrete="facetbar")) 
        ggplotly(p)
      })
    })
    
    
    # End of correlation scatter plot
    
    output$ma_plot_data_download_norm_v2 <- downloadHandler(filename = ".csv",
                                                              content = function(file) {
                                                                write.csv(download_dat_ma_plot_norm_v2, file, row.names = FALSE)
                                                              })
  
  })
  
  
  
  # End of MA Plot normalized data
  
  # Multiple MA Plots
  output$multi_ma_plots_input <- renderUI({
    num_of_file_input <- as.integer(input$num_of_lib_input)
    lapply(c(1:(num_of_file_input)), function(i) {
      tagList(
        fileInput(paste("multi_ma_lib_", as.character(i), sep = ""),
                  label = paste("Select one library file : library ", as.character(i), sep = ""), multiple = FALSE)
        
      )
      
    })
    
  })
  
  output$multi_ma_plots_input_pool <- renderUI({
    num_of_file_input <- as.integer(input$num_of_lib_input)
    lapply(c(1:(num_of_file_input)), function(i) {
      tagList(
        fileInput(paste("multi_ma_pool_", as.character(i), sep = ""),
                  label = paste("Select one pool file : pool library ", as.character(i), sep = ""), multiple = FALSE)
      ) 
      
      
    })
  })
  
  avg_zero_filter <- reactive({
    zero_filter <- input$zero_filter
    zero_filter
  })
  
  
  ###### Using input[["var"]] instead of input$"var" ######
  observeEvent(input[["multi_maplot"]], {
    # num_of_lib is the total number of the libraries in  ma plots
    num_of_lib <- as.integer(input$num_of_lib_input) 
    
    lib_control_df <- read.csv(file = input$library_control[["datapath"]], sep = ",")
    pool_control_df <- read.csv(file = input$virus_pool_control[["datapath"]], sep = ",")
    
    lib_df_list <- lapply(c(1:num_of_lib), function(x){
      file_obj <- input[[paste("multi_ma_lib_", as.character(x), sep = "")]]
      read.csv(file = file_obj$datapath, sep = ",")
    })
    
    pool_lib_df_list <- lapply(c(1:num_of_lib), function(x){
      file_obj <- input[[paste("multi_ma_pool_", as.character(x), sep = "")]]
      read.csv(file = file_obj$datapath, sep = ",")
    })
    
    #MA plot data download function#
    
    df_download <- data.frame("barcode_index" = lib_df_list[[1]][[1]], "x" = (2**lib_df_list[[1]][[2]] + 2**pool_lib_df_list[[1]][[2]]) / 2 , 
                              "y" = lib_df_list[[1]][[2]] - pool_lib_df_list[[1]][[2]], "index" = as.integer(row.names(lib_df_list[[1]])) - 1)
    
    if(num_of_lib >= 2) {
      for (i in 2:num_of_lib) {
        df_download1 <- data.frame("barcode_index" = lib_df_list[[i]][[1]], "x" = (2**lib_df_list[[i]][[2]] + 2**pool_lib_df_list[[i]][[2]]) / 2 , 
                                   "y" = lib_df_list[[i]][[2]] - pool_lib_df_list[[i]][[2]], "index" = as.integer(row.names(lib_df_list[[i]])) - 1)
        
        df_download <- rbind(df_download, df_download1)
      }
    }
    
    control_df_download <- data.frame("barcode_index" = lib_control_df[[1]], "x" = (2**lib_control_df[[2]] + 2**pool_control_df[[2]]) / 2 , 
                              "y" = lib_control_df[[2]] - pool_control_df[[2]], "index" = as.integer(row.names(lib_control_df)) - 1)
    
    df_download <- rbind(df_download, control_df_download)
    
    output$multi_ma_plot_data_download <- downloadHandler(filename = ".csv",
                                                          content = function(file) {
                                                            write.csv(df_download, file, row.names = FALSE)
                                                          })
    
    # print(df_download)
    
    
    # End of MA plot data download function#
    
    output$multi_maplot_out <- renderPlotly({
      p <- plot_ly(type = "scatter", mode = "markers", x = (2**lib_df_list[[1]][[2]] + 2**pool_lib_df_list[[1]][[2]]) / 2,
                   y = lib_df_list[[1]][[2]] - pool_lib_df_list[[1]][[2]], text = lib_df_list[[1]][[1]], hoverinfo = text, 
                   name = paste(colnames(lib_df_list[[1]])[2], strsplit(as.character(lib_df_list[[1]][[1]][1]), split = "BC")[[1]][1], sep = "_")) %>% 
        layout(title = paste("Multiple Libraries", colnames(pool_lib_df_list[[1]])[2], sep = " - " ), yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG Abundance"))
      
      if(num_of_lib >= 2){
        for (i in 2:num_of_lib) {
          p <- add_trace(p, x = (2**lib_df_list[[i]][[2]] + 2**pool_lib_df_list[[i]][[2]]) / 2,
                         y = lib_df_list[[i]][[2]] - pool_lib_df_list[[i]][[2]], text = lib_df_list[[i]][[1]], hoverinfo = text, 
                         name = paste(colnames(lib_df_list[[i]])[2], strsplit(as.character(lib_df_list[[i]][[1]][1]), split = "BC")[[1]][1] , sep = "_"))
        }
      }
      
      p <- add_trace(p, x = (2**lib_control_df[[2]] + 2**pool_control_df[[2]]) / 2, 
                     y = lib_control_df[[2]] - pool_control_df[[2]], text = lib_control_df[[1]], hoverinfo = text, 
                     name = paste(colnames(lib_control_df)[2], "Control", sep = "_"))
      
      layout(p, title = "Multiple MA Plots", yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG Abundance"))
      
      output$multi_ma_plot_img_download <- downloadHandler(filename = ".png",
                                                            content = function(file) {
                                                              plotly_IMAGE(p, width = 1000, height = 800,
                                                                           format = "png", scale = 1, out_file = file)
                                                            })
     
      output$multi_ma_plot_html_download <- downloadHandler(filename = ".html",
                                                            content = function(file) {
                                                              htmlwidgets::saveWidget(p, file = file)
                                                            })
      
      p
      
      
    })
    
    
    
  })
  
  observeEvent(input[["multi_maplot_log"]], {
    # num_of_lib is the total number of the libraries in  ma plots
    num_of_lib <- as.integer(input$num_of_lib_input) 
    
    lib_control_df <- read.csv(file = input$library_control[["datapath"]], sep = ",")
    pool_control_df <- read.csv(file = input$virus_pool_control[["datapath"]], sep = ",")
    
    lib_df_list <- lapply(c(1:num_of_lib), function(x){
      file_obj <- input[[paste("multi_ma_lib_", as.character(x), sep = "")]]
      read.csv(file = file_obj$datapath, sep = ",")
    })
    
    pool_lib_df_list <- lapply(c(1:num_of_lib), function(x){
      file_obj <- input[[paste("multi_ma_pool_", as.character(x), sep = "")]]
      read.csv(file = file_obj$datapath, sep = ",")
    })
    
    #MA plot data download function#
    
    df_download <- data.frame("barcode_index" = lib_df_list[[1]][[1]], "x" = log10((2**lib_df_list[[1]][[2]] + 2**pool_lib_df_list[[1]][[2]]) / 2) , 
                              "y" = lib_df_list[[1]][[2]] - pool_lib_df_list[[1]][[2]], "index" = as.integer(row.names(lib_df_list[[1]])) - 1)
    
    if(num_of_lib >= 2) {
      for (i in 2:num_of_lib) {
        df_download1 <- data.frame("barcode_index" = lib_df_list[[i]][[1]], "x" = log10((2**lib_df_list[[i]][[2]] + 2**pool_lib_df_list[[i]][[2]]) / 2) , 
                                   "y" = lib_df_list[[i]][[2]] - pool_lib_df_list[[i]][[2]], "index" = as.integer(row.names(lib_df_list[[i]])) - 1)
        
        df_download <- rbind(df_download, df_download1)
      }
    }
    
    control_df_download <- data.frame("barcode_index" = lib_control_df[[1]], "x" = log10((2**lib_control_df[[2]] + 2**pool_control_df[[2]]) / 2) , 
                              "y" = lib_control_df[[2]] - pool_control_df[[2]], "index" = as.integer(row.names(lib_control_df)) - 1)
    
    df_download <- rbind(df_download, control_df_download)
    
    output$multi_ma_plot_data_download <- downloadHandler(filename = ".csv",
                                                          content = function(file) {
                                                            write.csv(df_download, file, row.names = FALSE)
                                                          })
    
    
    # End of MA plot data download function#
    
    output$multi_maplot_out <- renderPlotly({
      p <- plot_ly(type = "scatter", mode = "markers", x = log10((2**lib_df_list[[1]][[2]] + 2**pool_lib_df_list[[1]][[2]]) / 2),
                   y = lib_df_list[[1]][[2]] - pool_lib_df_list[[1]][[2]], text = lib_df_list[[1]][[1]], hoverinfo = text, 
                   name = paste(colnames(lib_df_list[[1]])[2], strsplit(as.character(lib_df_list[[1]][[1]][1]), split = "BC")[[1]][1], sep = "_")) %>% 
        layout(title = paste("Multiple Libraries", colnames(pool_lib_df_list[[1]])[2], sep = " - " ), yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG Abundance"))
      
      if(num_of_lib >= 2){
        for (i in 2:num_of_lib) {
          p <- add_trace(p, x = log10((2**lib_df_list[[i]][[2]] + 2**pool_lib_df_list[[i]][[2]]) / 2),
                         y = lib_df_list[[i]][[2]] - pool_lib_df_list[[i]][[2]], text = lib_df_list[[i]][[1]], hoverinfo = text, 
                         name = paste(colnames(lib_df_list[[i]])[2], strsplit(as.character(lib_df_list[[i]][[1]][1]), split = "BC")[[1]][1] , sep = "_"))
        }
      }
      
      p <- add_trace(p, x = log10((2**lib_control_df[[2]] + 2**pool_control_df[[2]]) / 2), 
                     y = lib_control_df[[2]] - pool_control_df[[2]], text = lib_control_df[[1]], hoverinfo = text, 
                     name = paste(colnames(lib_control_df)[2], "Control", sep = "_"))
      
      layout(p, title = "Multiple MA Plots", yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG Abundance"))
      
      p
      
      
    })
    
  })
  
  
  # End of Multiple MA Plots
  
  
  # Binary table generator
  observeEvent(input$generate_percent, {
    df <- read.csv(df_bin_file_name(), header = TRUE, sep = ",")
    lib_size <- length(df[[1]])
    position_size <- log2(lib_size)
    zeros_list <- rep(0, position_size)
    matrix_data <- bin_matrix(lib_size - 1)
    for (i in 1:lib_size) {
      for (j in 1:position_size) {
        if (matrix_data[[j]][i] == 0) {
          zeros_list[j] <- zeros_list[j] + df[[2]][i]
        }
      }
    }
    zeros_list <- zeros_list / sum(df[[2]])
    ones_list <- 1 - zeros_list
    column_names_list <- lapply(c(1:length(zeros_list)), function(x) {paste("p", x, sep = "")})
    result_matrix <- data.frame(rbind(zeros_list, ones_list), row.names = c(0,1)) 
    colnames(result_matrix) <- column_names_list
    output$binary_table_out <- renderDataTable(result_matrix)
    
    if (is.null(result_matrix) == FALSE) {
      output$generate_percent_data_button <- renderUI(downloadButton("generate_percent_data_download", "Download Binary Table"))
    }
    output$generate_percent_data_download <- downloadHandler(filename = ".csv",
                                                             content = function(file) {
                                                               write.csv(result_matrix, file, row.names = TRUE)
                                                             })
    
    
  })
  
  
  #Ploting scatter matrix 
  observeEvent(input$scatterplot, {
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    df <- df_concat(df_list)
    df[2:length(df)] <- normalization(df[2:length(df)])
    output$scatterplot_out <- renderPlotly({
      pm <- GGally::ggpairs(df[2:length(df)], 
                            higher = list(continuous="cor", corMethod="spearman", combo="dot", discrete="facetbar"))
      ggplotly(pm) 
    }) 
  })
  
  #Plotting histogram
  observeEvent(input$histogram, {
    
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    df <- df_concat(df_list)
    df[2:length(df)] <- normalization(df[2:length(df)])
    output$histogram_out <- renderPlotly({
      p <- plot_ly(x = df[[2]], type = "histogram", alpha = 0.5, name = colnames(df)[2])
      for (i in 3:length(df)) {
        p <- add_trace(p, x = df[[i]], type = "histogram", name = colnames(df)[i])
      }
      p <- p %>% layout(barmode = "overlay", xaxis = list(title = "Counts (normalized)"), yaxis = list(title = "Frequency"))
      
    })
  })
  
  #Select range of data from the histogram and merge them into a new data frame.
  lower_bound <- reactive({
    input$hist_low
  })
  higher_bound <- reactive({
    input$hist_high
  })
  
  observeEvent(input$select_merge_range, {
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    df_list_norm <- df_list_normalization(df_list)
    merged_df <- range_select(df_list_norm, as.numeric(lower_bound()), as.numeric(higher_bound()))
    # View(merged_df)
    output$select_merged_out <- renderDataTable(merged_df)
    output$save_merged_data <- downloadHandler(
      filename = function() {
        paste(lower_bound(), "_", higher_bound(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(merged_df, file)
      }
    )
  })
  
  
  
  # Merging csv files and allows user to download combined data or just mean value data 
  
  # Give the name of the new created mean column
  mean_col_name <- eventReactive(input$column_merge, {
    input$mean_column_name
  })
  
  sd_col_name <- eventReactive(input$column_merge, {
    input$sd_column_name
  })
  
  csv_check_box <- reactive({
    if_checked <- input$csv_file_option
    if_checked
  })
  
  observeEvent(input$column_merge, {
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    # df_list_norm <- df_list_normalization(df_list)
    df <- df_concat(df_list)
    df[mean_col_name()] <- rowMeans(df[,-1]) # using df[,-1] to exclude the first column
    df[sd_col_name()] <-apply(df[c(-1, -length(df))], 1, sd)
    df_col_names <- colnames(df)
    output$merged_data_view <- renderDataTable(df)
    if(csv_check_box() == TRUE) {
      output$csv_merge_download <- downloadHandler(
        filename = ".csv",
        content = function(file) {
          write.csv(df, file, row.names = FALSE) # using row.names=FALSE to prevent creating an index column
        }
      )
    }
    else {
      output$csv_merge_download <- downloadHandler(
        filename = ".csv",
        content = function(file) {
          write.csv(df[, c(df_col_names[1], df_col_names[length(df_col_names) - 1], df_col_names[length(df_col_names)])],
                    file, row.names = FALSE) # using row.names=FALSE to prevent creating an index column
        }
      )
    }
    
  })
  
  observeEvent(input$row_merge, {
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    #print(df_list[[1]])
    download_df <- df_list[[1]]
    for (i in (2:length(df_list))) {
      download_df <- rbind(download_df, df_list[[i]])
    }
    output$merged_data_view <- renderDataTable(download_df)
    output$csv_merge_download <- downloadHandler(
      filename = ".csv", content = function(file) {
        write.csv(download_df, file, row.names = FALSE)
      }
    )
  })
  
  observeEvent(input$merge_normalize, {
    file_list <- df_name_list()$datapath
    df_list <- lapply(file_list, function(x) {read.csv(x, header = TRUE, sep = ",")})
    df_list_norm <- df_list_normalization(df_list)
    df <- df_concat(df_list_norm)
    df[mean_col_name()] <- rowMeans(df[,-1]) # using df[,-1] to exclude the first column
    df_col_names <- colnames(df)
    output$merged_data_view <- renderDataTable(df)
    if(csv_check_box() == FALSE) {
      output$csv_merge_download <- downloadHandler(
        filename = ".csv",
        content = function(file) {
          write.csv(df, file, row.names = FALSE) # using row.names=FALSE to prevent creating an index column
        }
      )
    }
    else {
      output$merged_data_view <- renderDataTable(df[, c(df_col_names[1], df_col_names[length(df_col_names)])])
      output$csv_merge_download <- downloadHandler(
        filename = ".csv",
        content = function(file) {
          write.csv(df[, c(df_col_names[1], df_col_names[length(df_col_names)])],
                    file, row.names = FALSE) # using row.names=FALSE to prevent creating an index column
        }
      )
    } 
  })
  
  # Multiple library normalization
  observeEvent(input$multi_lib_normalize, {
    file_list <- df_name_list()
    # View(file_list)
    sum_list <- c()
    for (x in file_list$datapath) {
     sum_list <- c(sum_list, sum(read.csv(x)[2])) 
    }
    total_sum <- sum(sum_list)
    
    norm_df_list <- lapply(file_list$datapath, function(x) {
      origin_df <- read.csv(x, header = TRUE, sep = ",")
      origin_df[2] <- log2((read.csv(x)[2] + 0.5) / (total_sum + 1) * 1000000)
      norm_df <- origin_df
    })
    
    if (is.null(norm_df_list) == FALSE) {
      output$multi_lib_normalization_download <- renderUI({
        downloadButton("multi_lib_download_button", "Download normalized files")
      })
    }
    
    output$multi_lib_download_button <- downloadHandler(
      filename = ".zip",
      content = function(file) {
        files <- NULL
        for (i in 1:length(file_list)) {
          file_name_split <- strsplit(file_list[i]$name, "[.]")
          file_name <- paste(paste(file_name_split[[i]][1], "_normalized", sep = ""), ".csv", sep = "")
          write.csv(norm_df_list[[i]], file, row.names = FALSE)
          files <- c(file_name, files)
          #print(files)
        } 
        zip(file, files)
        
      }
      
    )
  })
  
  # End of Merge App
  
  # Experiment form 
  output$exp_form <- renderUI({
    tagList(
    textInput("exp_name", label = "Experiment Name"),
    checkboxGroupInput("libs_selected", label = "Select the libraries that are in the experiment.", 
                       choices = list("Anc80" = "Anc80", "Anc81" = "Anc81", "Anc82" = "Anc82", "Anc83" = "Anc83", "Anc84" = "Anc84", 
                                      "Anc110" = "Anc110", "Anc126" = "Anc126", "Anc127" = "Anc127", "Control" = "Control")),
    dateInput("exp_date", "Date:", format = "mm/dd/yyyy"),
    downloadButton("exp_form_download", label = "Save the form")
    ) 
  })
  
  experiment_name <- reactive({
    input$exp_name
  })
  
  experiment_libraries_list <- reactive({
    input$libs_selected
  })
  
  experiment_date <- reactive({
    input$exp_date
  })
  
  
  # End of Experiment form
  
  # Dynamic UI for Gating APP
  output$gating_group_inputs <- renderUI({
    num_of_file_input <- as.integer(input$num_of_gating_input)
    
    # using lapply to get total number of fileInput fields
    lapply(c(1:(num_of_file_input*2)), function(i) {
      j <- 1
      k <- 1
      if (i %% 2 == 0) {
        j <- i / 2
        k <- 2
      }
      else {
        j <- floor(i / 2) + 1
        k <-  1
      }
      
      fileInput(paste("gating_file", as.character(i), sep = ""),
        paste(paste("group", as.character(j), sep = ""), paste("file", as.character(k), sep = ""), sep = " , "))
    })
  })
  
  #Creating a list of names which match the dynamic gating fileInput names
  gating_fileInput_names <- reactive({
    alist <- list(input$gating_file1, input$gating_file2, input$gating_file3, input$gating_file4, input$gating_file5,
                  input$gating_file6, input$gating_file7, input$gating_file8, input$gating_file9, input$gating_file10)
    alist
    })
  
  # gating dynamic ui testing
  gating_input_reactive <- reactive({
    a <- gating_fileInput_names()[[1]]
    b <- input$gating_file2
    x <- list(a$datapath, b$datapath)
    x
  })
  
  
  #Gating App
  observeEvent(input$gating, {
  ########################## new version #####################################
    # First get the input number of UI from the dynamic ui, and then create the exact number of file input list
    # Preparing data
    num_of_group_input<- as.integer(input$num_of_gating_input)
    selected_gating_fileInput_list <- gating_fileInput_names()[1:(num_of_group_input*2)] # the list of the file objects, it contains name, size, etc.
    df_list <- lapply(selected_gating_fileInput_list, function(x) {read.csv(x$datapath, header = TRUE, sep = ",")})
    
    # Start plotting
    # The original maplot output section
    output$gating_in1 <- renderPlotly({
      x_data <- log10((2**df_list[1][[1]][[2]] + 2**df_list[2][[1]][[2]]) / 2)
      y_data <- df_list[1][[1]][[2]] - df_list[2][[1]][[2]]
      plot_ly(x = x_data, y = y_data, type = "scatter", mode = "markers", text = df_list[1][[1]][[1]], hoverinfo = text, source = "sub_gate", 
              color = df_list[1][[1]][[3]], size = 1/df_list[1][[1]][[3]]) %>%
        layout(title = paste(colnames(df_list[1][[1]])[2], colnames(df_list[2][[1]])[2], sep = " - "),
          yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    }) 
    
    output$gating_in2 <- renderPlotly({
      x_data <- log10((2**df_list[3][[1]][[2]] + 2**df_list[4][[1]][[2]]) / 2)
      y_data <- df_list[3][[1]][[2]] - df_list[4][[1]][[2]]
      plot_ly(x = x_data, y = y_data, type = "scatter", mode = "markers", text = df_list[3][[1]][[1]], hoverinfo = text, source = "sub_gate",
              color = df_list[3][[1]][[3]], size = 1/df_list[3][[1]][[3]]) %>%
        layout(title = paste(colnames(df_list[3][[1]])[2], colnames(df_list[4][[1]])[2], sep = " - "),
          yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    }) 
  
    output$gating_in3 <- renderPlotly({
      x_data <- log10((2**df_list[5][[1]][[2]] + 2**df_list[6][[1]][[2]]) / 2)
      y_data <- df_list[5][[1]][[2]] - df_list[6][[1]][[2]]
      plot_ly(x = x_data, y = y_data, type = "scatter", mode = "markers", text = df_list[5][[1]][[1]], hoverinfo = text, source = "sub_gate",
              color = df_list[5][[1]][[3]], size = 1/df_list[5][[1]][[3]]) %>%
        layout(title = paste(colnames(df_list[5][[1]])[2], colnames(df_list[6][[1]])[2], sep = " - "),
          yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    }) 
    
    output$gating_in4 <- renderPlotly({
      x_data <- (2**df_list[7][[1]][[2]] + 2**df_list[8][[1]][[2]]) / 2
      y_data <- df_list[7][[1]][[2]] - df_list[8][[1]][[2]]
      plot_ly(x = x_data, y = y_data, type = "scatter", mode = "markers", text = df_list[7][[1]][[1]], hoverinfo = text, source = "sub_gate",
              color = df_list[7][[1]][[3]], size = 1/df_list[7][[1]][[3]]) %>%
        layout(title = paste(colnames(df_list[7][[1]])[2], colnames(df_list[8][[1]])[2], sep = " - "),
          yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    }) 
    
    output$gating_in5 <- renderPlotly({
      x_data <- (2**df_list[9][[1]][[2]] + 2**df_list[10][[1]][[2]]) / 2
      y_data <- df_list[9][[1]][[2]] - df_list[10][[1]][[2]]
      plot_ly(x = x_data, y = y_data, type = "scatter", mode = "markers", text = df_list[9][[1]][[1]], hoverinfo = text, source = "sub_gate",
              color = df_list[9][[1]][[3]], size = 1/df_list[9][[1]][[3]]) %>%
        layout(title = paste(colnames(df_list[9][[1]])[2], colnames(df_list[10][[1]])[2], sep = " - "),
          yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))
    }) # End of the original maplot output section 
    
    # Gating app subsets output section
    output$gating_out1 <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "sub_gate") 
      selected_index <- selected_data$pointNumber + 1
      df1 <- df_list[1][[1]]
      df2 <- df_list[2][[1]]
      x_data <- log10((2**df1[[2]] + 2**df2[[2]]) / 2)
      y_data <- df1[[2]] - df2[[2]]
      
      df1$color_group <- "not selected"
      df1[which(attr(df1, "row.names") %in% selected_index),]$color_group <- "selected"
      df2$color_group <- "not selected"
      df2[which(attr(df2, "row.names") %in% selected_index),]$color_group <- "selected"
      
      plot_ly(x = x_data, y= y_data, type = "scatter", mode = "markers", text = df1[[1]], hoverinfo = text, source = c("sub_gate"),
                    color = df1$color_group, colors = c("green", 'red')) %>%
                      layout(title = paste(colnames(df1)[2], colnames(df2)[2], sep = " - "),
                         yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))

    })
    
    output$gating_out2 <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "sub_gate") 
      selected_index <- selected_data$pointNumber + 1
      df3 <- df_list[3][[1]]
      df4 <- df_list[4][[1]]
      x_data <- log10((2**df3[[2]] + 2**df4[[2]]) / 2)
      y_data <- df3[[2]] - df4[[2]]
      
      df3$color_group <- "not selected"
      df3[which(attr(df3, "row.names") %in% selected_index),]$color_group <- "selected"
      df4$color_group <- "not selected"
      df4[which(attr(df4, "row.names") %in% selected_index),]$color_group <- "selected"
      
      plot_ly(x = x_data, y= y_data, type = "scatter", mode = "markers", text = df3[[1]], hoverinfo = text, source = c("sub_gate"),
                    color = df3$color_group, colors = c("green", 'red')) %>%
                      layout(title = paste(colnames(df3)[2], colnames(df4)[2], sep = " - "),
                         yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))

    })
    
    output$gating_out3 <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "sub_gate") 
      selected_index <- selected_data$pointNumber + 1
      df5 <- df_list[5][[1]]
      df6 <- df_list[6][[1]]
      x_data <- log10((2**df5[[2]] + 2**df6[[2]]) / 2)
      y_data <- df5[[2]] - df6[[2]]
      
      df5$color_group <- "not selected"
      df5[which(attr(df5, "row.names") %in% selected_index),]$color_group <- "selected"
      df6$color_group <- "not selected"
      df6[which(attr(df6, "row.names") %in% selected_index),]$color_group <- "selected"
      
      plot_ly(x = x_data, y= y_data, type = "scatter", mode = "markers", text = df5[[1]], hoverinfo = text, source = c("sub_gate"),
                    color = df5$color_group, colors = c("green", 'red')) %>%
                      layout(title = paste(colnames(df5)[2], colnames(df6)[2], sep = " - "),
                         yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))

    })
    
    output$gating_out4 <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "sub_gate") 
      selected_index <- selected_data$pointNumber + 1
      df7 <- df_list[7][[1]]
      df8 <- df_list[8][[1]]
      x_data <- (2**df7[[2]] + 2**df8[[2]]) / 2
      y_data <- df7[[2]] - df8[[2]]
      
      df7$color_group <- "not selected"
      df7[which(attr(df7, "row.names") %in% selected_index),]$color_group <- "selected"
      df8$color_group <- "not selected"
      df8[which(attr(df8, "row.names") %in% selected_index),]$color_group <- "selected"
      
      plot_ly(x = x_data, y= y_data, type = "scatter", mode = "markers", text = df7[[1]], hoverinfo = text, source = c("sub_gate"),
                    color = df7$color_group, colors = c("green", 'red')) %>%
                      layout(title = paste(colnames(df7)[2], colnames(df8)[2], sep = " - "),
                         yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))

    })
    
    output$gating_out5 <- renderPlotly({
      selected_data <- event_data("plotly_selected", source = "sub_gate") 
      selected_index <- selected_data$pointNumber + 1
      df9 <- df_list[9][[1]]
      df10 <- df_list[10][[1]]
      x_data <- (2**df9[[2]] + 2**df10[[2]]) / 2
      y_data <- df9[[2]] - df10[[2]]
      
      df9$color_group <- "not selected"
      df9[which(attr(df9, "row.names") %in% selected_index),]$color_group <- "selected"
      df10$color_group <- "not selected"
      df10[which(attr(df10, "row.names") %in% selected_index),]$color_group <- "selected"
      
      plot_ly(x = x_data, y= y_data, type = "scatter", mode = "markers", text = df9[[1]], hoverinfo = text, source = c("sub_gate"),
                    color = df9$color_group, colors = c("green", 'red')) %>%
                      layout(title = paste(colnames(df9)[2], colnames(df10)[2], sep = " - "),
                         yaxis = list(title = "log2 FC"), xaxis = list(title = "AVG"))

    })

  })
  
  # Gating app v2
  # Side panel part
    # fileInput id: first_input_file1, first_input_file2, etc. 
  output$gating_group_first_inputs_v2 <- renderUI({
    num_of_file_input <- as.integer(input$num_of_gating_first_input_v2)
    lapply(c(1:num_of_file_input), function(i) {
      fileInput(paste("first_input_file", as.character(i), sep = ""),
        paste("first input file No.", i, sep = ""))
    })
    
  })
    # fileInput id: second_input_file1, second_input_file2, etc. 
  output$gating_group_second_inputs_v2 <- renderUI({
    num_of_file_input <- as.integer(input$num_of_gating_second_input_v2)
    lapply(c(1:num_of_file_input), function(i) {
      fileInput(paste("second_input_file", as.character(i), sep = ""),
        paste("second input file No.", i, sep = ""))
    })
    
  })
  
  # End of side panel part
  
  num_of_gating_pair <- reactive({
    req(input$num_of_gating_first_input_v2)
    num_of_files <- input$num_of_gating_first_input_v2
    num_of_files
  })
  
  num_of_second_input_files <- reactive({
    req(input$num_of_gating_second_input_v2)
    num_of_files <- input$num_of_gating_second_input_v2
    num_of_files
  })
  
  observeEvent(input$gating_v2, {
    # Dynamically generating plots
    # Set the total number of plots
    num_of_plots <- as.numeric(num_of_gating_pair())
    # Second input doesn't have to be the same number of the first input, because the "pool" situation.
    num_of_second_input <- as.numeric(num_of_second_input_files())
    
    # Generating UIs
    output$gating_out_v2 <- renderUI(
      lapply(c(1: num_of_plots), function(i) {
        fluidRow(
          column(6, plotlyOutput(paste("original_ma", as.character(i), sep = ""), width = 800, height = 600)),
          column(6, plotlyOutput(paste("lasso_out", as.character(i), sep = ""), width = 800, height = 600))
        )
      })
    )
    
    # fisrt data set on the top of MA plot
    dat_sets_first <- lapply(c(1: num_of_plots), function(i){
      # [[4]] is the index for &datapath
      read.csv(file = input[[paste("first_input_file", as.character(i), sep = "")]]$datapath, header = TRUE, sep = ",")
    })

    # second data set on the bottom of MA plot
    if(num_of_second_input > 1) {
      dat_sets_second <- lapply(c(1: num_of_second_input), function(i){
        read.csv(file = input[[paste("second_input_file", as.character(i), sep = "")]]$datapath, header = TRUE, sep = ",")
      })
    }else {
      dat_sets_second <- lapply(c(1: num_of_plots), function(i){
        read.csv(file = input[[paste("second_input_file", as.character(1), sep = "")]]$datapath, header = TRUE, sep = ",")
      })
    }

    print(input$first_input_file1)
    print(input$first_input_file2)
    print(input$second_input_file1)
    print(input$second_input_file2)
    
    # Plotting 
    for (i in c(1:num_of_plots)) {
      output[[paste("original_ma", as.character(i), sep = "")]] <- renderPlotly({
        x_dat <- (2**dat_sets_first[i][[1]][[2]] + 2**dat_sets_second[i][[1]][[2]]) / 2
        y_dat <- dat_sets_first[i][[1]][[2]] - dat_sets_second[i][[1]][[2]]
        # test_dat <- read.csv("data/test/new_norm_test_with_heatmap_dat.csv", sep = ",")
        plot_ly(x = x_dat, y = y_dat, type = "scatter", mode = "markers")   
      })
    }
    
    
    
    
  })
  
  
  
  
  # End of Gating app v2
  
})