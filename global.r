library(shiny)

#List of plotlyOutput for gating app using dynamic rendering.
gating_ui_list <- c("gating_out0" = "0", "gating_out1" = "1", "gating_out2" = "2",
                    "gating_out3" = "3", "gating_out5" = "5", "gating_out6" = "6")


#concatenate all the columns(except the index column) from multiple df into one df
df_concat <- function(df_list) {
  df <- df_list[[1]]
  for (i in 2:length(df_list)) {
    df <- merge(df, df_list[[i]], sort = FALSE)
  }  
  return(df) 
}

normalization <- function(column_data) {
  return(log2((column_data + 0.5) / (sum(column_data) + 1) * 1000000))
}

# select a range of data points across all the columns and merge them together, NA will be shown while data is not in the range
range_select <- function(flist, low, high) {
  df <- flist[[1]][which(flist[[1]][2] >= low & flist[[1]][2] <= high),]
  for (i in 2:length(flist)) {
    df <- merge(df, flist[[i]][which(flist[[i]][2] >= low & flist[[i]][2] <= high),], by="barcode_index", all = TRUE, sort = FALSE) 
  }
  return(df)
}

#normalize a list of df
df_list_normalization <- function(df_list) {
  for (i in 1:length(df_list)) {
   # df_list[[i]][2] <- log2((df_list[[i]][2] + 0.5) / (sum(df_list[[i]][2]) + 1) * 1000000)
    df_list[[i]][2] <- normalization(df_list[[i]][2])
  }
  return(df_list)
}

# convert decimal to binary, return a list of 0 or 1
dec2bin <- function(dec_num) {
  if(dec_num == 0) {
    return(c(0))
  }
  bin_list_raw <- c(as.integer(intToBits(dec_num)))
  bin_list_len <- length(bin_list_raw)
  bin_list <- c()
  for (i in bin_list_len:1) {
    if(bin_list_raw[i] == 1 && is.null(bin_list) == TRUE) {
      bin_list <- c(bin_list, bin_list_raw[i])
    }
    else if(is.null((bin_list)) == FALSE) {
      bin_list <- c(bin_list, bin_list_raw[i])
    }
  }
  
  return(bin_list)
}

# Convert decimal to a binary matrix. dec_num is going to be the actual size - 1. e.g. a lib size of 1024 is going to be 1023
bin_matrix <- function(dec_num) {
  #Initialize a matrix by assigning a vector with mode="list", length = dec_num + 1, because it starts at 0
  bi_matrix <- vector(mode = "list", length = dec_num + 1)
  bin_length <- length(dec2bin(dec_num))
  names(bi_matrix) <- c(0:dec_num)
  # Function below fills out all the left 0s.
  bi_matrix <- lapply(c(0:dec_num), function(x) {
    if (length(dec2bin(x)) < bin_length) {
      bin_list <- dec2bin(x)
      bin_list_len <- length(bin_list)
      for (i in 1:(bin_length - bin_list_len)) {
        bin_list <- c(0, bin_list)
      }
      bi_matrix[[x+1]] <- bin_list
    }
    else {
      bi_matrix[[x+1]] <- dec2bin(x)}
    }
  )
  bi_matrix_df <- data.frame(t(sapply(bi_matrix, c)))
  # keep in mind, the binary digits's order is read in a reversed to the seq positions, e.g. p4p3p2p1 is binary form p1p2p3p4
  bi_matrix_df_titles <- lapply(c(1:ncol(bi_matrix_df)), function(x) {paste("P", x, sep = "")} )
  names(bi_matrix_df) <- bi_matrix_df_titles
  return(bi_matrix_df) 
}


# A list of Anc libraries with name:length information
# The lengthes are total length - 1, because they start at 0
library_list <- c(2047, 32767, 7, 255, 255, 15, 1023, 127, 511, 1023)
names(library_list) <- c("Anc80", "Anc81", "Anc82", "Anc83","Anc83_1", "Anc84", "Anc110", "Anc113", "Anc126", "Anc127")

position_number_list <- c(11, 15, 3, 8, 8, 4, 10, 7, 9, 10)
names(position_number_list) <- c("Anc80", "Anc81", "Anc82", "Anc83","Anc83_1", "Anc84", "Anc110", "Anc113", "Anc126", "Anc127")

javascript_highlight_3d <- "
  function(el, x) {
    el.on('plotly_click', function(data) {
    
    var highlight_trace = el.data.length - 1;
    //the coordinates of the point which was clicked on
    //is found in data
    var newPoint = {x: data.points[0].x,
                    y: data.points[0].y,
                    z: data.points[0].z
                   };
    
    //update the plot data and redraw it
    if (el.data[highlight_trace].x[0] != newPoint.x ||
        el.data[highlight_trace].y[0] != newPoint.y ||
        el.data[highlight_trace].z[0] != newPoint.z) {
          el.data[highlight_trace].x[0] = newPoint.x;
          el.data[highlight_trace].y[0] = newPoint.y      
          el.data[highlight_trace].z[0] = newPoint.z
          Plotly.redraw(el);
        }
    })
  }
"

anc80_pos_toggle = c("P1_0", "P1_1", "P2_0", "P2_1", "P3_0", "P3_1", "P4_0", "P4_1", "P5_0", "P5_1", 
              "P6_0", "P6_1", "P7_0", "P7_1", "P8_0", "P8_1", "P9_0", "P9_1", "P10_0", "P10_1", "P11_0", "P11_1")

anc80_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11")
anc81_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15")
anc82_pos = c("P1", "P2", "P3")
anc83_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")
anc84_pos = c("P1", "P2", "P3", "P4")
anc110_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")
anc113_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7")
anc126_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
anc127_pos = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")

anc80_bin_matrix_df = read.csv("anc_bin_lib/anc80_bin_matrix.csv", sep = ",")
anc81_bin_matrix_df = read.csv("anc_bin_lib/anc81_bin_matrix.csv", sep = ",")
anc82_bin_matrix_df = read.csv("anc_bin_lib/anc82_bin_matrix.csv", sep = ",")
anc83_bin_matrix_df = read.csv("anc_bin_lib/anc83_bin_matrix.csv", sep = ",")
anc84_bin_matrix_df = read.csv("anc_bin_lib/anc84_bin_matrix.csv", sep = ",")
anc110_bin_matrix_df = read.csv("anc_bin_lib/anc110_bin_matrix.csv", sep = ",")
anc113_bin_matrix_df = read.csv("anc_bin_lib/anc113_bin_matrix.csv", sep = ",")
anc126_bin_matrix_df = read.csv("anc_bin_lib/anc126_bin_matrix.csv", sep = ",")
anc127_bin_matrix_df = read.csv("anc_bin_lib/anc127_bin_matrix.csv", sep = ",")

