# functions to standardize the datasets for plotting 

prepare_plot_data <- function(data) {
  
  if ("Category" %in% names(data)) {
    group <- data$Category
    value <- data$x
  } else if ("Race" %in% names(data)){
    group <- data$Race
    value <- data$x
  } else if ("Education Level" %in% names(data)) {
    group <- data$`Education Level`
    
    if ("Net Worth" %in% names(data)) {
      value <- data$`Net Worth`
      
    } else if ("Retirement Account Value" %in% names(data)) {
      value <- data$`Retirement Account Value`
      
    } else if ("Financial Assets" %in% names(data)) {
      value <- data$`Financial Assets`
      
    } else if ("Liabilities" %in% names(data)) {
      value <- data$Liabilities
      
    } else if ("Debt" %in% names(data)) {
      value <- data$Debt
    } else {
      stop("Unrecognized education value column")
    }
  
} else {
  stop("Unrecognized dataset format")
}
  
  plot_data <- data.frame(
    Year = as.numeric(data$Year),
    Group = as.character(group),
    Value = as.numeric(value)
  )
  
  return(plot_data)
}


#function to create a line plot 

plot_group_trend <- function(data, plot_title, y_label, group_label = "Group") {
  ggplot(data, aes(x = Year, y = Value, color = Group)) + geom_line(linewidth = 1) + labs(title = plot_title, x = "Year", y = y_label, color = group_label)
}


#function to create a box plot 

plot_group_boxplot <- function(data, plot_title, y_label, group_label = "Group") {
  ggplot(data, aes(x = Group, y = Value, fill = Group)) + geom_boxplot() + labs(title = plot_title, x = group_label, y = y_label)
}

