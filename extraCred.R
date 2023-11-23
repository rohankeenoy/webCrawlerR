library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)

#user input for items
userInput = function() {
  
  itemNum = as.integer(readline(prompt = "How many stocks to compare? (1-6 only): "))
  
  #Validate input
  while (!(is.numeric(itemNum) && itemNum >= 1 && itemNum <= 6)) {
    cat("Invalid input. Please enter a number between 1 and 6.\n")
    itemNum <- as.integer(readline(prompt = "How many stocks to compare? (1-6 only): "))
  }
  
  items <- character(itemNum)  #initialize a vector
  
  # loop through items to get the amount of stock to compare
  for (i in 1:itemNum) {
    items[i] <- readline(prompt = paste("Enter stock", i, "name: "))
  }
  
  # Get user input for date and validate
  dateInput = readline(prompt = "What is the date you would like to search back to? (YYYY/MM/DD): ")
  while (!grepl("^\\d{4}/\\d{2}/\\d{2}$", dateInput)) {
    cat("Invalid date format. Please enter a date in the format YYYY/MM/DD.\n")
    dateInput <- readline(prompt = "What is the date you would like to search back to? (YYYY/MM/DD): ")
  }
  
  #Convert date 
  timeSet = as.Date(dateInput, format = "%Y/%m/%d")
  
  return(list(items = items, timeSet = timeSet))
}
#scrape financial data from Yahoo Finance
scrapper = function(item, timeSet) {
  base_url = "https://finance.yahoo.com/quote/"
  data_list = list()
  data = data.frame()
  
  for (i in 1:length(item)) {
    #cannot just use numeric Dates, had to use POSIXct 
    start_date = as.numeric(as.POSIXct(timeSet))
    end_date = as.numeric(as.POSIXct(Sys.Date()))
    
    #create the url call
    url = paste0(base_url, item[i], "/history?period1=", start_date, "&period2=", end_date, "&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true")
    print(url)
    #html data from the loaded page
    page = read_html(url)
    
    #extract and clean data
    table_data = page %>%
      html_nodes("table") %>%
      html_table(header = TRUE, fill = TRUE)
    
    #make sure the table isn't empty
    if (length(table_data) > 0) {
      #Clean the data by selecting only the required columns
      table_data_clean = table_data[[1]] %>%
        filter(!grepl("\\*", Date)) %>%
        select(Date, Open, High, Low, `Close*`, `Adj Close**`, Volume)
      
      # Store the cleaned data frame in the list
      data_list[[item[i]]] = table_data_clean
      #Append table
      data = bind_rows(data, table_data_clean)
    } else {
      cat("Table not found for", item[i], "\n")
    }
  }
  
  return(data_list)
}

# Get user input
item = userInput()
print(item)

# Scrape data
scrappedData = scrapper(item$items, item$timeSet)
df = data.frame(scrappedData)
print(df)

print(getwd())




