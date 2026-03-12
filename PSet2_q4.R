
# Clean our raw data to get all test scores
# 
# Script by Luke Miratrix

library( tidyverse )
library( rvest )
library(xml2)
library(purrr)

# We next load our raw data and process each saved webpage.

pages = readRDS( file="pages_scraped.rds" )
pages$url= NULL
pages


test_page = read_html( pages$file_name[[1]] )
test_page

test_page_tables = html_nodes( test_page, "table" )
test_page_tables

html_table( test_page_tables[[4]], fill = TRUE)

all_units <- function(file_name){
  page = read_html( pages$file_name[[1]] )
  
  page_tables = html_nodes( page, "table" )
  
  html_table( page_tables[[4]], fill = TRUE)
}


all_tables <- map(pages$file_name[1:16], all_units) #asked chatgpt to help me with this, I wasn't using map() 


# Should we do the cleaning in this script? What else should we do here? Unnest the tables?








