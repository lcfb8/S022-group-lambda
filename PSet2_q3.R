library(tidyverse)
theme_set( theme_minimal() )
knitr::opts_chunk$set(echo = TRUE, 
                      fig.width = 5,
                      fig.height = 3,
                      out.width = "5in", 
                      out.height = "3in", fig.align = "center")

library( rvest )

#Scrape multiple pages

url_state <- "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode=00000000&orgtypecode=0&=00000000&"

url_concordk8 <- "https://profiles.doe.mass.edu/ssdr/default.aspx?orgcode=00670000&orgtypecode=5&=00670000&"

url_carlislek8 <- "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode=00510000&orgtypecode=5&=00510000&"

url_cchs <- "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode=06400000&orgtypecode=5&=06400000&"

# get the pages (scrape)
schools = c( Massachusetts="00000000", 
             Concordk8="00670000", 
             Carlislek8="00510000", 
             CCHS="06400000")
schools

# add in the school names
school.name = tibble( school = names(schools),
                      schoolID = schools )
school.name


# Make our systematic URLs with str_glue from the stringr package

# demo of str_glue:
orgcode = c( "0","5","5","5")
myID = c( "00000000", "00670000","00510000","06400000" )
str_glue( "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode={myID}&orgtypecode={orgcode}&={myID}&" )

url_template = "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode={myID}&orgtypecode={orgcode}&={myID}&"

school_list <- mutate( school.name, 
                       url = str_glue( url_template ) )

school_list

# scrape the html from the page!

get_page_and_sleep = function( url ) {
  
  cat( "Working on: ", url, "\n" )
  
  pg = read_html( url )
  
  # Put in a pause so you don't hammer the website too fast
  Sys.sleep( runif( 1, 1, 2 ) )
  
  pg
}

get_page_and_sleep( school_list$url[[1]] )


# Do the scrape of all our URLs!
school_list = mutate( school_list,
                      data = map( url, get_page_and_sleep ) )

school_list

#put our data in folders

dir.create("data_folder", showWarnings = FALSE )

# Make filenames for each of our web pages.
school_list = mutate( school_list,
                      file_name = str_glue( "data_folder/{school}.xml" ) )

school_list

walk2( school_list$data, school_list$file_name, xml2::write_html )


# Also save our table of pages scraped.
school_list %>% 
  dplyr::select(-data ) %>%
  write_rds( "data_folder/schools_scraped.rds" )


# We next load our raw data and process each saved webpage.

school_list = readRDS( file="data_folder/schools_scraped.rds" )
school_list$url= NULL
school_list


#test with one page

test_page = xml2::read_html( school_list$file_name[[2]] )
test_page

test_page_tables = html_nodes( test_page, "table" )
test_page_tables

html_table( test_page_tables[[4]], fill = TRUE)


# run it on all of our schools

mass = xml2::read_html( school_list$file_name[[1]] )
concordk8 = xml2::read_html( school_list$file_name[[2]] )
carlislek8 = xml2::read_html( school_list$file_name[[3]] )
cchs = xml2::read_html( school_list$file_name[[4]] )

#how do I get the function below to return a table that has run the function but has the original name of school (the variable we input in the function)

#  WE GOT STUCK WRITING THIS FUNCTION BUT WE BRUTE FORCED IT BELOW

extract_table = function( school, x ){
  
  school = xml2::read_html( school_list$file_name[[x]] )
  schools = html_nodes( school, "table" )   
  html_table( schools[[4]], fill = TRUE) 
}


extract_table( mass, 1)

mass


#######


mass = xml2::read_html( school_list$file_name[[1]] )
concordk8 = xml2::read_html( school_list$file_name[[2]] )
carlislek8 = xml2::read_html( school_list$file_name[[3]] )
cchs = xml2::read_html( school_list$file_name[[4]] )


mass = html_nodes( mass, "table" )   
mass = html_table( mass[[4]], fill = TRUE) 

concordk8 = html_nodes( concordk8, "table" )   
concordk8 = html_table( concordk8[[4]], fill = TRUE) 

carlislek8 = html_nodes( carlislek8, "table" )   
carlislek8 = html_table( carlislek8[[4]], fill = TRUE) 

cchs = html_nodes( cchs, "table" )   
cchs = html_table( cchs[[4]], fill = TRUE) 


mass
concordk8
carlislek8
cchs