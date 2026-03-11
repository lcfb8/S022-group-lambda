
# Download and save all the pages we want
#
# Script by Luke Miratrix (adapted by the Lambda team (Dylan, Luciana, and Lydia)

library( tidyverse )
library( rvest )
library(xml2)


# Define school districts + MA state to answer our research question

# get the pages (scrape)
schools = c( Massachusetts="00000000", 
             Concordk8="00670000", 
             Carlislek8="00510000", 
             CCHS="06400000")
schools

orgcode = c("0","5","5","5")

# make a list of all the webpages we want to grab
pages = expand_grid( year = 2022:2025, 
                     schoolID = schools)

pages

# add in the school names
school.name = tibble( school = names(schools),
                      schoolID = schools,
                      orgcode = orgcode)
school.name
pages = left_join( pages, school.name, by="schoolID" )

pages


# Make our systematic URLs with str_glue from the stringr package

# we had to add orgcode, due to Massachusetts having a unique code, 0.


pages_url <- pages %>%
  mutate(url = str_glue( "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode={schoolID}&orgtypecode={orgcode}&fycode={year}"))
  
# url_template = "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode={schoolID}&orgtypecode={orgcode}&fycode={year}"



# school_list <- mutate( school.name, 
                       # url = str_glue( url_template ) )

# A helper function to get the pages
get_page_and_sleep = function( url ) {
  
  cat( "Working on: ", url, "\n" )
  
  pg = read_html( url )
  
  # Put in a pause so you don't hammer the website too fast
  Sys.sleep( runif( 1, 1, 2 ) )
  
  pg
}


get_page_and_sleep( pages_url$url[[1]] )


# Do the scrape of all our URLs!
pages = mutate( pages_url,
                data = map( url, get_page_and_sleep ) )
names(pages)


#### Saving our results ####

# Now we have to save the html to a file for safekeeping

# Make a folder to hold all our results
dir.create("data_folder", showWarnings = FALSE )

# Make filenames for each of our web pages.
pages = mutate( pages,
                file_name = str_glue( "data_folder/school_{schoolID}_{year}.xml" ) )

pages

# pages = mutate( pages,
#               file_name = str_glue( "school_data_{schoolID}_{year}.xml" ) )


# Now save each page as a separate file.  This is important because
# now we don't have to re-scrape the website each time we want to get
# our raw data.

html <- walk2( pages$data, pages$file_name, write_html )

?walk2

?write_html
?map
# NOTE: We might think we could just save the full "pages" table with
# the webpages inside of it, but this does not work.  The rvest
# package does something weird with memory management, and thus when
# you save the pages object, the web pages inside get corrupted.  So
# we have to save them one at a time like we do.  :-(

# Also save our table of pages scraped.
pages %>% 
  dplyr::select(-data ) %>%
  write_rds( "pages_scraped.rds" )


# We are done!  We have stored all the webpages on our own computer as
# separate files.



###### This is where I stopped, I still need to generate the tables Luciana #####

#test with one page

test_page = read_html( pages$file_name[[2]] )
test_page

test_page_tables = html_nodes( pages, "table" )
test_page_tables

html_table( test_page_tables[[4]], fill = TRUE)


# run it on all of our schools

mass = read_html( pages$file_name[[1]] )
concordk8 = xml2::read_html( pages$file_name[[2]] )
carlislek8 = xml2::read_html( pages$file_name[[3]] )
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