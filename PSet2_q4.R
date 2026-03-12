
# Clean our raw data to get all test scores
# 
# Script by Luke Miratrix

library( tidyverse )
library( rvest )

# We next load our raw data and process each saved webpage.

pages = readRDS( file="pages_scraped.rds" )
pages$url= NULL
pages


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

# As we write code, we stash a single page to check our code on.  Once
# we get that working, we will worry about processing all the pages.
test_page = xml2::read_html( pages$file_name[[16]] )
test_page

test_page_tables = html_nodes( test_page, "table" )
test_page_tables



###### From prior class: code for extracting a test-score table  ######

# This takes a raw html table and converts it to one of our test score
# tables, if it fits.  If it doesn't fit, it returns NA.
#
# NOTE: This function returns a dataframe.  This takes a number for
# which html chunk to grab and cleans it up into a table.
single_table_noisy = function( html_chunk ) {
    
    a = html_table( html_chunk, fill=TRUE )
    
    table_name = colnames(a)[[1]]
    
    # Make sure our table has the right number of columns.
    # Stop if not.
    if( ncol(a) != 28 ) {
        return( NA )
    }
    
    # We can set the names of our table by hand
    colnames(a) = c("Student Group", 
                    "Sch-N", "Sch-Rate", 
                    "Sch-A", "Sch-P", "Sch-NI", "Sch-W",
                    "Sch-CPI", "Sch-SGP", "Sch-SGP-N",
                    
                    "Dist-N", "Dist-Rate", 
                    "Dist-A", "Dist-P", "Dist-NI", "Dist-W", 
                    "Dist-CPI", "Dist-SGP", "Dist-SGP-N", 
                    
                    "State-N", "State-Part", 
                    "State-A", "State-P", "State-NI", "State-W", 
                    "State-CPI", "State-SGP", "State-SGP-N")
    
    # Drop the first 4 rows that are header junk
    a = a[ -(1:4), ]
    
    # Drop the "all students" header row
    a = dplyr::filter( a, !( `Student Group` %in% c("","All Students") ) )
    
    a = a %>% mutate_at( .vars = -1, parse_number )
    
    a$name = table_name
    a = relocate( a, name )
    
    return( a ) 
}


# Test the above
if ( FALSE ) {
    
    test_table <- single_table_noisy( test_page_tables[[24]] )
    
    test_table %>%
        dplyr::select(name:`Sch-W`)
    
}

# Check what we have
pages


load_and_clean = function( file_name ) {
    cat( "Cleaning ", file_name, "\n" )
    page = xml2::read_html( file_name )
    get_all_tables( page )
}

# Do the cleaning of all our URLs!
pages = mutate( pages,
                data = map( file_name, load_and_clean ) )
pages

# Wiew!  What a ride!
saveRDS( pages, file="discipline_ma_data.rda" )







