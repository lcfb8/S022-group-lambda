
# Clean our raw data to get all test scores
# 
# Script by Luke Miratrix

library( tidyverse )
library( rvest )

# We next load our raw data and process each saved webpage.

pages = readRDS( file="pages_scraped.rds" )
pages$url= NULL
pages

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




##### Dealing with warnings that we know will happen ######


# Convert all our columns (except the first one) to numbers
my_parse_number = function( strings ) {
    quiet_parse_number = quietly( parse_number )
    res = quiet_parse_number( strings )
    
    # Converting to as.numeric drops all the stored problems and just
    # gives us the numbers.
    as.numeric( res$result )
}


if ( FALSE ) {
    
    nums = c( "afsdf 444", "asadffd", "$433" )
    resA <- parse_number( nums )  
    resA
    attributes(resA)
    
    res = my_parse_number( nums )
    res
    attributes(res)
    
}




# Updated version of the above single_table_noisy() to make it quiet.
single_table = function( html_chunk ) {
    
    a = html_table( html_chunk, fill=TRUE )
    
    table_name = colnames(a)[[1]]
    
    
    # Make sure our table has the right number of columns.
    # Stop if not.
    if( ncol(a) != 28 ) {
        return( NA )
    }
    
    # If we can't figure out what table it is, just drop it
    if ( table_name == "" ) {
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
    a
    
    a = a %>% mutate_at( .vars = -1, my_parse_number )
    
    a$name = table_name
    a = relocate( a, name )
    return( a ) 
}



## Testing code
if ( FALSE ) {
    
    tb = single_table_noisy( test_page_tables[[26]] )
    tb
    
    tb = single_table( test_page_tables[[26]] )
    tb
    
    table( tb$`Student Group`)
    y2015 = filter( tb, `Student Group` == "2015" )
    y2015
    y2015$name
    
}





##### Reusing our function to get all the tables for a single webpage #####


cat( "There are", length(test_page_tables), "possible tables\n" )

# Things might go poorly, so we can capture errors to make sure we don't crash
single_table_maybe = possibly( single_table, otherwise=NA )

tables = map( test_page_tables, single_table_maybe )

is.na( tables )

tables = tables[ !is.na( tables ) ]
cat( "found", length( tables ), "tables\n" )

# put into one giant dataframe
tables = bind_rows( tables )

tables

table( tables$name )



table( tables$`Student Group` )

y2014 = filter( tables, `Student Group` == "2014" )
y2014 %>% dplyr::select( name, `Sch-N`:`Sch-SGP` )



##### Make a function to get all the tables for single webpage #####

# We wrap the above code to make a useful tool!

# Get all the testing tables from the given URL
get_all_tables = function( the_page ) {
    
    chunks = html_nodes( the_page, "table" )
    cat( "There are", length(chunks), "possible tables\n" )
    
    # This is an extra check so we don't waste time on confusing web pages
    if  ( length( chunks ) < 27 ) {
        cat( "Failed to get webpage with expected number of tables\n" )
        return( data.frame() )
    } else {
        
        # Things might go poorly, so we can capture errors to make sure we don't crash
        single_table_maybe = possibly( single_table, otherwise=NA )
        
        tables = map( chunks, single_table_maybe )
        tables = tables[ !is.na( tables ) ]
        cat( "found", length( tables ), "tables\n" )
        
        # put into one giant dataframe
        tables = do.call(bind_rows, tables )
        
        tables
    }
    
}



if ( FALSE ) {
    # more testing
    
    tbles = get_all_tables( test_page )
    
    table( tbles$name )
    
}




##### Now we want to clean all our saved pages  ####


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
saveRDS( pages, file="somerville_school_data.rda" )







