
# Web scraping script: Download and save all the pages we want
#
# Script by Luke Miratrix adapted to our analysis

library( tidyverse )
library( rvest )
library(xml2)


# Define school districts + MA state to answer our research question

# get the pages (scrape)
ma_units = c( Massachusetts="00000000",
              Concordk8="00670000",
              Carlislek8="00510000",
              CCHS="06400000")
ma_units

# we had to add orgcode, due to the Massachusetts state-wide data
# having a unique code, 0.

orgcode = c("0","5","5","5")

# we chose the last 3 years (data was scant during pandemic)
pages = expand_grid( year = 2022:2025,
                     unit_id = ma_units)

pages

# add in the unit names
unit.name = tibble( unit_name = names(ma_units),
                      unit_id = ma_units,
                      orgcode = orgcode)
unit.name
pages = left_join( pages, unit.name, by="unit_id" )

pages


# Make our systematic URLs with str_glue from the stringr package

# URL for the data on discipline
pages_url <- pages %>%
  mutate(discipline = str_glue(
    "https://profiles.doe.mass.edu/ssdr/default.aspx?orgcode={unit_id}&orgtypecode={orgcode}&fycode={year}"))

# URL for the data on days missed related to discipline
pages_url <- pages_url %>%
  mutate(days_missed = str_glue(
    "https://profiles.doe.mass.edu/ssdr/ssdr_days_missed_detail.aspx?orgcode={unit_id}&orgtypecode={orgcode}&fycode={year}"))

pages_url

pages_url <- pages_url %>%
  pivot_longer(discipline:days_missed,
               names_to = "data_type",
               values_to = "url")


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
# (don't run if you've already created the folder!)
dir.create("data_folder", showWarnings = FALSE )

# Make filenames for each of our web pages.
pages = mutate( pages,
                file_name = str_glue(
                  "data_folder/{data_type}_unit_{unit_id}_{year}.xml" ) )


pages

# Now save each page as a separate file.  This is important because
# now we don't have to re-scrape the website each time we want to get
# our raw data.

walk2( pages$data, pages$file_name, write_html )

# NOTE: We might think we could just save the full "pages" table with
# the webpages inside of it, but this does not work.  The rvest
# package does something weird with memory management, and thus when
# you save the pages object, the web pages inside get corrupted.  So
# we have to save them one at a time like we do.  :-(

# Also save our table of pages scraped, removing the heavy data of the table
pages %>%
  dplyr::select(-data ) %>%
  write_rds( "pages_scraped.rds" )

# We are done!  We have stored all the webpages on our own computer as
# separate files.
