
# Clean our raw data to get all test scores
#
# Script by Luke Miratrix

library( tidyverse )
library( rvest )
library(xml2)
library(purrr)

# We next load our raw data and process each saved webpage.

pages = readRDS( file=".gitignore/pages_scraped.rds" )
pages$url= NULL
pages


test_page = read_html( pages$file_name[[1]] )
test_page

test_page_tables = html_nodes( test_page, "table" )
test_page_tables

html_table( test_page_tables[[4]], fill = TRUE)

all_units <- function(file_name) {
  page = read_html( file_name )

  page_tables = html_nodes( page, "table" )

  html_table( page_tables[[4]], fill = TRUE)

}

test = all_units(pages$file_name[[3]])

test



all_tables <- map(pages$file_name[1:32], all_units)
#asked chatgpt to help me with this, I wasn't using map()


# now we have a list of all of our tables
all_tables

# add the tables to our pages dataframe
pages$tables = all_tables

pages

# add a column where we name our tables

pages <- pages %>%
  mutate( table_name = as.character(str_glue("{unit_name}_{data_type}_{year}")))

pages$table_name

# LB question: I'm just hoping the order of the tables in our list was
# the same as the order of the files in data_folder. If not, the names
# won't match the tables. We can cross-check with the website...


# Formatting/cleaning for all tables in the list:
# remove variables that include all 0s, clean up variable names

# THIS DOESN'T WORK, WE NEED TO MERGE TABLES FIRST

clean_tables = function( table ) {

  cols_to_remove <- c("% Expulsion", "% Alternate Setting",
                      "% Students with a School-Based Arrest",
                      "% Students with a Non-Arrest Law Enforcement Referral")
  table <- table %>% select(-all_of(cols_to_remove))

  table <- table %>%
    mutate(
      `Student Group` = fct_recode(
        `Student Group`,
        "AIAN"        = "American Indian or Alaska Native",
        "Black"       = "Black or African American",
        "Hispanic"    = "Hispanic or Latino",
        "Multiracial" = "Multi-Race, Not Hispanic or Latino",
        "NHPI"        = "Native Hawaiian or Other Pacific Islander",
        "High_Needs"  = "High Needs",
        "EL"          = "English Learners",
        "Low_SES"  = "Low Income",
        "SWD"         = "Students with Disabilities"
      )
    )

  # rename the variables
  table <- table %>%
    rename(
      Group                 = "Student Group",
      Students              = "Students",
      Disciplined           = "Students Disciplined",
      ISS_pct               = "% In-School Suspension",
      OSS_pct               = "% Out-of-School Suspension",
      Emergency_removal_pct = "% Emergency Removal",
      pct_1d                = "% 1 Day",
      pct_2_3d              = "% 2 to 3 Days",
      pct_4_7d              = "% 4 to 7 Days",
      pct_8_10d             = "% 8 to 10 Days",
      pct_over_10d          = "% > 10 Days"
    )
  table
}

# THIS DOES NOT WORK BECAUSE SOME OF THE TABLES ARE DISCIPLINE AND
# SOME ARE DAYS MISSED AND THEY DON'T HAVE THE SAME VARIABLES
tables_clean = map( all_tables, clean_tables  )





