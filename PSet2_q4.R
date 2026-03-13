
# Clean our raw data to get all test scores
#
# Script by Luke Miratrix adapted to our analysis

library( tidyverse )
library( rvest )
library(xml2)
library(purrr)
library(dplyr)

# We next load our raw data and process each saved webpage.

pages = readRDS( file="pages_scraped.rds" )
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



all_tables <- map(pages$file_name[1:16], all_units)
#asked chatgpt to help me with this, I wasn't using map()


# now we have a list of all of our tables
all_tables

# add the tables to our pages dataframe
pages$tables = all_tables

pages
names(pages)

# add a column where we name our tables

pages <- pages %>%
  mutate( table_name = as.character(str_glue("{unit_name}_{year}")))

pages$table_name


# Before merging, I decided to add a column with unit_id, unit_name, and year 
# inside each table, I was struggling with this part of the code:  
# ~ mutate(.x, unit_id = .y), so I asked chatgpt 

pages <- pages %>%
  mutate(
    tables = map2(tables, unit_id, ~ mutate(.x, unit_id = .y))
  )  %>%
  mutate(
  tables = map2(tables, unit_name, ~ mutate(.x, unit_name = .y))
  )  %>%
  mutate(
    tables = map2(tables, year, ~ mutate(.x, year = .y))
  )

# Before using bind_rows function we were getting an error related to inconsistent
# class types for columns containing the word "Students". So we decided to 
# convert all instances to characters and then convert them back to numbers

pages$tables <- map(
  pages$tables,
  ~ mutate(.x, across(any_of("Students"), as.character))
)

all_units_tbl <- bind_rows(pages$tables)

all_units_tbl <- all_units_tbl %>%
  mutate(across(any_of("Students"), parse_number))


# function to do the cleanup

clean_tables = function( table ) {

  cols_to_remove <- c("% Expulsion", "% Alternate Setting",
                      "% Students with a School-Based Arrest",
                      "% Students with a Non-Arrest Law Enforcement Referral")
  table <- table %>% select(-any_of(cols_to_remove))
  # our original function had all_of instead of any_of and this was causing 
  # errors when binding rows, this was a suggestion from Claude.
  
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
        "Low_SES"     = "Low Income",
        "SWD"         = "Students with Disabilities"
      )
    )
  
  # rename the variables
  table <- table %>%
    rename(any_of(c(
      Group                 = "Student Group",
      Students              = "Students",
      Disciplined           = "Students Disciplined",
      ISS_pct               = "% In-School Suspension",
      OSS_pct               = "% Out-of-School Suspension",
      Emergency_removal_pct = "% Emergency Removal"
    )))
  table
}

# Apply cleaning to each table individually, skipping failures
tables_clean <- clean_tables(all_units_tbl)

class(tables_clean)

dim(tables_clean)

  