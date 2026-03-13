
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


clean_tables = function( table ) {
  # The conversions below (from numeric to character and then character back to 
  # numeric) were suggested by Claude, we were trying to do it outside the 
  # function before binding the rows, but we were unsuccessful
 table <- table %>% mutate(across(everything(), as.character))
  
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
      Emergency_removal_pct = "% Emergency Removal",
      pct_1d                = "% 1 Day",
      pct_2_3d              = "% 2 to 3 Days",
      pct_4_7d              = "% 4 to 7 Days",
      pct_8_10d             = "% 8 to 10 Days",
      pct_over_10d          = "% > 10 Days"
    )))
  # Convert numeric columns back
  table <- table %>% 
    mutate(across(c(Students, Disciplined), as.numeric))
  table
}

# Apply cleaning to each table individually, skipping failures
tables_clean <- map(pages$tables, clean_tables)

class(tables_clean)
# Now bind
combined <- bind_rows(tables_clean)


dim(combined)


combined %>%
  group_by(year) %>%
  filter(unit_id == "00000000") %>%
  summarise(n())


(32 * 4)
  