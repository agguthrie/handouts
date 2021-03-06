## Web Scraping

library(httr)

response <- GET('http://research.jisao.washington.edu/pdo/PDO.latest')
response

library(rvest) 

pdo_doc <- read_html(response)
pdo_doc

pdo_node <- html_node(pdo_doc, "p")
pdo_text <- html_text(pdo_node)

library(stringr)
pdo_text_2017 <- str_match(pdo_text, "(?<=2017).*.(?=\\n2018)")

str_extract_all(pdo_text_2017[1], "[0-9-.]+")

## HTML Tables

census_vars_doc <- read_html('https://api.census.gov/data/2017/acs/acs5/variables.html')

table_raw <- html_node(census_vars_doc, "table")

census_vars <- html_table(table_raw, fill = TRUE) 

library(tidyverse)

census_vars %>%
  set_tidy_names() %>%
  select(Name, Label) %>%
  filter(grepl('Median household income', Label))

## Web Services
#API - application programing interface --> intended for programmers to get dat systematically 

path <- 'https://api.census.gov/data/2018/acs/acs5'
query_params <- list('get' = 'NAME,B19013_001E', 
                     'for' = 'county:*',
                     'in' = 'state:24')

response = GET(path, query =query_params)
response

response$headers['content-type']

## Response Content

library(jsonlite)

county_income <- response %>%
  content(as = 'text') %>%
  fromJSON()

head(county_income)
## Specialized Packages

library(tidycensus)
source(census_api_key.R)

variables <- c('NAME', 'B19013_001E')

county_income <- get_acs(geography = 'county',
                         variables = variables,
                         state = "MD",
                         year = 2018,
                         geometry = TRUE)

ggplot(...) + 
  geom_sf(aes(fill = ...), color = NA) + 
  coord_sf() + 
  theme_minimal() + 
  scale_fill_viridis_c()

## Paging & Stashing

api <- 'https://api.nal.usda.gov/fdc/v1/'
path <- ...

query_params <- list('api_key' = Sys.getenv('DATAGOV_KEY'),
                     'query' = ...)

doc <- GET(paste0(..., ...), query = query_params) %>%
  ...(as = 'parsed')

nutrients <- map_dfr(fruit$foodNutrients, 
                     ~ data.frame(name = .$nutrientName, 
                                  value = .$value))

library(DBI) 
library(RSQLite)

fruit_db <- ...(...(), 'fruits.sqlite') 

query_params$pageSize <- ...

for (i in 1:10) {
  # Advance page and query
  query_params$pageNumber <- ...
  response <- GET(paste0(api, path), query = query_params) 
  page <- content(response, as = 'parsed')

  # Convert nested list to data frame
  values <- tibble(food = page$foods) %>%
    unnest_wider(food) %>%
    unnest_longer(foodNutrients) %>%
    unnest_wider(foodNutrients) %>%
    filter(grepl('Sugars, total', nutrientName)) %>%
    select(fdcId, description, value) %>%
    setNames(c('foodID', 'name', 'sugar'))
  
  # Stash in database
  dbWriteTable(fruit_db, name = 'Food', value = values, append = TRUE)
  
}

fruit_sugar_content <- ...(fruit_db, name = 'Food')

dbDisconnect(...)
