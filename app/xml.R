library(xml2)
library(tidyverse)

testxml <- xml2::read_xml(here::here("app", "data", "study-characteristics", "study-characteristics.xml"))
rows <- xml_find_all(testxml, ".//ss:Row")

#Study names
study <- xml_find_all(testxml, ".//ss:Worksheet")
study <- xml_attr(study, "Name")

index_number <- 3
xml_extract <- function(index_number, source_name){
  if(missing(source_name)) {
    new_column <- xml_find_all(rows, glue::glue("//ss:Row[@ss:Index='{index_number}']"))
    xml_text(new_column)
  } else {
  new_column <- xml_find_all(rows, glue::glue("//ss:Row[@ss:Index='{index_number}']"))
  new_column <- xml_text(new_column)
  stringr::str_remove(new_column, source_name)
 }
 }

sponsorship <- xml_extract(3, "Sponsorship source")
country <- xml_extract(4, "Country")
setting <- xml_extract(5, "Setting")
design <- xml_extract(13, "Design")
inclusions <- xml_extract(17, "Inclusion criteria")
exclusions <- xml_extract(18, "Exclusion criteria")
temp_range <- xml_extract(25, "Temperature range (Celsius)")
groups <- xml_extract(21)
age <- xml_extract(22)
female <- xml_extract(23)
male <- xml_extract(24)

# Dataframe
dataframe <- tibble::tibble(study, country, setting, design,
                            inclusions, exclusions, sponsorship)

glimpse(dataframe)

#Resource
#https://stackoverflow.com/questions/42799674/r-and-xml2-how-to-read-text-that-is-not-in-children-nodes-and-read-information
