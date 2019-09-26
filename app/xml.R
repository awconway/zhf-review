library(xml2)
library(tidyverse)

testxml <- xml2::read_xml(here::here("app", "data", "study-characteristics.xml"))

#Study names
study <- xml_find_all(testxml, ".//ss:Worksheet")
study <- xml_attr(study, "Name")

#Inclusion criteria
rows <- xml_find_all(testxml, ".//ss:Row")
inclusions <- xml_find_all(rows, "//ss:Row[@ss:Index='17']")
inclusions <- xml_text(inclusions)
inclusions <- stringr::str_remove(inclusions, "Inclusion criteria")
# Dataframe
datadrame <- tibble::tibble(study, inclusions)

#Resource
#https://stackoverflow.com/questions/42799674/r-and-xml2-how-to-read-text-that-is-not-in-children-nodes-and-read-information
