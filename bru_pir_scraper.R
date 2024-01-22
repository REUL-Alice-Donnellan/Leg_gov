#  Chris Steinberg 01/12/2024
library(dplyr)
library(glue)
library(stringr)
library(rvest)
library(xml2)
library(purrr)
# Find all = TRUE means search from 1975. If false, then only current year
top_level <- function(find_all = TRUE, year = 1975) {
  
  #If you don't want everything, search from the current year
  if (find_all == FALSE){
    year = lubridate::year(lubridate::today())
  }
  
  urls = search_urls(year)
  
  text_search_results <- urls %>%
    map(., ~ text_searcher(.)) %>%
    bind_rows()
  
  pdf_search_results <- urls %>%
    map(., ~ pdf_searcher(.)) %>%
    bind_rows()
  
  output <-
    output_csv(text_search_results, pdf_search_results)
  
  return(output)
}

# Gather all search result pages
search_urls <- function(year = 1975) {

  #Formats search_string
  text_search <-
    str_replace_all("of the Secretary of State for Transport", " ", "%20")

  # Base Url
  leg_url <- glue('https://www.legislation.gov.uk/all/{year}-*?text={text_search}&page=')
  
  # Lists all pages of search and extracts urls
  urls <- url_getter(leg_url)    
  
  # Removes duplicates (as this returns two urls per url) then cleans to first 4 /
  reduced_urls <- urls %>%
    sub("^((/[^/]+){3})/.*", "\\1", .) %>% 
    unique()
  
  return(reduced_urls)
  
}

url_getter <- function(leg_url) {
  
  # Finds links in page
  urls <- c()
  i <- 1
  #Loops over the url, appending the page until it cannot find the "Next" page button
  while (i > 0) {
    
    leg_full_url <- glue("{leg_url}{i}")
    
    # Finds the urls in the main table
    leg_html <- leg_full_url %>%
      read_html() 
    
    leg_url_tree <-  leg_html %>% 
      html_element("tbody") %>% 
      html_nodes("a") %>%
      html_attr("href")
    
    # Appends relevant global variables
    urls <- c(urls, reduced_urls)
    i <- i + 1
    
    # Checks if there is a next page or not
    if (length(html_elements(leg_html, ".pageLink.next")) == 0){
      break
    } 
  }
  
  return(urls)
  
}

text_searcher <- function(leg_url) {
  #leg_url <- "/uksi/2002/2626"
  
  leg_full_url <-
    glue("https://www.legislation.gov.uk{leg_url}/made")
  
  leg_html <- leg_full_url %>%
    read_html() %>%
    html_element("body") %>%
    html_element("#layout1") %>%
    html_element("#layout2")
  
  # Title
  leg_name <- leg_html %>%
    html_element(".pageTitle") %>%
    html_text()
  
  # Reads plaintext of legislation
  pir_mentions <- find_in_leg_text(leg_html)
  
  cif_date = get_cif_date(leg_html)
  
  output <- tibble(
    "legislation_name" = leg_name,
    "legislation_url" = leg_full_url,
    "cif_date" = cif_date,
    "pir_text" = pir_mentions
  )
  
  return(output)
  
}

get_cif_date <- function(leg_html) {
  #Primary leg format
  primary_leg_cif <- leg_html %>%
    html_elements(".LegDateOfEnactment") %>%
    html_text() %>%
    gsub("[[:punct:]]", "", .)
  
  if (length(primary_leg_cif) > 0) {
    return(tail(primary_leg_cif, 1))
  }
  # Secondary leg format
  second_leg <- leg_html %>%
    html_element(".LegClearFix.LegPrelims") %>%
    html_elements(".LegDate") %>%
    html_children()
  
  # Captures the LegDateText just before the relevant LegDateDate
  date_text <-
    (grepl("Made|Coming into force",  html_text(second_leg)) &
       grepl("LegDateText$", html_attrs(second_leg)))
  # Extracts the child elements AFTER the child elements which is TRUE in date_text
  second_leg_cif <- html_text(second_leg)[which(date_text) + 1] %>% 
    tail(.,1)
  # Returns second element as can contain Made and Coming into Force
  if (length(second_leg_cif) > 0) {
    return(second_leg_cif)
  }
  
  return(NA_character_)
}

find_in_leg_text <- function(leg_html) {
  
  leg_text <- leg_html %>%
    html_element("#content") %>%
    html_text()
  
  # Locates
  pir_search_terms <- c(
    "review of the regulatory provision",
    "Review Regulation",
    "The Secretary of State must from time to time"
  )
  
  # Finds all instances of search terms and turns into list
  pir_mentioned <- str_locate_all(leg_text, pir_search_terms) %>% 
    compact()
  
  if (length(pir_mentioned) == 0){return(NA_character_)}
  
  start_points <- pir_mentioned  %>% 
    map(., ~as.data.frame(.)) %>% 
    bind_rows() %>% 
    .[["start"]] 
  
  relevant_text <- start_points %>%
    map(., ~ substring(leg_text, . - 10, . + 300)) %>%
    paste(., collapse = "\n\n")
  
  return(relevant_text)
  
}

pdf_searcher <- function(leg_url) {
  #leg_url <- "/uksi/2019/453"
  docs_url <- glue("https://www.legislation.gov.uk{leg_url}/resources")
  
  assoc_docs <- docs_url %>%
    read_html() %>%
    html_elements(".assocDocs") %>%
    html_elements("li") %>%
    html_elements("a")
  
  # If no docs, return NAs
  if (length(assoc_docs) < 1) {
    return(tibble("IAs" = NA_character_,
                  "PIRs" = NA_character_))
  } 
  
  relevant_docs <-
    set_names(
      glue(
        "https://www.legislation.gov.uk{xml_attr(assoc_docs, 'href')}"
      ),
      xml_text(assoc_docs)
    )
  
  ias <- find_doc_type(relevant_docs, c("Impact Assessment", "IA"))
  
  pirs <- find_doc_type(
    relevant_docs,
    c(
      "Post Implementation Review",
      "Regulatory Triage Assessment",
      "PIR"
    )
  )
  
  url_links <- tibble("IAs" = ias,
                      "PIRs" = pirs)
  
  return(url_links)
}

find_doc_type <- function(relevant_docs, search_terms){
  
  # Collapses search terms into regex string
  search_term <- glue_collapse(search_terms, sep = "|")
  
  # Filters list of docs
  doc_list <-
    relevant_docs[str_detect(names(relevant_docs), search_term)] %>%
    glue_collapse(., sep = "\n")
  
  if(length(doc_list) == 0){
    return(NA_character_)
  }
  
  return(doc_list)
  
}

output_csv <- function(text_search_results, pdf_search_results) {
  
  output <- bind_cols(text_search_results, pdf_search_results)
  
  output_name <-
    glue("pir-legislation-table-{lubridate::today()}.csv")
  
  readr::write_csv(output,
            output_name)
  
  return(output)
  
}
