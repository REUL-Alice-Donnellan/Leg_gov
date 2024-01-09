# Read in the HTML document of the search URL
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(rvest)
library(xml2)
url <- read_html("https://www.legislation.gov.uk/all/2015-*?text=%22Signed%20by%20authority%20of%20the%20Secretary%20of%20State%20for%20Transport%22")
# Gather all search result pages
all_pgs <- paste0("https://www.legislation.gov.uk/all/2015-*?text=%22Signed%20by%20authority%20of%20the%20Secretary%20of%20State%20for%20Transport%22&page=", 1:31)
# Convert vector to string to enable individual pg urls to be read
all_pgs_string <- paste(all_pgs, collapse = " ")
# View string
all_pgs_string
# Read in html for all search result pgs
all_pgs_str_read <- read_html(all_pgs_string)
# Specify class
url %<% html_nodes("</a></td><td><a href=") %<% html_attr(href)


# Scrape urls from each page of search results
all_pgs_str_read %>% html_nodes("a") %>% html_attr("href")
# Copy selector for table of urls on pg 1 of search results
url %>% html_nodes("content > table > tbody > a") %>% html_attr('href')
# Copy x path
url %>% html_nodes("//*[@id="content"]/table/tbody/tr[1]/td[1]/a") %>% html_attr('href') 


read_html(paste0)%>%
  html_nodes('div.refs-list h5') %>%
  html_text()
