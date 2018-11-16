#--------- LIBRARIES ------------------------------------------------------------------------------------------
# Load the prereq
packages <- c("XML", "tidyverse", "RCurl", "xtable", "rvest", "stringr", "hms", "lubridate", "tidytext", "qdapTools")

lapply(packages, library, character.only = T)

#--------- FIGURING OUT CODE ON A SMALL SCALE ------------------------------------------------------------------------------------------

# Base url
url <- read_html("https://onlinelibrary.wiley.com/action/doSearch?SeriesKey=15405907&content=articlesChapters&countTerms=true&sortBy=Earliest&target=default&AfterYear=2003&BeforeYear=2018&startPage=0&pageSize=20")

# Get links o other pages
for(i in 1:51){
  url_list <- data.frame(links = NA)
  url_list[i,] <- paste("https://onlinelibrary.wiley.com/action/doSearch?SeriesKey=15405907&content=articlesChapters&countTerms=true&sortBy=Earliest&target=default&AfterYear=2003&BeforeYear=2018&startPage15=&start", "Page=", i, "&pageSize=20", sep = "")
}

# Get the unique html links and create full link version
urls <- url %>% 
  html_nodes("#publication_title") %>% 
  html_attr("href") %>% 
  paste("https://onlinelibrary.wiley.com", ., sep= "")

# Dates
dates <- url %>% 
  html_nodes(".meta__epubDate") %>% 
  html_text() %>% 
  gsub('\\n', "", .)

# Turn into tibble
dates <- as.tibble(dates)

# Separate "first published" form the dates and parse as dates
dates <- dates %>% 
  separate(value, into= c("delete me", "date"), sep = ":", convert = T) %>% 
  select(date) %>% 
  mutate(date = as.Date(date, "%d %B %Y"))

# Function to read in articles
scrape_it <- function(url){
  read_url <- read_html(url)
  read_url %>% 
    html_nodes(".article-section__content+ .article-section__content p") %>% 
    html_text() %>% 
    gsub('\\n', "", .) %>% 
    gsub('\\"', "", .) %>% 
    print()
}

# Create individual text files for each article and write as csv
for(i in 1:length(urls)){
  scraped_urls <- scrape_it(urls[i])
  temp_date <- as.character(dates$date)
  write.csv(scraped_urls, paste("APSA_", temp_date[i], sep = ""))
}


######## GET ALL MAIN LINKS ###################################################################################################################################################################################################
# First Loop
rows = 51
links = 1
urls <- matrix(ncol = links, nrow = rows)
for(i in 1:rows){
  urls[i,] <- paste("https://onlinelibrary.wiley.com/action/doSearch?SeriesKey=15405907&content=articlesChapters&countTerms=true&sortBy=Earliest&target=default&AfterYear=2003&BeforeYear=2018&startPage15=&start", "Page=", 0 + i, "&pageSize=20", sep = "")
}
urls <- as.data.frame(urls)
new_row <- data.frame(V1 = c("https://onlinelibrary.wiley.com/action/doSearch?SeriesKey=15405907&content=articlesChapters&countTerms=true&sortBy=Earliest&target=default&AfterYear=2003&BeforeYear=2018&startPage15=&startPage=0&pageSize=20")) # Add the first missing link
final_urls <- rbind(new_row, urls)

######## DATES FOR ALL ARTICLES ###############################################
# Convert to character
n <- nrow(final_urls)
final_urls2 <- character()

for (i in 1:n) {
  final_urls2[i] <- as.character(final_urls[i,1])
}

# Read all the html links
read_links <- lapply(final_urls2, function(i){
  webpage <- read_html(i)
})

# Create a function to read in the dates
read_in_dates <- function(url) {
  final_dates <- url %>%
    html_nodes(".meta__epubDate") %>%
    html_text() %>%
    gsub('\\n', "", .)
  print(final_dates)
}

# Read in dates using lapply
dates <- lapply(read_links, read_in_dates)

# Turn into data frame
formatted_dates <- data.frame(matrix(unlist(dates), nrow = 1037, byrow = T),stringsAsFactors=FALSE)

# Separate "first published" form the dates and parse as dates
tidy_dates <- formatted_dates %>% 
  separate("matrix.unlist.dates...nrow...1037..byrow...T.", into = c("delete me", "date"), sep = ":", convert = T) %>% 
  select(date) %>% 
  mutate(date = as.Date(date, "%d %B %Y"))

######## ARTICLE NAMES ###############################################
# Function to grab article names
read_art_names <- function(url){
  webpage <- read_html(url)
  webpage %>% 
    html_nodes("#publication_title") %>% 
    html_text()
}

# Use lapply to grab all for every link in "final_urls2"
article_names <- lapply(final_urls2, read_art_names)

# Turn into data frame
formatted_names <- data.frame(matrix(unlist(article_names), nrow = 1037, byrow = T), stringsAsFactors=FALSE)
tidy_names <- formatted_names %>% 
  rename(article = "matrix.unlist.article_names...nrow...1037..byrow...T.")

######## SCRAPE ALL ARTICLES #######################################################################
# Get the unique html link to each article from each main page
find_uniques <- lapply(final_urls2, function(i){
  webpage <- read_html(i)
  webpage %>% 
  html_nodes("#publication_title") %>% 
  html_attr("href") %>% 
  paste("https://onlinelibrary.wiley.com", ., sep= "")
})

# Unlist into a dataframe
uniques <- unlist(find_uniques)
uniques <- as.data.frame(uniques)


# Function to read in articles
scrape_it_big <- function(url){
  read_url <- html_session(url)
  read_url %>% 
    html_nodes(".article-section__content+ .article-section__content p") %>% 
    html_text() %>% 
    gsub('\\n', "", .) %>% 
    gsub('\\"', "", .)
}


# Read in articles and create individual text files for each article and write as csv
chara_uniques <- as.character(uniques[ ,])

for(i in 1:length(chara_uniques)){
  scraped_urls <- scrape_it_big(chara_uniques[i])
  temp_date <- as.character(tidy_dates$date)
  temp_names <- as.character(tidy_names$article)
  write.csv(scraped_urls, paste("APSA_", temp_date[i], ":", temp_names[i], sep = ""))
}

# Reading in the files
files = list.files(pattern = "[APSA]")
fls <- NULL
lns <- NULL
for (file in files) {
  my_lines <- readLines(file)
  print(file)
  for (line in my_lines) {
    fls <- c(fls, file)
    lns <- c(lns, line)
  }
}

### Convert to clean data frame
df <- data.frame(file = fls, text = lns)

new_df <- df %>% 
  separate(file, into = c("article", "date", "delete"), sep = "_") %>% 
  separate(date, into = c("date", "name"), sep = ":") %>% 
  select(name, date, text)

write_csv(new_df, "article_texts.csv")
