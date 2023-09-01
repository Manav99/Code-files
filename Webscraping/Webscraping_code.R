                        ###### WEB SCRAPING R CODE ######

# This script scrapes information about pharmacies from the "medicineindia.org" website. It is a working sample on how to scrape any website for information.
# It constructs URLs for the FIRST FEW pharmacies, then scrapes details like name, address, city, postal code, and telephone for the first 10 of them.
# The scraped data is then saved to a CSV file named "Scraped_Data.csv".

# Create a sequence of numbers from 1 to 100
num2 <- 1:10

# Construct URLs using the sequence of numbers
url2 <- paste0("https://www.medicineindia.org/pharmacy-chemist-drugstore-details/", num2, "/", "a")
# Convert the vector of URLs to a data frame
url2 <- as.data.frame(url2)
# Rename the column of the data frame to "pages"
names(url2)[1] <- "pages"
# Convert the pages column to character type
url2$pages <- as.character(url2$pages)
# Display the first URL (for checking)
url2[1,1]

# Initialize an empty variable to store the scraped pharmacy information
pharma_info <- NULL

# Measure the time taken to scrape data from the first 10 URLs
system.time(
  for(i in 1:10) {
    # Read the HTML content of the URL
    doc <- read_html(url2[i,])
    
    # Extract the name of the pharmacy and clean the text
    name <- html_nodes(doc, 'dd[itemprop="name"]') %>% html_text() %>% gsub("\n", "", .) %>% trim()
    
    # Extract the street address of the pharmacy and clean the text
    streetAddress <- html_nodes(doc, 'dd[itemprop="streetAddress"]') %>% html_text() %>% gsub("\n", "", .) %>% trim()
    
    # Extract the city of the pharmacy and clean the text
    City <- html_nodes(doc, 'dd[itemprop="addressLocality"]') %>% html_text() %>% gsub("\n", "", .) %>% trim()
    
    # Extract the postal code of the pharmacy and clean the text
    postalCode <- html_nodes(doc, 'dd[itemprop="postalCode"]') %>% html_text() %>% gsub("\n", "", .) %>% trim()
    
    # Extract the telephone number of the pharmacy and clean the text
    telephone <- html_nodes(doc, 'dd[itemprop="telephone"]') %>% html_text() %>% gsub("\n", "", .) %>% trim()
    
    # Combine the extracted details and append to the pharma_info variable
    pharma_info <- rbind(pharma_info, cbind(name, streetAddress, City, postalCode, telephone))
  }
)

# Convert the pharma_info variable to a data frame
pharma_info <- data.frame(pharma_info)

# Save the scraped data to a CSV file
write.csv(pharma_info, "Scraped_Data.csv")
