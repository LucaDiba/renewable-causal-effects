#function to process the electricity data into long format
#this way I can merge with the other data on year and country
#easily TODO: write so I can pass path to root dir, find where
#I got this data

process_access <- function() {
    access <- as.data.frame(read.csv('data/electricity_access.csv'))
    entries <- list()
    i <- 1
    for (country in unique(access$'Country.Name')) {
        country_frame <- access[access$'Country.Name' == country,] #inefficient
        for (year in 1960:2020) {
            y_name <- paste('X', year, sep='')
            data <- country_frame[,y_name]
            if (!is.na(data)) {
                entries[[i]] <- list(Country=country, Year=year, Value=data)
                i <- i + 1
            }
        }
    }
    data <- as.data.frame(matrix(unlist(entries), ncol=3, byrow=TRUE,
        dimnames=list(NULL, c('Country', 'Year', 'Access'))))
    data$'Access' <- as.numeric(as.character((data$'Access')))
    return (data)
}

