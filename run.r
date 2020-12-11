#load the functions for processing all the data
source('scripts/access_processing.r')
source('scripts/electricity_processing.r')
source('scripts/ilo_processing.r')

get_one_country <- function(name, electricity, access) {
    #helper function to get all the data for one country
    #helper function to grab gdp and electricity data
    local_electricity <- electricity[electricity$'Country' == name,]
    local_access <- access[access$'Country' == name,]
    temp <- merge(local_electricity, local_access, by='Year')
    return (temp)
}

get_one_country_gdp <- function(name, electricity, access, gdp) {
    #helper function to get all the data for one country
    #helper function to grab gdp and electricity data
    local_electricity <- electricity[electricity$'Country' == name,]
    local_access <- access[access$'Country' == name,]
    local_gdp <- gdp[gdp$'Country.or.Area' == name,]
    temp <- merge(local_electricity, local_access, by='Year')
    temp <- merge(temp, local_gdp, by='Year')
    return (temp)
}

count_accessibles <- function(access, electricity) {
    tot_inc <- 0
    tot_inc_ren <- 0
    tot_inc_comb <- 0
    countries <- unique(access$'Country')
    for (country in countries) {
        cf <- get_one_country(country, electricity, access)
        if (max(cf$'Access') - min(cf$'Access') >= 20) {
            #count if an increaser
            tot_inc <- tot_inc + 1
            #now see if ren or comb by looking over the years
            ren <- sum(cf$'Total.Renewable' > 0.5*cf$'combustible')
            if (ren > 0.5*length(cf$'Year')) {
                tot_inc_ren <- tot_inc_ren + 1
            } else {
                tot_inc_comb <- tot_inc_comb + 1
            }
        }
    }
    print(paste('Total Increasers: ', tot_inc))
    print(paste('Total Increasers Ren: ', tot_inc_ren))
    print(paste('Total Increasers Comb: ', tot_inc_comb))
}

find_slope <- function(access, electricity, gdp) {
    countries <- unique(access$'Country')
    for (country in countries) {
        cf <- get_one_country_gdp(country, electricity, access, gdp)
        if (max(cf$'Access') - min(cf$'Access') >= 20) {
            #count if an increaser
            #now see if ren or comb by looking over the years
            ren <- sum(cf$'Total.Renewable' > 0.5*cf$'combustible')
            if (ren > 0.5*length(cf$'Year')) {
                print(paste('Country: ', country, ' Renewable'))
            } else {
                print(paste('Country: ', country, ' Combust'))
            }
            print(summary(lm(cf$'Access' ~ cf$'Total.Renewable' + cf$'combustible' + cf$'Value')))
        }
    }
}

deduce_slope <- function(access, electricity, gdp) {
    countries <- unique(access$'Country')
    tabulated_results <- matrix(0, nrow=4, ncol=length(countries), dimnames=list(list('GDPSIG', 'TRSIG', 'REN', 'SLOPE'), countries))
    for (country in countries) {
        cf <- get_one_country_gdp(country, electricity, access, gdp)
        if (max(cf$'Access') - min(cf$'Access') >= 20) {
            #count if an increaser
            #now see if ren or comb by looking over the years
            ren <- sum(cf$'Total.Renewable' > 0.5*cf$'combustible')
            result <- summary(lm(cf$'Access' ~ cf$'Total.Renewable' + cf$'combustible' + cf$'Value'))
            if (ren > 0.5*length(cf$'Year')) {
                #look at just the renewable countries and determine if slope is significant
                if (result$'coefficients'['cf$Total.Renewable', 'Pr(>|t|)'] < 0.05)
                    tabulated_results['TRSIG', country] <- tabulated_results['TRSIG', country]  + 1
                if (result$'coefficients'['cf$Value', 'Pr(>|t|)'] < 0.05)
                    tabulated_results['GDPSIG', country] <- tabulated_results['GDPSIG', country]  + 1
                tabulated_results['REN', country] <- 1
                tabulated_results['SLOPE', country] <- result$'coefficients'['cf$Total.Renewable', 'Estimate'] > 0 
            }
        }
    }
    #extract renewable subset
    subset <- tabulated_results[,tabulated_results['REN',] == 1]
}
main <- function() {
    #read csv files
    access <- as.data.frame(read.csv('data/access_processed.csv'))
    gdp <- as.data.frame(read.csv('data/per_capita_gdp.csv'))
    electricity <- as.data.frame(read.csv('data/electricity_processed.csv'))
    ilo <- as.data.frame(read.csv('data/ilo_processed.csv'))
    browser()

    deduce_slope(access, electricity, gdp)
}

main()
