#! /usr/bin/env Rscript

per_country_master_un <- function(files, fun) {
    #for the UN data
    #go through each file, collect the value of the function
    #fun must accept one argument, the dataframe per country
    results <- list()
    for (file in files) {
        df <- as.data.frame(read.csv(file = file))
        countries <- unique(df$'Country.or.Area')
        result = list()
        for (name in countries) {
            country_frame <- df[df$'Country.or.Area' == name,]
            result[[name]] <- fun(country_frame)
        }
        results[[file]] <- result
    }
    return (results)
}

normalize <- function(x) { #quick normalization
    min_x = min(x)
    max_x = max(x)
    return ((x-min_x)/(max_x-min_x))
}

longest_sequence <- function(df) { #find the longest sequence in the data (years)
    #initialize
    years <- unique(df$'Year')
    num_years <- length(years)
    if (num_years < 2) { #check a boundary case quick
        return(min(c(1, num_years)))
    }
    max_seq <- 1; current_seq <- 1
    #loop through data
    for (i in 2:num_years) {
        if ( (years[i] - years[i-1]) > 1) { #sequence gap
            current_seq <- 1
        } else {
            current_seq <- current_seq + 1
        }
        if (current_seq > max_seq) {
            max_seq <- current_seq
        }
    }
    return(max_seq)
}

observe_value <- function(df) { #observe the value field for a country
    plot.default(df$'Year', df$'Value')
}

per_country_master_ilo <- function(ilo_toc_country, ilo_data, fun, return_frame=FALSE) {
    #return frame incase other processing is needed
    #for the labor data from the ILO
    country_ilo <- as.data.frame(read.csv(ilo_toc_country))
    ilo_df <- as.data.frame(read.csv(ilo_data))
    countries <- unique(ilo_df$'ref_area')
    results <- list()
    for (country in countries) {
        country_df = ilo_df[ilo_df$'ref_area' == country,]
        results[[country]] <- fun(country_df)
    }
    if (return_frame) {
        return (list(master_frame=ilo_df, toc=country_ilo, counts=results))
    } else{
        return (results)
    }
}

count_tseries_len <- function(df) {
    #count time series length from years
    return (length(unique(df$time)))
}

find_counts_ilo <- function(ilo_toc_country, ilo_data) {
    #start with the counts
    counts <- per_country_master_ilo(
        ilo_toc_country,
        ilo_data,
        count_tseries_len)
    result <- unlist(counts)
    #show distribution of tseries lengths
    png(
        'figures/total_counts_raw.png',
        width=1024, height=1024,
        bg='transparent')
    hist(result,
        main='ILO Total Years Available by Country',
        xlab='Number of Years')
    dev.off()
    #get top
    sorted_countries <- sort(unlist(counts), decreasing=TRUE)
    print(names(sorted_countries[1:25]))
}

pivot_ilo <- function(df) {
    #assumes we have processed gender Total Employment, and NA's
    #and we have a country frame
    years <- unique(df$'time')
    classif <- unique(df$'classif1')
    num_years <- length(years)
    columns <- length(classif) + 2 # other columns: ref_area, time
    mat <- matrix(
        0, nrow=num_years, ncol=columns,
        dimnames=list(NULL, c('ref_area', 'time', as.character(classif))))
    for (i in 1:num_years) {
        year <- years[i]
        year_df <- df[df$'time' == year,]
        mat[i, 'time'] <- year
        mat[i, 'ref_area'] <- NULL
    }
}

collect_by_classif <- function(df, strings) {
    #helper function for map_to_revised
    #collects everyone in categories defined by regex
    final_sum <- 0
    for (pattern in strings) {
        matched_frame <- df[grepl(pattern, df$'classif1'),]
        final_sum <- final_sum + sum(matched_frame$'obs_value')
    }
    return (final_sum)
}

map_to_revised <- function(df) { #map the categories to my revised columns
    mapping <- list()
    #first check which revision I have
    is_isic3 <- sum(grepl('ISIC3', df$'classif1')) > 0
    if (is_isic3) { #map from isic3 to revised
        mapping[['A']] <- collect_by_classif( #agriculture
            df, c('ISIC3_A', 'ISIC3_B'))
        mapping[['B']] <- collect_by_classif( #mining and quarry
            df, c('ISIC3_C'))
        mapping[['C']] <- collect_by_classif( #manufacturing
            df, c('ISIC3_D'))
        mapping[['D']] <- collect_by_classif( #electricity gas h20
            df, c('ISIC3_E'))
        mapping[['E']] <- collect_by_classif( #construction
            df, c('ISIC3_F'))
        mapping[['F']] <- collect_by_classif( #retail and trade
            df, c('ISIC3_G'))
        mapping[['K']] <- collect_by_classif( #computer and r&d
            df, c('ISIC3_K72', 'ISIC3_K73'))
        mapping[['L']] <- collect_by_classif( #Education
            df, c('ISIC3_M'))
    } else { #map from isic4
        mapping[['A']] <- collect_by_classif( #agriculture
            df, c('ISIC4_A'))
        mapping[['B']] <- collect_by_classif( #mining and quarry
            df, c('ISIC4_B'))
        mapping[['C']] <- collect_by_classif( #manufacturing
            df, c('ISIC4_C'))
        mapping[['D']] <- collect_by_classif( #elect gas h20
            df, c('ISIC4_D', 'ISIC4_E'))
        mapping[['E']] <- collect_by_classif( #construction
            df, c('ISIC4_F'))
        mapping[['F']] <- collect_by_classif( #retail and trade
            df, c('ISIC4_G'))
        mapping[['K']] <- collect_by_classif( #computer and r&d
            df, c('ISIC4_J62', 'ISIC4_J63', 'ISIC4_M72', 'ISIC4_M74'))
        mapping[['L']] <- collect_by_classif( #Education
            df, c('ISIC4_P'))
    }
    return (mapping)
}

process_ilo_master <- function(ilo_toc_country, ilo_data) {
    #master function that pivots the whole table for processing
    #first retrieve the counts so we can determine how many rows
    #there is still some redundancy with per_country but ok for now
    results <- per_country_master_ilo(
        ilo_toc_country,
        ilo_data,
        count_tseries_len,
        return_frame=TRUE)
    result <- unlist(results[['counts']])
    rows <- sum(result) #every row is some year for a country
    #process total frame now
    toc_df <- results[['toc']]
    df <- results[['master_frame']] #master df
    df <- df[df$'sex' == 'SEX_T',] #only grab totals for gender
    df <- df[!grepl('TOTAL', df$'classif1'),] #drop Total Employment
    df[is.na(df$'obs_value'), 'obs_value'] <- 0 #replace NA with 0
    df <- df[,c('ref_area', 'classif1', 'time', 'obs_value')] #select columns
    countries <- unique(df$'ref_area')
    #do column processing
    classif <- unique(df$'classif1')
    column_names <- c(
        'ref_area', 'time', as.character(classif),
        'A', 'B', 'C', 'D', 'E', 'F', 'K', 'L') #my columns
    #proceed to build final frame
    i <- 1 #master index into the rows of new frame
    mat <- as.data.frame(matrix(
        0, nrow=rows, ncol=length(column_names),
        dimnames=list(NULL, column_names)))
    for (country in countries) {
        country_frame <- df[df$'ref_area' == country,]
        for (year in unique(country_frame$'time')) {
            year_frame <- country_frame[country_frame$'time' == year,]
            mat[i, 'ref_area'] <- country
            mat[i, 'time'] <- year
            for (classification in unique(year_frame$'classif1')) {
                #this part might be really inefficient
                #the unique might be unnecessary here but I include for safety
                mat[i, classification] <- year_frame[
                    year_frame$'classif1' == classification, 'obs_value']
            }
            #add the revised columns I manually mapped
            revised_columns <- map_to_revised(year_frame)
            for (column in names(revised_columns)) {
                mat[i, column] <- revised_columns[[column]]
            }
            i <- i + 1
        }
    }
    ilo_df <- as.data.frame(mat)
    return (ilo_df)
}

process_quantity <- function(frame) {
    #helper function for build electricity because quantity field is
    #dirty
    #determine if value or quantity
    if ('Quantity' %in% names(frame)) {
        quantity <- frame$'Quantity'
    } else {
        quantity <- frame$'Value'
    }

    if (length(quantity) > 1) {
        quantity <- max(quantity)
    }
    if (is.na(quantity)) {
        quantity <- 0
    }
    return (quantity)
}

find_good_ratio_countries <- function(dataframe) {
    #find countries with a good renewable ratio
    countries <- unique(dataframe$'Country')
    mat <- as.data.frame(matrix(0,
        nrow=length(countries), ncol=2,
        dimnames=list(NULL, c('Country', 'Ratio'))))
    i <- 1
    for (country in countries) {
        country_frame <- dataframe[dataframe$'Country' == country,]
        ratio <- (country_frame[,'Total.Renewable'] +1E-5)/(country_frame[,'combustible'] + 1E-5) #add a small value for stability
        mat[i, 'Country'] <- country
        mat[i, 'Ratio'] <- mean(ratio)
        i <- i + 1
    }
    browser()
}

build_electricity <- function(countries) {
    #read dataframes
    combustible <- as.data.frame(read.csv('data/electricity_combustible_fuel.csv'))
    solar <- as.data.frame(read.csv('data/electricity_solar.csv'))
    solar_thermal <- as.data.frame(read.csv('data/electricity_solar_thermal.csv'))
    solar_voltaic <- as.data.frame(read.csv('data/electricity_solar_voltaic.csv'))
    hydro <- as.data.frame(read.csv('data/electricity_hydro.csv'))
    wind <- as.data.frame(read.csv('data/electricity_wind.csv'))
    nuclear <- as.data.frame(read.csv('data/electricity_nuclear.csv'))
    imports <- as.data.frame(read.csv('data/electricity_imports.csv'))
    exports <- as.data.frame(read.csv('data/electricity_exports.csv'))
    consumption <- as.data.frame(read.csv('data/electricity_final_consumption.csv'))

    employee_compensation <- as.data.frame(read.csv('data/electricity_employee_compensation.csv'))
    employment <- as.data.frame(read.csv('data/electricity_employment.csv'))
    subsidies <- as.data.frame(read.csv('data/electricity_gov_subsidies.csv'))
    taxes <- as.data.frame(read.csv('data/electricity_taxes_production_imports.csv'))
    geothermal <- as.data.frame(read.csv('data/electricity_geothermal.csv'))

    #build aggregate frame for all energy
    column_names <- c(
        'Country', 'Year',
        'Total Renewable', 'combustible',
        'solar','solar_thermal', 'solar_voltaic',
        'hydro', 'wind', 'geothermal', 'nuclear',
        'imported', 'exported', 'Final Consumption',
        'Employee Compensation', 'Employment', 'Subsidies',
        'Taxes')
    dataframes <- list(
        combustible,
        solar, solar_thermal, solar_voltaic,
        hydro, wind, geothermal, nuclear,
        imports, exports, consumption,
        employee_compensation, employment, subsidies, taxes)
    countries <- vector()
    for (frame in dataframes) { #find the set of all possible country names
        countries <- union(countries, unique(frame$'Country.or.Area'))
    }
    years <- vector()
    for (country in countries) {
        #find the number of rows needed
        for (frame in dataframes) {
            cty_frame <- frame[frame$'Country.or.Area' == country,]
            if (nrow(cty_frame) > 0) {
                cty_years <- unique(cty_frame$'Year')
                years <- union(years, cty_years)
            }
        }
    }
    years <- years[!is.na(years)]
    years <- sort(years)
    electricity_df <- as.data.frame(matrix(
        0, nrow=length(years)*length(countries),
        ncol=length(column_names),
        dimnames=list(NULL, column_names)))
    i <- 1
    #initialize matrix
    for (country in countries) {
        for (year in years) {
            electricity_df[i, 'Country'] <- country
            electricity_df[i, 'Year'] <- year
            i <- i + 1
        }
    }
    #fill in with data
    for (country in countries) {
        for (i in 1:length(dataframes)) {
            var_name <- column_names[[i+3]]
            frame <- dataframes[[i]]
            cty_frame <- frame[frame$'Country.or.Area' == country,]
            for (year in unique(cty_frame$'Year')) {
                yr_cty_frame <- cty_frame[cty_frame$'Year' == year,]
                quantity <- process_quantity(yr_cty_frame)
                select_em <- electricity_df$'Country' == country &
                    electricity_df$'Year' == year
                if (!is.na(sum(select_em))) { #further processing
                    electricity_df[select_em, var_name] <- quantity
                }
            }
        }
    }
    electricity_df[,3] <- rowSums(electricity_df[,c(5, 8, 9, 10)])
    filled_rows <- rowSums(electricity_df[,4:10]) > 0 #bad hack with magic numbers
    electricity_df <- electricity_df[filled_rows,]
    return (electricity_df)
}

get_data <- function(name, electricity, gdp, access) {
    #helper function to grab gdp and electricity data
    local_electricity <- electricity[electricity$'Country' == name,]
    local_gdp <- gdp[gdp$'Country.or.Area' == name,]
    local_access <- access[access$'Country' == name,]
    temp <- merge(local_electricity, local_gdp, by='Year')
    return (temp)
    #return (merge(temp, local_access, by='Year'))
}

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

#run_search <- function(electricity, gdp, access) {
#    pdfpath <- 'result.pdf'
#    pdf(file=pdfpath)
#    for (country in unique(access$'Country')) {
#        data <- get_data(country, electricity, gdp, access)
#        data <- data[order(data$'Year'),]
#        if(nrow(data) > 0) {
#            if(max(data$'Access') - min(data$'Access') > 30) {
#                par(mfrow=c(2, 1))
#                plot(data$'Year', data$'Access', type='l', main=country)
#            }
#        }
#    }
#    dev.off()
#}

count_vars <- function(electricity, gdp, access) {
    y_total <- 0
    x_total_true <- 0
    ratios <- list()
    i <- 1
    for (country in unique(access$'Country')) {
        data <- get_data(country, electricity, gdp, access)
        data <- data[order(data$'Year'),]
        if(nrow(data) > 0) {
            if((max(data$'Access') - min(data$'Access') > 30) & max(data$'Access') > 70) {
                y_total <- y_total +1
                quantity <- sum(data$'Total.Renewable' - data$'combustible' > 0)/nrow(data)
                if(quantity > 0.5) {
                    x_total_true <- x_total_true + 1
                } else {
                    print(max(diff(data$'Access')))
                }
            }
        }
    }
    print(y_total)
    print(x_total_true)
}

count_renewable <- function(electricity, gdp, access) {
    renewable <- 0
    i <- 1
    for (country in unique(access$'Country')) {
        data <- get_data(country, electricity, gdp, access)
        data <- data[order(data$'Year', decreasing=TRUE),]
        example <- data[1,]
        ren <- example['Total.Renewable']
        consumption <- example['Final.Consumption']
        if (!is.na(ren/consumption))
        if ( ren/consumption> 0.5) {
            renewable <- renewable + 1
        }
    }
        print(renewable)
}

count_something <- function(electricity, gdp, access) {
    years <- 1960:2020
    results <- matrix(0, ncol=length(years), nrow=1, dimnames=list(NULL, years))
    for (country in unique(access$'Country')) {
        data <- get_data(country, electricity, gdp, access)
        data <- data[order(data$'Year'),]
        for (year in unique(data$'Year')) {
            year <- as.character(year)
            output <- data[data$'Year' == year, item]
            results[1, year] <- results[1, year] + output
        }
    }
    return (results)
}

main <- function() {
    access <- process_access()
    #un economic data
    gdp <- as.data.frame(read.csv('data/per_capita_gdp.csv'))
    gni <- as.data.frame(read.csv('data/per_capita_gni.csv'))

    #electricity
    #electricity <- build_electricity()
    electricity <- as.data.frame(read.csv('data/electricity_processed.csv'))

    #count_vars(electricity, gdp, access)
    count_renewable(electricity, gdp, access)
    combustible <- count_totals('combustible', electricity, gdp, access)
    solar <- count_totals('solar', electricity, gdp, access)
    hydro <- count_totals('hydro', electricity, gdp, access)

    #ILO data processing section
    ilo_toc_country <- 'data/ILO_table_of_contents_ref_area_en.csv'
    ilo_data <- 'data/ILO_employment_by_activity_gender.csv'
    ilo_processed_frame <- process_ilo_master(ilo_toc_country,ilo_data)
    browser()
#    build_stacked(list(
#        gdp=gdp,
#        electricity=electricity,
#        ilo=ilo_processed_frame))
    #find_good_ratio_countries(electricity)
}

main()



#example plot
#png('my_image.png')
#plot(data)
#dev.off()

#old code
 #   #read gdp
 #   gdp_raw <- read.csv(file = 'data/gdp.csv')
 #   gdp_dataframe <- as.data.frame(gdp_raw)
 #   #read electricity
 #   electricity_raw <- read.csv(file = 'data/electricity.csv')
 #   electricity_dataframe <- as.data.frame(electricity_raw)
 #   #define list of desired countries
 #   countries <- c(
 #       'Spain', 'Italy', 'France', 'Germany',
 #       'Norway', 'Sweden', 'Denmark',
 #       'India', 'China', 'Indonesia'
 #   )
 #   #create graph
 #   par(mfrow=c(5, 2))
 #   for (name in countries) {
 #       #select dataframes
 #       gdp <- gdp_dataframe[
 #           gdp_dataframe$'Country.or.Area' == name,
 #       ]
 #       electricity <- electricity_dataframe[
 #           electricity_dataframe$'Country.or.Area' == name,
 #       ]
 #       #merge on year
 #       merged_dataframe <- merge(gdp, electricity, by='Year')
 #       #normalize some columns
 #       merged_dataframe$ValueNormalized <- normalize(
 #           merged_dataframe$Value
 #       )
 #       merged_dataframe$QuantityNormalized <- normalize(
 #           merged_dataframe$Quantity
 #       )
 #       #plot the data
 #       plot.default(
 #           merged_dataframe$ValueNormalized,
 #           merged_dataframe$QuantityNormalized,
 #           xlab='GDP', ylab='Electricity - Gross Production',
 #           main=name
 #       )
 #   }

