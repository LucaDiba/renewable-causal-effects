#all the functions for processing the electricity data
#it comes from numerous csv files, find the details in
#build_electricity TODO: create nice list here and write
#build_electricity so it does not hard code the paths.

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

build_electricity <- function(countries) {
    #read dataframes
    combustible <- as.data.frame(read.csv('data/electricity_combustible_fuel.csv'))
    solar <- as.data.frame(read.csv('data/electricity_solar.csv'))
    solar_thermal <- as.data.frame(read.csv('data/electricity_solar_thermal.csv'))
    solar_voltaic <- as.data.frame(read.csv('data/electricity_solar_voltaic.csv'))
    hydro <- as.data.frame(read.csv('data/electricity_hydro.csv'))
    wind <- as.data.frame(read.csv('data/electricity_wind.csv'))
    geothermal <- as.data.frame(read.csv('data/electricity_geothermal.csv'))
    nuclear <- as.data.frame(read.csv('data/electricity_nuclear.csv'))
    imports <- as.data.frame(read.csv('data/electricity_imports.csv'))
    exports <- as.data.frame(read.csv('data/electricity_exports.csv'))
    consumption <- as.data.frame(read.csv('data/electricity_final_consumption.csv'))

    #unfortunately it seems there is not much data here .. could be how I merge
    employee_compensation <- as.data.frame(read.csv('data/electricity_employee_compensation.csv'))
    employment <- as.data.frame(read.csv('data/electricity_employment.csv'))
    subsidies <- as.data.frame(read.csv('data/electricity_gov_subsidies.csv'))
    taxes <- as.data.frame(read.csv('data/electricity_taxes_production_imports.csv'))

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
