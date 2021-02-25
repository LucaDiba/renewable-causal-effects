#all the functions for processing the ilo_data
#which dataset I got from ILO might require some investigating
#but I selected the csv with the level 2 classification
#TODO: write these functions so I only have to pass the path
#to the root dir
#below is the way it is called from the caller
#    ilo_toc_country <- 'data/ILO_table_of_contents_ref_area_en.csv'
#    ilo_data <- 'data/ILO_employment_by_activity_gender.csv'
#    ilo_processed_frame <- process_ilo_master(ilo_toc_country,ilo_data)

count_tseries_len <- function(df) {
    #count time series length from years
    return (length(unique(df$time)))
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
