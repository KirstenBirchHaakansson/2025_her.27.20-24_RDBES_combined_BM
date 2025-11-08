
library(icesTAF)

getwd()
# taf.skeleton()

# Make yearly folders for historical data


# draft.data(data.files = "preliminary_catch_statistics",
#            data.scripts = NULL,
#            originator = "ICES",
#            title = "Preliminary catch statistic from ICES",
#            file = T,
#            append = F)

draft.data(data.files = "historical_data",
           data.scripts = NULL,
           originator = "Data from previous AWG's",
           title = "Data from previous AWG's",
           period = "2000-2015",
           access = "Restricted",
           file = T,
           append = F)

draft.data(data.files = "data_from_recent_years",
           data.scripts = NULL,
           originator = "Data from previous AWG's",
           title = "Data from previous AWG's",
           period = "2016-2024",
           access = "Restricted",
           file = T,
           append = T)

draft.data(data.files = "updated_data_from_sweden",
           data.scripts = NULL,
           originator = "Data from previous AWG's",
           title = "Data from previous AWG's",
           period = "2016-2024",
           access = "Restricted",
           file = T,
           append = T)

draft.data(data.files = "download_from_stockassessment_org_multi_fleet",
           data.scripts = NULL,
           originator = "Downloaded from SAM, Marts 2025",
           title = "Multi fleet model data",
           period = "1991-2024",
           access = "Restricted",
           file = T,
           append = T)

draft.data(data.files = "download_from_stockassessment_org_single_fleet",
           data.scripts = NULL,
           originator = "Downloaded from SAM, Marts 2025",
           title = "Single fleet model data",
           period = "1991-2024",
           access = "Restricted",
           file = T,
           append = T)




taf.boot()

# mkdir("data")
# mkdir("output")

# sourceTAF("data") 
# 
# sourceTAF("report") 
# run model_0....
