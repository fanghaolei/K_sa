library(data.table)
library(lubridate)

# a function count number of words describing colors
word_count <- function(x){
    cleaned <- gsub("/", " ", x)
    parsed  <- strsplit(cleaned, " ")
    count <- length(unlist(parsed))
    return(count)    
}

# a clean function for training and testing
clean <- function(df){
    dt <- data.table(df)
    # parse names
    print("simplifying names...")
    dt[, INI := substr(Name, 1, 1)]
    dt[INI == "3", INI := "B"]
    dt[INI == "'", INI := "S"]
    dt[INI == "0", ]
    dt[INI == " ", INI := c("J", "M")]
    
    # create a new feature named 1:Yes, 0:No
    dt[INI == "", named := 0]
    dt[INI != "", named := 1]
    
    # parse date
    print("parsing date...")
    dt[, DateTime:= ymd_hms(DateTime)]
    dt[, time := .(hour(DateTime) + minute(DateTime)/60)]
    dt[, year := factor(lubridate::year(DateTime), ordered = T)]
    dt[, month := factor(lubridate::month(DateTime), ordered = T)]
    dt[, day := factor(lubridate::day(DateTime), ordered =T)]
    dt[, weekday := lubridate::wday(DateTime, label = T)]
    
    # parse sex and status
    print("parsing sex and status...")
    dt[, sex := "Unknown"]
    dt[SexuponOutcome %like% "Male", sex := "Male"]
    dt[SexuponOutcome %like% "Female", sex := "Female"]
    
    dt[, status := "Unknown"]
    dt[SexuponOutcome %like% "Neutered", status := "Sterilized"]
    dt[SexuponOutcome %like% "Spayed", status := "Sterilized"]
    dt[SexuponOutcome %like% "Intact", status := "Intact"]
    
    # form age in days
    print("transform age into days...")
    dt[AgeuponOutcome == "", AgeuponOutcome := "unknown unknown"]
    parsed_age <- do.call(rbind, sapply(dt[, AgeuponOutcome], strsplit, " "))
    dt[, c("num", "unit") := .(parsed_age[, 1], parsed_age[, 2])]
    dt[unit %like% "year", age := as.numeric(num)*365]
    dt[unit %like% "month", age := as.numeric(num)*30.5]
    dt[unit %like% "week", age := as.numeric(num)*7]
    dt[unit %like% "day", age := as.numeric(num)]
    dt[unit == "unknown", age := NA]
    
    # paring breed
    print("dividing breed groups...")
    dt[Breed %like% "Mix", Breed := "Mix"]
    dt[!Breed %like% "Mix", Breed := "Pure"]
    
    # simplify color
    print("simplifying colors into numbers...")
    color_count <- vector("numeric", length = nrow(dt))
    for(i in seq_along(dt[, Color])){
        color_count[i] <- word_count(dt[i, Color])
    }
    dt[, color := .(color_count)]
    
    # drop orginal features
    print("dropping orginal features...")
    
    
    if("OutcomeType" %in% colnames(df)){
        print("training set")
        dt <- dt[, .(OutcomeType, AnimalType, Breed, named, year, month, 
                     day, weekday, age, time, sex, status, color)]
        to_factors <- c("OutcomeType", "AnimalType", "Breed", 
                        "named",  "sex", "status")
    } else {
        print("testing set")
        dt <- dt[, .(AnimalType, Breed, named, year, month, day, weekday, 
                     age, time, sex, status, color)]
        to_factors <- c("AnimalType", "Breed", 
                        "named",  "sex", "status")
    }
    
    # convert to factor
    for(col in to_factors){
        print(paste("converting to factors...", col))
        set(dt, j = col, value = as.factor(dt[[col]]))
    }
    
    for_imp <- which(is.na(dt), arr.ind =T)
    print("these missing values need to be imputed: ")
    print(for_imp)
    return(dt)
}

dt.train <- clean(train)
dt.test <- clean(test)
write.csv(dt.train, file = "dt_train", row.names = F)
write.csv(dt.test, file = "dt_test", row.names = F)
