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
    cat("parsing names...\n")
    dt[, INI := substr(Name, 1, 1)]
    dt[INI == "3", Name := "Buster"]
    dt[INI == "'", Name := "Stache"]
    dt[INI == " ", Name := c("Joanie", "Mario")]
    dt[, INI := substr(Name, 1, 1)]
    
    # name is common or not? by frequency?
    cat("sorting nanes into frequency groups...\n")
    n_freq <- vector("numeric", length = nrow(dt))
    n_table <- table(dt[, Name])
    for(i in seq_along(dt[, Name])){
        n_freq[i] <- n_table[dt[i, Name]]
    }
    dt[, name_f := .(n_freq/nrow(dt))]
    dt[is.na(name_f), name_f := -1] #xgboost can handle this
    # create a new feature named 1:Yes, 0:No
    cat("simplifying names...\n")
    dt[INI == "", named := 0]
    dt[INI != "", named := 1]
    
    # parse date
    cat("parsing date...\n")
    dt[, DateTime:= ymd_hms(DateTime)]
    dt[, time := .(hour(DateTime) + minute(DateTime)/60)]
    dt[, year := factor(lubridate::year(DateTime), ordered = T)]
    dt[, month := factor(lubridate::month(DateTime), ordered = T)]
    dt[, day := factor(lubridate::day(DateTime), ordered =T)]
    dt[, weekday := lubridate::wday(DateTime, label = T)]
    
    # parse sex and status
    cat("parsing sex and status...\n")
    dt[, sex := "Unknown"]
    dt[SexuponOutcome %like% "Male", sex := "Male"]
    dt[SexuponOutcome %like% "Female", sex := "Female"]
    
    dt[, status := "Unknown"]
    dt[SexuponOutcome %like% "Neutered", status := "Sterilized"]
    dt[SexuponOutcome %like% "Spayed", status := "Sterilized"]
    dt[SexuponOutcome %like% "Intact", status := "Intact"]
    
    # form age in days
    cat("transform age into days...\n")
    dt[AgeuponOutcome == "", AgeuponOutcome := "unknown unknown"]
    parsed_age <- do.call(rbind, sapply(dt[, AgeuponOutcome], strsplit, " "))
    dt[, c("num", "unit") := .(parsed_age[, 1], parsed_age[, 2])]
    dt[unit %like% "year", age := as.numeric(num)*365]
    dt[unit %like% "month", age := as.numeric(num)*30.5]
    dt[unit %like% "week", age := as.numeric(num)*7]
    dt[unit %like% "day", age := as.numeric(num)]
    dt[unit == "unknown", age := NA]
    
    # paring breed
    cat("dividing breed groups...\n")
    dt[Breed %like% "Mix", Breed := "Mix"]
    dt[!Breed %like% "Mix", Breed := "Pure"]
    
    # simplify color
    cat("simplifying colors into numbers...\n")
    color_count <- vector("numeric", length = nrow(dt))
    for(i in seq_along(dt[, Color])){
        color_count[i] <- word_count(dt[i, Color])
    }
    dt[, color := .(color_count)]
    
    # further categorizing color.....
    
    
    
    
    
    # drop orginal features
    cat("dropping orginal features...\n")
    if("OutcomeType" %in% colnames(df)){
        print("training set")
        dt <- dt[, .(OutcomeType, AnimalType, Breed, named, name_f, year, month, 
                     day, weekday, age, time, sex, status, color)]
        to_factors <- c("OutcomeType", "AnimalType", "Breed", 
                        "named",  "sex", "status")
    } else {
        print("testing set")
        dt <- dt[, .(AnimalType, Breed, named, name_f, year, month, day, weekday, 
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
    cat("these missing values need to be imputed: \n")
    print(for_imp)
return(dt)
}

dt.train <- clean(train)
dt.test <- clean(test)