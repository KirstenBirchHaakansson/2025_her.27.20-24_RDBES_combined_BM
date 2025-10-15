
read_canum_sheet_old_format_IIIa <- function(file, sheet_name) {
  
  library(readxl)
  library(stringr)
  library(dplyr)
  
  dat <- read_excel(path = file, sheet = sheet_name)
  
  # Read header informations
  position_head <- which(apply(dat, 1, function(x) any(grepl("Division:", x))))
  
  head <- dat[position_head, ]
  
  division <- head[, grep("Division:", head) + 2]
  colnames(division) <- c("division")
  if (is.na(division$division)) {
    division <- head[, grep("Division:", head) + 1]
    colnames(division) <- c("division")
  }
  year <- head[, grep("Year:", head) + 2]
  colnames(year) <- c("year")
  if (is.na(year$year) | year$year == "Country:") {
    year <- head[, grep("Year:", head) + 1]
    colnames(year) <- c("year")
  }
  ctry <- head[, grep("Country:", head) + 2]
  colnames(ctry) <- c("ctry")
  if (is.na(ctry$ctry)) {
    ctry <- head[, grep("Country:", head) + 1]
    colnames(ctry) <- c("ctry")
  }
  
  head_done <- cbind(year, ctry, division)
  
  print(head_done)
  
  
  # Canum
  position_quarter <- which(apply(dat, 1, function(x) any(grepl("Quarter", x))))
  
  canum <- c()
  
  for (i in position_quarter[1:4]) {
    
    # Per Quarter
    q <- dat[c((i-1):(i+11)), ]
    
    head_col <- q[c(1:nrow(q)), c(1:2)]
    
    colnames(head_col) <- c("quarter", "wr")
    
    ## Per fleet
    position_fleet <- grep("Fleet", q)
    
    for (j in position_fleet) {
      
      fleet <- q[, c(j:(j+1))]  
      
      fleet_1 <- cbind(head_col, fleet)
      
      
      fleet_name <- gsub(".*\\s+","", as.character(fleet[1, 1]))
      fleet_1$quarter <- as.numeric(fleet_1$quarter)
      quarter <- subset(fleet_1, !(is.na(quarter)))
      quarter <- quarter[, 1]
      catch_t <- subset(fleet_1, wr == "SOP")
      catch_t <- as.numeric(catch_t[, 4])
      wr <- as.character(fleet_1[c(3:10), 2])
      
      fleet_dat <- fleet_1[c(3:10), c(3:4)]
      colnames(fleet_dat) <- c("canum", "weca")
      
      #Combine
      canum_qf <- mutate(fleet_dat, wr = wr, fleet = fleet_name, quarter = quarter,
                         catch_t = catch_t)
      
      canum <- rbind(canum, canum_qf)
    }
    
  }
  
  canum_head <- cbind(canum, head_done)
  
  return(canum_head)
  
}