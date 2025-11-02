

read_canum_sheet_old_format_IIIa <- function(file, sheet_name) {
  library(readxl)
  library(stringr)
  library(dplyr)
  
  dat <- read_excel(path = file, sheet = sheet_name)
  
  # Read header informations
  position_head <- which(apply(dat, 1, function(x)
    any(grepl("Division:", x))))
  
  head <- dat[position_head, ]
  
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
  
  fleet <- "F"
  
  head_done <- cbind(year, ctry, fleet)
  
  print(head_done)
  
  
  # Canum
  position_quarter <- which(apply(dat, 1, function(x)
    any(grepl("Quarter", x))))
  
  canum <- c()
  
  for (i in position_quarter[1:4]) {
    # Per Quarter
    q <- dat[c((i - 1):(i + 11)), ]
    
    head_col <- q[c(1:nrow(q)), c(1:2)]
    
    colnames(head_col) <- c("quarter", "wr")
    
    ## Per subdivision
    position_subdiv <- grep("Sub-division", q)
    
    for (j in position_subdiv[c(1:3)]) { 
      subdiv <- q[, c(j:(j + 1))]
      
      subdiv_1 <- cbind(head_col, subdiv)
      
      
      subdiv_name <- gsub(".*\\s+", "", as.character(subdiv[1, 1]))
      subdiv_1$quarter <- as.numeric(subdiv_1$quarter)
      quarter <- subset(subdiv_1, !(is.na(quarter)))
      quarter <- quarter[, 1]
      catch_t <- subset(subdiv_1, wr == "SOP")
      catch_t <- as.numeric(catch_t[, 4])
      if (quarter %in% c(1, 2)) {
        wr <- as.character(subdiv_1[c(3:10), 2])
        subdiv_dat <- subdiv_1[c(3:10), c(3:4)]
      } else {
        wr <- as.character(subdiv_1[c(3:11), 2])
        subdiv_dat <- subdiv_1[c(3:11), c(3:4)]
      }
      
      
      colnames(subdiv_dat) <- c("canum", "weca")
      
      #Combine
      canum_qf <- mutate(
        subdiv_dat,
        wr = wr,
        division = subdiv_name,
        quarter = quarter,
        catch_t = catch_t
      )
      
      canum <- rbind(canum, canum_qf)
    }
    
  }
  
  canum_head <- cbind(canum, head_done)
  canum_head$canum <- as.numeric(canum_head$canum)
  
  return(canum_head)
  
}