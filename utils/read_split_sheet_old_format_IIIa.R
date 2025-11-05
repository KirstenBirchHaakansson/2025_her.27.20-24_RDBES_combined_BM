

read_split_sheet_format_IIIa <- function(file, sheet_name) {
  library(readxl)
  library(stringr)
  library(dplyr)
  
  dat <- read_excel(path = file, sheet = sheet_name)
  
  # Read header informations
  position_head <- which(apply(dat, 1, function(x)
    any(grepl("Year:", x))))
  
  head <- dat[position_head, ]
  
  year <- head[, grep("Year:", head) + 2]
  colnames(year) <- c("year")
  if (is.na(year$year) | year$year == "Country:") {
    year <- head[, grep("Year:", head) + 1]
    colnames(year) <- c("year")
  }
  
  head_done <- cbind(year)
  
  print(head_done)
  
  
  # Canum
  position_quarter <- which(apply(dat, 1, function(x)
    any(grepl("Quarter", x))))
  
  split <- c()
  
  for (i in position_quarter[1:4]) {
    # Per Quarter
    q <- dat[c((i - 1):(i + 9)), ]
    
    head_col <- q[c(1:nrow(q)), c(1:2)]
    
    colnames(head_col) <- c("quarter", "wr")
    
    ## Per subdivision
    position_subdiv <- grep("Skagerrak|Kattegat", q)
    
    for (j in position_subdiv[c(1:2)]) { 
      subdiv <- q[, c(j:(j + 1))]
      
      subdiv_1 <- cbind(head_col, subdiv)
      
      
      subdiv_name <- as.character(subdiv[1, 1])
      subdiv_1$quarter <- as.numeric(subdiv_1$quarter)
      quarter <- subset(subdiv_1, !(is.na(quarter)))
      quarter <- quarter[, 1]
      if (quarter %in% c(1, 2)) {
        wr <- as.character(subdiv_1[c(3:10), 2])
        subdiv_dat <- subdiv_1[c(3:10), c(3:4)]
      } else {
        wr <- as.character(subdiv_1[c(3:11), 2])
        subdiv_dat <- subdiv_1[c(3:11), c(3:4)]
      }
      
      
      colnames(subdiv_dat) <- c("nsas", "wbss")
      
      #Combine
      split_qf <- mutate(
        subdiv_dat,
        wr = wr,
        division = subdiv_name,
        quarter = quarter
      )
      
      split <- rbind(split, split_qf)
    }
    
  }
  
  split_head <- cbind(split, head_done)
  split_head$nsas <- as.numeric(split_head$nsas)
  split_head$wbss <- as.numeric(split_head$wbss)
  split_head$year <- as.numeric(split_head$year)
  
  split_head$area <- NA
  split_head$area[split_head$division == "Skagerrak"] <- "27.3.a.20"
  split_head$area[split_head$division == "Kattegat"] <- "27.3.a.21"
  
  return(split_head)
  
}