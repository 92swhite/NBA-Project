temp <- list.files(pattern = "*.csv")
temp_names <- gsub(".csv", "", temp)

for(i in 1:length(temp)){
  assign(paste("year_", temp_names[i], sep = ""), 
         read.table(temp[i], header = T, sep = ","))
}


years <- rbind(`year_1996-1997`, `year_1997-1998`, `year_1998-1999`, `year_1999-2000`,
      `year_2000-2001`, `year_2001-2002`, `year_2002-2003`, `year_2003-2004`,
      `year_2004-2005`, `year_2005-2006`, `year_2006-2007`, `year_2007-2008`,
      `year_2008-2009`, `year_2009-2010`, `year_2010-2011`, `year_2011-2012`, 
      `year_2012-2013`, `year_2013-2014`, `year_2014-2015`, `year_2015-2016`, 
      `year_2016-2017`)
head(years)

years_test <- years

years_test$ref1 <- factor(years_test$ref1)
years_test$ref2 <- factor(years_test$ref2)
years_test$ref3 <- factor(years_test$ref3)
years_test

for(i in 1:length(rownames(years_test))){
  if((years_test$v_points[i] > years_test$h_points[i]) == T){
    years_test$winner[i] <- as.character(years_test$v_name[i])
  } else {
    years_test$winner[i] <- as.character(years_test$h_name[i])
  }
}


for(i in 1:length(rownames(years_test))){
  if((years_test$v_points[i] > years_test$h_points[i]) == T){
    years_test$winner_HOA[i] <- "Home"
  } else {
    years_test$winner_HOA[i] <- "Away"
  }
}


for(i in 1:length(rownames(years_test))){
  years_test$diff[i] <- abs(years_test$v_points[i] - years_test$h_points[i])
}

