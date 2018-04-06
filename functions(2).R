getwd()
setwd('D:\\NBA ref project\\Redux\\New excel fouls')
temp = list.files(pattern="*.csv")
df <- NULL
qn <- c(paste(c('1st', '2nd', '3rd', '4th'), 'quarter'), paste0(c('', '1', '2', '3', '4'), 'OT'))
f_after_f <- NULL
h_counts <- NULL
v_counts <- NULL
get_data <- function(temp){
  for (i in temp){
    x <- read.csv(i, stringsAsFactors = F)
    test <- strsplit(x$foul_score, 'score: ')
    test <- unlist(lapply(test, '[', 2))
    test2 <- strsplit(test, '-')
    test2 <- unlist(lapply(test2, '['))
    test3 <- NULL
    for (i in seq(1, length(test2), 2)){
      test3 <- c(test3, (as.numeric(test2[i]) - as.numeric(test2[(i + 1)])))
    }
    x$foul_score_diff <- test3
    x$mean <- mean(x$foul_score_diff)
    x$sd <- sd(x$foul_score_diff)
    y <- strsplit(x$foul_time, ':')
    for (i in 1:length(x$foul_time)){
      r <- strsplit(x$foul_time[i], ':')
      if (length(unlist(strsplit(r[[1]][1], ''))) < 2){
        r[[1]][1] <- gsub(' ', '' , paste('0', r[[1]][1]))
      }
      if (length(unlist(strsplit(r[[1]][2], ''))) < 2){
        r[[1]][2] <- gsub(' ' , '', paste(r[[1]][2], 0))
      }
      
      x$foul_time[i] <- (gsub(' ', '', paste(r[[1]][1], ':', r[[1]][2])))
    }
    dec_holder <- NULL
    dec_holder <- as.numeric(lapply(y, "[", 1)) + as.numeric(lapply(y, "[", 2))/60
    x$foul_time_dec <- dec_holder
    q4 <- which(x$foul_quarter == qn[4])
    a <- x$foul_time_dec[which(x$foul_quarter == qn[4])]
    b <- a - c(a[-1], 0)
    if((length(which(b < 0)) == 1) == T){
      x$foul_quarter[q4][(which(b < 0)[1] + 1):length(x$foul_quarter[q4])] <- 'OT'
    }
    if((length(which(b < 0)) == 2) == T){
      x$foul_quarter[q4][(which(b < 0)[1] + 1):length(x$foul_quarter[q4])] <- 'OT'
      x$foul_quarter[q4][(which(b < 0)[2] + 1):length(x$foul_quarter[q4])] <- '2OT'
    }
    if((length(which(b < 0)) == 3) == T){
      x$foul_quarter[q4][(which(b < 0)[1] + 1):length(x$foul_quarter[q4])] <- 'OT'
      x$foul_quarter[q4][(which(b < 0)[2] + 1):length(x$foul_quarter[q4])] <- '2OT'
      x$foul_quarter[q4][(which(b < 0)[3] + 1):length(x$foul_quarter[q4])] <- '3OT'
    }
    if((length(which(b < 0)) == 4) == T){
      x$foul_quarter[q4][(which(b < 0)[1] + 1):length(x$foul_quarter[q4])] <- 'OT'
      x$foul_quarter[q4][(which(b < 0)[2] + 1):length(x$foul_quarter[q4])] <- '2OT'
      x$foul_quarter[q4][(which(b < 0)[3] + 1):length(x$foul_quarter[q4])] <- '3OT'
      x$foul_quarter[q4][(which(b < 0)[4] + 1):length(x$foul_quarter[q4])] <- '4OT'
    }
    x$v_fc <- length(which(x$foul_team == x$v_name))
    x$v_first_q <- length(which(x$foul_team == x$v_name & x$foul_quarter == '1st quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$v_second_q <- length(which(x$foul_team == x$v_name & x$foul_quarter == '2nd quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$v_third_q <- length(which(x$foul_team == x$v_name & x$foul_quarter == '3rd quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$v_fourth_q <- length(which(x$foul_team == x$v_name & x$foul_quarter == '4th quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$v_2m <- length(which(x$foul_team == x$v_name & x$foul_quarter == '4th quarter' & x$foul_time_dec <= 2))
    x$h_fc <- length(which(x$foul_team == x$h_name))
    x$h_first_q <- length(which(x$foul_team == x$h_name & x$foul_quarter == '1st quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$h_second_q <- length(which(x$foul_team == x$h_name & x$foul_quarter == '2nd quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$h_third_q <- length(which(x$foul_team == x$h_name & x$foul_quarter == '3rd quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$h_fourth_q <- length(which(x$foul_team == x$h_name & x$foul_quarter == '4th quarter' & (x$foul_time_dec <= 10 & x$foul_time_dec >= 1)))
    x$h_2m <- length(which(x$foul_team == x$h_name & x$foul_quarter == '4th quarter' & x$foul_time_dec <= 2))
    x$winner_hv <- NA
    x$winner_hv[which(x$v_points < x$h_points)] <- 'h'
    x$winner_hv[which(x$v_points > x$h_points)] <- 'v'
    x$v_addF <- 0
    x$h_addF <- 0
    
    for(j in 1:length(x$date)){
      if(as.character(x$foul_team[j]) == as.character(x$v_name[j])){
        x$v_addF[j] <- length(x$v_addF[which(x$v_addF != 0)]) + 1
      } else {
        x$h_addF[j] <- length(x$h_addF[which(x$h_addF != 0)]) + 1
      }
    }
    x$v_addF[which(x$v_addF == 0)[-1]] <- NA
    x$h_addF[which(x$h_addF == 0)[-1]] <- NA
    x$v_addP <- 0
    x$h_addP <- 0
    for (j in 1:length(x$date)){
      for (k in qn){
        if((as.character(x$foul_team[j]) == as.character(x$v_name[j])) 
           & x$foul_quarter[j] == k){
          x$v_addP[j] <- length(x$v_addP[which(x$v_addP != 0 
                                               & x$foul_quarter == k)]) + 1
        } else if ((as.character(x$foul_team[j]) == as.character(x$h_name[j])) 
                   & x$foul_quarter[j] == k) {
          x$h_addP[j] <- length(x$h_addP[which(x$h_addP != 0
                                               & x$foul_quarter == k)]) + 1
        }
      }
    }
    
    for(k in qn){
      x$v_addP[which(x$v_addP == 0 & x$foul_quarter == k)][-1] <- NA
      x$h_addP[which(x$h_addP == 0 & x$foul_quarter == k)][-1] <- NA
    }
    x$foul_time_dec_diff <- NA
    for(k in qn){
      aa <- x$foul_time_dec[which(x$foul_quarter == k)]
      bb <- aa - c(aa[-1], NA)
      x$foul_time_dec_diff[which(x$foul_quarter == k)] <- bb
    }
    f_after_f <- x[c('v_addF', 'h_addF')]
    f_after_f$v_addF[which(is.na(f_after_f$v_addF) == F)] <- 1
    f_after_f$v_addF[which(is.na(f_after_f$v_addF) == T)] <- 0
    f_after_f$h_addF[which(is.na(f_after_f$h_addF) == F)] <- 1
    f_after_f$h_addF[which(is.na(f_after_f$h_addF) == T)] <- 0
    z1 <- which(f_after_f$v_addF == 0)
    z2 <- which(f_after_f$h_addF == 0)
    z1 <- z1 - c(z1[-1], NA)
    z2 <- z2 - c(z2[-1], NA)
    v_counts <<- c(v_counts, z1)
    h_counts <<- c(h_counts, z2)
    df <<- rbind(df, x)
  }
}



# makes master df of fouls by quarter
t1 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '1st quarter')]))
t2 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '2nd quarter')]))
t3 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '3rd quarter')]))
t4 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '4th quarter')]))
t5 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == 'OT')]))
t6 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '2OT')]))
t7 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '3OT')]))
t8 <- as.data.frame(table(df$foul_type[which(df$foul_quarter == '4OT')]))
tA <- as.data.frame(table(df$foul_type))
fq <- data.frame(foul_type = unique(df$foul_type))
t_list <- list(t1, t2, t3, t4, t5, t6, t7, t8)
for(j in 1:length(t_list)){
  t <- as.data.frame(t_list[j])
  for(i in 1:length(fq$foul_type)){
    if(any(as.character(t$Var1) == as.character(fq$foul_type[i]))){
      x <- which(as.character(t[, 1]) == as.character(fq$foul_type[i]))
      fq$qq[i] <- t[x, 2]
    } else {
      fq$qq[i] <- NA
    }
  }
  colnames(fq)[j + 1] <- paste0('q', j)
}

## fq with ot
fqot <- data.frame(foul_type = unique(df$foul_type))
t_list <- list(t1, t2, t3, t4, t5, t6, t7, t8)
for(j in 1:length(t_list)){
  t <- as.data.frame(t_list[j])
  for(i in 1:length(fqot$foul_type)){
    if(any(as.character(t$Var1) == as.character(fqot$foul_type[i]))){
      x <- which(as.character(t[, 1]) == as.character(fqot$foul_type[i]))
      fqot$qq[i] <- t[x, 2]
    } else {
      fqot$qq[i] <- NA
    }
  }
  colnames(fqot)[j + 1] <- paste0('q', j)
}

for(i in 1:length(fq$foul_type)){
  fqot$total[i] <- sum(fqot[i, 2:9], na.rm = T)
}
fqot

## fq without ot
fq <- data.frame(foul_type = unique(df$foul_type))
t_list <- list(t1, t2, t3, t4)
for(j in 1:length(t_list)){
  t <- as.data.frame(t_list[j])
  for(i in 1:length(fq$foul_type)){
    if(any(as.character(t$Var1) == as.character(fq$foul_type[i]))){
      x <- which(as.character(t[, 1]) == as.character(fq$foul_type[i]))
      fq$qq[i] <- t[x, 2]
    } else {
      fq$qq[i] <- NA
    }
  }
  colnames(fq)[j + 1] <- paste0('q', j)
}


for(i in 1:length(fq$foul_type)){
  fq$total[i] <- sum(fq[i, 2:5], na.rm = T)
}
fq

# makes master df of fouls by quarter, percentage

## without ot
fqp <- fq
fqp[, 2:5] <- fqp[ , 2:5]/fqp[, 6]
fqp


## with ot
fqotp <- fqot
fqotp[, 2:9] <- fqotp[ , 2:9]/fqotp[, 10]
fqotp

# Mean and SD of score diff by quarter, no OT

sd_m4 <- mean(df$foul_score_diff[which(df$foul_quarter == '4th quarter')])
sd_m3 <- mean(df$foul_score_diff[which(df$foul_quarter == '3rd quarter')])
sd_m2 <- mean(df$foul_score_diff[which(df$foul_quarter == '2nd quarter')])
sd_m1 <- mean(df$foul_score_diff[which(df$foul_quarter == '1st quarter')])

sd_s1 <- sd(df$foul_score_diff[which(df$foul_quarter == '1st quarter')])
sd_s2 <- sd(df$foul_score_diff[which(df$foul_quarter == '2nd quarter')])
sd_s3 <- sd(df$foul_score_diff[which(df$foul_quarter == '3rd quarter')])
sd_s4 <- sd(df$foul_score_diff[which(df$foul_quarter == '4th quarter')])

sd_df <- data.frame(mean = c(sd_m1, sd_m2, sd_m3, sd_m4), 
                    sd = c(sd_s1, sd_s2, sd_s3, sd_s4))

# A df of all games played, unique

df_games <- unique(df[c('date', 'v_name', 'v_points', 'h_name', 'h_points', 
                        'ovt', 'mean', 'sd', 'winner_hv', 'v_fc', 'h_fc')])
length(df_games$date)

# df of only games nuggets played in

df_nugs <- NULL


df_nugs <- unique(df[c('date', 'v_name', 'v_points', 'h_name', 'h_points', 
                       'ovt', 'mean', 'sd', 'winner_hv', 'v_fc', 'v_first_q', 
                       'v_second_q', 'v_third_q', 'v_fourth_q', 'h_fc', 'h_first_q',
                       'h_second_q', 'h_third_q', 'h_fourth_q')])


df_nugs <- df_nugs[which(df_nugs$v_name == 'Denver Nuggets' | df_nugs$h_name == 'Denver Nuggets'), ]
head(df_nugs)

df_nugs$nugs_fc[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$h_fc[df_nugs$h_name == 'Denver Nuggets']
df_nugs$nugs_fc[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$v_fc[df_nugs$v_name == 'Denver Nuggets']
df_nugs$opp_fc[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_fc[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_fc[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_fc[df_nugs$v_name == 'Denver Nuggets']
df_nugs$opp_name[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_name[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_name[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_name[df_nugs$v_name == 'Denver Nuggets']

df_nugs$nugs_first_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$h_first_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$nugs_first_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$v_first_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$nugs_second_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$h_second_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$nugs_second_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$v_second_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$nugs_third_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$h_third_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$nugs_third_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$v_third_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$nugs_fourth_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$h_fourth_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$nugs_fourth_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$v_fourth_q[df_nugs$v_name == 'Denver Nuggets']

df_nugs$opp_first_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_first_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_first_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_first_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$opp_second_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_second_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_second_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_second_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$opp_third_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_third_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_third_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_third_q[df_nugs$v_name == 'Denver Nuggets']
df_nugs$opp_fourth_q[which(df_nugs$h_name == 'Denver Nuggets')] <- df_nugs$v_fourth_q[df_nugs$h_name == 'Denver Nuggets']
df_nugs$opp_fourth_q[which(df_nugs$v_name == 'Denver Nuggets')] <- df_nugs$h_fourth_q[df_nugs$v_name == 'Denver Nuggets']


# vector of NW Div team names
nw_div <- c('Portland Trail Blazers', 'Oklahoma City Thunder', 'Minnesota Timberwolves', 
            'Utah Jazz')

nw_div2 <- c('Portland Trail Blazers', 'Oklahoma City Thunder', 'Minnesota Timberwolves', 
            'Utah Jazz', 'Denver Nuggets')

# df of nuggets games played against division teams
df_nugs_div <- df_nugs[which(df_nugs$opp_name == nw_div[1] | df_nugs$opp_name == nw_div[2] | 
                               df_nugs$opp_name == nw_div[3] | df_nugs$opp_name == nw_div[4]), ]


# boxplot of nuggets total game fouls home vs away

a <- (df_nugs$v_fc[which(df_nugs$v_name == 'Denver Nuggets')])
b <- (df_nugs$h_fc[which(df_nugs$h_name == 'Denver Nuggets')])
boxplot(a, b, names = c('V', 'H'))

# fouls called on team B after being called on team A
h_after_v <- NULL
for(i in 1:length(df$date)){
  if(df$foul_team[i] == df$v_name[i] & df$foul_team[i+1] == df$h_name[i+1]){
    h_after_v <- c(h_after_v, i)
  }
}

v_after_h <- NULL
for(i in 1:length(df$date)){
  if(df$foul_team[i] == df$v_name[i] & df$foul_team[i+1] == df$h_name[i+1]){
    v_after_h <- c(v_after_h, i)
  }
}

h_after_h <- NULL
for(i in 1:length(df$date)){
  if(df$foul_team[i] == df$h_name[i] & df$foul_team[i+1] == df$h_name[i+1]){
    h_after_h <- c(h_after_h, i)
  }
}

v_after_v <- NULL
for(i in 1:length(df$date)){
  if(df$foul_team[i] == df$v_name[i] & df$foul_team[i+1] == df$v_name[i+1]){
    v_after_v <- c(v_after_v, i)
  }
}

?table

after <- data.frame(v_after_v = length(v_after_v), v_after_h = length(v_after_h), 
                    h_after_h = length(h_after_h), h_after_v = length(h_after_v))
after

after[, 5] <- sum(after[1, 1:4])
after[2, 1:4] <- after[1, 1:4]/after[,5]
colnames(after)[5] <- 'total'

after[2, 5] <- sum(after[2, 1:4])
after
as.vector(as.numeric(after[2, 1:4]))
barplot(x = 1:4, y = as.vector(after[2, 1:4]))
barplot(x = c(1:4))
barplot(as.vector(as.numeric(after[2, 1:4])))


# creates q1:4 with t1:12 for each

q1 <- df$foul_time_dec[df$foul_quarter == qn[1]]
q2 <- df$foul_time_dec[df$foul_quarter == qn[2]]
q3 <- df$foul_time_dec[df$foul_quarter == qn[3]]
q4 <- df$foul_time_dec[df$foul_quarter == qn[4]]

## rep this for each qn

for(i in seq(0, 11, 1)){
  a <- i + .99999
  b <- b + 1 
  assign(paste0('q4t', i + 1), q4[q4 > i & q4 < a]) 
}

## better version
aq1tt <- list(NULL)
b <- 0
for(i in seq(1, 9.5, .5)){
  a <- i + .49999
  b <- b + 1 
  print(a)
  print(i)
  print('fkdgj')
  aq1tt[b] <- length(q4[q4 > i & q4 < a])
}
barplot(as.double(aq1tt))
chisq.test(as.double(aq1tt), p = c(rep(1/18, 18)))

df_nugs_div <- df_nugs[which(df_nugs$opp_name == nw_div[1] | df_nugs$opp_name == nw_div[2] | 
                               df_nugs$opp_name == nw_div[3] | df_nugs$opp_name == nw_div[4]), ]


MLE_df <- data.frame('OKC' = 0.04183536, 'POR' = 0.04771574, 'MIN' = 0.0458891, 'UTA' = 0.04296161)
MLE_df
LM_df <- data.frame('OKC' = 0.2679, 'POR' = 0.1415, 'MIN' = 0.08992, 'UTA' = 0.4076)

#df of lambdas for nugs vs nw div teams
#NBA
nba_f <- sum(df_nugs$nugs_fc)/(length(df_nugs$nugs_fc)*9*4)

nba_1 <- sum(df_nugs$nugs_first_q)/(length(df_nugs$nugs_first_q)*9)

nba_2 <- sum(df_nugs$nugs_second_q)/(length(df_nugs$nugs_second_q)*9)

nba_3 <- sum(df_nugs$nugs_third_q)/(length(df_nugs$nugs_third_q)*9)

nba_4 <- sum(df_nugs$nugs_fourth_q)/(length(df_nugs$nugs_fourth_q)*9)

#POR
por_f <- sum(df_nugs$nugs_fc[df_nugs$opp_name == nw_div[1]])/(length((df_nugs$nugs_fc[df_nugs$opp_name == nw_div[1]]))*9*4)

por_1 <- sum(df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[1]])/(length((df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[1]]))*9)

por_2 <- sum(df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[1]])/(length((df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[1]]))*9)

por_3 <- sum(df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[1]])/(length((df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[1]]))*9)

por_4 <- sum(df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[1]])/(length((df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[1]]))*9)


#OKC
okc_f <- sum(df_nugs$nugs_fc[df_nugs$opp_name == nw_div[2]])/(length((df_nugs$nugs_fc[df_nugs$opp_name == nw_div[2]]))*9*4)

okc_1 <- sum(df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[2]])/(length((df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[2]]))*9)

okc_2 <- sum(df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[2]])/(length((df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[2]]))*9)

okc_3 <- sum(df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[2]])/(length((df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[2]]))*9)

okc_4 <- sum(df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[2]])/(length((df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[2]]))*9)


#MIN
min_f <- sum(df_nugs$nugs_fc[df_nugs$opp_name == nw_div[3]])/(length((df_nugs$nugs_fc[df_nugs$opp_name == nw_div[3]]))*9*4)

min_1 <- sum(df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[3]])/(length((df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[3]]))*9)

min_2 <- sum(df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[3]])/(length((df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[3]]))*9)

min_3 <- sum(df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[3]])/(length((df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[3]]))*9)

min_4 <- sum(df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[3]])/(length((df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[3]]))*9)


#UTA
uta_f <- sum(df_nugs$nugs_fc[df_nugs$opp_name == nw_div[4]])/(length((df_nugs$nugs_fc[df_nugs$opp_name == nw_div[4]]))*9*4)

uta_1 <- sum(df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[4]])/(length((df_nugs$nugs_first_q[df_nugs$opp_name == nw_div[4]]))*9)

uta_2 <- sum(df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[4]])/(length((df_nugs$nugs_second_q[df_nugs$opp_name == nw_div[4]]))*9)

uta_3 <- sum(df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[4]])/(length((df_nugs$nugs_third_q[df_nugs$opp_name == nw_div[4]]))*9)

uta_4 <- sum(df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[4]])/(length((df_nugs$nugs_fourth_q[df_nugs$opp_name == nw_div[4]]))*9)


df_lam_nugs <- data.frame("NBA" = c(nba_f, nba_1, nba_2, nba_3, nba_4), 
                          'POR' = c(por_f, por_1, por_2, por_3, por_4), 
                          "OKC" = c(okc_f, okc_1, okc_2, okc_3, okc_4), 
                          "MIN" = c(min_f, min_1, min_2, min_3, min_4), 
                          "UTA" = c(uta_f, uta_1, uta_2, uta_3, uta_4))

df_lam_nugs_results <- data.frame('NBA' = c(rep(NA, 5)), 'POR' = c(rep('No', 5)), 
                                  'OKC' = c('Yes', 'No', 'No', 'Yes', 'No'), 
                                  'MIN' = c(rep('No', 5)), 
                                  'UTA' = c('Yes', rep('No', 4)))

# function to find lambda for every team
get_lam <- function(team){
  df_temp <- unique(df[c('date', 'v_name', 'v_points', 'h_name', 'h_points', 
                         'ovt', 'mean', 'sd', 'winner_hv', 'v_fc', 'v_first_q', 
                         'v_second_q', 'v_third_q', 'v_fourth_q', 'h_fc', 'h_first_q',
                         'h_second_q', 'h_third_q', 'h_fourth_q')])
  
  for(i in 1:1){
    
    df_temp$team_fc <- NA
    df_temp$opp_fc <- NA
    df_temp$opp_name <- NA
    df_temp$team_first_q <- NA
    df_temp$team_second_q <- NA
    df_temp$team_third_q <- NA
    df_temp$team_fourth_q <- NA
    df_temp$opp_first_q <- NA
    df_temp$opp_second_q <- NA
    df_temp$opp_third_q <- NA
    df_temp$opp_fourth_q <- NA
    
    df_temp$team_fc[which(df_temp$h_name == team)] <- df_temp$h_fc[df_temp$h_name == team]
    df_temp$team_fc[which(df_temp$v_name == team)] <- df_temp$v_fc[df_temp$v_name == team]
    df_temp$opp_fc[which(df_temp$h_name == team)] <- df_temp$v_fc[df_temp$h_name == team]
    df_temp$opp_fc[which(df_temp$v_name == team)] <- df_temp$h_fc[df_temp$v_name == team]
    df_temp$opp_name[which(df_temp$h_name == team)] <- df_temp$v_name[df_temp$h_name == team]
    df_temp$opp_name[which(df_temp$v_name == team)] <- df_temp$h_name[df_temp$v_name == team]
    
    df_temp$team_first_q[which(df_temp$h_name == team)] <- df_temp$h_first_q[df_temp$h_name == team]
    df_temp$team_first_q[which(df_temp$v_name == team)] <- df_temp$v_first_q[df_temp$v_name == team]
    df_temp$team_second_q[which(df_temp$h_name == team)] <- df_temp$h_second_q[df_temp$h_name == team]
    df_temp$team_second_q[which(df_temp$v_name == team)] <- df_temp$v_second_q[df_temp$v_name == team]
    df_temp$team_third_q[which(df_temp$h_name == team)] <- df_temp$h_third_q[df_temp$h_name == team]
    df_temp$team_third_q[which(df_temp$v_name == team)] <- df_temp$v_third_q[df_temp$v_name == team]
    df_temp$team_fourth_q[which(df_temp$h_name == team)] <- df_temp$h_fourth_q[df_temp$h_name == team]
    df_temp$team_fourth_q[which(df_temp$v_name == team)] <- df_temp$v_fourth_q[df_temp$v_name == team]
    
    df_temp$opp_first_q[which(df_temp$h_name == team)] <- df_temp$v_first_q[df_temp$h_name == team]
    df_temp$opp_first_q[which(df_temp$v_name == team)] <- df_temp$h_first_q[df_temp$v_name == team]
    df_temp$opp_second_q[which(df_temp$h_name == team)] <- df_temp$v_second_q[df_temp$h_name == team]
    df_temp$opp_second_q[which(df_temp$v_name == team)] <- df_temp$h_second_q[df_temp$v_name == team]
    df_temp$opp_third_q[which(df_temp$h_name == team)] <- df_temp$v_third_q[df_temp$h_name == team]
    df_temp$opp_third_q[which(df_temp$v_name == team)] <- df_temp$h_third_q[df_temp$v_name == team]
    df_temp$opp_fourth_q[which(df_temp$h_name == team)] <- df_temp$v_fourth_q[df_temp$h_name == team]
    df_temp$opp_fourth_q[which(df_temp$v_name == team)] <- df_temp$h_fourth_q[df_temp$v_name == team]
    
  }
  
  df_temp <- df_temp[which(df_temp$opp_name == nw_div[1] | df_temp$opp_name == nw_div[2] | 
                             df_temp$opp_name == nw_div[3] | df_temp$opp_name == nw_div[4] |
                             df_temp$opp_name == nw_div2[5]), ]
  
  #NBA
  t_nba_f <- sum(df_temp$team_fc)/(length(df_temp$team_fc)*9*4)
  t_nba_1 <- sum(df_temp$team_first_q)/(length(df_temp$team_first_q)*9)
  t_nba_2 <- sum(df_temp$team_second_q)/(length(df_temp$team_second_q)*9)
  t_nba_3 <- sum(df_temp$team_third_q)/(length(df_temp$team_third_q)*9)
  t_nba_4 <- sum(df_temp$team_fourth_q)/(length(df_temp$team_fourth_q)*9)
  
  #POR
  t_por_f <- sum(df_temp$team_fc[df_temp$opp_name == nw_div[1]])/(length((df_temp$team_fc[df_temp$opp_name == nw_div[1]]))*9*4)
  t_por_1 <- sum(df_temp$team_first_q[df_temp$opp_name == nw_div[1]])/(length((df_temp$team_first_q[df_temp$opp_name == nw_div[1]]))*9)
  t_por_2 <- sum(df_temp$team_second_q[df_temp$opp_name == nw_div[1]])/(length((df_temp$team_second_q[df_temp$opp_name == nw_div[1]]))*9)
  t_por_3 <- sum(df_temp$team_third_q[df_temp$opp_name == nw_div[1]])/(length((df_temp$team_third_q[df_temp$opp_name == nw_div[1]]))*9)
  t_por_4 <- sum(df_temp$team_fourth_q[df_temp$opp_name == nw_div[1]])/(length((df_temp$team_fourth_q[df_temp$opp_name == nw_div[1]]))*9)
  
  
  #OKC
  t_okc_f <- sum(df_temp$team_fc[df_temp$opp_name == nw_div[2]])/(length((df_temp$team_fc[df_temp$opp_name == nw_div[2]]))*9*4)
  t_okc_1 <- sum(df_temp$team_first_q[df_temp$opp_name == nw_div[2]])/(length((df_temp$team_first_q[df_temp$opp_name == nw_div[2]]))*9)
  t_okc_2 <- sum(df_temp$team_second_q[df_temp$opp_name == nw_div[2]])/(length((df_temp$team_second_q[df_temp$opp_name == nw_div[2]]))*9)
  t_okc_3 <- sum(df_temp$team_third_q[df_temp$opp_name == nw_div[2]])/(length((df_temp$team_third_q[df_temp$opp_name == nw_div[2]]))*9)
  t_okc_4 <- sum(df_temp$team_fourth_q[df_temp$opp_name == nw_div[2]])/(length((df_temp$team_fourth_q[df_temp$opp_name == nw_div[2]]))*9)
  
  
  #MIN
  t_min_f <- sum(df_temp$team_fc[df_temp$opp_name == nw_div[3]])/(length((df_temp$team_fc[df_temp$opp_name == nw_div[3]]))*9*4)
  t_min_1 <- sum(df_temp$team_first_q[df_temp$opp_name == nw_div[3]])/(length((df_temp$team_first_q[df_temp$opp_name == nw_div[3]]))*9)
  t_min_2 <- sum(df_temp$team_second_q[df_temp$opp_name == nw_div[3]])/(length((df_temp$team_second_q[df_temp$opp_name == nw_div[3]]))*9)
  t_min_3 <- sum(df_temp$team_third_q[df_temp$opp_name == nw_div[3]])/(length((df_temp$team_third_q[df_temp$opp_name == nw_div[3]]))*9)
  t_min_4 <- sum(df_temp$team_fourth_q[df_temp$opp_name == nw_div[3]])/(length((df_temp$team_fourth_q[df_temp$opp_name == nw_div[3]]))*9)
  
  
  #UTA
  t_uta_f <- sum(df_temp$team_fc[df_temp$opp_name == nw_div[4]])/(length((df_temp$team_fc[df_temp$opp_name == nw_div[4]]))*9*4)
  t_uta_1 <- sum(df_temp$team_first_q[df_temp$opp_name == nw_div[4]])/(length((df_temp$team_first_q[df_temp$opp_name == nw_div[4]]))*9)
  t_uta_2 <- sum(df_temp$team_second_q[df_temp$opp_name == nw_div[4]])/(length((df_temp$team_second_q[df_temp$opp_name == nw_div[4]]))*9)
  t_uta_3 <- sum(df_temp$team_third_q[df_temp$opp_name == nw_div[4]])/(length((df_temp$team_third_q[df_temp$opp_name == nw_div[4]]))*9)
  t_uta_4 <- sum(df_temp$team_fourth_q[df_temp$opp_name == nw_div[4]])/(length((df_temp$team_fourth_q[df_temp$opp_name == nw_div[4]]))*9)
  
  #DEN
  t_den_f <- sum(df_temp$team_fc[df_temp$opp_name == nw_div2[5]])/(length((df_temp$team_fc[df_temp$opp_name == nw_div2[5]]))*9*4)
  t_den_1 <- sum(df_temp$team_first_q[df_temp$opp_name == nw_div2[5]])/(length((df_temp$team_first_q[df_temp$opp_name == nw_div2[5]]))*9)
  t_den_2 <- sum(df_temp$team_second_q[df_temp$opp_name == nw_div2[5]])/(length((df_temp$team_second_q[df_temp$opp_name == nw_div2[5]]))*9)
  t_den_3 <- sum(df_temp$team_third_q[df_temp$opp_name == nw_div2[5]])/(length((df_temp$team_third_q[df_temp$opp_name == nw_div2[5]]))*9)
  t_den_4 <- sum(df_temp$team_fourth_q[df_temp$opp_name == nw_div2[5]])/(length((df_temp$team_fourth_q[df_temp$opp_name == nw_div2[5]]))*9)
  
  df_temp <- data.frame("NBA" = c(t_nba_f,t_nba_1,t_nba_2,t_nba_3,t_nba_4), 
                        'POR' = c(t_por_f,t_por_1,t_por_2,t_por_3,t_por_4), 
                        "OKC" = c(t_okc_f,t_okc_1,t_okc_2,t_okc_3,t_okc_4), 
                        "MIN" = c(t_min_f, min_1, min_2, min_3, min_4), 
                        "UTA" = c(t_uta_f,t_uta_1,t_uta_2,t_uta_3,t_uta_4), 
                        "DEN" = c(t_den_f,t_den_1,t_den_2,t_den_3,t_den_4))
  
  
  assign(paste0('df_lam_', tolower(substr(team, 1, 3))), df_temp, envir = .GlobalEnv)
}

