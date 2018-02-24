# fouls in last two minutes of close games
length(df[which(df$foul_quarter == '4th quarter'), 1])
  # 184475
length(df$date)
  # 625156
184475/625156
  # 29.50863% of fouls are in the 4th quarter

length(df[which(df$foul_quarter == '3rd quarter'), 1])
  # 158252
  # 25.314%

length(df[which(df$foul_quarter == '2nd quarter'), 1])
  # 158241
  # 25.31224%

length(df[which(df$foul_quarter == '1st quarter'), 1])
  # 124177
  # 19.86336

FQ <- which(df$foul_quarter == '4th quarter')

table(df$foul_type[FQ])/table(df$foul_type)
table(df$foul_type[FQ])['shooting']/length(df$foul_type[FQ])
table(df$foul_type)['shooting']/length(df$foul_type)

as.data.frame(table(df$foul_type[FQ]))
as.data.frame(table(df$foul_type))

t1 <- as.data.frame(table(df$foul_type[FQ]))
t2 <- as.data.frame(table(df$foul_type))


rbind(t1[1:4, ], data.frame(Var1 = 'elbow', Freq = 2, row.names = 5), 
      t1[5:19, ])

rev(row.names(t1[5:18, ])) <- as.character(c(19:6))


# 44.20111% of fouls in the 4th are shooting, 47.1887% of fouls
# in all are shooting

