
# load packages---------------------------------
library(tidyverse)
library(rethinking)

# load data---------------------------
football_original <- read.csv("FootballEurope.csv", stringsAsFactors = F)

# should have dataset football_original with n=9127

# data downloaded from 
# https://www.kaggle.com/jangot/ligue1-match-statistics/version/3
# by Jemilu Mohammed
# acknowledged scraped source is whoscored.com
# downloaded 6th July 2017 (version 3)
# licence CC0: Public Domain

# separate home and away formations, i.e. one formation per row (long format)
f <- gather(football_original, homeFormation, awayFormation, key = "homeaway", value = "formation")
f$homeaway[f$homeaway == "homeFormation"] <- "home"
f$homeaway[f$homeaway == "awayFormation"] <- "away"
f$homeaway <- factor(f$homeaway)


# OPTIONAL: create dummy data------------------------------
# keep overall structure, with same divisions, managers, dates, homeaway
# generate formations in proportion to their actual frequency in the dataset
# randomise win/draw/loss: need to do this in original wide dataset otherwise will have non-paired results (e.g. every win needs to have a corresponding loss in the long dataset)

# # create football_dummy with same structure as football_original
# football_dummy <- read.csv("FootballEurope.csv", stringsAsFactors = F)
# 
# # dummy scores
# N <- nrow(football_dummy)
# football_dummy$homeGoalFT <- sample(0:10, N, replace = T, prob = table(football_dummy$homeGoalFT) / N)
# football_dummy$awayGoalFT <- sample(0:9, N, replace = T, prob = table(football_dummy$awayGoalFT) / N)
# 
# # dummy formations
# football_dummy$homeFormation <- sample(levels(as.factor(football_dummy$homeFormation)), N, replace = T, prob = table(football_dummy$homeFormation) / N)
# football_dummy$awayFormation <- sample(levels(as.factor(football_dummy$awayFormation)), N, replace = T, prob = table(football_dummy$awayFormation) / N)
# 
# # now run below command and repeat subsequent formatting and predictors
# f <- gather(football_dummy, homeFormation, awayFormation, key = "homeaway", value = "formation")
# f$homeaway[f$homeaway == "homeFormation"] <- "home"
# f$homeaway[f$homeaway == "awayFormation"] <- "away"
# f$homeaway <- factor(f$homeaway)

# various preprocessing-------------------------

# combine 343 and 343d
f$formation[f$formation == "343d"] <- "343"

# code 'result' based on homeGoalFT and awayGoalFT
f$result <- NA

f$result[f$homeGoalFT == f$awayGoalFT] <- "draw"
f$result[(f$homeGoalFT > f$awayGoalFT) & f$homeaway == "home"] <- "win"
f$result[(f$homeGoalFT < f$awayGoalFT) & f$homeaway == "home"] <- "loss"
f$result[(f$homeGoalFT > f$awayGoalFT) & f$homeaway == "away"] <- "loss"
f$result[(f$homeGoalFT < f$awayGoalFT) & f$homeaway == "away"] <- "win"

# collect team, managerName and goalFT to separate home and away stats
f$team <- ifelse(f$homeaway == "home", f$homeTeam, f$awayTeam)
f$manager <- ifelse(f$homeaway == "home", f$homeManagerName, f$awayManagerName)
f$goalsScored <- ifelse(f$homeaway == "home", f$homeGoalFT, f$awayGoalFT)
f$goalsConceded <- ifelse(f$homeaway == "home", f$awayGoalFT, f$homeGoalFT)

# keep relevant variables, discard the rest
f <- select(f, date, division, homeaway, formation, team, manager, goalsScored, goalsConceded, result)

# typos / inconsistencies
f$manager[f$manager == "Arsène Wenger"] <- "Arsene Wenger"
f$manager[f$manager == "André Villas-Boas"] <- "Andre Villas-Boas"
f$manager[f$manager == "Gustavo Poyet"] <- "Gus Poyet"
f$manager[f$manager == "Louis van Gaal"] <- "Louis Van Gaal"
f$manager[f$manager == "Maurizio Pellegrino"] <- "Mauricio Pellegrino"
f$manager[f$manager == "Rafael Benítez"] <- "Rafael Benitez"

# fill in two blanks (checked via wikipedia and sky sports)
f$manager[f$manager == ""] <- "François Ciccolini"
f$formation[is.na(f$formation)] <- "4231"

# make formations, divisions etc. factors
f$homeaway <- factor(f$homeaway) 
f$division <- factor(f$division) 
f$formation <- factor(f$formation)
f$team <- factor(f$team)
f$manager <- factor(f$manager)
f$result <- factor(f$result)

# make date a Date variable
f$date <- as.Date(f$date)

# add season variable; useful for calculating e.g. strength during a season for a team
# obtained from wikipedia
f$season <- NA

f$season[f$division == "Bundesliga" & f$date > "2012-08-23" & f$date < "2013-05-19"] <- 1
f$season[f$division == "Bundesliga" & f$date > "2013-08-08" & f$date < "2014-05-11"] <- 2
f$season[f$division == "Bundesliga" & f$date > "2014-08-21" & f$date < "2015-05-24"] <- 3
f$season[f$division == "Bundesliga" & f$date > "2015-08-13" & f$date < "2016-05-15"] <- 4
f$season[f$division == "Bundesliga" & f$date > "2016-08-25" & f$date < "2017-05-21"] <- 5

f$season[f$division == "EPL" & f$date > "2012-08-17" & f$date < "2013-05-20"] <- 1
f$season[f$division == "EPL" & f$date > "2013-08-16" & f$date < "2014-05-12"] <- 2
f$season[f$division == "EPL" & f$date > "2014-08-15" & f$date < "2015-05-25"] <- 3
f$season[f$division == "EPL" & f$date > "2015-08-07" & f$date < "2016-05-18"] <- 4
f$season[f$division == "EPL" & f$date > "2016-08-12" & f$date < "2017-05-22"] <- 5

f$season[f$division == "La_Liga" & f$date > "2012-08-01" & f$date < "2013-06-02"] <- 1
f$season[f$division == "La_Liga" & f$date > "2013-08-01" & f$date < "2014-05-31"] <- 2
f$season[f$division == "La_Liga" & f$date > "2014-08-01" & f$date < "2015-05-31"] <- 3
f$season[f$division == "La_Liga" & f$date > "2015-08-01" & f$date < "2016-05-31"] <- 4
f$season[f$division == "La_Liga" & f$date > "2016-08-01" & f$date < "2017-05-31"] <- 5

f$season[f$division == "Ligue_1" & f$date > "2012-08-01" & f$date < "2013-06-02"] <- 1
f$season[f$division == "Ligue_1" & f$date > "2013-08-01" & f$date < "2014-05-31"] <- 2
f$season[f$division == "Ligue_1" & f$date > "2014-08-01" & f$date < "2015-05-31"] <- 3
f$season[f$division == "Ligue_1" & f$date > "2015-08-01" & f$date < "2016-05-31"] <- 4
f$season[f$division == "Ligue_1" & f$date > "2016-08-01" & f$date < "2017-05-31"] <- 5

f$season[f$division == "Serie_A" & f$date > "2012-08-01" & f$date < "2013-06-02"] <- 1
f$season[f$division == "Serie_A" & f$date > "2013-08-01" & f$date < "2014-05-31"] <- 2
f$season[f$division == "Serie_A" & f$date > "2014-08-01" & f$date < "2015-06-01"] <- 3
f$season[f$division == "Serie_A" & f$date > "2015-08-01" & f$date < "2016-05-31"] <- 4
f$season[f$division == "Serie_A" & f$date > "2016-08-01" & f$date < "2017-05-31"] <- 5

# make season a factor to avoid treating as continuous
f$season <- factor(f$season)

# create 4231 variable 0 or 1
f$fourtwothreeone <- ifelse(f$formation == 4231, 1, 0)

# create win variable 0 or 1
f$win <- ifelse(f$result == "win", 1, 0)

# reduce German mid-season breaks to 10 days, in line with other leagues (except EPL), to make X-day window work
# season 1 mid-break ends 2012-12-16 starts 2013-01-18 (33 days, reduce by 23)
f$date[f$division == "Bundesliga" & f$season == 1 & f$date > "2012-12-16"] <- f$date[f$division == "Bundesliga" & f$season == 1 & f$date > "2012-12-16"] - 23

# s2 ends 2013-12-22 starts 2014-01-24 (33 days, reduce by 23)
f$date[f$division == "Bundesliga" & f$season == 2 & f$date > "2013-12-22"] <- f$date[f$division == "Bundesliga" & f$season == 2 & f$date > "2013-12-22"] - 23

# s3 ends 2014-12-21 starts 2015-01-30 (40 days, reduce by 30)
f$date[f$division == "Bundesliga" & f$season == 3 & f$date > "2014-12-21"] <- f$date[f$division == "Bundesliga" & f$season == 3 & f$date > "2014-12-21"] - 30

# s4 ends 2015-12-20 starts 2016-01-22 (33 days, reduce by 23)
f$date[f$division == "Bundesliga" & f$season == 4 & f$date > "2015-12-20"] <- f$date[f$division == "Bundesliga" & f$season == 4 & f$date > "2015-12-20"] - 23

# s5 ends 2016-12-21 starts 2017-01-20 (30 days, reduce by 20)
f$date[f$division == "Bundesliga" & f$season == 5 & f$date > "2016-12-21"] <- f$date[f$division == "Bundesliga" & f$season == 5 & f$date > "2016-12-21"] - 20


# check: all cases are complete
sum(!complete.cases(f))

# should have dataset f with n=18254


# create predictors--------------------------------------

# time window: number of days prior to game i to build variables
X <- 30

# create empty predictor variables
f$pers_use <- NA  # frequency of that manager's use of 4231, for that season and division in the X-day window, centred on 0.5
f$pop_use <- NA  # use frequency of 4231 in that division and season in the X-day window, centred on 0.5
f$pers_win <- NA  # manager's win rate with 4231 in that division and season in the X-day window, centred on mean win rate of non4231
f$pop_win <- NA  # division-wide win rate with 4231 in that season in the X-day window, centred on mean win rate of non4231
f$pers_win_all <- NA  # manager's win rate for all their games, centred on mean win rate for all managers in that season and division
f$team_strength <- NA  # proportion of games won by that team in a season, centred on mean win rate of all teams in that league

f$n_division <- NA  # variable to check how many games in window for game i
f$n_manager <- NA  # variable to check how many games in window for manager of team in game i

# cycle through each match and create predictors
for (i in 1:nrow(f)) {
  
  # first day of season for game i
  first_day <- min(f$date[f$season == f$season[i] & f$division == f$division[i]])
  
  # first X days remain NA
  if (f$date[i] - X > first_day) {
    
    window_division <- f$date < f$date[i] & f$date > (f$date[i] - X) & f$season == f$season[i] & f$division == f$division[i]
    window_manager <- window_division & f$manager == f$manager[i] & f$team == f$team[i]
    
    # number of games in previous X days (validity check, not used in analysis)
    f$n_division[i] <- sum(window_division)
    # and by that manager for that team in the window
    f$n_manager[i] <- sum(window_manager)
    
    # mean games played using 4231 in window by that manager for that team
    f$pers_use[i] <- mean(f$fourtwothreeone[window_manager]) - 0.5
    
    # mean games played using 4231 in window in that division
    f$pop_use[i] <- mean(f$fourtwothreeone[window_division]) - 0.5
    
    # mean games won with 4231 by that manager in the window, relative to non4231 win rate
    f$pers_win[i] <- mean(f[window_manager,]$win == 1 & f[window_manager,]$fourtwothreeone == 1) - mean(f[window_manager,]$win == 1 & f[window_manager,]$fourtwothreeone == 0)
    
    # mean number of games in which 4231 was played that were won, relative to mean number of non-4231 games that were won
    f$pop_win[i] <- mean(f[window_division & f$fourtwothreeone == 1,]$win) - mean(f[window_division & f$fourtwothreeone == 0,]$win)
    
    # mean games won by that manager in the window, relative to mean win rate of all managers
    f$pers_win_all[i] <- mean(f[window_manager,]$win) - mean(f[window_division,]$win)
    
    # mean games won by that team in the season, relative to overall win rate by all teams (not windowed)
    f$team_strength[i] <-   mean(f[f$team == f$team[i] & f$season == f$season[i],]$win) - mean(f[f$division == f$division[i] & f$season == f$season[i],]$win)
      
    }
  
}

# create use*win interaction variables
f$pers_useXpers_win <- f$pers_use * f$pers_win
f$pers_useXpers_win_all <- f$pers_use * f$pers_win_all
f$pop_useXpop_win <- f$pop_use * f$pop_win
f$pop_useXpers_win_all <- f$pop_use * f$pers_win_all

# remove temp vars
rm(window_manager, window_division, first_day, i)  

# retain complete cases only
football_full <- f
f <- football_full[complete.cases(football_full), ]
rownames(f) <- NULL  # reset row names
f$manager <- factor(f$manager)  # re-factor as some managers have been lost, messes with rethinking later otherwise

# gives n=16003 with X=30

# m1: null model with varying effects for division and manager, with homeaway and team_strength------
dat_list <- list(
  fourtwothreeone = as.integer(f$fourtwothreeone),
  homeaway = as.integer(ifelse(f$homeaway == "home", 1, 2)), # home=1, away=2,
  manager_id = as.integer(f$manager),
  division_id = as.integer(f$division),
  team_strength = f$team_strength
)

m1 <- ulam(
  alist(
    fourtwothreeone ~ dbinom( 1 , p ) ,
    logit(p) <- a_homeaway[homeaway] + b_team_strength*team_strength + a_manager[manager_id]*sigma_manager + a_division[division_id]*sigma_division,
    a_homeaway[homeaway] ~ dnorm( 0 , 1 ) ,
    b_team_strength ~ dnorm( 0 , 1 ) ,
    a_manager[manager_id] ~ dnorm( 0 , 1 ) ,
    a_division[division_id] ~ dnorm( 0 , 1 ) ,
    sigma_manager ~ dexp( 1 ) ,
    sigma_division ~ dexp( 1 )
  ) ,
  data=dat_list , chains=4 , log_lik = T, cores=4, iter = 2000 )

traceplot(m1)
pairs(m1, depth = 2, pars = c("a_homeaway", "b_team_strength", "sigma_manager", "sigma_division"))
precis(m1, depth = 2, pars = c("a_homeaway", "b_team_strength", "a_division", "sigma_manager", "sigma_division"))

#                  mean   sd  5.5% 94.5% n_eff Rhat
# a_homeaway[1]   -0.53 0.47 -1.23  0.26   724 1.00
# a_homeaway[2]   -0.64 0.47 -1.34  0.13   729 1.00
# b_team_strength -0.01 0.26 -0.42  0.39  1438 1.00
# a_division[1]    0.57 0.48 -0.12  1.39   781 1.00
# a_division[2]   -0.70 0.39 -1.34 -0.11   901 1.00
# a_division[3]    0.63 0.49 -0.08  1.48   727 1.00
# a_division[4]   -0.16 0.40 -0.79  0.47   694 1.00
# a_division[5]   -1.74 0.58 -2.72 -0.87  1291 1.00
# sigma_manager    1.85 0.10  1.70  2.01   468 1.01
# sigma_division   1.26 0.46  0.72  2.10  1242 1.00

post <- extract.samples(m1)

inv_logit(colMeans(post$a_homeaway))  # slightly more likely to play 4231 at home
# [1] 0.3701951 0.3444764

# much lower prob of 4231 in Serie A, highest in Bundesliga and La Liga
data.frame(division = levels(f$division), prob_4231 = inv_logit(colMeans(post$a_division)))
#     division prob_4231
# 1 Bundesliga 0.6393390
# 2        EPL 0.3317791
# 3    La_Liga 0.6525862
# 4    Ligue_1 0.4589095
# 5    Serie_A 0.1494498

# same for managers; lots of variation
data.frame(division = levels(f$manager), prob_4231 = inv_logit(colMeans(post$a_manager)))

# save as RDS file, use readRDS to load
saveRDS(m1, file="modelfits/m1.rds")
#m1 <- readRDS("modelfits/m1.rds")

# m2: personal variables model, varying slopes with pers_use------------------------
# non-centred parameterisation
dat_list <- list(
  fourtwothreeone = as.integer(f$fourtwothreeone),
  homeaway = as.integer(ifelse(f$homeaway == "home", 1, 2)), # home=1, away=2,
  team_strength = f$team_strength,
  manager_id = as.integer(f$manager),
  division_id = as.integer(f$division),
  pers_use = f$pers_use,
  pers_win = f$pers_win,
  pers_useXpers_win = f$pers_useXpers_win,
  pers_useXpers_win_all = f$pers_useXpers_win_all
)

m2 <- ulam(
  alist(
    fourtwothreeone ~ dbinom( 1 , p ) ,
    logit(p) <- a_homeaway[homeaway] + b_team_strength*team_strength + a_manager[manager_id,1] + a_manager[manager_id,2]*pers_use + a_division[division_id,1] + a_division[division_id,2]*pers_use + b_pers_use*pers_use + b_pers_win*pers_win + b_pers_useXpers_win*pers_useXpers_win + b_pers_useXpers_win_all*pers_useXpers_win_all,
    
    a_homeaway[homeaway] ~ dnorm( 0 , 1 ) ,
    b_team_strength ~ dnorm( 0 , 1 ) ,
    
    # adaptive priors - non-centered
    transpars> matrix[manager_id,2]:a_manager <-
      compose_noncentered( sigma_manager , L_Rho_manager , z_manager ),
    transpars> matrix[division_id,2]:a_division <-
      compose_noncentered( sigma_division , L_Rho_division , z_division ),
    
    matrix[2,manager_id]:z_manager ~ normal( 0 , 1 ),
    matrix[2,division_id]:z_division ~ normal( 0 , 1 ),
    
    b_pers_use ~ dnorm( 0 , 1 ) ,
    b_pers_win ~ dnorm( 0 , 1 ) ,
    b_pers_useXpers_win ~ dnorm( 0 , 1 ) ,
    b_pers_useXpers_win_all ~ dnorm( 0 , 1 ) ,
    
    vector[2]:sigma_manager ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho_manager ~ lkj_corr_cholesky( 2 ),
    vector[2]:sigma_division ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho_division ~ lkj_corr_cholesky( 2 )
    
  ) ,
  data=dat_list , chains=4 , log_lik = T, cores=4, iter = 2000 )

pairs(m2, pars = c("a_homeaway","b_team_strength", "b_pers_use", "b_pers_win", "b_pers_useXpers_win", "b_pers_useXpers_win_all"))

precis(m2, depth = 2)
#                          mean   sd  5.5% 94.5% n_eff Rhat
# a_homeaway[1]           -0.10 0.24 -0.46  0.30   900    1
# a_homeaway[2]           -0.22 0.24 -0.58  0.18   923    1
# b_team_strength          0.08 0.25 -0.31  0.49  2969    1
# b_pers_use               2.28 0.67  0.99  3.16  1291    1
# b_pers_win               0.82 0.13  0.61  1.03  4102    1
# b_pers_useXpers_win     -0.52 0.21 -0.85 -0.19  5010    1
# b_pers_useXpers_win_all  0.06 0.34 -0.48  0.61  3979    1
# sigma_manager[1]         0.65 0.07  0.55  0.76   820    1
# sigma_manager[2]         1.33 0.14  1.11  1.56  1141    1
# sigma_division[1]        0.50 0.21  0.26  0.90  1193    1
# sigma_division[2]        1.32 0.61  0.62  2.44  1409    1


# save as RDS file, use readRDS to load
saveRDS(m2, file="modelfits/m2.rds")
#m2 <- readRDS("modelfits/m2.rds")


# m3: population variables model, varying slopes with pop use-----------------
# non-centred parameterisation
dat_list <- list(
  fourtwothreeone = as.integer(f$fourtwothreeone),
  homeaway = as.integer(ifelse(f$homeaway == "home", 1, 2)), # home=1, away=2
  team_strength = f$team_strength,
  manager_id = as.integer(f$manager),
  division_id = as.integer(f$division),
  pop_use = f$pop_use,
  pop_win = f$pop_win,
  pop_useXpop_win = f$pop_useXpop_win,
  pop_useXpers_win_all = f$pop_useXpers_win_all
)

m3 <- ulam(
  alist(
    fourtwothreeone ~ dbinom( 1 , p ) ,
    logit(p) <- a_homeaway[homeaway] + b_team_strength*team_strength + a_manager[manager_id,1] + a_manager[manager_id,2]*pop_use + a_division[division_id,1] + a_division[division_id,2]*pop_use + b_pop_use*pop_use + b_pop_win*pop_win + b_pop_useXpop_win*pop_useXpop_win + b_pop_useXpers_win_all*pop_useXpers_win_all,
    
    a_homeaway[homeaway] ~ dnorm( 0 , 1 ) ,
    b_team_strength ~ dnorm( 0 , 1 ) ,
    
    # adaptive priors - non-centered
    transpars> matrix[manager_id,2]:a_manager <-
      compose_noncentered( sigma_manager , L_Rho_manager , z_manager ),
    transpars> matrix[division_id,2]:a_division <-
      compose_noncentered( sigma_division , L_Rho_division , z_division ),
    
    matrix[2,manager_id]:z_manager ~ normal( 0 , 1 ),
    matrix[2,division_id]:z_division ~ normal( 0 , 1 ),
    
    b_pop_use ~ dnorm( 0 , 1 ) ,
    b_pop_win ~ dnorm( 0 , 1 ) ,
    b_pop_useXpop_win ~ dnorm( 0 , 1 ) ,
    b_pop_useXpers_win_all ~ dnorm( 0 , 1 ) ,
    
    vector[2]:sigma_manager ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho_manager ~ lkj_corr_cholesky( 2 ),
    vector[2]:sigma_division ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho_division ~ lkj_corr_cholesky( 2 )
    
  ) ,
  data=dat_list , chains=4 , log_lik = T, cores=4, iter = 2000 )

pairs(m3, pars = c("a_homeaway", "b_team_strength", "b_pop_use", "b_pop_win", "b_pop_useXpop_win", "b_pop_useXpers_win_all"))  

precis(m3, depth = 2)
#                         mean   sd  5.5% 94.5% n_eff Rhat
# a_homeaway[1]          -0.16 0.35 -0.72  0.41   927 1.01
# a_homeaway[2]          -0.28 0.35 -0.84  0.29   928 1.01
# b_team_strength        -0.08 0.30 -0.55  0.40  2745 1.00
# b_pop_use               1.54 1.03 -0.12  3.18  2685 1.00
# b_pop_win               0.34 0.19  0.03  0.63  4737 1.00
# b_pop_useXpop_win      -0.46 0.64 -1.49  0.55  4863 1.00
# b_pop_useXpers_win_all  0.60 0.45 -0.12  1.33  6415 1.00
# sigma_manager[1]        1.89 0.12  1.71  2.09   765 1.00
# sigma_manager[2]        5.73 0.46  5.01  6.49  1007 1.00
# sigma_division[1]       0.70 0.30  0.35  1.24  2091 1.00
# sigma_division[2]       3.58 1.22  1.84  5.75  1912 1.00


# save as RDS file, use readRDS to load
saveRDS(m3, file="modelfits/m3.rds")
#m3 <- readRDS("modelfits/m3.rds")

# m4: full model, varying slopes with pers & pop use-----------------
# non-centred parameterisation
dat_list <- list(
  fourtwothreeone = as.integer(f$fourtwothreeone),
  homeaway = as.integer(ifelse(f$homeaway == "home", 1, 2)), # home=1, away=2
  team_strength = f$team_strength,
  manager_id = as.integer(f$manager),
  division_id = as.integer(f$division),
  pers_use = f$pers_use,
  pers_win = f$pers_win,
  pers_useXpers_win = f$pers_useXpers_win,
  pers_useXpers_win_all = f$pers_useXpers_win_all,
  pop_use = f$pop_use,
  pop_win = f$pop_win,
  pop_useXpop_win = f$pop_useXpop_win,
  pop_useXpers_win_all = f$pop_useXpers_win_all
)

m4 <- ulam(
  alist(
    fourtwothreeone ~ dbinom( 1 , p ) ,
    
    logit(p) <- a_homeaway[homeaway] + b_team_strength*team_strength + a_manager[manager_id,1] + a_manager[manager_id,2]*pers_use + a_manager[manager_id,3]*pop_use + a_division[division_id,1] + a_division[division_id,2]*pers_use + a_division[division_id,3]*pop_use + b_vars,
    
    b_vars <- b_pers_use*pers_use + b_pers_win*pers_win + b_pers_useXpers_win*pers_useXpers_win + b_pers_useXpers_win_all*pers_useXpers_win_all + b_pop_use*pop_use + b_pop_win*pop_win + b_pop_useXpop_win * pop_useXpop_win + b_pop_useXpers_win_all * pop_useXpers_win_all,
    
    a_homeaway[homeaway] ~ dnorm( 0 , 1 ) ,
    b_team_strength ~ dnorm( 0 , 1 ) ,
    
    # adaptive priors - non-centered
    transpars> matrix[manager_id,3]:a_manager <-
      compose_noncentered( sigma_manager , L_Rho_manager , z_manager ),
    transpars> matrix[division_id,3]:a_division <-
      compose_noncentered( sigma_division , L_Rho_division , z_division ),
    
    matrix[3,manager_id]:z_manager ~ normal( 0 , 1 ),
    matrix[3,division_id]:z_division ~ normal( 0 , 1 ),
    
    b_pers_use ~ dnorm( 0 , 1 ) ,
    b_pers_win ~ dnorm( 0 , 1 ) ,
    b_pers_useXpers_win ~ dnorm( 0 , 1 ) ,
    b_pers_useXpers_win_all ~ dnorm( 0 , 1 ) ,
    b_pop_use ~ dnorm( 0 , 1 ) ,
    b_pop_win ~ dnorm( 0 , 1 ) ,
    b_pop_useXpop_win ~ dnorm( 0 , 1 ) ,
    b_pop_useXpers_win_all ~ dnorm( 0 , 1 ) ,
    
    vector[3]:sigma_manager ~ dexp(1),
    cholesky_factor_corr[3]:L_Rho_manager ~ lkj_corr_cholesky( 2 ),
    vector[3]:sigma_division ~ dexp(1),
    cholesky_factor_corr[3]:L_Rho_division ~ lkj_corr_cholesky( 2 )
    
  ) ,
  data=dat_list , chains=4 , log_lik = T, cores=4, iter = 2000 )

pairs(m4, pars = c("a_homeaway", "b_team_strength", "b_pers_use", "b_pers_win", "b_pers_useXpers_win", "b_pers_useXpers_win_all", "b_pop_use", "b_pop_win", "b_pop_useXpop_win", "b_pop_useXpers_win_all"))

precis(m4, depth = 2)
#                          mean   sd  5.5% 94.5% n_eff Rhat
# a_homeaway[1]           -0.01 0.17 -0.27  0.24  1150 1.01
# a_homeaway[2]           -0.14 0.17 -0.41  0.11  1156 1.01
# b_team_strength          0.03 0.27 -0.39  0.46  3150 1.00
# b_pers_use               2.08 0.63  0.96  2.92  1654 1.00
# b_pers_win               0.84 0.13  0.63  1.06  4788 1.00
# b_pers_useXpers_win     -0.64 0.21 -0.98 -0.30  5283 1.00
# b_pers_useXpers_win_all  0.11 0.35 -0.46  0.66  4518 1.00
# b_pop_use                1.34 0.48  0.55  2.03  2284 1.00
# b_pop_win               -0.11 0.20 -0.43  0.22  5007 1.00
# b_pop_useXpop_win       -0.62 0.67 -1.69  0.45  5673 1.00
# b_pop_useXpers_win_all  -1.07 0.51 -1.87 -0.24  6213 1.00
# sigma_manager[1]         0.71 0.07  0.60  0.83  1133 1.00
# sigma_manager[2]         1.26 0.14  1.04  1.50  1506 1.00
# sigma_manager[3]         2.00 0.42  1.35  2.67   730 1.00
# sigma_division[1]        0.27 0.19  0.05  0.62  1637 1.00
# sigma_division[2]        1.25 0.59  0.57  2.32  1281 1.00
# sigma_division[3]        0.69 0.53  0.06  1.66  1735 1.00



# save as RDS file, use readRDS to load
saveRDS(m4, file="modelfits/m4.rds")
#m4 <- readRDS("modelfits/m4.rds")



# test hypotheses----------------------

# H1. the full model (m4) will be the best supported model as measured using WAIC, relative to personal-only, population-only and null models (m1, m2 and m3)
compare(m1, m2, m3, m4)

# supported
#       WAIC pWAIC  dWAIC weight     SE    dSE
# m4 12273.1 338.3    0.0      1 144.03     NA
# m2 12346.3 298.2   73.2      0 144.19  15.16
# m3 13547.7 356.9 1274.6      0 137.23  88.05
# m1 14331.9 251.0 2058.8      0 135.11 101.69

# H1. in the full model, there are effects of (a) personal 4231 use and (b) win rate, (c) population 4231 use and (d) win rate, and interactions between (e) personal 4231 use and win, and between (f) population 4231 use and win rates. Effects are indicated by the parameter estimates' 89% CI not including zero in the full model.
precis(m4, prob = 0.89)

#                          mean   sd  5.5% 94.5% n_eff Rhat
# b_team_strength          0.03 0.27 -0.39  0.46  3150    1
# b_pers_use               2.08 0.63  0.96  2.92  1654    1  (a) yes
# b_pers_win               0.84 0.13  0.63  1.06  4788    1  (b) yes
# b_pers_useXpers_win     -0.64 0.21 -0.98 -0.30  5283    1  (e) yes
# b_pers_useXpers_win_all  0.11 0.35 -0.46  0.66  4518    1
# b_pop_use                1.34 0.48  0.55  2.03  2284    1  (c) yes
# b_pop_win               -0.11 0.20 -0.43  0.22  5007    1  (d) no
# b_pop_useXpop_win       -0.62 0.67 -1.69  0.45  5673    1  (f) no
# b_pop_useXpers_win_all  -1.07 0.51 -1.87 -0.24  6213    1

# plot effects of b variables
post <- extract.samples(m4)

# plot of pers_use: when pers 4231 use is low, then win rate makes a (small) difference
b.seq <- seq( from=0 , to=1 , by=0.01 )
logodds <- matrix(nrow = 4000, ncol = length(b.seq))
for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pers_use*b.seq[i]
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
plot( NULL , xlab="personal 4231 use" , ylab="probability of 4231" ,
      ylim = c(0,1) , xlim = c(0,1) )
lines( b.seq , p_mu , col = "black", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("black",0.2))
#text(0.5,0.7,"average performance")

# add pers use at +W% and -W% win rate compared to non-4231 win rate
W <- 0.5
for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pers_use * b.seq[i] + post$b_pers_win * W + post$b_pers_useXpers_win * b.seq[i] * W
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "chocolate1", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("chocolate1",0.2))
text(0.4, 0.9, paste("+",W*100,"% performance", sep = ""), col = "chocolate1")

for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pers_use * b.seq[i] + post$b_pers_win * -W + post$b_pers_useXpers_win * b.seq[i] * -W
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "deepskyblue2", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("deepskyblue2",0.2))
text(0.6, 0.4, paste("-",W*100,"% performance", sep = ""), col = "deepskyblue2")

# plot of pop_use: slight difference due to win rate at high pop 4231 use, but opposite to expected (higher performance->lower use)
b.seq <- seq( from=0 , to=1 , by=0.01 )
logodds <- matrix(nrow = 4000, ncol = length(b.seq))
for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pop_use*b.seq[i]
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
plot( NULL , xlab="population 4231 use" , ylab="probability of 4231" ,
      ylim = c(0,1) , xlim = c(0,1) )
lines( b.seq , p_mu , col = "black", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("black",0.2))
#text(0.5,0.6,"average performance")

# add pop use at +W% and -W% win rate compared to non-4231 win rate
for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pop_use * b.seq[i] + post$b_pop_win * W + post$b_pop_useXpop_win * b.seq[i] * W
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "chocolate1", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("chocolate1",0.2))
text(0.7, 0.35, paste("+",W*100,"% performance", sep = ""), col = "chocolate1")

for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pop_use * b.seq[i] + post$b_pop_win * -W + post$b_pop_useXpop_win * b.seq[i] * -W
}
logodds <- inv_logit(logodds)

p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "deepskyblue2", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("deepskyblue2",0.2))
text(0.6, 0.9, paste("-",W*100,"% performance", sep = ""), col = "deepskyblue2")


# H2. The ratio of population : personal use is greater than 1, indicating greater reliance on social information than personal information
post <- extract.samples(m4)
round(c(mean(post$b_pop_use / post$b_pers_use), PI(post$b_pop_use / post$b_pers_use, prob = 0.89)), 2)
# No, opposite: mean 0.68, but very uncertain
#        5%  94% 
# 0.68 0.24 1.41 

# H3. There is more variation between (a) managers in personal info use, (b) managers in population info use, (c) divisions in personal info use, and (d) divisions in population info use, than there is in a model of dummy data with randomised formation and win rates across managers and divisions, as indicated by sigma_manager[2,3] and sigma_division[2,3]

post.d <- extract.samples(m4d) # run dummy model using code above
post.r <- extract.samples(m4)

sigma_diffs <- data.frame(
  diff_pers_manager=post.r$sigma_manager[,2] - post.d$sigma_manager[,2], 
  diff_pop_manager=post.r$sigma_manager[,3] - post.d$sigma_manager[,3], 
  diff_pers_division=post.r$sigma_division[,2] - post.d$sigma_division[,2], 
  diff_pop_division=post.r$sigma_division[,3] - post.d$sigma_division[,3])

precis(sigma_diffs)  # differences in all except population use & division
plot(precis(sigma_diffs))

# H4. There is an n-shaped relationship between information use ratio and personal win rate across managers, i.e. managers who combine personal and population information do better than managers who predominantly use one or the other. Model this with unit of analysis as a manager, predicting their personal win rate from their population:personal use ratio squared (negative b^2 coefficient in a quadratic polynomial)

post <- extract.samples(m4)

# mean pop use rate / mean pers use rate  NB ADDED INV_LOGIT UNLIKE PREREG VERSION
info_ratio <- apply(inv_logit(post$a_manager[,,3]), 2, mean) / apply(inv_logit(post$a_manager[,,2]), 2, mean)
# info_ratio <- standardize(info_ratio)  # standardise NB REMOVED THIS TOO

win_rate <- NA
for (i in 1:323) win_rate[i] <- mean(f[f$manager == levels(f$manager)[i],]$win)
win_rate <- standardize(win_rate)  # relative to mean win_rate

m <- data.frame(win_rate = win_rate, info_ratio = info_ratio, info_ratio_sq = info_ratio^2)

dat_list <- list(
  info_ratio = m$info_ratio,
  win_rate = m$win_rate,
  info_ratio_sq = m$info_ratio_sq
)

m.manager <- ulam(
  alist(
    win_rate ~ dnorm( mu , sigma ) ,
    mu <- a + b1*info_ratio + b2*info_ratio_sq,
    a ~ normal( 0 , 1.5 ),
    b1 ~ normal( 0 , 0.2),
    b2 ~ normal( -0.1 , 0.2),
    sigma ~ dexp( 1 )
  ) ,
  data=dat_list , chains=1 , log_lik = T, cores=4, iter = 1000 )

# prior predictive simulation: slightly more likely to be n-shaped, but still allow u-shaped
N <- 323
a <- rnorm(N, 0, 1.5)
b1 <- rnorm(N, 0, 0.2)
b2 <- rnorm(N, -0.1, 0.2)
plot( NULL , xlim=range(info_ratio) , ylim=range(win_rate) ,
      xlab="info_ratio" , ylab="win_rate" )
#xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b1[i]*x + b2[i]*x^2,
                        from=min(info_ratio) , to=max(info_ratio) , add=TRUE ,
                        col=col.alpha("black",0.2) )

precis(m.manager)  # H4 not supported: very slightly positive b2 coefficient. Mostly no relationship.
#        mean   sd  5.5% 94.5% n_eff Rhat
# a      0.02 0.12 -0.17  0.22   224 1.01
# b1    -0.15 0.15 -0.39  0.11   171 1.01
# b2     0.10 0.06  0.00  0.19   180 1.00
# sigma  1.00 0.04  0.94  1.06   291 1.00

# plot
ratio.seq <- seq( from=min(info_ratio) , to=max(info_ratio) , length.out=300 )
pred_dat <- list( info_ratio=ratio.seq , info_ratio_sq=ratio.seq^2 )
mu <- link( m.manager , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.win <- sim( m.manager , data=pred_dat )
win.PI <- apply( sim.win , 2 , PI , prob=0.89 )
plot( win_rate ~ info_ratio )
lines( ratio.seq , mu.mean )
shade( mu.PI , ratio.seq )
shade( win.PI , ratio.seq )


inv_logit(colMeans(post$a_homeaway))  # slightly more likely to play 4231 at home
# [1] 0.4987088 0.4661959

# plot of pers_use in diff divisions
col_list <- c("red","darkgreen","brown","darkseagreen1","dodgerblue1")

b.seq <- seq( from=0 , to=1 , by=0.01 )
logodds <- matrix(nrow = 4000, ncol = length(b.seq))
plot( NULL , xlab="pers 4231 use" , ylab="probability of 4231" ,
      ylim = c(0,1) , xlim = c(0,1) )

for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pers_use*b.seq[i]
}
logodds <- inv_logit(logodds)
p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "black", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("black",0.2))

for (j in 1:5) {
  for (i in 1:length(b.seq)) {
    logodds[,i] <- rowMeans(post$a_homeaway) + post$a_division[,j,1] + post$b_pers_use*b.seq[i] + post$a_division[,j,2]*b.seq[i]
  }
  logodds <- inv_logit(logodds)
  p_mu <- apply( logodds , 2 , mean )
  p_ci <- apply( logodds , 2 , PI )
  lines( b.seq , p_mu , col = col_list[j], lwd = 2)
  shade( p_ci , b.seq , col=col.alpha(col_list[j],0.2))
  text(0.1+j*.15,(j-1)*.05,levels(f$division)[j], col = col_list[j])
}


# plot of pop_use in diff divisions
b.seq <- seq( from=0 , to=1 , by=0.01 )
logodds <- matrix(nrow = 4000, ncol = length(b.seq))
plot( NULL , xlab="pop 4231 use" , ylab="probability of 4231" ,
      ylim = c(0,1) , xlim = c(0,1) )

for (i in 1:length(b.seq)) {
  logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pop_use*b.seq[i]
}
logodds <- inv_logit(logodds)
p_mu <- apply( logodds , 2 , mean )
p_ci <- apply( logodds , 2 , PI )
lines( b.seq , p_mu , col = "black", lwd = 2)
shade( p_ci , b.seq , col=col.alpha("black",0.2))

for (j in 1:5) {
  for (i in 1:length(b.seq)) {
    logodds[,i] <- rowMeans(post$a_homeaway) + post$b_pop_use*b.seq[i] + post$a_division[,j,1] + post$a_division[,j,3]*b.seq[i]
  }
  logodds <- inv_logit(logodds)
  p_mu <- apply( logodds , 2 , mean )
  p_ci <- apply( logodds , 2 , PI )
  lines( b.seq , p_mu , col = col_list[j], lwd = 2)
  shade( p_ci , b.seq , col=col.alpha(col_list[j],0.2))
  text(0.1+j*.15,(j-1)*.05,levels(f$division)[j], col = col_list[j])
}


