team_names <- c("Utah Jazz", "Washington Wizards", "Boston Celtics", 
                "Denver Nuggets", "Memphis Grizzlies", "Milwaukee Bucks",
                "Philadelphia 76ers", "Sacramento Kings", "Cleveland Cavaliers",
                "Phoenix Suns", "Brooklyn Nets", "Dallas Mavericks", 
                "LA Clippers", "Miami Heat", "New Orleans Pelicans", 
                "New York Knicks", "Atlanta Hawks", "Minnesota Timberwolves",
                "Golden State Warriors", "Toronto Raptors", "Charlotte Hornets", 
                "Chicago Bulls", "Detroit Pistons", "Houston Rockets", 
                "Indiana Pacers", "Los Angeles Lakers", "Oklahoma City Thunder", 
                "Orlando Magic", "Portland Trail Blazers", "San Antonio Spurs")
team_names_search = paste(team_names, sep = "", collapse = "|")

team_abbr <- c("Jazz", "Wizards", "Celtics", "Nuggets", "Grizzlies", "Bucks",
               "76ers", "Kings", "Cavaliers", "Suns", "Nets", "Mavericks", 
               "Clippers", "Heat", "Pelicans", "Knicks", "Hawks", "Timberwolves",
               "Warriors", "Raptors", "Hornets", "Bulls", "Pistons", "Rockets",
               "Pacers", "Lakers", "Thunder", "Magic", "Trail Blazers", "Spurs")


teams <- data.frame(team_names, team_abbr)

clean_dat <- function(df, conf) {
  colnames(df) <- tolower(colnames(df))
  df <- df %>%
    mutate(team_full = str_extract(var.1, team_names_search),
           var.1 = gsub(team_names_search, "", var.1),
           team_abb = str_extract(var.1, "[A-Z]+"),
           current_seed = as.numeric(str_extract(var.1, "[0-9]+")),
           w_last10 = as.numeric(str_extract(l10, "[0-9]+")),
           l_last10 = str_extract(l10, "-[0-9]+"),
           l_last10 = as.numeric(gsub("\\-", "", l_last10)),
           pct_last10 = (w_last10)/(w_last10+l_last10)) %>%
    select(current_seed, team_abb, team_full, w, l, pct, w_last10, l_last10, pct_last10) %>%
    mutate(g_left = 82-w-l,
           conference = conf)
  
  return(df)
}

clean_dat538 <- function(teams) {
  tmp <- read_html("https://projects.fivethirtyeight.com/2023-nba-predictions/") %>%
    html_table()
  df <- tmp[[1]] 
  df <- df[, 4:8]
  colnames(df) <- c("team", "conference", "elo", "record_proj", "diff")
  
  df <- df %>%
    filter(team != "" & team != "Team") %>%
    mutate(team_abbr = str_extract(team, paste(teams$team_abbr, sep = "", collapse = "|"))) %>%
    left_join(teams, by = c("team_abbr"))
  
  df <- df %>%
    mutate(w_proj538 = as.numeric(str_extract(record_proj, "[0-9]+")),
           l_proj538 = str_extract(record_proj, "-[0-9]+"),
           l_proj538 = as.numeric(gsub("\\-", "", l_proj538))) %>%
    select(team_names, w_proj538, l_proj538) %>%
    rename(team_full = team_names) %>%
    mutate(pct_538 = w_proj538/82)
  return(df)
}

get_reqs <- function(df, tname, desired_seed=10, dat538, ass_538) {

  df <- left_join(df, dat538, by = "team_full")
  
  df_curr <- df %>%
    filter(team_full == tname)
  
  wins_curr = df_curr$w[1]
  loss_curr = df_curr$l[1]

  gleft_cur = df_curr$g_left[1]
  
  df <- df %>%
    filter(conference == df_curr$conference[1]) %>%
    mutate(w_pace = ceiling(pct*82),
           w_needed = w_pace - wins_curr)
  
  if(ass_538 == T) {
    df <- df %>%
      mutate(w_needed = w_proj538 - wins_curr, 
             pct = pct_538, 
             w = w_proj538,
             l = l_proj538) %>%
      arrange(desc(pct)) %>%
      mutate(proj_seed = 1:n())
  } else{
    df <- df %>%
      mutate(proj_seed = current_seed)
  }
  
  df <- df %>%
    mutate(pct_needed = w_needed/gleft_cur)
  
  df_needed <- df %>%
    filter(proj_seed == desired_seed)
  
  pct_needed = df_needed$pct_needed[1]
  
  seed_tobeat = which(df$pct>pct_needed)+1

  seed_tobeat = seed_tobeat[!is.na(seed_tobeat)]
  
  # print()
  if(df_curr$team_full == df_needed$team_full) {
    text = print("The selected team is already projected to be this seed.")
    
  } else if(pct_needed > 1){
    seed_tobeat = -99
    text = print("Not possible to achieve this seed given assumptions (see info panel).")
  } else if (pct_needed > max(df$pct, na.rm = T)) {
    text = print(paste0("Need to do better than the current ", 1, " seed (", 
                        df$team_full[1], ")."))
  } else{
    seed_tobeat = max(seed_tobeat)

    
    if(seed_tobeat==16) {
      text = print(paste0("This required win pct is below even the 15th conference seed (",
                   df$team_full[15], ")."))
    } 
    else { 
      text = print(paste0("This required win pct is better than the current ", seed_tobeat, " seed (", 
                   df$team_full[seed_tobeat], ")."))
      }
    
  }
  
  out <- list(df, df_curr, df_needed, text)
  names(out) <- c("data", "data_curr", "data_needed", "text")
  
  return(out)
}

scrape_sched <- function(team_curr, tms, df) {
  team_abb = team_curr$team_abb[1]

  sched <- read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", team_abb)) %>%
    html_table() %>%
    data.frame()
  
  colnames(sched) <- sched[1,]
  sched <- sched[-1,]
  start = grep("DATE", sched$DATE)+1
  sched <- sched[start:nrow(sched), 1:2]
  
  sched <- sched %>%
    mutate(opp = gsub("vs|@", "", OPPONENT))
  
  #Ghetto merge
  team_match = sapply(sched$opp, function(x) teams$team_names[str_detect(teams$team_names, x)])
  
  sched$team_full = team_match
  
  sched <- left_join(sched, df[, c("team_full", "pct")], by = "team_full")
  
  team_pct = team_curr$pct[1]

  sched <- sched %>%
    rename(opp_pct = pct) %>%
    mutate(proj_win_num = ifelse(opp_pct > team_pct, 0, 1),
           proj_win = ifelse(opp_pct > team_pct, "L", "W"),
           day = str_extract(DATE, "[0-9]+"),
           month = str_extract(DATE, "\\, [A-Za-z]+"),
           month = gsub(", ", "", month),
           month = match(month, month.abb),
           date = as.Date(paste("2023", month, day, sep = "-"))) %>%
    relocate(date, opp, opp_pct, proj_win)
  
  return(sched)
}

get_custom_wins <- function(df, curr, sch, full_dat) {
  proj_win = sum(sch$proj_win_num)
  
  total_win = curr$w + proj_win
  total_loss = 82-total_win
  
  proj_pct = total_win/82
  
  full_dat <- full_dat[full_dat$conference == curr$conference & 
                         full_dat$pct > proj_pct,]
  
  proj_seed = nrow(full_dat)+1
  
  
  
  custom_table <- df %>%
    select(current_seed, team_full, record, pct) %>%
    mutate(proj_record = paste0(total_win, "-", total_loss),
           proj_pct = total_win/82,
           proj_pct = format(round(proj_pct, 3), nsmall = 3),
           proj_seed = proj_seed)
  return(custom_table)
}



# test = scrape_sched(cur, teams, dat)
# 
# proj_remaining_record = sum(test$proj_win_num)
# 
# tmp <- get_reqs(dat, "Los Angeles Lakers", 10, dat_538, F)
# 
# cur <- tmp$data_curr

