server <- function(input, output) {
  font_factor = 17.5/15
  
  # source(paste0("funcs.R"))
  source(paste0("/Users/smsarkar/Documents/One_offs/18_lakers_suck/lakerapp/funcs.R"))
  
  url = "https://www.espn.com/nba/standings"
  tmp <- read_html(url) %>%
    html_table()
  
  dat_east <- cbind(tmp[[1]], tmp[[2]])
  dat_west <- cbind(tmp[[3]], tmp[[4]])
  
  dat_east <- clean_dat(dat_east, "east")
  dat_west <- clean_dat(dat_west, "west")
  dat <- rbind(dat_east, dat_west)
  
  dat_538 = clean_dat538(teams)
  
  observeEvent(input$do, {
    # print(input$tabs)
    
    if(input$team %in% team_names & input$dseed <= 10) {
      if(input$ass == "proj_current") {
        output$seed_label = renderText({
          paste0("<h4>", "Desired Seed (current pace)", "</h4>")

        })
        ass = F
      } else{
        output$seed_label = renderText({
          paste0("<h4>", "Desired Seed (538 pace)", "</h4>")
        })
        ass = T
      }

      out <- get_reqs(dat, input$team, desired_seed = input$dseed,
                      dat_538, ass_538 = ass)
      
      out_dat = out$data
      out_dat_curr = out$data_curr
      out_dat_needed = out$data_needed
      out_text = out$text
      
      output$text_output <- renderText({
        paste0("<b>", out_text, "</b>")
      })
      
      curr_wins = out_dat_curr$w
      
      label1 = paste0("The <b>", input$team, "</b> currently have <b> <font color = \" #007D00\">", 
                      curr_wins, "</b></font> wins. <br/> <br/>")
      
      if(out_dat_needed$w_needed[1] <= 0) {
        label2 = paste0("To end up as the ", input$dseed, " seed, they <b> do not need to win any more games.")
      } else{
        label2 = paste0("To end up as the ", input$dseed, " seed, they need <b>", 
                        out_dat_needed$w_needed[1], "</b> more wins (given selected assumptions).",
                        "<br/>Given that they only have <b>", out_dat_curr$g_left[1], " games remaining</b>, this", 
                        " works out to needing a <b>", 
                        format(round(out_dat_needed$w_needed[1]/out_dat_curr$g_left[1], 3), nsmall = 3),
                        "</b> win pct moving forward.")
      }
      
      
      
      
      output$info_output <- renderText({
        paste0(label1, label2)
      })
      
      stable <- datatable(dat %>%
                            filter(conference == out_dat_curr$conference[1]) %>%
                            mutate(record = paste0(w, "-", l),
                                   record_l10 = paste0(w_last10, "-", l_last10),
                                   pct = format(round(pct, 3), nsmall = 3)) %>%
                            select(current_seed, team_full, record, pct, record_l10),
                          options = list(autowidth = T,
                                         paging = F,
                                         ordering = T,
                                         scrollX = T,
                                         searchable = F,
                                         bFilter=0,
                                         bInfo=0,
                                         pagelength = 15,
                                         columnDefs = list(list(className = 'dt-center', targets = 2:4))),
                          rownames = F,
                          colnames = c("Seed", "Team", "Record", "Win Pct (%)",
                                       "Last 10")
      ) 
      
      
      
      output$table_standings <- DT::renderDataTable({stable})
      
      
      currtable <- out_dat_curr %>%
        mutate(record = paste0(w, "-", l),
               record_l10 = paste0(w_last10, "-", l_last10),
               conference = str_to_title(conference),
               w_needed = out_dat_needed$w_needed[1],
               pct_needed = out_dat_needed$pct_needed[1],
               final_w = ifelse(out_dat_curr$g_left[1]<w_needed,
                                w+out_dat_curr$g_left[1], w+w_needed),
               final_record = paste0(final_w, "-", 82-final_w),
               pct_needed = ifelse(pct_needed > 1 | pct_needed < 0, NA, pct_needed),
               pct_needed = format(round(pct_needed, 3), nsmall = 3),
               pct_needed = ifelse(pct_needed == "NA", "", pct_needed),
               pct = format(round(pct, 3), nsmall = 3),
               current_seed = which(dat$team_full[dat$conference == out_dat_curr$conference[1]] 
                                    == out_dat_curr$team_full[1])) %>%
        select(conference, current_seed, team_full, record, pct, record_l10, g_left, w_needed,
               pct_needed, final_record) %>%
        select(-conference)
      
      
      
      output$table_selected <- renderReactable({
        reactable(
          currtable,
          # wrap = T,
          pagination = T, 
          defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
          columns = list(
            # conference = colDef(
            #   headerStyle = list(fontWeight = 700),
            #   name = "Conference", 
            #   maxWidth = font_factor* 95,
            #   style = list(fontWeight = 600),
            #   cell = function(value, index) {
            #     div(
            #       style = "white-space: pre;",
            #       value
            #     )
            #   }
            # ),
            current_seed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Seed", 
              maxWidth = font_factor* 80,
              align = "center",
              style = list(fontWeight = 600),
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            team_full = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Team", 
              style = list(background = "rgba(0, 0, 0, 0.05)", fontWeight = 600, whiteSpace = "unset"),
              maxWidth = font_factor* font_factor* 120,
              align = "center"
            ),
            record = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Record", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            pct = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Win Pct", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            record_l10 = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Last 10", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            g_left = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Games Left", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            w_needed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Wins Needed", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            pct_needed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Win Pct Needed", 
              maxWidth = font_factor* 100,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            final_record = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Final Record", 
              maxWidth = font_factor* 85,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            )
          )
        )
        
        
      })
      
      comptable <- out_dat_needed %>%
        mutate(record = paste0(w, "-", l),
               record_l10 = paste0(w_last10, "-", l_last10),
               conference = str_to_title(conference),
               # w_needed = out_dat_needed$w_needed[1],
               # pct_needed = out_dat_needed$pct_needed[1],
               # pct_needed = ifelse(pct_needed > 1 | pct_needed < 0, NA, pct_needed),
               # pct_needed = format(round(pct_needed, 3), nsmall = 3),
               # pct_needed = ifelse(pct_needed == "NA", "", pct_needed),
               pct = format(round(pct, 3), nsmall = 3),
               current_seed = input$dseed) %>%
        select(conference, current_seed, team_full, record, pct, record_l10) %>%
        select(-conference)
      
      
      output$table_comparison <- renderReactable({
        reactable(
          comptable,
          # wrap = T,
          pagination = T, 
          defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
          columns = list(
            # conference = colDef(
            #   headerStyle = list(fontWeight = 700),
            #   name = "Conference", 
            #   maxWidth = font_factor* 95,
            #   style = list(fontWeight = 600),
            #   cell = function(value, index) {
            #     div(
            #       style = "white-space: pre;",
            #       value
            #     )
            #   }
            # ),
            current_seed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Seed", 
              maxWidth = font_factor* 80,
              align = "center",
              style = list(fontWeight = 600),
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            team_full = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Team", 
              style = list(background = "rgba(0, 0, 0, 0.05)", fontWeight = 600, whiteSpace = "unset"),
              maxWidth = font_factor* 120,
              align = "center"
            ),
            record = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Record", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            pct = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Win Pct", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            record_l10 = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Last 10", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            )
          )
        )
      })
      
    }
   
    if(input$tabs == '<strong>Pick Remaining Games</strong>' & input$team %in% team_names) {
      sched <- scrape_sched(out_dat_curr, teams, dat)

      custom_sched <- datatable(sched %>%
                            mutate(opp_pct = format(round(opp_pct, 3), nsmall = 3)) %>%
                            select(date, opp, opp_pct, proj_win),
                          options = list(autowidth = T,
                                         paging = F,
                                         ordering = T,
                                         scrollX = T,
                                         searchable = F,
                                         bFilter=0,
                                         bInfo=0,
                                         pagelength = 15,
                                         columnDefs = list(list(className = 'dt-center', targets = 1:3))),
                          rownames = F,
                          selection = 'none',
                          colnames = c("Date", "Opponent", "Opponent Pct", "Proj. Outcome"),
                          editable = list(target = "cell", 
                                          disable = list(columns = 0:2))
      ) 

      observeEvent(input$custom_sched_cell_edit, {
        sched <<- editData(sched, 
                          input$custom_sched_cell_edit, rownames = F)
        

        sched <- sched %>%
          mutate(proj_win_num = ifelse(proj_win %in% c("W", "w"), 1, 
                                       ifelse(proj_win %in% c("L", "l"), 0, NA)))
        
        custom_table <- get_custom_wins(currtable, out_dat_curr, sched, dat)
        
        color_seed = ifelse(custom_table$proj_seed >= 10, 
                            "#FF4E11", ifelse(custom_table$proj_seed >= 5,
                                              "#FAB733", "#69B34C"))
        
        output$custom_selected <- renderReactable({
          reactable(
            custom_table,
            # wrap = T,
            pagination = T, 
            defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
            columns = list(
              current_seed = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Seed", 
                maxWidth = font_factor* 80,
                align = "center",
                style = list(fontWeight = 600),
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              ),
              team_full = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Team", 
                style = list(background = "rgba(0, 0, 0, 0.05)", fontWeight = 600, whiteSpace = "unset"),
                maxWidth = font_factor* 120,
                align = "center"
              ),
              record = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Record", 
                maxWidth = font_factor* 80,
                align = "center",
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              ),
              pct = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Win Pct", 
                maxWidth = font_factor* 80,
                align = "center",
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              ),
              proj_record = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Proj. Record", 
                maxWidth = font_factor* 150,
                align = "center",
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              ),
              proj_pct = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Proj. Win Pct", 
                maxWidth = font_factor* 150,
                align = "center",
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              ),
              proj_seed = colDef(
                headerStyle = list(fontWeight = 700),
                name = "Proj. Seed", 
                maxWidth = font_factor* 100,
                align = "center",
                style = list(fontWeight = 600, background = color_seed),
                cell = function(value, index) {
                  div(
                    style = "white-space: pre;",
                    value
                  )
                }
              )
            )
          )
          
          
        })
      })
      
      output$custom_sched <- DT::renderDataTable({custom_sched})
      
      
      custom_table <- get_custom_wins(currtable, out_dat_curr, sched, dat)
  
      print(custom_table)

      color_seed = ifelse(custom_table$proj_seed >= 10, 
                          "#FF4E11", ifelse(custom_table$proj_seed >= 5,
                                            "#FAB733", "#69B34C"))
      
      output$custom_selected <- renderReactable({
        reactable(
          custom_table,
          # wrap = T,
          pagination = T, 
          defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
          columns = list(
            current_seed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Seed", 
              maxWidth = font_factor* 80,
              align = "center",
              style = list(fontWeight = 600),
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            team_full = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Team", 
              style = list(background = "rgba(0, 0, 0, 0.05)", fontWeight = 600, whiteSpace = "unset"),
              maxWidth = font_factor* 120,
              align = "center"
            ),
            record = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Record", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            pct = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Win Pct", 
              maxWidth = font_factor* 80,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            proj_record = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Proj. Record", 
              maxWidth = font_factor* 150,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            proj_pct = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Proj. Win Pct", 
              maxWidth = font_factor* 150,
              align = "center",
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            ),
            proj_seed = colDef(
              headerStyle = list(fontWeight = 700),
              name = "Proj. Seed", 
              maxWidth = font_factor* 100,
              align = "center",
              style = list(fontWeight = 600, background = color_seed),
              cell = function(value, index) {
                div(
                  style = "white-space: pre;",
                  value
                )
              }
            )
          )
        )
        
        
      })
      
      
    }
    

    #end observe event 
  })
  
  
  
  
  
}