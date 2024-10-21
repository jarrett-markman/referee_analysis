# Libraries
library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)
library(tidyr)
library(shiny)
library(DT)
# Load in old pbp/sched data as a .rds file
historical_data <- readRDS("historical_data_1999_2023.rds")
create_data <- function(pbp_data, sched_data) { # Create a fcn that aggregates pbp and sched data
  # Get pbp data over all years in nflfastR data base
  pbp <- pbp_data %>%
    # Get weather descriptions and sentiments
    mutate(
      weather_desc = str_extract(weather, "^[^T]*"),
      # Detect if rainy/snowy/etc. conditions are the case, "bad" weather
      weather_sentiment = ifelse(str_detect(weather_desc, regex("rain|showers|thunderstorms|snow|fog|cold|drizzle", ignore_case = TRUE)), "bad", "neutral"),
      weather_sentiment = ifelse(str_detect(weather_desc, regex("sunny|clear|fair|warm", ignore_case = TRUE)), "good", weather_sentiment),
      # Get generalized play types, and specificy a column for specific play types
      exact_play_type = play_type,
      play_type = ifelse(grepl("kickoff|punt|extra_point|field_goal", exact_play_type), "special_teams", 
                         ifelse(grepl("pass|run", exact_play_type), "pass/rush", "misc/unidentified"))
    ) %>%
    # Select cols for data frame
    select(game_id, season_type, season, week, weather_sentiment, qtr, down, play_type, penalty, penalty_type)
  # Get schedule data over date range
  referee <- sched_data %>%
    # Create primetime and playoff dummy vars
    mutate(primetime = ifelse((weekday %in% c("Monday", "Thursday", "Friday", "Saturday") | (weekday == "Sunday" & gametime == "20:20")), 1, 0),
           playoff = ifelse(game_type != "REG", 1, 0),
           primetime = ifelse(is.na(primetime), 0, primetime), playoff = ifelse(is.na(playoff), 0, playoff)) %>%
    select(game_id, referee, primetime, playoff, home_team, away_team) # Select cols. 
  # Combine referee data w/ game data
  df <- left_join(pbp, referee, by = "game_id") %>% # Left join pbp data w/ ref, primetime/playoff vars, teams
    # Change name misspelling for Bill Carolo, Adrian Hall
    mutate(referee = ifelse(referee == "Bill Carolo", "Bill Carollo", referee),
           referee = ifelse(referee == "Adrian Hall", "Adrian Hill", referee)) %>% 
    filter(!is.na(referee)) # Remove NA referees and penalty types 
  return(df) # Return df
}
# Pull data from 2024+ years
pbp <- load_pbp(2024:most_recent_season())
sched <- load_schedules(2024:most_recent_season())
data <- create_data(pbp, sched) # Apply fn to return df
df <- bind_rows(historical_data, data) # Use bind_rows fn to combine historical data
# Aggregate data function 
aggregate_data <- function(group_cols = NULL, yr1 = 1999, yr2 = most_recent_season(),
                           filter_col = NULL, filter_operator = NULL, filter_value = NULL) { 
  # Use tryCatch to handle errors
  tryCatch({
  df <- df %>%
    filter(season %in% yr1:yr2) # Add in season range filter
  if (!is.null(filter_col) && !is.null(filter_operator) && !is.null(filter_value)) {
    df <- df %>%
      filter(eval(parse(text = paste0(filter_col, filter_operator, "'", filter_value, "'")))) # Add in categorical filter
  }
  info <- df %>%
    # Select group cols. 
    group_by(referee, across(all_of(group_cols))) %>%
    # Get general information cols
    summarise("All Penalties" = sum(penalty, na.rm = TRUE),
              Plays = n()) %>%
    ungroup()
  # Get a leaderboards data frame
  leaderboards <- df %>%
    group_by(referee, penalty_type, across(all_of(group_cols))) %>%
    count(name = "penalty_freq") %>% # Count frequency of each penalty type occurring based on grouped cols
    ungroup() %>%
    filter(!is.na(penalty_type)) %>% # Remove NA penalties
    # Widen penalty frequency data based on penalty types
    pivot_wider(names_from = penalty_type, id_cols = c(referee, all_of(group_cols)), values_from = penalty_freq,
                names_glue = "{penalty_type}"
    ) %>%
    mutate(across(where(is.list), ~ map_dbl(.x, sum))) %>% # Ensure numeric columns
    # Mutate all "NA" cols (penalties) to 0 (never occurred)
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    right_join(info, by = c("referee", group_cols)) %>% # Join penalty info. onto gen. info
    summarise( # Transform all cols. for ref/plays/grouped cols. 
      referee, Plays, !!!syms(group_cols),
      across( # Calculate frequencies across all columns except selected cols.
        where(is.numeric), # Where there are numeric values
        ~ . / Plays * 100, # Get frequency rates for penalty calls
        .names = "{.col} Percentage"
      )
    ) %>%
    mutate( # Mutate percentage cols. into penalty plus cols.
      across( # Get penalty plus data for all cols. except selected cols.
        where(is.numeric),
        ~ . / mean(.) * 100, # Normalize the data
        .names = "{.col} Plus"
      )
    ) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 4)), # Round all cols. to 4 digits
           across(contains("Plus"), ~ round(., digits = 0))) # Round "Plus" cols. to whole number
  reports <- leaderboards %>%
    pivot_longer( # Elongate columns from leaderboards data
      cols = -c(referee, Plays, all_of(group_cols)),
      names_to = c("penalty", "type"),
      names_pattern = "^(.*) (Percentage(?: Plus)?)$", # Cols. with "Percentage Plus"
      values_to = "value"
    ) %>%
    pivot_wider( # Widen data based on penalty types
      names_from = type,
      values_from = value,
      names_sep = "_"
    ) %>%
    rename( # Rename columns
      plays = Plays,
      penalty_frequency = Percentage,
      penalty_plus = `Percentage Plus`
    ) %>%
    select(referee, all_of(group_cols), plays, penalty, penalty_frequency, penalty_plus)
  return(list(leaderboards, reports))},
  error = function(e) {
    print("Sorry! There was an error while aggregating data.")
  })
}
# Create a function that generates a leaderboards graphic
generate_leaderboard <- function(group_cols = NULL, yr1 = 1999, yr2 = most_recent_season(), 
                                 filter_col = NULL, filter_operator = NULL, filter_value = NULL, 
                                 penalty_cols = "All Penalties") {
  tryCatch({# Apply aggregate_data function
  df <- aggregate_data(group_cols, yr1, yr2, filter_col, filter_operator, filter_value)[[1]]
  # Get "yr_range" for table title
  if (yr1 == yr2) {
    yr_range = yr1
  } else{
    yr_range = paste(yr1, yr2, sep = "-")
  }
  # Display pct and plus stats for a selected penalty
  plus_cols <- paste(penalty_cols, "Percentage Plus")
  data <- df %>%
    select(referee, Plays, !!!syms(group_cols), # Select cols. 
           all_of(plus_cols)) %>%
    rename(Referee = referee)
  # Modify "plus" cols. to appear as + instead of "Plus"
  names1 <- gsub("Percentage Plus", "+", names(data))
  names(data) <- names1
  return(data)},
  error = function(e) {
    print("Sorry! There was an error in getting a Leaderboards.")
  })
}
# Create a function that generates an individual report
generate_report <- function(group_cols = NULL, yr1 = 1999, yr2 = most_recent_season(), 
                            filter_col = NULL, filter_operator = NULL, filter_value = NULL,
                            ref = "Adrian Hill", penalty_cols = "All Penalties") {
  tryCatch({# Apply aggregate_data function
  df <- aggregate_data(group_cols, yr1, yr2, filter_col, filter_operator, filter_value)[[2]]
  # Get "yr_range" for table title
  if (yr1 == yr2) {
    yr_range = yr1
  } else {
    yr_range = paste(yr1, yr2, sep = "-")
  }
  data <- df %>%
    # Add in input filters
    filter(referee == ref & penalty %in% penalty_cols) %>%
    select(penalty, !!!syms(group_cols), penalty_frequency, penalty_plus) %>%
    rename(
      Penalty = penalty,
      "Percentage of Plays Called" = penalty_frequency,
      "Penalty+" = penalty_plus
    )
  return(data)},
  error = function(e) {
    print("Sorry! There was an error while generating a report.")
  })
}
# Generate game comparison reports
generate_game_report <- function(filter_col = NULL, filter_operator = NULL, filter_value = NULL,
                                 season = 1999, wk = 1, away_team = "ARI", home_team = "PHI", 
                                 penalty_cols = "All Penalties") {
  tryCatch({
    # Apply aggregate data function to calculate career data 
    career_data <- aggregate_data(NULL, 1999, most_recent_season(), filter_col, filter_operator, filter_value)[[2]] %>%
      rename(car_pen_freq = penalty_frequency,
             car_pen_plus = penalty_plus)
    season_data <- aggregate_data(c("season"), 1999, most_recent_season(), filter_col, filter_operator, filter_value)[[2]] %>%
      rename(szn_pen_freq = penalty_frequency,
             szn_pen_plus = penalty_plus)
    df <- aggregate_data(c("game_id"), 1999, most_recent_season(), filter_col, filter_operator, filter_value)[[2]] %>%
      rename(gm_pen_freq = penalty_frequency,
             gm_pen_plus = penalty_plus)
    viz <- df %>%
      # Paste inputs together to match game_ids
      filter(game_id == paste(season, ifelse(wk %in% c(1:9), paste0("0", wk), wk), away_team, home_team, sep = "_")) %>%
      mutate(season = season) %>% # Create season column based on season input
      # Join season and career data
      left_join(season_data, by = c("referee", "season", "penalty")) %>%
      left_join(career_data, by = c("referee", "penalty"))
    data <- viz %>%
      filter(penalty %in% penalty_cols) %>%
      # Select/order cols for table
      select(referee, penalty, gm_pen_freq, gm_pen_plus, szn_pen_freq, szn_pen_plus, car_pen_freq, car_pen_plus) %>%
      rename(
        Referee = referee,
        Penalty = penalty,
        "Game Penalty Frequency" = gm_pen_freq,
        "Game Penalty+" = gm_pen_plus,
        "Season Penalty Frequency" = szn_pen_freq,
        "Season Penalty+" = szn_pen_plus,
        "Career Penalty Frequency" = car_pen_freq,
        "Career Penalty+" = car_pen_plus
      )
    return(data)
  },
  error = function(e) {
    print(e)
  })
}
# Generate referee lookup
generate_lookup <- function(group_cols = c("game_id"), yr1 = 1999, yr2 = most_recent_season(), 
                            filter_col = NULL, filter_operator = NULL, filter_value = NULL,
                            ref = "Adrian Hill") {
  all_ref_games <- aggregate_data(group_cols, yr1, yr2, filter_col, filter_operator, filter_value)[[2]] %>%
    # Aggregate data for all game_ids
    distinct(referee, game_id) %>% # Get distinct referee/game_id combinations
    arrange(desc(game_id)) %>% # Arrange in decreasing order (newest - oldest)
    separate(game_id, c("Season", "Week", "Away Team", "Home Team")) # Separate cols. for game_id
  display <- all_ref_games %>%
    filter(referee == ref) %>% # Filter referee 
    select(-referee) # Remove referee column
  return(display)
}
# Testing functions
aggregate_data()
generate_leaderboard()
generate_report()
generate_game_report()
generate_lookup()
# Get penalty types for tab inputs
penalty_types <- c("All Penalties", na.omit(unique(df$penalty_type)))
##### Shiny
ui <- navbarPage(
  "Referee Evaluation",  # Title tab
  id = "main", # Set tab id
  # 1. Home Page
  tabPanel(
    "Home",
    h1("NFL Referee Evaluation:"),
    p("A Shiny App that provides users with the ability to analyze referee penalty tendencies in multiple different ways."),
    # Set the layout for the home/title page
    fluidRow(
      column(
        12, 
        h3("Data:"), # Set the data header and information
        p("This Shiny App sources data from nflfastR from 1999 until the current NFL season."),
        
        h3("Interpretation:"), # Why this shiny app
        p("The goal of this Shiny App is to understand the general tendencies of all referees, in addition to how specific referees have called certain penalties, as well as how certain game situation affect their tendencies. nflfastR provides penalty data on each play since 1999, and nflreadr has the crew chief for each game since 1999. Using the data nflfastR and nflreadr provide, a user can find out and identify the tendencies for a referee with a variety of different filtration options, such as referee, time frame, penalties, and various game situation variables. This Shiny App can be extremely valuable in game planning for a specific crew chief, to identify prior penalty calling tendencies over their career, how they compare to other referees, and how they may have reffed in prior, recent games. The data across this Shiny App includes the percentage of plays in which that penalty was called in addition to {penalty} +, which is the normalized version of penalty frequency - for example, a referee with a 150 penalty + calls penalties 50% more frequently than average, whereas a referee with 75 penalty + calls penalties 25% less frequently than average."),
        
        h3("How to use this Shiny App:"), # How to use this shiny app
        p("The tabs in this Shiny App are:"),
        tags$ul(
          tags$li("Leaderboards creates a list of all the referees that fit the selected inputs."),
          tags$li("Reports creates an individual report for any selected referee for the selected inputs."),
          tags$li("Game Comparison provides an individual game comparison for a referee in any selected game, and compares their penalty frequency to their average over the course of that season and their referee career."),
          tags$li("Referee Lookup provides information on the games referee'd for a selected referee.")
        ),
        
        h3("Possible Limitations:"), # Describe where this shiny could be improved
        p("While this Shiny App offers a way to evaluate referee tendencies, there are a variety of factors to take into account, in addition to ways to improve the analysis. For example, the data doesn't include information for which referee made which calls, it only provides the crew chief for that game. For certain games or seasons referee'd, it's likely that the frequency of penalties are a byproduct of the actions of the teams involved (whether that includes a higher or lower frequency of penalties). Additionally, just because a referee has a certain proclivity to (or not to) make a certain call, that doesn't mean it's the more likely case. While the leaderboards, reports, and game comparison tabs offer a lot of value to understand referee tendencies, the could all be improved and added upon. For example, a referee comparison tab, that lets you select two specific referees and compare how they call games.")
      )
    ),
    HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh;"><img src="https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1024px-National_Football_League_logo.svg.png" style="max-width: 100%; max-height: 100%;"></div>')
  ),
  # 2. Leaderboards
  tabPanel(
    "Leaderboards",
    id = "leaderboards",
    sidebarPanel( # Set leaderboards inputs, with values/choices
      selectInput("lead_group_cols", "Select Career/Season/Game for Aggregating Data:", choices = c("Career", "Season", "Game")),
      sliderInput("lead_yr1", "Start Year:", value = 1999, min = 1999, max = most_recent_season(), sep = "", step = 1),
      sliderInput("lead_yr2", "End Year:", value = most_recent_season(), min = 1999, max = most_recent_season(), sep = "", step = 1),
      checkboxGroupInput("lead_penalty_cols", "Select Penalty Columns:", choices = penalty_types),
      selectInput("lead_filter", "Select Categorical Filter (can be left blank):", choices = c("", "home_team", "away_team", "weather_sentiment", "play_type", "exact_play_type", "qtr", "down", "playoff", "primetime")),
      selectInput("lead_operator", "Select Categorical Operator (can be left blank):", choices = c("", "==", "!=")),
      selectInput("lead_value", "Select Categorical Value (can be left blank):", choices = NULL),
      actionButton("submit_lead", "Submit") # Create an actionButton that lets the user select when to display
    ),
    mainPanel(
      DTOutput("leaderboards") # Display leaderboards as a data table
    )
  ),
  # 3. Reports
  tabPanel(
    "Reports",
    id = "reports",
    sidebarPanel( # Set reports inputs, with values/choices
      selectInput("rep_group_cols", "Select Career/Season/Game for Aggregating Data:", choices = c("Career", "Season", "Game")),
      sliderInput("rep_yr1", "Start Year:", value = 1999, min = 1999, max = most_recent_season(), sep = "", step = 1),
      sliderInput("rep_yr2", "End Year:", value = most_recent_season(), min = 1999, max = most_recent_season(), sep = "", step = 1),
      selectInput("ref", "Select Referee:", choices = sort(unique(df$referee))),
      checkboxGroupInput("rep_penalty_cols", "Select Penalty Columns:", choices = penalty_types),
      selectInput("rep_filter", "Select Categorical Filter (can be left blank):", choices = c("", "home_team", "away_team", "weather_sentiment", "play_type", "exact_play_type", "qtr", "down", "playoff", "primetime")), 
      selectInput("rep_operator", "Select Categorical Operator (can be left blank):", choices = c("", "==", "!=")),
      selectInput("rep_value", "Select Categorical Value (can be left blank):", choices = NULL),
      actionButton("submit_rep", "Submit")
    ),
    mainPanel(
      DTOutput("reports") # Display reports as a data table
    )
  ),
  # 4. Game Comparison Tool
  tabPanel(
    "Game Comparison",
    id = "game_reports",
    sidebarPanel( # Set game comparison inputs, with values/choices
      sliderInput("gm_yr", "Select Season:", min = 1999, max = most_recent_season(), sep = "", value = 1999, step = 1),
      sliderInput("gm_wk", "Select Week", min = min(df$week), max = max(df$week), value = 1, step = 1),
      selectInput("gm_game", "Select Game", choices = NULL),
      checkboxGroupInput("gm_penalty_cols", "Select Penalty Columns:", choices = penalty_types),
      selectInput("gm_filter", "Select Categorical Filter (can be left blank):", choices = c("", "home_team", "away_team", "weather_sentiment", "play_type", "exact_play_type", "qtr", "down", "playoff", "primetime")),
      selectInput("gm_operator", "Select Categorical Operator (can be left blank):", choices = c("", "==", "!=")),
      selectInput("gm_value", "Select Categorical Value (can be left blank):", choices = NULL),
      actionButton("submit_gm", "Submit")
    ),
    mainPanel(
      DTOutput("game_comparison") # Display game comparison as a data table
    )
  ),
  # 5. Ref lookup
  tabPanel(
    "Referee Lookup",
    id = "lookup",
    sidebarPanel( # Set lookup inputs
      selectInput("ref_lookup", "Select Referee:", choices = sort(unique(df$referee))),
      actionButton("submit_lookup", "Submit")
    ),
    mainPanel(
      DTOutput("lookup") # Display lookup as a data table
    )
  ),
  # 6. References
  tabPanel(
    "References & Help",
    h3(HTML('All data provided is from <a href="https://nflfastr.com" target="_blank">nflfastR</a>.'), align = "center"),
    h3(HTML('The code made for this Shiny App can be found on <a href="https://github.com/jarrett-markman/referee_analysis/tree/main" target="_blank">GitHub</a>.'), align = "center"),
    h3(HTML('If you have any questions, feel free to message me on <a href="https://www.linkedin.com/in/jarrett-markman/" target="_blank">LinkedIn</a>.'), align = "center")
  )
)
# Create server to handle functional inputs
server <- function(input, output, session) {
  # Create reactive values to store results of generating functions
  leaderboard_data <- reactiveVal()
  report_data <- reactiveVal()
  game_report_data <- reactiveVal()
  lookup_data <- reactiveVal()
  # Observe the submit button from the event, and render a leaderboards data table
  observeEvent(input$submit_lead, {
    leaderboard_data(
      generate_leaderboard( # Generate a leaderboards based on the inputs 
        if (input$lead_group_cols == "Season") "season" else if (input$lead_group_cols == "Game") "game_id" else NULL,
        input$lead_yr1,
        input$lead_yr2,
        if (input$lead_filter != "") input$lead_filter else NULL,
        if (input$lead_operator != "") input$lead_operator else NULL,
        if (input$lead_value != "") input$lead_value else NULL,
        input$lead_penalty_cols
      )
    )
    output$leaderboards <- renderDT({
      leaderboard_data() # Set the output as the leaderboard_data (reactive based on generate_leaderboards fcn)
    })
  })
  # Observe submit button for reports
  observeEvent(input$submit_rep, {
    ### Same as for leaderboards, apply to reports
    report_data(
      generate_report(
        if (input$rep_group_cols == "Season") "season" else if (input$rep_group_cols == "Game") "game_id" else NULL,
        input$rep_yr1,
        input$rep_yr2,
        if (input$rep_filter != "") input$rep_filter else NULL,
        if (input$rep_operator != "") input$rep_operator else NULL,
        if (input$rep_value != "") input$rep_value else NULL,
        input$ref,
        input$rep_penalty_cols
      )
    )
    output$reports <- renderDT({
      report_data()
    })
  })
  # Observe submit button for game report
  observeEvent(input$submit_gm, {
    # If the game is not nothing, split it up by " v " to get the away and home team
    if (input$gm_game != "") {
      game_parts <- strsplit(input$gm_game, " v ")[[1]]
      away_team <- game_parts[1]
      home_team <- game_parts[2]
    } else {
      away_team <- NULL
      home_team <- NULL
    }
    game_report_data( # Apply reactie game_report_data val, with generate_game_report fcn
      generate_game_report(
        if (input$gm_filter != "") input$gm_filter else NULL,
        if (input$gm_operator != "") input$gm_operator else NULL,
        if (input$gm_value != "") input$gm_value else NULL,
        input$gm_yr,
        input$gm_wk,
        away_team,
        home_team,
        input$gm_penalty_cols
      )
    )
    output$game_comparison <- renderDT({
      game_report_data() # Render data table
    })
  })
  # Observe submit button for lookup
  observeEvent(input$submit_lookup, {
    lookup_data( # For lookup_data reactiveVal, generate a lookuptable
      generate_lookup(
        ref = input$ref_lookup,
      )
    )
    output$lookup <- renderDT({
      lookup_data()
    })
  })
  # Observe the input for the categorical filters on a page
  observe({
    ### For each part of the filter individually pull its filtered value
    lead_filter_ops <- if (input$lead_filter != "") {
      df %>% pull(input$lead_filter) %>% unique() %>% na.omit()
    } else {
      c("")
    }
    rep_filter_ops <- if (input$rep_filter != "") {
      df %>% pull(input$rep_filter) %>% unique() %>% na.omit()
    } else {
      c("")
    }
    gm_filter_ops <- if (input$gm_filter != "") {
      df %>% pull(input$gm_filter) %>% unique() %>% na.omit()
    } else {
      c("")
    }
    # Update inputs
    updateSelectInput(session, "lead_value", choices = c("", lead_filter_ops), selected = NULL)
    updateSelectInput(session, "rep_value", choices = c("", rep_filter_ops), selected = NULL)
    updateSelectInput(session, "gm_value", choices = c("", gm_filter_ops), selected = NULL)
  })
  # Update team combinations for an input
  observe({
    # For team input values, display the teams as game_combinations
    filtered_games <- df %>% filter(season == input$gm_yr, week == input$gm_wk)
    game_combinations <- unique(paste(filtered_games$away_team, "v", filtered_games$home_team))
    updateSelectInput(session, "gm_game", choices = c("", game_combinations), selected = NULL)
  })
}
shinyApp(ui, server)
