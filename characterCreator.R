#| standalone: true
# Enhanced Mythras character tracker – full Shinylive-compatible app
# ---------------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(DT)
library(jsonlite)

# ---------------------------------------------------------------------------
# Embed JS for exp +/- delegation and save/load to localStorage -------------
jsCode <- "
// experience buttons
$(document).on('click', '.exp-plus', function(){
  Shiny.setInputValue('exp_change', {table: $(this).data('table'), skill: $(this).data('skill'), delta: 1}, {priority:'event'});
});
$(document).on('click', '.exp-minus', function(){
  Shiny.setInputValue('exp_change', {table: $(this).data('table'), skill: $(this).data('skill'), delta: -1}, {priority:'event'});
});

// localStorage helpers
Shiny.addCustomMessageHandler('saveChar', function(payload){
  localStorage.setItem('mythrasChar', JSON.stringify(payload));
  Shiny.setInputValue('save_success', true, {priority:'event'});
});

Shiny.addCustomMessageHandler('loadChar', function(x){
  const saved = localStorage.getItem('mythrasChar');
  if(saved) {
    Shiny.setInputValue('loadedChar', saved, {priority:'event'});
  } else {
    Shiny.setInputValue('load_error', true, {priority:'event'});
  }
});

// Export character
Shiny.addCustomMessageHandler('exportChar', function(payload){
  const dataStr = JSON.stringify(payload, null, 2);
  const dataUri = 'data:application/json;charset=utf-8,'+ encodeURIComponent(dataStr);
  const exportFileDefaultName = payload.name + '_mythras_character.json';
  
  const linkElement = document.createElement('a');
  linkElement.setAttribute('href', dataUri);
  linkElement.setAttribute('download', exportFileDefaultName);
  linkElement.click();
});
"

# ---------------------------------------------------------------------------
# Rulebook constants (p. 12-13) --------------------------------------------
BG_POOL       <- 100
CAREER_POOL   <- 100
AGE_POOLS     <- c(Young=100, Adult=150, `Middle-Aged`=200, Senior=250, Old=300)
MIN_PER_SKILL <- 5
MAX_BGCAREER  <- 15
MAX_AGE       <- 15

# Background & career packages ---------------------------------------------
bg_packages <- list(
  Barbarian = c("Athletics","Brawn","Endurance","First Aid","Locale","Perception","Boating","Ride"),
  Civilised = c("Customs","Deceit","Dance","Influence","Insight","Locale","Native Tongue","Streetwise"),
  Nomadic   = c("Athletics","Ride","Survival","Perception","Brawn","Customs","Stealth"),
  Primitive = c("Athletics","Endurance","Survival","Brawn","Stealth","Locale","Swim")
)

career_packages <- list(
  Warrior = c("Athletics","Brawn","Endurance","Evade","Unarmed","Craft","Lore","Survival"),
  Thief   = c("Stealth","Evade","Athletics","Deceit","Insight","Acting","Lockpicking","Mechanisms","Streetwise"),
  Scout   = c("Athletics","Endurance","Perception","Stealth","Swim","Navigation","Survival","Track"),
  Scholar = c("Customs","Insight","Locale","Lore","Language","Literacy","Teach","Culture"),
  Priest  = c("Customs","Influence","Insight","Willpower","Culture","Lore","Oratory","Teach"),
  Merchant = c("Commerce","Deceit","Influence","Insight","Locale","Language","Streetwise","Customs"),
  Entertainer = c("Acting","Dance","Sing","Influence","Insight","Play Instrument","Oratory","Sleight")
)

# Skill categories for better organization
skill_categories <- list(
  Combat = c("Unarmed", "Evade", "Combat Style"),
  Physical = c("Athletics", "Brawn", "Endurance", "Swim", "Ride", "Boating", "Acrobatics"),
  Social = c("Influence", "Insight", "Deceit", "Oratory", "Customs", "Dance", "Sing", "Acting"),
  Knowledge = c("Lore", "Culture", "Language", "Literacy", "Native Tongue", "Locale"),
  Professional = c("Craft", "Commerce", "Mechanisms", "Healing", "Navigation", "Survival", "Track"),
  Magic = c("Folk Magic", "Invocation", "Binding", "Mysticism", "Sorcery"),
  Stealth = c("Stealth", "Conceal", "Sleight", "Lockpicking"),
  Other = c("First Aid", "Willpower", "Perception", "Drive", "Courtesy", "Bureaucracy", 
            "Teach", "Art", "Play Instrument", "Streetwise")
)

# -------------------------- Skill base formulae ---------------------------
a2 <- function(x,y) x+y
std_formulas <- list(
  Athletics = \(STR,DEX,...) a2(STR,DEX),   Boating   = \(STR,CON,...) a2(STR,CON),
  Brawn     = \(STR,SIZ,...) a2(STR,SIZ),   Conceal   = \(DEX,POW,...) a2(DEX,POW),
  Customs   = \(INT,...)     INT*2,         Dance     = \(DEX,CHA,...) a2(DEX,CHA),
  Deceit    = \(INT,CHA,...) a2(INT,CHA),   Drive     = \(DEX,POW,...) a2(DEX,POW),
  Endurance = \(CON,...)     CON*2,         Evade     = \(DEX,...)     DEX*2,
  `First Aid` = \(INT,DEX,...) a2(INT,DEX), Influence = \(CHA,...)     CHA*2,
  Insight   = \(INT,POW,...) a2(INT,POW),   Locale    = \(INT,...)     INT*2,
  `Native Tongue` = \(INT,CHA,...) a2(INT,CHA),
  Perception= \(INT,POW,...) a2(INT,POW),   Ride      = \(DEX,POW,...) a2(DEX,POW),
  Sing      = \(CHA,POW,...) a2(CHA,POW),   Stealth   = \(DEX,INT,...) a2(DEX,INT),
  Swim      = \(STR,CON,...) a2(STR,CON),   Unarmed   = \(STR,DEX,...) a2(STR,DEX),
  Willpower = \(POW,...)     POW*2
)
std_names <- names(std_formulas)

prof_formulas <- list(
  Acting     = \(CHA,...) CHA*2,            Acrobatics = \(STR,DEX,...) a2(STR,DEX),
  Art        = \(POW,CHA,...) a2(POW,CHA),  Binding    = \(POW,CHA,...) a2(POW,CHA),
  Bureaucracy= \(INT,...) INT*2,            Commerce   = \(INT,CHA,...) a2(INT,CHA),
  Courtesy   = \(INT,CHA,...) a2(INT,CHA),  Craft      = \(DEX,INT,...) a2(DEX,INT),
  Culture    = \(INT,...) INT*2,            `Folk Magic` = \(POW,CHA,...) a2(POW,CHA),
  Healing    = \(INT,POW,...) a2(INT,POW),  Invocation = \(INT,...) INT*2,
  Language   = \(INT,CHA,...) a2(INT,CHA),  Literacy   = \(INT,...) INT*2,
  Lore       = \(INT,...) INT*2,            Mechanisms = \(DEX,INT,...) a2(DEX,INT),
  Mysticism  = \(POW,CON,...) a2(POW,CON),  Navigation = \(INT,POW,...) a2(INT,POW),
  Oratory    = \(POW,CHA,...) a2(POW,CHA),  `Play Instrument` = \(DEX,CHA,...) a2(DEX,CHA),
  Sleight    = \(DEX,CHA,...) a2(DEX,CHA),  Sorcery    = \(INT,POW,...) a2(INT,POW),
  Streetwise = \(POW,CHA,...) a2(POW,CHA),  Survival   = \(CON,POW,...) a2(CON,POW),
  Teach      = \(INT,CHA,...) a2(INT,CHA),  Track      = \(INT,CON,...) a2(INT,CON),
  Lockpicking= \(DEX,INT,...) a2(DEX,INT),  `Combat Style` = \(STR,DEX,...) a2(STR,DEX)
)
prof_names <- sort(unique(names(prof_formulas)))

# ------------------ UI -----------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c()),
  tags$head(
    tags$style(HTML("
      .exp-minus, .exp-plus {
        width: 30px;
        height: 30px;
        font-size: 18px;
        font-weight: bold;
        margin: 0 5px;
      }
      .skill-category {
        font-weight: bold;
        background-color: #f0f0f0;
        padding: 5px;
        margin-top: 10px;
        margin-bottom: 5px;
      }
      .over-budget {
        color: red;
        font-weight: bold;
      }
      .points-display {
        font-size: 16px;
        padding: 5px;
        background-color: #f8f9fa;
        border-radius: 3px;
        margin-top: 10px;
      }
      .validation-error {
        color: red;
        font-size: 14px;
        margin-top: 5px;
      }
    "))
  ),
  titlePanel("Mythras Character Sheet (Enhanced)"),
  sidebarLayout(
    sidebarPanel(
      h4("Character Details"),
      fluidRow(
        column(6, textInput("name", "Name", placeholder = "Enter character name")),
        column(6, textInput("player", "Player", placeholder = "Player name"))
      ),
      fluidRow(
        column(4, numericInput("age", "Age", 25, 15, 80)),
        column(4, selectInput("gender", "Gender", c("Male", "Female", "Other"))),
        column(4, selectInput("handed", "Handed", c("Right", "Left", "Ambidextrous")))
      ),
      hr(),
      h4("Characteristics"),
      fluidRow(
        column(4, numericInput("STR", "STR", 10, 3, 21)),
        column(4, numericInput("CON", "CON", 10, 3, 21)),
        column(4, numericInput("SIZ", "SIZ", 10, 8, 21))
      ),
      fluidRow(
        column(4, numericInput("DEX", "DEX", 10, 3, 21)),
        column(4, numericInput("INT", "INT", 10, 8, 21)),
        column(4, numericInput("POW", "POW", 10, 3, 21))
      ),
      fluidRow(
        column(4, numericInput("CHA", "CHA", 10, 3, 21)),
        column(8, actionButton("roll_stats", "🎲 Roll Stats", class = "btn-warning"))
      ),
      hr(),
      h4("Character Options"),
      actionButton("save", "💾 Save", class = "btn-success"),
      actionButton("load", "📂 Load", class = "btn-info"),
      actionButton("export", "📤 Export", class = "btn-primary"),
      actionButton("reset", "🔄 Reset", class = "btn-danger"),
      div(id = "file_upload_div", style = "margin-top: 10px;",
          fileInput("import_file", "Import Character", accept = c(".json"))
      )
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Creation",
                           tabsetPanel(id = "creation_tabs",
                                       tabPanel("Background",
                                                selectInput("background", "Culture", choices = names(bg_packages)),
                                                div(class = "points-display", 
                                                    "Points remaining: ", 
                                                    span(id = "bg_points_span", textOutput("bg_left", inline = TRUE))),
                                                div(id = "bg_validation", class = "validation-error"),
                                                uiOutput("bg_sliders")
                                       ),
                                       tabPanel("Career",
                                                selectInput("career", "Career", choices = names(career_packages)),
                                                div(class = "points-display", 
                                                    "Points remaining: ", 
                                                    span(id = "career_points_span", textOutput("career_left", inline = TRUE))),
                                                div(id = "career_validation", class = "validation-error"),
                                                uiOutput("career_sliders")
                                       ),
                                       tabPanel("Age",
                                                selectInput("age_cat", "Age Category", choices = names(AGE_POOLS), selected = "Adult"),
                                                selectizeInput("age_new_prof", "Add extra professional skill", 
                                                               choices = prof_names, multiple = FALSE, 
                                                               options = list(placeholder = "(optional)")),
                                                div(class = "points-display", 
                                                    "Points remaining: ", 
                                                    span(id = "age_points_span", textOutput("age_left", inline = TRUE))),
                                                div(id = "age_validation", class = "validation-error"),
                                                uiOutput("age_sliders")
                                       ),
                                       tabPanel("Passions",
                                                h4("Starting Passions"),
                                                p("Choose up to 3 starting passions (or add custom ones):"),
                                                selectizeInput("passions", "Passions", 
                                                               choices = c("Loyalty to Clan", "Love (Family)", "Hate (Enemies)",
                                                                           "Devotion (Deity)", "Honor", "Greed", "Ambition"),
                                                               multiple = TRUE, options = list(create = TRUE, maxItems = 5)),
                                                uiOutput("passion_sliders")
                                       )
                           )
                  ),
                  tabPanel("Stats",
                           h4("Derived Attributes"),
                           tableOutput("derived"),
                           h4("Hit Points"),
                           tableOutput("hp_tbl"),
                           h4("Movement Rates"),
                           tableOutput("movement_tbl")
                  ),
                  tabPanel("Standard Skills",
                           textInput("std_search", "Search skills", placeholder = "Type to filter..."),
                           DTOutput("std_dt")
                  ),
                  tabPanel("Professional Skills",
                           checkboxInput("show_pkg_prof", "Show background/career skills", value = TRUE),
                           selectInput("prof_category_filter", "Filter by category", 
                                       choices = c("All", names(skill_categories)), selected = "All"),
                           selectizeInput("prof_select", "Add / remove skills", 
                                          choices = prof_names, multiple = TRUE),
                           DTOutput("prof_dt")
                  ),
                  tabPanel("Combat",
                           h4("Combat Styles"),
                           actionButton("add_combat_style", "Add Combat Style"),
                           uiOutput("combat_styles_ui"),
                           hr(),
                           h4("Weapons & Equipment"),
                           p("Equipment tracking coming soon...")
                  ),
                  tabPanel("Summary",
                           h4("Character Summary"),
                           uiOutput("character_summary"),
                           hr(),
                           actionButton("print_sheet", "🖨️ Print Character Sheet", class = "btn-info")
                  )
      )
    )
  )
)

# ------------------ Server --------------------------------------------------
server <- function(input, output, session) {
  
  # ---------- Reactive Values ----------------------------------------------
  values <- reactiveValues(
    combat_styles = list(),
    combat_style_count = 0
  )
  
  # ---------- Dynamic slider UIs ------------------------------------------
  slider_ui_id <- function(prefix, skill) paste0(prefix, "_", gsub("[^A-Za-z0-9]", "", skill))
  
  build_bg_controls <- function(skills) {
    lapply(skills, function(s) {
      # Show checkbox for all skills - they select which to include as professional skills
      div(style = "display:flex;align-items:center;gap:6px;margin-bottom:5px;",
          tags$div(style = "flex:1;",
                   sliderInput(slider_ui_id("bg", s), s, 
                               min = MIN_PER_SKILL, max = MAX_BGCAREER, value = MIN_PER_SKILL, 
                               step = 1, width = "100%")),
          checkboxInput(paste0("bgchk_", gsub("[^A-Za-z0-9]", "", s)), 
                        "Professional skill", value = !(s %in% std_names), width = "150px")
      )
    })
  }
  
  build_sliders <- function(skills, prefix, minv = MIN_PER_SKILL, maxv = MAX_BGCAREER) {
    lapply(skills, function(s) {
      sliderInput(slider_ui_id(prefix, s), s, 
                  min = minv, max = maxv, value = minv, 
                  step = 1, width = "100%")
    })
  }
  
  build_career_controls <- function(skills) {
    lapply(skills, function(s) {
      # Show checkbox for all skills - they select which to include as professional skills
      div(style = "display:flex;align-items:center;gap:6px;margin-bottom:5px;",
          tags$div(style = "flex:1;",
                   sliderInput(slider_ui_id("cr", s), s, 
                               min = MIN_PER_SKILL, max = MAX_BGCAREER, value = MIN_PER_SKILL, 
                               step = 1, width = "100%")),
          checkboxInput(paste0("crchk_", gsub("[^A-Za-z0-9]", "", s)), 
                        "Professional skill", value = !(s %in% std_names), width = "150px")
      )
    })
  }
  
  output$bg_sliders <- renderUI({ build_bg_controls(bg_packages[[input$background]]) })
  output$career_sliders <- renderUI({ build_career_controls(career_packages[[input$career]]) })
  
  age_skill_list <- reactive({
    base <- unique(c(bg_packages[[input$background]], career_packages[[input$career]]))
    extra <- if(!is.null(input$age_new_prof) && nzchar(input$age_new_prof)) input$age_new_prof else NULL
    unique(c(base, extra))
  })
  
  output$age_sliders <- renderUI({ build_sliders(age_skill_list(), "age", 0, MAX_AGE) })
  
  # ---------- Passion sliders ----------------------------------------------
  output$passion_sliders <- renderUI({
    if(length(input$passions) > 0) {
      lapply(input$passions, function(p) {
        sliderInput(paste0("passion_", gsub("[^A-Za-z0-9]", "", p)), 
                    p, min = 10, max = 100, value = 60, step = 5)
      })
    }
  })
  
  # ---------- Pool calculations -------------------------------------------
  `%||%` <- function(a, b) if(!is.null(a)) a else b
  
  get_alloc <- function(skills, prefix) {
    sapply(skills, function(s) input[[slider_ui_id(prefix, s)]] %||% 0)
  }
  
  bg_alloc <- reactive({ get_alloc(bg_packages[[input$background]], "bg") })
  career_alloc <- reactive({ get_alloc(career_packages[[input$career]], "cr") })
  age_alloc <- reactive({ get_alloc(age_skill_list(), "age") })
  
  bg_left <- reactive({ BG_POOL - sum(bg_alloc()) })
  career_left <- reactive({ CAREER_POOL - sum(career_alloc()) })
  age_left <- reactive({ AGE_POOLS[input$age_cat] - sum(age_alloc()) })
  
  output$bg_left <- renderText({ bg_left() })
  output$career_left <- renderText({ career_left() })
  output$age_left <- renderText({ age_left() })
  
  # Pool validation
  observe({
    if(bg_left() < 0) {
      shinyjs::addClass("bg_points_span", "over-budget")
      shinyjs::html("bg_validation", "⚠️ Over budget! Please reduce allocations.")
    } else {
      shinyjs::removeClass("bg_points_span", "over-budget")
      shinyjs::html("bg_validation", "")
    }
  })
  
  observe({
    if(career_left() < 0) {
      shinyjs::addClass("career_points_span", "over-budget")
      shinyjs::html("career_validation", "⚠️ Over budget! Please reduce allocations.")
    } else {
      shinyjs::removeClass("career_points_span", "over-budget")
      shinyjs::html("career_validation", "")
    }
  })
  
  observe({
    if(age_left() < 0) {
      shinyjs::addClass("age_points_span", "over-budget")
      shinyjs::html("age_validation", "⚠️ Over budget! Please reduce allocations.")
    } else {
      shinyjs::removeClass("age_points_span", "over-budget")
      shinyjs::html("age_validation", "")
    }
  })
  
  bg_show_flags <- reactive({
    skills <- bg_packages[[input$background]]
    if(is.null(skills) || length(skills) == 0) {
      return(logical(0))
    }
    
    flags <- sapply(skills, function(s) {
      val <- input[[paste0("bgchk_", gsub("[^A-Za-z0-9]", "", s))]]
      if(is.null(val)) FALSE else val
    })
    
    # Return named logical vector
    setNames(flags, skills)
  })
  
  # ---------- Bonus stores -------------------------------------------------
  bonuses <- reactiveValues(
    derived_std = setNames(rep(0, length(std_names)), std_names),
    derived_prof = setNames(rep(0, length(prof_names)), prof_names),
    exp_std = setNames(rep(0, length(std_names)), std_names),
    exp_prof = setNames(rep(0, length(prof_names)), prof_names)
  )
  
  # Recompute derived bonuses
  observe({
    ds <- setNames(rep(0, length(std_names)), std_names)
    dp <- setNames(rep(0, length(prof_names)), prof_names)
    
    # Background
    for(i in seq_along(bg_packages[[input$background]])) {
      sk <- bg_packages[[input$background]][i]
      pts <- bg_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk] + pts else dp[sk] <- dp[sk] + pts
    }
    
    # Career
    for(i in seq_along(career_packages[[input$career]])) {
      sk <- career_packages[[input$career]][i]
      pts <- career_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk] + pts else dp[sk] <- dp[sk] + pts
    }
    
    # Age
    for(i in seq_along(age_skill_list())) {
      sk <- age_skill_list()[i]
      pts <- age_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk] + pts else dp[sk] <- dp[sk] + pts
    }
    
    bonuses$derived_std <- ds
    bonuses$derived_prof <- dp
  })
  
  # ---------- Experience change handler -----------------------------------
  observeEvent(input$exp_change, {
    tbl <- input$exp_change$table
    skill <- input$exp_change$skill
    delta <- input$exp_change$delta
    key <- if(tbl == "std") "exp_std" else "exp_prof"
    bonuses[[key]][skill] <- max(0, bonuses[[key]][skill] + delta)
  })
  
  # ---------- Roll stats ---------------------------------------------------
  observeEvent(input$roll_stats, {
    roll3d6 <- function() sum(sample(1:6, 3, replace = TRUE))
    roll2d6plus6 <- function() sum(sample(1:6, 2, replace = TRUE)) + 6
    
    updateNumericInput(session, "STR", value = roll3d6())
    updateNumericInput(session, "CON", value = roll3d6())
    updateNumericInput(session, "SIZ", value = roll2d6plus6())
    updateNumericInput(session, "DEX", value = roll3d6())
    updateNumericInput(session, "INT", value = roll2d6plus6())
    updateNumericInput(session, "POW", value = roll3d6())
    updateNumericInput(session, "CHA", value = roll3d6())
    
    showNotification("Stats rolled!", type = "message", duration = 2)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # ---------- Derived attributes & HP -------------------------------------
  derived <- reactive({
    STR <- input$STR; CON <- input$CON; SIZ <- input$SIZ
    DEX <- input$DEX; INT <- input$INT; POW <- input$POW; CHA <- input$CHA
    
    ap <- ifelse(INT + DEX <= 12, 1,
                 ifelse(INT + DEX <= 24, 2,
                        ifelse(INT + DEX <= 36, 3, 3 + floor((INT + DEX - 25) / 12))))
    
    dmg_brks <- c(-Inf, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, Inf)
    dmg_lbl <- c("-1d8", "-1d6", "-1d4", "-1d2", "0", "+1d2", "+1d4", "+1d6", 
                 "+1d8", "+1d10", "+2d6", "+3d6", "+4d6", "+5d6")
    dmg_mod <- as.character(cut(STR + SIZ, breaks = dmg_brks, labels = dmg_lbl))
    
    heal_rate <- ceiling(CON / 5) + 1
    init_bonus <- floor((DEX + INT) / 2)
    luck_pts <- ifelse(POW <= 6, 1,
                       ifelse(POW <= 12, 2,
                              ifelse(POW <= 18, 3, 3 + floor((POW - 18) / 6))))
    
    data.frame(
      Attribute = c("Action Points", "Damage Modifier", "Healing Rate", 
                    "Initiative Bonus", "Luck Points", "Magic Points"),
      Value = c(ap, dmg_mod, heal_rate, init_bonus, luck_pts, POW)
    )
  })
  
  output$derived <- renderTable(derived(), striped = TRUE, colnames = FALSE)
  
  # Movement rates
  output$movement_tbl <- renderTable({
    data.frame(
      Type = c("Base Movement", "Sprint"),
      Value = c("6 metres", "18 metres")
    )
  }, striped = TRUE, colnames = FALSE)
  
  # HP calculation
  hp_values <- list(
    leg = c(1, 2, 3, 4, 5, 6, 7, 8),
    abdomen = c(2, 3, 4, 5, 6, 7, 8, 9),
    chest = c(3, 4, 5, 6, 7, 8, 9, 10),
    arm = c(1, 1, 2, 3, 4, 5, 6, 7),
    head = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
  
  hp_lookup <- function(total, part) {
    idx <- pmax(1, pmin(8, findInterval(total, c(5, 10, 15, 20, 25, 30, 35, 40)) + 1))
    val <- hp_values[[part]][idx]
    if(total > 40) val <- val + floor((total - 40) / 5) + 1
    val
  }
  
  output$hp_tbl <- renderTable({
    total <- input$CON + input$SIZ
    data.frame(
      Location = c("Left Leg", "Right Leg", "Abdomen", "Chest", 
                   "Left Arm", "Right Arm", "Head"),
      HP = c(hp_lookup(total, "leg"), hp_lookup(total, "leg"),
             hp_lookup(total, "abdomen"), hp_lookup(total, "chest"),
             hp_lookup(total, "arm"), hp_lookup(total, "arm"),
             hp_lookup(total, "head"))
    )
  }, striped = TRUE, colnames = FALSE)
  
  # ---------- Skill tables -------------------------------------------------
  base_std <- reactive({
    vars <- list(STR = input$STR, CON = input$CON, SIZ = input$SIZ,
                 DEX = input$DEX, INT = input$INT, POW = input$POW, CHA = input$CHA)
    sapply(std_formulas, function(f) do.call(f, vars))
  })
  
  base_prof <- reactive({
    vars <- list(STR = input$STR, CON = input$CON, SIZ = input$SIZ,
                 DEX = input$DEX, INT = input$INT, POW = input$POW, CHA = input$CHA)
    sapply(prof_formulas, function(f) do.call(f, vars))
  })
  
  build_dt <- function(skills, base, derived, exp, table_id, search = "") {
    # Filter by search
    if(nzchar(search)) {
      skills <- skills[grepl(search, skills, ignore.case = TRUE)]
    }
    
    if(length(skills) == 0) return(NULL)
    
    tbl <- data.frame(
      Skill = skills,
      Base = base[skills],
      Derived = derived[skills],
      Exp = exp[skills],
      Total = base[skills] + derived[skills] + exp[skills],
      stringsAsFactors = FALSE
    )
    
    # Add button HTML
    tbl$Exp <- mapply(function(skill, val) {
      paste0('<button class="exp-minus btn btn-sm btn-default" data-table="', table_id,
             '" data-skill="', skill, '">−</button> ',
             val,
             ' <button class="exp-plus btn btn-sm btn-default" data-table="', table_id,
             '" data-skill="', skill, '">+</button>')
    }, skills, exp[skills])
    
    # Color code skill levels
    tbl$Total <- mapply(function(total) {
      color <- if(total >= 90) "darkgreen"
      else if(total >= 70) "green"
      else if(total >= 50) "orange"
      else if(total >= 30) "black"
      else "gray"
      paste0('<span style="color:', color, ';font-weight:bold;">', total, '%</span>')
    }, tbl$Total)
    
    datatable(tbl, escape = FALSE, options = list(
      dom = 'tp',
      pageLength = 100,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:4)
      )
    ))
  }
  
  output$std_dt <- renderDT({
    build_dt(std_names, base_std(), bonuses$derived_std, bonuses$exp_std, "std", input$std_search)
  })
  
  career_show_flags <- reactive({
    skills <- career_packages[[input$career]]
    if(is.null(skills) || length(skills) == 0) {
      return(logical(0))
    }
    
    flags <- sapply(skills, function(s) {
      val <- input[[paste0("crchk_", gsub("[^A-Za-z0-9]", "", s))]]
      if(is.null(val)) {
        # Default to TRUE for professional skills, FALSE for standard skills
        !(s %in% std_names)
      } else {
        val
      }
    })
    
    # Return named logical vector
    setNames(flags, skills)
  })
  
  prof_skill_list <- reactive({
    # Get all background skills that have checkbox checked
    bg_skills <- bg_packages[[input$background]]
    bg_flags <- bg_show_flags()
    
    # Filter for checked skills
    checked_bg_skills <- if(length(bg_flags) > 0) {
      bg_skills[bg_flags]
    } else {
      character(0)
    }
    
    # Only keep professional skills from background
    checked_bg_prof <- checked_bg_skills[!(checked_bg_skills %in% std_names)]
    
    # Get all career skills that have checkbox checked
    career_skills <- career_packages[[input$career]]
    career_flags <- career_show_flags()
    
    # Filter for checked career skills
    checked_career_skills <- if(length(career_flags) > 0) {
      career_skills[career_flags]
    } else {
      character(0)
    }
    
    # Only keep professional skills from career
    checked_career_prof <- checked_career_skills[!(checked_career_skills %in% std_names)]
    
    # Combine all sources
    pkg_skills <- unique(c(checked_career_prof, checked_bg_prof))
    sel_skills <- input$prof_select
    
    # Get skills with points allocated
    derived_skills <- names(bonuses$derived_prof)[bonuses$derived_prof > 0]
    exp_skills <- names(bonuses$exp_prof)[bonuses$exp_prof > 0]
    
    full <- unique(c(pkg_skills, sel_skills, derived_skills, exp_skills))
    
    if(!isTRUE(input$show_pkg_prof)) {
      full <- unique(c(sel_skills, derived_skills, exp_skills))
    }
    
    # Filter by category
    if(!is.null(input$prof_category_filter) && input$prof_category_filter != "All") {
      cat_skills <- skill_categories[[input$prof_category_filter]]
      full <- intersect(full, cat_skills)
    }
    
    # Return empty character vector if no skills, otherwise sort
    if(length(full) == 0) character(0) else sort(full)
  })
  
  output$prof_dt <- renderDT({
    skills <- prof_skill_list()
    if(length(skills) == 0) {
      # Return empty data table with message
      datatable(
        data.frame(Message = "No professional skills selected. Use the selector above or allocate points to professional skills from your background/career."),
        options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE)
      )
    } else {
      build_dt(skills, base_prof(), bonuses$derived_prof, bonuses$exp_prof, "prof")
    }
  })
  
  # ---------- Combat Styles ------------------------------------------------
  observeEvent(input$add_combat_style, {
    values$combat_style_count <- values$combat_style_count + 1
    style_id <- paste0("combat_style_", values$combat_style_count)
    
    values$combat_styles[[style_id]] <- list(
      name = paste("Combat Style", values$combat_style_count),
      value = 0
    )
  })
  
  output$combat_styles_ui <- renderUI({
    if(length(values$combat_styles) > 0) {
      lapply(names(values$combat_styles), function(style_id) {
        div(class = "combat-style-entry", style = "margin-bottom: 10px;",
            fluidRow(
              column(6, 
                     textInput(paste0(style_id, "_name"), NULL, 
                               value = values$combat_styles[[style_id]]$name,
                               placeholder = "Style name (e.g., Sword & Shield)")
              ),
              column(4,
                     numericInput(paste0(style_id, "_value"), NULL, 
                                  value = values$combat_styles[[style_id]]$value,
                                  min = 0, max = 100)
              ),
              column(2,
                     actionButton(paste0(style_id, "_remove"), "✖", 
                                  class = "btn-danger btn-sm",
                                  style = "margin-top: 25px;")
              )
            )
        )
      })
    }
  })
  
  # ---------- Character Summary --------------------------------------------
  output$character_summary <- renderUI({
    name <- if(nzchar(input$name)) input$name else "Unnamed Character"
    player <- if(nzchar(input$player)) input$player else "Unknown Player"
    
    # Get highest skills
    std_totals <- base_std() + bonuses$derived_std + bonuses$exp_std
    prof_skills <- prof_skill_list()
    
    # Calculate professional skill totals only if there are any
    if(length(prof_skills) > 0) {
      prof_totals <- base_prof()[prof_skills] + bonuses$derived_prof[prof_skills] + 
        bonuses$exp_prof[prof_skills]
      all_skills <- c(std_totals, prof_totals)
    } else {
      all_skills <- std_totals
    }
    
    top_skills <- sort(all_skills, decreasing = TRUE)[1:min(10, length(all_skills))]
    
    div(
      h5(strong(name), " - ", input$background, " ", input$career),
      p("Player: ", player, " | Age: ", input$age, " (", input$age_cat, ") | ",
        "Gender: ", input$gender),
      hr(),
      h5("Characteristics"),
      p("STR: ", input$STR, " CON: ", input$CON, " SIZ: ", input$SIZ,
        " DEX: ", input$DEX, " INT: ", input$INT, " POW: ", input$POW,
        " CHA: ", input$CHA),
      hr(),
      h5("Top Skills"),
      tags$ul(
        lapply(names(top_skills), function(skill) {
          tags$li(paste0(skill, ": ", top_skills[skill], "%"))
        })
      ),
      if(length(input$passions) > 0) {
        div(
          hr(),
          h5("Passions"),
          tags$ul(
            lapply(input$passions, function(p) {
              val <- input[[paste0("passion_", gsub("[^A-Za-z0-9]", "", p))]] %||% 60
              tags$li(paste0(p, ": ", val, "%"))
            })
          )
        )
      }
    )
  })
  
  # ---------- Save / Load / Export -----------------------------------------
  get_character_data <- reactive({
    # Collect all sliders
    bg_sliders <- setNames(
      lapply(bg_packages[[input$background]], function(s) {
        input[[slider_ui_id("bg", s)]] %||% 0
      }),
      bg_packages[[input$background]]
    )
    
    career_sliders <- setNames(
      lapply(career_packages[[input$career]], function(s) {
        input[[slider_ui_id("cr", s)]] %||% 0
      }),
      career_packages[[input$career]]
    )
    
    age_sliders <- setNames(
      lapply(age_skill_list(), function(s) {
        input[[slider_ui_id("age", s)]] %||% 0
      }),
      age_skill_list()
    )
    
    # Collect passions
    passion_values <- if(length(input$passions) > 0) {
      setNames(
        lapply(input$passions, function(p) {
          input[[paste0("passion_", gsub("[^A-Za-z0-9]", "", p))]] %||% 60
        }),
        input$passions
      )
    } else NULL
    
    list(
      version = "1.0",
      name = input$name,
      player = input$player,
      age = input$age,
      gender = input$gender,
      handed = input$handed,
      stats = list(
        STR = input$STR, CON = input$CON, SIZ = input$SIZ,
        DEX = input$DEX, INT = input$INT, POW = input$POW, CHA = input$CHA
      ),
      background = input$background,
      career = input$career,
      age_cat = input$age_cat,
      prof_select = input$prof_select,
      age_new_prof = input$age_new_prof,
      sliders = list(
        bg = bg_sliders,
        career = career_sliders,
        age = age_sliders
      ),
      bg_show_flags = bg_show_flags(),
      career_show_flags = career_show_flags(),
      passions = input$passions,
      passion_values = passion_values,
      exp_std = bonuses$exp_std,
      exp_prof = bonuses$exp_prof,
      combat_styles = values$combat_styles
    )
  })
  
  observeEvent(input$save, {
    if(bg_left() < 0 || career_left() < 0 || age_left() < 0) {
      showNotification("Cannot save - fix over-budget allocations first!", 
                       type = "error", duration = 3)
      return()
    }
    
    js$saveChar(get_character_data())
  })
  
  observeEvent(input$save_success, {
    showNotification("Character saved successfully!", type = "success", duration = 2)
  })
  
  observeEvent(input$load, {
    js$loadChar(NULL)
  })
  
  observeEvent(input$load_error, {
    showNotification("No saved character found!", type = "warning", duration = 3)
  })
  
  load_character_data <- function(data) {
    # Basic info
    updateTextInput(session, "name", value = data$name %||% "")
    updateTextInput(session, "player", value = data$player %||% "")
    updateNumericInput(session, "age", value = data$age %||% 25)
    updateSelectInput(session, "gender", selected = data$gender %||% "Male")
    updateSelectInput(session, "handed", selected = data$handed %||% "Right")
    
    # Stats
    updateNumericInput(session, "STR", value = data$stats$STR %||% 10)
    updateNumericInput(session, "CON", value = data$stats$CON %||% 10)
    updateNumericInput(session, "SIZ", value = data$stats$SIZ %||% 10)
    updateNumericInput(session, "DEX", value = data$stats$DEX %||% 10)
    updateNumericInput(session, "INT", value = data$stats$INT %||% 10)
    updateNumericInput(session, "POW", value = data$stats$POW %||% 10)
    updateNumericInput(session, "CHA", value = data$stats$CHA %||% 10)
    
    # Character options
    updateSelectInput(session, "background", selected = data$background)
    updateSelectInput(session, "career", selected = data$career)
    updateSelectInput(session, "age_cat", selected = data$age_cat)
    updateSelectizeInput(session, "prof_select", selected = data$prof_select)
    updateSelectizeInput(session, "age_new_prof", selected = data$age_new_prof)
    
    # Passions
    updateSelectizeInput(session, "passions", selected = data$passions)
    
    # Restore sliders after a delay to ensure UI is built
    shinyjs::delay(500, {
      # Background sliders
      if(!is.null(data$sliders$bg)) {
        for(skill in names(data$sliders$bg)) {
          updateSliderInput(session, slider_ui_id("bg", skill), 
                            value = data$sliders$bg[[skill]])
        }
      }
      
      # Career sliders
      if(!is.null(data$sliders$career)) {
        for(skill in names(data$sliders$career)) {
          updateSliderInput(session, slider_ui_id("cr", skill), 
                            value = data$sliders$career[[skill]])
        }
      }
      
      # Age sliders
      if(!is.null(data$sliders$age)) {
        for(skill in names(data$sliders$age)) {
          updateSliderInput(session, slider_ui_id("age", skill), 
                            value = data$sliders$age[[skill]])
        }
      }
      
      # Background checkboxes
      if(!is.null(data$bg_show_flags)) {
        for(skill in names(data$bg_show_flags)) {
          updateCheckboxInput(session, 
                              paste0("bgchk_", gsub("[^A-Za-z0-9]", "", skill)),
                              value = data$bg_show_flags[[skill]])
        }
      }
      
      # Career checkboxes
      if(!is.null(data$career_show_flags)) {
        for(skill in names(data$career_show_flags)) {
          updateCheckboxInput(session, 
                              paste0("crchk_", gsub("[^A-Za-z0-9]", "", skill)),
                              value = data$career_show_flags[[skill]])
        }
      }
      
      # Passion values
      if(!is.null(data$passion_values)) {
        for(passion in names(data$passion_values)) {
          updateSliderInput(session,
                            paste0("passion_", gsub("[^A-Za-z0-9]", "", passion)),
                            value = data$passion_values[[passion]])
        }
      }
    })
    
    # Experience points
    bonuses$exp_std <- data$exp_std %||% setNames(rep(0, length(std_names)), std_names)
    bonuses$exp_prof <- data$exp_prof %||% setNames(rep(0, length(prof_names)), prof_names)
    
    # Combat styles
    values$combat_styles <- data$combat_styles %||% list()
    values$combat_style_count <- length(values$combat_styles)
    
    showNotification("Character loaded successfully!", type = "success", duration = 2)
  }
  
  observeEvent(input$loadedChar, {
    tryCatch({
      data <- fromJSON(input$loadedChar)
      load_character_data(data)
    }, error = function(e) {
      showNotification("Error loading character data!", type = "error", duration = 3)
    })
  })
  
  # Export
  observeEvent(input$export, {
    data <- get_character_data()
    data$name <- ifelse(nzchar(input$name), input$name, "mythras_character")
    js$exportChar(data)
  })
  
  # Import
  observeEvent(input$import_file, {
    req(input$import_file)
    
    tryCatch({
      data <- fromJSON(readLines(input$import_file$datapath))
      load_character_data(data)
    }, error = function(e) {
      showNotification("Error importing character file!", type = "error", duration = 3)
    })
  })
  
  # Reset
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Reset Character?",
      "Are you sure you want to reset the entire character? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    removeModal()
    session$reload()
  })
  
  # Print sheet
  observeEvent(input$print_sheet, {
    # Generate printable HTML
    printable_html <- generate_printable_sheet()
    
    # Create a temporary HTML file
    temp_file <- tempfile(fileext = ".html")
    writeLines(printable_html, temp_file)
    
    # Open in new window for printing
    browseURL(temp_file)
    
    showNotification("Character sheet opened in new window. Use browser's print function to save as PDF.", 
                     type = "message", duration = 5)
  })
  
  # Generate printable character sheet HTML
  generate_printable_sheet <- function() {
    # Get character data
    name <- if(nzchar(input$name)) input$name else "Unnamed Character"
    player <- if(nzchar(input$player)) input$player else "Unknown Player"
    
    # Calculate all skills
    std_skills <- data.frame(
      Skill = std_names,
      Base = base_std()[std_names],
      Derived = bonuses$derived_std[std_names],
      Exp = bonuses$exp_std[std_names],
      Total = base_std()[std_names] + bonuses$derived_std[std_names] + bonuses$exp_std[std_names]
    )
    
    prof_skills_list <- prof_skill_list()
    if(length(prof_skills_list) > 0) {
      prof_skills <- data.frame(
        Skill = prof_skills_list,
        Base = base_prof()[prof_skills_list],
        Derived = bonuses$derived_prof[prof_skills_list],
        Exp = bonuses$exp_prof[prof_skills_list],
        Total = base_prof()[prof_skills_list] + bonuses$derived_prof[prof_skills_list] + 
          bonuses$exp_prof[prof_skills_list]
      )
    } else {
      prof_skills <- data.frame(Skill = character(0), Base = numeric(0), 
                                Derived = numeric(0), Exp = numeric(0), Total = numeric(0))
    }
    
    # Get derived attributes
    STR <- input$STR; CON <- input$CON; SIZ <- input$SIZ
    DEX <- input$DEX; INT <- input$INT; POW <- input$POW; CHA <- input$CHA
    
    ap <- ifelse(INT + DEX <= 12, 1,
                 ifelse(INT + DEX <= 24, 2,
                        ifelse(INT + DEX <= 36, 3, 3 + floor((INT + DEX - 25) / 12))))
    
    dmg_brks <- c(-Inf, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, Inf)
    dmg_lbl <- c("-1d8", "-1d6", "-1d4", "-1d2", "0", "+1d2", "+1d4", "+1d6", 
                 "+1d8", "+1d10", "+2d6", "+3d6", "+4d6", "+5d6")
    dmg_mod <- as.character(cut(STR + SIZ, breaks = dmg_brks, labels = dmg_lbl))
    
    heal_rate <- ceiling(CON / 5) + 1
    init_bonus <- floor((DEX + INT) / 2)
    luck_pts <- ifelse(POW <= 6, 1,
                       ifelse(POW <= 12, 2,
                              ifelse(POW <= 18, 3, 3 + floor((POW - 18) / 6))))
    
    # Calculate HP
    hp_values <- list(
      leg = c(1, 2, 3, 4, 5, 6, 7, 8),
      abdomen = c(2, 3, 4, 5, 6, 7, 8, 9),
      chest = c(3, 4, 5, 6, 7, 8, 9, 10),
      arm = c(1, 1, 2, 3, 4, 5, 6, 7),
      head = c(1, 2, 3, 4, 5, 6, 7, 8)
    )
    
    hp_lookup <- function(total, part) {
      idx <- pmax(1, pmin(8, findInterval(total, c(5, 10, 15, 20, 25, 30, 35, 40)) + 1))
      val <- hp_values[[part]][idx]
      if(total > 40) val <- val + floor((total - 40) / 5) + 1
      val
    }
    
    total_hp <- CON + SIZ
    
    # Generate HTML
    html <- paste0('
<!DOCTYPE html>
<html>
<head>
<title>', name, ' - Mythras Character Sheet</title>
<style>
  @media print {
    body { margin: 0; }
    .page-break { page-break-after: always; }
  }
  body {
    font-family: Arial, sans-serif;
    font-size: 10pt;
    line-height: 1.2;
    margin: 20px;
  }
  h1 { font-size: 18pt; margin: 10px 0; }
  h2 { font-size: 14pt; margin: 8px 0; border-bottom: 2px solid black; }
  h3 { font-size: 12pt; margin: 6px 0; }
  table {
    border-collapse: collapse;
    width: 100%;
    margin-bottom: 10px;
  }
  th, td {
    border: 1px solid black;
    padding: 3px 5px;
    text-align: left;
  }
  th { background-color: #f0f0f0; font-weight: bold; }
  .stats-table { width: auto; display: inline-block; margin-right: 20px; }
  .info-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-bottom: 10px; }
  .info-item { border: 1px solid black; padding: 5px; }
  .skill-section { margin-bottom: 15px; }
  .hp-table { width: auto; }
  .two-column { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
</style>
</head>
<body>

<h1>MYTHRAS CHARACTER SHEET</h1>

<div class="info-grid">
  <div class="info-item"><strong>Name:</strong> ', name, '</div>
  <div class="info-item"><strong>Player:</strong> ', player, '</div>
  <div class="info-item"><strong>Culture:</strong> ', input$background, '</div>
  <div class="info-item"><strong>Career:</strong> ', input$career, '</div>
  <div class="info-item"><strong>Age:</strong> ', input$age, ' (', input$age_cat, ')</div>
  <div class="info-item"><strong>Gender:</strong> ', input$gender, '</div>
</div>

<h2>Characteristics & Attributes</h2>
<div style="display: flex; gap: 20px; flex-wrap: wrap;">
  <table class="stats-table">
    <tr><th>Characteristic</th><th>Value</th></tr>
    <tr><td>STR</td><td>', STR, '</td></tr>
    <tr><td>CON</td><td>', CON, '</td></tr>
    <tr><td>SIZ</td><td>', SIZ, '</td></tr>
    <tr><td>DEX</td><td>', DEX, '</td></tr>
    <tr><td>INT</td><td>', INT, '</td></tr>
    <tr><td>POW</td><td>', POW, '</td></tr>
    <tr><td>CHA</td><td>', CHA, '</td></tr>
  </table>
  
  <table class="stats-table">
    <tr><th>Attribute</th><th>Value</th></tr>
    <tr><td>Action Points</td><td>', ap, '</td></tr>
    <tr><td>Damage Modifier</td><td>', dmg_mod, '</td></tr>
    <tr><td>Healing Rate</td><td>', heal_rate, '</td></tr>
    <tr><td>Initiative Bonus</td><td>', init_bonus, '</td></tr>
    <tr><td>Luck Points</td><td>', luck_pts, '/□□□</td></tr>
    <tr><td>Magic Points</td><td>', POW, '/□□□□□□□□□□</td></tr>
    <tr><td>Movement</td><td>6m</td></tr>
  </table>
  
  <table class="hp-table">
    <tr><th>Location</th><th>AP</th><th>HP</th></tr>
    <tr><td>Head</td><td>□</td><td>', hp_lookup(total_hp, "head"), '/□□□□□□□□</td></tr>
    <tr><td>Chest</td><td>□</td><td>', hp_lookup(total_hp, "chest"), '/□□□□□□□□□□</td></tr>
    <tr><td>Abdomen</td><td>□</td><td>', hp_lookup(total_hp, "abdomen"), '/□□□□□□□□□</td></tr>
    <tr><td>Right Arm</td><td>□</td><td>', hp_lookup(total_hp, "arm"), '/□□□□□□□</td></tr>
    <tr><td>Left Arm</td><td>□</td><td>', hp_lookup(total_hp, "arm"), '/□□□□□□□</td></tr>
    <tr><td>Right Leg</td><td>□</td><td>', hp_lookup(total_hp, "leg"), '/□□□□□□□□</td></tr>
    <tr><td>Left Leg</td><td>□</td><td>', hp_lookup(total_hp, "leg"), '/□□□□□□□□</td></tr>
  </table>
</div>

<h2>Standard Skills</h2>
<table>
  <tr><th>Skill</th><th>Base</th><th>Culture/Career</th><th>Bonus</th><th>Total</th><th>Exp</th></tr>')
    
    # Add standard skills
    for(i in 1:nrow(std_skills)) {
      html <- paste0(html, '
  <tr>
    <td>', std_skills$Skill[i], '</td>
    <td>', std_skills$Base[i], '</td>
    <td>', std_skills$Derived[i], '</td>
    <td>', std_skills$Exp[i], '</td>
    <td><strong>', std_skills$Total[i], '%</strong></td>
    <td>□</td>
  </tr>')
    }
    
    html <- paste0(html, '
</table>

<div class="page-break"></div>

<h2>Professional Skills</h2>
<table>
  <tr><th>Skill</th><th>Base</th><th>Culture/Career</th><th>Bonus</th><th>Total</th><th>Exp</th></tr>')
    
    # Add professional skills
    if(nrow(prof_skills) > 0) {
      for(i in 1:nrow(prof_skills)) {
        html <- paste0(html, '
  <tr>
    <td>', prof_skills$Skill[i], '</td>
    <td>', prof_skills$Base[i], '</td>
    <td>', prof_skills$Derived[i], '</td>
    <td>', prof_skills$Exp[i], '</td>
    <td><strong>', prof_skills$Total[i], '%</strong></td>
    <td>□</td>
  </tr>')
      }
    }
    
    # Add empty rows for additional skills
    for(i in 1:10) {
      html <- paste0(html, '
  <tr>
    <td>_____________________</td>
    <td>____</td>
    <td>____</td>
    <td>____</td>
    <td>____</td>
    <td>□</td>
  </tr>')
    }
    
    html <- paste0(html, '
</table>')
    
    # Add passions if any
    if(length(input$passions) > 0) {
      html <- paste0(html, '
<h2>Passions</h2>
<table style="width: 50%;">
  <tr><th>Passion</th><th>Score</th><th>Exp</th></tr>')
      
      for(p in input$passions) {
        val <- input[[paste0("passion_", gsub("[^A-Za-z0-9]", "", p))]] %||% 60
        html <- paste0(html, '
  <tr>
    <td>', p, '</td>
    <td><strong>', val, '%</strong></td>
    <td>□</td>
  </tr>')
      }
      
      html <- paste0(html, '
</table>')
    }
    
    # Add combat styles
    if(length(values$combat_styles) > 0) {
      html <- paste0(html, '
<h2>Combat Styles</h2>
<table style="width: 50%;">
  <tr><th>Style</th><th>Score</th><th>Exp</th></tr>')
      
      for(style_id in names(values$combat_styles)) {
        style_name <- input[[paste0(style_id, "_name")]] %||% values$combat_styles[[style_id]]$name
        style_value <- input[[paste0(style_id, "_value")]] %||% values$combat_styles[[style_id]]$value
        html <- paste0(html, '
  <tr>
    <td>', style_name, '</td>
    <td><strong>', style_value, '%</strong></td>
    <td>□</td>
  </tr>')
      }
      
      html <- paste0(html, '
</table>')
    }
    
    html <- paste0(html, '

<h2>Equipment & Notes</h2>
<div style="border: 1px solid black; min-height: 200px; padding: 10px;">
  <p>Equipment:</p>
  <br/><br/><br/><br/>
  <p>Notes:</p>
  <br/><br/><br/><br/>
</div>

<div style="margin-top: 20px; text-align: center; font-size: 8pt;">
  Generated from Mythras Character Creator - ', Sys.Date(), '
</div>

</body>
</html>')
    
    return(html)
  }
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)