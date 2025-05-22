#| standalone: true
# Mythras character tracker – full Shinylive‑compatible app (v4 – exp buttons working, skill‑visibility)
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
});
Shiny.addCustomMessageHandler('loadChar', function(x){
  const saved = localStorage.getItem('mythrasChar');
  Shiny.setInputValue('loadedChar', saved);
});
"

# ---------------------------------------------------------------------------
# Rulebook constants (p. 12‑13) --------------------------------------------
BG_POOL       <- 100
CAREER_POOL   <- 100
AGE_POOLS     <- c(Young=100, Adult=150, `Middle‑Aged`=200, Senior=250, Old=300)
MIN_PER_SKILL <- 5
MAX_BGCAREER  <- 15
MAX_AGE       <- 15

# Background & career packages ---------------------------------------------
# (shortened list for demo – extend as needed)
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
  Priest  = c("Customs","Influence","Insight","Willpower","Culture","Lore","Oratory","Teach")
)

# -------------------------- Skill base formulae ---------------------------
a2 <- function(x,y) x+y
std_formulas <- list(
  Athletics = \(STR,DEX,...) a2(STR,DEX),   Boating   = \(STR,CON,...) a2(STR,CON),
  Brawn     = \(STR,SIZ,...) a2(STR,SIZ),   Conceal   = \(DEX,POW,...) a2(DEX,POW),
  Customs   = \(INT,...)     INT*2,         Dance     = \(DEX,CHA,...) a2(DEX,CHA),
  Deceit    = \(INT,CHA,...) a2(INT,CHA),   Drive     = \(DEX,POW,...) a2(DEX,POW),
  Endurance = \(CON,...)     CON*2,         Evade     = \(DEX,...)     DEX*2,
  FirstAid  = \(INT,DEX,...) a2(INT,DEX),   Influence = \(CHA,...)     CHA*2,
  Insight   = \(INT,POW,...) a2(INT,POW),   Locale    = \(INT,...)     INT*2,
  NativeTongue = \(INT,CHA,...) a2(INT,CHA),
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
  Culture    = \(INT,...) INT*2,            FolkMagic  = \(POW,CHA,...) a2(POW,CHA),
  Healing    = \(INT,POW,...) a2(INT,POW),  Invocation = \(INT,...) INT*2,
  Language   = \(INT,CHA,...) a2(INT,CHA),  Literacy   = \(INT,...) INT*2,
  Lore       = \(INT,...) INT*2,            Mechanisms = \(DEX,INT,...) a2(DEX,INT),
  Navigation = \(INT,POW,...) a2(INT,POW),  Oratory    = \(POW,CHA,...) a2(POW,CHA),
  Sleight    = \(DEX,CHA,...) a2(DEX,CHA),  Streetwise = \(POW,CHA,...) a2(POW,CHA),
  Survival   = \(CON,POW,...) a2(CON,POW),  Teach      = \(INT,CHA,...) a2(INT,CHA),
  Track      = \(INT,CON,...) a2(INT,CON)
)
prof_names <- sort(unique(names(prof_formulas)))

# ------------------ UI -----------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c()),
  titlePanel("Mythras Character Sheet (Shinylive)"),
  sidebarLayout(
    sidebarPanel(
      textInput("name","Name"),
      numericInput("STR","STR",10,3,21), numericInput("CON","CON",10,3,21),
      numericInput("SIZ","SIZ",10,8,21), numericInput("DEX","DEX",10,3,21),
      numericInput("INT","INT",10,8,21), numericInput("POW","POW",10,3,21),
      numericInput("CHA","CHA",10,3,21),
      hr(),
      tabsetPanel(id="attr_tabs",
                  tabPanel("Background",
                           selectInput("background","Culture",choices = names(bg_packages)),
                           uiOutput("bg_sliders"),
                           span("Points left:"), textOutput("bg_left", inline = TRUE)
                  ),
                  tabPanel("Career",
                           selectInput("career","Career", choices = names(career_packages)),
                           uiOutput("career_sliders"),
                           span("Points left:"), textOutput("career_left", inline = TRUE)
                  ),
                  tabPanel("Age",
                           selectInput("age_cat","Age Category", choices = names(AGE_POOLS), selected = "Adult"),
                           selectizeInput("age_new_prof","Add extra professional skill", choices = prof_names,
                                          multiple = FALSE, options = list(placeholder="(optional)")),
                           uiOutput("age_sliders"),
                           span("Points left:"), textOutput("age_left", inline = TRUE)
                  )
      ),
      hr(),
      actionButton("save","💾 Save"), actionButton("load","📂 Load")
    ),
    mainPanel(
      tabsetPanel(id="main_tabs",
                  tabPanel("Stats",
                           h4("Derived Attributes"), tableOutput("derived"),
                           h4("Hit Points"), tableOutput("hp_tbl")
                  ),
                  tabPanel("Standard Skills", DTOutput("std_dt")),
                  tabPanel("Professional Skills",
                           checkboxInput("show_pkg_prof","Show background/career skills", value = TRUE),
                           selectizeInput("prof_select","Add / remove skills", choices = prof_names, multiple = TRUE),
                           DTOutput("prof_dt")
                  )
      )
    )
  )
)

# ------------------ Server --------------------------------------------------
server <- function(input, output, session){
  
  # ---------- Dynamic slider UIs ------------------------------------------
  slider_ui_id <- function(prefix, skill) paste0(prefix, "_", gsub(" ", "", skill))
  
  build_bg_controls <- function(skills){
    lapply(skills, function(s){
      div(style="display:flex;align-items:center;gap:6px;",
          tags$div(style="flex:1;",
                   sliderInput(slider_ui_id("bg",s), s, min = MIN_PER_SKILL, max = MAX_BGCAREER, value = MIN_PER_SKILL, step = 1, width = "100%")),
          checkboxInput(paste0("bgchk_",gsub(" ","",s)), NULL, value = TRUE, width = "20px"))
    })
  }
  build_sliders <- function(skills, prefix, minv = MIN_PER_SKILL, maxv = MAX_BGCAREER){
    lapply(skills, function(s){
      sliderInput(slider_ui_id(prefix,s), s, min = minv, max = maxv, value = minv, step = 1, width = "100%")
    })
  }
  
  output$bg_sliders     <- renderUI({ build_bg_controls(bg_packages[[input$background]]) })
  output$career_sliders <- renderUI({ build_sliders(career_packages[[input$career]], "cr") })
  
  age_skill_list <- reactive({
    base <- unique(c(bg_packages[[input$background]], career_packages[[input$career]]))
    extra <- if(!is.null(input$age_new_prof) && nzchar(input$age_new_prof)) input$age_new_prof else NULL
    unique(c(base, extra))
  })
  output$age_sliders <- renderUI({ build_sliders(age_skill_list(), "age", 0, MAX_AGE) })
  
  # ---------- Pool calculations -------------------------------------------
  `%||%` <- function(a,b) if(!is.null(a)) a else b
  get_alloc <- function(skills, prefix){ sapply(skills, function(s) input[[slider_ui_id(prefix,s)]] %||% 0) }
  bg_alloc     <- reactive({ get_alloc(bg_packages[[input$background]], "bg") })
  career_alloc <- reactive({ get_alloc(career_packages[[input$career]], "cr") })
  age_alloc    <- reactive({ get_alloc(age_skill_list(), "age") })
  
  output$bg_left     <- renderText({ BG_POOL - sum(bg_alloc()) })
  output$career_left <- renderText({ CAREER_POOL - sum(career_alloc()) })
  output$age_left    <- renderText({ AGE_POOLS[input$age_cat] - sum(age_alloc()) })
  
  bg_show_flags <- reactive({ setNames(sapply(bg_packages[[input$background]], function(s) input[[paste0("bgchk_",gsub(" ","",s))]] %||% FALSE), bg_packages[[input$background]]) })
  
  # ---------- Bonus stores -------------------------------------------------
  bonuses <- reactiveValues(
    derived_std  = setNames(rep(0,length(std_names)), std_names),
    derived_prof = setNames(rep(0,length(prof_names)), prof_names),
    exp_std      = setNames(rep(0,length(std_names)), std_names),
    exp_prof     = setNames(rep(0,length(prof_names)), prof_names)
  )
  
  # recompute derived bonuses ---------------------------------------------
  observe({
    ds <- setNames(rep(0,length(std_names)), std_names)
    dp <- setNames(rep(0,length(prof_names)), prof_names)
    
    # background
    for(i in seq_along(bg_packages[[input$background]])){
      sk <- bg_packages[[input$background]][i]; pts <- bg_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk]+pts else dp[sk] <- dp[sk]+pts
    }
    # career
    for(i in seq_along(career_packages[[input$career]])){
      sk <- career_packages[[input$career]][i]; pts <- career_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk]+pts else dp[sk] <- dp[sk]+pts
    }
    # age
    for(i in seq_along(age_skill_list())){
      sk <- age_skill_list()[i]; pts <- age_alloc()[[i]]
      if(sk %in% std_names) ds[sk] <- ds[sk]+pts else dp[sk] <- dp[sk]+pts
    }
    
    bonuses$derived_std  <- ds
    bonuses$derived_prof <- dp
  })
  
  # ---------- Experience change handler -----------------------------------
  observeEvent(input$exp_change, {
    tbl   <- input$exp_change$table
    skill <- input$exp_change$skill
    delta <- input$exp_change$delta
    key   <- if(tbl=="std") "exp_std" else "exp_prof"
    bonuses[[key]][skill] <- max(0, bonuses[[key]][skill] + delta)
  })
  
  # ---------- Derived attributes & HP -------------------------------------
  derived <- reactive({
    STR<-input$STR;CON<-input$CON;SIZ<-input$SIZ;DEX<-input$DEX;INT<-input$INT;POW<-input$POW;CHA<-input$CHA
    ap <- ifelse(INT+DEX<=12,1,ifelse(INT+DEX<=24,2,ifelse(INT+DEX<=36,3,3+floor((INT+DEX-25)/12))))
    dmg_brks <- c(-Inf,5,10,15,20,25,30,35,40,45,50,60,70,80,Inf)
    dmg_lbl  <- c("-1d8","-1d6","-1d4","-1d2","0","+1d2","+1d4","+1d6","+1d8","+1d10","+2d6","+3d6","+4d6","+5d6")
    dmg_mod  <- as.character(cut(STR+SIZ, breaks=dmg_brks, labels=dmg_lbl))
    heal_rate<- ceiling(CON/5)+1
    init_bonus<- floor((DEX+INT)/2)
    luck_pts<- ifelse(POW<=6,1,ifelse(POW<=12,2,ifelse(POW<=18,3,3+floor((POW-18)/6))))
    data.frame(Attribute=c("Action Points","Damage Mod","Healing Rate","Initiative","Luck Points"),
               Value=c(ap,dmg_mod,heal_rate,init_bonus,luck_pts))
  })
  output$derived <- renderTable(derived(), striped=TRUE, colnames=FALSE)
  
  hp_values <- list(leg=c(1,2,3,4,5,6,7,8),abdomen=c(2,3,4,5,6,7,8,9),
                    chest=c(3,4,5,6,7,8,9,10),arm=c(1,1,2,3,4,5,6,7),head=c(1,2,3,4,5,6,7,8))
  hp_lookup <- function(total,part){ idx<-pmax(1,pmin(8,findInterval(total,c(5,10,15,20,25,30,35,40))+1));val<-hp_values[[part]][idx]; if(total>40) val<-val+floor((total-40)/5)+1; val }
  output$hp_tbl <- renderTable({ total<-input$CON+input$SIZ; data.frame(Location=c("Left Leg","Right Leg","Abdomen","Chest","Left Arm","Right Arm","Head"), HP=c(hp_lookup(total,"leg"),hp_lookup(total,"leg"),hp_lookup(total,"abdomen"),hp_lookup(total,"chest"),hp_lookup(total,"arm"),hp_lookup(total,"arm"),hp_lookup(total,"head"))) }, striped=TRUE, colnames=FALSE)
  
  # ---------- Skill tables -------------------------------------------------
  base_std <- reactive({
    vars <- list(STR=input$STR,CON=input$CON,SIZ=input$SIZ,DEX=input$DEX,INT=input$INT,POW=input$POW,CHA=input$CHA)
    sapply(std_formulas, function(f) do.call(f, vars))
  })
  base_prof <- reactive({
    vars <- list(STR=input$STR,CON=input$CON,SIZ=input$SIZ,DEX=input$DEX,INT=input$INT,POW=input$POW,CHA=input$CHA)
    sapply(prof_formulas, function(f) do.call(f, vars))
  })
  
  build_dt <- function(skills, base, derived, exp, table_id){
    tbl <- data.frame(
      Skill = skills,
      Base = base[skills],
      Derived = derived[skills],
      Exp = exp[skills],
      Total = base[skills] + derived[skills] + exp[skills],
      stringsAsFactors = FALSE
    )
    # add button html
    tbl$Exp <- mapply(function(skill,val){ paste0('<button class="exp-minus" data-table="',table_id,'" data-skill="',skill,'">−</button> ', val, ' <button class="exp-plus" data-table="',table_id,'" data-skill="',skill,'">+</button>') }, skills, exp[skills])
    datatable(tbl, escape = FALSE, options = list(dom='tp', pageLength = 100))
  }
  
  output$std_dt <- renderDT({
    build_dt(std_names, base_std(), bonuses$derived_std, bonuses$exp_std, "std")
  })
  
  prof_skill_list <- reactive({
    pkg_skills <- unique(c(career_packages[[input$career]], bg_packages[[input$background]][bg_show_flags()]))
    sel_skills <- input$prof_select
    derived_skills <- names(bonuses$derived_prof)[bonuses$derived_prof>0]
    exp_skills <- names(bonuses$exp_prof)[bonuses$exp_prof>0]
    full <- unique(c(pkg_skills, sel_skills, derived_skills, exp_skills))
    if(!isTRUE(input$show_pkg_prof)) full <- unique(c(sel_skills, derived_skills, exp_skills))
    sort(full)
  })
  
  output$prof_dt <- renderDT({
    skills <- prof_skill_list()
    build_dt(skills, base_prof(), bonuses$derived_prof, bonuses$exp_prof, "prof")
  })
  
  # ---------- Save / Load --------------------------------------------------
  observeEvent(input$save,{ js$saveChar(list(char=list(
    stats=list(STR=input$STR,CON=input$CON,SIZ=input$SIZ,DEX=input$DEX,INT=input$INT,POW=input$POW,CHA=input$CHA),
    background=input$background, career=input$career, age_cat=input$age_cat,
    prof_select=input$prof_select, age_new_prof=input$age_new_prof,
    sliders=list(bg=input$background, cr=input$career) ))) })
  
  observeEvent(input$load,{ js$loadChar(NULL) })
  observeEvent(input$loadedChar,{ # future work: parse and restore
    showNotification("Load feature to be completed", type="warning")
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
```
