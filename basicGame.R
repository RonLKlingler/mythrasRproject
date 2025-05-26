# Mythras AI RPG Combat Framework – Aligned with Official Rules
# This version incorporates the official character creation rules from the 
# Mythras Character Creation Workbook, including proper skill calculations,
# cultures, careers, and expanded combat mechanics.

# ------------------------------ Dependencies -----------------------------
library(jsonlite)
library(httr)
library(R6)

# ------------------------- 1. Configuration ------------------------------
CONFIG <- list(
  # Combat settings
  DEFAULT_MAX_ROUNDS = 10,
  DEFAULT_MAX_CYCLES_PER_ROUND = 10,
  
  # Skill modifiers
  PRONE_MODIFIER = -40,
  SERIOUS_WOUND_MODIFIER = -20,
  
  # Special effect thresholds
  CRITICAL_DIVISOR = 5,
  FUMBLE_THRESHOLD = 99,
  
  # Character Creation
  SKILL_POINTS_CULTURE = 100,
  SKILL_POINTS_CAREER = 100,
  SKILL_POINTS_BONUS = 150,
  
  # API settings
  API_TIMEOUT = 10,
  API_MAX_RETRIES = 2
)

# ------------------------- 2. Culture Definitions ------------------------
CULTURES <- list(
  barbarian = list(
    name = "Barbarian",
    standard_skills = c("Athletics", "Brawn", "Endurance", "First Aid", 
                        "Locale", "Perception", "Boating/Ride"),
    professional_skills = c("Craft (any)", "Healing", "Lore (any)", "Musicianship", 
                            "Navigate", "Seamanship", "Survival", "Track"),
    combat_styles = c("Barbarian Fyrdman", "Berserker", "Horse Eater", 
                      "Seaborne Reiver", "Weapon Thegn", "Wolf Hunter"),
    passions = list(
      "Loyalty to Clan Chieftain" = "POW+INT",
      "Love (friend/sibling/lover)" = "POW+CHA",
      "Hate (creature/rival/clan)" = "POW*2"
    )
  ),
  civilised = list(
    name = "Civilised",
    standard_skills = c("Conceal", "Deceit", "Drive", "Influence", 
                        "Insight", "Locale", "Willpower"),
    professional_skills = c("Art (any)", "Commerce", "Craft (any)", "Courtesy", 
                            "Language (any)", "Lore (any)", "Musicianship", "Streetwise"),
    combat_styles = c("Citizen Legionary", "City-state Phalangite", "Levied Archer", 
                      "Light Skirmisher", "Street Thug", "Town Militia"),
    passions = list(
      "Loyalty to Town/City" = "POW+INT",
      "Love (friend/sibling/lover)" = "POW+CHA",
      "Hate (rival/gang/district)" = "POW*2"
    )
  ),
  nomadic = list(
    name = "Nomadic",
    standard_skills = c("Endurance", "First Aid", "Locale", "Perception", 
                        "Stealth", "Athletics/Boating/Swim/Drive/Ride"),
    professional_skills = c("Craft (any)", "Culture (any)", "Language (any)", 
                            "Lore (any)", "Musicianship", "Navigate", "Survival", "Track"),
    combat_styles = c("Camel Cavalry", "Feathered Death Flinger", "Horse Lord", 
                      "Whale Hunter", "Wheeled Warrior", "Wolf Runner"),
    passions = list(
      "Loyalty to Tribal Chieftain" = "POW+INT",
      "Love (friend/sibling/lover)" = "POW+CHA",
      "Hate (creature/rival/tribe)" = "POW*2"
    )
  ),
  primitive = list(
    name = "Primitive",
    standard_skills = c("Brawn", "Endurance", "Evade", "Locale", 
                        "Perception", "Stealth", "Athletics/Boating/Swim"),
    professional_skills = c("Craft (any)", "Healing", "Lore (any)", "Musicianship", 
                            "Navigate", "Survival", "Track"),
    combat_styles = c("Flint Death Dealer", "Ghost Warrior", "Head Hunter", 
                      "Jaguar Brother", "Jungle Savage", "Savannah Hunter"),
    passions = list(
      "Loyalty to Chief/Headman" = "POW+INT",
      "Love (friend/sibling/lover)" = "POW+CHA",
      "Hate (something frightening)" = "POW*2"
    )
  )
)

# ------------------------- 3. Career Definitions -------------------------
CAREERS <- list(
  warrior = list(
    name = "Warrior",
    standard_skills = c("Athletics", "Brawn", "Endurance", "Evade", "Unarmed"),
    professional_skills = c("Craft (any)", "Engineering", "Gambling", 
                            "Lore (Military History)", "Lore (Strategy and Tactics)", 
                            "Oratory", "Survival"),
    combat_styles = c("Cultural Style", "Speciality Style")
  ),
  scout = list(
    name = "Scout",
    standard_skills = c("Athletics", "Endurance", "First Aid", "Perception", 
                        "Stealth", "Swim"),
    professional_skills = c("Culture (any)", "Healing", "Language (any)", 
                            "Lore (any)", "Navigation", "Survival", "Track"),
    combat_styles = c("Hunting Style", "Cultural Style")
  ),
  thief = list(
    name = "Thief",
    standard_skills = c("Athletics", "Deceit", "Evade", "Insight", 
                        "Perception", "Stealth"),
    professional_skills = c("Acting", "Commerce", "Disguise", "Lockpicking", 
                            "Mechanisms", "Sleight", "Streetwise"),
    combat_styles = c("Concealable Weapons Style")
  ),
  scholar = list(
    name = "Scholar",
    standard_skills = c("Customs", "Influence", "Insight", "Locale", 
                        "Native Tongue", "Perception", "Willpower"),
    professional_skills = c("Culture (any)", "Language (any)", "Literacy", 
                            "Lore (Primary)", "Lore (Secondary)", "Oratory", "Teach"),
    combat_styles = c()
  ),
  mystic = list(
    name = "Mystic",
    standard_skills = c("Athletics", "Endurance", "Evade", "Insight", 
                        "Perception", "Willpower"),
    professional_skills = c("Art (any)", "Folk Magic", "Literacy", "Lore (any)", 
                            "Meditation", "Musicianship", "Mysticism"),
    combat_styles = c("Cultural Style")
  )
)

# ------------------------- 4. Shared Rule Helpers ------------------------

# Action Points - Official table from p.57
ap_from_stats <- function(DEX, INT) {
  s <- DEX + INT
  if (s <= 12)      return(1)
  else if (s <= 24) return(2)
  else if (s <= 36) return(3)
  else              return(3 + floor((s - 25) / 12))
}

# Damage Modifier - Official table
damage_modifier <- function(STR, SIZ) {
  total <- STR + SIZ
  if (total <= 5)        return("-1d8")
  else if (total <= 10)  return("-1d6")
  else if (total <= 15)  return("-1d4")
  else if (total <= 20)  return("-1d2")
  else if (total <= 25)  return("0")
  else if (total <= 30)  return("+1d2")
  else if (total <= 35)  return("+1d4")
  else if (total <= 40)  return("+1d6")
  else if (total <= 45)  return("+1d8")
  else if (total <= 50)  return("+1d10")
  else if (total <= 60)  return("+2d6")
  else if (total <= 70)  return("+3d6")
  else if (total <= 80)  return("+4d6")
  else                   return("+5d6")
}

# Experience Modifier
experience_modifier <- function(CHA) {
  if (CHA <= 6)       return(-1)
  else if (CHA <= 12) return(0)
  else if (CHA <= 18) return(1)
  else                return(1 + floor((CHA - 13) / 6))
}

# Healing Rate
healing_rate <- function(CON) {
  if (CON <= 6)       return(1)
  else if (CON <= 12) return(2)
  else if (CON <= 18) return(3)
  else                return(3 + floor((CON - 13) / 6))
}

# Luck Points
luck_points <- function(POW) {
  if (POW <= 6)       return(1)
  else if (POW <= 12) return(2)
  else if (POW <= 18) return(3)
  else                return(3 + floor((POW - 13) / 6))
}

# Hit Points per Location - Official table
hit_points_per_location <- function(CON, SIZ) {
  total <- CON + SIZ
  
  # Define HP values for each bracket
  if (total <= 5) {
    hp <- list(head=1, chest=3, abdomen=2, arm=1, leg=1)
  } else if (total <= 10) {
    hp <- list(head=2, chest=4, abdomen=3, arm=1, leg=2)
  } else if (total <= 15) {
    hp <- list(head=3, chest=5, abdomen=4, arm=2, leg=3)
  } else if (total <= 20) {
    hp <- list(head=4, chest=6, abdomen=5, arm=3, leg=4)
  } else if (total <= 25) {
    hp <- list(head=5, chest=7, abdomen=6, arm=4, leg=5)
  } else if (total <= 30) {
    hp <- list(head=6, chest=8, abdomen=7, arm=5, leg=6)
  } else if (total <= 35) {
    hp <- list(head=7, chest=9, abdomen=8, arm=6, leg=7)
  } else {
    hp <- list(head=8, chest=10, abdomen=9, arm=7, leg=8)
  }
  
  list(
    head = hp$head,
    chest = hp$chest,
    abdomen = hp$abdomen,
    left_arm = hp$arm,
    right_arm = hp$arm,
    left_leg = hp$leg,
    right_leg = hp$leg
  )
}

# Enhanced dice roller
roll_dice <- function(expr, verbose = FALSE) {
  if (is.null(expr) || is.na(expr) || !nzchar(expr) || expr == "0") {
    return(0L)
  }
  
  # Clean expression
  expr <- gsub("[\u2013\u2014]", "-", expr, perl = TRUE)
  expr <- gsub("\\s", "", expr)
  
  # Split on + or - while keeping the sign
  parts <- strsplit(expr, "(?=[+-])", perl = TRUE)[[1]]
  total <- 0L
  
  tryCatch({
    for (p in parts) {
      sign <- 1L
      if (startsWith(p, "+")) {
        sign <- 1L
        p <- substring(p, 2)
      } else if (startsWith(p, "-")) {
        sign <- -1L
        p <- substring(p, 2)
      }
      
      if (grepl("d", p, fixed = TRUE)) {
        nd <- strsplit(p, "d", fixed = TRUE)[[1]]
        n <- ifelse(nzchar(nd[1]), as.integer(nd[1]), 1L)
        d <- as.integer(nd[2])
        
        if (is.na(n) || is.na(d) || n < 1 || d < 1) next
        
        roll_result <- sum(sample.int(d, n, replace = TRUE))
        total <- total + sign * roll_result
        
        if (verbose) {
          cat(sprintf("%s%dd%d = %d\n", 
                      ifelse(sign > 0, "+", "-"), n, d, roll_result))
        }
      } else {
        val <- suppressWarnings(as.integer(p))
        if (!is.na(val)) {
          total <- total + sign * val
        }
      }
    }
  }, error = function(e) {
    if (verbose) warning(sprintf("Error parsing dice: %s", e$message))
    return(0L)
  })
  
  total
}

# Skill base calculation
calculate_skill_base <- function(characteristics, skill_name) {
  # Standard skills with their base characteristics
  skill_bases <- list(
    "Athletics" = "STR+DEX",
    "Boating" = "STR+CON",
    "Brawn" = "STR+SIZ",
    "Conceal" = "DEX+POW",
    "Customs" = "INT*2",
    "Dance" = "DEX+CHA",
    "Deceit" = "INT+CHA",
    "Drive" = "DEX+POW",
    "Endurance" = "CON*2",
    "Evade" = "DEX*2",
    "First Aid" = "INT+DEX",
    "Influence" = "CHA*2",
    "Insight" = "INT+POW",
    "Locale" = "INT*2",
    "Perception" = "INT+POW",
    "Ride" = "DEX+POW",
    "Sing" = "CHA+POW",
    "Stealth" = "DEX+INT",
    "Swim" = "STR+CON",
    "Unarmed" = "STR+DEX",
    "Willpower" = "POW*2",
    # Professional skills
    "Acting" = "CHA*2",
    "Acrobatics" = "STR+DEX",
    "Art" = "CHA+POW",
    "Combat Style" = "STR+DEX",
    "Commerce" = "INT+CHA",
    "Courtesy" = "INT+CHA",
    "Craft" = "DEX+INT",
    "Culture" = "INT*2",
    "Disguise" = "INT+CHA",
    "Engineering" = "INT*2",
    "Gambling" = "INT+POW",
    "Healing" = "INT+POW",
    "Language" = "INT+CHA",
    "Literacy" = "INT*2",
    "Lockpicking" = "DEX*2",
    "Lore" = "INT*2",
    "Mechanisms" = "DEX+INT",
    "Musicianship" = "DEX+CHA",
    "Navigation" = "INT+POW",
    "Oratory" = "POW+CHA",
    "Seamanship" = "INT+CON",
    "Seduction" = "INT+CHA",
    "Sleight" = "DEX+CHA",
    "Streetwise" = "POW+CHA",
    "Survival" = "CON+POW",
    "Teach" = "INT+CHA",
    "Track" = "INT+CON",
    # Magic skills
    "Folk Magic" = "CHA+POW",
    "Meditation" = "INT+CON",
    "Mysticism" = "POW+CON"
  )
  
  # Get base formula
  base_formula <- skill_bases[[skill_name]]
  if (is.null(base_formula)) return(0)
  
  # Parse formula
  if (grepl("\\*", base_formula)) {
    parts <- strsplit(base_formula, "\\*")[[1]]
    return(characteristics[[parts[1]]] * as.numeric(parts[2]))
  } else if (grepl("\\+", base_formula)) {
    parts <- strsplit(base_formula, "\\+")[[1]]
    return(characteristics[[parts[1]]] + characteristics[[parts[2]]])
  } else {
    return(characteristics[[base_formula]])
  }
}

# Expanded weapon profiles
WEAPON_DATA <- list(
  # Melee weapons
  Longsword = list(dice = "1d8", size = "M", reach = "L", ap = 2, 
                   traits = c("Bleed", "Impale"), 
                   effects = c("Bleed", "Impale", "Sunder")),
  Shortsword = list(dice = "1d6", size = "M", reach = "S", ap = 1, 
                    traits = c("Bleed", "Impale"),
                    effects = c("Bleed", "Impale")),
  Greatsword = list(dice = "2d8", size = "H", reach = "L", ap = 3,
                    traits = c("Bleed", "Impale", "Sunder"),
                    effects = c("Bleed", "Impale", "Sunder", "Damage Weapon")),
  Dagger = list(dice = "1d4+1", size = "S", reach = "S", ap = 1,
                traits = c("Bleed", "Impale"),
                effects = c("Bleed", "Impale", "Bypass Armor")),
  Staff = list(dice = "1d8", size = "M", reach = "L", ap = 1,
               traits = c("Defensive"),
               effects = c("Stun Location", "Pin Weapon")),
  Spear = list(dice = "1d8+1", size = "M", reach = "L", ap = 1,
               traits = c("Impale"),
               effects = c("Impale", "Pin Weapon")),
  # Ranged weapons
  Bow = list(dice = "1d8", size = "L", reach = "Range", ap = 1,
             traits = c("Impale", "2H"),
             effects = c("Impale", "Choose Location")),
  Crossbow = list(dice = "1d10", size = "L", reach = "Range", ap = 2,
                  traits = c("Impale", "Reload 3"),
                  effects = c("Impale", "Bypass Armor"))
)

# ---------------------- 5. Claude / AI narration API ---------------------
API_CONFIG <- list(
  endpoint = "https://api.anthropic.com/v1/messages",
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  model = "claude-3-haiku-20240307",
  version = "2023-06-01"
)

call_ai_api <- function(prompt, role = "You are a Mythras RPG combat narrator. Describe combat actions vividly but concisely.", max_tokens = 150) {
  if (!nzchar(API_CONFIG$api_key)) {
    return(generate_fallback_narration(prompt))
  }
  
  body <- list(
    model = API_CONFIG$model,
    max_tokens = max_tokens,
    temperature = 0.7,
    system = role,
    messages = list(list(role = "user", content = prompt))
  )
  
  tryCatch({
    resp <- POST(
      API_CONFIG$endpoint,
      add_headers(
        "x-api-key" = API_CONFIG$api_key,
        "anthropic-version" = API_CONFIG$version,
        "content-type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      timeout(CONFIG$API_TIMEOUT)
    )
    
    if (status_code(resp) == 200) {
      content(resp, "parsed")$content[[1]]$text
    } else {
      generate_fallback_narration(prompt)
    }
  }, error = function(e) {
    generate_fallback_narration(prompt)
  })
}

generate_fallback_narration <- function(prompt) {
  if (grepl("attacks", prompt, ignore.case = TRUE)) {
    return("The combatants clash in a flurry of steel!")
  } else if (grepl("defends", prompt, ignore.case = TRUE)) {
    return("A desperate parry deflects the blow!")
  } else {
    return("The battle continues...")
  }
}

# -------------------------- 6. Combat Classes ----------------------------

# Combat State Class
CombatState <- R6Class("CombatState",
                       public = list(
                         round = 1,
                         conditions = list(),
                         combat_log = character(),
                         narrative_log = character(),
                         
                         initialize = function() {
                           self$round <- 1
                           self$conditions <- list()
                           self$combat_log <- character()
                           self$narrative_log <- character()
                         },
                         
                         add_condition = function(who, cond) {
                           if (!(who %in% names(self$conditions))) {
                             self$conditions[[who]] <- character()
                           }
                           self$conditions[[who]] <- unique(c(self$conditions[[who]], cond))
                         },
                         
                         remove_condition = function(who, cond) {
                           if (who %in% names(self$conditions)) {
                             self$conditions[[who]] <- setdiff(self$conditions[[who]], cond)
                           }
                         },
                         
                         get_conditions = function(who) {
                           if (who %in% names(self$conditions)) {
                             self$conditions[[who]]
                           } else {
                             character(0)
                           }
                         },
                         
                         log = function(message, narrative = FALSE) {
                           entry <- paste0("[R", self$round, "] ", message)
                           self$combat_log <- c(self$combat_log, entry)
                           if (narrative) {
                             self$narrative_log <- c(self$narrative_log, entry)
                           }
                           cat(entry, "\n")
                         },
                         
                         summary = function() {
                           cat("\n=== COMBAT SUMMARY ===\n")
                           cat(sprintf("Total Rounds: %d\n", self$round))
                           cat(sprintf("Total Actions: %d\n", length(self$combat_log)))
                           cat("\n")
                         }
                       )
)

# Enhanced Character Class with proper Mythras rules
Character <- R6Class("Character",
                     public = list(
                       # Basic info
                       name = NULL,
                       culture = NULL,
                       career = NULL,
                       
                       # Characteristics
                       attributes = list(),
                       
                       # Derived attributes
                       action_points = 0,
                       damage_modifier = "0",
                       experience_modifier = 0,
                       healing_rate = 0,
                       initiative_bonus = 0,
                       luck_points = 0,
                       magic_points = 0,
                       movement_rate = 6,
                       
                       # Hit locations
                       hit_locations = list(),
                       
                       # Skills
                       skills = list(),
                       combat_styles = list(),
                       passions = list(),
                       
                       # Combat state
                       current_ap = 0,
                       fatigue_level = "Fresh",
                       conditions = character(),
                       initiative = 0,  # Add this field
                       
                       # Equipment
                       weapon = NULL,
                       armor = list(),
                       
                       # Personality
                       personality = NULL,
                       
                       initialize = function(name, culture = "civilised", career = "warrior", 
                                             custom_stats = NULL, random_stats = TRUE) {
                         self$name <- name
                         self$culture <- culture
                         self$career <- career
                         
                         # Generate or use provided characteristics
                         if (!is.null(custom_stats)) {
                           self$attributes <- custom_stats
                         } else if (random_stats) {
                           # Roll 3d6 for most stats, 2d6+6 for INT and SIZ
                           self$attributes <- list(
                             STR = sum(sample(1:6, 3, replace = TRUE)),
                             CON = sum(sample(1:6, 3, replace = TRUE)),
                             SIZ = sum(sample(1:6, 2, replace = TRUE)) + 6,
                             DEX = sum(sample(1:6, 3, replace = TRUE)),
                             INT = sum(sample(1:6, 2, replace = TRUE)) + 6,
                             POW = sum(sample(1:6, 3, replace = TRUE)),
                             CHA = sum(sample(1:6, 3, replace = TRUE))
                           )
                         } else {
                           # Standard array
                           self$attributes <- list(STR=13, CON=13, SIZ=13, DEX=13, INT=13, POW=13, CHA=13)
                         }
                         
                         # Calculate derived attributes
                         self$action_points <- ap_from_stats(self$attributes$DEX, self$attributes$INT)
                         self$damage_modifier <- damage_modifier(self$attributes$STR, self$attributes$SIZ)
                         self$experience_modifier <- experience_modifier(self$attributes$CHA)
                         self$healing_rate <- healing_rate(self$attributes$CON)
                         self$initiative_bonus <- ceiling((self$attributes$DEX + self$attributes$INT) / 2)
                         self$luck_points <- luck_points(self$attributes$POW)
                         self$magic_points <- self$attributes$POW
                         
                         # Set hit locations
                         self$hit_locations <- hit_points_per_location(self$attributes$CON, self$attributes$SIZ)
                         
                         # Initialize skills
                         self$initialize_skills()
                         
                         # Set equipment based on career
                         self$set_default_equipment()
                         
                         # Set personality
                         self$set_personality()
                         
                         # Combat readiness
                         self$current_ap <- self$action_points
                       },
                       
                       initialize_skills = function() {
                         # Get culture and career data
                         culture_data <- CULTURES[[self$culture]]
                         career_data <- CAREERS[[self$career]]
                         
                         # Initialize all standard skills with base values
                         standard_skills <- c("Athletics", "Boating", "Brawn", "Conceal", "Customs",
                                              "Dance", "Deceit", "Drive", "Endurance", "Evade", 
                                              "First Aid", "Influence", "Insight", "Locale", "Perception",
                                              "Ride", "Sing", "Stealth", "Swim", "Unarmed", "Willpower")
                         
                         for (skill in standard_skills) {
                           base <- calculate_skill_base(self$attributes, skill)
                           self$skills[[skill]] <- base
                         }
                         
                         # Native Tongue and Customs get +40
                         self$skills[["Native Tongue"]] <- calculate_skill_base(self$attributes, "Language") + 40
                         self$skills[["Customs"]] <- self$skills[["Customs"]] + 40
                         
                         # Apply culture skill bonuses (simplified - in full system players distribute points)
                         culture_skills <- culture_data$standard_skills
                         for (skill in culture_skills) {
                           # Handle choices like "Boating/Ride"
                           if (grepl("/", skill)) {
                             skill <- strsplit(skill, "/")[[1]][1]  # Pick first option
                           }
                           if (skill %in% names(self$skills)) {
                             self$skills[[skill]] <- self$skills[[skill]] + sample(5:15, 1)
                           }
                         }
                         
                         # Apply career skill bonuses
                         if (!is.null(career_data)) {
                           for (skill in career_data$standard_skills) {
                             if (skill %in% names(self$skills)) {
                               self$skills[[skill]] <- self$skills[[skill]] + sample(5:15, 1)
                             }
                           }
                         }
                         
                         # Add combat style if available
                         if (length(career_data$combat_styles) > 0 || length(culture_data$combat_styles) > 0) {
                           style_name <- if (length(career_data$combat_styles) > 0) {
                             sample(career_data$combat_styles, 1)
                           } else {
                             sample(culture_data$combat_styles, 1)
                           }
                           
                           base <- calculate_skill_base(self$attributes, "Combat Style")
                           self$combat_styles[[style_name]] <- base + sample(40:60, 1)
                         }
                         
                         # Add a passion
                         passion_data <- culture_data$passions
                         if (length(passion_data) > 0) {
                           passion_name <- sample(names(passion_data), 1)
                           formula <- passion_data[[passion_name]]
                           
                           # Calculate passion value
                           if (formula == "POW+INT") {
                             value <- 30 + self$attributes$POW + self$attributes$INT
                           } else if (formula == "POW+CHA") {
                             value <- 30 + self$attributes$POW + self$attributes$CHA
                           } else if (formula == "POW*2") {
                             value <- 30 + (self$attributes$POW * 2)
                           } else {
                             value <- 50  # Default
                           }
                           
                           self$passions[[passion_name]] <- value
                         }
                       },
                       
                       set_default_equipment = function() {
                         # Weapon based on career/culture
                         weapon_options <- switch(self$career,
                                                  warrior = c("Longsword", "Greatsword", "Spear"),
                                                  scout = c("Spear", "Bow", "Shortsword"),
                                                  thief = c("Dagger", "Shortsword"),
                                                  scholar = c("Staff", "Dagger"),
                                                  mystic = c("Staff", "Dagger"),
                                                  c("Shortsword")  # Default
                         )
                         self$weapon <- sample(weapon_options, 1)
                         
                         # Armor
                         self$armor <- switch(self$career,
                                              warrior = list(head = 5, chest = 5, abdomen = 5, arms = 5, legs = 5),
                                              scout = list(head = 0, chest = 2, abdomen = 2, arms = 0, legs = 0),
                                              thief = list(head = 0, chest = 1, abdomen = 1, arms = 0, legs = 0),
                                              list(head = 0, chest = 0, abdomen = 0, arms = 0, legs = 0)  # Default
                         )
                       },
                       
                       set_personality = function() {
                         personalities <- switch(self$career,
                                                 warrior = c("Glory-seeker", "Defensive fighter", "Brutal warrior", "Honor-bound"),
                                                 scout = c("Cautious observer", "Swift striker", "Patient hunter", "Wilderness expert"),
                                                 thief = c("Opportunist", "Shadow dancer", "Cunning deceiver", "Quick fingers"),
                                                 scholar = c("Analytical mind", "Cautious scholar", "Wise counselor", "Knowledge seeker"),
                                                 mystic = c("Serene warrior", "Spiritual guide", "Mystic wanderer", "Inner strength"),
                                                 c("Determined fighter")
                         )
                         self$personality <- sample(personalities, 1)
                       },
                       
                       # Combat methods
                       reset_ap = function() {
                         self$current_ap <- self$action_points
                       },
                       
                       spend_ap = function(n = 1) {
                         self$current_ap <- max(0, self$current_ap - n)
                       },
                       
                       # Get best combat skill
                       get_combat_skill = function() {
                         combat_skill <- 0
                         skill_name <- "Unarmed"
                         
                         # Check combat styles
                         if (length(self$combat_styles) > 0) {
                           best_style <- which.max(self$combat_styles)
                           if (length(best_style) > 0) {
                             combat_skill <- self$combat_styles[[best_style]]
                             skill_name <- names(self$combat_styles)[best_style]
                           }
                         }
                         
                         # Compare with unarmed
                         if (self$skills[["Unarmed"]] > combat_skill) {
                           combat_skill <- self$skills[["Unarmed"]]
                           skill_name <- "Unarmed"
                         }
                         
                         list(value = combat_skill, name = skill_name)
                       },
                       
                       roll_weapon_damage = function(maximize = FALSE) {
                         weapon_info <- WEAPON_DATA[[self$weapon]]
                         
                         if (is.null(weapon_info)) {
                           # Unarmed damage
                           dice_expr <- "1d3"
                         } else {
                           dice_expr <- weapon_info$dice
                         }
                         
                         # Add damage modifier
                         if (self$damage_modifier != "0") {
                           dice_expr <- paste0(dice_expr, self$damage_modifier)
                         }
                         
                         if (maximize) {
                           # Parse and maximize damage
                           max_dmg <- gsub("(\\d+)d(\\d+)", "\\1*\\2", dice_expr)
                           eval(parse(text = max_dmg))
                         } else {
                           roll_dice(dice_expr)
                         }
                       },
                       
                       take_damage = function(location, damage) {
                         if (is.na(damage) || damage < 0) damage <- 0L
                         
                         # Apply armor
                         armor_value <- 0
                         if (location == "head" && !is.null(self$armor$head)) armor_value <- self$armor$head
                         else if (location == "chest" && !is.null(self$armor$chest)) armor_value <- self$armor$chest
                         else if (location == "abdomen" && !is.null(self$armor$abdomen)) armor_value <- self$armor$abdomen
                         else if (grepl("arm", location) && !is.null(self$armor$arms)) armor_value <- self$armor$arms
                         else if (grepl("leg", location) && !is.null(self$armor$legs)) armor_value <- self$armor$legs
                         
                         actual_damage <- max(0, damage - armor_value)
                         
                         # Apply damage to location
                         if (!is.null(self$hit_locations[[location]])) {
                           self$hit_locations[[location]] <- max(0, self$hit_locations[[location]] - actual_damage)
                           
                           # Check for serious wounds
                           max_hp <- hit_points_per_location(self$attributes$CON, self$attributes$SIZ)[[location]]
                           if (self$hit_locations[[location]] <= 0) {
                             self$conditions <- c(self$conditions, paste0(location, "_disabled"))
                           } else if (actual_damage > max_hp / 2) {
                             self$conditions <- c(self$conditions, "serious_wound")
                           }
                         }
                         
                         actual_damage
                       },
                       
                       get_total_hp = function() {
                         sum(unlist(self$hit_locations))
                       },
                       
                       get_max_hp = function() {
                         max_hp <- hit_points_per_location(self$attributes$CON, self$attributes$SIZ)
                         sum(unlist(max_hp))
                       },
                       
                       is_alive = function() {
                         # Character is alive if head or chest have HP > 0
                         self$hit_locations$head > 0 && self$hit_locations$chest > 0
                       },
                       
                       is_incapacitated = function() {
                         # Check if too many locations are disabled
                         disabled_count <- sum(sapply(self$hit_locations, function(x) x <= 0))
                         disabled_count >= 3 || !self$is_alive()
                       },
                       
                       can_act = function() {
                         self$is_alive() && !self$is_incapacitated() && self$current_ap > 0
                       },
                       
                       get_status = function() {
                         status <- sprintf("%s: HP %d/%d, AP %d/%d", 
                                           self$name, self$get_total_hp(), self$get_max_hp(), 
                                           self$current_ap, self$action_points)
                         
                         if (length(self$conditions) > 0) {
                           status <- paste0(status, " [", paste(self$conditions, collapse=", "), "]")
                         }
                         
                         # Add location status if wounded
                         wounded_locations <- character()
                         for (loc in names(self$hit_locations)) {
                           if (self$hit_locations[[loc]] <= 0) {
                             wounded_locations <- c(wounded_locations, paste0(loc, ":DISABLED"))
                           }
                         }
                         
                         if (length(wounded_locations) > 0) {
                           status <- paste0(status, " {", paste(wounded_locations, collapse=", "), "}")
                         }
                         
                         status
                       }
                     )
)

# ----------------------- 7. Skill check utilities ------------------------
perform_skill_check <- function(skill, mod = 0, verbose = FALSE) {
  eff <- min(100, max(1, skill + mod))
  roll <- sample.int(100, 1)
  
  # Calculate critical and fumble thresholds
  crit <- ceiling(eff / CONFIG$CRITICAL_DIVISOR)
  fum <- if (eff >= 100) 100 else CONFIG$FUMBLE_THRESHOLD
  
  # Determine result
  if (roll == 100 || (roll >= fum && eff < 100)) {
    result <- "Fumble"
  } else if (roll <= crit) {
    result <- "Critical"
  } else if (roll <= eff) {
    result <- "Success"
  } else {
    result <- "Failure"
  }
  
  # Success effects
  se <- switch(result,
               Critical = 2L,
               Success = 1L,
               0L
  )
  
  margin <- eff - roll
  
  if (verbose) {
    cat(sprintf("Skill check: %d%% (roll: %d) = %s\n", eff, roll, result))
  }
  
  list(
    result = result,
    roll = roll,
    skill = eff,
    se = se,
    margin = margin,
    critical_range = crit,
    fumble_range = fum
  )
}

# ----------------------- 8. Combat Resolution ----------------------------
# Hit location table
roll_hit_location <- function() {
  roll <- sample.int(20, 1)
  if (roll >= 19) return("head")
  else if (roll >= 16) return("left_arm")
  else if (roll >= 13) return("right_arm")
  else if (roll >= 10) return("chest")
  else if (roll >= 7) return("abdomen")
  else if (roll >= 4) return("left_leg")
  else return("right_leg")
}

# Enhanced special effects
SPECIAL_EFFECTS <- list(
  offensive = list(
    "Arise" = list(cost = 1, desc = "May use Evade to stand up automatically"),
    "Bash" = list(cost = 1, desc = "Opponent knocked back"),
    "Bleed" = list(cost = 1, desc = "Inflict ongoing damage", weapons = c("edged")),
    "Bypass Armor" = list(cost = 1, desc = "Ignore armor on location"),
    "Choose Location" = list(cost = 1, desc = "Select hit location"),
    "Disarm Opponent" = list(cost = 1, desc = "Force opponent to drop weapon"),
    "Damage Weapon" = list(cost = 1, desc = "Damage opponent's weapon/shield"),
    "Entangle" = list(cost = 1, desc = "Entangle opponent", weapons = c("flexible")),
    "Force Failure" = list(cost = 1, desc = "Turn opponent's success into failure"),
    "Grip" = list(cost = 1, desc = "Grab opponent", weapons = c("unarmed")),
    "Impale" = list(cost = 1, desc = "Double damage, weapon stuck", weapons = c("impaling")),
    "Stun Location" = list(cost = 1, desc = "Stun body part", weapons = c("bludgeon")),
    "Sunder" = list(cost = 1, desc = "Damage armor/weapon", weapons = c("two-handed")),
    "Trip Opponent" = list(cost = 1, desc = "Knock opponent prone")
  ),
  defensive = list(
    "Blind Opponent" = list(cost = 1, desc = "Throw dirt/sand in eyes"),
    "Change Range" = list(cost = 1, desc = "Adjust engagement distance"),
    "Enhance Parry" = list(cost = 1, desc = "Reduce size penalty for parrying"),
    "Outmaneuver" = list(cost = 1, desc = "Opponent cannot attack next turn"),
    "Pin Weapon" = list(cost = 1, desc = "Trap opponent's weapon"),
    "Regain Footing" = list(cost = 1, desc = "Stand up from prone"),
    "Withdraw" = list(cost = 1, desc = "Safely disengage from melee")
  )
)

select_special_effect <- function(character, effects_available, opponent = NULL, offensive = TRUE) {
  if (effects_available <= 0) return(character())
  
  weapon_info <- WEAPON_DATA[[character$weapon]]
  
  # Get weapon traits
  weapon_traits <- if (!is.null(weapon_info)) weapon_info$traits else character()
  
  # Build list of valid effects
  valid_effects <- character()
  effect_list <- if (offensive) SPECIAL_EFFECTS$offensive else SPECIAL_EFFECTS$defensive
  
  for (effect_name in names(effect_list)) {
    effect <- effect_list[[effect_name]]
    
    # Check weapon requirements
    if (!is.null(effect$weapons)) {
      weapon_ok <- FALSE
      for (req in effect$weapons) {
        if (req == "edged" && any(c("Bleed", "Impale") %in% weapon_traits)) weapon_ok <- TRUE
        if (req == "impaling" && "Impale" %in% weapon_traits) weapon_ok <- TRUE
        if (req == "flexible" && character$weapon == "Whip") weapon_ok <- TRUE
        if (req == "unarmed" && is.null(weapon_info)) weapon_ok <- TRUE
        if (req == "two-handed" && "2H" %in% weapon_traits) weapon_ok <- TRUE
        if (req == "bludgeon" && character$weapon %in% c("Staff", "Club", "Mace")) weapon_ok <- TRUE
      }
      if (!weapon_ok) next
    } else {
      valid_effects <- c(valid_effects, effect_name)
    }
  }
  
  # Personality-based selection
  if (offensive) {
    if (grepl("brutal", tolower(character$personality))) {
      if ("Impale" %in% valid_effects) return("Impale")
      if ("Bleed" %in% valid_effects) return("Bleed")
      if ("Choose Location" %in% valid_effects) return("Choose Location")
    }
    
    if (grepl("tactical|cunning", tolower(character$personality))) {
      if ("Disarm Opponent" %in% valid_effects && !is.null(opponent) && opponent$current_ap <= 1) {
        return("Disarm Opponent")
      }
      if ("Trip Opponent" %in% valid_effects) return("Trip Opponent")
      if ("Force Failure" %in% valid_effects) return("Force Failure")
    }
    
    if (grepl("defensive", tolower(character$personality))) {
      if ("Bash" %in% valid_effects) return("Bash")
      if ("Damage Weapon" %in% valid_effects) return("Damage Weapon")
    }
  } else {
    # Defensive effects
    if ("prone" %in% character$conditions && "Regain Footing" %in% valid_effects) {
      return("Regain Footing")
    }
    if ("Outmaneuver" %in% valid_effects) return("Outmaneuver")
    if ("Withdraw" %in% valid_effects && character$get_total_hp() < character$get_max_hp() / 2) {
      return("Withdraw")
    }
  }
  
  # Default to first valid effect
  if (length(valid_effects) > 0) valid_effects[1] else character()
}

resolve_combat <- function(att, def, state) {
  # Validate inputs
  if (!att$can_act()) {
    return(list(type = "no-ap", message = sprintf("%s cannot act", att$name)))
  }
  
  # Calculate modifiers
  att_mod <- 0
  def_mod <- 0
  
  # Condition modifiers
  conditions_att <- c(att$conditions, state$get_conditions(att$name))
  conditions_def <- c(def$conditions, state$get_conditions(def$name))
  
  if ("prone" %in% conditions_att) att_mod <- att_mod + CONFIG$PRONE_MODIFIER
  if ("serious_wound" %in% conditions_att) att_mod <- att_mod + CONFIG$SERIOUS_WOUND_MODIFIER
  
  if ("prone" %in% conditions_def) def_mod <- def_mod + CONFIG$PRONE_MODIFIER
  if ("serious_wound" %in% conditions_def) def_mod <- def_mod + CONFIG$SERIOUS_WOUND_MODIFIER
  
  # Perform rolls
  att_skill <- att$get_combat_skill()
  att_roll <- perform_skill_check(att_skill$value, att_mod)
  
  # Defender chooses defense type
  def_skill_name <- if (grepl("evade|acrobatic", tolower(def$personality))) {
    "Evade"
  } else {
    "Parry"
  }
  
  def_skill_value <- if (def_skill_name == "Evade") {
    def$skills[["Evade"]]
  } else {
    def$get_combat_skill()$value
  }
  
  def_roll <- if (def$can_act()) {
    perform_skill_check(def_skill_value, def_mod)
  } else {
    list(result = "Failure", roll = 100, skill = 0, se = 0, margin = -Inf)
  }
  
  # Spend AP
  att$spend_ap(1)
  if (def$can_act() && def_roll$result != "Failure") {
    def$spend_ap(1)
  }
  
  # Determine outcome
  outcome <- determine_outcome(att_roll, def_roll)
  
  # Apply results
  dmg <- 0
  actual_dmg <- 0
  location <- "chest"
  effects <- character()
  
  if (outcome$winner == "att") {
    # Select special effects
    if (outcome$se > 0) {
      effects <- character()
      for (i in 1:outcome$se) {
        effect <- select_special_effect(att, 1, def, offensive = TRUE)
        if (length(effect) > 0) effects <- c(effects, effect)
      }
    }
    
    # Determine hit location
    if ("Choose Location" %in% effects) {
      # Attacker chooses location - go for head or chest
      location <- if (def$hit_locations$head > 0) "head" else "chest"
    } else {
      location <- roll_hit_location()
    }
    
    # Calculate damage
    dmg <- att$roll_weapon_damage()
    
    # Apply effect modifiers
    if ("Impale" %in% effects) {
      dmg <- dmg * 2
      state$add_condition(att$name, "weapon_stuck")
    }
    
    if ("Bypass Armor" %in% effects) {
      # Temporarily set armor to 0 for this location
      old_armor <- def$armor[[location]]
      def$armor[[location]] <- 0
      actual_dmg <- def$take_damage(location, dmg)
      def$armor[[location]] <- old_armor
    } else {
      actual_dmg <- def$take_damage(location, dmg)
    }
    
    # Apply conditions from effects
    if ("Trip Opponent" %in% effects) {
      state$add_condition(def$name, "prone")
    }
    
    if ("Bleed" %in% effects) {
      state$add_condition(def$name, "bleeding")
    }
    
    if ("Bash" %in% effects) {
      state$add_condition(def$name, "off_balance")
    }
    
    if ("Stun Location" %in% effects) {
      state$add_condition(def$name, paste0(location, "_stunned"))
    }
    
  } else if (outcome$winner == "def" && outcome$se > 0) {
    # Defender gets special effects
    effects <- character()
    for (i in 1:outcome$se) {
      effect <- select_special_effect(def, 1, att, offensive = FALSE)
      if (length(effect) > 0) effects <- c(effects, effect)
    }
    
    if ("Outmaneuver" %in% effects) {
      state$add_condition(att$name, "outmaneuvered")
    }
    
    if ("Pin Weapon" %in% effects) {
      state$add_condition(att$name, "weapon_pinned")
    }
  }
  
  list(
    winner = outcome$winner,
    att_res = att_roll,
    att_skill = att_skill$name,
    def_res = def_roll,
    def_skill = def_skill_name,
    effects = effects,
    location = location,
    dmg = dmg,
    actual_dmg = actual_dmg
  )
}

determine_outcome <- function(att, def) {
  # Rank results
  rank <- function(r) {
    switch(r,
           Fumble = 0,
           Failure = 1,
           Success = 2,
           Critical = 3,
           0
    )
  }
  
  att_rank <- rank(att$result)
  def_rank <- rank(def$result)
  
  if (att_rank > def_rank) {
    list(winner = "att", se = att$se)
  } else if (def_rank > att_rank) {
    list(winner = "def", se = def$se)
  } else if (att$margin > def$margin) {
    list(winner = "att", se = max(0, att$se - 1))
  } else if (def$margin > att$margin) {
    list(winner = "def", se = max(0, def$se - 1))
  } else {
    list(winner = "draw", se = 0)
  }
}

# --------------------- 9. Encounter Orchestrator -------------------------
run_combat <- function(participants = NULL, max_rounds = CONFIG$DEFAULT_MAX_ROUNDS, 
                       use_ai = TRUE, verbose = TRUE) {
  cat("=== MYTHRAS COMBAT SIMULATOR (Official Rules) ===\n\n")
  
  if (!nzchar(API_CONFIG$api_key) && use_ai) {
    cat("*No API key set - using fallback narration.*\n")
    cat("*Set ANTHROPIC_API_KEY environment variable for AI narration.*\n\n")
  }
  
  # Initialize combat state
  cs <- CombatState$new()
  
  # Create participants if not provided
  if (is.null(participants)) {
    participants <- list(
      Character$new("Marcus the Bold", "civilised", "warrior"),
      Character$new("Swift Shadow", "nomadic", "scout"),
      Character$new("Gruk Ironhand", "barbarian", "warrior"),
      Character$new("Whisper", "civilised", "thief")
    )
  }
  
  # Determine teams (simple: odd vs even indices)
  teams <- list(
    A = participants[seq(1, length(participants), 2)],
    B = participants[seq(2, length(participants), 2)]
  )
  
  # Roll initiative
  for (f in participants) {
    f$initiative <- sample(1:10, 1) + f$initiative_bonus
  }
  
  # Sort by initiative
  all_fighters <- unlist(teams, recursive = FALSE)
  all_fighters <- all_fighters[order(sapply(all_fighters, function(x) x$initiative), decreasing = TRUE)]
  
  # Display initial status
  cat("=== COMBATANTS ===\n")
  for (f in all_fighters) {
    cat(sprintf("- %s (%s %s)\n  %s\n", 
                f$name, f$culture, f$career, f$get_status()))
  }
  cat("\n")
  
  # Combat rounds
  for (round in 1:max_rounds) {
    cs$round <- round
    cat(sprintf("\n===== ROUND %d =====\n", round))
    
    # Reset AP for all fighters
    for (f in all_fighters) {
      f$reset_ap()
      
      # Handle conditions at start of round
      if ("bleeding" %in% c(f$conditions, cs$get_conditions(f$name))) {
        bleed_dmg <- sample(1:2, 1)
        location <- roll_hit_location()
        f$take_damage(location, bleed_dmg)
        cs$log(sprintf("%s bleeds for %d damage to %s", f$name, bleed_dmg, location))
      }
    }
    
    # Check for end conditions
    alive_teams <- character()
    for (team_name in names(teams)) {
      if (any(sapply(teams[[team_name]], function(f) f$is_alive()))) {
        alive_teams <- c(alive_teams, team_name)
      }
    }
    
    if (length(alive_teams) < 2) {
      cs$log("=== COMBAT ENDED ===")
      if (length(alive_teams) == 1) {
        cs$log(sprintf("Team %s is victorious!", alive_teams[1]))
      } else {
        cs$log("All combatants have fallen!")
      }
      break
    }
    
    # Action cycles
    cycle <- 1
    while (cycle <= CONFIG$DEFAULT_MAX_CYCLES_PER_ROUND) {
      # Get active fighters
      active <- Filter(function(f) f$can_act(), all_fighters)
      
      if (length(active) == 0) break
      
      # Check if multiple teams still have active fighters
      active_teams <- unique(sapply(active, function(f) {
        for (team_name in names(teams)) {
          if (f$name %in% sapply(teams[[team_name]], `[[`, "name")) {
            return(team_name)
          }
        }
        return(NA)
      }))
      
      if (length(na.omit(active_teams)) < 2) break
      
      if (cycle > 1 && verbose) {
        cs$log(sprintf("\n-- Cycle %d --", cycle))
      }
      
      # Process actions in initiative order
      for (att in active) {
        if (!att$can_act()) next
        
        # Check conditions that prevent action
        if ("outmaneuvered" %in% cs$get_conditions(att$name)) {
          cs$remove_condition(att$name, "outmaneuvered")
          cs$log(sprintf("%s recovers from being outmaneuvered", att$name))
          att$spend_ap(1)
          next
        }
        
        # Find attacker's team
        att_team <- NA
        for (team_name in names(teams)) {
          if (att$name %in% sapply(teams[[team_name]], `[[`, "name")) {
            att_team <- team_name
            break
          }
        }
        
        # Find valid targets
        enemy_team <- setdiff(names(teams), att_team)
        if (length(enemy_team) == 0) next
        
        targets <- Filter(function(f) f$is_alive() && !f$is_incapacitated(), teams[[enemy_team[1]]])
        if (length(targets) == 0) next
        
        # Select target (prefer wounded)
        def <- targets[[which.min(sapply(targets, function(t) t$get_total_hp()))]]
        
        # Resolve combat
        result <- resolve_combat(att, def, cs)
        
        if (!is.null(result$type) && result$type == "no-ap") {
          next
        }
        
        # Log results
        cs$log(sprintf("%s (%s) attacks %s! [%s %d/%d] vs [%s %d/%d]",
                       att$name, result$att_skill, def$name,
                       result$att_res$result, result$att_res$roll, result$att_res$skill,
                       result$def_skill, result$def_res$roll, result$def_res$skill))
        
        if (result$winner == "att") {
          cs$log(sprintf("  >> HIT to %s! %d damage (-%d after armor)",
                         result$location, result$dmg, result$actual_dmg))
          if (length(result$effects) > 0) {
            cs$log(sprintf("  >> Special Effects: %s",
                           paste(result$effects, collapse = ", ")))
          }
          
          # Add narrative description
          if (use_ai && result$actual_dmg > 0) {
            prompt <- sprintf("%s strikes %s's %s with their %s for %d damage. %s",
                              att$name, def$name, result$location, att$weapon, 
                              result$actual_dmg,
                              ifelse(length(result$effects) > 0, 
                                     paste("Special:", paste(result$effects, collapse=", ")), 
                                     ""))
            narration <- call_ai_api(prompt)
            cs$log(sprintf("  * %s", narration), narrative = TRUE)
          }
          
          if (!def$is_alive()) {
            cs$log(sprintf("  >> %s has been SLAIN!", def$name))
          } else if (def$is_incapacitated()) {
            cs$log(sprintf("  >> %s is INCAPACITATED!", def$name))
          }
        } else if (result$winner == "def") {
          cs$log("  >> DEFENDED successfully!")
          if (length(result$effects) > 0) {
            cs$log(sprintf("  >> Defender Special Effects: %s",
                           paste(result$effects, collapse = ", ")))
          }
        } else {
          cs$log("  >> Both combatants miss!")
        }
        
        # Check if combat should continue
        if (!any(sapply(teams[[enemy_team[1]]], function(f) f$is_alive()))) {
          break
        }
      }
      
      cycle <- cycle + 1
    }
    
    # End of round status
    if (verbose) {
      cs$log("\n=== End of Round Status ===")
      for (f in all_fighters) {
        if (f$is_alive()) {
          cs$log(f$get_status())
        }
      }
    }
  }
  
  # Combat summary
  cs$summary()
  
  # Final status
  cat("\n=== FINAL STATUS ===\n")
  for (f in all_fighters) {
    status <- if (f$is_alive()) {
      if (f$is_incapacitated()) "Incapacitated" else "Alive"
    } else {
      "Dead"
    }
    cat(sprintf("- %s: %s\n", f$name, status))
  }
  
  # Return results
  invisible(list(
    state = cs,
    fighters = all_fighters,
    log = cs$combat_log,
    narrative = cs$narrative_log
  ))
}

# ----------------------- 10. Utility Functions ---------------------------

# Create a character with specific culture and career
create_character <- function(name, culture = "civilised", career = "warrior", 
                             custom_stats = NULL, random = TRUE) {
  Character$new(name, culture, career, custom_stats, random)
}

# Quick battle with specified cultures
culture_clash <- function(rounds = 5) {
  participants <- list(
    create_character("Aldric the Bold", "civilised", "warrior"),
    create_character("Swift Wind", "nomadic", "scout"),
    create_character("Grok Skullsplitter", "barbarian", "warrior"),
    create_character("Silent Death", "primitive", "scout")
  )
  
  run_combat(participants, max_rounds = rounds)
}

# Test character creation
test_character_creation <- function() {
  cat("=== CHARACTER CREATION TEST ===\n\n")
  
  # Create one character of each culture
  cultures <- c("barbarian", "civilised", "nomadic", "primitive")
  careers <- c("warrior", "scout", "thief", "scholar", "mystic")
  
  for (culture in cultures) {
    career <- sample(careers, 1)
    char <- create_character(paste0("Test ", culture), culture, career)
    
    cat(sprintf("\n%s %s:\n", char$culture, char$career))
    cat(sprintf("  Name: %s\n", char$name))
    cat(sprintf("  Attributes: STR %d, CON %d, SIZ %d, DEX %d, INT %d, POW %d, CHA %d\n",
                char$attributes$STR, char$attributes$CON, char$attributes$SIZ,
                char$attributes$DEX, char$attributes$INT, char$attributes$POW,
                char$attributes$CHA))
    cat(sprintf("  Action Points: %d\n", char$action_points))
    cat(sprintf("  Damage Modifier: %s\n", char$damage_modifier))
    cat(sprintf("  Initiative: %d\n", char$initiative_bonus))
    cat(sprintf("  Weapon: %s\n", char$weapon))
    
    # Show some skills
    cat("  Key Skills:\n")
    combat_skill <- char$get_combat_skill()
    cat(sprintf("    %s: %d%%\n", combat_skill$name, combat_skill$value))
    cat(sprintf("    Evade: %d%%\n", char$skills[["Evade"]]))
    cat(sprintf("    Willpower: %d%%\n", char$skills[["Willpower"]]))
    
    # Show passion if any
    if (length(char$passions) > 0) {
      cat("  Passions:\n")
      for (p in names(char$passions)) {
        cat(sprintf("    %s: %d%%\n", p, char$passions[[p]]))
      }
    }
  }
}

# Test dice rolling with official examples
test_dice_official <- function() {
  cat("=== DICE ROLLING TEST (Official) ===\n")
  
  # Test damage modifiers
  modifiers <- c("-1d8", "-1d6", "-1d4", "-1d2", "0", "+1d2", "+1d4", "+1d6", "+1d8", "+1d10", "+2d6", "+3d6")
  
  cat("\nDamage Modifiers:\n")
  for (mod in modifiers) {
    if (mod == "0") {
      cat(sprintf("  %s: 0\n", mod))
    } else {
      # Roll 5 times to show range
      rolls <- replicate(5, roll_dice(mod))
      cat(sprintf("  %s: %s (avg: %.1f)\n", mod, 
                  paste(rolls, collapse=", "), mean(rolls)))
    }
  }
  
  cat("\nWeapon Damage Examples:\n")
  weapons <- c("1d8", "1d6", "2d8", "1d4+1", "1d8+1")
  for (w in weapons) {
    rolls <- replicate(5, roll_dice(w))
    cat(sprintf("  %s: %s (avg: %.1f)\n", w, 
                paste(rolls, collapse=", "), mean(rolls)))
  }
}

# ----------------------- 11. Example Usage --------------------------------

# Uncomment to run examples:

# # Example 1: Quick culture clash
 set.seed(42)
 culture_clash(rounds = 5)

# # Example 2: Custom high-powered duel
hero <- create_character("Conan", "barbarian", "warrior",
                         custom_stats = list(STR=18, CON=16, SIZ=15, DEX=14, INT=10, POW=13, CHA=12))

villain <- create_character("Dark Sorcerer", "civilised", "mystic",
                            custom_stats = list(STR=10, CON=12, SIZ=11, DEX=15, INT=18, POW=17, CHA=14))

run_combat(list(hero, villain), max_rounds = 10)

# # Example 3: Test character creation
# test_character_creation()

# # Example 4: Test dice rolling
# test_dice_official()

# Export main functions
if (exists("mythras_api")) {
  mythras_api <- list(
    run_combat = run_combat,
    Character = Character,
    create_character = create_character,
    culture_clash = culture_clash,
    test_character_creation = test_character_creation,
    roll_dice = roll_dice,
    perform_skill_check = perform_skill_check
  )
}

# Print usage instructions
cat("\n=== MYTHRAS COMBAT SIMULATOR READY ===\n")
cat("Usage examples:\n")
cat("  culture_clash(5)        # Run a 5-round battle between cultures\n")
cat("  test_character_creation()  # Test character generation\n")
cat("  test_dice_official()    # Test dice rolling\n")
cat("\nFor custom battles:\n")
cat("  hero <- create_character('Name', 'culture', 'career')\n")
cat("  run_combat(list(hero, villain), max_rounds = 10)\n")