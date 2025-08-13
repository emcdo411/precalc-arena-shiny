library(shiny)
library(bslib)
library(shinyWidgets)

# -----------------------------
# Game constants & helpers
# -----------------------------
QUESTS <- list(
  "Functions & Graphs" = list(code = "functions", missions = 6),
  "Polynomial & Rational" = list(code = "poly", missions = 6),
  "Exponential & Logarithmic" = list(code = "exp_log", missions = 6),
  "Trigonometry & Identities" = list(code = "trig", missions = 6),
  "Systems & Matrices" = list(code = "systems", missions = 6),
  "Sequences & Series" = list(code = "seq_series", missions = 6),
  "Limits & Intro Calculus" = list(code = "limits", missions = 6)
)

DIFFICULTIES <- c("Easy", "Normal", "Hard")

xp_award <- function(difficulty, correct, streak) {
  if (!correct) return(0)
  base <- switch(difficulty,
                 "Easy" = 12,
                 "Normal" = 22,
                 "Hard" = 35,
                 15)
  bonus <- min(streak, 5) * 3
  base + bonus
}

level_from_xp <- function(xp) {
  lvl <- floor(sqrt(max(0, xp) / 120)) + 1
  min(max(1, lvl), 100)
}

xp_to_next <- function(xp) {
  lvl <- level_from_xp(xp)
  current_floor <- ((lvl - 1)^2) * 120
  next_threshold <- (lvl^2) * 120
  progress <- if (lvl == 1) min(1, xp / next_threshold) else (xp - current_floor) / (next_threshold - current_floor)
  list(
    current_lvl = lvl,
    current_floor = current_floor,
    next_threshold = next_threshold,
    progress = progress
  )
}

num_tol_equal <- function(user, correct, tol = 1e-4) {
  if (is.null(user) || is.na(user) || !is.numeric(user)) return(FALSE)
  abs(user - correct) <= tol
}

# -----------------------------
# Procedural mission generators
# -----------------------------
rint <- function(a, b) sample(seq(a, b), 1)
rchoice <- function(x) sample(x, 1)

make_functions_mission <- function(diff) {
  a <- rint(-6, 6); if (a == 0) a <- 2
  b <- rint(-8, 8)
  x0 <- rint(-6, 6)
  type <- rchoice(c("value", "inverse", "composition"))
  if (diff == "Easy") type <- "value"
  if (type == "value") {
    prompt <- sprintf("Let f(x) = %dx %s %d. Compute f(%d).", a, if (b >= 0) "+" else "-", abs(b), x0)
    ans <- a * x0 + b
    hint <- "Plug x into ax+b, then simplify."
    return(list(prompt = prompt, answer = ans, hint = hint, type = "numeric"))
  } else if (type == "inverse") {
    y0 <- a * x0 + b
    prompt <- sprintf("f(x) = %dx %s %d. Given y = %d, solve for x.", a, if (b >= 0) "+" else "-", abs(b), y0)
    ans <- (y0 - b) / a
    hint <- "Rearrange: y-b = ax, so x = (y-b)/a."
    return(list(prompt = prompt, answer = ans, hint = hint, type = "numeric"))
  } else {
    m <- rint(-5, 5); if (m == 0) m <- 3
    c0 <- rint(-5, 5)
    x0 <- rint(-4, 4)
    prompt <- sprintf("f(x)=%dx %s %d and g(x)=%dx %s %d. Compute f(g(%d)).", a, if (b >= 0) "+" else "-", abs(b), m, if (c0 >= 0) "+" else "-", abs(c0), x0)
    ans <- a * (m * x0 + c0) + b
    hint <- "First compute g(x0), then plug into f."
    return(list(prompt = prompt, answer = ans, hint = hint, type = "numeric"))
  }
}

make_poly_mission <- function(diff) {
  r1 <- rint(-5, 5); r2 <- rint(-5, 5)
  while (r1 == r2) r2 <- rint(-5, 5)
  a <- rchoice(c(1, 1, 1, 2, -1))
  b <- -a * (r1 + r2)
  c <- a * (r1 * r2)
  whichroot <- rchoice(c("r1", "r2", "sum", "product"))
  if (diff == "Easy") whichroot <- rchoice(c("r1", "r2"))
  prompt <- sprintf("For %dx^2 %s %dx %s %d, ", a, if (b >= 0) "+" else "-", abs(b), if (c >= 0) "+" else "-", abs(c))
  if (whichroot == "r1") prompt <- paste0(prompt, "find one root.")
  if (whichroot == "r2") prompt <- paste0(prompt, "find one root.")
  if (whichroot == "sum") prompt <- paste0(prompt, "what is the sum of the roots?")
  if (whichroot == "product") prompt <- paste0(prompt, "what is the product of the roots?")
  ans <- switch(whichroot, r1 = r1, r2 = r2, sum = r1 + r2, product = r1 * r2)
  hint <- "Use Vieta's formulas: sum = -b/a, product = c/a."
  list(prompt = prompt, answer = ans, hint = hint, type = "numeric")
}

make_exp_log_mission <- function(diff) {
  a <- rchoice(c(2, 3, 5, 10))
  k <- rint(1, 4)
  b <- a^k
  mode <- rchoice(c("log", "exp"))
  if (diff == "Hard") {
    b <- if (rchoice(c(TRUE, FALSE))) a^k else 1 / a^k
  }
  if (mode == "log") {
    prompt <- sprintf("Compute log_%d(%s).", a, if (b < 1) sprintf("1/%d", a^k) else as.character(b))
    ans <- if (b < 1) -k else k
    hint <- "log_a(a^k)=k and log_a(a^-k)=-k."
  } else {
    x0 <- rint(1, 3)
    prompt <- sprintf("Compute %d^%d.", a, x0)
    ans <- a^x0
    hint <- "Multiply a by itself x times."
  }
  list(prompt = prompt, answer = as.numeric(ans), hint = hint, type = "numeric")
}

make_trig_mission <- function(diff) {
  angs <- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  fn <- rchoice(c("sin", "cos", "tan"))
  theta <- rchoice(angs)
  val <- switch(fn,
                sin = sinpi(theta / 180),
                cos = cospi(theta / 180),
                tan = tanpi(theta / 180))
  if (is.infinite(val) || is.na(val)) {
    val_txt <- "undefined"; ans_type <- "text"
    ans <- "undefined"
  } else {
    val_txt <- round(val, 4); ans_type <- "numeric"
    ans <- as.numeric(val_txt)
  }
  prompt <- sprintf("Compute %s(%d°). If undefined, type 'undefined'.", fn, theta)
  hint <- "Use the unit circle values for special angles."
  list(prompt = prompt, answer = ans, hint = hint, type = ans_type)
}

make_systems_mission <- function(diff) {
  x_sol <- rint(-5, 5); y_sol <- rint(-5, 5)
  a <- rint(1, 6); b <- rint(1, 6); d <- rint(1, 6); e <- rint(1, 6)
  c <- a * x_sol + b * y_sol
  f <- d * x_sol + e * y_sol
  ask <- rchoice(c("x", "y"))
  if (diff == "Hard") ask <- rchoice(c("x", "y", "both"))
  if (ask == "x") {
    prompt <- sprintf("Solve the system: %dx %s %dy = %d, %dx %s %dy = %d. Find x.",
                      a, if (b >= 0) "+" else "-", abs(b), c, d, if (e >= 0) "+" else "-", abs(e), f)
    ans <- x_sol; type <- "numeric"
  } else if (ask == "y") {
    prompt <- sprintf("Solve the system: %dx %s %dy = %d, %dx %s %dy = %d. Find y.",
                      a, if (b >= 0) "+" else "-", abs(b), c, d, if (e >= 0) "+" else "-", abs(e), f)
    ans <- y_sol; type <- "numeric"
  } else {
    prompt <- sprintf("Solve the system: %dx %s %dy = %d, %dx %s %dy = %d. Give (x,y) as 'x,y'.",
                      a, if (b >= 0) "+" else "-", abs(b), c, d, if (e >= 0) "+" else "-", abs(e), f)
    ans <- paste0(x_sol, ",", y_sol); type <- "text"
  }
  hint <- "Use elimination or substitution."
  list(prompt = prompt, answer = ans, hint = hint, type = type)
}

make_seq_series_mission <- function(diff) {
  mode <- rchoice(c("arith", "geom"))
  if (mode == "arith") {
    a1 <- rint(-8, 8); d <- rint(-5, 5); if (d == 0) d <- 3
    n <- rint(4, 12)
    prompt <- sprintf("Arithmetic sequence with a1=%d and d=%d. Compute a_%d.", a1, d, n)
    ans <- a1 + (n - 1) * d
    hint <- "a_n = a1 + (n-1)d"
  } else {
    a1 <- rint(1, 6); r <- rchoice(c(2, 3, 1/2)); n <- rint(3, 7)
    prompt <- sprintf("Geometric sequence with a1=%s and r=%s. Compute a_%d.", a1, format(r, trim = TRUE), n)
    ans <- a1 * (r)^(n - 1)
    hint <- "a_n = a1 * r^(n-1)"
  }
  list(prompt = prompt, answer = as.numeric(round(ans, 6)), hint = hint, type = "numeric")
}

# ---------- FIXED: Limits mission ----------
make_limits_mission <- function(diff) {
  mode <- rchoice(c("direct", "removable"))
  if (diff == "Easy") mode <- "direct"
  
  if (mode == "direct") {
    m <- rint(-5, 5); if (m == 0) m <- 2
    b <- rint(-8, 8)
    x0 <- rint(-6, 6)
    prompt <- sprintf("Compute lim_{x->%d} (%dx %s %d).", x0, m, if (b >= 0) "+" else "-", abs(b))
    ans <- m * x0 + b
    hint <- "Linear is continuous: substitute x0."
    return(list(prompt = prompt, answer = ans, hint = hint, type = "numeric"))
  } else {
    # (x^2 - a^2)/(x - a) -> x + a -> 2a as x -> a
    a <- rint(-5, 5); if (a == 0) a <- 2
    prompt <- sprintf("Compute lim_{x->%d} (x^2 %s %d)/(x %s %d).",
                      a,
                      if ((-a^2) >= 0) "+" else "-", abs(-a^2),
                      if ((-a) >= 0) "+" else "-", abs(-a))
    ans <- 2 * a
    hint <- "Factor numerator as (x-a)(x+a); cancel (x-a), then substitute."
    return(list(prompt = prompt, answer = ans, hint = hint, type = "numeric"))
  }
}

make_mission <- function(topic_code, diff) {
  switch(topic_code,
         functions = make_functions_mission(diff),
         poly = make_poly_mission(diff),
         exp_log = make_exp_log_mission(diff),
         trig = make_trig_mission(diff),
         systems = make_systems_mission(diff),
         seq_series = make_seq_series_mission(diff),
         limits = make_limits_mission(diff))
}

make_boss <- function(topic_code) {
  list(
    make_mission(topic_code, "Normal"),
    make_mission(topic_code, "Normal"),
    make_mission(topic_code, "Hard")
  )
}

# -----------------------------
# UI
# -----------------------------
theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = font_google("Rubik"),
  heading_font = font_google("Rubik")
)

ui <- page_fluid(
  theme = theme,
  tags$head(tags$style(HTML("
    .xp-bar{height:20px;border-radius:10px;background:#222;}
    .xp-fill{height:100%;border-radius:10px;background:linear-gradient(90deg,#00d4ff,#00ff88);}
    .badge{font-size:0.85rem;}
    .card{border-radius:1rem;border:1px solid #444;box-shadow:0 0 10px rgba(0,0,0,0.5);}
    .hint{opacity:0.9;}
    .gm{font-family: 'Rubik', sans-serif;}
    .boss{border:2px dashed #888;border-radius:14px;padding:12px;}
    .streak{font-weight:600;}
    .qprompt{font-size:1.15rem;}
    .xpnum{font-weight:700;}
    .loot{font-size:0.95rem;}
    .muted{opacity:0.85;}
    .code{font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas;}
    .btn-lg{border-radius:0.8rem;}
    .pill{border-radius:999px;}
    .alert{min-height:50px;padding:10px;margin-top:10px;border:1px solid #444;border-radius:8px;z-index:1000;}
  "))),
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h3("🎮 Pre-Calculus Arena", class = "gm"),
      p("Welcome, Math Challenger. Answer missions, earn XP, level up, and defeat Bosses.", class = "muted"),
      selectInput("quest", "Choose Quest:", choices = names(QUESTS)),
      radioGroupButtons(
        inputId = "difficulty", label = "Difficulty",
        choices = DIFFICULTIES, selected = "Easy",
        justified = TRUE
      ),
      actionButton("startMission", "Start Mission", class = "btn btn-primary btn-lg w-100 mt-2"),
      hr(),
      h5("Your Stats"),
      uiOutput("xpUI"),
      div(class = "d-flex justify-content-between mt-2",
          span("Level: ", textOutput("level", inline = TRUE)),
          span("Streak: ", textOutput("streak", inline = TRUE))
      ),
      div(class = "d-flex justify-content-between mt-1",
          span("Missions in Quest: ", textOutput("questProg", inline = TRUE)),
          span("Boss Ready: ", textOutput("bossReady", inline = TRUE))
      ),
      hr(),
      h6("Debug Info (Temporary)"),
      uiOutput("debugInfo"),
      hr(),
      h6("Copy-paste Prompt (optional)"),
      tags$textarea(
        class = "form-control code", rows = 8, readonly = NA,
        "You are now my Pre-Calculus Game Master AI. Turn topics into quests and problems into missions. Ask 3–5 diagnostic warm-ups, adapt difficulty, award XP with streak bonuses, and unlock boss battles after a quest. Keep feedback witty and positive."
      )
    ),
    
    card(
      card_header(h4("🧠 Current Mission")),
      card_body(
        uiOutput("missionPrompt"),
        fluidRow(
          column(6,
                 uiOutput("answerUI")),
          column(6,
                 actionButton("submit", "Submit", class = "btn btn-success btn-lg w-100 mt-2"),
                 actionButton("hint", "Hint", class = "btn btn-secondary w-100 mt-2"),
                 actionButton("giveup", "Give Up (Show Answer)", class = "btn btn-outline-light w-100 mt-2")
          )
        ),
        uiOutput("feedback")
      )
    ),
    
    card(
      card_header(h4("👑 Boss Battle")),
      card_body(
        p("Defeat the boss by clearing all its chained challenges.", class = "muted"),
        uiOutput("bossPanel")
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(
    xp = 0,
    level = 1,
    streak = 0,
    quest_code = QUESTS[[1]]$code,
    missions_done_in_quest = 0,
    mission = NULL,
    mission_id = 0,
    awaiting = FALSE,
    feedback = NULL,
    last_correct = NA,
    boss_ready = FALSE,
    boss_nodes = NULL,
    boss_state = NULL,
    boss_feedback = NULL
  )
  
  observe({
    rv$quest_code <- QUESTS[[input$quest]]$code
    cat("Quest updated:", rv$quest_code, "\n")
  })
  
  output$xpUI <- renderUI({
    meta <- xp_to_next(rv$xp)
    pct <- round(100 * meta$progress)
    div(class = "xp-bar",
        div(class = "xp-fill", style = sprintf("width:%d%%;", max(2, pct))),
        div(class = "d-flex justify-content-between mt-1",
            span(sprintf("XP: %d", rv$xp), class = "xpnum"),
            span(sprintf("To L%d: %d", meta$current_lvl + 1, max(0, meta$next_threshold - rv$xp)))
        )
    )
  })
  
  output$level <- renderText({ level_from_xp(rv$xp) })
  output$streak <- renderText({ rv$streak })
  output$questProg <- renderText({ sprintf("%d / %d", rv$missions_done_in_quest, QUESTS[[input$quest]]$missions) })
  output$bossReady <- renderText({ if (rv$boss_ready) "YES" else "no" })
  
  output$debugInfo <- renderUI({
    div(class = "text-muted",
        p("Mission Active: ", if (is.null(rv$mission)) "No" else "Yes"),
        p("Awaiting Answer: ", if (isTRUE(rv$awaiting)) "Yes" else "No"),
        p("Mission ID: ", rv$mission_id)
    )
  })
  
  observeEvent(input$startMission, {
    cat("Start Mission clicked\n")
    rv$mission <- make_mission(rv$quest_code, input$difficulty)
    rv$mission_id <- rv$mission_id + 1
    rv$awaiting <- TRUE
    rv$feedback <- NULL
    updateNumericInput(session, "ans_num", value = NA)
    updateTextInput(session, "ans_text", value = "")
    cat("New mission started, ID:", rv$mission_id, "\n")
  })
  
  output$missionPrompt <- renderUI({
    req(rv$mission)
    cat("Rendering mission prompt:", rv$mission$prompt, "\n")
    div(class = "qprompt", HTML(paste0("<b>Mission:</b> ", rv$mission$prompt)))
  })
  
  output$answerUI <- renderUI({
    req(rv$mission, rv$mission_id)
    cat("Rendering answer UI, type:", rv$mission$type, ", mission_id:", rv$mission_id, "\n")
    if (rv$mission$type == "numeric") {
      numericInput("ans_num", "Your Answer:", value = NA)
    } else {
      textInput("ans_text", "Your Answer:", value = "")
    }
  })
  
  output$feedback <- renderUI({
    req(rv$feedback)
    cat("Rendering feedback\n")
    rv$feedback
  })
  
  observeEvent(input$submit, {
    cat("Submit button clicked, awaiting:", rv$awaiting, "\n")
    if (!isTRUE(rv$awaiting) || is.null(rv$mission)) {
      rv$feedback <- div(class = "alert alert-warning", "No active mission. Start a new mission!")
      cat("No active mission\n")
      return()
    }
    correct <- FALSE
    if (rv$mission$type == "numeric") {
      user <- as.numeric(input$ans_num)
      if (is.na(user)) {
        rv$feedback <- div(class = "alert alert-danger", "Please enter a valid number.")
        cat("Invalid numeric input\n")
        return()
      }
      correct <- num_tol_equal(user, as.numeric(rv$mission$answer))
      cat("Numeric answer check: user =", user, ", correct =", rv$mission$answer, ", result =", correct, "\n")
    } else {
      user <- trimws(tolower(input$ans_text))
      if (user == "") {
        rv$feedback <- div(class = "alert alert-danger", "Please enter an answer.")
        cat("Empty text input\n")
        return()
      }
      correct <- identical(user, trimws(tolower(as.character(rv$mission$answer))))
      cat("Text answer check: user =", user, ", correct =", rv$mission$answer, ", result =", correct, "\n")
    }
    
    if (correct) {
      rv$streak <- rv$streak + 1
      gained <- xp_award(input$difficulty, TRUE, rv$streak)
      rv$xp <- rv$xp + gained
      rv$missions_done_in_quest <- rv$missions_done_in_quest + 1
      rv$feedback <- div(class = "alert alert-success", HTML(sprintf("✅ <b>Correct!</b> +%d XP (Streak x%d).", gained, rv$streak)))
      rv$awaiting <- FALSE
      updateNumericInput(session, "ans_num", value = NA)
      updateTextInput(session, "ans_text", value = "")
      cat("Correct answer, XP gained:", gained, ", Streak:", rv$streak, "\n")
      
      if (rv$missions_done_in_quest >= QUESTS[[input$quest]]$missions) {
        rv$boss_ready <- TRUE
        cat("Boss ready unlocked\n")
      }
    } else {
      rv$streak <- 0
      rv$feedback <- div(class = "alert alert-danger", "❌ Not quite. Try again or press <b>Hint</b>/<b>Give Up</b>.")
      cat("Incorrect answer, streak reset\n")
    }
  })
  
  observeEvent(input$hint, {
    cat("Hint button clicked\n")
    req(rv$mission)
    rv$feedback <- div(class = "alert alert-info hint", HTML(paste0("💡 Hint: ", rv$mission$hint)))
  })
  
  observeEvent(input$giveup, {
    cat("Give Up button clicked\n")
    req(rv$mission)
    rv$streak <- 0
    rv$feedback <- div(class = "alert alert-warning", HTML(sprintf("📘 Answer revealed: <b>%s</b>", as.character(rv$mission$answer))))
    rv$awaiting <- FALSE
    updateNumericInput(session, "ans_num", value = NA)
    updateTextInput(session, "ans_text", value = "")
  })
  
  output$bossPanel <- renderUI({
    if (!rv$boss_ready) {
      return(div(class = "text-muted", "Boss locked. Clear all missions in the selected quest to unlock."))
    }
    
    if (is.null(rv$boss_nodes)) {
      rv$boss_nodes <- make_boss(rv$quest_code)
      rv$boss_state <- list(step = 1, correct = 0)
      cat("Boss battle initialized, nodes created\n")
    }
    
    node <- rv$boss_nodes[[rv$boss_state$step]]
    
    tagList(
      div(class = "boss",
          h5(sprintf("Boss Step %d of 3", rv$boss_state$step)),
          p(node$prompt, class = "qprompt"),
          if (node$type == "numeric") numericInput("boss_ans_num", "Your Answer:", value = NA)
          else textInput("boss_ans_text", "Your Answer:", value = ""),
          div(class = "d-flex gap-2 mt-2",
              actionButton("boss_submit", "Strike!", class = "btn btn-danger"),
              actionButton("boss_hint", "Hint", class = "btn btn-outline-light"),
              actionButton("boss_giveup", "Yield (Show)", class = "btn btn-outline-secondary")
          ),
          uiOutput("boss_feedback")
      )
    )
  })
  
  output$boss_feedback <- renderUI({
    req(rv$boss_feedback)
    cat("Rendering boss feedback\n")
    rv$boss_feedback
  })
  
  observeEvent(input$boss_submit, {
    cat("Boss Submit button clicked, step:", rv$boss_state$step, "\n")
    req(rv$boss_nodes, rv$boss_state)
    node <- rv$boss_nodes[[rv$boss_state$step]]
    correct <- FALSE
    if (node$type == "numeric") {
      user <- as.numeric(input$boss_ans_num)
      if (is.na(user)) {
        rv$boss_feedback <- div(class = "alert alert-danger", "Please enter a valid number.")
        cat("Invalid boss numeric input\n")
        return()
      }
      correct <- num_tol_equal(user, as.numeric(node$answer))
      cat("Boss numeric answer check: user =", user, ", correct =", node$answer, ", result =", correct, "\n")
    } else {
      user <- trimws(tolower(input$boss_ans_text))
      if (user == "") {
        rv$boss_feedback <- div(class = "alert alert-danger", "Please enter an answer.")
        cat("Empty boss text input\n")
        return()
      }
      correct <- identical(user, trimws(tolower(as.character(node$answer))))
      cat("Boss text answer check: user =", user, ", correct =", node$answer, ", result =", correct, "\n")
    }
    
    if (correct) {
      rv$boss_state$correct <- rv$boss_state$correct + 1
      if (rv$boss_state$step < 3) {
        rv$boss_state$step <- rv$boss_state$step + 1
        rv$boss_feedback <- div(class = "alert alert-success", "💥 Critical hit! Advance to the next step.")
        updateNumericInput(session, "boss_ans_num", value = NA)
        # ---------- FIXED: clear the boss text input, not regular one ----------
        updateTextInput(session, "boss_ans_text", value = "")
        cat("Boss step advanced to", rv$boss_state$step, "\n")
      } else {
        bonus <- 60 + 10 * rv$boss_state$correct
        rv$xp <- rv$xp + bonus
        rv$boss_ready <- FALSE
        rv$boss_nodes <- NULL
        rv$boss_state <- NULL
        rv$missions_done_in_quest <- 0
        rv$boss_feedback <- div(class = "alert alert-success", HTML(sprintf("🏆 Boss defeated! Bonus +%d XP. Quest resets for replay.", bonus)))
        cat("Boss defeated, XP bonus:", bonus, "\n")
      }
    } else {
      rv$boss_feedback <- div(class = "alert alert-danger", "⛔ Missed! Try a different angle or use a hint.")
      cat("Boss answer incorrect\n")
    }
  })
  
  observeEvent(input$boss_hint, {
    cat("Boss Hint button clicked\n")
    req(rv$boss_nodes, rv$boss_state)
    node <- rv$boss_nodes[[rv$boss_state$step]]
    rv$boss_feedback <- div(class = "alert alert-info", paste("💡 Hint:", node$hint))
  })
  
  observeEvent(input$boss_giveup, {
    cat("Boss Give Up button clicked\n")
    req(rv$boss_nodes, rv$boss_state)
    node <- rv$boss_nodes[[rv$boss_state$step]]
    rv$boss_feedback <- div(class = "alert alert-warning", HTML(sprintf("📘 Answer: <b>%s</b>", as.character(node$answer))))
    updateNumericInput(session, "boss_ans_num", value = NA)
    # ---------- FIXED: clear the boss text input, not regular one ----------
    updateTextInput(session, "boss_ans_text", value = "")
  })
}

shinyApp(ui, server)


