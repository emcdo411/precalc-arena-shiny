# 🎮 Pre-Calculus Arena — Shiny App

[![App - Pre-Calculus Arena](https://img.shields.io/badge/App-Pre--Calculus%20Arena-blueviolet?style=for-the-badge\&logo=R\&logoColor=white\&color=ff69b4)](https://www.shinyapps.io/)

A **gamified Shiny app** for mastering pre-calculus concepts through quests, missions, XP, levels, and boss battles. Designed for all skill levels — from beginner to advanced — in a fun, adaptive, RPG-style learning environment.

---

## 📚 Table of Contents

* [Summary](#summary)
* [Features](#features)
* [Folder Structure](#folder-structure)
* [Run Locally](#run-locally)
* [Deploy to ShinyApps.io](#deploy-to-shinyappsio)
* [Contributing](#contributing)
* [License](#license)

---

## 📝 Summary

**Pre-Calculus Arena** transforms learning into an interactive adventure:

* Select a quest from topics like Functions, Polynomials, Trigonometry, or Limits.
* Complete missions to earn XP, build streak bonuses, and level up.
* Unlock **Boss Battles** by completing all missions in a quest.
* Play at your own pace, with adaptive difficulty and instant feedback.

Whether you’re a **student**, **teacher**, or **math enthusiast**, this app makes mastering pre-calculus more engaging than ever.

---

## 🚀 Features

* **7 Quests** covering major pre-calculus topics, each with 6 missions.
* **Adaptive Difficulty**: Easy, Normal, or Hard.
* **Gamification**: XP awards, streak bonuses, and boss chains.
* **Dynamic UI**: Dark Bootstrap theme, responsive design.
* **Deployment Ready**: Works locally or on [shinyapps.io](https://www.shinyapps.io/).

---

## 📂 Folder Structure

```
precalc-arena-shiny/
├── app.R          # Main Shiny app code (single file)
├── README.md      # Project documentation
└── .gitignore     # Optional: Ignore R-specific files (.Rhistory, .RData, etc.)
```

> If you add images or datasets, create an `assets/` folder.

---

## 💻 Run Locally

1. **Install required packages**:

   ```r
   install.packages(c("shiny", "bslib", "shinyWidgets"))
   ```
2. **Save the code** as `app.R` in your project folder.
3. **Run** in R or RStudio:

   ```r
   shiny::runApp()
   ```

---

## ☁️ Deploy to ShinyApps.io

1. **Sign up** for a free account at [shinyapps.io](https://www.shinyapps.io/).
2. **Install deployment package**:

   ```r
   install.packages("rsconnect")
   ```
3. **Authenticate**:

   ```r
   rsconnect::setAccountInfo(
     name='your-username',
     token='YOUR_TOKEN',
     secret='YOUR_SECRET'
   )
   ```

   > Token & secret are available in your shinyapps.io dashboard.
4. **Deploy**:

   ```r
   rsconnect::deployApp(appDir = "/path/to/precalc-arena-shiny")
   ```
5. Access your app at:

   ```
   https://your-username.shinyapps.io/precalc-arena-shiny/
   ```

---

## 🤝 Contributing

* **Fork** the repo
* **Add features or fix bugs** (e.g., new quests, UI tweaks)
* **Submit a pull request**

Issues & feature requests welcome!

---

## 📄 License

MIT License — free to use, modify, and share.

---

If you’d like, I can also add:

* **Screenshots** of the app in action
* A **Mermaid diagram** showing the gamified learning flow
* A **student progress badge** section for social sharing

Do you want me to add those visual elements next? That would make the README much more engaging on GitHub.

