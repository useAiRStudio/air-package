# AiR — AI Assistant for RStudio

Fix errors, generate EDA reports, clean data, and edit code — all from inside RStudio.

## Install

```r
install.packages("devtools")
devtools::install_github("useAiRStudio/air-package")
```

## Get Started

```r
library(AiR)
air_login()
```

This opens your browser to create an account (or sign in). Once complete, AiR starts automatically — you'll see the panel in your RStudio Viewer pane.

## Usage

Just type in the AiR panel. Examples:

- **Fix errors** — errors are detected automatically and AiR offers fixes
- **EDA** — "run an EDA on mtcars" generates a full R Markdown report
- **Edit code** — "add error handling to this function" edits your open file
- **Data cleaning** — "clean this dataset" creates a documented cleaning script

## Commands

| Function | Description |
|---|---|
| `air_login()` | Log in and start AiR |
| `air_start()` | Restart the AiR panel |
| `air_stop()` | Stop AiR |

## Requirements

- RStudio (Desktop or Server)
- R >= 4.0
- Internet connection (AiR uses a cloud backend)
