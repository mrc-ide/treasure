# AGENTS.md — Guidance for AI Agents

This file guides AI code-generation agents working on this R package.

------------------------------------------------------------------------

## 🧩 Project Structure

-   `R/` — R functions
-   `man/` — roxygen-generated documentation
-   `tests/testthat/` — unit tests
-   `vignettes/` — usage guides (knitr)
-   `data/` — included datasets
-   `.github/workflows/` — CI pipelines (R CMD check, linting, pkgdown)
-   `_pkgdown.yml` — site configuration (deployed to `gh-pages`)

------------------------------------------------------------------------

## 🔨 Coding & Style Conventions

-   Language, naming, comments, and commit messages in **English** only.
-   Use **roxygen2** (e.g. `#' @param`, `@return`) for documentation.
-   Follow **tidyverse style**: 2-space indent, spaces around operators.
-   Functions and variable names in `snake_case`.

------------------------------------------------------------------------

## ✅ Core Coding Principles

-   Prioritise **clarity** and **readability** over cleverness.
-   Use **pure functions** with clearly defined inputs and outputs where possible.
-   Follow the **Single Responsibility Principle** — each function should do one thing.
-   Avoid code duplication — use helper functions to factor repeated logic.
-   Use **early returns** to reduce indentation and nesting.
-   Avoid deeply nested logic or long functions; split logic into modular components.
-   Document every exported function with consistent roxygen tags. Use @export for public functions only.

------------------------------------------------------------------------

## 📄 Testing

This package uses testthat for unit testing:

-   Mirror function names in `tests/testthat/test-*.R`.
-   Use test_that("description", { ... }) blocks.
-   Prefer multiple small, readable expectations.
-   Mock external data or randomness where needed for repeatability.

------------------------------------------------------------------------

## 🧹 Pull Request Checklist

AI-generated pull requests should meet these criteria:

-   A clear, descriptive title and summary
-   Related issue referenced (if applicable)
-   New tests for new or modified functions
-   Code passes checks
-   Documentation updated (roxygen and/or vignette)
-   No changes to CI, versioning, or .github/ files unless requested
-   Branch names for opened PRs should be succinct, concise, clear

------------------------------------------------------------------------

## 📦 Build & Release Workflow

-   Update documentation: `devtools::document()`
-   Run tests: `devtools::test()`
-   Run package checks: `devtools::check()`

------------------------------------------------------------------------

## 🔭 Scope

Agents must **not** manually edit:

-   `man/`
-   `.github/`
-   `pkgdown/`
-   `data/`
-   `data-raw/`
-   `DESCRIPTION` version number
-   Licence or authorship information

------------------------------------------------------------------------

## 📖 Tone and Style Guide for Vignettes

Vignettes should aim to be:

-   **Friendly and accessible** — Write as if you're explaining the concepts to an informed but non-expert colleague.
-   **Accurate and precise** — Technical correctness is essential. Avoid oversimplifying core ideas.
-   **Narrative in structure** — Use a clear logical flow: introduce the purpose, explain the context, walk through examples.
-   **Example-driven** — Use realistic, well-commented examples to demonstrate concepts. Wherever possible, relate examples to plausible use cases.

### Do:
-   Use a warm, professional tone: "Let's explore how this function works..."
-   Explain reasoning as well as code steps.
-   Show intermediate outputs when helpful.
-   You can use emoji and other elements to help with engagement

### Don’t:
-   Assume deep prior knowledge of the package internals.
-   Use overly casual language.
-   Skip steps or leave important arguments unexplained.

### Voice checklist:
-   Conversational but professional
-   No unexplained acronyms or jargon
-   Every code block has context and interpretation
-   The reader should feel guided and confident, not testedTone and Style Guide for Vignettes

------------------------------------------------------------------------
