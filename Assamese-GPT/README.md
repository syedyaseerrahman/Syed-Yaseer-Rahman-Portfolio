# Assamese GPT — Exam-Focused AI Assistant (UPSC + State PSC)

## What this is
I am developing a **domain-restricted GPT system** that allows students to **ask questions in Assamese and receive Assamese answers** aligned to **UPSC and State Public Service exam preparation**.

Unlike general chatbots, this assistant is designed to:
- **Respond only within the exam syllabus domain**
- Provide **structured, study-oriented explanations**
- Remain **safe and consistent** (no unrelated outputs)

## Why this matters
Many high-quality AI study tools assume strong English proficiency. This creates a practical access gap for regional-language aspirants.  
This project aims to improve **AI accessibility for Assamese-medium students**, built in my own time for social impact.

## Core requirements
- **Assamese-first interface** (questions and answers in Assamese)
- **Domain restriction** (UPSC + State PSC preparation only)
- **Syllabus-aligned outputs** (exam-style explanation, not casual chat)
- **Controlled behaviour** (refuse irrelevant queries and redirect)

## Technical approach (high level)
Planned components:
- LLM integration (API-based inference)
- Domain restriction through prompt rules + retrieval from curated material
- Assamese language handling (prompt conditioning + evaluation)
- Simple web UI for pilot users

## Current status
This repository currently contains:
- A clear build roadmap and milestone plan
- Example Assamese inputs/outputs and expected assistant behaviour
- Initial design decisions for domain restriction and evaluation

See: `ROADMAP.md` and `sample_inputs_outputs.md`.
