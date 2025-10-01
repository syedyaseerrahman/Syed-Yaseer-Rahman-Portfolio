# AI-Assisted Risk Scenario Explainer

This project is a self-guided demonstration of how risk simulation data can be transformed into clear management insights using AI.  
It shows an understanding of Monte Carlo simulation, statistical thresholds, variance drivers, and narrative generation using a large language model (LLM).

---

## Project Overview

In traditional financial or operational risk modeling, analysts often run **Monte Carlo simulations** to understand uncertainty — for example, how much a company’s EBITDA might fluctuate under stress.

However, senior management does not read simulation tables. They want to know:
> “How bad can it get? What’s the chance of breach? Why does it happen? What should we do?”

This app automates that translation process.  
It accepts summary statistics or simulated results (such as Mean, P5, P50, P95, breach probability) and uses an **AI model (Groq Llama 3)** to generate a narrative summary, including:

1. An **executive summary** of the risk profile  
2. A **driver interpretation** explaining key variance contributors  
3. **Actionable mitigation steps** based on results  
4. A short **board note** summarising conclusions  

This makes the output suitable for decision-makers who need insight, not raw numbers.

---

## Tools and Frameworks Used

| Tool | Purpose |
|------|----------|
| **Python** | Core programming language |
| **Streamlit** | To build an interactive dashboard |
| **Pandas** | Data handling and summary calculations |
| **Groq API (Llama 3)** | AI narrative generation |
| **Monte Carlo summary statistics** | Risk input structure |
| **Markdown Export** | To produce readable summaries |

---

## Key Conceptual Steps

1. **Input Simulation Data:**  
   The user either enters summary statistics (Mean, P5, P50, P95, Worst 1%, Breach %) or uploads a CSV file of Monte Carlo simulation outcomes.

2. **Threshold Setting:**  
   A breach threshold is defined — for example, if EBITDA drops below £85m, it is considered a risk breach.

3. **Deterministic Analysis:**  
   The app computes a “risk rating” based on the breach probability (e.g., High if >10%, Medium if 5–10%, Low if <5%).

4. **AI Narrative Generation:**  
   The AI converts the numerical data into a clear written interpretation.  
   For example, if the breach chance is 10%, it may say:  
   > “The model shows a 1-in-10 chance of falling below the £85m downside threshold, indicating thin headroom under the current appetite.”  

5. **Variance Drivers:**  
   The user can specify key drivers (e.g., “Demand 45%, FX 30%, Input Cost 25%”), which the AI uses to discuss causes and sensitivities.

6. **Mitigation Suggestions:**  
   The AI provides practical recommendations like hedging, dual sourcing, or price adjustment clauses.

7. **Final Summary:**  
   A structured output is generated, written in plain English for board-level audiences.

---

## Example AI Prompt Used

When the user clicks **Generate Summary**, the app sends a structured prompt to the AI model, such as:

