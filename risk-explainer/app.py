# AI-Assisted Risk Scenario Explainer
# Streamlit app: enter stats or upload a CSV of KPI outcomes, then get a concise AI narrative.

import os
import numpy as np
import pandas as pd
import streamlit as st

# OpenAI (new SDK)
try:
    from openai import OpenAI
except Exception:
    OpenAI = None

st.set_page_config(page_title="Risk Scenario Explainer", page_icon="📊", layout="wide")
st.title("AI-Assisted Risk Scenario Explainer")
st.caption("Turn Monte Carlo results or scenario summaries into a concise, management-ready narrative.")

# -----------------------------
# Sidebar: scenario context
# -----------------------------
with st.sidebar:
    st.header("Scenario inputs")
    scenario = st.text_input("Scenario name", "Supply Chain Disruption")
    metric = st.text_input("Metric (e.g., EBITDA £m)", "EBITDA (£m)")
    trials = st.number_input("Number of trials (if known)", min_value=0, value=10000, step=1000)
    threshold = st.number_input("Threshold for breach", value=85.0)
    breach_logic = st.selectbox("Breach when metric is…", ["≤ threshold (downside)", "≥ threshold (upside)"])
    drivers = st.text_area("Key drivers (ranked by variance contribution)", "Demand 45%, FX 30%, Input cost 25%")
    mitigations = st.text_area("Mitigation options under consideration", "Hedge 50%, Dual supplier, Price adjustment")
    appetite = st.text_input("Risk appetite", "≤10% chance of covenant breach")

st.markdown("### 1) Provide results manually **or** upload a CSV of simulation outcomes")

# -----------------------------
# Manual entry of summary stats
# -----------------------------
with st.expander("Enter summary statistics", expanded=True):
    c1, c2, c3 = st.columns(3)
    with c1:
        mean = st.number_input("Mean", value=100.0)
        p5 = st.number_input("P5 (5th percentile)", value=80.0)
    with c2:
        p50 = st.number_input("P50 (median)", value=100.0)
        p95 = st.number_input("P95 (95th percentile)", value=120.0)
    with c3:
        worst1 = st.number_input("Worst 1%", value=70.0)
        prob_breach_manual = st.number_input("P(breach) % (manual)", value=10.0, min_value=0.0, max_value=100.0)

# -----------------------------
# Optional: upload CSV of outcomes
# (one numeric column of KPI results)
# -----------------------------
uploaded = st.file_uploader("Upload CSV of simulated KPI outcomes (one numeric column)", type=["csv"])
prob_breach_calc = None

if uploaded:
    try:
        df = pd.read_csv(uploaded)
        numeric_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        if not numeric_cols:
            st.error("No numeric columns found in the CSV.")
        else:
            selected = st.selectbox("Select KPI column", numeric_cols)
            arr = df[selected].dropna().values
            if arr.size == 0:
                st.error("Selected column has no numeric data.")
            else:
                mean = float(np.mean(arr))
                p5 = float(np.percentile(arr, 5))
                p50 = float(np.percentile(arr, 50))
                p95 = float(np.percentile(arr, 95))
                worst1 = float(np.percentile(arr, 1))
                if "≤" in breach_logic:
                    prob_breach_calc = float((arr <= threshold).mean() * 100)
                else:
                    prob_breach_calc = float((arr >= threshold).mean() * 100)
                st.success("Computed statistics from CSV.")
    except Exception as e:
        st.error(f"Failed to read CSV: {e}")

prob_breach = prob_breach_calc if prob_breach_calc is not None else prob_breach_manual

# -----------------------------
# Show the stats table
# -----------------------------
st.markdown("### 2) Summary statistics")
stats = pd.DataFrame([{
    "Metric": metric, "Mean": round(mean, 2), "P5": round(p5, 2), "P50": round(p50, 2),
    "P95": round(p95, 2), "Worst 1%": round(worst1, 2), "P(breach) %": round(prob_breach, 2),
    "Threshold": threshold, "Trials": trials if trials else None
}])
st.dataframe(stats, use_container_width=True)

# -----------------------------
# OpenAI client helper
# -----------------------------
def get_client():
    api_key = st.secrets.get("OPENAI_API_KEY") or os.getenv("OPENAI_API_KEY", "")
    if not api_key:
        return None, "OpenAI API key missing. Set it in Streamlit Secrets or as an env var."
    if OpenAI is None:
        return None, "OpenAI SDK not installed."
    try:
        return OpenAI(api_key=api_key), None
    except Exception as e:
        return None, f"OpenAI init failed: {e}"

def build_prompt():
    return f"""
You are a professional risk analyst writing for senior management. UK English. Neutral, analytical tone.

Scenario: {scenario}
Metric: {metric}
Trials: {trials}
Results:
- Mean: {mean}
- P5 / P50 / P95: {p5} / {p50} / {p95}
- Worst 1%: {worst1}
- Probability of breach: {prob_breach}% versus threshold {threshold} ({'≤' if '≤' in breach_logic else '≥'} condition)

Key drivers (variance contribution): {drivers}
Mitigation options: {mitigations}
Risk appetite: {appetite}

Write:
1) Executive summary (4–6 sentences) that states central outcome, downside, and breach probability.
2) Driver interpretation explaining which inputs matter and why.
3) Three specific, practical mitigation actions tied to the drivers and appetite.
4) One visual suggestion a PMO can build in Excel (name only).
5) A short three-sentence 'Board note' in plain language.

Be concise, decision-oriented, and avoid buzzwords.
"""

st.markdown("### 3) Generate AI narrative")
if st.button("Generate summary"):
    client, err = get_client()
    if err:
        st.error(err)
    else:
        with st.spinner("Asking AI…"):
            try:
                resp = client.chat.completions.create(
                    model="gpt-4o-mini",   # change to gpt-4o if you have access
                    messages=[{"role": "user", "content": build_prompt()}],
                    temperature=0.25,
                )
                out = resp.choices[0].message.content
                st.markdown("#### AI-generated summary")
                st.write(out)

                md = f"# {scenario} — Risk Scenario Summary\n\n" + stats.to_markdown(index=False) + "\n\n---\n\n" + out
                st.download_button(
                    "Download as Markdown",
                    data=md.encode("utf-8"),
                    file_name=f"{scenario.replace(' ', '_')}_summary.md",
                    mime="text/markdown",
                )
            except Exception as e:
                st.error(f"OpenAI request failed: {e}")

st.info("Tip: either enter stats manually or upload a CSV of KPI outcomes. Keep your API key in Streamlit Secrets.")
