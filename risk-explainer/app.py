import os
import io
import numpy as np
import pandas as pd
import streamlit as st

# OpenAI (new SDK)
try:
    from openai import OpenAI
except Exception:
    OpenAI = None

# -------------------------------
# Page setup
# -------------------------------
st.set_page_config(page_title="AI-Assisted Risk Scenario Explainer", page_icon="📊", layout="wide")
st.title("AI-Assisted Risk Scenario Explainer")
st.caption("Turn Monte Carlo results or scenario summaries into a clear, management-ready narrative.")

# -------------------------------
# Helper: get API key safely
# -------------------------------
def get_openai_client():
    api_key = st.secrets.get("OPENAI_API_KEY", None) or os.getenv("OPENAI_API_KEY", "")
    if not api_key:
        return None, "No API key found. Set OPENAI_API_KEY in Streamlit secrets or as an environment variable."
    if OpenAI is None:
        return None, "OpenAI SDK not installed correctly."
    try:
        client = OpenAI(api_key=api_key)
        return client, None
    except Exception as e:
        return None, f"Failed to init OpenAI client: {e}"

# -------------------------------
# Sidebar inputs
# -------------------------------
with st.sidebar:
    st.header("Scenario Inputs")
    scenario = st.text_input("Scenario name", "Supply Chain Disruption")
    metric = st.text_input("Metric name", "EBITDA (£m)")
    trials = st.number_input("Number of trials (if known)", min_value=0, value=10000, step=1000)
    threshold = st.number_input("Breach threshold", value=85.0)
    breach_logic = st.selectbox(
        "Breach condition applies when metric is...",
        options=["≤ threshold (downside breach)", "≥ threshold (upside breach)"],
        index=0
    )
    drivers = st.text_area("Key drivers (ranked by variance contribution)",
                           "Demand variance 45%, FX 30%, Input cost 25%")
    mitigations = st.text_area("Mitigation options being considered",
                               "Hedge 50%, Dual supplier, Price adjustment")
    risk_appetite = st.text_input("Risk appetite statement",
                                  "No more than 10% chance of covenant breach")

st.markdown("### 1) Provide results manually **or** upload a CSV of simulation outcomes")

# -------------------------------
# Manual stats entry
# -------------------------------
with st.expander("Enter summary stats manually", expanded=True):
    c1, c2, c3 = st.columns(3)
    with c1:
        mean = st.number_input("Mean", value=100.0)
        p5 = st.number_input("P5 (5th percentile)", value=80.0)
    with c2:
        p50 = st.number_input("P50 (median)", value=100.0)
        p95 = st.number_input("P95 (95th percentile)", value=120.0)
    with c3:
        worst1 = st.number_input("Worst 1%", value=70.0)
        prob_breach_manual = st.number_input("Probability of breach (%)", value=10.0, min_value=0.0, max_value=100.0)

# -------------------------------
# CSV upload route (optional)
# -------------------------------
uploaded = st.file_uploader("Upload CSV of simulated outcomes (one numeric column of the KPI)", type=["csv"])
selected_col = None
prob_breach_calc = None
df = None

if uploaded:
    try:
        df = pd.read_csv(uploaded)
        numeric_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        if not numeric_cols:
            st.error("No numeric columns found in the CSV.")
        else:
            selected_col = st.selectbox("Select the KPI column", options=numeric_cols)
            arr = df[selected_col].dropna().values
            if arr.size > 0:
                mean = float(np.mean(arr))
                p5 = float(np.percentile(arr, 5))
                p50 = float(np.percentile(arr, 50))
                p95 = float(np.percentile(arr, 95))
                worst1 = float(np.percentile(arr, 1))
                if "≤" in breach_logic:
                    prob_breach_calc = float((arr <= threshold).mean() * 100)
                else:
                    prob_breach_calc = float((arr >= threshold).mean() * 100)
                st.success("Computed stats from CSV.")
            else:
                st.error("Selected column has no numeric data.")
    except Exception as e:
        st.error(f"Failed to read CSV: {e}")

# Use calculated breach prob if present; else manual
prob_breach = prob_breach_calc if prob_breach_calc is not None else prob_breach_manual

# -------------------------------
# Show the stats table
# -------------------------------
st.markdown("### 2) Summary statistics")
stats_df = pd.DataFrame([{
    "Metric": metric,
    "Mean": round(mean, 2),
    "P5": round(p5, 2),
    "P50": round(p50, 2),
    "P95": round(p95, 2),
    "Worst 1%": round(worst1, 2),
    "P(breach) %": round(prob_breach, 2),
    "Threshold": threshold,
    "Trials": trials if trials else None
}])
st.dataframe(stats_df, use_container_width=True)

# -------------------------------
# Build prompt for AI
# -------------------------------
def build_prompt():
    return f"""
You are a professional risk analyst writing for senior management. UK English. Neutral, analytical tone. No hype.

Scenario: {scenario}
Metric: {metric}
Trials: {trials}
Results:
- Mean: {mean}
- Stdev: unavailable (use percentiles to infer dispersion)
- P5 / P50 / P95: {p5} / {p50} / {p95}
- Worst 1%: {worst1}
- Probability of breach: {prob_breach}% versus threshold {threshold} ({'≤' if '≤' in breach_logic else '≥'} condition)

Drivers (variance contribution, self-reported): {drivers}
Mitigation options under consideration: {mitigations}
Risk appetite: {risk_appetite}

Write the following:
1) Executive summary in 4 to 6 sentences stating central outcome, material downside, breach probability, and an overall read.
2) Driver interpretation explaining which factors matter most and why.
3) Three specific mitigation actions logically tied to drivers and risk appetite.
4) One useful visual name a PMO could use in Excel (name only, no code).
5) A short 'Board note' in three sentences for senior readers.

Keep it concise and decision oriented. Avoid generic phrases. Avoid buzzwords.
"""

# -------------------------------
# Generate AI narrative
# -------------------------------
st.markdown("### 3) Generate management narrative")
col_a, col_b = st.columns([1, 1])
with col_a:
    run_ai = st.button("Generate AI summary")
with col_b:
    client, client_err = (None, None)  # initialise

output_text = ""

if run_ai:
    prompt = build_prompt()
    client, client_err = get_openai_client()
    if client_err:
        st.error(client_err)
    else:
        with st.spinner("Generating summary..."):
            try:
                resp = client.chat.completions.create(
                    model="gpt-4o-mini",  # small, fast; change to gpt-4o for higher quality
                    messages=[{"role": "user", "content": prompt}],
                    temperature=0.25,
                )
                output_text = resp.choices[0].message.content
                st.markdown("#### AI-generated summary")
                st.write(output_text)
            except Exception as e:
                st.error(f"OpenAI request failed: {e}")

# -------------------------------
# Download report
# -------------------------------
st.markdown("### 4) Download")
if output_text:
    md = f"# {scenario} — Risk Scenario Explainer\n\n" + stats_df.to_markdown(index=False) + "\n\n---\n\n" + output_text
    st.download_button("Download as Markdown", data=md.encode("utf-8"),
                       file_name=f"{scenario.replace(' ', '_')}_summary.md", mime="text/markdown")
