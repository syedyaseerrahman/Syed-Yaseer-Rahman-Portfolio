import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import io
from groq import Groq
import os

# ------------------------------
# APP CONFIG
# ------------------------------
st.set_page_config(page_title="AI-Assisted Risk Scenario Explainer", layout="wide")

st.title("AI-Assisted Risk Scenario Explainer")
st.caption("Deterministic risk statistics + Groq Llama 3 for a management-ready narrative. Upload non-confidential data only.")

# ------------------------------
# GROQ CLIENT
# ------------------------------
client = Groq(api_key=os.environ.get("GROQ_API_KEY"))

# ------------------------------
# SIDEBAR INPUTS
# ------------------------------
with st.sidebar:
    st.header("Scenario inputs")

    model_name = st.text_input("Groq model", value="moonshotai/kimi-k2-instruct-0905")

    scenario = st.text_input("Scenario name", "Supply Chain Disruption")
    metric = st.text_input("Metric (e.g., EBITDA £m)", "EBITDA (£m)")
    trials = st.number_input("Number of trials (if known)", min_value=1, value=10000)
    threshold = st.number_input("Threshold for breach", value=85.0)
    breach_condition = st.selectbox("Breach when metric is…", ["≤ threshold (downside)", "≥ threshold (upside)"])
    drivers = st.text_area("Key drivers (ranked by variance contribution)", "Demand 45%, FX 30%, Input cost 25%")
    appetite = st.slider("Risk appetite: max acceptable breach probability (%)", 1, 20, 10)

# ------------------------------
# UPLOAD OR ENTER STATS
# ------------------------------
st.subheader("1) Provide results manually or upload a CSV of simulation outcomes")

tab1, tab2 = st.tabs(["Enter summary statistics", "Upload CSV"])

with tab1:
    mean = st.number_input("Mean", value=100.0)
    p5 = st.number_input("P5 (5th percentile)", value=80.0)
    p50 = st.number_input("P50 (median)", value=100.0)
    p95 = st.number_input("P95 (95th percentile)", value=120.0)
    worst1 = st.number_input("Worst 1%", value=70.0)
    prob_breach_manual = st.number_input("P(breach) % (manual)", value=10.0)

with tab2:
    uploaded = st.file_uploader("Upload CSV with one numeric KPI column (e.g., EBITDA)", type=["csv"])

# ------------------------------
# CSV HANDLING
# ------------------------------
if uploaded is not None:
    data = pd.read_csv(uploaded)
    col = data.columns[0]
    outcomes = data[col]
    mean = outcomes.mean()
    p5 = outcomes.quantile(0.05)
    p50 = outcomes.median()
    p95 = outcomes.quantile(0.95)
    worst1 = outcomes.quantile(0.01)
    if "≤" in breach_condition:
        prob_breach = (outcomes <= threshold).mean() * 100
        tail_loss = outcomes[outcomes <= threshold].mean() if (outcomes <= threshold).any() else None
    else:
        prob_breach = (outcomes >= threshold).mean() * 100
        tail_loss = outcomes[outcomes >= threshold].mean() if (outcomes >= threshold).any() else None
else:
    outcomes = None
    prob_breach = prob_breach_manual
    tail_loss = None

# ------------------------------
# SANITY CHECKS
# ------------------------------
warnings = []
if p5 > p50:
    warnings.append("⚠️ P5 is greater than P50. Check your inputs.")
if p50 > p95:
    warnings.append("⚠️ P50 is greater than P95. Check your inputs.")
if worst1 > p5:
    warnings.append("⚠️ Worst 1% is higher than P5. Typically, worst 1% should be lower.")
for w in warnings:
    st.warning(w)

# ------------------------------
# SUMMARY STATS TABLE
# ------------------------------
st.subheader("2) Summary statistics")

stats_df = pd.DataFrame({
    "Metric": [metric],
    "Mean": [mean],
    "P5": [p5],
    "P50": [p50],
    "P95": [p95],
    "Worst 1%": [worst1],
    "P(breach) %": [prob_breach],
    "Threshold": [threshold],
    "Trials": [trials]
})

st.dataframe(stats_df, use_container_width=True)

# ------------------------------
# Appetite chip + risk rating
# ------------------------------
if prob_breach <= appetite:
    appetite_status = "🟢 Within appetite"
else:
    appetite_status = "🔴 Outside appetite"

if prob_breach >= 20 or (p5 < threshold and "≤" in breach_condition):
    risk_rating = "High"
elif prob_breach >= 10:
    risk_rating = "Medium"
else:
    risk_rating = "Low"

st.markdown(f"**Risk rating:** {risk_rating} | **Appetite status:** {appetite_status}")

# ------------------------------
# HISTOGRAM VISUAL
# ------------------------------
if outcomes is not None:
    st.write("### Distribution of outcomes")
    fig, ax = plt.subplots(figsize=(6, 3))
    ax.hist(outcomes, bins=30, color="#4c72b0", alpha=0.7)
    ax.axvline(threshold, color="red", linestyle="--", label="Threshold")
    for val, label in [(p5, "P5"), (p50, "P50"), (p95, "P95")]:
        ax.axvline(val, linestyle=":", color="black")
        ax.text(val, ax.get_ylim()[1]*0.9, label, rotation=90, ha="center", color="black")
    ax.legend()
    ax.set_xlabel(metric)
    ax.set_ylabel("Frequency")
    st.pyplot(fig)

# ------------------------------
# LLM-ASSISTED SUMMARY
# ------------------------------
st.subheader("3) Generate AI narrative")

prompt = f"""
You are a financial risk analyst. Write a concise, management-ready risk summary using only the information below. 
Do NOT invent new numbers or drivers. Explain clearly, using human professional tone.

Scenario: {scenario}
Metric: {metric}
Mean: {mean:.1f}, P5: {p5:.1f}, P50: {p50:.1f}, P95: {p95:.1f}, Worst1%: {worst1:.1f}
Threshold: {threshold:.1f}, Breach condition: {breach_condition}
Probability of breach: {prob_breach:.1f}%
Expected shortfall (average outcome below threshold): {tail_loss if tail_loss else 'N/A'}
Drivers: {drivers}
Risk appetite: {appetite}%
Risk rating: {risk_rating}
Appetite status: {appetite_status}
Trials: {trials}

Follow this structure:
1. Executive summary (1 paragraph)
2. Driver interpretation (1 paragraph)
3. Actionable mitigation steps (3 numbered)
4. Board note (brief one-liner)
"""

if st.button("Generate summary"):
    try:
        completion = client.chat.completions.create(
            model=model_name,
            messages=[{"role": "user", "content": prompt}],
            temperature=0.4,
            max_tokens=400
        )
        result = completion.choices[0].message.content
    except Exception as e:
        result = f"⚠️ LLM unavailable. Fallback narrative.\n\nThe scenario shows mean {metric} {mean:.1f} with {prob_breach:.1f}% chance of breach. Risk rating: {risk_rating}. Drivers: {drivers}."
    st.markdown("### AI-generated summary")
    st.markdown(result)
    md_content = f"# {scenario} – Risk Scenario Summary\n\n" + stats_df.to_markdown(index=False) + "\n\n" + result
    st.download_button("Download as Markdown", data=md_content, file_name=f"{scenario}_summary.md")

st.info("Free to use. Accuracy derives from deterministic inputs. Groq Llama 3 adds professional narrative polish.")
