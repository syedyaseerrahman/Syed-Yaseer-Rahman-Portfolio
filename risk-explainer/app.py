# AI-Assisted Risk Scenario Explainer — Groq version (portfolio-ready)
# Deterministic statistics + Llama 3 narrative (free) with a safe template fallback.

import os
import io
import numpy as np
import pandas as pd
import streamlit as st
import matplotlib.pyplot as plt

# Groq SDK
try:
    from groq import Groq
except Exception:
    Groq = None

# ------------------ Page config ------------------
st.set_page_config(page_title="Risk Scenario Explainer", page_icon="📊", layout="wide")
st.title("AI-Assisted Risk Scenario Explainer")
st.caption("Deterministic risk statistics + Groq Llama 3 for a management-ready narrative. Upload non-confidential data only.")

# Small how-to nudge
with st.expander("How to use (quick)"):
    st.write(
        "1) Enter summary stats **or** upload a CSV with a single numeric KPI column.\n"
        "2) Check the computed percentiles, worst case, and breach probability.\n"
        "3) Click **Generate summary** to produce an executive summary, drivers and actions.\n"
        "Tip: For downside risk, a breach occurs when KPI ≤ threshold."
    )

# ------------------ Sidebar inputs ------------------
with st.sidebar:
    st.header("Scenario inputs")

    # Model selector (future-proof, defaults to a current Groq model)
    model_id = st.text_input(
        "Groq model",
        value="moonshotai/kimi-k2-instruct-0905",
        help="If this model is deprecated later, paste another from Groq Console (e.g., llama-3.1-8b-instant)."
    )

    scenario = st.text_input("Scenario name", "Supply Chain Disruption")
    metric = st.text_input("Metric (e.g., EBITDA £m)", "EBITDA (£m)")
    trials = st.number_input("Number of trials (if known)", min_value=0, value=10000, step=1000)
    threshold = st.number_input("Threshold for breach", value=85.0, help="For downside risk, a breach is KPI ≤ threshold.")
    breach_logic = st.selectbox("Breach when metric is…", ["≤ threshold (downside)", "≥ threshold (upside)"])
    drivers = st.text_area("Key drivers (ranked by variance contribution)", "Demand 45%, FX 30%, Input cost 25%")
    mitigations = st.text_area("Mitigation options under consideration", "Hedge 50%, Dual supplier, Price adjustment")
    appetite = st.text_input("Risk appetite", "≤10% chance of covenant breach")

st.markdown("### 1) Provide results manually **or** upload a CSV of simulation outcomes")

# ------------------ Manual stats entry ------------------
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

# ------------------ CSV upload route ------------------
st.write("**Upload CSV with one numeric KPI column (e.g., EBITDA).**")
uploaded = st.file_uploader("Upload CSV", type=["csv"])
prob_breach_calc = None
arr = None
selected_col = None

if uploaded:
    try:
        df = pd.read_csv(uploaded)
        numeric_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        if not numeric_cols:
            st.error("No numeric columns found in the CSV.")
        else:
            selected_col = st.selectbox("Select KPI column", numeric_cols)
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
                st.success("Computed statistics from CSV.")
            else:
                st.error("Selected column has no numeric data.")
    except Exception as e:
        st.error(f"Failed to read CSV: {e}")

# Provide a quick sample CSV for testers
with st.expander("Need a sample CSV?"):
    st.write("Download a synthetic sample (1,000 draws, roughly N(100, 10)) with column name `KPI`.")
    rng = np.random.default_rng(42)
    sample = pd.DataFrame({"KPI": rng.normal(loc=100, scale=10, size=1000)})
    sample_buf = io.StringIO()
    sample.to_csv(sample_buf, index=False)
    st.download_button("Download sample_outcomes.csv", data=sample_buf.getvalue(), file_name="sample_outcomes.csv", mime="text/csv")

# choose manual vs computed breach probability
prob_breach = prob_breach_calc if prob_breach_calc is not None else prob_breach_manual

# ------------------ Summary statistics table ------------------
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

# Optional: quick histogram if CSV provided
if arr is not None and arr.size > 0:
    fig, ax = plt.subplots()
    ax.hist(arr, bins=30)
    ax.axvline(threshold, linestyle="--")
    ax.set_title(f"{metric} distribution (threshold shown)")
    st.pyplot(fig)

# ------------------ Deterministic risk rating ------------------
def risk_rating(prob, p5_val, p95_val, threshold_val, downside=True):
    """
    Defensible rules:
      High   if P(breach) >= 20% OR (downside and P5 < threshold) OR (upside and P95 > threshold)
      Medium if 5% <= P(breach) < 20%
      Low    if P(breach) < 5% AND (downside: P5 >= threshold) / (upside: P95 <= threshold)
    """
    if downside:
        if prob >= 20 or p5_val < threshold_val:
            return "High"
        elif prob >= 5:
            return "Medium"
        else:
            return "Low"
    else:
        if prob >= 20 or p95_val > threshold_val:
            return "High"
        elif prob >= 5:
            return "Medium"
        else:
            return "Low"

downside = "≤" in breach_logic
rating = risk_rating(prob_breach, p5, p95, threshold, downside)

# Show a colored status chip
rating_color = {"High": "#d9534f", "Medium": "#f0ad4e", "Low": "#5cb85c"}[rating]
st.markdown(
    f"""<div style="margin:8px 0 0 0;">
        <span style="background:{rating_color}; color:white; padding:6px 10px; border-radius:16px; font-weight:600;">
        Risk rating: {rating}
        </span>
    </div>""",
    unsafe_allow_html=True,
)

# ------------------ Groq client + prompt ------------------
def get_groq_client():
    api_key = st.secrets.get("GROQ_API_KEY") or os.getenv("GROQ_API_KEY", "")
    if not api_key:
        return None, "Groq API key missing. Add GROQ_API_KEY in Streamlit Secrets."
    if Groq is None:
        return None, "Groq SDK not installed. Add `groq` to requirements.txt."
    try:
        return Groq(api_key=api_key), None
    except Exception as e:
        return None, f"Failed to init Groq client: {e}"

def build_prompt():
    return f"""
You are a risk analyst writing a concise management summary in UK English. Neutral, factual, decision-oriented.

Scenario: {scenario}
Metric: {metric}
Trials: {trials}
Breach condition: {"≤ threshold (downside)" if downside else "≥ threshold (upside)"}
Threshold: {threshold}

Results:
- Mean: {mean}
- P5 / P50 / P95: {p5} / {p50} / {p95}
- Worst 1%: {worst1}
- Probability of breach: {prob_breach}%
- Deterministic risk rating: {rating}

Self-reported drivers: {drivers}
Mitigation options: {mitigations}
Risk appetite: {appetite}

Write:
1) Executive summary (4–6 sentences) stating central outcome, material downside/upside, breach probability, and overall read.
2) Driver interpretation (which inputs matter most and why).
3) Three actionable mitigation steps tied to drivers and appetite (no generic advice).
4) One visual a PMO can build in Excel (name only).
5) A three-sentence 'Board note' in plain language.

Keep it specific and concise. Avoid buzzwords and filler.
"""

def llm_summarise(prompt_text: str, model_name: str):
    client, err = get_groq_client()
    if err:
        return None, err
    try:
        resp = client.chat.completions.create(
            model=model_name,                      # user-selectable model
            messages=[{"role": "user", "content": prompt_text}],
            temperature=0.2,
            max_tokens=700,
        )
        return resp.choices[0].message.content, None
    except Exception as e:
        return None, str(e)

def template_summary():
    side = "downside" if downside else "upside"
    lines = [
        f"Executive summary: The analysis indicates a {rating.lower()} level of {side} risk to {metric}.",
        f"The probability of threshold breach is approximately {prob_breach:.1f}%, with P5/P50/P95 at {p5:.1f}/{p50:.1f}/{p95:.1f}.",
        f"Worst 1% outcome is {worst1:.1f}. Threshold is {threshold:.1f}; risk appetite is '{appetite}'.",
        f"Key drivers: {drivers}.",
        f"Recommended actions (for illustration): {mitigations}.",
        "Board note: Findings are based on deterministic percentiles and breach probability; actions align with stated appetite."
    ]
    return "\n\n".join(lines)

# ------------------ Generate narrative ------------------
st.markdown("### 3) Generate AI narrative")
if st.button("Generate summary"):
    prompt = build_prompt()
    text, err = llm_summarise(prompt, model_id)
    if err:
        st.warning(f"LLM unavailable ({err}). Showing deterministic template instead.")
        text = template_summary()

    st.markdown("#### AI-generated summary")
    st.write(text)

    # Download as Markdown (robust even if tabulate is missing)
    table_md = stats_df.to_markdown(index=False) if hasattr(stats_df, "to_markdown") else stats_df.to_csv(index=False)
    md = f"# {scenario} — Risk Scenario Summary\n\n{table_md}\n\n---\n\n{text}"
    st.download_button(
        "Download as Markdown",
        data=md.encode("utf-8"),
        file_name=f"{scenario.replace(' ', '_')}_summary.md",
        mime="text/markdown",
    )

st.info("Free to use. Accuracy comes from deterministic statistics; Groq Llama 3 provides narrative polish. Template fallback ensures continuity.")
