# AI-Assisted Risk Scenario Explainer

This Streamlit app transforms Monte Carlo results or scenario summaries into a clear, management-ready narrative using GPT.

---

## How to Run Locally

Follow these steps to run the app on your own computer.

1. **Clone the repository**

```
git clone https://github.com/syedyaseerrahman/Syed-Yaseer-Rahman-Portfolio.git
```

2. **Navigate into the app folder**

```
cd Syed-Yaseer-Rahman-Portfolio/risk-explainer
```

3. **Install required packages**

```
pip install -r requirements.txt
```

4. **Run the app**

```
streamlit run app.py
```

When you run that last command, Streamlit will open a local web page (usually at http://localhost:8501) where you can use the app interactively.

---

## How to Deploy on Streamlit Cloud

You can host this app online for free using Streamlit Cloud.

1. Go to [Streamlit.io](https://streamlit.io) and sign in using your GitHub account.  
2. Click the **Create app** button.  
3. Choose this repository: `syedyaseerrahman/Syed-Yaseer-Rahman-Portfolio`.  
4. Set the **Main file path** to:
```
risk-explainer/app.py
```
5. Open **Advanced settings → Secrets** and paste your OpenAI key like this:
```
OPENAI_API_KEY=sk-your-openai-key
```
6. Click **Deploy**.  
7. Wait 2–3 minutes while Streamlit builds your app.  
8. You’ll then get a public link (for example, `https://your-app-name.streamlit.app`) that anyone can open.

---

## Example Use

Users can:
- Enter summary statistics (mean, percentiles, breach probability), or  
- Upload a CSV of simulated results.  

The app then:
- Calculates statistics automatically,  
- Summarises the results using GPT,  
- Provides driver analysis and mitigation ideas, and  
- Generates a short board note in plain English.

---

## Folder Structure

```
risk-explainer/
│
├── app.py               # Main Streamlit app
├── requirements.txt     # Python dependencies
└── README.md            # This documentation file
```

---

## Author

**Syed Yaseer Rahman**  
Graduate in Financial Risk Management (Sept 2025)  
[GitHub Profile](https://github.com/syedyaseerrahman)
