# Assamese GPT — Domain-Restricted AI System for UPSC and State Public Service Examination Support

## Overview

This project involves the development of a **domain-restricted, Assamese-language GPT system** designed to provide structured, syllabus-aligned responses for **UPSC and State Public Service examination preparation**.

Unlike general-purpose chatbots, this system is being engineered to operate within a **controlled academic domain**, ensuring responses remain relevant, reliable, and aligned with examination material.

The assistant will allow users to interact fully in Assamese, enabling regional-language aspirants to access AI-supported learning without requiring English-language interaction.

---

## Motivation

Most existing AI systems are optimised for English-language users, creating accessibility barriers for regional-language examination aspirants.

This project aims to address that gap by building an **Assamese-first AI assistant** that provides:

- Structured academic explanations  
- Syllabus-aligned responses  
- Controlled, domain-restricted behaviour  

This work is being developed independently to improve accessibility and demonstrate applied AI system engineering capability.

A significant number of capable students preparing for public service examinations in India have completed their education entirely in regional languages and may not have the English-language fluency required to effectively use modern AI tools. As a result, they remain excluded from the productivity and learning benefits these systems provide.

The broader objective of this project is to develop a scalable architecture that can, over time, be extended to support additional regional languages where similar accessibility barriers exist, enabling more equitable access to AI-assisted academic preparation.

---

## Technical Architecture

The system is being designed using a **Retrieval-Augmented Generation (RAG) architecture**, enabling responses to be grounded in curated examination material rather than relying solely on general model knowledge.

This ensures that responses are based on syllabus-aligned sources and remain domain-restricted.

Core components include:

### 1. LLM Inference Layer

- API-based large language model inference  
- Assamese-language prompt conditioning  
- Structured response formatting aligned with examination answer styles  

### 2. Knowledge Base Construction

- Curated UPSC and State Public Service examination material  
- Topic-wise content organisation  
- Conversion of source material into machine-retrievable format  

### 3. Retrieval Pipeline

- User queries converted into semantic representations  
- Similarity-based retrieval of relevant examination content  
- Injection of retrieved content into the model prompt to guide response generation  

This ensures that the model generates answers grounded in relevant syllabus material rather than general-purpose responses.

### 4. Domain Restriction and Guardrails

- Prompt-level behavioural constraints  
- Explicit refusal logic for out-of-scope queries  
- Enforcement of examination-focused response scope  

### 5. Interface Layer (Planned)

- Lightweight web interface for user interaction  
- Assamese input and output support  

### 6. Evaluation Framework (Planned)

- Response quality testing  
- Domain compliance verification  
- Hallucination and refusal behaviour testing    

---

## Development Status

### Completed

- System architecture design  
- Behavioural and domain restriction specification  
- Retrieval and knowledge base framework planning  

### In Progress

- Knowledge base construction  
- Retrieval pipeline implementation  
- Initial system prototyping  

### Planned

- Pilot deployment with limited users  
- Evaluation and iterative improvement  
- Interface deployment  

---

## Example Use Case

**User input (Assamese):**

ভাৰতীয় সংবিধানৰ মৌলিক অধিকাৰ কি?

**Expected system behaviour:**

- Retrieve relevant examination-aligned material  
- Generate a structured explanation in Assamese  
- Maintain syllabus relevance and academic tone  
- Refuse or redirect queries outside examination scope  

---

## Technical Significance

This project demonstrates applied capability in:

- Retrieval-Augmented Generation (RAG)  
- Domain-restricted LLM system design  
- Knowledge base and retrieval pipeline construction  
- Multilingual NLP application  
- AI system architecture and deployment planning  

---

## Repository Structure

assamese-gpt/

- README.md — Project overview and architecture  
- ROADMAP.md — Development milestones and progress tracking  
- sample_inputs_outputs.md — Example queries and behaviour specification  
- app.py — Initial system scaffold  

---

## Project Status

Active development.
