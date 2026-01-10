# AIPF - Agentic Intelligence Processing Framework

The name is inspired by famous **Business Object Processing Framework (BOPF)**

**So far AIPF is under heavy development!!!**

**Disclaimer**: SAP, ABAP, BTP, and BOPF are trademarks or registered trademarks of SAP SE in Germany and other countries. AIPF is an independent open-source project and is not affiliated with, sponsored by, or endorsed by SAP SE.

### 🧠 The "Why"
Standard AI calls in ABAP are stateless—they send a prompt and get a response. **AIPF** adds the **Brain and Muscles**:
* **Brain:** State management, long-term memory, and self-reflection loops.
* **Muscles:** The ability to execute BAPIs, RAP Actions, and OData services autonomously.

### 🛠 Key Features
* **Agentic Orchestration:** Define multi-step reasoning loops directly in ABAP.
* **Clean Core Ready:** Built for S/4HANA Cloud using released APIs.
* **Memory Management:** Persistence layer for conversation history and "thinking" states.
* **Tool Integration:** Seamless binding between LLM reasoning and ABAP executable logic.

### ☁️ Architecture: ABAP Cloud & SAP BTP

**AIPF** follows a **side-by-side extension pattern** and **clean core** design, bridging the gap between high-level AI reasoning and on-stack business execution:

* **Orchestration Layer**: Runs on **ABAP Cloud** (S/4HANA Public or Private Cloud and BTP), managing the state of the agentic loop, memory persistence, and tool dispatching.
* **Intelligence Layer**: Connects to **SAP BTP (Generative AI Hub)** to securely access Large Language Models (LLMs) such as GPT-4, Claude, or Mistral.
* **Communication**: Leverages the **ABAP AI SDK** for secure, authenticated, and "Clean Core" compliant communication between the SAP backend and BTP AI services.

### 📦 Installation
1. Install [abapGit](https://abapgit.org/).
2. Create a new Online Repo with the URL: `https://github.com/IlyaPrusakou/aipf`
3. Pull the objects into the system.

### :mechanical_arm: Technical Overview
```mermaid

flowchart TD
    Start(["User Input: Prompt String + Agent Name"]) --> Init["Initialize Agent and Query Object"]
    Init --> Decision1{"Invoke Decision Engine<br/>(LLM, ML, IF-ELSE)"}
    Decision1 --> Plan["Generate Step Plan"]
    Plan --> Build["Build Execution Run + Query + Steps"]
    Build --> Exec["Execute Steps according to Step Plan"]
    Exec --> Decision2{"Invoke Decision Engine"}
    Decision2 --> Response["Generate Final Response"]
    Response --> End(["Return Final Response String to User"])

    style Start fill:#f9f,stroke:#333,stroke-width:2px
    style End fill:#f9f,stroke:#333,stroke-width:2px
    style Decision1 fill:#fff4dd,stroke:#d4a017,stroke-width:2px
    style Decision2 fill:#fff4dd,stroke:#d4a017,stroke-width:2px

```
