import streamlit as st

# Language selection
language = st.radio("Please select language", ["中文","English", "Italiano"])

# Define content for each language
if language == "English":
    st.title("About This App")
    st.write("""
        #### This app allows users to input their health data such as age, sex, BMI, ASCVD status, and renin levels, and predict the probability of disease (0 or 1).
        #### This tool is meant to assist users and should not be used as a definitive medical diagnosis. Please consult a healthcare professional for any health concerns.
    """)
elif language == "中文":
    st.title("关于此应用程序")
    st.write("""
        #### 此应用程序允许用户输入他们的健康数据，例如年龄、性别、BMI、ASCVD 状态和肾素水平，并预测患病的可能性 (0 或 1)。
        #### 该工具旨在协助用户，不应作为最终的医学诊断。请咨询医疗专业人士以解决任何健康问题。
    """)
elif language == "Italiano":
    st.title("Informazioni su questa app")
    st.write("""
        #### Questa applicazione consente agli utenti di inserire i propri dati sanitari come età, sesso, BMI, stato di ASCVD e livelli di renina, e di prevedere la probabilità di malattia (0 o 1).
        #### Questo strumento è progettato per assistere gli utenti e non dovrebbe essere utilizzato come diagnosi medica definitiva. Si prega di consultare un professionista sanitario per eventuali problemi di salute.
    """)

