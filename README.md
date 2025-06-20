# 🩸 Iron Deficiency Anemia Dashboard

This project provides an interactive data dashboard to explore insights related to **Iron Deficiency Anemia (IDA)** — a common form of anemia caused by insufficient iron. The application is built using **R Shiny** and visualizes various aspects of the disease, including demographics, comorbidities, and global comparisons.

## 🌐 Live Demos

- **IDA WHO Global Statistics Dashboard**:  
  [https://quynhphuong.shinyapps.io/IDA_WHO/](https://quynhphuong.shinyapps.io/IDA_WHO/)  
  → Visualizes global data on iron deficiency anemia from WHO.

- **Vietnam Clinical Data Dashboard**:  
  [https://quynhphuong.shinyapps.io/Iron_deficiency_anemia/](https://quynhphuong.shinyapps.io/Iron_deficiency_anemia/)  
  → Analyzes patient-level clinical data on anemia cases in Vietnam.

## 📁 Project Structure

Iron_deficiency_anemia/
├── app.R # Main Shiny app
├── data/ # Folder containing data files (.xlsx)
│ ├── admission.xlsx
│ ├── diagnosis.xlsx
│ └── ICD10.xlsx
├── www/ # Assets (e.g., logos, custom CSS)
└── README.md # Project overview and instructions


## 📊 Features

### 1. **IDA WHO Dashboard**
- Map-based visualization of global anemia burden
- Comparison by sex, age groups, and regions
- WHO target indicators and country rankings

### 2. **Clinical Dashboard (Vietnam data)**
- Patient demographics (age, sex, weight)
- Top 20 ICD-10 codes associated with IDA
- Comorbidities, hospital stay duration (LOS), and correlations
- T-test, ANOVA, and ML classification (predicting long-stay patients)

## 📦 Technologies Used
- **R** & **Shiny**
- **ggplot2** and **plotly** for interactive plots
- **shinydashboard** for layout
- **readxl**, **dplyr**, **tidyr** for data wrangling
- **corrplot**, **caret**, and **randomForest** for analysis

## 📌 How to Run Locally

1. Clone the repository:
```bash
git clone https://github.com/quynhphuong1209/Iron_deficiency_anemia.git
```
2. Open app.R in RStudio

3. Install missing packages if needed:
```{r}
install.packages(c("shiny", "shinydashboard", "ggplot2", "readxl", "dplyr", "tidyr", "plotly", "corrplot", "caret", "randomForest"))
```
4. Run the app:
```{r}
shiny::runApp("app.R")
```
👩‍💻 Author
Đinh Lê Quỳnh Phương
Student – Data Science | Hanoi University of Public Health
