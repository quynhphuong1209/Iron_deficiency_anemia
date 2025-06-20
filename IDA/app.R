#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)
library(caret)
library(DT)
library(ggpubr)
library(openxlsx)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = span(icon("tint"), "PHÂN TÍCH THIẾU MÁU DO THIẾU SẮT"), 
    titleWidth = 450
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Tổng quan dữ liệu", tabName = "overview", icon = icon("database")),
      menuItem("Phân tích theo mã bệnh", tabName = "icd", icon = icon("file-medical")),
      menuItem("Thông tin nhân khẩu", tabName = "demo", icon = icon("user-friends")),
      menuItem("Phân tích chẩn đoán", tabName = "diag", icon = icon("diagnoses")),
      menuItem("Thời gian nằm viện", tabName = "los", icon = icon("procedures")),
      menuItem("Phân tích nâng cao", tabName = "advanced", icon = icon("chart-line"))
    ),
    tags$div(
      style = "padding: 15px;",
      helpText("Phân tích bệnh nhân có mã ICD10: D500, D508, D509")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$style(HTML("
        .main-header .logo {
          font-weight: bold;
          font-size: 20px;
          font-family: 'Arial', sans-serif;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .box-header {
          background-color: #f8f9fa !important;
          border-bottom: 2px solid #dee2e6;
          font-weight: bold;
          color: #2c3e50;
          font-size: 16px;
        }
        .box.box-solid.box-primary>.box-header {
          background-color: #1976D2 !important;
          color: white !important;
        }
        .btn-default {
          color: #333;
          background-color: #fff;
          border-color: #ccc;
        }
        .shiny-input-container {
          color: #333;
        }
        .content-wrapper {
          background-color: #f9f9f9;
        }
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #3c8dbc;
        }
        .btn-success {
          background-color: #28a745;
          border-color: #28a745;
        }
        .btn-success:hover {
          background-color: #218838;
          border-color: #1e7e34;
        }
      "))
    ),
    tabItems(
      # Tab tổng quan dữ liệu
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Thông tin dữ liệu", width = 12, status = "primary", solidHeader = TRUE,
            verbatimTextOutput("data_summary")
          )
        ),
        fluidRow(
          box(
            title = "Xem trước dữ liệu nhập viện", width = 6, solidHeader = TRUE,
            DTOutput("admission_preview")
          ),
          box(
            title = "Xem trước dữ liệu chẩn đoán", width = 6, solidHeader = TRUE,
            DTOutput("diagnosis_preview")
          )
        )
      ),
      
      # Tab phân tích theo mã bệnh
      tabItem(
        tabName = "icd",
        fluidRow(
          box(
            title = "Phân bố các loại thiếu máu", width = 6, solidHeader = TRUE,
            plotOutput("icd_dist_plot", height = 400)
          ),
          box(
            title = "Thống kê theo mã bệnh", width = 6, solidHeader = TRUE,
            DTOutput("icd_stats_table")
          )
        ),
        fluidRow(
          box(
            title = "Phân bố giới tính theo mã bệnh", width = 6, solidHeader = TRUE,
            plotOutput("gender_by_icd_plot", height = 400)
          ),
          box(
            title = "Phân bố tuổi theo mã bệnh", width = 6, solidHeader = TRUE,
            plotOutput("age_by_icd_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Thời gian nằm viện theo mã bệnh", width = 12, solidHeader = TRUE,
            plotOutput("los_by_icd_plot", height = 400)
          )
        )
      ),
      
      # Tab thông tin nhân khẩu
      tabItem(
        tabName = "demo",
        fluidRow(
          box(
            title = "Phân bố giới tính", width = 6, solidHeader = TRUE,
            plotOutput("gender_plot", height = 400)
          ),
          box(
            title = "Phân bố độ tuổi", width = 6, solidHeader = TRUE,
            plotOutput("age_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Phân bố theo nhóm tuổi", width = 6, solidHeader = TRUE,
            plotOutput("age_group_plot", height = 400)
          ),
          box(
            title = "Nguồn nhập viện", width = 6, solidHeader = TRUE,
            plotOutput("admsource_plot", height = 400)
          )
        )
      ),
      
      # Tab phân tích chẩn đoán
      tabItem(
        tabName = "diag",
        fluidRow(
          box(
            title = "Phân loại chẩn đoán", width = 6, solidHeader = TRUE,
            plotOutput("icd_plot", height = 400)
          ),
          box(
            title = "Vị trí chẩn đoán", width = 6, solidHeader = TRUE,
            plotOutput("position_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Top 20 bệnh kèm theo", width = 12, solidHeader = TRUE,
            plotOutput("comorbidity_plot", height = 500)
          )
        )
      ),
      
      # Tab thời gian nằm viện
      tabItem(
        tabName = "los",
        fluidRow(
          box(
            title = "Phân bố thời gian nằm viện", width = 6, solidHeader = TRUE,
            plotOutput("los_hist", height = 400)
          ),
          box(
            title = "Thời gian NV theo giới tính", width = 6, solidHeader = TRUE,
            plotOutput("los_gender", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Thời gian NV theo nhóm tuổi", width = 6, solidHeader = TRUE,
            plotOutput("los_age", height = 400)
          ),
          box(
            title = "Thời gian NV theo tuổi & giới tính", width = 6, solidHeader = TRUE,
            plotOutput("los_age_gender", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Kết quả kiểm định T-test", width = 12, solidHeader = TRUE,
            verbatimTextOutput("ttest_result")
          )
        )
      ),
      
      # Tab phân tích nâng cao
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Ma trận tương quan", width = 6, solidHeader = TRUE,
            plotOutput("corr_plot", height = 400)
          ),
          box(
            title = "Phân tích nhóm nguy cơ", width = 6, solidHeader = TRUE,
            plotOutput("risk_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Mô hình dự đoán thời gian NV", width = 12, solidHeader = TRUE,
            plotOutput("prediction_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Đánh giá mô hình", width = 12, solidHeader = TRUE,
            verbatimTextOutput("model_summary")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load pre-existing data files
  filtered_admission <- reactive({
    # Read from your local path
    read_excel("filtered_admission.xlsx")
  })
  
  filtered_diag <- reactive({
    read_excel("diagnosis_chi_D500_D508_D509.xlsx")
  })
  
  # Combine data for analysis
  combined_data <- reactive({
    req(filtered_admission(), filtered_diag())
    
    filtered_admission() %>%
      left_join(filtered_diag(), by = "admission_id") %>%
      mutate(
        Gender = case_when(
          Gender == 1 ~ "Nam",
          Gender == 2 ~ "Nữ",
          TRUE ~ "Không xác định"
        ),
        age_group = cut(age_years,
                        breaks = c(0, 10, 20, 30, 40, 50, 60, Inf),
                        labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                        right = FALSE),
        high_risk = ifelse(age_years > 65 | los > 7, "Cao", "Thấp")
      )
  })
  
  # Data summary
  output$data_summary <- renderPrint({
    req(filtered_admission(), filtered_diag())
    
    cat("TỔNG QUAN DỮ LIỆU\n")
    cat("=================\n\n")
    cat("Số bệnh nhân:", nrow(filtered_admission()), "\n")
    cat("Số chẩn đoán:", nrow(filtered_diag()), "\n")
    
    cat("\nPhân bố theo mã bệnh:\n")
    print(table(filtered_diag()$diagnosis))
    
    cat("\nThống kê thời gian nằm viện:\n")
    print(summary(filtered_admission()$los))
  })
  
  # Data preview
  output$admission_preview <- renderDT({
    req(filtered_admission())
    datatable(head(filtered_admission(), 20), 
              options = list(scrollX = TRUE, dom = 'tip'),
              rownames = FALSE) %>%
      formatStyle(names(filtered_admission()), backgroundColor = 'white')
  })
  
  output$diagnosis_preview <- renderDT({
    req(filtered_diag())
    datatable(head(filtered_diag(), 20), 
              options = list(scrollX = TRUE, dom = 'tip'),
              rownames = FALSE) %>%
      formatStyle(names(filtered_diag()), backgroundColor = 'white')
  })
  
  # ICD-specific analysis
  output$icd_dist_plot <- renderPlot({
    req(filtered_diag())
    
    icd_stats <- filtered_diag() %>%
      count(diagnosis) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    ggplot(icd_stats, aes(x = diagnosis, y = n, fill = diagnosis)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Percentage, 1), "% (", n, ")")), 
                vjust = -0.5, size = 5) +
                  scale_fill_brewer(palette = "Set1") +
                  labs(title = "PHÂN BỐ CÁC LOẠI THIẾU MÁU DO THIẾU SẮT",
                       x = "Mã ICD10", y = "Số lượng") +
                  theme_minimal(base_size = 14) +
                  theme(legend.position = "none",
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  })
    
    output$icd_stats_table <- renderDT({
      req(combined_data())
      
      stats_by_icd <- combined_data() %>%
        group_by(diagnosis) %>%
        summarise(
          Số_bệnh_nhân = n_distinct(admission_id),
          Tuổi_trung_bình = round(mean(age_years, na.rm = TRUE), 1),
          Tuổi_trung_vị = median(age_years, na.rm = TRUE),
          Tỷ_lệ_nữ = round(mean(Gender == "Nữ", na.rm = TRUE) * 100, 1),
          LOS_trung_bình = round(mean(los, na.rm = TRUE), 1)
        ) %>%
        mutate(Tỷ_lệ_nữ = paste0(Tỷ_lệ_nữ, "%"))
      
          
          datatable(stats_by_icd,
                    options = list(dom = 't', pageLength = 5),
                    rownames = FALSE,
                    colnames = c('Mã bệnh', 'Số bệnh nhân', 'Tuổi TB', 'Tuổi trung vị', 'Tỷ lệ nữ', 'LOS TB')) %>%
            formatStyle(names(stats_by_icd), backgroundColor = 'white')
    })
      
      output$gender_by_icd_plot <- renderPlot({
        req(combined_data())
        
        gender_icd <- combined_data() %>%
          group_by(diagnosis, Gender) %>%
          summarise(n = n_distinct(admission_id)) %>%
          mutate(Percentage = n / sum(n) * 100)
        
        ggplot(gender_icd, aes(x = diagnosis, y = n, fill = Gender)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                    position = position_dodge(width = 0.9), 
                    vjust = -0.5, size = 4) +
                      scale_fill_brewer(palette = "Set2") +
                      labs(title = "PHÂN BỐ GIỚI TÍNH THEO MÃ BỆNH",
                           x = "Mã ICD10", y = "Số lượng") +
                      theme_minimal(base_size = 14) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
      })
        
        output$age_by_icd_plot <- renderPlot({
          req(combined_data())
          
          ggplot(combined_data(), aes(x = diagnosis, y = age_years, fill = diagnosis)) +
            geom_boxplot() +
            stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
            scale_fill_brewer(palette = "Set3") +
            labs(title = "PHÂN BỐ TUỔI THEO MÃ BỆNH",
                 x = "Mã ICD10", y = "Tuổi") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
        })
        
        output$los_by_icd_plot <- renderPlot({
          req(combined_data())
          
          los_icd <- combined_data() %>%
            group_by(diagnosis) %>%
            summarise(mean_los = mean(los, na.rm = TRUE),
                      sd_los = sd(los, na.rm = TRUE))
          
          ggplot(los_icd, aes(x = diagnosis, y = mean_los, fill = diagnosis)) +
            geom_bar(stat = "identity") +
            geom_errorbar(aes(ymin = mean_los - sd_los, ymax = mean_los + sd_los), 
                          width = 0.2) +
            geom_text(aes(label = round(mean_los, 1)), vjust = -0.5, size = 5) +
            scale_fill_brewer(palette = "Set1") +
            labs(title = "THỜI GIAN NẰM VIỆN TRUNG BÌNH THEO MÃ BỆNH",
                 x = "Mã ICD10", y = "Số ngày trung bình") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
        })
        
        # Demographics plots
        output$gender_plot <- renderPlot({
          gender_stats <- combined_data() %>%
            distinct(admission_id, .keep_all = TRUE) %>%
            count(Gender) %>%
            mutate(Percentage = n / sum(n) * 100)
          
          ggplot(gender_stats, aes(x = Gender, y = n, fill = Gender)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                      vjust = -0.5, size = 5) +
            scale_fill_brewer(palette = "Set2") +
            labs(title = "PHÂN BỐ GIỚI TÍNH",
                 x = "Giới tính", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
          
        output$age_plot <- renderPlot({
          unique_patients <- combined_data() %>%
            distinct(admission_id, .keep_all = TRUE)
          
          ggplot(unique_patients, aes(x = age_years)) +
            geom_histogram(binwidth = 5, fill = "#3498db", color = "white", alpha = 0.8) +
            geom_vline(xintercept = c(10, 20, 30, 40, 50, 60), 
                       linetype = "dashed", color = "#e74c3c", size = 0.7) +
            labs(title = "PHÂN BỐ ĐỘ TUỔI",
                 x = "Tuổi (năm)", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
          
        output$age_group_plot <- renderPlot({
          age_group_stats <- combined_data() %>%
            distinct(admission_id, .keep_all = TRUE) %>%
            count(age_group) %>%
            mutate(Percentage = n / sum(n) * 100)
          
          ggplot(age_group_stats, aes(x = age_group, y = n, fill = age_group)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                      vjust = -0.5, size = 5) +
            scale_fill_brewer(palette = "Set3") +
            labs(title = "PHÂN BỐ THEO NHÓM TUỔI",
                 x = "Nhóm tuổi", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
            
        output$admsource_plot <- renderPlot({
          admsource_stats <- combined_data() %>%
            distinct(admission_id, .keep_all = TRUE) %>%
            count(admsource) %>%
            mutate(Percentage = n / sum(n) * 100)
          
          ggplot(admsource_stats, aes(x = admsource, y = n, fill = admsource)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                      vjust = -0.5, size = 5) +
            scale_fill_brewer(palette = "Pastel1") +
            labs(title = "NGUỒN NHẬP VIỆN",
                 x = "Nguồn nhập viện", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
              
              # Diagnosis plots
        output$icd_plot <- renderPlot({
          icd_stats <- filtered_diag() %>%
            count(diagnosis) %>%
            mutate(Percentage = n / sum(n) * 100)
          
          ggplot(icd_stats, aes(x = diagnosis, y = n, fill = diagnosis)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                      vjust = -0.5, size = 5) +
            scale_fill_brewer(palette = "Set1") +
            labs(title = "PHÂN LOẠI CHẨN ĐOÁN",
                 x = "Mã ICD10", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
                
        output$position_plot <- renderPlot({
          position_stats <- filtered_diag() %>%
            count(position) %>%
            mutate(Percentage = n / sum(n) * 100)
          
          ggplot(position_stats, aes(x = factor(position), y = n)) +
            geom_bar(stat = "identity", fill = "#3498db") +
            geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                      vjust = -0.5, size = 5) +
            labs(title = "VỊ TRÍ CHẨN ĐOÁN",
                 x = "Vị trí", y = "Số lượng") +
            theme_minimal(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14))
        })
        
                  
                  output$comorbidity_plot <- renderPlot({
                    # Get all diagnoses for our patients
                    all_diag_for_patients <- filtered_diag() %>%
                      filter(admission_id %in% unique(filtered_admission()$admission_id))
                    
                    # Remove the main diagnoses (D500, D508, D509)
                    comorbidity_data <- all_diag_for_patients %>%
                      filter(!diagnosis %in% c("D500", "D508", "D509")) %>%
                      count(diagnosis) %>%
                      arrange(desc(n)) %>%
                      head(20)
                    
                    if (nrow(comorbidity_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, 
                                        label = "Không có dữ liệu bệnh kèm theo để hiển thị", size = 6) + 
                               theme_void())
                    }
                    
                    ggplot(comorbidity_data, aes(x = reorder(diagnosis, n), y = n)) +
                      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
                      coord_flip() +
                      geom_text(aes(label = n), hjust = -0.3, size = 4) +
                      labs(title = "TOP 20 BỆNH KÈM THEO",
                           x = "Mã chẩn đoán", 
                           y = "Số lượng") +
                      theme_minimal(base_size = 14) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                        axis.text = element_text(size = 12),
                        axis.title = element_text(size = 14),
                        panel.grid.major.y = element_blank()
                      ) +
                      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
                  })
                  
                  # Hospital Stay plots
                  output$los_hist <- renderPlot({
                    unique_patients <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE)
                    
                    if(nrow(unique_patients) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "Không có dữ liệu thời gian nằm viện", size = 6) + 
                               theme_void())
                    }
                    
                    mean_los <- mean(unique_patients$los, na.rm = TRUE)
                    
                    ggplot(unique_patients, aes(x = los)) +
                      geom_histogram(binwidth = 1, fill = "#3498db", color = "white", alpha = 0.8) +
                      geom_vline(xintercept = mean_los, color = "#e74c3c", linetype = "dashed", size = 1.2) +
                      geom_text(aes(x = mean_los + 2, y = 5, 
                                    label = paste("Trung bình:", round(mean_los, 1), "ngày")), 
                                color = "#e74c3c", size = 5) +
                      labs(title = "PHÂN BỐ THỜI GIAN NẰM VIỆN",
                           x = "Số ngày", 
                           y = "Số bệnh nhân") +
                      theme_minimal(base_size = 14) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                        axis.text = element_text(size = 12),
                        axis.title = element_text(size = 14)
                      )
                  })
                  
                  output$los_gender <- renderPlot({
                    unique_patients <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE)
                    
                    ggplot(unique_patients, aes(x = Gender, y = los, fill = Gender)) +
                      geom_boxplot(alpha = 0.7) +
                      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#e74c3c") +
                      scale_fill_brewer(palette = "Set2") +
                      labs(title = "THỜI GIAN NV THEO GIỚI TÍNH",
                           x = "Giới tính", y = "Số ngày") +
                      theme_minimal(base_size = 14) +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                            axis.text = element_text(size = 12),
                            axis.title = element_text(size = 14))
                  })
                  
                  output$los_age <- renderPlot({
                    los_by_age <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE) %>%
                      group_by(age_group) %>%
                      summarise(Mean_LOS = round(mean(los, na.rm = TRUE), 2))
                    
                    ggplot(los_by_age, aes(x = age_group, y = Mean_LOS, fill = age_group)) +
                      geom_bar(stat = "identity") +
                      geom_text(aes(label = Mean_LOS), vjust = -0.5, size = 5) +
                      scale_fill_brewer(palette = "Set3") +
                      labs(title = "THỜI GIAN NV TRUNG BÌNH THEO NHÓM TUỔI",
                           x = "Nhóm tuổi", y = "Số ngày trung bình") +
                      theme_minimal(base_size = 14) +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                            axis.text = element_text(size = 12),
                            axis.title = element_text(size = 14))
                  })
                  
                  output$los_age_gender <- renderPlot({
                    los_by_age_gender <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE) %>%
                      group_by(age_group, Gender) %>%
                      summarise(Mean_LOS = round(mean(los, na.rm = TRUE), 2))
                    
                    ggplot(los_by_age_gender, aes(x = age_group, y = Mean_LOS, fill = Gender)) +
                      geom_bar(stat = "identity", position = position_dodge(0.9)) +
                      geom_text(aes(label = Mean_LOS), 
                                position = position_dodge(width = 0.9), 
                                vjust = -0.5, size = 4) +
                      scale_fill_brewer(palette = "Set2") +
                      labs(title = "THỜI GIAN NV THEO TUỔI & GIỚI TÍNH",
                           x = "Nhóm tuổi", y = "Số ngày trung bình") +
                      theme_minimal(base_size = 14) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                            axis.text = element_text(size = 12),
                            axis.title = element_text(size = 14),
                            legend.title = element_text(size = 12),
                            legend.text = element_text(size = 12))
                  })
                  
                  output$ttest_result <- renderPrint({
                    unique_patients <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE)
                    
                    male_los <- unique_patients %>% filter(Gender == "Nam") %>% pull(los)
                    female_los <- unique_patients %>% filter(Gender == "Nữ") %>% pull(los)
                    
                    cat("KẾT QUẢ KIỂM ĐỊNH T-TEST\n")
                    cat("=======================\n\n")
                    
                    if (length(male_los) > 1 & length(female_los) > 1) {
                      ttest_result <- t.test(male_los, female_los)
                      print(ttest_result)
                      
                      cat("\n\nKẾT LUẬN:\n")
                      if (ttest_result$p.value < 0.05) {
                        cat("Có sự khác biệt có ý nghĩa thống kê về thời gian nằm viện giữa nam và nữ.")
                      } else {
                        cat("Không có sự khác biệt có ý nghĩa thống kê về thời gian nằm viện giữa nam và nữ.")
                      }
                    } else {
                      cat("Không đủ dữ liệu để kiểm định t-test.")
                    }
                  })
                  
                  # Advanced analysis plots
                  output$corr_plot <- renderPlot({
                    corr_data <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE) %>%
                      select(age_years, los, weight) %>%
                      mutate(weight = na_if(weight, 9999)) %>%
                      select(where(~ !all(is.na(.))))
                    
                    if (ncol(corr_data) < 2) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, 
                                        label = "Không đủ dữ liệu để tạo ma trận tương quan", size = 6) + 
                               theme_void())
                    }
                    
                    cor_matrix <- cor(corr_data, use = "complete.obs")
                    
                    corrplot(cor_matrix,
                             method = "color",
                             type = "upper",
                             tl.col = "black",
                             tl.srt = 45,
                             addCoef.col = "black",
                             number.cex = 0.8,
                             title = "MA TRẬN TƯƠNG QUAN",
                             mar = c(0, 0, 1, 0))
                  })
                  
                  output$risk_plot <- renderPlot({
                    risk_stats <- combined_data() %>%
                      distinct(admission_id, .keep_all = TRUE) %>%
                      count(high_risk) %>%
                      mutate(Percentage = n / sum(n) * 100)
                    
                    ggplot(risk_stats, aes(x = high_risk, y = n, fill = high_risk)) +
                      geom_bar(stat = "identity") +
                      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                                vjust = -0.5, size = 6) +
                      scale_fill_brewer(palette = "Set1") +
                      labs(title = "PHÂN BỐ NHÓM NGUY CƠ",
                           x = "Nhóm nguy cơ", y = "Số lượng") +
                      theme_minimal(base_size = 14) +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                            axis.text = element_text(size = 12),
                            axis.title = element_text(size = 14))
                  })
                  
                    
                    output$prediction_plot <- renderPlot({
                      model_data <- combined_data() %>%
                        distinct(admission_id, .keep_all = TRUE) %>%
                        select(age_years, Gender, los, admsource, admtype) %>%
                        mutate_if(is.character, as.factor)
                      
                      model_data <- na.omit(model_data)
                      
                      if (nrow(model_data) == 0) return()
                      
                      set.seed(123)
                      train_index <- createDataPartition(model_data$los, p = 0.8, list = FALSE)
                      train_data <- model_data[train_index, ]
                      test_data <- model_data[-train_index, ]
                      
                      model <- train(
                        los ~ .,
                        data = train_data,
                        method = "lm",
                        trControl = trainControl(method = "cv", number = 5)
                      )
                      
                      predictions <- predict(model, newdata = test_data)
                      eval_df <- data.frame(
                        Actual = test_data$los,
                        Predicted = predictions
                      )
                      
                      ggplot(eval_df, aes(x = Actual, y = Predicted)) +
                        geom_point(color = "#3498db", alpha = 0.6, size = 3) +
                        geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linetype = "dashed", size = 1.2) +
                        labs(title = "MÔ HÌNH DỰ ĐOÁN THỜI GIAN NẰM VIỆN",
                             x = "Giá trị thực tế (ngày)",
                             y = "Giá trị dự đoán (ngày)") +
                        theme_minimal(base_size = 14) +
                        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                              axis.text = element_text(size = 12),
                              axis.title = element_text(size = 14))
                    })
                    
                    output$model_summary <- renderPrint({
                      model_data <- combined_data() %>%
                        distinct(admission_id, .keep_all = TRUE) %>%
                        select(age_years, Gender, los, admsource, admtype) %>%
                        mutate_if(is.character, as.factor)
                      
                      model_data <- na.omit(model_data)
                      
                      if (nrow(model_data) == 0) return("Không đủ dữ liệu để xây dựng mô hình")
                      
                      set.seed(123)
                      train_index <- createDataPartition(model_data$los, p = 0.8, list = FALSE)
                      train_data <- model_data[train_index, ]
                      test_data <- model_data[-train_index, ]
                      
                      model <- train(
                        los ~ .,
                        data = train_data,
                        method = "lm",
                        trControl = trainControl(method = "cv", number = 5)
                      )
                      
                      predictions <- predict(model, newdata = test_data)
                      rmse <- sqrt(mean((predictions - test_data$los)^2))
                      
                      cat("ĐÁNH GIÁ MÔ HÌNH DỰ ĐOÁN\n")
                      cat("========================\n\n")
                      cat("Sai số bình phương trung bình (RMSE):", round(rmse, 2), "\n\n")
                      cat("Tóm tắt mô hình:\n")
                      print(summary(model$finalModel))
                    })
}

# Run the application
shinyApp(ui = ui, server = server)