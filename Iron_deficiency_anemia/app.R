#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)
library(caret)
library(DT)
library(ggpubr)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = span(icon("tint"), "PHÂN TÍCH THIẾU MÁU DO THIẾU SẮT"), 
    titleWidth = 450
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Tải lên dữ liệu", tabName = "upload", icon = icon("upload")),
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
          background-color: #1976D2 !important;  /* Màu xanh dương đậm */
          color: white !important;              /* Chữ màu trắng */
        }
        
        /* Style cho nút chọn file */
        .btn-default {
          color: #333;
          background-color: #fff;
          border-color: #ccc;
        }
        
       
        .shiny-input-container {
          color: #333; /* Màu chữ đen cho dễ đọc */
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
      # Tab tải lên dữ liệu
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "TẢI LÊN DỮ LIỆU", 
            width = 12, 
            status = "primary", 
            solidHeader = TRUE,  # Quan trọng: tạo thanh màu đậm
            column(6,
                   fileInput("admission_file", "Tải lên dữ liệu nhập viện (Excel)",
                             accept = c(".xlsx", ".xls"),
                             buttonLabel = "Chọn file...",
                             placeholder = "Chưa chọn file")
            ),
            column(6,
                   fileInput("diagnosis_file", "Tải lên dữ liệu chẩn đoán (Excel)",
                             accept = c(".xlsx", ".xls"),
                             buttonLabel = "Chọn file...",
                             placeholder = "Chưa chọn file")
            ),
            actionButton("load_sample", "Dùng dữ liệu mẫu", 
                         icon = icon("file-import"),
                         class = "btn-success")
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
        ),
        fluidRow(
          box(
            title = "Tóm tắt dữ liệu đã lọc", width = 12, status = "info", solidHeader = TRUE,
            verbatimTextOutput("filter_summary")
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
  # Reactive values to store data
  rv <- reactiveValues(
    admission_df = NULL,
    diagnosis_df = NULL,
    filtered_admission = NULL,
    filtered_diag = NULL,
    matching_ids = NULL,
    all_diag_for_patients = NULL,
    comorbidity_data = NULL
  )
  
  # Load sample data
  observeEvent(input$load_sample, {
    tryCatch({
      # Đọc dữ liệu mẫu từ package
      path <- system.file("extdata", package = "readxl")
      rv$admission_df <- read_excel(paste0(path, "/admission.xlsx"))
      rv$diagnosis_df <- read_excel(paste0(path, "/diagnosis.xlsx"))
      process_data()
      showNotification("Đã tải dữ liệu mẫu thành công!", type = "message")
    }, error = function(e) {
      showNotification(paste("Lỗi khi tải dữ liệu mẫu:", e$message), type = "error")
    })
  })
  
  # Handle file uploads
  observe({
    req(input$admission_file)
    tryCatch({
      rv$admission_df <- read_excel(input$admission_file$datapath)
      showNotification("Đã tải dữ liệu nhập viện thành công!", type = "message")
    }, error = function(e) {
      showNotification(paste("Lỗi khi tải dữ liệu nhập viện:", e$message), type = "error")
    })
  })
  
  observe({
    req(input$diagnosis_file)
    tryCatch({
      rv$diagnosis_df <- read_excel(input$diagnosis_file$datapath)
      showNotification("Đã tải dữ liệu chẩn đoán thành công!", type = "message")
    }, error = function(e) {
      showNotification(paste("Lỗi khi tải dữ liệu chẩn đoán:", e$message), type = "error")
    })
  })
  
  # Process data when files are uploaded
  observe({
    req(rv$admission_df, rv$diagnosis_df)
    process_data()
  })
  
  # Data processing function - SỬA LẠI PHẦN NÀY QUAN TRỌNG
  process_data <- function() {
    tryCatch({
      target_icds <- c("D500", "D508", "D509")
      
      # Lọc chẩn đoán thiếu máu
      rv$filtered_diag <- rv$diagnosis_df %>% 
        filter(diagnosis %in% target_icds)
      
      # Lấy danh sách ID bệnh nhân thiếu máu
      rv$matching_ids <- unique(rv$filtered_diag$admission_id)
      
      # Lấy tất cả chẩn đoán của bệnh nhân thiếu máu (cho bệnh kèm theo)
      rv$all_diag_for_patients <- rv$diagnosis_df %>%
        filter(admission_id %in% rv$matching_ids)
      
      # Chuẩn bị dữ liệu bệnh kèm theo
      rv$comorbidity_data <- rv$all_diag_for_patients %>%
        filter(!diagnosis %in% target_icds) %>%
        count(diagnosis) %>%
        arrange(desc(n)) %>%
        head(20)
      
      # Lọc và xử lý dữ liệu nhập viện
      rv$filtered_admission <- rv$admission_df %>% 
        filter(admission_id %in% rv$matching_ids) %>%
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
          admsource = case_when(
            admsource == "H" ~ "Tự đến",
            admsource == "T" ~ "Chuyển viện",
            admsource == "N" ~ "Cấp cứu",
            admsource == "S" ~ "Phẫu thuật",
            TRUE ~ as.character(admsource)
          ),
          admtype = case_when(
            admtype == "C" ~ "Nội trú thường",
            admtype == "L" ~ "Nội trú dài ngày",
            admtype == "O" ~ "Ngoại trú",
            admtype == "X" ~ "Khác",
            TRUE ~ as.character(admtype)
          ),
          high_risk = ifelse(age_years > 65 | los > 7, "Cao", "Thấp")
        )
      
      showNotification("✅ Xử lý dữ liệu thành công!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Lỗi khi xử lý dữ liệu:", e$message), type = "error")
    })
  }
  
  
  # Data preview outputs
  output$admission_preview <- renderDT({
    req(rv$admission_df)
    datatable(head(rv$admission_df, 20), 
              options = list(scrollX = TRUE, dom = 'tip'),
              rownames = FALSE) %>%
      formatStyle(names(rv$admission_df), backgroundColor = 'white')
  })
  
  output$diagnosis_preview <- renderDT({
    req(rv$diagnosis_df)
    datatable(head(rv$diagnosis_df, 20), 
              options = list(scrollX = TRUE, dom = 'tip'),
              rownames = FALSE) %>%
      formatStyle(names(rv$diagnosis_df), backgroundColor = 'white')
  })
  
  output$filter_summary <- renderPrint({
    req(rv$filtered_admission)
    req(rv$filtered_diag)
    
    cat("TÓM TẮT DỮ LIỆU ĐÃ LỌC\n")
    cat("=======================\n\n")
    cat("Số bệnh nhân:", nrow(rv$filtered_admission), "\n")
    cat("Số chẩn đoán:", nrow(rv$filtered_diag), "\n")
    cat("\nPhân bố loại chẩn đoán:\n")
    print(table(rv$filtered_diag$diagnosis))
  })
  
  # Demographics plots
  output$gender_plot <- renderPlot({
    req(rv$filtered_admission)
    
    gender_stats <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    ggplot(rv$filtered_admission, aes(x = age_years)) +
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
    req(rv$filtered_admission)
    
    age_group_stats <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    admsource_stats <- rv$filtered_admission %>%
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
    req(rv$filtered_diag)
    
    icd_stats <- rv$filtered_diag %>%
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
    req(rv$filtered_diag)
    
    position_stats <- rv$filtered_diag %>%
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
    req(rv$comorbidity_data)
    
    if (nrow(rv$comorbidity_data) == 0) {
      # Hiển thị thông báo nếu không có dữ liệu
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Không có dữ liệu bệnh kèm theo để hiển thị", size = 6) + 
               theme_void())
    }
    
    ggplot(rv$comorbidity_data, aes(x = reorder(diagnosis, n), y = n)) +
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
  
  
  # Hospital Stay plots - THÊM KIỂM TRA DỮ LIỆU
  output$los_hist <- renderPlot({
    req(rv$filtered_admission)
    
    if(nrow(rv$filtered_admission) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Không có dữ liệu thời gian nằm viện", size = 6) + 
               theme_void())
    }
    
    mean_los <- mean(rv$filtered_admission$los, na.rm = TRUE)
    
    ggplot(rv$filtered_admission, aes(x = los)) +
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
    req(rv$filtered_admission)
    
    ggplot(rv$filtered_admission, aes(x = Gender, y = los, fill = Gender)) +
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
    req(rv$filtered_admission)
    
    los_by_age <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    los_by_age_gender <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    male_los <- rv$filtered_admission %>% filter(Gender == "Nam") %>% pull(los)
    female_los <- rv$filtered_admission %>% filter(Gender == "Nữ") %>% pull(los)
    
    ttest_result <- t.test(male_los, female_los)
    
    cat("KẾT QUẢ KIỂM ĐỊNH T-TEST\n")
    cat("=======================\n\n")
    print(ttest_result)
    
    cat("\n\nKẾT LUẬN:\n")
    if (ttest_result$p.value < 0.05) {
      cat("Có sự khác biệt có ý nghĩa thống kê về thời gian nằm viện giữa nam và nữ.")
    } else {
      cat("Không có sự khác biệt có ý nghĩa thống kê về thời gian nằm viện giữa nam và nữ.")
    }
  })
  
  # Advanced analysis plots
  output$corr_plot <- renderPlot({
    req(rv$filtered_admission)
    
    corr_data <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    risk_stats <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    model_data <- rv$filtered_admission %>%
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
    req(rv$filtered_admission)
    
    model_data <- rv$filtered_admission %>%
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