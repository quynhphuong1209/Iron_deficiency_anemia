# 🩸 Bảng điều khiển Thiếu máu do thiếu sắt (Iron Deficiency Anemia)

Dự án này cung cấp **bảng điều khiển dữ liệu tương tác** nhằm phân tích bệnh **thiếu máu do thiếu sắt (IDA)** – một dạng thiếu máu phổ biến do cơ thể thiếu sắt. Ứng dụng được xây dựng bằng **R Shiny** để trực quan hóa cả dữ liệu **toàn cầu (WHO)** và **dữ liệu lâm sàng tại Việt Nam**.

---

## 🌐 Liên kết Dashboard trực tuyến

- **🌍 Bảng điều khiển dữ liệu toàn cầu WHO:**  
  👉 [https://quynhphuong.shinyapps.io/IDA_WHO/](https://quynhphuong.shinyapps.io/IDA_WHO/)  
  → Hiển thị dữ liệu thiếu máu do thiếu sắt trên toàn cầu từ WHO, phân tích theo giới tính, độ tuổi và vùng địa lý.

- **🇻🇳 Bảng điều khiển dữ liệu bệnh viện tại Việt Nam:**  
  👉 [https://quynhphuong.shinyapps.io/Iron_deficiency_anemia/](https://quynhphuong.shinyapps.io/Iron_deficiency_anemia/)  
  → Phân tích dữ liệu lâm sàng chi tiết tại Việt Nam bao gồm nhân khẩu học, bệnh đi kèm và mô hình học máy.

---

## 📁 Cấu trúc thư mục dự án
```plaintext
Iron_deficiency_anemia/
├── app.R # File chính chạy ứng dụng Shiny
├── data/ # Thư mục chứa dữ liệu (.xlsx, .csv)
│ ├── admission.xlsx
│ ├── diagnosis.xlsx
│ └── ICD10.xlsx
├── www/ # Thư mục chứa logo, CSS tùy chỉnh
├── IDA/ # Bản Shiny phụ / thư mục triển khai
├── Imagine in R/ # Notebook minh họa xử lý dữ liệu bằng R
├── Imagine in Python/ # Notebook minh họa xử lý bằng Python
├── Imagine web IDA WHO/ # Hình ảnh minh họa giao diện WHO
├── Imagine web Iron deficiency anemia/ # Hình ảnh minh họa giao diện VN
├── Nhóm 5_D50.Rmd # File markdown dùng trong Shiny
├── Mã_hoá_bệnh_tật_D50__Thiếu_máu_do_thiếu_sắt.ipynb # Notebook mã hoá ICD
├── README.md # File mô tả này
├── các file báo cáo nhóm: .docx, .pdf
```

---

## 📊 Tính năng nổi bật

### 1. Bảng điều khiển WHO – Dữ liệu toàn cầu
- Bản đồ tương tác thể hiện gánh nặng thiếu máu theo từng quốc gia
- So sánh theo giới tính, nhóm tuổi, khu vực WHO
- Đánh giá các chỉ số mục tiêu WHO, xếp hạng quốc gia

### 2. Bảng điều khiển Việt Nam – Dữ liệu bệnh viện
- Tổng quan nhân khẩu học: độ tuổi, giới tính, thời gian nằm viện
- Top 20 bệnh đi kèm phổ biến liên quan đến thiếu máu (ICD-10)
- Phân tích thống kê: T-test, ANOVA, ma trận tương quan
- Mô hình học máy dự đoán bệnh nhân có nguy cơ nằm viện dài ngày

---

## 🧰 Công nghệ sử dụng

- **Ngôn ngữ:** R, R Markdown
- **Giao diện:** R Shiny, ShinyDashboard
- **Trực quan hóa:** ggplot2, plotly
- **Xử lý dữ liệu:** readxl, dplyr, tidyr
- **Phân tích:** caret, randomForest, corrplot

---

## ▶️ Hướng dẫn chạy ứng dụng cục bộ (local)

### 1. Tải về dự án:
```bash
git clone https://github.com/quynhphuong1209/Iron_deficiency_anemia.git
```
### 2.  Mở file app.R bằng RStudio
### 3. Cài đặt các gói cần thiết (nếu chưa có):
```{r}
install.packages(c("shiny", "shinydashboard", "ggplot2", "readxl", "dplyr",
                   "tidyr", "plotly", "corrplot", "caret", "randomForest"))
```
### 4. Chạy ứng dụng:
```{r}
shiny::runApp("app.R")
```
# 👩‍💻 Tác giả
- Đinh Lê Quỳnh Phương
- Sinh viên ngành Khoa học Dữ liệu – Trường Đại học Y tế Công cộng
- 📎 GitHub: @quynhphuong1209
