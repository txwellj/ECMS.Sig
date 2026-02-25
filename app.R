# -------------------------- 1. 全局配置 & 依赖加载 -------------------------- 
options(shiny.maxRequestSize = 1000*1024^2)  # 最大文件上传限制1000MB 

# 自动安装并加载依赖包 
required_packages <- c("shiny", "shinythemes", "ggplot2", "survival", "survminer", 
                       "timeROC", "ggsci", "DT", "openxlsx", "factoextra") 
for(pkg in required_packages){ 
  if(!require(pkg, character.only = TRUE)) { 
    install.packages(pkg, dependencies = TRUE) 
    library(pkg, character.only = TRUE) 
  } 
} 

# 自定义CSS样式---- 
custom_css <- " 
#home_text { 
  font-size: 18px; 
  font-family: Arial, sans-serif; 
  text-align: center; 
  margin-top: 20px; 
} 
#home_logo { 
  display: flex; 
  justify-content: center; 
  margin-top: 30px; 
} 
#plot_container { 
  border: 2px solid #ccc; /* 边框颜色 */ 
  padding: 15px;         /* 内边距 */ 
  margin-bottom: 20px;   /* 底部外边距 */ 
  border-radius: 5px;    /* 圆角边框 */ 
  background-color: #f9f9f9; /* 背景颜色 */ 
} 
#divider { 
  border-top: 1px solid #ccc; /* 浅灰色分割线 */ 
  margin-top: 20px; 
  margin-bottom: 20px; 
} 
.tab-content { 
  padding-top: 15px; 
} 
.notification { 
  font-size: 14px; 
} 
" 

# -------------------------- 2. 全局变量定义 -------------------------- 
# Kla.Sig 风险评分基因集 
rs_gene_var <- c("LAMA3","FGFR2","HMMR","TMPRSS11E","COL5A1","TSPAN7","LAD1","COL4A3","PECAM1","GDF10") 
# 无监督聚类基因集 
cluster_gene_var <- c("CEACAM21", "LAMA3" , "COL11A1" , "FGFR2" , "HMMR", "ITGA8" , "FCN1", "TMPRSS11E" ,"CTSG","SGCG","COL1A1","COL5A1","POSTN","ADAMTS8","GPC3","ADAM12","ABI3BP" ,  
                      "TSPAN7","LAD1","CPA3", "CLEC3B","CTHRC1","MFAP4","GREM1","KLK11","COL3A1","COL4A3","SLIT3","MMP1","COL5A2","COL6A6","PECAM1","ANXA8","GDF10" ) 

# -------------------------- 3. UI 界面 -------------------------- 
ui <- navbarPage( 
  title = "LESig", 
  theme = shinytheme("spacelab"), 
  tags$head(tags$style(HTML(custom_css))),  # 添加自定义CSS样式 
  
  # Model页 
  tabPanel( 
    "Model",  
    
    sidebarPanel( 
      width = 4,  
      tabsetPanel( 
        id = "sidebar_tabs", 
        # ✅ 必须用tabPanel作为tabsetPanel的直接子元素，包裹所有内容 
        tabPanel( 
          title = "Data & Analysis",  # 侧边栏标签页标题 
          h4("Data Selection"), 
          selectInput("data_source", "Data Source:", choices = c("Use Provided Datasets", "Upload Your File","Unsupervised clustering")), 
          
          conditionalPanel( 
            condition = "input.data_source == 'Use Provided Datasets'", 
            selectInput("dataset", "Choose Dataset:", choices = c("TCGA-LUAD", "GSE14814", "GSE29016", "GSE30219", "GSE31210", 
                                                                  "GSE37745",  "GSE50081", "GSE68465", "GSE72094")) 
          ), 
          
          conditionalPanel( 
            condition = "input.data_source == 'Upload Your File'", 
            fileInput("upload_file", "Upload CSV File:", accept = c(".csv")), 
            helpText("Upload a CSV file with: <br>1. Column 1: Sample_ID <br>2. Column 2: status (1=event, 0=censor) <br>3. Column 3: time (survival time) <br>4. Columns 4+: Gene expression values") 
          ), 
          
          conditionalPanel( 
            condition = "input.data_source == 'Unsupervised clustering'", 
            fileInput( 
              inputId = "upload_files", 
              label = "Upload Two CSV Files:", 
              accept = c(".csv"), 
              multiple = TRUE 
            ), 
            helpText( 
              "1. First file: Training cluster data <br> • Rows: Samples, Columns: cluster + all ECM.Sig genes <br>", 
              "2. Second file: New sample data <br> • Rows: Samples, Columns: all ECM.Sig genes" 
            ) 
          ), 
          
          # ✅ 确保按钮在所有场景下可见，避免被条件面板意外隐藏 
          actionButton("run_fit", "Run Analysis", class = "btn-primary btn-block"), 
          
          tags$hr(style = "border-top: 2px solid #ccc;"), 
          
          h4("Download Results"), 
          conditionalPanel( 
            condition = "input.data_source != 'Unsupervised clustering'", 
            radioButtons("plot_format", "Image Format:", choices = c("png", "pdf"), selected = "png"), 
            downloadButton("download_km_plot", "Download K-M Plot", class = "btn-block"), 
            downloadButton("download_roc_plot", "Download ROC Curve", class = "btn-block") 
          ), 
          conditionalPanel( 
            condition = "input.data_source == 'Unsupervised clustering'", 
            downloadButton("download_pca_plot", "Download PCA Plot", class = "btn-block") 
          ), 
          downloadButton("download_table", "Download Result Table", class = "btn-block") 
        ) 
      ) 
    ) ,
    
    mainPanel( 
      width = 8,  
      
      # 标签页部分 
      tabsetPanel( 
        id = "main_tabs", 
        
        # 风险评分模块专属标签页 
        conditionalPanel( 
          condition = "input.data_source != 'Unsupervised clustering'", 
          tabPanel( 
            "K-M Plot", 
            fluidRow( 
              column(12, 
                     div(id = "plot_container", plotOutput("km_plot", height = "500px")) 
              ) 
            ) 
          ), 
          
          tabPanel( 
            "ROC Curve", 
            fluidRow( 
              column(12, 
                     div(id = "plot_container", plotOutput("roc_plot", height = "500px")) 
              ) 
            ) 
          ) 
        ), 
        
        # 无监督聚类模块专属标签页 
        conditionalPanel( 
          condition = "input.data_source == 'Unsupervised clustering'", 
          tabPanel( 
            "PCA Plot", 
            fluidRow( 
              column(12, 
                     div(id = "plot_container", plotOutput("pca_plot", height = "600px")) 
              ) 
            ), 
            fluidRow( 
              column(12, 
                     div(id = "plot_container", verbatimTextOutput("cluster_result")) 
              ) 
            ) 
          ) 
        ), 
        
        # 通用结果表格标签页 
        tabPanel( 
          "Result Table", 
          fluidRow( 
            column(12, 
                   div(id = "plot_container", DTOutput("dataset_table")) 
            ) 
          ) 
        ) 
      ) 
    ) 
  ) 
) 

# -------------------------- 4. Server 核心逻辑 -------------------------- 
server <- function(input, output, session) { 
  # 反应式变量存储分析结果 
  rs_result <- reactiveVal(NULL)  # 风险评分结果：$data(表格), $kmfit(生存分析模型), $roc_list(ROC数据) 
  cluster_result <- reactiveVal(list(plot=NULL, text=NULL, table=NULL))  # 聚类结果 
  
  # -------------------------- 模块1：风险评分分析 -------------------------- 
  # 1.1 加载&预处理数据 
  load_rs_data <- reactive({ 
    req(input$data_source != "Unsupervised clustering") 
    
    if (input$data_source == "Use Provided Datasets") { 
      req(input$dataset) 
      # 加载内置数据集（请确保项目根目录下data文件夹存在对应RDS文件） 
      tryCatch({ 
        data <- readRDS(file.path("data", paste0(gsub("-", "_", input$dataset), ".rds"))) 
      }, error = function(e) { 
        showNotification(paste("Failed to load built-in dataset:", e$message), type = "error", duration = 10) 
        return(NULL) 
      }) 
    } else { 
      req(input$upload_file) 
      # 加载用户上传的文件 
      tryCatch({ 
        data <- read.csv(input$upload_file$datapath, check.names = FALSE, stringsAsFactors = FALSE) 
        # 校验前3列是否为必填字段 
        if (!all(c("Sample_ID", "status", "time") %in% colnames(data)[1:3])) { 
          stop("The first 3 columns of the uploaded file must be Sample_ID, status, time") 
        } 
        # 重命名前3列确保统一 
        colnames(data)[1:3] <- c("Sample_ID", "status", "time") 
      }, error = function(e) { 
        showNotification(paste("Failed to read uploaded file:", e$message), type = "error", duration = 10) 
        return(NULL) 
      }) 
    } 
    
    # 校验基因集是否完整 
    missing_genes <- setdiff(rs_gene_var, colnames(data)) 
    if (length(missing_genes) > 0) { 
      showNotification(paste("Missing risk score genes in data:", paste(missing_genes, collapse = ", ")), type = "error", duration = 10) 
      return(NULL) 
    } 
    
    # 标准化基因表达数据 
    data[, rs_gene_var] <- scale(data[, rs_gene_var]) 
    return(data) 
  }) 
  
  # 1.2 计算风险评分 & 生存分析 
  observeEvent(input$run_fit, { 
    req(input$data_source != "Unsupervised clustering") 
    showNotification("Starting risk score analysis...", type = "message", duration = 2) 
    
    data <- load_rs_data() 
    req(data) 
    
    tryCatch({ 
      # 计算风险评分 
      risk_score <- with(data, 
                         LAMA3*3.0212304 - FGFR2*2.5763831 + HMMR*3.3921564 + TMPRSS11E*0.6052692 + 
                           COL5A1*7.2609516 - TSPAN7*1.0795986 + LAD1*2.1925162 - COL4A3*1.5857519 - 
                           PECAM1*10.6190902 - GDF10*1.4482857 
      ) 
      risk_group <- ifelse(risk_score > median(risk_score), "High-Risk", "Low-Risk") 
      
      # 整理结果表格 
      rs_data <- data.frame( 
        Sample_ID = data$Sample_ID, 
        time = as.numeric(data$time), 
        status = as.integer(data$status), 
        RiskScore = round(risk_score, 4), 
        RiskGroup = risk_group, 
        stringsAsFactors = FALSE 
      ) 
      
      # 生存分析模型 
      kmfit <- survfit(Surv(time = time, event = status) ~ RiskGroup, data = rs_data) 
      
      # ROC曲线数据 
      roc_list <- list( 
        year1 = timeROC(T = rs_data$time, delta = rs_data$status, marker = rs_data$RiskScore, cause = 1, times = 1), 
        year3 = timeROC(T = rs_data$time, delta = rs_data$status, marker = rs_data$RiskScore, cause = 1, times = 3), 
        year5 = timeROC(T = rs_data$time, delta = rs_data$status, marker = rs_data$RiskScore, cause = 1, times = 5) 
      ) 
      
      # 存储结果 
      rs_result(list(data = rs_data, kmfit = kmfit, roc_list = roc_list)) 
      showNotification("Risk score analysis completed!", type = "success", duration = 3) 
    }, error = function(e) { 
      showNotification(paste("Analysis failed:", e$message), type = "error", duration = 10) 
    }) 
  }) 
  
  # 1.3 可视化：K-M生存曲线 
  output$km_plot <- renderPlot({ 
    req(rs_result()$kmfit, rs_result()$data) 
    rs_data <- rs_result()$data 
    kmfit <- rs_result()$kmfit 
    
    # 自定义配色 
    high_color <- pal_npg("nrc")(10)[1] 
    low_color <- pal_npg("nrc")(10)[2] 
    
    ggsurvplot( 
      fit = kmfit, data = rs_data, 
      pval = TRUE, pval.method = TRUE, 
      conf.int = TRUE, risk.table = TRUE, 
      legend.labs = c("High-Risk", "Low-Risk"), 
      legend.title = "Risk Group", 
      risk.table.col = "strata", 
      surv.median.line = "hv", 
      ggtheme = theme_bw(base_size = 12) + theme(panel.grid = element_blank()), 
      palette = c(high_color, low_color) 
    ) 
  }) 
  
  # 1.4 可视化：时间依赖ROC曲线 
  output$roc_plot <- renderPlot({ 
    req(rs_result()$roc_list) 
    roc_list <- rs_result()$roc_list 
    
    # 初始化画布 
    plot(roc_list$year1, time = 1, col = pal_npg("nrc")(10)[1], 
         title = "Time-dependent ROC Curve", lwd = 2, xlim = c(0,1), ylim = c(0,1)) 
    plot(roc_list$year3, time = 3, add = TRUE, col = pal_npg("nrc")(10)[2], lwd = 2) 
    plot(roc_list$year5, time = 5, add = TRUE, col = pal_npg("nrc")(10)[3], lwd = 2) 
    
    # 添加图例 
    legend("bottomright", 
           legend = c( 
             paste0("1-Year AUC: ", sprintf("%.3f", roc_list$year1$AUC[2])), 
             paste0("3-Year AUC: ", sprintf("%.3f", roc_list$year3$AUC[2])), 
             paste0("5-Year AUC: ", sprintf("%.3f", roc_list$year5$AUC[2])) 
           ), 
           col = pal_npg("nrc")(10)[1:3], lty = 1, lwd = 2, bty = "n") 
    # 添加参考线 
    abline(a=0, b=1, lty=2, col="gray50") 
  }) 
  # -------------------------- 模块2：无监督聚类分析 -------------------------- 
  observeEvent(input$run_fit, { 
    if (input$data_source != "Unsupervised clustering") return(NULL) 
    
    req(input$upload_files) 
    if (nrow(input$upload_files) != 2) { 
      showNotification("Please upload exactly two CSV files!", type = "warning", duration = 5) 
      return(NULL) 
    } 
    showNotification("Starting automatic file type recognition...", type = "message", duration = 2) 
    
    tryCatch({ 
      # 1. 读取文件并识别训练集/测试集 
      file_paths <- input$upload_files$datapath 
      file_names <- input$upload_files$name 
      
      file_list <- list() 
      has_cluster <- logical(2) 
      for (i in 1:2) { 
        df <- read.csv( 
          file = file_paths[i], 
          row.names = 1, 
          check.names = FALSE, 
          stringsAsFactors = FALSE, 
          na.strings = c("", "NA", "N/A") 
        ) 
        file_list[[i]] <- df 
        has_cluster[i] <- "cluster" %in% colnames(df) 
      } 
      
      if (sum(has_cluster) == 1) { 
        train_idx <- which(has_cluster)[1] 
        test_idx <- which(!has_cluster)[1] 
        train_data <- file_list[[train_idx]] 
        new_data <- file_list[[test_idx]] 
        
        showNotification( 
          HTML(paste0( 
            "✅ 文件识别完成<br>", 
            "训练集：<code>", file_names[train_idx], "</code><br>", 
            "测试集：<code>", file_names[test_idx], "</code>" 
          )), 
          type = "message", 
          duration = 8 
        ) 
      } else if (sum(has_cluster) == 2) { 
        stop("Both files contain 'cluster' column! Please confirm the test set file (should not have 'cluster' column)") 
      } else { 
        stop("No file contains 'cluster' column! Training set must include 'cluster' column") 
      } 
      
      # 2. 基因列对齐 
      train_gene_cols <- setdiff(colnames(train_data), "cluster") 
      test_gene_cols <- colnames(new_data) 
      common_genes <- intersect(train_gene_cols, test_gene_cols) 
      
      if (length(common_genes) == 0) { 
        stop("No common gene columns between training and test sets, please check file format") 
      } 
      
      train_aligned <- train_data[, c(common_genes, "cluster")] 
      new_aligned <- new_data[, common_genes] 
      
      # 3. 数据标准化 
      scaled_train <- scale(train_aligned[, common_genes]) 
      scaled_new <- t(mapply( 
        function(x, mean_val, sd_val) { 
          if (sd_val == 0) return(x - mean_val) 
          return((x - mean_val) / sd_val) 
        }, 
        new_aligned, 
        attr(scaled_train, "scaled:center"), 
        attr(scaled_train, "scaled:scale") 
      )) 
      
      # 4. 计算簇中心与预测 
      cluster_centers <- aggregate( 
        scaled_train, 
        by = list(cluster = train_aligned$cluster), 
        mean, 
        na.rm = TRUE 
      ) 
      centers_matrix <- as.matrix(cluster_centers[, -1]) 
      rownames(centers_matrix) <- paste0("Cluster_", cluster_centers$cluster) 
      
      euclidean_dist <- function(vec1, vec2) { 
        if (length(vec1) != length(vec2)) { 
          stop("Dimension mismatch for distance calculation") 
        } 
        sqrt(sum((vec1 - vec2)^2, na.rm = TRUE)) 
      } 
      
      new_clusters <- apply(scaled_new, 1, function(sample_vec) { 
        dists <- apply(centers_matrix, 1, function(center_vec) { 
          euclidean_dist(sample_vec, center_vec) 
        }) 
        names(which.min(dists)) 
      }) 
      
      # 5. 生成结果 
      result_table <- data.frame( 
        Sample_ID = rownames(new_aligned), 
        Predicted_Cluster = gsub("Cluster_", "", new_clusters), 
        stringsAsFactors = FALSE 
      ) 
      
      result_text <- paste0( 
        "📊 Clustering Results\n", 
        "-------------------------\n", 
        "Training Sample Count: ", nrow(train_aligned), "\n", 
        "Test Sample Count: ", nrow(new_aligned), "\n", 
        "Gene Count (Used): ", length(common_genes), "\n\n", 
        "New Sample Predictions:\n", 
        paste(paste0(result_table$Sample_ID, " → Cluster ", result_table$Predicted_Cluster), collapse = "\n") 
      ) 
      
      # 6. PCA可视化（带完整校验） 
      combined_scaled <- rbind(scaled_train, scaled_new) 
      
      # 数据有效性校验 
      if (nrow(combined_scaled) == 0) stop("Merged data is empty; visualization cannot be performed.") 
      if (all(apply(combined_scaled, 2, sd) == 0)) stop("All gene expression values have zero variance; PCA dimensionality reduction cannot be performed.") 
      
      # 生成PCA模型（带错误捕获） 
      pca_fit <- tryCatch({ 
        prcomp(combined_scaled, scale. = FALSE, na.action = na.omit) 
      }, error = function(e) { 
        stop(paste("PCA降维失败：", e$message)) 
      }) 
      
      # 校验PCA结果有效性 
      if (ncol(pca_fit$x) < 2) stop("PCA generated fewer than 2 principal components; 2D scatter plot cannot be drawn.") 
      
      # 计算主成分方差解释率 
      var_explained <- round(pca_fit$sdev^2 / sum(pca_fit$sdev^2) * 100, 1) 
      pc1_label <- paste0("PC1 (", var_explained[1], "% variance)") 
      pc2_label <- paste0("PC2 (", var_explained[2], "% variance)") 
      
      # 整理PCA数据框 
      pca_data <- as.data.frame(pca_fit$x[, 1:2]) 
      colnames(pca_data) <- c("PC1", "PC2") 
      pca_data$Sample_Type <- c( 
        rep("Training Sample", nrow(scaled_train)), 
        rep("New Sample", nrow(scaled_new)) 
      ) 
      pca_data$Cluster <- c( 
        as.character(train_aligned$cluster), 
        gsub("Cluster_", "", new_clusters) 
      ) 
      
      # 绘制PCA散点图（确保生成完整的ggplot对象） 
      pca_plot <- tryCatch({
        # 为新样本/EH-P分组创建组合标识，用于图例区分
        pca_data <- pca_data %>%
          mutate(
            Cluster_Label = ifelse(
              Sample_Type == "New Sample", 
              paste0("New - ", Cluster),  # 新样本标注为 "New - EH-P"
              paste0("Training - ", Cluster)  # 训练样本标注为 "Training - ClusterX"
            )
          )
        
        ggplot(pca_data, aes(x = PC1, y = PC2)) +
          
          # 训练样本:半透明圆点,按簇着色
          geom_point(
            data = subset(pca_data, Sample_Type == "Training Sample"),
            aes(color = Cluster_Label, shape = Sample_Type),
            size = 2.5, alpha = 0.3
          ) +
          
          # 新样本:高亮星号,显示样本ID，EH-P分组单独突出
          geom_point(
            data = subset(pca_data, Sample_Type == "New Sample"),
            aes(color = Cluster_Label, shape = Sample_Type),
            size = 4, alpha = 1
          ) +
          geom_text(
            data = subset(pca_data, Sample_Type == "New Sample"),
            aes(label = rownames(subset(pca_data, Sample_Type == "New Sample")), color = Cluster_Label),
            hjust = 1.2, vjust = 0, size = 3, fontface = "bold"
          ) +
          
          # 自定义图例和样式，确保EH-P分组独立显示
          scale_shape_manual(
            name = "Sample Type",
            values = c("Training Sample" = 16, "New Sample" = 8),
            breaks = c("Training Sample", "New Sample")
          ) +
          scale_color_npg(
            name = "Cluster & Sample Type",
            breaks = unique(pca_data$Cluster_Label)
          ) +
          labs(
            title = "PCA Visualization with EH-P New Sample Highlighting",
            x = pc1_label,
            y = pc2_label,
            caption = paste("Total variance explained by PC1+PC2:", sum(var_explained[1:2]), "%")
          ) +
          theme_bw(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.caption = element_text(hjust = 0, size = 9, color = "gray50"),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.just = "center"
          )
      }, error = function(e) {
        stop(paste("PCA plot generation failed:", e$message))
      })
      
      # 调试：打印绘图对象信息（在RStudio控制台查看） 
      message("✅ PPCA Plot Object Generated Successfully") 
      message("   Plot Type：", class(pca_plot)) 
      message("   Number of Samples Included：", nrow(pca_data)) 
      
      # 7. 存储聚类结果（必须完整替换反应式变量，不能只修改内部元素） 
      cluster_result(list( 
        plot = pca_plot,          # 完整的ggplot对象 
        text = result_text, 
        table = result_table, 
        pca_data = pca_data,      # 可选：存储PCA数据用于调试 
        var_explained = var_explained 
      )) 
      
      showNotification( 
        paste0("✅ Clustering Analysis Completed! Predicted", nrow(result_table), " New Samples in Total"), 
        type = "message", duration = 8 
      ) 
      
    }, error = function(e) { 
      showNotification( 
        paste0("❌ Clustering Analysis Failed", e$message), 
        type = "error", duration = 15 
      ) 
      # 控制台打印完整错误栈，方便调试 
      message("\n 🔍 Clustering Error Details") 
      print(e) 
    }) 
  }) 
  
  # -------------------------- 绑定PCA图输出到UI -------------------------- 
  output$pca_plot <- renderPlot({ 
    # 强制等待聚类结果生成，确保plot对象存在 
    req(cluster_result()$plot) 
    
    # 调试：确认绘图对象已传递到输出 
    message("🔄 Rendering PCA plot...") 
    message("   Plot object type：", class(cluster_result()$plot)) 
    
    # 返回绘图对象 
    cluster_result()$plot 
  }) 
  
  # -------------------------- 绑定聚类结果文本输出 -------------------------- 
  output$cluster_result <- renderPrint({ 
    req(cluster_result()$text) 
    cat(cluster_result()$text) 
  }) 
  
  # -------------------------- 绑定聚类结果表格输出 -------------------------- 
  output$dataset_table <- renderDT({ 
    req(cluster_result()$table) 
    datatable( 
      cluster_result()$table, 
      options = list(pageLength = 10, scrollX = TRUE), 
      rownames = FALSE, 
      caption = "Prediction Results of Unsupervised Clustering" 
    ) 
  }) 
      
      
  # -------------------------- 通用模块：结果下载 -------------------------- 
  # 4.1 下载K-M生存曲线（仅风险评分模式） 
  output$download_km_plot <- downloadHandler( 
    filename = function() { 
      paste0("km_survival_plot_", Sys.Date(), ".", input$plot_format) 
    }, 
    content = function(file) { 
      req(rs_result()$kmfit, rs_result()$data, input$plot_format) 
      rs_data <- rs_result()$data 
      kmfit <- rs_result()$kmfit 
      
      tryCatch({ 
        # 重新绘制K-M图确保下载版本一致 
        high_color <- pal_npg("nrc")(10)[1] 
        low_color <- pal_npg("nrc")(10)[2] 
        
        surv_plot <- ggsurvplot( 
          fit = kmfit, data = rs_data, 
          pval = TRUE, pval.method = TRUE, 
          conf.int = TRUE, risk.table = TRUE, 
          legend.labs = c("High-Risk", "Low-Risk"), 
          legend.title = "Risk Group", 
          risk.table.col = "strata", 
          surv.median.line = "hv", 
          ggtheme = theme_bw(base_size = 12) + theme(panel.grid = element_blank()), 
          palette = c(high_color, low_color) 
        ) 
        
        # 根据格式保存，ggsurvplot返回的是列表，需要转换为ggplot对象 
        if (input$plot_format == "pdf") { 
          pdf(file, width = 10, height = 8) 
          print(surv_plot, newpage = FALSE) 
          dev.off() 
        } else { 
          # 保存为图片格式 
          png(file, width = 1000, height = 800, res = 100) 
          print(surv_plot, newpage = FALSE) 
          dev.off() 
        } 
      }, error = function(e) { 
        showNotification(paste("Failed to download K-M Plot:", e$message), type = "error", duration = 5) 
      }) 
    } 
  ) 
  
  # 4.2 下载ROC曲线（仅风险评分模式） 
  output$download_roc_plot <- downloadHandler( 
    filename = function() { 
      paste0("time_roc_curve_", Sys.Date(), ".", input$plot_format) 
    }, 
    content = function(file) { 
      req(rs_result()$roc_list, input$plot_format) 
      roc_list <- rs_result()$roc_list 
      
      tryCatch({ 
        # 重新绘制ROC曲线 
        if (input$plot_format == "pdf") { 
          pdf(file, width = 8, height = 8) 
        } else { 
          png(file, width = 800, height = 800, res = 100) 
        } 
        
        plot(roc_list$year1, time = 1, col = pal_npg("nrc")(10)[1], 
             main = "Time-dependent ROC Curve", lwd = 2, xlim = c(0,1), ylim = c(0,1)) 
        plot(roc_list$year3, time = 3, add = TRUE, col = pal_npg("nrc")(10)[2], lwd = 2) 
        plot(roc_list$year5, time = 5, add = TRUE, col = pal_npg("nrc")(10)[3], lwd = 2) 
        # 添加图例和参考线 
        legend("bottomright", 
               legend = c( 
                 paste0("1-Year AUC: ", sprintf("%.3f", roc_list$year1$AUC[2])), 
                 paste0("3-Year AUC: ", sprintf("%.3f", roc_list$year3$AUC[2])), 
                 paste0("5-Year AUC: ", sprintf("%.3f", roc_list$year5$AUC[2])) 
               ), 
               col = pal_npg("nrc")(10)[1:3], lty = 1, lwd = 2, bty = "n") 
        abline(a=0, b=1, lty=2, col="gray50") 
        
        dev.off() 
      }, error = function(e) { 
        showNotification(paste("Failed to download ROC Curve:", e$message), type = "error", duration = 5) 
      }) 
    } 
  ) 
  
  # 4.3 下载PCA聚类图（仅无监督聚类模式） 
  output$download_pca_plot <- downloadHandler( 
    filename = function() { 
      paste0("cluster_pca_plot_", Sys.Date(), ".png") # PCA图默认用高清PNG 
    }, 
    content = function(file) { 
      req(cluster_result()$plot) 
      
      tryCatch({ 
        ggsave( 
          filename = file, 
          plot = cluster_result()$plot, 
          width = 12, 
          height = 8, 
          dpi = 150, 
          device = "png" 
        ) 
        # 也支持PDF格式，可根据需求扩展 
        # ggsave(file, plot = cluster_result()$plot, width = 12, height = 8, device = "pdf") 
      }, error = function(e) { 
        showNotification(paste("Failed to download PCA Plot:", e$message), type = "error", duration = 5) 
      }) 
    } 
  ) 
  
  # 4.4 通用结果表格下载（支持风险评分/聚类两种模式） 
  output$download_table <- downloadHandler( 
    filename = function() { 
      if (input$data_source != "Unsupervised clustering") { 
        paste0("risk_score_result_", Sys.Date(), ".xlsx") 
      } else { 
        paste0("cluster_prediction_result_", Sys.Date(), ".xlsx") 
      } 
    }, 
    content = function(file) { 
      tryCatch({ 
        if (input$data_source != "Unsupervised clustering") { 
          req(rs_result()$data) 
          # 写入风险评分结果表格 
          write.xlsx( 
            x = rs_result()$data, 
            file = file, 
            sheetName = "Risk_Score_Result", 
            rowNames = FALSE, 
            colNames = TRUE, 
            borders = "all" 
          ) 
        } else { 
          req(cluster_result()$table) 
          # 写入聚类预测结果表格 
          write.xlsx( 
            x = cluster_result()$table, 
            file = file, 
            sheetName = "Cluster_Prediction", 
            rowNames = FALSE, 
            colNames = TRUE, 
            borders = "all" 
          ) 
        } 
        showNotification("✅ Result Table Downloaded Successfully!", type = "message", duration = 3) 
      }, error = function(e) { 
        showNotification(paste("❌ Table Download Failed:", e$message), type = "error", duration = 5) 
      }) 
    } 
  ) 
  
  # -------------------------- 辅助模块：清理资源 -------------------------- 
  session$onSessionEnded(function() { 
    # 清除临时文件 
    if (!is.null(input$upload_file)) file.remove(input$upload_file$datapath) 
    if (!is.null(input$upload_files)) file.remove(input$upload_files$datapath) 
  }) 
} 

# -------------------------- 启动Shiny应用 -------------------------- 
shinyApp(ui = ui, server = server) 
