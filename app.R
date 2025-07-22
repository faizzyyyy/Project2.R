library(shiny)
library(tidyverse)
library(caret)
library(DT)
library(ggplot2)
library(rpart)
library(rpart.plot)

# Load the dataset
poverty_data <- read.csv("poverty.csv")

# Clean column names
names(poverty_data) <- make.names(names(poverty_data))

# Split data
set.seed(123)
train_index <- createDataPartition(poverty_data$Unemployment.Rate...., p = 0.8, list = FALSE)
train_data <- poverty_data[train_index, ]
test_data <- poverty_data[-train_index, ]

# Train models
model_dt <- train(Unemployment.Rate.... ~ ., data = train_data, method = "rpart")
model_rf <- train(Unemployment.Rate.... ~ ., data = train_data, method = "rf")

# Predictions
predictions_dt <- predict(model_dt, test_data)
predictions_rf <- predict(model_rf, test_data)

# Evaluation Metrics
rmse_dt <- RMSE(predictions_dt, test_data$Unemployment.Rate....)
rmse_rf <- RMSE(predictions_rf, test_data$Unemployment.Rate....)
r2_dt <- R2(predictions_dt, test_data$Unemployment.Rate....)
r2_rf <- R2(predictions_rf, test_data$Unemployment.Rate....)

# Residuals
residuals_dt <- test_data$Unemployment.Rate.... - predictions_dt
residuals_rf <- test_data$Unemployment.Rate.... - predictions_rf

# UI
ui <- navbarPage("Unemployment Analysis (COVID Impact)",
                 
                 tabPanel("Data View",
                          fluidPage(style = "background-color: #F0F4C3;",
                                    titlePanel("Dataset Preview"),
                                    DTOutput("data_table"))
                 ),
                 
                 tabPanel("Exploratory Data Analysis",
                          fluidPage(style = "background-color: #FFECB3;",
                                    titlePanel("Trends Over the Years"),
                                    plotOutput("line_unemp"),
                                    plotOutput("bar_gdp"),
                                    plotOutput("line_poverty"),
                                    plotOutput("bar_disasters"),
                                    plotOutput("scatter_unemp_inflation"),
                                    plotOutput("correlation_heatmap"))
                 ),
                 
                 tabPanel("Machine Learning",
                          fluidPage(style = "background-color: #C8E6C9;",
                                    titlePanel("Predicting Unemployment Rate"),
                                    selectInput("model_choice", "Select Model:",
                                                choices = c("Decision Tree", "Random Forest")),
                                    verbatimTextOutput("model_summary"),
                                    plotOutput("tree_plot"))
                 ),
                 
                 tabPanel("Model Evaluation",
                          fluidPage(style = "background-color: #D1C4E9;",
                                    titlePanel("Model Performance Evaluation"),
                                    verbatimTextOutput("metrics_output"),
                                    plotOutput("scatter_plot"),
                                    plotOutput("residual_plot"),
                                    plotOutput("rmse_bar_plot"))
                 )
)

# Server
server <- function(input, output) {
  
  output$data_table <- renderDT({
    poverty_data
  })
  
  output$line_unemp <- renderPlot({
    ggplot(poverty_data, aes(x = Year, y = Unemployment.Rate....)) +
      geom_line(color = "red", size = 1.2) +
      geom_point() +
      labs(title = "Unemployment Rate Over the Years", y = "Unemployment Rate (%)") +
      theme_minimal()
  })
  
  output$bar_gdp <- renderPlot({
    ggplot(poverty_data, aes(x = Year, y = GDP.Growth.Rate....)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "GDP Growth Rate by Year", y = "GDP Growth Rate (%)") +
      theme_minimal()
  })
  
  output$line_poverty <- renderPlot({
    ggplot(poverty_data, aes(x = Year, y = Poverty.Headcount.Ratio....)) +
      geom_line(color = "purple", size = 1.2) +
      geom_point() +
      labs(title = "Poverty Rate Over the Years", y = "Poverty Headcount Ratio (%)") +
      theme_minimal()
  })
  
  output$bar_disasters <- renderPlot({
    ggplot(poverty_data, aes(x = Year, y = Climate.Disasters..count.)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(title = "Climate Disasters Over the Years", y = "Number of Disasters") +
      theme_minimal()
  })
  
  output$scatter_unemp_inflation <- renderPlot({
    ggplot(poverty_data, aes(x = Inflation.Rate...., y = Unemployment.Rate....)) +
      geom_point(color = "darkred", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Unemployment vs Inflation", x = "Inflation Rate (%)", y = "Unemployment Rate (%)") +
      theme_minimal()
  })
  
  output$correlation_heatmap <- renderPlot({
    numeric_data <- poverty_data %>% select_if(is.numeric)
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    corr_df <- as.data.frame(as.table(corr_matrix))
    
    ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Freq, 2)), size = 4) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$model_summary <- renderPrint({
    if (input$model_choice == "Decision Tree") {
      model_dt
    } else {
      model_rf
    }
  })
  
  output$tree_plot <- renderPlot({
    if (input$model_choice == "Decision Tree") {
      rpart.plot(model_dt$finalModel, main = "Decision Tree for Unemployment Rate")
    } else {
      plot(varImp(model_rf), main = "Variable Importance - Random Forest")
    }
  })
  
  output$metrics_output <- renderPrint({
    if (input$model_choice == "Decision Tree") {
      cat("Decision Tree Metrics:\n")
      cat("RMSE:", round(rmse_dt, 2), "\n")
      cat("R²:", round(r2_dt, 3), "\n")
    } else {
      cat("Random Forest Metrics:\n")
      cat("RMSE:", round(rmse_rf, 2), "\n")
      cat("R²:", round(r2_rf, 3), "\n")
    }
  })
  
  output$scatter_plot <- renderPlot({
    df <- test_data %>% mutate(
      Actual = Unemployment.Rate....,
      Predicted = if (input$model_choice == "Decision Tree") predictions_dt else predictions_rf
    )
    ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point(color = "blue", size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgreen") +
      labs(title = "Actual vs Predicted", x = "Actual Unemployment Rate", y = "Predicted") +
      theme_minimal()
  })
  
  output$residual_plot <- renderPlot({
    res <- if (input$model_choice == "Decision Tree") residuals_dt else residuals_rf
    ggplot(data.frame(Residuals = res), aes(x = Residuals)) +
      geom_histogram(fill = "coral", bins = 10, color = "black") +
      labs(title = "Residual Distribution", x = "Residuals", y = "Frequency") +
      theme_minimal()
  })
  
  output$rmse_bar_plot <- renderPlot({
    df <- data.frame(
      Model = c("Decision Tree", "Random Forest"),
      RMSE = c(rmse_dt, rmse_rf)
    )
    ggplot(df, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(title = "Model Comparison: RMSE", y = "RMSE") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
