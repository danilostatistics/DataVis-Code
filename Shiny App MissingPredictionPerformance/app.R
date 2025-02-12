#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(mice)
library(ggplot2)
library(caret)

# Define UI
ui <- fluidPage(
  titlePanel("Impact of Missing Data on Regression Predictions"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of Observations:", min = 10, max = 500, value = 100, step = 10),
      sliderInput("sd", "Standard Deviation of X:", min = 10, max = 100, value = sqrt(2000), step = 5),
      sliderInput("miss_proportion", "Proportion of Missing Data:", min = 0, max = 0.9, value = 0.3, step = 0.05),
      sliderInput("Beta", "Regression Coefficient (Beta):", min = 1, max = 10, value = 5, step = 0.5),
      sliderInput("sd_random_error", "Standard Deviation of Random Error:", min = 10, max = 500, value = 150, step = 10),
      selectInput("missing_mechanism", "Missing Mechanism:", choices = c("MCAR", "MAR", "MNAR"), selected = "MNAR"),
      numericInput("seed", "Random Seed:", value = 123, min = 1, step = 1)
    ),
    
    mainPanel(
      plotOutput("coefPlot"),
      p("This app demonstrates how missing data affects regression predictions. 
   The predictor variable (X) is generated from a normal distribution, 
   and the response variable (Y) is computed as a linear combination of X with added Gaussian noise. 
   The user can adjust parameters such as the standard deviation of X, the regression coefficient, 
   the proportion of missing data, and the missing data mechanism (MCAR, MAR, MNAR). 
   The graph compares predictions using mean imputation and multiple imputation. 
   The shaded area surrounding the two regression lines represents the 95% confidence interval, 
   illustrating the uncertainty in the predictions.")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$coefPlot <- renderPlot({
    set.seed(input$seed)
    n <- input$n
    sd <- input$sd
    miss_proportion <- input$miss_proportion
    missing_mechanism <- input$missing_mechanism
    Beta <- input$Beta
    sd_random_error <- input$sd_random_error
    
    # Generate predictor variables
    mu <- 30
    X <- as.matrix(rnorm(n, mean = mu, sd = sd))
    
    # Generate response variable
    Intercept <- 20
    Y <- Intercept + X %*% Beta + rnorm(n, mean = 0, sd = sd_random_error)
    
    df <- data.frame(Y = as.vector(Y), X)
    colnames(df) <- c("Y", "X1")
    
    # Split data into training and test sets
    trainIndex <- createDataPartition(df$Y, p = 0.7, list = FALSE)
    train <- df[trainIndex, ]
    test <- df[-trainIndex, ]
    
    # Introduce missing data
    set.seed(input$seed + 1)
    train_miss <- ampute(train, prop = miss_proportion, mech = missing_mechanism)$amp
    test_miss <- ampute(test, prop = miss_proportion, mech = missing_mechanism)$amp
    
    # Mean Imputation
    train_mean_imputed <- train_miss
    for (col in colnames(train_mean_imputed)) {
      train_mean_imputed[[col]][is.na(train_mean_imputed[[col]])] <- mean(train_mean_imputed[[col]], na.rm = TRUE)
    }
    
    # Multiple Imputation
    set.seed(input$seed + 2)
    imp <- mice(train_miss, m = 5, method = "pmm", printFlag = FALSE)
    train_imputed <- complete(imp)
    
    # Fit models
    fit_mean_imp <- lm(Y ~ ., data = train_mean_imputed)
    fit_imputed <- lm(Y ~ ., data = train_imputed)
    
    test_miss <- na.omit(test_miss)  # Remove remaining NAs
    
    # Predictions
    pred_mean_imp <- predict(fit_mean_imp, newdata = test_miss, interval = "confidence", level= 0.95)
    pred_imputed <- predict(fit_imputed, newdata = test_miss, interval = "confidence", level = 0.95)
    
    # Plot results
    ggplot() +
      geom_point(data = test_miss, aes(x = X1, y = Y), color = "black", alpha = 0.6) +
      
      # Mean Imputation
      geom_line(data = test_miss, aes(x = X1, y = pred_mean_imp[,1], color = "Mean Imputation"), linetype = "dashed", size = 1) +
      geom_ribbon(data = test_miss, aes(x = X1, ymin = pred_mean_imp[,2], ymax = pred_mean_imp[,3], fill = "Mean Imputation"), alpha = 0.2) +
      
      # Multiple Imputation
      geom_line(data = test_miss, aes(x = X1, y = pred_imputed[,1], color = "Multiple Imputation"), size = 1) +
      geom_ribbon(data = test_miss, aes(x = X1, ymin = pred_imputed[,2], ymax = pred_imputed[,3], fill = "Multiple Imputation"), alpha = 0.2) +
      
      ggtitle(paste("Prediction Performance:", missing_mechanism)) +
      labs(x = "X1", y = "Predicted Y", color = "Method", fill = "Method") +
      
      scale_color_manual(values = c("Mean Imputation" = "blue", "Multiple Imputation" = "red")) +
      scale_fill_manual(values = c("Mean Imputation" = "blue", "Multiple Imputation" = "red")) +
      
      theme_minimal() +
      theme(legend.position = "top")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

