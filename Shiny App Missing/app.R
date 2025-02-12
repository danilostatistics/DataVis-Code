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
library(VIM)
library(lme4)
library(Matrix)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Impact of Missing Data on Regression Coefficients"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_obs", "Number of Observations:",
                  min = 10, max = 500, value = 100, step = 10),
      sliderInput("correlation", "Correlation Between Covariates:",
                  min = 0, max = 1, value = 0.5, step = 0.1),
      sliderInput("miss_proportion", "Proportion of Missing Data:",
                  min = 0, max = 0.9, value = 0.3, step = 0.05),
      selectInput("missing_mechanism", "Missing Mechanism:",
                  choices = c("MCAR", "MAR", "MNAR"), selected = "MNAR"),
      numericInput("seed", "Random Seed:", value = 123, min = 1, step = 1)
    ),
    mainPanel(
      plotOutput("coefPlot"),
      
      h3("Exercise Description"),
      HTML("<p>Let <b>X</b> be a design matrix with covariance matrix Cov(<b>X</b>) = <b>Σ</b>. 
        Define the correlation matrix as <b>R = D<sup>-1/2</sup> Σ D<sup>-1/2</sup></b>, 
        where <b>D<sup>-1/2</sup></b> is a diagonal matrix with elements 1/σ<sub>j</sub>.</p>
        
        <p>The outcome variable is defined as: 
        <b>Y = Xβ + e</b>, where <b>β</b> is a vector of parameters and <b>e</b> is normally distributed noise, centered in zero and unit variance.</p>

        <h4>Steps in the Exercise</h4>
        <ol>
          <li><b>Generate Y</b> using the defined model.</li>
          <li><b>Introduce missing data</b> in either <b>Y</b> or <b>X</b>, obtaining amputed versions.</li>
          <li><b>Handle missing data</b>:
            <ul>
              <li>Apply <b>multiple imputation</b> to recover missing values.</li>
              <li>Use <b>listwise deletion</b> to analyze only complete cases.</li>
            </ul>
          </li>
          <li><b>Fit two regression models</b>:</li>
          <ul>
            <li>Model using <b>Multiple Imputation (MI)</b>: <i>Ŷ<sub>MI</sub> = X<sub>MI</sub> β̂<sub>MI</sub></i></li>
            <li>Model using <b>Listwise Deletion (LD)</b>: <i>Ŷ<sub>o</sub> = X<sub>o</sub> β̂<sub>o</sub></i></li>
          </ul>
          <li><b>Compare the results</b>, analyzing bias in estimated coefficients.</li>
        </ol>
        
        <p>This app demonstrates the impact of missing data and the effectiveness of multiple imputation techniques in recovering true regression coefficients.</p>")
    )
    
  )
)

# Define server logic
server <- function(input, output) {
  output$coefPlot <- renderPlot({
    set.seed(input$seed)
    n <- input$n_obs
    p <- 5
    
    # Generate correlated predictor variables
    R <- matrix(input$correlation, nrow = p, ncol = p)
    diag(R) <- 1
    mu <- runif(p, 0, 10)
    X <- MASS::mvrnorm(n, mu = mu, Sigma = R)
    Beta <- runif(p, 1, 3)
    Intercept <- 0  # Assuming true intercept is 0
    Y <- Intercept + X %*% Beta + rnorm(n, mean = 0, sd = 1)
    df <- data.frame(Y = as.vector(Y), X)
    colnames(df) <- c("Y", paste("X", 1:p, sep = ""))
    
    # Introduce missingness
    set.seed(input$seed + 1)
    df_miss <- ampute(df, prop = input$miss_proportion, mech = input$missing_mechanism)$amp
    
    # Perform multiple imputation
    set.seed(input$seed + 2)
    imp <- mice(df_miss, m = 6, method = "pmm", printFlag = FALSE)
    
    # Regression models
    fit_na <- lm(Y ~ ., data = df_miss)
    fit_imp <- with(imp, lm(Y ~ X1 + X2 + X3 + X4 + X5))
    pooled_fit <- pool(fit_imp)
    
    # Convert regression coefficients to a data frame
    coef_df <- data.frame(
      Method = rep(c("Listwise Deletion", "Multiple Imputation"), each = p),
      Variable = rep(c(paste("X", 1:p, sep = "")), times = 2),
      Estimate = c(coef(fit_na)[2:6], summary(pooled_fit)[2:6,2])
    )
    
    # True parameter values
    true_coef_df <- data.frame(
      Variable = paste("X", 1:p, sep = ""),
      TrueValue = Beta
    )
    
    # Plot comparison with individual points for true values
    ggplot() +
      geom_bar(data = coef_df, aes(x = Variable, y = Estimate, fill = Method), 
               stat = "identity", position = "dodge") +
      geom_point(data = true_coef_df, aes(x = Variable, y = TrueValue, shape = "True Parameter"), 
                 color = "black", size = 3) +
      scale_fill_manual(values = c("Listwise Deletion" = "skyblue", "Multiple Imputation" = "coral"),
                        name = "Method") +
      scale_shape_manual(values = c("True Parameter" = 16),
                         name = "Legend") +
      ggtitle(paste("Comparison of Regression Coefficients: Missing Imputation -", input$missing_mechanism)) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



