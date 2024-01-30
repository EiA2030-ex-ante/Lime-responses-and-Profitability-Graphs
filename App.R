# Load Required Packages
library(shiny)
library(shinythemes)
library(shinymaterial)
library(shinyjs)
library(bslib)
library(ggplot2)
library(dplyr)
library(leaflet)
library(lmtest) # for robust standard errors
library(margins) # for marginal effects and marginal effects plots
library(sandwich)

# Prepare Data:
# Create data frames for each set of data.
predictions <- data.frame(
  lime_tha = 0:7,
  yhat = c(3.576989, 4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346)
)

# Marginal effects
marginal_effects <- data.frame(
  lime_tha = 0:7,
  yhat = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, -0.0866508, -0.2186458)
)

# Net Revenue
net_rev_ci <- data.frame(
  lime_tha = 0:7,
  net_rev = c(357.6989, 491.631, 612.3637, 719.8969, 814.2306, 895.3647, 963.2994, 1018.0346)
)

# AVCR
avcr_ci <- data.frame(
  lime_tha = 1:7,
  low = c(4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346),
  avcr = c(6.02, 3.37, 2.43, 1.91, 1.56, 1.29, 1.08),
  high = c(4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346)
)

# MVCR
mvcr_ci <- data.frame(
  lime_tha = 0:7,
  low = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, 0.0, 0.0),
  mvcr = c(1.0, 0.81, 0.63, 0.44, 0.25, 0.06, 0.0, 0.0),
  high = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, 0.0, 0.0)
)

#read csv containing the data
data <- read.csv("gaia-trials-year1.csv")

# Subset data for crop == 3
maize_data <- data[data$crop == 3, ]

#'crop' is a categorical variable, encode it using factor
maize_data$crop <- as.factor(maize_data$crop)

# Run regression
#Ploted graphs are not as expected
#model <- lm(yield_tha ~ lime_tha * lime_tha + ph + soc + clay + tn + m3al + m3b + m3ca + m3fe + m3k + m3s + m3mg + m3mn + m3na + districtname, data = subset(maize_data, crop == 3))

# Perform regression analysis
#plots expected results
model <- lm(yield_tha ~ lime_tha + I(lime_tha^2) + district, data = maize_data)

# Define UI for application
ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(bg = "rgb(249, 247, 245)", 
                   fg = "rgb(59, 37, 2)",
                   primary = "#5579B6",
                   font_scale = 0.7, bootswatch = "litera"),
  # run bs_theme_preview() to select customize
  # Application title
  titlePanel("Agricultural Lime Responses and Profitability"),
  # Sidebar with the input from users
  sidebarLayout(
    sidebarPanel(
      # Numeric inputs
      numericInput("p_x", "Price of lime (USD/MT)", value = 70),
      numericInput("p_y", "Price of maize (USD/MT)", value = 100),
      numericInput("x_uncertainty", "Uncertainty of lime price (distance to upper/lower 95th percentile CI)", value = 0),
      numericInput("y_uncertainty", "Uncertainty of maize price (distance to upper/lower 95th percentile CI)", value = 50),
      
      conditionalPanel(
        'input.out === "Map"',
        HTML('This tab is meant to show the points on a map, then enable the user to select a subset of these points by selecting on the map, and then recalculate the curve based on the subset of points.'),
        verbatimTextOutput("selected_points"),
      ),
      
      conditionalPanel(
        'input.out === "Agronomic responses"',
        h5(""),
        HTML('This tool is designed to allow users to explore the economic profitability of agricultural liming to address soil acidity-related production constraints. The data used are for maize, collected from farms in moderately acidic areas of Ethiopia, Rwanda and Tanzania. The intention is to allow users to explore the farm-level profitability of lime applications, given an empirical yield response function, at different input and output prices. This tool is part of the analytical resources being assembled to facilitate ex ante analysis of agronomic investments. Tool development was supported by the <a href=\'https://eia.cgiar.org/\'> Excellence in Agronomy (EiA)</a> Initiative, part of the <a href=\'https://www.cgiar.org/food-security-impact/one-cgiar/\'>One CGIAR???s</a> research portfolio to deliver science and innovation to transform food, land, and water systems in a climate crisis. Find more information <a href=\'https://eia.cgiar.org/\'>here</a>. The code used in the assembly of this this tool is avilable at this <a href=\'https://github.com/EiA2030-ex-ante/Lime-responses-and-Profitability-Graphs/blob/main/App.R\'>Github repository</a>.'),
        h5(""),
        h5("About the data:"),
        HTML("The data used in this tool were collected in 2020 from on-farm trials in Ethiopia, Rwanda and Tanzania as part of the Guiding Acid Soil Management Investments in Africa <a href='https://www.cimmyt.org/projects/gaia/'>(GAIA)</a> project, led by CIMMYT and funded by the Bill and Melinda Gates Foundation. "),
        h5(""),
        HTML('Please note that this tool is still in development. Feedback is very welcome. For more information about this tool, other ex ante resources for agronomy, or related activities in <a href=\'https://eia.cgiar.org/\'>EiA</a>, please contact <a href=\'EiA@cgiar.org\'>EiA@cgiar.org</a>.'),
      ),
      conditionalPanel(
        'input.out === "Marginal Effects"',
        h5(""),
        HTML('This graph shows the marginal effects of agricultural lime applications (i.e., the additional maize produced for each additional unit of lime applied) at different levels of application.')
      ),
      conditionalPanel(
        'input.out === "AVCR"',
        h5(""),
        HTML('The average value-cost ratio (AVCR) is the average product (AP) multiplied by the input???output price ratio. The AP is calculated as the difference in the estimated yields from zero fertilizer and some reference level of fertilizer application (which may be the level observed in the data, or the level at which fertilizer is most profitable), divided by total input amount. Here, we use the levels of fertilizer observed in the data, which were defined by agronomists to represent reasonable treatment levels. This graph shows the AVCR of agricultural lime applications at different levels of application. AVCR values are frequently used by agricultural economists to evaluate the profitability of a technology. An AVCR value exceeding 1 indicates profitability in an absolute sense, although an AVCR value of 2 is often used as the minimal profitability threshold for an investment to be attractive to a risk-averse farmer. (The canonical reference for this is <a href=\'https://ageconsearch.umn.edu/record/11686\'>Crawford and Kelly 2001</a>). Typically, the way that AVCR is used in practice is by calculating the share of a sample that has an estimated AVCR above some critical value (often 0 or 1), with the resulting share being indicative of technology profitability in given sample. See peer-reviewed examples of such analysis <a href=\'https://onlinelibrary.wiley.com/doi/10.1002/ldr.3940\'>here</a> and <a href=\'https://onlinelibrary.wiley.com/doi/10.1111/agec.12299\'>here</a>. ')
      ),
      conditionalPanel(
        'input.out === "MVCR"',
        h5(""),
        HTML('The marginal value-cost ratio (MVCR) is computed as the marginal product (MP), or the unit of additional output gained per unit of additional input, multiplied by the ratio of output and input prices (P^y???P^x ).  MP corresponds to input use efficiency (UE), a concept often used by agronomists to define input-output relationships of interest. An MVCR value of 0 indicates the optimal input level for a risk-neutral farmer (because marginal returns are zero). However, MVCR values of 1 or greater are often used analytically as more reasonable indicators of acceptable minimum marginal returns, given risk-aversion by producers and the possibility of incompletely observed costs of production or transactions that would effectively attenuate the actual returns to an investment by a farmer in real world conditions. Typically, the way that MVCR is used is by calculating the share of a sample that has an estimated MVCR above 0 (or some other reference value, e.g., 1 or 2). See peer-reviewed examples of such analysis <a href=\'https://onlinelibrary.wiley.com/doi/10.1002/ldr.3940\'>here</a> and <a href=\'https://onlinelibrary.wiley.com/doi/10.1111/agec.12299 \'>here</a>. This graph shows the MVCR of agricultural lime applications at different levels of application.')
      ),
      img(src = "https://s3.amazonaws.com/eventtia/event_logos/31142/medium/eialogovrgb16569197111656919711.png?1656919711", width = "20%")
    ),
    mainPanel(
      tabsetPanel(
        id = 'out',
        tabPanel("Agronomic responses", 
                 plotOutput("plot_predictions"), 
                 h5(""),
                 HTML('<strong>Estimated yield (MT/HA):</strong> This graph shows empirical yield responses in maize (along with 95 percentile confidence intervals) as a continuous function of agricultural lime. '),
                 h5(""),
                 plotOutput("plot_net_rev"),
                 h5(""),
                 HTML('<strong>Net Revenue:</strong> This graph shows the expected net revenue returns to lime investments at different application levels. Net revenue is calculated as R = (y * P^y )-(x * P^x ) where y is output, x is the input being evaluated (holding other inputs constant), and P^y and P^x are the prices of outputs and inputs, respectively.')
        ),
        
        tabPanel("Marginal Effects", plotOutput("plot_marginal_effects")),
        tabPanel("AVCR", plotOutput("plot_avcr")),
        tabPanel("MVCR", plotOutput("plot_mvcr")),
        tabPanel("Map",
                 fluidRow(
                   column(6, leafletOutput("map")),
                   column(6, plotOutput("yield_plot")),  # Changed plot name here
                   verbatimTextOutput("hover_text")
                 )
        )
      )
    )
  )
))


# Server part
server <- function(input, output) {
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data = maize_data) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = ~paste("District:", districtname))
  })
  
  # Get the selected point from the map
  selected_point <- reactive({
    req(input$map_marker_click)  # Ensure a marker is clicked
    click <- input$map_marker_click
    # Extract districtname from the click event
    district_name <- maize_data$districtname[maize_data$lon == click$lng & maize_data$lat == click$lat]
    if (!is.null(district_name)) {
      return(district_name)
    } else {
      return(NULL)
    }
  })
  
  # Output the selected point
  output$selected_points <- renderPrint({
    if (!is.null(selected_point())) {
      selected_point()
    } else {
      "No point selected"
    }
  })
  
  # Function to update the plot based on selected point
  update_plot <- function(selected_point) {
    if (!is.null(selected_point)) {
      # Filter data based on selected point
      selected_data <- maize_data %>%
        filter(districtname == selected_point)
      
      # Predict yield based on selected point
      selected_data$yhat <- predict(model, newdata = selected_data)
      
      # Plot the data
      output$yield_plot <- renderPlot({
        ggplot(selected_data, aes(x = lime_tha, y = yhat)) +
          geom_point()+
          geom_line(aes(x = lime_tha, y = yhat, color = "selected_data"), linewidth = 1) +  # Adding a line to the plot
          xlab("Lime application (MT/HA)") +
          ylab("Estimated yield ") +
          ggtitle("Estimated yield (MT/HA)") +
          theme(axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)) +
          scale_color_manual(values = "orange") +
          theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
      })
    }
  }
  
  # Reactively update the plot when a point is selected
  observeEvent(input$map_marker_click, {
    update_plot(selected_point())
  })
  
  # Disable numeric inputs
  observe({
    shinyjs::disable("p_x")
    shinyjs::disable("p_y")
  })
  
  output$plot_predictions <- renderPlot({
    ggplot(predictions, aes(x = lime_tha, y = yhat)) +
      geom_point() +
      geom_errorbar(aes(ymin = yhat - 0.156282, ymax = yhat + 0.156281), width = 0.2) +
      geom_line(aes(x = lime_tha, y = yhat, color = "Predictions"), linewidth = 1) +
      labs(title = "Estimated yield (MT/HA)", x = "Lime application (MT/HA)", y = "Estimated yield (MT/HA)") +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_color_manual(values = "blue") +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_net_rev <- renderPlot({
    lower_bound_net_rev <- (predictions$yhat * (input$p_y - input$y_uncertainty)) + (predictions$lime_tha * (input$p_x - input$x_uncertainty))
    upper_bound_net_rev <- (predictions$yhat * (input$p_y + input$y_uncertainty)) + (predictions$lime_tha * (input$p_x + input$x_uncertainty))
    
    ggplot(net_rev_ci, aes(x = lime_tha)) +
      geom_line(aes(y = lower_bound_net_rev, color = "Low"), linewidth = 1) +
      geom_line(aes(y = net_rev, color = "Net Revenue"), linewidth = 1) +
      geom_line(aes(y = upper_bound_net_rev, color = "High"), linewidth = 1) +
      geom_point(aes(y = net_rev), color = "green", size = 3) +
      labs(title = "Net Revenue", x = "Lime application (MT/HA)", y = "Net Revenue") +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_marginal_effects <- renderPlot({
    ggplot(marginal_effects, aes(x = lime_tha, y = yhat)) +
      geom_point() +
      geom_line(aes(x = lime_tha, y = yhat, color = "Marginal Effects"), linewidth = 1) +
      labs(title = "Marginal Effects", x = "Lime application (MT/HA)", y = "Estimated yield (MT/HA)") +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_color_manual(values = "red") +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_avcr <- renderPlot({
    lower_bound_avcr <- (avcr_ci$low * (input$p_y - input$y_uncertainty)) / (avcr_ci$lime_tha * (input$p_x - input$x_uncertainty))
    upper_bound_avcr <- (avcr_ci$high * (input$p_y + input$y_uncertainty)) / (avcr_ci$lime_tha * (input$p_x + input$x_uncertainty))
    
    ggplot(avcr_ci, aes(x = lime_tha[1:7])) +
      geom_line(aes(y = lower_bound_avcr[1:7], color = "Low"), linewidth = 1) +
      geom_line(aes(y = avcr_ci$avcr[1:7], color = "AVCR"), linewidth = 1) +
      geom_line(aes(y = upper_bound_avcr[1:7], color = "High"), linewidth = 1) +
      geom_point(aes(y = avcr_ci$avcr), color = "red", size = 3) +
      labs(title = "Average Value-Cost Ratio", x = "Lime application (MT/HA)", y = "AVCR") +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_mvcr <- renderPlot({
    lower_bound_mvcr <- (mvcr_ci$low * (input$p_y - input$y_uncertainty)) / (input$p_x - input$x_uncertainty)
    upper_bound_mvcr <- (mvcr_ci$high * (input$p_y + input$y_uncertainty)) / (input$p_x + input$x_uncertainty)
    
    ggplot(mvcr_ci, aes(x = lime_tha)) +
      geom_line(aes(y = lower_bound_mvcr, color = "Low"), linewidth = 1) +
      geom_line(aes(y = mvcr_ci$mvcr, color = "MVCR"), linewidth = 1) +
      geom_line(aes(y = upper_bound_mvcr, color = "High"), linewidth = 1) +
      geom_point(aes(y = mvcr_ci$mvcr), color = "green", size = 3) +
      labs(title = "Marginal Value-Cost Ratio", x = "Lime application (MT/HA)", y = "MVCR") +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
}

# Run the Shiny app
shinyApp(ui, server)
