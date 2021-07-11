
# =====
# Notes
# =====
# Goals
# - input a point (b1, b2)
# - output the data and associated model

# - slide t/lambda
# - show contour of error plot



# Global Constants
XMIN = -3
XMAX = 3
YMIN = -3
YMAX = 3

# Set Up Parameters
n = 30
lambda = 1
B = c(1, 1)
t = 1


# Simulate Data
X1 = rnorm(n)
X2 = rnorm(n)
e = rnorm(n, 0, 0.5)
Y = B[1]*X1 + B[2]*X2 + e



# ===============
# Draw Constraint
# ===============
# Define Axes
B1 = seq(XMIN, XMAX, 0.1)
B2 = seq(YMIN, YMAX, 0.1)

# Draw Constraint
plot(NA, xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX))
    
    # Plot Axis
    abline(h=0)
    abline(v=0)

    # Draw L1 Constraint
    segments(-t, 0, 0, t)
    segments( 0, t, t, 0)
    segments( t, 0, 0,-t)
    segments( 0,-t,-t, 0)


# ===========
# Grid Search
# ===========

# Draw OLS Solution
#rss = outer(B1, B2) # initialize 
rss = matrix(NA, nrow=length(B1), ncol=length(B2))
const = matrix('blue', nrow=length(B1), ncol=length(B2))

for(i in 1:length(B1)){
    for(j in 1:length(B2)){
        Yhat = X1 * B1[i] + X2 * B2[j]
        rss[i, j] = sum((Y - Yhat)^2)

        if(B1[i] + B2[j] <= t){
            const[i, j] = 'red'
        }
    }
}

# Draw Constraint
grid = outer(B1, B2)
b1 = B1[(grid[,1] + grid[,2]) <= t]
b2 = B2[(grid[,1] + grid[,2]) <= t]

rss_constrained = outer(b1, b2)
for(i in 1:length(b1)){
    for(j in 1:length(b2)){
        
        Yhat = X1 * b1[i] + X2 * b2[j]
        rss_constrained[i, j] = sum((Y - Yhat)^2)
    }
}

persp(B1, B2, rss, col = const,
        xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX), zlim=c(-1, 300), 
        ticktype='detailed', axes=T, theta=-45, phi=-15)




# ==========
# Draw Model
# ==========
library('scatterplot3d')

# Modify Regression Coefficients
df = data.frame(X1, X2, Y)
fit = lm(Y ~ X1 + X2, data=df)
fit$coefficients[1] = 0
fit$coefficients[2] = B1[1]
fit$coefficients[3] = B2[1]

# Plot Results
g = scatterplot3d(df)
g$plane3d(fit)
g$points3d(df)


# 





# ------------------------------------------------------------
library('shiny')

ui <- fluidPage(
    sidebarPanel(

        # Inputs 
        sliderInput("B1", "B1", min = -3, max = 3, value = 0),
        sliderInput("B2", "B2", min = -3, max = 3, value = 0),
        sliderInput("t", "t", min = 0, max = 3, value = 1),

        numericInput("n", "N:", min = 0, max = 100, value = 50),
        br(),
        actionButton("goButton", "Go!"),
        p("Click the button to update the value displayed in the main panel.")

    ),

    mainPanel(

        # Plot
        plotOutput("plot", width = 250, height = 250)
    )
)

server <- function(input, output, session) {
    output$plot <- renderPlot(
        width = 600,
        height = 400,
        res = 96,
        {
            t = input$t
            B1 = input$B1
            B2 = input$B2
            X1 = X1
            X2 = X2

            # Process Parameters
            df = data.frame(X1, X2, Y)
            fit = lm(Y ~ X1 + X2, data=df)
            fit$coefficients[1] = 0
            fit$coefficients[2] = B1[1]
            fit$coefficients[3] = B2[1]

            par(mfrow=c(1,2)) # change plot dimensions
                
                # draw data
                g = scatterplot3d(df)
                g$plane3d(fit)
                g$points3d(df)

                # 2nd plot
                # Plot Graph
                plot(X1, X2, 
                     xlim=c(-3,3), ylim=c(-3,3))
                abline(v=0)
                abline(h=0)
                points(B1, B2, pch='*', col='red')

                # Draw L1 Constraint
                t = input$t
                segments(-t, 0, 0, t, col='blue')
                segments( 0, t, t, 0, col='blue')
                segments( t, 0, 0,-t, col='blue')
                segments( 0,-t,-t, 0, col='blue')

            par(mfrow=c(1,1)) # reset plot dimensions 

        }

    )

    # End Function 
    session$onSessionEnded(function()stopApp())
}
shinyApp(ui, server)














