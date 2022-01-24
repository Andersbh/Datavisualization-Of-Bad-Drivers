# plot_timeline_UI
# This is the UI function of our module.
# It works similar to ui.R, except when creating outputs
# you have to remember to encapsulate them with ns()
# ns() concatenates the module ID to your outputs.

plot_violin_scatter_UI <- function(id) {
  ns = NS(id)
  list(
    fluidPage(
      fluidRow(
      strong("Which seem to have the highest impact on the number of fatal collisions; drivers who were speeding or alcohol-impaired?"),
      column(12, plotlyOutput(ns("violin"))),
      br(),
      strong("Does the scatter plot insinuate any correlation between the number of drivers who were speeding and alcohol-impaired?"),
      column(12, plotlyOutput(ns("scatter")))
    ))
  )
}

# plot_timeline
# This is our server function of the module.
# Beyond storing the namespace, all computations must happen inside the
# plotlyOutput reactive context.
plot_violin_scatter <- function(input, output, session, df) {
  ns <- session$ns
  
  output$violin <- renderPlotly({
    #validate() ensures that our code is only executed if the dataframe
    # is available. The dataframe may not be present if the user hasnt uploaded
    # any csv file yet. The "vis" errorClass is used to show where the plot will
    # be plotted (optional).
    validate(need(df(), "Waiting for data."), errorClass = "vis")

    # To read the reactive dataframe 'df', we need to "evaluate" it.
    # We do this by calling it as a function df(). 
    df_vis <- df()

    # Now we can create a plot of the data.
    violin <- df_vis %>% plot_ly(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,
                               type = 'violin',
                               name = "Percentage of drivers who were speeding",
                               box = list(
                                 visible = F
                               ),
                               meanline = list(
                                 visible = F))
    violin <- violin %>% add_trace(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,
                                   type = 'violin',
                                   name = "Percentage of drivers who were alcohol-impaired",
                                   box = list(
                                     visible = F
                                   ),
                                   meanline = list(
                                     visible = F))
    violin <- violin %>% layout(title = "Distribution of drivers who were speeding and alcohol-impaired",
                                xaxis = list(
                                  title = "",
                                  tickfont = list(
                                    size = 12,
                                    color = 'rgb(107, 107, 107)')),
                                yaxis = list(
                                  title = "Percentage",
                                  titlefont = list(size = 16),
                                  tickfont = list(
                                    size = 12,
                                    color = 'rgb(107, 107, 107)')),
                                plot_bgcolor = "whitesmoke")
    
    return(violin)
  })
  output$scatter <- renderPlotly({
    #validate() ensures that our code is only executed if the dataframe
    # is available. The dataframe may not be present if the user hasnt uploaded
    # any csv file yet. The "vis" errorClass is used to show where the plot will
    # be plotted (optional).
    validate(need(df(), "Waiting for data."), errorClass = "vis")
    
    # To read the reactive dataframe 'df', we need to "evaluate" it.
    # We do this by calling it as a function df(). 
    df_vis <- df()
    
    model1 <- lm(data = df_vis, num_alcohol~num_speed)
    # Now we can create a plot of the data.
    speed_alcohol <- df_vis %>% plot_ly(x = ~num_speed, 
                                      y = ~num_alcohol, 
                                      text = ~State, 
                                      type = 'scatter',
                                      height = 800,
                                      marker = list(color = "steelblue"))
    speed_alcohol <- speed_alcohol %>% add_trace(x = ~num_speed, y = fitted(model1), mode = 'lines', name = "Linear model")
    speed_alcohol <- speed_alcohol %>% layout(title = 'Speeding VS Alcohol-Impaired',
                                              xaxis = list(
                                                title = "Number of drivers who were speeding",
                                                titlefont = list(size = 14),
                                                tickfont = list(
                                                  size = 12,
                                                  color = 'rgb(107, 107, 107)')),
                                              yaxis = list(
                                                title = "Number of drivers who were alcohol-impaired",
                                                titlefont = list(size = 14),
                                                tickfont = list(
                                                  size = 12,
                                                  color = 'rgb(107, 107, 107)')),
                                              plot_bgcolor = "whitesmoke")
    
    return(speed_alcohol)
  })
  
}
