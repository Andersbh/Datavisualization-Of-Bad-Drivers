# plot_linked_UI
# This is the UI function of our module.
# It works similar to ui.R, except when creating outputs
# you have to remember to encapsulate them with ns()
# ns() concatenates the module ID to your outputs.

plot_linked_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      strong("Identify the percentage of drivers who were speeding or who were alcohol-impaired for both the state with the highest number of fatal collisions and the state with the lowest."),
      column(12, plotlyOutput(ns("linked_plot_1"))),
      br(),
      column(12, plotlyOutput(ns("linked_plot_2")))
    )
  )
}

# plot_time
# This is our server function of the module.
# Beyond storing the namespace, all computations must happen inside the
# plotlyOutput reactive context.
plot_linked <- function(input, output, session, df) {
  ns <- session$ns
  
  output$linked_plot_1 <- renderPlotly({
    #validate() ensures that our code is only executed if the dataframe
    # is available. The dataframe may not be present if the user hasnt uploaded
    # any csv file yet. The "vis" errorClass is used to show where the plot will
    # be plotted (optional).
    validate(need(df(), "Waiting for data."), errorClass = "vis")

    # To read the reactive dataframe 'df', we need to "evaluate" it.
    # We do this by calling it as a function df(). 
    df_vis <- df()
  
    df_vis$rowID <- 1:nrow(df_vis)
    # To link timePlot to whackPlot we need to use "event_register()" and
    # specify what interaction we want to link. "plotly_selecting" sends
    # events everytime you make a selection and drag over an item.
    state2 <-  df_vis %>% plot_ly(x = ~State,
                               y = ~Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,
                               type = "bar", 
                               marker = list(color = "steelblue", 
                                             line = list(color = "black", 
                                                         width = 1)))
    state2 <- state2 %>% layout(title = 'Number of drivers involved in fatal collisions per billion miles',
                              xaxis = list(
                                title = "State",
                                titlefont = list(size = 16),
                                tickfont = list(
                                  size = 12,
                                  color = 'rgb(107, 107, 107)')),
                              bargap = 0.25,
                              yaxis = list(
                                #categoryorder='total ascending',
                                title = "",
                                tickfont = list(
                                  size = 12,
                                  color = 'rgb(107, 107, 107)')),
                              plot_bgcolor = "whitesmoke")
    
    state2 <- state2 %>% layout(dragmode = 'select', clickmode = 'event+select')  %>%
      event_register("plotly_selecting")
    
    return(state2)
  })
    
  output$linked_plot_2 <- renderPlotly({
    #validate() ensures that our code is only executed if the dataframe
    # is available. The dataframe may not be present if the user hasnt uploaded
    # any csv file yet. The "vis" errorClass is used to show where the plot will
    # be plotted (optional).
    validate(need(df(), "Waiting for data."), errorClass = "vis")
    
    # To read the reactive dataframe 'df', we need to "evaluate" it.
    # We do this by calling it as a function df(). 
    df_vis <- df()
    
    df_vis$rowID <- 1:nrow(df_vis)
    
    select.data <- event_data(event = "plotly_selecting")
    
    if (!is.null(select.data)) {
      df_vis = df_vis %>% filter(rowID %in% (select.data$pointNumber+1))
    }
    
    grouped_bar <- df_vis %>% plot_ly(x = ~State, 
                                    y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding, 
                                    type = 'bar', 
                                    name = 'Percentage of drivers who were speeding',
                                    marker = list(color = "steelblue", 
                                                  line = list(color = "black", 
                                                              width = 0.5)))
    grouped_bar <- grouped_bar %>% add_trace(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired, 
                                             name = 'Percentage of drivers who were alcohol-impaired',
                                             marker = list(color = "salmon", 
                                                           line = list(color = "black", 
                                                                       width = 0.5)))
    grouped_bar <- grouped_bar %>% layout(title = "Speeding VS Alcohol-Impaired",
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
                                          barmode = 'group', bargap = 0.25,
                                          plot_bgcolor = "whitesmoke")
    return(grouped_bar)
  })
  
}