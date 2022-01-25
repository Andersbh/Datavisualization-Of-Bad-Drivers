# plot_ggplot_UI
# This is the UI function of our module.
# It works similar to ui.R, except when creating outputs
# you have to remember to encapsulate them with ns()
# ns() concatenates the module ID to your outputs.

plot_ggplot_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      strong("Which state has the highest number of fatal collisions, and which has the lowest?"),
      strong("In both cases, identify the number of drivers that have not been involved in any previous accidents"),
      plotlyOutput(ns("state")), 
    )
  )
}

# plot_ggplot
# This is our server function of the module.
# Beyond storing the namespace, all computations must happen inside the
# plotlyOutput reactive context.
plot_ggplot <- function(input, output, session, df) {
  ns <- session$ns
  
  output$state <- renderPlotly({
    #validate() ensures that our code is only executed if the dataframe
    # is available. The dataframe may not be present if the user hasnt uploaded
    # any csv file yet. The "vis" errorClass is used to show where the plot will
    # be plotted (optional).
    validate(need(df(), "Waiting for data."), errorClass = "vis")

    # To read the reactive dataframe 'df', we need to "evaluate" it.
    # We do this by calling it as a function df(). 
    df_vis <- df()

    # Now we can create a plot of the data.
    state <-  df_vis %>% plot_ly(x = ~Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, 
                               y = ~State,
                               name = "Total Number of drivers involved in fatal collisions per billion miles",
                               type = "bar", 
                               width = 1500, height = 1000,
                               marker = list(color = "lightsteelblue", 
                                             line = list(color = "black", 
                                                         width = 1)))
    state <- state %>% add_trace(x = ~New,
                                 name = "Number of drivers involved in fatal collisions without previous accidents",
                                 marker = list(color = "steelblue"))
    state <- state %>% layout(title = 'Number of drivers involved in fatal collisions per billion miles',
                              xaxis = list(
                                title = "Number of drivers per billion miles",
                                tickfont = list(
                                  size = 12,
                                  color = 'rgb(107, 107, 107)')),
                              yaxis = list(
                                #categoryorder='total ascending',
                                title = "State",
                                titlefont = list(size = 16),
                                tickfont = list(
                                  size = 12,
                                  color = 'rgb(107, 107, 107)')),
                              bargap = 0.25,
                              barmode = 'overlay',
                              plot_bgcolor = "whitesmoke")
    
    return(state)
  })
}
