shinyServer(function(input, output) {
    
    # Use reactive expression, to enable caching, if input parameters (eg: the user string, or the number of alternative predictions to return) have not changed.
    do_Predict <- reactive({  
        #PLACEHOLDER# paste("Reactive function activated:", input$txt_input)
        get_Predicted_Words(input$txt_input, input$sld_num_predictions)
    })
            
    output$txt_prediction <- renderText({
        head(do_Predict(), 1)  # Return just the first value
    })
    
    output$txt_alternatives <- renderText({
        paste(do_Predict()[-1], collapse = ", ")  # Return all except the first value
    })
    
})