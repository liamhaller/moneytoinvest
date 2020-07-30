
library(quantmod)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(quantmod)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(scales)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(bsplus)
source("/Users/liamhaller/Desktop/Portfolio/helper.R")


#TODO
#enter valid ticker
#faq accordian
  #create FAQ
  #somethign went wrong?
  #more resources
    #beta isn't in range
    #don't add to dollar amount entered
    #not all stocks are showing
#error messages

#write text  for site


createOption <- "
function(input, callback) {
  var item = input.toUpperCase();
  callback({value: item, label: item});
}
"

linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(theme = "bootstrap.css",
           
                useShinyjs(),
                tags$img(src="sex.jpeg"),
                tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #374885;
                                                  border-top: 1px solid #374885 ;
                                                  border-bottom: 1px solid #374885 ;}
                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #374885 }'))),   
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: visible; content: 'An error occurred. 
                           1. Make sure you have entered at least 5 tickers. 
                           2. Make sure each of the tickers you have entered are valid '; }"),

        fluidRow( #row 1
            column(2, hr()),
            column(8, tags$h3("This passive investing tool calculates how much money you
            should allocate towards each of your selected stocks, 
            ETFs, or mutual funds to create an optimized 
            portfolio with risk that you feel comfortable with.", align = "center"),
                   column(2, hr()),
                   linebreaks(2),
                   fluidRow( #row 2
                       column(4,
                              numericInput("investment", "Initial Investment:", value = 1000, min = 1, step = 1),
                              selectizeInput("stocks", "Input stock tickers", choices = NULL, multiple = TRUE, options = list(create = I(createOption))),
                              sliderInput("factor", "Market Exposure (portfolio beta)", value = c(0.6, 1.4), min = 0, max = 2.5, step = 0.01),
                              actionButton("click", "Optimize")), #column four closed
                       column(8,
                              conditionalPanel(
                                  condition = ("input.click == 0"),
                                  tags$ol(
                                     tags$li(h4("Choose amount to invest")), 
                                     tags$li(h4("Pick stocks, ETFs, or mutual funds to invest in")),
                                        tags$p("If you choose to invest in only stocks", tags$a(href="https://www.jstor.org/stable/2330969?seq=1#metadata_info_tab_contents",target="_blank", 
                                              "reserch shows"), "approximately 20-30 individual stocks are necessary for proper diversification. 
                                               If you are using a combination of ETFs/Mutual Funds and stocks fewer investments may be necessary. 
                                                To learn more read", tags$a(href="https://www.thebalance.com/diversified-passive-investing-strategy-357878",target="_blank", 
                                                "passive investing"), "or", tags$a(href="http://news.morningstar.com/classroom2/course.asp?docId=2938&page=4&CN=COM",target="_blank", "ways to diversify."),""),
                                     tags$li(h4("Select market exposure")),
                                        tags$p("Market exposure is a measure of your portfolio’s volatility in relation to the overall market. 
                                               Essentially, the higher the range, the larger your portfolio’s risk-reward ratio will be. 
                                               If you are unsure of where to set the range, a market exposure near 1 would mirror changes in the 
                                               overall market. For more information see",tags$a(href="https://www.nasdaq.com/glossary/p/portfolio-beta#:~:text=The%20beta%20of%20a%20portfolio,the%20portfolio%20beta%20is%201.50.",target="_blank", "Nasdaq's definition"),"or",
                                               tags$a(href="https://www.thebalance.com/how-to-calculate-your-portfolio-beta-4590382",target="_blank", "how to calculate portfolio beta"),"."),
                                     tags$li(h4("Optimize")),
                                     )),
                              conditionalPanel(
                                condition = "input.updateButton != 0",
                                div(style = 'overflow-x: scroll', tableOutput("weights")),
                                tableOutput("description")%>% withSpinner(color="#805d8d"),
                                uiOutput("reset"))),
                    fluidRow(
                      linebreaks(10),
                      column(12, 
                             actionButton("button", "Disclaimer", 
                                          style="color: #000000; background-color: #DBE6E0; border-color: #DBE6E0"),
                             hidden(
                               div(id='text_div',
                                   verbatimTextOutput("text")

                  )
                )
              )
            )
          )
        )
      )
    )


server <- function(input, output) {

wait_time <- observeEvent(input$click, {
    showModal(modalDialog(
        "calculating portfolio - this process may take up to 5 minutes.",
        easyClose = TRUE,
        footer = modalButton("Dismiss")))})

stocks <- eventReactive(input$click, {as.vector(input$stocks)})
hist <-  eventReactive(input$click, {data_clean(stocks())})
betas <- eventReactive(input$click, {get_capm(hist())})
opt_qu <- eventReactive(input$click, {create_portfolio(hist(), stocks(), betas(), input$factor[1], input$factor[2])})
data <- eventReactive(input$click, {create_df(opt_qu()$weights,input$investment, stocks())})
desc <- eventReactive(input$click, {print_desc(opt_qu()$objective_measures, betas(), opt_qu()$weights, hist())})

values <- eventReactive(input$click, {meanorstdev(opt_qu())})




#RESET BUTTON
#timing
output$reset <- renderUI({
    req(input$click, desc())
    actionButton("reset", "Reset ")
})

#Features of reset button
observeEvent(input$reset, {
    reset("investment")})
observeEvent(input$reset, {
    reset("stocks")})
observeEvent(input$reset, {
    reset("factor")})
observeEvent(input$reset, {
    reset("click")})

#OUTPUTS
#stock info
output$weights <- renderTable(data())
output$description <- renderTable(desc())
    
#disclaimer
observeEvent(input$button, {
  toggle('text_div')
  output$text <- renderText({
    "This website is distributed for general informational and educational purposes only 
    and is not intended to constitute legal, tax, accounting or investment advice. The information, opinions and 
    views contained herein have not been tailored to the investment objectives of any one individual, are current
    only as of the date hereof and may be subject to change at any time without prior notice.All investment strategies 
    and investments involve risk of loss.  Nothing contained in this website should be construed as investment advice. 
    Any reference to an investment’s past or potential performance is not, and should not be construed as, a recommendation
    or as a guarantee of any specific outcome or profit. "})
})

observeEvent(input$click, {
  insertUI(
    selector = "#reset",
    where = "afterEnd",
    ui = 
     fluidRow(
       column(12, 
              linebreaks(3)),
      fluidRow(
            column(1, hr()),
            column(11,
                   actionButton("faq1", "Portfolio Standard Deviation", 
                                style = 'width: 550px;
                                        background-color:#DBE6E0;
                                        border-color: #374885;
                                        color:#374885; '),
                                hidden(div(id='text_div1',
                                           uiOutput("faq1text"))))),
            fluidRow(
              column(1, hr()),
              column(11,
                     actionButton("faq2", "Years of Data", 
                                  style = 'width: 550px;
                                        background-color:#DBE6E0;
                                        border-color: #374885;
                                        color:#374885; '),
                     hidden(div(id='text_div2',
                                uiOutput("faq2text"))))),
              fluidRow(
                column(1, hr()),
                column(11,
                       actionButton("faq3", "A Little Math", 
                                    style = 'width: 550px;
                                        background-color:#DBE6E0;
                                        border-color: #374885;
                                        color:#374885; '),
                       hidden(div(id='text_div3',
                         uiOutput("faq3text"))))
      )
    )
  )
})

observeEvent(input$faq1, {
  toggle('text_div1')
  output$faq1text <- renderUI({p("To illustrate standard deviation let's look at", tags$strong("your portfolio."), "It has an anual expected return of", paste(round(values()[1]*1200,0), "%", sep=""), 
                                 "and a standard deviation of", paste(round(values()[2]*346.4102,0), "%", sep=""), ". Most of the time",  
                                 tags$a(href="https://en.wikipedia.org/wiki/68%E2%80%9395%E2%80%9399.7_rule#:~:text=For%20an%20approximately%20normal%20data,deviations%20account%20for%20about%2099.7%25.", "(or more precisely, 68% of the time)"),
                                 "your portfolio's annual returns will range between", paste(round((values()[1]*1200-values()[2]*346.4102),0), "%", sep="")," and", paste(round((values()[1]*1200+values()[2]*346.4102),0), "%", sep=""),
                                 ". If we look at two standard deviations, 95% of the time your portfolio's returns will fall between",
                                 paste(round((values()[1]*1200-2*values()[2]*346.4102),0), "%", sep="")," and",  paste(round((values()[1]*1200+2*values()[2]*346.4102),0), "%", sep=""),"
                                ")})
})

observeEvent(input$faq2, {
  toggle('text_div2')
  output$faq2text <- renderUI({p("It is reccomened that you have a minimum of 5 years worth of historical data. 
                                 In order to estimate portfolio beta this program will take data starting from the date of the most recently listed stock, ETF, or mutual fund.
                                 For that reason you should avoid incluidng recent IPOs since it will skew the output")})
})

observeEvent(input$faq3, {
  toggle('text_div3')
  output$faq3text <- renderUI({p("This program computes the optimal portfolio by maximizing the the return to CvAR ratio. 
                                 To learn more about the math behind this see", tags$a(href="https://www.hindawi.com/journals/isrn/2013/570950/",target="_blank", "this aplied mathmatics article published in Hindawi"),"." )})
})



}

# Run the application 
shinyApp(ui = ui, server = server)
