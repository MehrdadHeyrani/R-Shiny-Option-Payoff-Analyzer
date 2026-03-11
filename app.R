library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(plotly)
library(lubridate)

# ======================================
# Helpers
# ======================================
.safe_T <- function(T) pmax(T, 1e-12)
.sign_side <- function(side) ifelse(toupper(side) == "SELL", -1, 1)

# ======================================
# Black–Scholes functions
# ======================================
.d1 <- function(S, K, r, q, sigma, T) {
  T <- .safe_T(T)
  (log(S/K) + (r - q + 0.5*sigma^2)*T) / (sigma*sqrt(T))
}
.d2 <- function(S, K, r, q, sigma, T) {
  .d1(S,K,r,q,sigma,T) - sigma*sqrt(.safe_T(T))
}

bs_price <- function(type, S, K, r, q, sigma, T) {
  type <- toupper(type)
  if (T <= 0) return(if (type=="CALL") pmax(S-K,0) else pmax(K-S,0))
  d1 <- .d1(S,K,r,q,sigma,T)
  d2 <- d1 - sigma*sqrt(.safe_T(T))
  if (type=="CALL") {
    S*exp(-q*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  } else {
    K*exp(-r*T)*pnorm(-d2) - S*exp(-q*T)*pnorm(-d1)
  }
}

bs_delta <- function(type,S,K,r,q,sigma,T) {
  type <- toupper(type)
  d1 <- .d1(S,K,r,q,sigma,T)
  if (type=="CALL") exp(-q*T)*pnorm(d1) else exp(-q*T)*(pnorm(d1)-1)
}

bs_gamma <- function(S,K,r,q,sigma,T){
  d1 <- .d1(S,K,r,q,sigma,T)
  exp(-q*T)*dnorm(d1)/(S*sigma*sqrt(.safe_T(T)))
}

bs_vega <- function(S,K,r,q,sigma,T){
  d1 <- .d1(S,K,r,q,sigma,T)
  S*exp(-q*T)*dnorm(d1)*sqrt(.safe_T(T))
}

bs_theta <- function(type,S,K,r,q,sigma,T){
  type <- toupper(type)
  if (T<=0) return(0)
  d1 <- .d1(S,K,r,q,sigma,T)
  d2 <- d1 - sigma*sqrt(.safe_T(T))
  term <- -(S*exp(-q*T)*dnorm(d1)*sigma)/(2*sqrt(.safe_T(T)))
  if (type=="CALL"){
    term - r*K*exp(-r*T)*pnorm(d2) + q*S*exp(-q*T)*pnorm(d1)
  } else {
    term + r*K*exp(-r*T)*pnorm(-d2) - q*S*exp(-q*T)*pnorm(-d1)
  }
}

bs_rho <- function(type,S,K,r,q,sigma,T){
  type <- toupper(type)
  d2 <- .d2(S,K,r,q,sigma,T)
  if (type=="CALL") K*T*exp(-r*T)*pnorm(d2) else -K*T*exp(-r*T)*pnorm(-d2)
}

bs_greeks <- function(type,S,K,r,q,sigma,T){
  c(
    BS_Value = bs_price(type,S,K,r,q,sigma,T),
    Delta    = bs_delta(type,S,K,r,q,sigma,T),
    Gamma    = bs_gamma(S,K,r,q,sigma,T),
    Vega     = bs_vega(S,K,r,q,sigma,T),
    Theta    = bs_theta(type,S,K,r,q,sigma,T),
    Rho      = bs_rho(type,S,K,r,q,sigma,T)
  )
}

option_payoff <- function(type,buy_sell,strike,notional,price){
  type <- toupper(type); side <- toupper(buy_sell)
  intrinsic <- if (type=="CALL") pmax(price-strike,0) else pmax(strike-price,0)
  payoff <- intrinsic*notional
  if (side=="SELL") payoff <- -payoff
  payoff
}

# ======================================
# UI
# ======================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card { margin-bottom: 12px; border-radius: 6px; }
      .card-header { cursor: pointer; background:#e9ecef; }
      .btn-link { text-decoration:none !important; font-size:17px; }
    "))
  ),
  
  titlePanel("Dynamic Option Portfolio Analyzer"),
  
  
  tags$div(
    style = "background:#f8f9fa; border-left: 4px solid #2C7FB8; padding: 15px; margin-bottom: 20px;",
    h4("📘 How to Use This Application"),
    p("- Input your option deal parameters to perform a Payoff, Greeks, and Position analysis."),
    
    p("- This application serves as a practical demonstration of option strategy evaluation for risk management purposes."),
    p("- For questions or additional support, please contact: ",
      tags$b("Mehrdad.Heyrani@gmail.com  |  linkedin.com/in/mehrdad-heyrani"),
      withMathJax("$$Call = \\max(S_T - K, 0) \\quad |\\quad C = S_0 N(d_1) - Ke^{-rT}N(d_2), \\quad Put = \\max(K - S_T, 0) \\quad |\\quad P = Ke^{-rT}N(-d_2) - S_0 N(-d_1)$$" )
      #withMathJax("$$C = S_0 N(d_1) - Ke^{-rT}N(d_2), \\quad  P = Ke^{-rT}N(-d_2) - S_0 N(-d_1)$$")
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      
      div(class="card",
          div(class="card-header", tags$b("Market Inputs")),
          div(class="card-body",
              dateInput("asof","As of Date",value=Sys.Date()),
              dateInput("expiry","Expiry Date",value=Sys.Date()+30),
              numericInput("spot","Spot Price",value=100),
              numericInput("vol","Volatility",value=0.3),
              numericInput("r","Risk-Free Rate",value=0.02),
              numericInput("q","Dividend Yield",value=0)
          )
      ),
      
      div(class="card",
          div(class="card-header", tags$b("Simulation Settings")),
          div(class="card-body",
              sliderInput("price_range","Stock Price Range",0,1000,value=c(50,200)),
              numericInput("price_step","Price Step",1,min=0.01,step=0.01)
          )
      ),
      
      div(class="card",
          div(class="card-header", tags$b("Option Deals")),
          div(class="card-body",
              numericInput("nDeals","Number of Deals",2,min=1,max=10),
              numericInput("global_premium","Global Premium",0),
              uiOutput("deals_ui")
          )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Option Payoff",
                 plotOutput("totalPayoffPlot"),
                 br(),
                 plotOutput("perDealPayoffPlot"),
                 br(),
                 downloadButton("downloadPayoffCSV","Download CSV"),
                 br(), br(),
                 DTOutput("payoffTable")
        ),
        
        # ===================== GREEKS TAB =====================
        tabPanel("Greeks vs Price",
                 fluidRow(
                   column(6, plotOutput("plot_BS_Value", height=300)),
                   column(6, plotOutput("plot_Delta",    height=300))
                 ),
                 fluidRow(
                   column(6, plotOutput("plot_Gamma", height=300)),
                   column(6, plotOutput("plot_Vega",  height=300))
                 ),
                 fluidRow(
                   column(6, plotOutput("plot_Theta", height=300)),
                   column(6, plotOutput("plot_Rho",   height=300))
                 )
        ),
        
        # ===================== PORTFOLIO DELTA TAB =====================
        tabPanel("Portfolio Delta / BS Value",
                 plotlyOutput("deltaPlotly", height=420),
                 br(),
                 plotlyOutput("bsPlotly", height=420)
        )
      )
    )
  )
)

# ======================================
# SERVER
# ======================================
server <- function(input, output, session) {
  
  output$deals_ui <- renderUI({
    n <- input$nDeals
    
    panels <- lapply(seq_len(n), function(i) {
      div(class="card",
          div(class="card-header",
              tags$button(
                class="btn btn-link",
                type="button",
                `data-toggle`="collapse",
                `data-target`=paste0("#deal_",i),
                paste("Deal", i)
              )
          ),
          div(id=paste0("deal_",i), class="collapse",
              div(class="card-body",
                  selectInput(paste0("type_",i),"Option Type",c("CALL","PUT")),
                  selectInput(paste0("side_",i),"Buy/Sell",c("BUY","SELL")),
                  numericInput(paste0("strike_",i),"Strike",100),
                  numericInput(paste0("notional_",i),"Notional",1)
              )
          )
      )
    })
    tagList(panels)
  })
  
  deals <- reactive({
    n <- input$nDeals
    tibble(
      Deal = paste0("Deal_",1:n),
      Type = sapply(1:n, function(i) input[[paste0("type_",i)]]),
      Side = sapply(1:n, function(i) input[[paste0("side_",i)]]),
      Strike = sapply(1:n, function(i) input[[paste0("strike_",i)]]),
      Notional = sapply(1:n, function(i) input[[paste0("notional_",i)]])
    )
  })
  
  prices <- reactive(seq(input$price_range[1], input$price_range[2], by=input$price_step))
  
  T_years <- reactive({
    as.numeric(difftime(input$expiry, input$asof, units="days"))/365
  })
  
  payoff_df <- reactive({
    d <- deals()
    price_vec <- prices()
    out <- tibble(`Stock Price` = price_vec)
    
    for (i in 1:nrow(d)) {
      out[[d$Deal[i]]] <- option_payoff(
        d$Type[i], d$Side[i], d$Strike[i], d$Notional[i], price_vec
      )
    }
    
    out$Total_Payoff <- rowSums(out[,-1]) + input$global_premium
    out
  })
  
  # ---------------- PAYOFF PLOTS ----------------
  output$totalPayoffPlot <- renderPlot({
    df <- payoff_df()
    ggplot(df, aes(`Stock Price`, Total_Payoff)) +
      geom_line(size=1.5, color="#2C7FB8") +
      geom_vline(xintercept=input$spot, linetype="dashed") +
      theme_minimal()
  })
  
  output$perDealPayoffPlot <- renderPlot({
    df <- payoff_df() %>% pivot_longer(-`Stock Price`)
    ggplot(df, aes(`Stock Price`, value, color=name)) +
      geom_line(size=1.1) +
      geom_vline(xintercept=input$spot, linetype="dashed") +
      theme_minimal()
  })
  
  # ---------------- TABLE + CSV ----------------
  output$payoffTable <- renderDT({
    datatable(payoff_df(), options=list(pageLength=20, scrollX=TRUE))
  })
  
  output$downloadPayoffCSV <- downloadHandler(
    filename=function() paste0("payoff_",Sys.Date(),".csv"),
    content=function(file) write.csv(payoff_df(),file,row.names=FALSE)
  )
  
  # ---------------- GREEKS ----------------
  greek_df <- function(greek) {
    d <- deals()
    price_vec <- prices()
    T_val <- T_years()
    df <- tibble(`Stock Price` = price_vec)
    
    for (i in 1:nrow(d)) {
      df[[d$Deal[i]]] <-
        sapply(price_vec, function(S)
          bs_greeks(d$Type[i], S, d$Strike[i], input$r, input$q, input$vol, T_val)[greek]
        ) * d$Notional[i] * .sign_side(d$Side[i])
    }
    df$Total <- rowSums(df[,-1])
    df
  }
  
  plot_greek <- function(df, label) {
    df_long <- df %>% pivot_longer(-`Stock Price`)
    ggplot(df_long, aes(`Stock Price`, value, color=name)) +
      geom_line(size=1.2) +
      geom_vline(xintercept=input$spot, linetype="dashed") +
      labs(title=paste(label,"vs Price")) +
      theme_minimal()
  }
  
  output$plot_BS_Value <- renderPlot({ plot_greek(greek_df("BS_Value"), "BS Value") })
  output$plot_Delta    <- renderPlot({ plot_greek(greek_df("Delta"),    "Delta") })
  output$plot_Gamma    <- renderPlot({ plot_greek(greek_df("Gamma"),    "Gamma") })
  output$plot_Vega     <- renderPlot({ plot_greek(greek_df("Vega"),     "Vega") })
  output$plot_Theta    <- renderPlot({ plot_greek(greek_df("Theta"),    "Theta") })
  output$plot_Rho      <- renderPlot({ plot_greek(greek_df("Rho"),      "Rho") })
  
  # ---------------- PORTFOLIO DELTA / BS VALUE ----------------
  portfolio_totals <- reactive({
    d <- deals()
    price_vec <- prices()
    T_val <- T_years()
    
    total_delta <- rep(0,length(price_vec))
    total_bs <- rep(0,length(price_vec))
    
    for (i in 1:nrow(d)) {
      side <- .sign_side(d$Side[i])
      res <- t(sapply(price_vec, function(S)
        bs_greeks(d$Type[i], S, d$Strike[i], input$r, input$q, input$vol, T_val)[c("Delta","BS_Value")]
      ))
      total_delta <- total_delta + d$Notional[i]*side*res[,1]
      total_bs    <- total_bs    + d$Notional[i]*side*res[,2]
    }
    
    tibble(`Stock Price` = price_vec,
           Total_Delta = total_delta,
           Total_BS = total_bs)
  })
  
  output$deltaPlotly <- renderPlotly({
    df <- portfolio_totals()
    plot_ly(df) %>%
      add_bars(x=~`Stock Price`, y=~Total_Delta, name="Delta", marker=list(color="green")) %>%
      layout(title="Portfolio Delta", xaxis=list(title="Stock Price"))
  })
  
  output$bsPlotly <- renderPlotly({
    df <- portfolio_totals()
    plot_ly(df) %>%
      add_bars(x=~`Stock Price`, y=~Total_BS, name="BS Value", marker=list(color="orange")) %>%
      layout(title="Portfolio BS Value", xaxis=list(title="Stock Price"))
  })
}

shinyApp(ui, server)