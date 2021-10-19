library(shiny)
library(semantic.dashboard)
library(shiny.semantic)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

ui <- dashboardPage(
  dashboard_header(center = h1("Marketing Analytics Dashboard",icon("chart line"),style="margin-left: 15px")),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("value1")
    ),
    fluidRow(
      tabBox(title = "Customer Distribution", color = "black", ribbon = FALSE, height = "400px",
             tabs = list(
               list(menu = "Age", content = plotOutput("customerdonut")),
               list(menu = "Income", content = plotOutput("customerdonut2")))),
      box(width = 8,
          title = "Buyer Behavior",
          color = 'black',
          ribbon = FALSE,
          column(width = 8,
                 plotOutput("totalspent",height = "458px")))),
    fluidRow(
      box(width = 8,
          title = "Shopping Preference",
          color = 'black',
          ribbon = FALSE,
          column(width = 8,
                 plotlyOutput("shoppref", height = "400px"))),
      box(width = 8,
          title = "Campaign Success",
          color = 'black',
          ribbon = FALSE,
          column(width = 8,
                 plotOutput("campsuccess",height = "400px"))))
  )
)

server <- shinyServer(function(input, output, session) {
    total.revenue = sum(clean.marketing$total.spent)
    ## creating valuebox output
    plot2 = clean.marketing %>% pivot_longer(Web:Store, names_to = "purchase.type", values_to = "number.purchases") %>%
      group_by(AgeGroup,purchase.type) %>% summarise(total = sum(number.purchases)) %>% mutate(percent.spent = total/sum(total)) %>%
      ggplot(aes(purchase.type, percent.spent,fill=fct_relevel(AgeGroup,"Under 36","36-50","51-65","66-80","80+"),color=fct_relevel(AgeGroup,"Under 36","36-50","51-65","66-80","80+"))) +
      geom_bar(stat = "identity",position = position_dodge()) +
      scale_fill_manual(values=alpha(c("#CF5353","#F1C789","#F1EBC3","#6E8C8E","#343A4A"),0.75), name="Age Group") +
      scale_color_manual(values=c("#CF5353","#F1C789","#F1EBC3","#6E8C8E","#343A4A"), name="Age Group") + theme_cowplot(12) +
      scale_y_continuous(labels = scales::percent) + labs(x="Purchase Type",y="Percent of Purchases",title = "Percent of Purchases Made by Type\n and Age Group") +
      theme(plot.title = element_text(size=14,face="bold",hjust = 0.5), legend.title = element_text(size=10, face="bold"), legend.text = element_text(size=10), axis.title = element_text(face="bold"))

    output$value1 = renderValueBox({
      valueBox('Total Revenue Generated',formatC(total.revenue,format = "d",big.mark = ","),icon("dollar sign icon"),color = "black",width = 6,size="small")
    })
    output$customerdonut <- renderPlot({
      clean.marketing %>% count(AgeGroup) %>% mutate(percent = n/sum(n) * 100) %>% as_tibble() %>% 
        ggplot(aes(x=2,y=percent, fill=AgeGroup,color=AgeGroup)) + geom_col(size=1) +
        coord_polar(theta="y",start = 1) +  xlim(c(0.5, 2.5)) + theme_void() + 
        geom_text(aes(label=paste0(round(percent),"%")),color="black",position = position_stack(vjust = 0.5),check_overlap = T,size=6,show.legend = FALSE) +
        scale_fill_manual(values=alpha(c("#CF5353","#F1C789","#F1EBC3","#6E8C8E","#343A4A"),0.75),name="Age Group",breaks = c("Under 36","36-50","51-65","66-80","80+")) +
        scale_color_manual(values=c("#CF5353","#F1C789","#F1EBC3","#6E8C8E","#343A4A"),name="Age Group",breaks = c("Under 36","36-50","51-65","66-80","80+")) +
        ggtitle("Customer Distribution by Age Group") + theme(plot.title = element_text(size=20,face="bold",hjust = 0.5), legend.title = element_text(size=14, face="bold"), legend.text = element_text(size=14))
      
    })
    output$customerdonut2 <- renderPlot({
      clean.marketing %>% count(IncomeRange) %>% mutate(percent = n/sum(n) * 100) %>% na.omit() %>% as_tibble() %>% 
        ggplot(.,aes(x=2,y=percent, fill=IncomeRange,color=IncomeRange)) + geom_col(size=1) +
        coord_polar(theta="y",start = 1) +  xlim(c(0.5, 2.5)) + theme_void() + 
        geom_text(aes(label=paste0(round(percent),"%")),color="black",position = position_stack(vjust = 0.5),check_overlap = T,size=6,show.legend = FALSE) +
        scale_fill_manual(values=alpha(c("#A96565","#E1E3C1","#6B9C96","#456274"),0.75),name="Age Group",breaks = c("Under 50K","50K-75K","75K-100K","Over 100K")) +
        scale_color_manual(values=c("#A96565","#E1E3C1","#6B9C96","#456274"),name="Age Group",breaks = c("Under 50K","50K-75K","75K-100K","Over 100K")) +
        ggtitle("Customer Distribution by Income Range") + theme(plot.title = element_text(size=20,face="bold",hjust = 0.5), legend.title = element_text(size=14, face="bold"), legend.text = element_text(size=14))
    })
    output$shoppref <- renderPlotly({
      ggplotly(plot2) 
    })
    output$totalspent <- renderPlot({
      ggplot(na.omit(clean.marketing),aes(webvisits,total.spent)) + geom_point(size=3,alpha=0.6) +
        geom_smooth(method = lm,se=TRUE,color="#CF5353",size=2) + 
        labs(x="Number of Web Visits per Customer", y="Total Spent per Customer ($)") + theme_cowplot(12) +
        ggtitle("Comparing Number of Visits to Website\n and Money Spent") +
        theme(plot.title = element_text(size=20,face="bold",hjust = 0.5), axis.title = element_text(face="bold",size = 14), axis.text = element_text(size=14)) +
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + ylim(-10,2800)
    })
    output$campsuccess <- renderPlot({
      clean.marketing %>% pivot_longer(20:24, names_to = "campaign", values_to = "accept.reject") %>% group_by(campaign) %>%
        summarise(total = sum(accept.reject)) %>% ggplot(aes(campaign, total)) +
        geom_segment(aes(x=campaign,xend=campaign, y=0,yend=total),color="grey50",size=0.75) +
        geom_point(size=5, color="#6E8C8E",fill=alpha("#6E8C8E",0.5),shape=21,stroke=2) + theme_cowplot(12) +
        labs(x="",y="Acceptance Total",title = "Campaign Success") + 
        theme(plot.title = element_text(size=20,face="bold",hjust = 0.5), axis.text.y = element_text(face="bold"), axis.text = element_text(size=14), axis.title = element_text(size=14)) +
        coord_flip() + scale_x_discrete(labels=c("Campaign 1","Campaign 2","Campaign 3","Campaign 4","Campaign 5"))
    })
  })
  
shinyApp(ui, server)
  