

library(shiny)

shinyUI(bootstrapPage(
  
  HTML('<div id="header" style="position:absolute; width: 100%; height: 65px; float:left; z-index: 999; text-align:left; 
  		background: rgb(245,245,245); background: -moz-linear-gradient(top, rgba(220,220,220,1) 1%, rgba(245,245,245,1) 100%); 
			background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba(220,220,220,1)),	color-stop(100%,rgba(245,245,245,1)));">	
       <div id="logo" style="padding:10px;"><img src="stemmark_300.png" width="200"></div></div>')
  ,
  br(),
  br(),
  br(),
  h4('Koláče rostou na stromech'),
  
  tags$style(type="text/css", '#pietree {height: 600px !important;}'),
  tags$style(type="text/css", "#tabset1 {width: 70% !important; min-width:400px}"),
  tags$style(type="text/css", "#side {width: 25% !important; min-width:200px !important;}"),
  
  
  wellPanel(id = "plocha",
    div(class = "row",
      div(class="col-sm-3" , id="side",
          uiOutput("vyber"),
          uiOutput("model"),
          uiOutput("parstromu"),
          paste("1) Načteme data a načteme proměnné"), br(),
          paste("2) Zadáme klíčovou proměnnou "), br(),
          paste("3) Dělící proměnné (x1+x2..)"), br(),
          paste("4) Upravíme parametru stromu"), br(),
          paste("5) [Vyrob strom] a Hotovo..")
          ),
      
      div(class="col-sm-9", id = "tabset1",
      uiOutput("tabset")  
      )
      )
  )
 
))
