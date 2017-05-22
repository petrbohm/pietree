#install.packages("gridBase",repos="http://cran.us.r-project.org")

library(shiny)
#library(memisc)

library(foreign)
#library(sjPlot)
library(party)
library(grid)
library(gridBase)
library(RColorBrewer)
library(grDevices)

napoveda <- HTML('<p> 
<b>Jaká data potřebuji?</b>  <br>      
Jako vstup poslouží libovolný SPSS (.sav) soubor. Pokud jsou hodnoty olabelované, vytvoří se nich faktory např ("Ano","Ne").
<br><br>
<b>Klíčová otázka</b>
<ul>
  <li>olabelovaná - stačí uvést pouze jménem "vaha"</li>
  <li>konkrétní hodnota olabelované - (smoke=="ANO") </li>
  <li>vzorec nabývající hodnot PRAVDA/NEPRAVDA - (vaha>80 & vyska <180) </li>
</ul>
Struktura na záložce [Strom] ukazuje rozložení dané proměnné ve všech uzlech. <br>
Label nad vnitřním uzlem ukazuje, podle které proměnné se strom dělí. Popisy nad hranami ukazují, jak se dělí (např "<30",">=30")
<br><br>
Světle šedým číslem je označeno číslo uzlu - na záložce [Popis stromu] je tabulka s počty respondentů v daných koncových uzlech.
<br><br>
<b>Parametry stromu</b><br>
<ul>
  <li>Klíčová proměnná - proměnná, jejíž rozložení se zobrazuje v grafech</li>
  <li>Dělící proměnné - proměnné, podle kterých se má strom větvit "s50+vaha" "obvod+(vek>50)" apod.  </li>
  <li>Kritérium dělení - minimální hodnota testu - default 0.95</li>
  <li>Velikost uzlu - logaritmické měřítko pro jednodušší volbu (jemnost škály)</li>
</ul>
Pro zobrazení stromu je nutné buď načíst data (nebo použít ukázková data) a stisknout [Načti proměnné].<br>
Poté si ze seznamu proměnných navolíme klíčovou a dělící proměnné a stiskneme [Vytvoř/změň strom].<br>
Strom se vykreslí na záložce [Strom] .

</p>')

#funkce definující vzhled koláče
kolac <-function(x) {
  paleta <- colorRampPalette(c("#EBC7C6","#F0D2B6","#C3DBC9"))(length(x))
  
  #paleta <- brewer.pal(8,"Pastel2")
  pushViewport(viewport(x= .5, y = .5 , w = min(.9 ,unit(3,"cm"))  , min(.9 , unit(3, "cm")) )) 
  par(plt = gridPLT(), new = TRUE)
  if (length(x) ==1) {pie(c(x, 1-x), col = c("green","grey"), labels = NA, border = "grey", clockwise = TRUE, radius =.75)}
  else {pie(x, labels = NA, col = paleta, border = "grey", clockwise = TRUE, radius =.75 )}
  popViewport(1)
}

my_outer <- function(node){
  
  kolac(node$prediction)
  mainlab <- paste( "n =", sum(node$weights))
  if (length(node$prediction)==1) {mainlab <- paste(mainlab, "\n(",100*round(node$prediction,2),"%)" , sep = "")}
  else {mainlab <- paste(mainlab, "\n(", paste(round(100*node$prediction,0), collapse =","),")")}
  grid.text(x = 0.5, mainlab,gp = gpar(col='red', cex = 1))
  grid.text(x = 0.25, y = 0.67, node$nodeID, gp = gpar(col='grey', cex = 1))
}


my_edge <- function(split, ordered = FALSE, left = TRUE) {
  
  if (is.numeric(split)) {split <- round(split, digits = 3)}
  if (!ordered) {
    if (length(split) > 1){split <- paste("{", paste(split, collapse = "\n"),"}", sep=" ")}  # collapse = ", "
  } else {
    if (left){split <- as.expression(bquote(phantom(0) <= .(split)))}
    else {split <- as.expression(bquote(phantom(0) > .(split)))}
  }
  grid.rect(gp = gpar(fill = "white", col = NA),width = unit(.9, "strwidth", split), height = unit(.9, "strheight", split) ) 
  grid.text(split, just = "center",  gp = gpar( cex = .8))
}



shinyServer(function(input, output,session){
  

  
  buck <- reactive({  #přepočet šoupátka na logaritmy a hezké zaokrouhlení
    
    input$minbuck
    minb <- input$minbuck
    if (is.null(minb)) {minb<-1}
    buck <- round(10^(minb),0)
    if (buck>10000) {buck <- 500* round(buck/500,0)}  #nad 10000 počítá po 500
    else{
      if (buck>1000) {buck <- 100* round(buck/100,0)}  #nad 1000 počítá po 100
      else {
        if (buck>100) {buck <- 10* round(buck/10,0)}  #nad 100 počítá po 10
        else {if (buck>10) {buck <- 5* round(buck/5,0)} #nad 10 po 5ti
        }
      }
    }
    if (buck==0) {buck <- 1}
    return(buck)
  })
  
  datar <- reactive({    #načtená SPSS data pomocí read.spss {foreign} - má už olabelováno
    input$data
    file <- input$data
    if (input$r==0) {return(NULL)}
    
    if (is.null(file)) {datar<- read.spss("DATA_show.sav",to.data.frame = TRUE)}
    else{datar<- read.spss(file$datapath,to.data.frame = TRUE)  }
        
    })
  
#   datasj <- reactive({  #načtená SPSS data pomocí sji.SPSS {memisc}  - umí totiž labely
#     datar()
#     file <- input$data
#     if (is.null(file)) {datasj<- read_spss("DATA_show.sav")}
#     else{datasj <- read_spss(file$datapath)}
#     })
  
   varlaby<- reactive({  #labely k datům, hodí se k labelování uzlů stromu
    input$data
     file <- input$data
     if (is.null(file)) {data_object<- read.spss("DATA_show.sav",to.data.frame = TRUE)}
     else{data_object<- read.spss(file$datapath,to.data.frame = TRUE)  }
     #get data
     
     #get variable labels
     varlaby <- attr(data_object,"variable.labels") 
     print(str(varlaby))
    varlaby
    })
   
  strom <- reactive({  # tradá.. krasaveček inteligentný - reaktivní stromeček
    
    minb <- isolate(buck()) 
    if (input$r==0) {return(NULL)}   

    input$r1
    crit <- isolate(input$krit)
    p1 <- isolate(input$prom)
    p2 <- isolate(input$formula)
    data <- isolate(datar())
    
    
    try(data <- data[!is.na(data[,which(colnames(data)==p1)]),], silent =T)
    
    if (is.numeric(data[,which(colnames(data)==p1)])){
      a <- data[,which(colnames(data)==p1)] 
      
      ac <- factor(cut(a, breaks= quantile(a, probs=seq(0,1, by=0.25)), 
                       include.lowest=TRUE),labels=c("1Q","2Q","3Q","4Q"))
      
      data[,which(colnames(data)==p1)] <-ac
    }
    
      
    #print(str(data[,which(colnames(data)==p1)]))
    #print(summary(data[,which(colnames(data)==p1)]))
    
    #toto neni nutne, staci dat do ctree "data=data" pripadne pouzit with(data,ctree(....))
    attach(data, warn.conflicts = F)  # abych nemusel před každou proměnnou psát data$
    
    varlabs <- isolate(varlaby())
    vzor <- as.formula(paste(p1,p2,sep = "~"))   # vytvořím kouzelnou formuli x ~ a +b + c
    strom <- ctree(vzor,controls=ctree_control(minbucket=minb, mincriterion = crit))  # vytvořím reaktivní strom (parametry formulka, minb, crit)
    
    })
  
  output$vyber <- renderUI({     
   wellPanel(
      fileInput("data","Load data (SAV file)", accept=c("sav",".sav")) ,
      actionButton("r1","Load variables")
    )
    })
  
  output$model <- renderUI({
    wellPanel(
    textInput("prom","Key variable", value = "hyper"),
    textInput("formula","Independent variables", value ="S50+vaha+obvod"),
    actionButton("r","Create/change tree")
    )
    
  })
  
  output$parstromu <- renderUI({
    wellPanel(
      sliderInput("krit", "Criterion strength", value = .95 , min = .05, max = .99, step = .01 ),
      sliderInput("minbuck", "Minimal size of node [log(10)]", value = 1.33, min = 0, max = 5, step = .01),
      textOutput("uzel")  #ukazuje přepočet logaritmu    
      
      )
    })
  
  output$uzel <- renderText({
    buck()
    minb <- isolate(buck())
    paste("Minimal size of node:", minb)
  })

  #zobrazí seznam proměnných "varlaby"
  output$prehled <- renderTable({
    
    if (input$r1==0) {return(NULL)}
    input$data
    var <- isolate(varlaby())
    if (is.null(var)){return(NULL)}
    print("WTF")
    print(var)
    tab <- data.frame(var= names(var),
                      label=var)
    
    #as.matrix(var)
    })
  
  #vytvoří tabulku - koncový uzel x klíčová proměnná
  output$popis <- renderTable({
    if (input$r==0) {return(NULL)}
    p1 <- isolate(input$prom)
    fit <- isolate(strom())
    t <- table(eval(parse(text=p1)), attributes(fit)$where)
    prop.table(t,2)*100
    
    })
 
  output$popisN <- renderTable({
    if (input$r==0) {return(NULL)}
    p1 <- isolate(input$prom)
    fit <- isolate(strom())
    t <- table(eval(parse(text=p1)), attributes(fit)$where)
    round(t(margin.table(t,2)),0)
    
  })
  
  output$popis2 <- renderPrint({
    if (input$r==0) {return(NULL)}
    fit <- isolate(strom())
    fit
    })

  #graf stromu - 
  output$pietree <- renderPlot({
    
    if (input$r==0) {return(NULL)}
    
    paleta <- brewer.pal(8,"Pastel2")    # hezká světlá paleta
    
    dataiso <- isolate(datar())
    minb <- isolate(buck())
    var <- isolate(varlaby())
    fit <- isolate(strom())
    p1 <- isolate(input$prom)
    
    #paleta <- brewer.pal(8,"Pastel2")   #nastaví barevnou paletu
    
    
  
    #funkce definující vzhled vnitřního uzlu
    my_inner <- function(node){
      
      kolac(node$prediction)
      name <- node$psplit$variableName
      
      label <-var[names(var)==name]   # zjisti, jaký má dělící proměnná label (jestli ho má)
      
      if (length(label) == 0) {nazev <- paste( node$psplit$variableName)}
      else {nazev <- paste(label)}
      #"Popis dělící proměnné"
      grid.rect(x=0.5, y = .1, gp = gpar(fill = "white", col = NA),width = unit(1, "strwidth", nazev),height = unit(1.1,"char"))  
      grid.text(x=0.5, y = .1, label = nazev,gp = gpar(col='black',fill = "white", cex = 1))
      
      mainlab <- paste("\n(n = ",sum(node$weights),")" , sep = "")
      if (length(node$prediction)==1) {mainlab <- paste(mainlab, "\n(",100*round(node$prediction,2),"%)" , sep = "")}
      else {mainlab <- paste(mainlab, "\n(", paste(round(100*node$prediction,0), collapse =","),")")}
      grid.text(x=.5, y =.6, label = mainlab,gp = gpar(col='red', cex = 1))
      grid.text(x = 0.15, y = 0.67, node$nodeID, gp = gpar(col='grey', cex = 1))
    }
    
    plot.new()  
    plot(fit, inner_panel = my_inner, terminal_panel = my_outer, edge_panel = my_edge,  drop_terminal= FALSE)
        
    x <- attributes(fit)$tree$prediction
    paleta <- colorRampPalette(c("#EBC7C6","#F0D2B6","#C3DBC9"))(length(x))
    
    pushViewport(viewport(x= 0, y = 1, just = c("left","top") , w = unit(4, "cm") , h = unit(1, "cm") )) 
    par(plt = gridPLT(), new = TRUE)
    
    long <- levels(eval(parse(text = p1)))
    
    if (length(x)==1) {long <- c("Ano (TRUE)","Ne (FALSE)")}
    
#     if (length(x) ==1) {pie(c(x, 1-x), col = paleta, labels = c("Ano","Ne"), border = "grey", clockwise = TRUE, rad = .5, gp = gpar(cex = .5))}
#     if (length(x) >1)  {pie(x, col = paleta, labels= short, border = "grey", clockwise = TRUE, rad = .8)}  
#     
    testlabel <- var[names(var)==p1]
    if(length(testlabel)==0){testlabel<- p1}

#     if (length(x) >1)  {
      for (i in 1 :length(long)){
      
      grid.rect(x=unit(1,"cm"),y= unit(-i,"char"),just=c("left","top"),height= unit(1,"char"),width = unit(1,"char"),gp = gpar(fill=paleta[i], col="white" ))
      grid.text(x=unit(2,"cm"),y= unit(-i,"char"),just=c("left","top"), paste(long[i],"\n"),gp = gpar(col="black" ))
#     }
    }
    grid.text(testlabel, x = .5, y = unit(1,"char"),just= c("left","top"))
    popViewport(1)
    
    
    if (minb>(nrow(dataiso)/2)) {grid.text(x = 0.5, y = .85, "Zadejte menší velikost uzlu (vzorek nejde dělit)", gp = gpar(cex=2, col = "red") )}
    
    grid.text(paste("Zdroj: STEM/MARK (statistiky.stemmark.cz),",  format(Sys.time(), "%B %Y")), x = 0.02, y = .03, just= "left")
    })
  
  output$tabset <- renderUI({
    tabsetPanel(id="tab1",
                tabPanel("Help",napoveda), 
                tabPanel("Variable list",value="seznam",
                         conditionalPanel("input.r1==0", paste("Load data to show variables of use demo-data [Load variables]")),
                         tableOutput("prehled")
                         ),
                tabPanel("Tree", value = "strom", plotOutput("pietree")),
                tabPanel("Tree description",value = 3, 
                         paste("Numbers of cases in each node"),
                         tableOutput("popis"), 
                         tableOutput("popisN"), 
                         verbatimTextOutput("popis2")   
                         )

                )
})


observe({
  if (length(input$r1)==0){}
  else if (input$r1>0) { updateTabsetPanel(session, "tab1", selected = "seznam") }    
})

observe({
    if (length(input$r)==0){}
    else if (input$r>0) { updateTabsetPanel(session, "tab1", selected = "strom") }    
})



})

# TO DO:
#zjisti, kolik má strom uzlů a podle toho nastav velikost koláčů
# h = min (.9 , unit(2, "cmi"))






