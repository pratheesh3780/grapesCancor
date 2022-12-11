
######## List of required packages for the tool

library(ggplot2)
library(GGally)
library(CCA)
library(CCP)
library(Hmisc)
library(dplyr)

############################################### server
server <- function(input, output, session) {
  # file uploading
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return()
    }

    # Read the uploaded file
    dt <- read.csv(csvfile$datapath,
      header = input$header,
      sep = ","
    )
    dt
  })

  # After reading the file the variables appear
  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    colnames <- reactive({
      names(csvfile())
    })

    observeEvent(csvfile(), {
      updateCheckboxGroupInput(session, "X",
        choices = colnames(),
        selected = FALSE
      )
    })

    inputVar <- reactive({
      Fixedvar <- input$X

      if (setequal(colnames(), Fixedvar)) {
        # If sets are equal, return an empty string
        return("")
      } else {
        # Return elements that are only in colnames
        setdiff(colnames(), Fixedvar)
      }
    })

    observeEvent(inputVar(), {
      updateCheckboxGroupInput(session, "Y", choices = inputVar())
    })
    list(
      # first checker box input
      checkboxGroupInput(
        "X",
        "Please select the first set of variables (X)",
        choices = names(csvfile()) # Choices should be the column names
      ),
      # second checker box input
      checkboxGroupInput(
        "Y",
        "Please select the second set of variables (Y)",
        choices = names(csvfile()) # Choices should be the column names
      ),
      actionBttn(
        inputId = "submit",
        label = "Run Analysis!",
        color = "danger",
        style = "jelly"
      )
    )
  })

  ############################### This note appear on opening
  output$note1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(HTML(
        paste0(
          " <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Values of variables should be entered as columns </li>
<li>Variable names should be given as column headings </li>
<li>Don't type or delete anything on other cells without data</li>
<li>You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Variable name’ should be written as Variable_name or Variable.name</li>
<li>Short names may be selected for column name as it may look good in path diagram</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank </li>
<li>Type 'NA' in the cell with no observation</li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<h5> You can download a model data set from below and test the App  </h5>
"
        )
      ))
    } else {
      return()
    }
  })

  ############################## download Example data set
  output$data_set <- renderUI({
    if (is.null(input$file1$datapath)) {
      list(
        selectInput(
          "filenames",
          "Choose a dataset:",
          list.files(pattern = ".csv")
        ),
        downloadButton("downloadData", label = "Download csv file to test", class = "butt1")
      )
    } else {
      return()
    }
  })

  datasetInput <- reactive({
    switch(input$filenames,
      filenames
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      input$filenames
    },
    content = function(file) {
      write.csv(
        read.csv
        (input$filenames, header = TRUE, sep = ","),
        file,
        row.names = FALSE
      )
    }
  )
  ######################### end data set download


  ####################################################### Details about model dataset
  output$note2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(HTML(
        paste0(
          "
<ui>
<li>Following are the details about model datasets</li>
<li><b>mmreg.csv</b>: details will be given here</li>
</ui>
</p>

"
        )
      ))
    } else {
      return()
    }
  })
  ###########################

  ############## ANALYSIS BEGINS


  # correlation in the dataset
  output$corrmat1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return() 
      }
      if (input$submit > 0) {
        input$reload
        Sys.sleep(2)
        x <- csvfile()
        cormat <- Hmisc::rcorr(as.matrix(x))
        R <- round(cormat$r, 3)
        p <- cormat$P
        ## Define notions for significance levels; spacing is important.
        mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
        Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
        diag(Rnew) <- paste(diag(R), " ", sep = "")
        row.names(Rnew) <- names(x)
        colnames(Rnew) <- names(x)
        Rnew
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Pearson correlation between variables</b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  ### Adding note under corrmat
  output$note4corrmat <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      HTML(paste0("<p>Significance Indicator: ***0.1%; **1%; *5% level</p>
                    "))
    }
  })


  ##  Testing significance using wilks lambda
  output$wilks <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- dim(X)[1]
        p <- length(X)
        q <- length(Y)
        wilk <- CCP::p.asym(rho, n, p, q, tstat = "Wilks")
        wilk <- as.data.frame(wilk)
        wilk <- wilk[, -1]
        wilk
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Wilks' Lambda, using F-approximation (Rao's F)</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )

  ##  Canonical Correlations
  output$cancorr <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        rho_2 <- rho^2
        cancorr <- cbind(rho, rho_2)
        colnames(cancorr) <- c(
          "Canonical Correlations",
          "Squared Canonical Correlations"
        )
        cancorr
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Canonical Correlation Analysis</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )


  ##  Raw canonical variables in set X
  output$rawU <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        raw <- cc1[3:4]
        raw1 <- raw$xcoef
        raw1 <- as.data.frame(raw1)
        colnames(raw1) <- paste("U", 1:n, sep = "")
        raw1
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Raw Canonical Coefficients for the set X Variables</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )

  ##  Raw canonical variables in set Y
  output$rawV <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        raw <- cc1[3:4]
        raw2 <- raw$ycoef
        raw2 <- as.data.frame(raw2)
        colnames(raw2) <- paste("V", 1:n, sep = "")
        raw2
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Raw Canonical Coefficients for the set Y Variables</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )

  ##  Correlation between X variables and U
  output$load1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        cc2 <- CCA::comput(X, Y, cc1)
        load1 <- cc2$corr.X.xscores
        load1 <- as.data.frame(load1)
        colnames(load1) <- paste("U", 1:n, sep = "")
        load1
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Correlations Between X Variables and their Canonical Variables(U) (Loadings)</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )

  ##  Correlation between Y variables and V
  output$load2 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        cc2 <- CCA::comput(X, Y, cc1)
        load2 <- cc2$corr.Y.yscores
        load2 <- as.data.frame(load2)
        colnames(load2) <- paste("V", 1:n, sep = "")
        load2
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Correlations Between Y Variables and their Canonical Variables (V) (Loadings)</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )

  ##  Correlation between X variables and V
  output$load3 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        cc2 <- CCA::comput(X, Y, cc1)
        load3 <- cc2$corr.X.yscores
        load3 <- as.data.frame(load3)
        colnames(load3) <- paste("V", 1:n, sep = "")
        load3
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Correlations Between X Variables and the opposite group of canonical variates (V) (Loadings)</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )  
  
  
  ##  Correlation between Y variables and U
  output$load4 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        cc1 <- CCA::cc(X, Y)
        rho <- cc1$cor
        n <- length(rho)
        cc2 <- CCA::comput(X, Y, cc1)
        load4 <- cc2$corr.Y.xscores
        load4 <- as.data.frame(load4)
        colnames(load4) <- paste("U", 1:n, sep = "")
        load4
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Correlations Between Y Variables and the opposite group of canonical variates (U) (Loadings)</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )  
  
  
###################################################### Plot 1
  
  plotInput1 <- reactive({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    
    if (input$submit > 0) {
      X <- as.data.frame(subset(csvfile(), select = input$X))
      Y <- as.data.frame(subset(csvfile(), select = input$Y))
      p1<-GGally::ggpairs(X)
      p1
    }
  })
  
  
  ######## UI 
  
  output$plot1 <- renderPlot(
    {
      
      plotInput1()
    },
    bg = "transparent"
  )
  
  
  
  
  #################################### Download Image
  output$image_down <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      list(downloadButton("downloadImage1",
                          label = "Download plot1", class = "butt1"
      ))
    }
    
  })
  
  ###### download plot1
  output$downloadImage1 <- downloadHandler(
    filename = "plot1.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 500, units = "in"
        )
      }
      ggsave(file, plot = plotInput1(), device = device)
    }
  )
  
 ###################################### Plot 1 finish
  
  
   ########################################## Plot 2
  
  plotInput2 <- reactive({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    
    if (input$submit > 0) {
      X <- as.data.frame(subset(csvfile(), select = input$X))
      Y <- as.data.frame(subset(csvfile(), select = input$Y))
      p2<-GGally::ggpairs(Y)
      p2
    }
  })
  
  
  ######## UI 
  
  output$plot2 <- renderPlot(
    {
      
      plotInput2()
    },
    bg = "transparent"
  )
  
  
  
  
  #################################### Download Image
  output$image_down1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      list(downloadButton("downloadImage2",
                          label = "Download plot2", class = "butt1"
      ))
    }
    
  })
  
  ###### download plot2
  output$downloadImage2 <- downloadHandler(
    filename = "plot2.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 500, units = "in"
        )
      }
      ggsave(file, plot = plotInput2(), device = device)
    }
  ) 
 ########################################################### plot 2 finish 
 
  
  ############################################ Download Report
  
  ################################### Download Button
  
  output$var1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      list(
        radioButtons("format", "Download report:", c("HTML"),
                     inline = TRUE
        ),
        downloadButton("downloadReport")
      )
    }
  })
  
  
  
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("grapesCancor", sep = ".", switch(input$format,
                                           HTML = "html"
                                           
      ))
    },
    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      out <- render("report.Rmd", switch(input$format,
                                         HTML = html_document()
                                     ))
      file.rename(out, file)
    }
  )
  
  
  
 ############################### END 
  
  }
