# draw a grid in random color shades

rm(list=ls())
library(grid)

#dev.new()


new_color <- function()
   
 {
    # we want random colors so need to remove numbers from string
    colcount <- length(colors())
    mycolors <- colors()
    xcolor <- mycolors[sample(1:colcount,1)]
    xcolor <- gsub("\\d", "", xcolor)
    
 }


check_color <- function(zcolor)
    {
      identical(zcolor %in% colors(),TRUE)
    }


create_art <- function()
  {
    sq <- sample(10:70,1)
   
    dat <- data.frame(x = rep(seq(0, 0.8, 1/10), sq),
		      y = rep(seq(0, 0.8, 1/10), each = sq) )
    
    acolor <- ''
    badcolor=c("black","grey","gray","slategray","darkslategray")

    acolor <- new_color()
         
    while (acolor %in% badcolor)
        {acolor <- new_color()}
     
    dat$col <- paste(acolor,sample(1:4,nrow(dat),replace=TRUE),sep="")
    testdat <- dat$col
    maxdat=nrow(dat)
    for (j in 1:maxdat)
          { 
            testcol=testdat[j]
            if ((check_color(testcol) == FALSE) )
                {
                 print(paste("Error   : ",testcol))
                 testdat[j] <- new_color()
                  while (testdat[j] %in% badcolor)
                    {testdat[j] <- new_color()}
                 print(paste("Updated : ",testdat[j],j))
                 }
            #print (paste("Checked  : ",acolor,testdat[j]))
         
          }
    #dat$col <- sort(testdat) # for an orderly display
    dat$col <- testdat
    okcol <- dat$col 
    
    grid.newpage()
    vp1 <- viewport(x = 0.1, y = 0.1, w = 0.9, h = 0.9, 
        just = c("left", "bottom"), name = "vp1")

    # plotting rectangles 
    try(grid.rect(x=dat$x,y=dat$y,height=1/10,width=1/10,hjust=0,vjust=0,vp=vp1,
	      gp=gpar(col=1, fill=as.character(okcol))),silent=FALSE )
    
    #grid.grill(h=dat$x+0.1, v=dat$y-0.3, gp=gpar(col=acolor))
    
  return (c(acolor))
  }



# note double loop so we jump over error thrown with gpar
# if color_checker code not working
for (z in 1:50){
  
for (k in 1:2)
  {
    print(paste("Run : ",k,z))
    print(create_art())
    Sys.sleep(1)
  } 
#dev.off()
}

########################################

