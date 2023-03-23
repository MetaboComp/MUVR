#' Plot for comparison of actual model fitness vs permutation
#'
#' Plots histogram of null hypothesis (permutation) distribution, actual model fitness and cumulative p-value.
#' Plot defaults to "greater than" or "smaller than" tests and cumulative probability in Student's t-distribution
#'
#' @param actual Actual model fitness (e.g. Q2, AUROC or number of misclassifications)
#' @param distribution Null hypothesis (permutation) distribution of similar metric as `actual`
#' @param xlab Label for x-axis (e.g. 'Q2 using real value',"Q2 using distributions","BER" 'AUROC', or 'Misclassifications')
#' @param side Cumulative p either "greater" or "smaller" than H0 distribution (defaults to side of median(H0))
#' @param type c('t','non',"smooth","rank","ecdf")
#' @param xlim Choice of user-specified x-limits (if default is not adequate)
#' @param ylim Choice of user-specified y-limits (if default is not adequate)
#' @param breaks Choice of user-specified histogram breaks (if default is not adequate)
#' @param main Choice of user-specified plot title
#' @param permutation_visual choice of showing median or mean or none
#' @param pos Choice of position of p-value label (if default is not adequate)
#' @param curve if add curve or not base on the mid
#' @param extend how many percenrtage of the orignical range do we start
#' @return Plot
#' @export
plotPerm=function(actual,
                  distribution,     ####a distribution
                  xlab=NULL,
                  side=c('greater','smaller'),
                  type="t",
                  #               type=c('t','non',"smooth","ecdf","rank"),
                  xlim,
                  ylim=NULL,
                  breaks='Sturges',
                  pos,  ####Choice of position of p-value label (if default is not adequate)
                  main=NULL,
                  permutation_visual="none",
                  curve=F,
                  extend=0.1
                  ) {

  if(!permutation_visual%in%c("mean","median","none")){stop("this type not supoorted")}
  if(!missing(type)){if(!any(type%in%c('t','non',"smooth","ecdf","rank"))){stop("This type can not be implemented")}}
  if(missing(type)) {type=='t'}

  if(!missing(side)){if(side!="smaller"&side!="greater")stop("This side can not be implemented")}

######when it is Q2 or MISS
  if(!missing(pos)){pos_rev=6-pos}
  if(missing(side)) {side=ifelse(actual<median(distribution),'smaller','greater')}

  if(missing(pos)) {pos<-ifelse(side=='smaller',4,2)
                    pos_rev<-ifelse(side=='smaller',2,4)}
  ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
  ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.

  pP=pPerm(actual,
           distribution,
           side,
           type=type,
           extend=extend)     ####calculate p value
  #if(missing(ylim)) {
  #  xlim =c(0,max(h$density))
  #}

  ran<-range(c(actual,distribution))
  from=ran[1]-diff(ran)*extend
  to=ran[2]+diff(ran)*extend
  if(missing(xlim)) {
    xlim =c(from,to)
  }
  (h=hist(distribution,
          breaks,
          xlim=xlim,
          ylim=ylim,
          axes=F,     ###remove both axes
          xlab=xlab,
          freq=FALSE,   ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
          main=main))

  h2=max(h$density)*.75  ######as estimated density values, This is to decide how high the vertical line will be drawn
  if(curve==T){
    if(type=="smooth"){
      ran<-range(c(actual,distribution))
      from=ran[1]-diff(ran)*extend
      to=ran[2]+diff(ran)*extend

    #dx=density(distribution,
    #           #adjust=0.0001,
    #           from=from,
    #           to=to,
    #           n = 100000)
    #dx_line=density(distribution,
    #           #adjust=0.0001,
    #           from=xlim[1],
    #           to=xlim[2],
    #           n = 100000)

    lines(pP$dens,lwd = 2, col = "red")
    }

    if(type=="t"){
      #e = 1 * diff(range(distribution))
      x_values <- seq(from, to, length = 1000)
      y_values <- dt(x_values,df=length(distribution)-1)
      #y_values <- y_values * diff(h$mids[1:2]) * length(distribution)
      lines(x_values, y_values, lwd = 2,col = "red")

    }
  }

  axis(1,pos=0)   ###the coordinate at which the axis line is to be drawn: if not NA this overrides the value of line.
  if(side=='smaller') axis(2,pos=0,las=1)   #### the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
  else axis(2,pos=h$breaks[1],las=1)

  lines(rep(actual,2),     ###x1,x2 for the line
        c(0,h2))          ##y1 ,y2 forthe line


  if(!is.nan(pP$p)&is.numeric(pP$p)){
  text(actual,    ###x position of the text
       h2,        ##y position of the text
       pos=pos,
       ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
       ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
       labels=paste('p=',signif(pP$p,4),sep='')) ####what is the text
       ##For signif the recognized values of digits are 1...22, and non-missing values are rounded to the nearest integer in that range.
       ##Complex numbers are rounded to retain the specified number of digits in the larger of the components.
       ##Each element of the vector is rounded individually, unlike printing.
  }

  if(!is.nan(pP$p)&!is.numeric(pP$p)){
    text(actual,    ###x position of the text
         h2,        ##y position of the text
         pos=pos,
         ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
         ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
         labels=paste("p",pP$p,sep='')) ####what is the text
    ##For signif the recognized values of digits are 1...22, and non-missing values are rounded to the nearest integer in that range.
    ##Complex numbers are rounded to retain the specified number of digits in the larger of the components.
    ##Each element of the vector is rounded individually, unlike printing.
  }

  if(!is.null(pP$points)){
    ################################ add curves

    #lines(dens)
  }


    text(actual,    ###x position of the text
       max(h$density)*0.003,        ##y position of the text
       pos=3,
       ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
       labels=paste0(signif(actual,4))
  )
if(permutation_visual=="mean"){
  text(median(distribution),    ###x position of the text
       max(h$density)*0.003,        ##y position of the text
       pos=3,
       ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
       labels=paste0("mean=",
                    signif(mean(distribution),4)
                    )
  )
}

  if(permutation_visual=="median"){
    text(median(distribution),    ###x position of the text
         max(h$density)*0.003,        ##y position of the text
         pos=3,
         ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
         labels=paste0("median=",
                       signif(median(distribution),4)
         )
    )
  }

  if(permutation_visual=="none"|missing(permutation_visual)){

  }

}






