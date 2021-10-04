library(tcltk)

mywait <- function() {
  tt <- tktoplevel()
  tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
          side='bottom')
  tkbind(tt,'<Key>', function()tkdestroy(tt) )
  
  tkwait.window(tt)
}
