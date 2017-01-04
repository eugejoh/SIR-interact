##############################################
# FUNCTION FOR SETTING PARAMETERS BETA,GAMMA #
##############################################

SIR.model <- function(t, b, g){ # function of t, b and g
	require(deSolve) # call in of the deSolve package
	init <- c(S=1-1e-6,I=1e-6,R=0) #initial conditions of proportions
	parameters <- c(bet=b,gamm=g) #paramters in the ode
	time <- seq(0,t,by=t/(2*length(1:t))) #time sequence for the ode solution
	eqn <- function(time,state,parameters){ #SIR odes
	with(as.list(c(state,parameters)),{ #solve the ode using the parameters
		dS <- -bet*S*I #change in proportion of susceptibles (dS/dt)
		dI <- bet*S*I-gamm*I #change in proportion of infected (dI/dt)
		dR <- gamm*I #change in proportion of the recovered (dR/dt)
		return(list(c(dS,dI,dR)))}) #out as a list containing the values
		}
	out<-ode(y=init,times=time,eqn,parms=parameters) #solve the ode using ode() in deSolve package
	out.df<-as.data.frame(out) #create a data frame of the output of ode()
	require(ggplot2) #call in ggplot2 package
	mytheme4 <- theme_bw() + # assign a theme, all NULL values will default to bw-theme
	theme(text=element_text(colour="black")) + #set all text in the plot to white
	theme(panel.grid = element_line(colour = "white")) + #set grid in plot to white
	theme(panel.background = element_rect(fill = "#B2B2B2")) #set plot bg as grey
	theme_set(mytheme4) #http://docs.ggplot2.org/current/theme_update.html
	title <- bquote("SIR Model: Basic") #title for plot
	subtit<-bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]))) #use of bquote to include Greek symbols of beta and gamma into subtitle
res<-ggplot(out.df,aes(x=time))+ #set plot of ode data frame output and x-variable as time
	ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+ # create the title and subtitle based on http://stackoverflow.com/q/30338719/6168956
	geom_line(aes(y=S,colour="Susceptible"))+ #assign plot line as S from out.df
	geom_line(aes(y=I,colour="Infected"))+ #assign plot line as I from out.df
	geom_line(aes(y=R,colour="Recovered"))+ #assign plot line as R from out.df
	ylab(label="Proportion")+ #y-axis label
	xlab(label="Time (days)")+ #x-axis label
	theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend justification - anchorpoint of legend, legend.position based on two-element numeric vector (x,y)
	theme(legend.title=element_text(size=12,face="bold"), #set font specification of title
	legend.background = element_rect(fill='#FFFFFF',size=0.5,linetype="solid"), #legend background set to white
	legend.text=element_text(size=10), #set legend text size
	legend.key=element_rect(colour="#FFFFFF", #set legend keys border to white
	fill='#C2C2C2', #fill set to gray
	size=0.25, #size of border
	linetype="solid"))+ #line type of border
	scale_colour_manual("Compartments", #title of legend
	breaks=c("Susceptible","Infected","Recovered"), #each level of lines, set to colour
	values=c("blue","red","darkgreen")) #colours for each respective level
	
	print(res) #print output of plot
	ggsave(plot=res, # call plot name
	filename=paste0("SIRplot_","time",t,"beta",b,"gamma",g,".png"), #set the filename with parameters of time, beta and gamma
	width=8,height=6,dpi=180) #dimensions and resolution of .png file
}