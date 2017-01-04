##############################################
# FUNCTION FOR SETTING PARAMETERS BETA,GAMMA #
##############################################

SIR.model <- function(t, b, g){ # function of t, b and g
	require(deSolve) # use of the deSolve package
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
	require(ggplot2)
	mytheme4 <- theme_bw() + # assign a theme, all NULL values will default to bw-theme
	theme(text=element_text(colour="black")) + #set all text in the plot to white
	theme(panel.grid = element_line(colour = "white")) + #set grid in plot to white
	theme(panel.background = element_rect(fill = "#B2B2B2")) #set plot bg as grey
	theme_set(mytheme4) #http://docs.ggplot2.org/current/theme_update.html
	title <- bquote("SIR Model: Basic")
	subtit<-bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2])))
res<-ggplot(out.df,aes(x=time))+
	ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+ #http://stackoverflow.com/q/30338719/6168956
	geom_line(aes(y=S,colour="Susceptible"))+
	geom_line(aes(y=I,colour="Infected"))+
	geom_line(aes(y=R,colour="Recovered"))+
	theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
	theme(legend.title=element_text(size=12,face="bold"),
	legend.background = element_rect(fill='#FFFFFF',size=0.5,linetype="solid"),
	legend.text=element_text(size=10),
	legend.key=element_rect(colour="#FFFFFF",
	fill='#C2C2C2',
	size=0.25,
	linetype="solid"))+
	scale_colour_manual("Compartments",
	breaks=c("Susceptible","Infected","Recovered"),
	values=c("blue","red","darkgreen"))+
	ylab(label="Proportion")+
	xlab(label="Time (days)")
	print(res)
	ggsave(plot=res,
	filename=paste0("SIRplot_","time",t,"beta",b,"gamma",g,".png"),
	width=8,height=6,dpi=180)
}