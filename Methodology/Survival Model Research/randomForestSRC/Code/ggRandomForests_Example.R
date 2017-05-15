###############################################################################
#
# Project: 	Yerex
# Script:	ggRandomForests_Example.R
# Version:	
# Created:	Mar 22, 2016
# Updated:	Mar 23, 2016
# Author: 	Robert P. Yerex
# Copyright Robert P., Yerex, 2016
###############################################################################
library("ggplot2")         # Graphics engine
library("RColorBrewer")    # Nice color palettes
library("plot3D")          # for 3d surfaces. 
library("dplyr")           # Better data manipulations
library("parallel")        # mclapply for multicore processing
library("tidyr")        # Transforming wide data into long data (gather)
library("knitr")

# Analysis packages.
library("randomForestSRC") # random forest for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

################ Default Settings ##################
theme_set(theme_bw())     # A ggplot2 theme with white background

## Set open circle for censored, and x for events 
event.marks <- c(1, 4)
event.labels <- c(FALSE, TRUE)

## We want red for death events, so reorder this set.
strCol <- brewer.pal(3, "Set1")[c(2,1,3)]

data("pbc", package = "randomForestSRC")

for(ind in 1:dim(pbc)[2]){
	if(!is.factor(pbc[, ind])){
		if(length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 2) {
			if(sum(range(pbc[, ind], na.rm = TRUE) ==  c(0, 1)) ==  2){
				pbc[, ind] <- as.logical(pbc[, ind])
			}
		}
	}else{
		if(length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 2) {
			if(sum(sort(unique(pbc[, ind])) ==  c(0, 1)) ==  2){
				pbc[, ind] <- as.logical(pbc[, ind])
			}
			if(sum(sort(unique(pbc[, ind])) ==  c(FALSE, TRUE)) ==  2){
				pbc[, ind] <- as.logical(pbc[, ind])
			}
		}
	}
	if(!is.logical(pbc[, ind]) & 
			length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 5) {
		pbc[, ind] <- factor(pbc[, ind])
	}
}
# Convert age to years
pbc$age <- pbc$age/364.24
pbc$years <- pbc$days/364.24
pbc <- pbc %>% select(-days)
pbc$treatment <- as.numeric(pbc$treatment)
pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
pbc$treatment <- factor(pbc$treatment)

cls <- sapply(pbc, class) 

labels <- c("Event (F = censor, T = death)", 
		"Treament (DPCA, Placebo)", 
		"Age (years)", 
		"Female = T", 
		"Presence of Asictes", 
		"Presence of Hepatomegaly", 
		"Presence of Spiders", 
		"Edema (0, 0.5, 1)", 
		"Serum Bilirubin (mg/dl)", 
		"Serum Cholesterol (mg/dl)", 
		"Albumin (gm/dl)", 
		"Urine Copper (ug/day)", 
		"Alkaline Phosphatase (U/liter)", 
		"SGOT (U/ml)", 
		"Triglicerides (mg/dl)", 
		"Platelets per cubic ml/1000", 
		"Prothrombin time (sec)", 
		"Histologic Stage", 
		"Time (years)")

dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels, type = cls))

dta.labs <- rbind(dta.labs[nrow(dta.labs),], dta.labs[-nrow(dta.labs),])
st.labs <- as.character(dta.labs$label)
names(st.labs) <- rownames(dta.labs)
tmp <- dta.labs
colnames(tmp) <- c("Variable name", "Description", "Type")

kable(tmp, 
		#format="latex",
		caption = "\\label{T:dataLabs}\\code{pbc} data set variable dictionary.",
		row.names = FALSE,
		booktabs=TRUE)

cnt <- c(which(cls == "numeric" ), which(cls == "integer"))
fct <- setdiff(1:ncol(pbc), cnt)
fct <- c(fct, which(colnames(pbc) == "years"))
dta <- gather(pbc[,fct], variable, value, -years)

# plot panels for each covariate colored by the logical chas variable.
x11()
ggplot(dta, aes(x = years, fill = value)) +
		geom_histogram(color = "black", binwidth = 1) +
		labs(y = "", x = st.labs["years"]) +
		scale_fill_brewer(palette="RdBu",na.value = "white" ) +
		facet_wrap(~variable, scales = "free_y", nrow = 2) +
		theme(legend.position = "none")

cnt <- c(cnt, which(colnames(pbc) == "status"))
dta <- gather(pbc[,cnt], variable, value, -years, -status)

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x = years, y = value, color = status, shape = status)) +
		geom_point(alpha = 0.4) +
		geom_rug(data = dta[which(is.na(dta$value)),], color = "grey50") +
		labs(y = "", x = st.labs["years"], color = "Death", shape = "Death") +
		scale_color_manual(values = strCol) +
		scale_shape_manual(values = event.marks) +
		facet_wrap(~variable, scales = "free_y", ncol = 4) +
		theme(legend.position = c(0.8, 0.2))

pbc.trial <- pbc %>% filter(!is.na(treatment))
st <- apply(pbc,2, function(rw){sum(is.na(rw))})
st.t <- apply(pbc.trial,2, function(rw){sum(is.na(rw))})
st <- data.frame(cbind(full = st, trial = st.t))
st <- st[which(st$full>0),]
colnames(st) <- c("pbc", "pbc.trial")

kable(st, 
		format="latex",
		caption = "\\label{T:missing}Missing value counts in \\code{pbc} data set and pbc clinical trial observations (\\code{pbc.trial}).",
		digits = 3,
		booktabs=TRUE)

		
pbc.bili <- pbc.trial
pbc.bili$bili_grp <- cut(pbc.bili$bili, breaks = c(0, 0.8, 1.3, 3.4, 29))
plot(gg_survival(interval = "years", censor = "status", by = "bili_grp", 
data = pbc.bili), error = "none") +
labs(y = "Survival Probability", x = "Observation Time (years)", 
color = "Bilirubin")
		
		fleming.table <- data.frame(matrix(ncol = 3, nrow = 5))
		rownames(fleming.table) <- 
		c("Age", "log(Albumin)", "log(Bilirubin)", "Edema", "log(Prothrombin Time)")
		colnames(fleming.table) <- c("Coef.", "Std. Err.", "Z stat.")
		fleming.table[,1] <- c(0.0333, -3.0553,0.8792, 0.7847, 3.0157) 
		fleming.table[,2] <- c(0.00866, 0.72408,0.09873,0.29913,1.02380) 
		fleming.table[,3] <- c(3.84,-4.22,8.9,2.62,2.95) 
		
		kable(fleming.table, 
		format="latex",
		caption = "\\label{T:FHmodel}\\code{pbc} proportional hazards model summary of 312 randomized cases in \\code{pbc.trial} data set. ~\\citep[Table 4.4.3c]{fleming:1991} ", 
		digits = 3,
		booktabs=TRUE)


	
		ggRFsrc <- plot(gg_rfsrc(rfsrc_pbc), alpha = 0.2) + 
		scale_color_manual(values = strCol) + 
		theme(legend.position = "none") + 
		labs(y = "Survival Probability", x = "Time (years)") +
		coord_cartesian(ylim = c(-0.01, 1.01))
		show(ggRFsrc)
		 
		plot(gg_rfsrc(rfsrc_pbc, by = "treatment")) +  
		theme(legend.position = c(0.2, 0.2)) + 
		labs(y = "Survival Probability", x = "Time (years)") +
		coord_cartesian(ylim = c(-0.01, 1.01))

		rfsrc_pbc_test <- predict(rfsrc_pbc, newdata = pbc.test,
		na.action = "na.impute")

		rfsrc_pbc_test
 
		plot(gg_rfsrc(rfsrc_pbc_test), alpha=.2) + 
		scale_color_manual(values = strCol) + 
		theme(legend.position = "none") + 
		labs(y = "Survival Probability", x = "Time (years)") +
		coord_cartesian(ylim = c(-0.01, 1.01))
		 
		plot(gg_vimp(rfsrc_pbc), lbls = st.labs) + 
		theme(legend.position = c(0.8, 0.2)) + labs(fill = "VIMP > 0")
		@
		<<nms>>=
		## calculate for document
		ggda <- gg_vimp(rfsrc_pbc)
		@
		The \code{gg_vimp} plot of Figure~\ref{fig:rf-vimp} details VIMP ranking for the \code{pbc.trial} baseline variables, from the largest (\Sexpr{gsub(" \\(mg/dl\\)", "",st.labs[as.character(ggda$vars)[1]])}) at the top, to smallest (\Sexpr{gsub(" \\(mg/dl\\)", "",st.labs[as.character(ggda$vars)[nrow(ggda)]], )}) at the bottom. VIMP measures are shown using bars to compare the scale of the error increase under permutation and colored by the sign of the measure (red for negative values). Note that four of the five highest ranking variables by VIMP match those selected by the~\cite{fleming:1991} model listed in Table~\ref{T:FHmodel}, with urine copper (2) ranking higher than age (8). We will return to this in Section~\ref{S:modelSelection}.
		
		\subsection[Minimal Depth]{Minimal Depth (\code{gg\_minimal\_depth})}\label{S:minimalDepth}
		
		In VIMP, prognostic risk factors are determined by testing the forest prediction under alternative data settings, ranking the most important variables according to their impact on predictive ability of the forest. An alternative method uses inspection of the forest construction to rank variables. \emph{Minimal depth}~\citep{Ishwaran:2010, Ishwaran:2011} assumes that variables with high impact on the prediction are those that most frequently split nodes nearest to the root node, where they partition the largest samples of the population. 
		
		Within each tree, node levels are numbered based on their relative distance to the root of the tree (with the root at 0). Minimal depth measures important risk factors by averaging the depth of the first split for each variable over all trees within the forest. The assumption in the metric is that smaller minimal depth values indicate the variable separates large groups of observations, and therefore has a large impact on the forest prediction.
		
		In general, to select variables according to VIMP, we examine the VIMP values, looking for some point along the ranking where there is a large difference in VIMP measures. Given minimal depth is a quantitative property of the forest construction, \cite{Ishwaran:2010} also derive an analytic threshold for evidence of variable impact. A simple optimistic threshold rule uses the mean of the minimal depth distribution, classifying variables with minimal depth lower than this threshold as important in forest prediction. 
		
		The \pkg{randomForestSRC} \code{var.select} function uses the minimal depth methodology for variable selection, returning an object with both minimal depth and vimp measures. The \pkg{ggRandomForests} \code{gg_minimal_depth} function is analogous to the \code{gg_vimp} function. Variables are ranked from most important at the top (minimal depth measure), to least at the bottom (maximal minimal depth). 
		
		<<mindepth-view, eval=FALSE, echo=TRUE>>= 
		varsel_pbc <- var.select(rfsrc_pbc)
		gg_md <- gg_minimal_depth(varsel_pbc, lbls = st.labs)
		print(gg_md)
		@
		
		<<mindepth-load>>= 
		data("varsel_pbc", package = "ggRandomForests")
		gg_md <- gg_minimal_depth(varsel_pbc)
		gg_md
		@
		
		The \code{gg_minimal_depth} summary mostly reproduces the output from the \code{var.select} function from the \pkg{randomForestSRC} package. We report the minimal depth threshold (\code{threshold} \Sexpr{round(gg_md$md.obj$threshold, digits=3)}) and the number of variables with depth below that threshold (\code{model size} \Sexpr{gg_md$modelsize}). We also list a table of the top (\Sexpr{gg_md$modelsize}) selected variables, in minimal depth rank order with the associated VIMP measures. The minimal depth numbers indicate that \code{bili} tends to split between the first and second node level, and the next three variables (\code{albumin}, \code{copper}, \code{prothrombin}) split between the second and third levels on average.
		
		<<mindepth-plot, echo=TRUE, fig.cap="Minimal Depth variable selection. Low minimal depth indicates important variables. The dashed line is the threshold of maximum value for variable selection.", fig.width=5>>= 
		plot(gg_md, lbls = st.labs)
		@
		
		The \code{gg_minimal_depth} plot of Figure~\ref{fig:mindepth-plot} is similar to the \code{gg_vimp} plot in Figure~\ref{fig:rf-vimp}, ranking variables from most important at the top (minimal depth measure), to least at the bottom (maximal minimal depth). The vertical dashed line indicates the minimal depth threshold where smaller minimal depth values indicate higher importance and larger values indicate lower importance.
		
		\subsection{Variable selection comparison}\label{S:modelSelection}
		
		Since the VIMP and Minimal Depth measures use different criteria, we expect the variable ranking to be somewhat different. We use \code{gg_minimal_vimp} function to compare rankings between minimal depth and VIMP in Figure~\ref{fig:depthVimp}.
		
		<<depthVimp, fig.cap="Comparing Minimal Depth and Vimp rankings. Points on the red dashed line are ranked equivalently, points above have higher VIMP ranking, those below have higher minimal depth ranking.", fig.width=5, echo=TRUE>>= 
		plot(gg_minimal_vimp(gg_md), lbls = st.labs) +
		theme(legend.position=c(0.8, 0.2))
		@
		
		The points along the red dashed line indicate where the measures are in agreement. Points above the red dashed line are ranked higher by VIMP than by minimal depth, indicating the variables are more sensitive to misspecification. Those below the line have a higher minimal depth ranking, indicating they are better at dividing large portions of the population. The further the points are from the line, the more the discrepancy between measures. 
		
		<<models>>=
		fleming.table$nm <- c("age","albumin", "bili","edema", "prothrombin")
		fh.model <- data.frame(cbind(names = fleming.table$nm, 
		FH = order(abs(fleming.table$`Z stat.`), 
		decreasing = TRUE),
		Variable=rownames(fleming.table), 
		Coeff=fleming.table$Coef.
		))
		gg_v <- gg_vimp(rfsrc_pbc)
		gg_v$rank <- 1:nrow(gg_v)
		rownames(gg_v) <- gg_v$vars
		md <- data.frame(cbind(names=gg_md$topvars))
		md$rank <- 1:nrow(md)
		rownames(md) <- gg_md$topvars
		
		md$vimp <- gg_v[rownames(md),]$rank
		md <- left_join(md, fh.model, by = "names")
		md <- md[,c(1, 4, 2,3)]
		colnames(md) <- c("Variable", "FH","Min depth", "VIMP" )
		kable(md, 
		format="latex",
		caption = "\\label{T:modelComp}Comparison of variable selection criteria. Minimal depth ranking, VIMP ranking and ~\\cite{fleming:1991} (FH) proportional hazards model ranked according to \\code{abs(Z stat)} from Table~\\ref{T:FHmodel}.", 
		align=c("l", "r","r","r"),
		digits = 3,
		row.names = FALSE,
		booktabs=TRUE)
		@
		
		
		We examine the ranking of the different variable selection methods further in Table~\ref{T:modelComp}. We can use the Z statistic from Table~\ref{T:FHmodel} to rank variables selected in the~\cite{fleming:1991} model to compare with variables selected by minimal depth and VIMP. The table is constructed by taking the \Sexpr{nrow(gg_md)} top ranked minimal depth variables (below the selection threshold) and matching the VIMP ranking and~\cite{fleming:1991} model transforms. We see all three methods indicate a strong relation of serum bilirubin to survival, and overall, the minimal depth and VIMP rankings agree reasonably well with the~\cite{fleming:1991} model. 
		
		The minimal depth selection process reduced the number of variables of interest from~\Sexpr{ncol(pbc)-2} to \Sexpr{length(varsel_pbc$topvars)}, which is still a rather large subset of interest. An obvious selection set is to examine the five variables selected by~\cite{fleming:1991}. Combining the Minimal Depth and~\cite{fleming:1991} model, there may be evidence to keep the top 7 variables.  Though minimal depth does not indicate the \code{edema} variable is very interesting, VIMP ranking does agree with the proportional hazards model, indicating we might not want to remove the \code{edema} variable. Both minimal depth and VIMP suggest including \code{copper}, a measure associated with liver disease. 
		
		Regarding the \code{chol} variable, recall missing data summary of Table~\ref{T:missing}. In in the trial data set, there were 28 observations missing \code{chol} values. The forest imputation randomly sorts observations with missing values into daughter nodes when using the \code{chol} variable, which is also how \pkg{randomForestSRC} calculates VIMP. We therefore expect low values for VIMP when a variable has a reasonable number of missing values.
		
		Restricting our remaining analysis to the five~\cite{fleming:1991} variables, plus the \code{copper} retains the biological sense of these analysis. We will now examine how these six variables are related to survival using variable dependence methods to determine the direction of the effect and verify that the log transforms used by~\cite{fleming:1991} are appropriate. 
		
		\section{Variable dependence}\label{S:dependence}
		
		As random forest is not parsimonious, we have used minimal depth and VIMP to reduce the number of variables to a manageable subset. Once we have an idea of which variables contribute most to the predictive accuracy of the forest, we would like to know how the response depends on these variables.
		
		Although often characterized as a \emph{black box} method, the forest predictor is a function of the predictor variables $\hat{f}_{RF} = f(x).$ We use graphical methods to examine the forest predicted response dependency on covariates. We again have two options, variable dependence plots (Section~\ref{S:variabledependence}) are quick and easy to generate, and partial dependence plots (Section~\ref{S:partialdependence}) are more computationally intensive but give us a risk adjusted look at variable dependence. 
		
		\subsection[Variable Dependence]{Variable Dependence (\code{gg\_variable})}\label{S:variabledependence}
		
		\emph{Variable dependence} plots show the predicted response relative to a covariate of interest, with each training set observation represented by a point on the plot. Interpretation of variable dependence plots can only be in general terms, as point predictions are a function of all covariates in that particular observation. 
		
		Variable dependence is straight forward to calculate, involving only the getting the predicted response for each observation. In survival settings, we must  account for the additional dimension of time. We plot the response at specific time points of interest, for example survival at 1 or 3 years.
		<<rfsrc-plot3Mnth, echo=TRUE, fig.cap="Random forest predicted survival (Figure~\\ref{fig:rfsrc-plot}) with vertical dashed lines indicate the 1 and 3 year survival estimates.">>= 
		ggRFsrc + geom_vline(aes(xintercept = 1), linetype = "dashed") + 
		geom_vline(aes(xintercept = 3), linetype = "dashed") + 
		coord_cartesian(xlim = c(0, 5))
		@
		The \code{gg_rfsrc} of Figure~\ref{fig:rfsrc-plot3Mnth} identical to Figure~\ref{fig:rfsrc-plot} (stored in the \code{ggRFsrc} variable) with the addition of a vertical dashed line at the 1 and 3 year survival time. A variable dependence plot is generated from the predicted response value of each survival curve at the intersecting time line plotted against covariate value for that observation. This can be visualized as taking a slice of the predicted response at each time line, and spreading the resulting points out along the variable of interest.
		
		The \code{gg_variable} function extracts the training set variables and the predicted OOB response from \code{rfsrc} and \code{predict} objects. In the following code block, we store the \code{gg_variable} data object for later use (\code{gg_v}), as all remaining variable dependence plots can be constructed from this object. 
		<<variable-plotbili, echo=TRUE, fig.cap="Variable dependence of survival at 1 and 3 years on \\code{bili} variable. Individual cases are marked with blue circles (alive or censored) and red `x's (dead). Loess smooth curve with shaded 95\\% confidence band indicates decreasing survival with increasing bilirubin.", fig.height=4>>= 
		gg_v <- gg_variable(rfsrc_pbc, time = c(1, 3), 
		time.labels = c("1 Year", "3 Years"))
		
		plot(gg_v, xvar = "bili", alpha = 0.4) + #, se=FALSE
		labs(y = "Survival", x = st.labs["bili"]) + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) +
		coord_cartesian(ylim = c(-0.01, 1.01))
		@
		The \code{gg_variable} plot of Figure~\ref{fig:variable-plotbili} shows variable dependence for the Serum Bilirubin (\code{bili}) variable. Again censored cases are shown as blue circles, events are indicated by the red `x' symbols. Each predicted point is dependent on the full combination of all other covariates, not only on the covariate displayed in the dependence plot. The smooth loess line~\citep{cleveland:1981, cleveland:1988} indicates the trend of the prediction over the change in the variable.

Examination of Figure~\ref{fig:variable-plotbili} indicates most of the cases are grouped in the lower end of \code{bili} values. We also see that most of the higher values experienced an event. The ``normal'' range of Bilirubin is from 0.3 to 1.9 mg/dL, indicating the distribution from our population is well outside the normal range. These values make biological sense considering Bilirubin is a pigment created in the liver, the organ effected by the PBC disease. The figure also shows that the risk of death increases as time progresses. The risk at 3 years is much greater than that at 1 year for patients with high Bilirubin values compared to those with values closer to the normal range.

The \code{plot.gg_variable} function call operates on the \code{gg_variable} object controlled by the list of variables of interest in the \code{xvar} argument. By default, the \code{plot.gg_variable} function returns a list of \code{ggplot} objects, one figure for each variable named in \code{xvar}. The remaining arguments are passed to internal \pkg{ggplot2} functions controlling the display of the figure. The \code{se} argument is passed to the internal call to \code{geom_smooth} for fitting smooth lines to the data. The \code{alpha} argument lightens the coloring points in the \code{geom_point} call, making it easier to see point over plotting. We also demonstrate modification of the plot labels using the \code{labs} function and point attributes with the \code{scale_} functions.

An additional \code{plot.gg_variable} argument (\code{panel = TRUE}) can be used to combine multiple variable dependence plots into a single figure. In the following code block, we plot the remaining continuous variables of interest found in Section~\ref{S:modelSelection}. 
<<variable-plot, echo=TRUE, fig.cap="Variable dependence of predicted survival at 1 and 3 years on continuous variables of interest. Individual cases are marked with blue circles for censored cases and red `x's for death events. Loess smooth curve indicates the survival trend with increasing values.", fig.height=4, fig.width=7>>= 
		xvar <- c("bili", "albumin", "copper", "prothrombin", "age")
xvar.cat <- c("edema")

plot(gg_v, xvar = xvar[-1], panel = TRUE, alpha = 0.4) + #se = FALSE, span=1
		labs(y = "Survival") + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) +
		coord_cartesian(ylim = c(-0.05, 1.05))
@
		The \code{gg_variable} plot in Figure~\ref{fig:variable-plot} displays a panel of the remaining continuous variable dependence plots. The panels are sorted in the order of variables in the \code{xvar} argument and include a smooth loess line~\citep{cleveland:1981,cleveland:1988} to indicate the trend of the prediction dependence over the covariate values. The \code{se=FALSE} argument turns off the loess confidence band, and the \code{span=1} argument controls the degree of smoothing.

The figures indicate that survival increases with \code{albumin} level, and decreases with \code{bili}, \code{copper}, \code{prothrombin} and \code{age}. Note the extreme value of \code{prothrombin} (> 16) influences the loess curve more than other points, which would make it a candidate for further investigation.

We expect survival at 3 years to be lower than at 1 year. However, comparing the two time plots for each variable does indicate a difference in response relation for \code{bili}, \code{copper} and \code{prothrombine}. The added risk for high levels of these variables at 3 years indicates a non-proportional hazards response. The similarity between the time curves for \code{albumin} and \code{age} indicates the effect of these variables is constant over the disease progression.

There is not a convenient method to panel scatter plots and boxplots together, so we recommend creating panel plots for each variable type separately. We plot the categorical variable (\code{edema}) in Figure~\ref{fig:variable-plotCat} separately from the continuous variables in Figure~\ref{fig:variable-plot}.

<<variable-plotCat, echo=TRUE, fig.cap="Variable dependence of survival 1 and 3 years on \\code{edema} categorical variable. Symbols with blue circles indicate censored cases and red `x's indicate death events. Boxplots indicate distribution of predicted survival for all observations within each \\code{edema} group.", fig.height=4>>= 
		plot(gg_v, xvar = xvar.cat, alpha = 0.4) + labs(y = "Survival") + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) +
		coord_cartesian(ylim = c(-0.01, 1.02))
@
		The \code{gg_variable} plot of Figure~\ref{fig:variable-plotCat} for categorical variable dependence displays boxplots to examine the distribution of predicted values within each level of the variable. The points are plotted with a jitter to see the censored and event markers more clearly. The boxes are shown with horizontal bars indicating the median, 75th (top) and 25th (bottom) percentiles. Whiskers extend to 1.5 times the interquartile range. Points plotted beyond the whiskers are considered outliers. 

When using categorical variables with linear models, we use boolean dummy variables to indicate class membership. In the case of \code{edema}, we would probably create two logical variables for \code{edema = 0.5} (complex Edema presence indicator) and \code{edema = 1.0} (Edema with diuretics) contrasted with the \code{edema = 0} variable (no Edema). Random Forest can use factor variables directly, separating the populations into homogeneous groups of \code{edema} at nodes that split on that variable. Figure~\ref{fig:variable-plotCat} indicates similar survival response distribution between 1 and 3 year when \code{edema = 1.0}. The distribution of predicted survival does seem to spread out more than for the other values, again indicating a possible non-proportional hazards response.

\subsection[Partial Dependence]{Partial Dependence (\code{gg\_partial})}\label{S:partialdependence}

\emph{Partial dependence} plots are a risk adjusted alternative to variable dependence. Partial plots are generated by integrating out the effects of variables beside the covariate of interest. The figures are constructed by selecting points evenly spaced along the distribution of the variable of interest. For each of these points ($X = x$), we calculate the average RF prediction over all remaining covariates in the training set by
\begin{equation}
\tilde{f}(x) = \frac{1}{n} \sum_{i = 1}^n \hat{f}(x, x_{i, o}), 
\label{E:partial}
\end{equation}
where $\hat{f}$ is the predicted response from the random forest and $x_{i, o}$ is the value for all other covariates other than $X = x$ for observation $i$~\citep{Friedman:2000}. 

Generating partial dependence data is effectively averaging the response for a series of nomograms constructed for each observation by varying the variable of interest. The operation is computationally intensive, especially when there are a large number of observations. The default parameters for the \code{plot.variable} function generate partial dependence estimates at \code{npts = 25} points along the variable of interest. For each point of interest, the \code{plot.variable} function averages the \code{n} response predictions. This process is repeated for each of the variables of interest. 

For time to event data, we also have to deal with the additional time dimension, as with variable dependence. The following code block uses the \code{mclapply} function from the \pkg{parallel} package to run the \code{plot.variable} function for three time points (\code{time}=1, 3 and 5 years) in parallel. For RSF models, we calculate a risk adjusted survival estimates (\code{surv.type="surv"}), suppressing the internal base graphs (\code{show.plots = FALSE}) and store the point estimates in the \code{partial_pbc} \code{list}.
<<pbc-partial, echo=TRUE, eval=FALSE>>= 
		xvar <- c(xvar, xvar.cat)
partial_pbc <- mclapply(c(1,3,5), function(tm){
			plot.variable(rfsrc_pbc, surv.type = "surv", time = tm, xvar.names = xvar,
					partial = TRUE, show.plots = FALSE)
		})
@

<<pbc-partial-load>>= 
		data("partial_pbc", package = "ggRandomForests")
xvar <- c(xvar, xvar.cat)
@
		
		Because partial dependence data is collapsed onto the risk adjusted response, we can show multiple time curves on a single panel. The following code block converts the \code{plot.variable} output into a list of \code{gg_partial} objects, and then combines these data objects, with descriptive labels, along each variable of interest using the \code{combine.gg_partial} function.
<<pbc-partial-bili, echo=TRUE>>= 
		gg_dta <- mclapply(partial_pbc, gg_partial)
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], 
		lbls = c("1 Year", "3 Years"))
@
		
		We then segregate the continuous and categorical variables, and generate a panel plot of all continuous variables in the \code{gg_partial} plot of Figure~\ref{fig:pbc-partial-panel}. The panels are ordered by minimal depth ranking. Since all variables are plotted on the same Y-axis scale, those that are strongly related to survival make other variables look flatter. The figures also confirm the strong non-linear contribution of these variables. Non-proportional hazard response is also evident in at least the \code{bili} and \code{copper} variables by noting the divergence of curves as time progresses.
<<pbc-partial-panel, echo=TRUE, fig.cap="Partial dependence of predicted survival at 1 year (red circle) and 3 years (blue triangle) as a function continuous variables of interest. Symbols are partial dependence point estimates with loess smooth line to indicate trends.", fig.width=5, fig.height=5>>= 
		ggpart <- pbc_ggpart
ggpart$edema <- NULL

plot(ggpart, panel = TRUE) + #, se = FALSE
		labs(x = "", y = "Survival", color = "Time", shape = "Time") +
		theme(legend.position = c(0.8, 0.2)) + 
		coord_cartesian(ylim = c(25, 101))
@
		
		Categorical partial dependence is displayed as boxplots, similar to categorical variable dependence. Risk adjustment greatly reduces the spread of the response as expected, and may also move the mean response compared to the unadjusted results. The categorical \code{gg_partial} plot of Figure~\ref{fig:pbc-partial-edema} indicates that, adjusting for other variables, survival decreases with rising \code{edema} values. We also note that the risk adjusted distribution does spread out as we move further out in time.
<<pbc-partial-edema, echo=TRUE, fig.cap="Partial dependence plot of predicted survival at 1 year (red) and 3 years (blue) as a function of \\code{edema} groups (categorical variable). Boxplots indicate distribution within each group.">>= 
		ggplot(pbc_ggpart[["edema"]], aes(y=yhat, x=edema, col=group))+
		geom_boxplot(notch = TRUE, 
				outlier.shape = NA) + # panel=TRUE, 
		labs(x = "Edema", y = "Survival (%)", color="Time", shape="Time") +
		theme(legend.position = c(0.2, 0.2)) +
		coord_cartesian(ylim = c(25, 101))
@
		
		Partial dependence is an extrapolation operation. By averaging over a series of nomograms, the algorithm constructs observations for all values of the variable of interest, regardless of the relation with other variables. In contrast, variable dependence only uses observations from within the training set. A simple example would be for a model including BMI, weight and height. When examining partial dependence of BMI, the algorithm only manipulates BMI values, height or weight values. The averaging operation is then confounded in two directions. First, dependence on height and weight is shared with BMI, making it difficult to see the true response dependence. Second, partial dependence is calculated over nomograms that can not physically occur. For simple variable combinations, like BMI, it is not difficult to recognize this and modify the independent variable list to avoid these issues. However, care must be taken when interpreting more complex biological variables.

\subsection{Partial dependence as a function of time}\label{S:timeSurface}

In the previous section, we calculated risk adjusted (partial) dependence at two time points (1 and 3 years). The selection of these points can be driven by biological times of interest (i.e., 1 year and 5 year survival in cancer studies) or by investigating time points of interest from a \code{gg_rfsrc} prediction plot. We typically restrict generating \code{gg_partial} plots to the variables of interest at two or three time points of interest due to computational constraints. 

It is instructive to see a more detailed map of the risk adjusted response to get a feel for interpreting partial and variable dependence plots. In Figure~\ref{fig:pbc-partial-panel}, we can visualize the two curves as extending into the plane of the page along a time axis. Filling in more partial dependence curves, it is possible to create a partial dependence surface. 

For this exercise, we will generate a series of 50 \code{gg_partial} plot curves for the \code{bili} variable. To fill the surface in, we also increased the number of points along the distribution of \code{bili} to \code{npts=50} to create a grid of $50 \times 50$ risk adjusted estimates of survival along time in one dimension and the \code{bili} variable in the second.

<<timeSurface3d, fig.cap="Partial dependence surface. Partial dependence of predicted survival (0 to 5 years) as a function of \\code{bili}. Blue lines indicate partial dependence at 1 and 3 years, as in \\code{bili} panel of Figure~\\ref{fig:pbc-partial-panel}.", fig.width=7, fig.height=5, echo=FALSE>>= 
# Restrict the time of interest to less than 5 years.
		time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]

# Find the 50 points in time, evenly space along the distribution of 
# event times for a series of partial dependence curves
time_cts <-quantile_pts(time_pts, groups = 50)

# Load the stored partial coplot data.
data("partial_pbc_time")

# We need to attach the time points of interest to our data.
time.tmp <- do.call(c,lapply(time_cts, 
				function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to gg_partial
partial_time <- do.call(rbind,lapply(partial_pbc_time, gg_partial))

# attach the time data to the gg_partial_coplot
partial_time$time <- time.tmp

# Modify the figure margins to make it larger
par(mai = c(0.5,0.55,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_time, c("time", "bili", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = heat.colors(25),
		colkey = FALSE, border = "black", bty = "b2", 
		shade = 0.5, expand = 0.5, theta=110, phi=15,
		lighting = TRUE, lphi = -50, ticktype="detailed",
		ylab = "Bilirubin", xlab = "Time", zlab = "Survival"
)

# Extract the 1 and 3 year points.
# Find the indices of the points closest in time
t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
indx <- vector("list", length=2)
indx[[1]] <- which(abs(srf$x - 1) < t.pts[1]+1.e-5)
indx[[2]] <- which(abs(srf$x - 3) < t.pts[2]+1.e-5)

# Generate curves along 1 and 3 year partial dependence 
alt <- lapply(indx, function(ind){
			lines3D(x=srf$x[ind], y=srf$y[ind],z=srf$z[ind],
					add=TRUE, col="blue", lwd=6)
		})
@
		
		The \code{gg_partial} surface of Figure~\ref{fig:timeSurface3d} was constructed using the \code{surf3D} function from the \pkg{plot3D} package~\citep[\url{http://CRAN.R-project.org/package=plot3D}]{plot3D:2014}. Source code for generating this figure is shown in Appendix~\ref{A:TimeDomain}. 

The figure shows partial dependence of survival (Z-axis) as a function of \code{bili} over a five year follow up time period. Lines perpendicular to the Bilirubin axis are distributed along the \code{bili} variable. Lines parallel to the Bilirubin axis are taken at 50 training set event times, the first event after $t=0$ at the back to last event before $t=5$ years at the front. The distribution of the time lines is also evenly selected using the same procedure as selecting points for partial dependence curves. 

The 2500 estimated partial dependence points are joined together with a simple straight line interpolation to create the surface, colored according to the survival estimates (yellow close to 1, red for lower values) to aid the visualization of 3 dimensions on a 2 dimensional page. The blue lines in Figure~\ref{fig:timeSurface3d} correspond to the 1 and 3 year partial dependence, as shown in the \code{bili} panel of Figure~\ref{fig:pbc-partial-panel}. 

Viewed as a surface, we see how the partial dependence changes with time. For low values of \code{bili}, survival decreases at a constant rate. For higher values, the rate seems constant until somewhere near 2 years, where it increases rapidly before slowing again as we approach the 5 year point. 
%' 
		%' \section{Variable Interactions}\label{S:interactions}
		%' 
		%' We could stop with the results that our RF analysis has found these six variables to be important in predicting survival. Where the survival response is decreasing with increasing \code{bili}, \code{copper}, \code{prothrombin}, \code{age} and \code{edema} and increasing with increasing \code{albumin}. These results agree with the sign of the~\cite{fleming:1991} model coefficients shown in Table~\ref{T:FHmodel}. The \code{gg_partial} plot in Figure~\ref{fig:pbc-partial-panel} supports the \code{log} transform of \code{bili}, \code{albumin} and \code{prothrombin} and suggest a similar transform for including the \code{copper} variable in a proportional hazards model. The \code{age} variable does seem to have a more linear response than the other continuous variables, and using dummy variables for \code{edema} would preclude the need for a transformation.
		%' 
		%' Using minimal depth, it is also possible to calculate measures of pairwise interactions among variables. Recall that minimal depth measure is defined by averaging the tree depth of variable $i$ relative to the root node. To detect interactions, this calculation can be modified to measure the minimal depth of a variable $j$ with respect to the maximal subtree for variable $i$~\citep{Ishwaran:2010,Ishwaran:2011}.
		%' 
		%' The \code{randomForestSRC::find.interaction} function traverses the forest, calculating all pairwise minimal depth interactions, and returns a $p \times p$ matrix of interaction measures. The diagonal terms are normalized to the root node, and off diagonal terms are normalized measures of pairwise variable interaction. 
		%' 
		%' <<interaction-show, echo=TRUE, eval=FALSE>>= 
		%' ggint <- gg_interaction(rfsrc_pbc)
		%' @
		%' 
		%' <<interaction>>= 
		%' data(interaction_pbc, package = "ggRandomForests")
		%' ggint <- gg_interaction(interaction_pbc)
		%' @
		%' 
		%' The \code{gg_interaction} function wraps the \code{find.interaction} matrix for use with the \pkg{ggRandomForests} plot and print functions. The \code{xvar} argument is used to restrict the variables of interest and the \code{panel = TRUE} argument displays the results in a single figure.
		%' <<interactionPanel, echo=TRUE, fig.cap="Minimal depth variable interaction plot for six variables of interest. Higher values indicate lower interactivity with target variable marked in red.", fig.width=7, fig.height=5>>= 
		%' plot(ggint, xvar = xvar)
		%' @
		%' 
		%' The \code{gg_interaction} plots in Figure~\ref{fig:interactionPanel} show interactions for the target variable (shown with the red cross) with interaction scores for all remaining variables. We expect the covariate with lowest minimal depth (\code{bili}) to be associated with almost all other variables, as it typically splits close to the root node, so viewed alone it may not be as informative as looking at a collection of interactive depth plots. Scanning across the panels, we see each successive target depth increasing, as expected. We also see the interactive variables increasing with increasing target depth. 
		
		\section{Conditional dependence plots}\label{S:coplots}

Conditioning plots (coplots)~\citep{chambers:1992,cleveland:1993}  are a powerful visualization tool to efficiently study how a response depends on two or more variables~\citep{cleveland:1993}. The method allows us to view data by grouping observations on some conditional membership. The simplest example involves a categorical variable, where we plot our data conditional on class membership, for instance on groups of the \code{edema} variable. We can view a coplot as a stratified variable dependence plot, indicating trends in the RF prediction results within panels of group membership.

Interactions with categorical data can be generated directly from variable dependence plots. Recall the variable dependence for bilirubin shown in Figure~\ref{fig:variable-plotbili}. We recreated the \code{gg_variable} plot in  Figure~\ref{fig:var_dep}, modified by adding a linear smooth as we intend on segregating the data along conditional class membership.
<<var_dep, echo=TRUE, fig.cap="Variable dependence of survival at 1 year against \\code{bili} variable. Reproduction of top panel of Figure~\\ref{fig:variable-plotbili} with a linear smooth to indicate trend.">>= 
# Get variable dependence at 1 year
		ggvar <- gg_variable(rfsrc_pbc, time = 1)

# For labeling coplot membership
ggvar$edema <- paste("edema = ", ggvar$edema, sep = "")

# Plot with linear smooth (method argument)
var_dep <- plot(ggvar, xvar = "bili", 
				alpha = 0.5) + 
#  geom_smooth(method = "glm",se = FALSE) +
		labs(y = "Survival", 
				x = st.labs["bili"]) + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) +
		coord_cartesian(y = c(-.01,1.01))

var_dep
@
		
		We can view the conditional dependence of survival against bilirubin, conditional on \code{edema} group membership (categorical variable) in Figure~\ref{fig:coplot_bilirubin} by reusing the saved \code{ggplot} object (\code{var_dep}) and adding a call to the \code{facet_grid} function.
<<coplot_bilirubin, echo=TRUE, fig.cap="Variable dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{edema} group membership. Linear smooth indicates trend of variable dependence.", fig.width=7>>= 
		var_dep + facet_grid(~edema)
@
		
		Comparing Figure~\ref{fig:var_dep} with conditional panels of Figure~\ref{fig:coplot_bilirubin}, we see the overall response is similar to the \code{edema=0} response. The survival for \code{edema=0.5} is slightly lower, though the slope of the smooth indicates a similar relation to \code{bili}. The \code{edema=1} panel shows that the survival for this (smaller) group of patients is worse, but still follows the trend of decreasing with increasing \code{bili}.

Conditional membership within a continuous variable requires stratification at some level. We can sometimes make these stratification along some feature of the variable, for instance a variable with integer values, or 5 or 10 year age group cohorts. However with our variables of interest, there are no logical stratification indications. Therefore we arbitrarily stratify our variables into 6 groups of roughly equal population size using the \code{quantile_cuts} function. We pass the break points located by \code{quantile_cuts} to the \code{cut} function to create grouping intervals, which we can then add to the \code{gg_variable} object before plotting with the \code{plot.gg_variable} function. This time we use the \code{facet_wrap} function to generate the panels grouping interval, which automatically sorts the six panels into two rows of three panels each. 

<<albumin-coplot, fig.cap="Variable dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{albumin} interval group membership.", fig.width=7, fig.height=4, echo=TRUE>>= 
# Find intervals with similar number of observations and create groups.
		albumin_cts <- quantile_pts(ggvar$albumin, groups = 6, intervals = TRUE)
ggvar$albumin_grp <- cut(ggvar$albumin, breaks = albumin_cts)

# Adjust naming for facets
levels(ggvar$albumin_grp) <- paste("albumin =", levels(ggvar$albumin_grp))

plot(ggvar, xvar = "bili", alpha = 0.5) +  #method = "glm", , se = FALSE
		labs(y = "Survival", x = st.labs["bili"]) + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) + 
		facet_wrap(~albumin_grp) +
		coord_cartesian(y = c(-.01,1.01))
@
		The \code{gg_variable} coplot of Figure~\ref{fig:albumin-coplot} indicates that the effect of \code{bili} decreases conditional on membership within increasing \code{albumin} groups. To get a better feel for how the response depends on both these variables together, it is instructive to look at the compliment coplot of \code{albumin} conditional on membership in \code{bili} groups. We repeat the previous coplot process, predicted survival as a function of the \code{albumin} variable, conditional on membership within 6 groups \code{bili} intervals. As the code to create the coplot of Figure~\ref{fig:bili-coplot} is nearly identical to the code for creating Figure~\ref{fig:albumin-coplot}, we include the source code for this figure in Appendix~\ref{A:biliCoplot}. 

<<bili-coplot, fig.cap="Variable dependence coplot of survival at 1 year against \\code{albumin}, conditonal on \\code{bili} interval group membership.", fig.width=7, fig.height=4, echo=FALSE, results=FALSE>>= 
# Find intervals with similar number of observations.
		bili_cts <-quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)

# We need to move the minimal value so we include that observation
bili_cts[1] <- bili_cts[1] - 1.e-7

# Create the conditional groups and add to the gg_variable object
bili_grp <- cut(ggvar$bili, breaks = bili_cts)
ggvar$bili_grp <- bili_grp

# Adjust naming for facets
levels(ggvar$bili_grp) <- paste("bilirubin =", levels(bili_grp))

# plot.gg_variable
plot(ggvar, xvar = "albumin", alpha = 0.5) +
#     method = "glm", se = FALSE) + 
		labs(y = "Survival", x = st.labs["albumin"]) + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) + 
		facet_wrap(~bili_grp) +
		coord_cartesian(ylim = c(-0.01,1.01))
@
		The \code{gg_variable} coplot of Figure~\ref{fig:bili-coplot} indicates the probability of survival increases with increasing \code{albumin} and increases within groups of increasing \code{bili}.

Typically, conditional plots for continuous variables include overlapping intervals along the grouped variable~\citep{cleveland:1993}. We chose to use mutually exclusive continuous variable intervals for the following reasons:
		\begin{itemize}
\item Simplicity - We can create the coplot figures directly from the \code{gg_variable} object by adding a conditional group column directly to the object.

\item Interpretability - We find it easier to interpret and compare the panels if each observation is only in a single panel.

\item Clarity - We prefer using more space for the data portion of the figures than typically displayed in the \code{coplot} function which requires the bar plot to present the overlapping segments.
\end{itemize}

It is still possible to augment the \code{gg_variable} to include overlapping conditional membership with continuous variables by duplicating rows of the training set data within the \code{rfsrc$xvar}  object, and then setting the conditional group membership as described. The \code{plot.gg_variable} function recipe above could be used to generate the panel plot, with panels ordered according to the factor levels of the grouping variable. We leave this as an exercise for the reader.

\subsection[Partial dependence coplots]{Partial dependence coplots (\code{gg\_partial\_coplot})}\label{S:partialcoplots}

By characterizing conditional plots as stratified variable dependence plots, the next logical step would be to generate an analogous conditional partial dependence plot. The process is similar to variable dependence coplots, first determine conditional group membership, then calculate the partial dependence estimates on each subgroup using the \code{plot.variable} function with a \code{subset} argument for each grouped interval. The \pkg{ggRandomForests} \code{gg_partial_coplot} function is a wrapper for generating conditional partial dependence data objects. Given a random forest (\code{rfsrc}) object and a \code{groups} vector for conditioning the training data set observations, \code{gg_partial_coplot} calls the \code{plot.variable} function the training set observations conditional on \code{groups} membership. The function returns a \code{gg_partial_coplot} object, a subclass of the \code{gg_partial} object, which can be plotted with the \code{plot.gg_partial} function.

The following code block will generate the data object for creating partial dependence coplot of 1 year survival as a function of \code{bili} conditional on membership within the 6 groups of \code{albumin} intervals that we examined in the Figure~\ref{fig:albumin-coplot}.
<<build-bili-albumin, eval=FALSE,echo=TRUE>>= 
		partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili", 
				groups = ggvar$albumin_grp, 
				surv_type = "surv", 
				time = 1, 
				show.plots = FALSE)
@

<<load-pbc-coplot, echo=FALSE>>=
# Load cached partial plot data
		data("partial_coplot_pbc", package = "ggRandomForests")
@

<<bili-albumin, fig.cap="Partial dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{albumin} interval group membership. Points estimates with loess smooth to indicate trend within each group.", fig.width=7, fig.height=4, echo=TRUE>>= 
		ggplot(partial_coplot_pbc, aes(x=bili, y=yhat, col=group, shape=group)) + #
		geom_smooth(se = FALSE) +
		labs(x = st.labs["bili"], y = "Survival at 1 year (%)", 
				color = "albumin", shape = "albumin") +
		coord_cartesian(y = c(49,101))
@
		The \code{gg_partial_coplot} of Figure~\ref{fig:bili-albumin} shows point estimates of the risk adjusted survival as a function of \code{bili} conditional on group membership defined by \code{albumin} intervals. The figure is slightly different than the \code{gg_partial} plot of Figure~\ref{fig:pbc-partial-panel} as each set of partial dependence estimates is calculated over a subset of the training data. We again connect the point estimates with a Loess curve.

For completeness, we construct the compliment coplot view of one year survival as a function of \code{albumin} conditional on \code{bili} interval group membership in Figure~\ref{fig:albumin-bili}. We list the source code for this figure in Appendix~\ref{A:biliPartialCoplot}.

<<albumin-bili, fig.cap="Partial dependence coplot of survival at 1 year against \\code{albumin}, conditional on \\code{bili} interval group membership. Points estimates with loess smooth to indicate trend within each group.", fig.width=7, fig.height=4, echo=FALSE>>= 
# Load cached partial plot data
		data("partial_coplot_pbc2", package = "ggRandomForests")

# Partial coplot
ggplot(partial_coplot_pbc2, aes(x=albumin, y=yhat, col=group, shape=group))+
		geom_smooth(se = FALSE) +
		labs(x = st.labs["albumin"], y = "Survival at 1 year (%)", 
				color = "Bilirubin", shape = "Bilirubin") +
		coord_cartesian(y = c(49,101))
@

\subsection{Partial plot surfaces}\label{S:partialSurface}

Just as in partial dependence, we can view the partial coplot curves as slices along a surface that could extend along an axis into the page. This visualization is made a bit difficult by our choice to select groups of similar population size, as the curves are not evenly spaced along the grouping variables. So, similar to the partial dependence surface we created along time in Section~\ref{S:timeSurface}, we can examine the relation of these two variables using a partial dependence surface. 
A difficulty with conditional dependence for this exercise is the reduction of the sample sizes for calculating a coplot surface. So instead, we calculate the full partial dependence surface by generating 50 \code{albumin} values spaced evenly along the data distribution. For each value of \code{albumin}, we calculate the partial dependence on \code{bili} at \code{npts = 50} points with the \code{plot.variable} function. We generate the surface again using the \code{surf3D} function.

<<surface3d, fig.cap="Partial dependence surface of survival at 1 year as a funtion of \\code{bili} and \\code{albumin}. Blue lines indicate partial coplot cut points for \\code{albumin} (Figure~\\ref{fig:bili-albumin}) and \\code{bili} (Figure~\\ref{fig:albumin-bili}).", fig.width=7, fig.height=5, echo=FALSE>>= 
# Find the quantile points to create 50 cut points
		alb_partial_pts <-quantile_pts(ggvar$albumin, groups = 50)

# Load the stored partial coplot data.
data("partial_pbc_surf")

# Instead of groups, we want the raw albumin point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the albumin direction
albumin.tmp <- do.call(c,lapply(alb_partial_pts, 
				function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_surf <- do.call(rbind,lapply(partial_pbc_surf, gg_partial))

# attach the data to the gg_partial_coplot
partial_surf$albumin <- albumin.tmp

# Modify the figure margins to make the figure larger
par(mai = c(0.5,.55,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_surf, c("bili", "albumin", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = topo.colors(25),
		colkey = FALSE, border = "black", bty = "b2", 
		shade = 0.5, expand = 0.5, theta=55, phi=15,
		lighting = TRUE, lphi = -50, ticktype="detailed",
		xlab = "Bilirubin", ylab = "Albumin", zlab = "Survival at 1 Year"
)

# Extract the albumin and bilirubin points
# Remove end points
bli <- bili_cts[-c(1,7)]
alb <- albumin_cts[-c(1,7)]

# Find the indices of the points closest to split points
alb.pts <- lapply(alb, function(pt){min(abs(srf$y - pt), na.rm=TRUE)})
bli.pts <- lapply(bli, function(pt){min(abs(srf$x - pt), na.rm=TRUE)})

indx.alb <- lapply(1:length(alb.pts), function(al){
			which(abs(srf$y - alb[al]) < alb.pts[[al]]+1.e-5)})
indx.bli <- lapply(1:length(bli.pts), function(al){
			which(abs(srf$x - bli[al]) < bli.pts[[al]]+1.e-5)})

# Draw the lines
indx <- c(indx.alb, indx.bli)
st <- lapply(indx, function(ind){
			lines3D(x=srf$x[ind], 
					y=srf$y[ind],
					z=srf$z[ind],
					add=TRUE, col="blue", lwd=6)})
@
		The partial dependence surface of Figure~\ref{fig:surface3d} shows partial dependence of 1 year survival on the Z-axis against values of Bilirubin and Albumin. We again use linear interpolation between the 2500 estimates, and color the surface by the response. Here blue corresponds to lower and yellow to higher risk adjusted survival. The blue lines are placed at the cut points between groups of \code{albumin} and \code{bili} used in the partial coplots of Figures~\ref{fig:bili-albumin} and~\ref{fig:albumin-bili} respectively.

To construct the partial coplot for groups of \code{albumin} in Figure~\ref{fig:bili-albumin}, we arbitrarily segmented the training set into 6 groups of equal membership size. The segments between blue lines parallel to the Bilirubin axis indicate where on the surface these observations are located. Similarly, the blues lines perpendicular to the Bilirubin axis segment observations into the 6 groups of \code{bili} intervals. Figure~\ref{fig:surface3d} indicates the arbitrary grouping for groups of \code{bili} in Figure~\ref{fig:albumin-bili}. 

The figure indicates that partial dependence of higher \code{albimun} levels are similar, which results in the over plotting seen in Figure~\ref{fig:bili-albumin}. The distribution is sparser at lower  \code{albimun} levels, creating the larger area in lowest  \code{albimun} values, where the partial dependence changes the most.

\section{Conclusion}\label{S:conclusion}

In this vignette, we have demonstrated the use of Random Survival Forest methods with the \pkg{ggRandomForests}~(\url{http://CRAN.R-project.org/package=ggRandomForests}) package. We have shown how to grow a random forest model and determine which variables contribute to the forest prediction accuracy using both VIMP and Minimal Depth measures. We outlined how to investigate variable associations with the response variable using variable dependence and the risk adjusted partial dependence plots. We've also explored variable interactions by using pairwise minimal depth interactions and directly viewed these interactions using variable dependence coplots and partial dependence coplots. Along the way, we've demonstrated the use of additional commands from the \pkg{ggplot2} package~\citep[\url{http://CRAN.R-project.org/package=ggplot2}]{Wickham:2009} package for modifying and customizing plots from \pkg{ggRandomForests} functions. 

% -----------------------------------------------------
		\section{Computational details}
% -----------------------------------------------------
		
		This document is a package vignette for the \pkg{ggRandomForests} package for ``Visually Exploring Random Forests'' (\url{http://CRAN.R-project.org/package=ggRandomForests}). The \pkg{ggRandomForests} package is designed for use with the \pkg{randomForestSRC} package~\citep[\url{http://CRAN.R-project.org/package=randomForestSRC}]{Ishwaran:RFSRC:2014} for growing survival, regression and classification random forest models and uses the \pkg{ggplot2} package~\citep[\url{http://CRAN.R-project.org/package=ggplot2}]{Wickham:2009} for plotting diagnostic and variable association results. \pkg{ggRandomForests} is  structured to extract data objects from \pkg{randomForestSRC} objects and provides functions for printing and plotting these objects.

The vignette is a tutorial for using the \pkg{ggRandomForests} package with the \pkg{randomForestSRC} package for building and post-processing random survival forests. In this tutorial, we explore a random forest for survival model constructed for the primary biliary cirrhosis (PBC) of the liver data set~\citep{fleming:1991}, available in the \pkg{randomForestSRC} package. We grow a random survival forest and demonstrate how \pkg{ggRandomForests} can be used when determining how the survival response depends on predictive variables within the model. The tutorial demonstrates the design and usage of many of \pkg{ggRandomForests} functions and features and also how to modify and customize the resulting \code{ggplot} graphic objects along the way.

The vignette is written in \LaTeX using the \pkg{knitr} package~\citep[\url{http://CRAN.R-project.org/package=knitr}]{Xie:2015, Xie:2014,Xie:2013}, which facilitates weaving \proglang{R}~\citep{rcore} code, results and figures into document text. 

This vignette is available within the \pkg{ggRandomForests} package on the Comprehensive R Archive Network (CRAN)~\citep[\url{http://cran.r-project.org}]{rcore}. Once the package has been installed, the vignette can be viewed directly from within \proglang{R} with the following command:
		<<vignette, eval=FALSE, echo=TRUE>>= 
		vignette("randomForestSRC-Survival", package = "ggRandomForests")
@
		
		A development version of the \pkg{ggRandomForests} package is also available on GitHub (\url{https://github.com}). We invite comments, feature requests and bug reports for this package at \url{https://github.com/ehrlinger/ggRandomForests}.


\section*{Acknowledgement}
This work was supported in part by the National Institutes of Health grant R01-HL103552-01A1.

\singlespacing
\bibliography{ggRandomForests}


%%\end{document}


%\doublespacing
		%\newpage
		
		\appendix
\section{Source Code}
Throughout this document, we have listed all \proglang{R} source code to create the figures included here with a few exceptions. For completeness, we include the missing code blocks in this appendix. The code blocks are included here in order of appearance in the document.

\subsection{Partial Dependence in Time Dimension}\label{A:TimeDomain}

The surface plot of~\ref{S:timeSurface} demonstrates how partial dependence curves relate to the survival curves. This code block is the \proglang{R} source code for creating Figure~\ref{fig:timeSurface3d}.

<<src-listing-timeSurface, echo=TRUE, eval=FALSE>>= 
# Restrict the time of interest to less than 5 years.
		time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]

# Find the 50 points in time, evenly space along the distribution of 
# event times for a series of partial dependence curves
time_cts <-quantile_pts(time_pts, groups = 50)

# Load stored data from the package.
# See ?partial_pbc_time for how this data was generated.
#
# Time surfaces are created with the partial.rfsrc command
# partial_pbc_time <- partial.rfsrc(rfsrc_pbc, xvar = "bili",sav
#                                   npts = 50, show.plots = FALSE,
#                                   surv.type="surv")
#
load(partial_pbc_time, package="ggRandomForests")

# We need to attach the time points of interest to our data.
time.tmp <- do.call(c,lapply(time_cts, 
				function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to gg_partial
partial_time <- do.call(rbind,lapply(partial_pbc_time, gg_partial))

# attach the time data to the gg_partial_coplot
partial_time$time <- time.tmp

# Modify the figure margins to make it larger
par(mai = c(0,0.3,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_time, c("time", "bili", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = heat.colors(25),
		colkey = FALSE, border = "black", bty = "b2", 
		shade = 0.5, expand = 0.5, theta=110,phi=15,
		lighting = TRUE, lphi = -50,
		ylab = "Bilirubin", xlab = "Time", zlab = "Survival"
)

# Extract the 1 and 3 year points.
# Find the indices of the points closest in time
t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
# Extract the 1 and 3 year points.
# Find the indices of the points closest in time
t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
indx <- vector("list", length=2)
indx[[1]] <- which(abs(srf$x - 1) < t.pts[1]+1.e-5)
indx[[2]] <- which(abs(srf$x - 3) < t.pts[2]+1.e-5)

# Generate curves along 1 and 3 year partial dependence 
alt <- lapply(indx, function(ind){
			lines3D(x=srf$x[ind], y=srf$y[ind],z=srf$z[ind],
					add=TRUE, col="blue", lwd=6)
		})
@

\subsection{Bilirubin Coplot}\label{A:biliCoplot}

In Section~\ref{S:coplots}, we generate variable dependence coplots for the \code{bili} variable conditional on grouping on intervals of the \code{albumin} variable, and the complimentary \code{albumin} variable conditional on grouping on intervals of the \code{bili} variable. We include the source code for Figure~\ref{fig:albumin-coplot} in the document. Since the code is nearly identical for the later case, we include the source code for generating Figure~\ref{fig:bili-coplot} here.

<<src-listing-bilicoplot, echo=TRUE, eval=FALSE>>= 
# Find intervals with similar number of observations.
		bili_cts <-quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)

# We need to move the minimal value so we include that observation
bili_cts[1] <- bili_cts[1] - 1.e-7

# Create the conditional groups and add to the gg_variable object
ggvar$bili_grp <- cut(ggvar$bili, breaks = bili_cts)

# Adjust naming for facets
levels(ggvar$bili_grp) <- paste("bilirubin = ",levels(ggvar$bili_grp), sep = "")

# plot.gg_variable
plot(ggvar[-which(is.na(ggvar$albumin)),], xvar = "albumin", 
				method = "glm", alpha = 0.5, se = FALSE) + 
		labs(y = "Survival", x = st.labs["albumin"]) + 
		theme(legend.position = "none") + 
		scale_color_manual(values = strCol, labels = event.labels) + 
		scale_shape_manual(values = event.marks, labels = event.labels) + 
		facet_wrap(~bili_grp) +
		coord_cartesian(ylim = c(-0.01, 1.01))
@

\subsection{Bilirubin Partial Coplot}\label{A:biliPartialCoplot}
Similar to variable dependence coplots, In Section~\ref{S:partialcoplots}, we compare the partial dependence coplots for the same \code{albumin} and \code{bili} variable groupings. Again, the source code for Figure~\ref{fig:bili-albumin} is nearly identical to the source code for generating Figure~\ref{fig:albumin-bili}. We include the partial dependence coplot source code for the \code{albumin} variable conditional on grouping on intervals of the \code{bili} variable.

<<albumin-bili-src, eval=FALSE,echo=TRUE>>= 
		partial_coplot_pbc2 <- gg_partial_coplot(rfsrc_pbc, xvar = "albumin", 
				groups = bili_grp, 
				surv_type = "surv", 
				time = 1, 
				show.plots = FALSE)


# Stored in 
# data(partial_coplot_pbc2, package = "ggRandomForests")

plot(partial_coplot_pbc2, se = FALSE) +
		labs(x = st.labs["albumin"], y = "Survival at 1 year (%)", 
				color = "Bilirubin", shape = "Bilirubin") +
		scale_color_brewer(palette = "Set2") +
		coord_cartesian(y = c(49,101))
@

\subsection{Partial Dependence in Multiple Variable Dimensions}\label{A:variableDomain}
In Section~\ref{S:partialSurface}, we generate a partial dependence surface of one year survival dependence on both \code{bili} and \code{albumin} variables. We include the Source code for generating Figure~\ref{fig:surface3d} here.

<<src-listing-variableSurface, echo=TRUE, eval=FALSE>>= 
# Find the quantile points to create 50 cut points
		alb_partial_pts <-quantile_pts(ggvar$albumin, groups = 50)

# Load the stored partial coplot data.
# See ?partial_pbc_surf for how this data was generated.
#
# partial_pbc_surf <- lapply(alb_partial_pts, function(ct){
#    rfsrc_pbc$xvar$albumin <- ct
#     plot.variable(rfsrc_pbc, xvar = "bili", time = 1,
#                   npts = 50, show.plots = FALSE,
#                   partial = TRUE, surv.type="surv")
# })
#
data("partial_pbc_surf")

# Instead of groups, we want the raw albumin point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the albumin direction
albumin.tmp <- do.call(c,lapply(alb_partial_pts, 
				function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_surf <- do.call(rbind,lapply(partial_pbc_surf, gg_partial))

# attach the data to the gg_partial_coplot
partial_surf$albumin <- albumin.tmp

# Modify the figure margins to make the figure larger
par(mai = c(0,.3,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_surf, c("bili", "albumin", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = topo.colors(25),
		colkey = FALSE, border = "black", bty = "b2", 
		shade = 0.5, expand = 0.5, theta=55, phi=15,
		lighting = TRUE, lphi = -50,
		xlab = "Bilirubin", ylab = "Albumin", zlab = "Survival at 1 Year"
)

# Extract the albumin and bilirubin points
# Remove end points
bli <- bili_cts[-c(1,7)]
alb <- albumin_cts[-c(1,7)]

# Find the indices of the points closest to split points
alb.pts <- lapply(alb, function(pt){min(abs(srf$y - pt), na.rm=TRUE)})
bli.pts <- lapply(bli, function(pt){min(abs(srf$x - pt), na.rm=TRUE)})

indx.alb <- lapply(1:length(alb.pts), function(al){
			which(abs(srf$y - alb[al]) < alb.pts[[al]]+1.e-5)})
indx.bli <- lapply(1:length(bli.pts), function(al){
			which(abs(srf$x - bli[al]) < bli.pts[[al]]+1.e-5)})

# Draw the lines
indx <- c(indx.alb, indx.bli)
st <- lapply(indx, function(ind){
			lines3D(x=srf$x[ind], 
					y=srf$y[ind],
					z=srf$z[ind],
					add=TRUE, col="blue", lwd=6)})
@