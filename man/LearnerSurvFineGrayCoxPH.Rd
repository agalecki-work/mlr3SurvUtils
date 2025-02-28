% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerSurvFineGrayCoxPH.R
\name{LearnerSurvFineGrayCoxPH}
\alias{LearnerSurvFineGrayCoxPH}
\title{Fine-Gray Survival Learner with Cox Proportional Hazards}
\description{
A survival learner implementing the Fine-Gray model for competing risks analysis
using \code{survival::coxph}. This learner extends the \code{LearnerSurv} class from
\code{mlr3proba} and fits a proportional subdistribution hazards model after
transforming the data with \code{survival::finegray}. The main event of interest
is dynamically set as the second level of the event status factor, with the
third level treated as a competing risk.
}
\details{
The Fine-Gray model estimates the subdistribution hazard of a specific event
in the presence of competing risks. This implementation uses the standard
Cox proportional hazards approach without penalization. The event status
must have at least three levels: censored (first level), main event of
interest (second level), and competing risk (third level). Predictions
include the linear predictor (\code{lp}), crank scores, and distribution (\code{distr})
as survival probabilities over time.
}
\section{Parameters}{

The learner supports the following parameters:
\describe{
\item{ties}{Character, method for handling ties in \code{coxph}. Options are
\code{"efron"} (default), \code{"breslow"}, or \code{"exact"}.}
\item{iter.max}{Integer, maximum number of iterations for \code{coxph}.
Default is 100, ranging from 1 to 1000.}
}
}

\section{Methods}{

\describe{
\item{\verb{$initialize()}}{Initializes a new instance of the learner.}
\item{\verb{$train(task)}}{Trains the Fine-Gray model on the provided survival task.}
\item{\verb{$predict(task)}}{Predicts survival outcomes for new data.}
}
}

\examples{
library(mlr3)
library(mlr3proba)

# Create example data
set.seed(123)
data <- data.frame(
  survival_time = c(5, 10, 15, 20, 25, 7, 12, 18, 22, 30, 8, 14, 19, 23, 28),
  event_status = c(1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0),
  x1 = c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5, 2, 3, 4, 5, 6),
  x2 = c(0.5, 1, 1.5, 2, 2.5, 0.7, 1.2, 1.8, 2.3, 2.8, 0.9, 1.4, 1.9, 2.4, 2.9)
)
data$event_status <- factor(data$event_status,
                           levels = c(0, 1, 2),
                           labels = c("censored", "death", "relapse"))

# Create a survival task
task <- TaskSurv$new("fg_test",
                     backend = data,
                     time = "survival_time",
                     event = "event_status",
                     type = "mstate")
print(task)
part <- partition(task)

# Initialize and train the learner
FG_cox <- lrn("surv.finegray_coxph")
pred <- FG_cox$train(task, part$train)$predict(task, part$test)
St <- pred$distr[1:5]$survival(c(0, 10, 20, 30))
CIF <- 1 - St
cindex <- msr("surv.cindex")
print(cindex$score(pred))

}
\concept{Learners}
\section{Super classes}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{\link[mlr3proba:LearnerSurv]{mlr3proba::LearnerSurv}} -> \code{LearnerSurvFineGrayCoxPH}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LearnerSurvFineGrayCoxPH-new}{\code{LearnerSurvFineGrayCoxPH$new()}}
\item \href{#method-LearnerSurvFineGrayCoxPH-clone}{\code{LearnerSurvFineGrayCoxPH$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="base_learner"><a href='../../mlr3/html/Learner.html#method-Learner-base_learner'><code>mlr3::Learner$base_learner()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="encapsulate"><a href='../../mlr3/html/Learner.html#method-Learner-encapsulate'><code>mlr3::Learner$encapsulate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="format"><a href='../../mlr3/html/Learner.html#method-Learner-format'><code>mlr3::Learner$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="help"><a href='../../mlr3/html/Learner.html#method-Learner-help'><code>mlr3::Learner$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict"><a href='../../mlr3/html/Learner.html#method-Learner-predict'><code>mlr3::Learner$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict_newdata"><a href='../../mlr3/html/Learner.html#method-Learner-predict_newdata'><code>mlr3::Learner$predict_newdata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="print"><a href='../../mlr3/html/Learner.html#method-Learner-print'><code>mlr3::Learner$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="reset"><a href='../../mlr3/html/Learner.html#method-Learner-reset'><code>mlr3::Learner$reset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="train"><a href='../../mlr3/html/Learner.html#method-Learner-train'><code>mlr3::Learner$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerSurvFineGrayCoxPH-new"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerSurvFineGrayCoxPH-new}{}}}
\subsection{Method \code{new()}}{
Initializes a new instance of the Fine-Gray CoxPH survival learner.
Sets up the parameter set and configures the learner properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvFineGrayCoxPH$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerSurvFineGrayCoxPH-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerSurvFineGrayCoxPH-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvFineGrayCoxPH$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
