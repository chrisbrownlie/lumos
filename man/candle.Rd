% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/candle.R
\name{candle}
\alias{candle}
\title{candle class}
\value{
an object with class 'candle'
}
\description{
candle class

candle class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{lumens}}{the light that this candle emits in lumens}

\item{\code{x_offset}}{the distance (cm) in the x direction of the candle
from the centre of the candelabra in which it sits}

\item{\code{y_offset}}{the distance (cm) in the y direction of the candle
from the centre of the candelabra in which it sits}

\item{\code{candle_height}}{the height of the candle (in cm) from the base
of the candelabra in which it sits}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{id}}{a uniquely generated ID for the candle}

\item{\code{candelabra}}{the ID of the candelabra in which this candle sits}

\item{\code{stage}}{the name of the stage on which this candle sits}

\item{\code{x_position}}{the overall x coordinate of the candle on the stage}

\item{\code{y_position}}{the overall y coordinate of the candle on the stage}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-candle-new}{\code{candle$new()}}
\item \href{#method-candle-clone}{\code{candle$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-candle-new"></a>}}
\if{latex}{\out{\hypertarget{method-candle-new}{}}}
\subsection{Method \code{new()}}{
create a new candle object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{candle$new(candelabra, lumens, x_offset, y_offset, candle_height)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{candelabra}}{the candelabra object in which the new candle should sit}

\item{\code{lumens}}{the amount of light the new candle emits, in lumens}

\item{\code{x_offset}}{the distance (cm) in the x direction between the new candle and
the centre of the candelabra in which it sits}

\item{\code{y_offset}}{the distance (cm) in the y direction between the new candle and
the centre of the candelabra in which it sits}

\item{\code{candle_height}}{the height of the candle in cm}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-candle-clone"></a>}}
\if{latex}{\out{\hypertarget{method-candle-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{candle$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
