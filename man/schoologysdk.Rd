\name{schoologysdk}
\alias{schoologysdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
PDF table conversion to CSV
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
pdf2csv utilizes Google's Vision API to detect the presence of table cell borders on PDF pages and scrape the table's text into CSV format.
}
\usage{
pdf2csv(pdfFileName, exportDirectory, tableBorderColor = 'black', pagesToScrape = 'all', debug = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pdfFileName}{
%%     ~~Describe \code{x} here~~
the name of the PDF file to be converted to CSV format
}
\item{exportDirectory}{
%%     ~~Describe \code{x} here~~
the directory where the generated CSV(s) will end up
}
\item{tableBorderColor}{
%%     ~~Describe \code{x} here~~
the color of the borders of the table on the PDF (black or white only)
}
\item{pagesToScrape}{
%%     ~~Describe \code{x} here~~
a list of page numbers to scrape. defaults to 'all'
}
\item{debug}{
%%     ~~Describe \code{x} here~~
whether to export images of the pdf pages with table borders overlayed
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
pdf2csv reads in a PDF, converts it to a JPG, uses Google's Vision API to detect text positions, detects table borders, and separates text into CSV cells mirroring the table cells.
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A series of CSVs are generated but no data returned.
}
\references{
%% ~put references to the literature/web site here ~
https://cloud.google.com/vision/docs/ocr

https://gist.github.com/flovv/524f5bf2f465e5a4da25
}
\author{
%%  ~~who you are~~
Sam Terfa
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{dbscan}}
}
\examples{
pdf2csv(pdfFilePath = paste0(path.package('pdf2csv'), '/sample.pdf'),       
        exportDirectory = '~/Desktop/', 
        pagesToScrape = c(1, 3), 
        debug = T, 
        tableBorderColor = 'black', 
        includeBorders = F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
