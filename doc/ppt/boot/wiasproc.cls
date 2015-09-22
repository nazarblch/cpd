%%
%% This is file `wiasproc.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `proc')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.06}
\def\filedate{2013/02/12}
\def\filename{wiasproc}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Conference Proceedings]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\WIASCD@addto@globaloptions{titlepage,10pt,headings=normal}
\newif\if@WIASCD@proc@cover\@WIASCD@proc@coverfalse
\newif\if@WIASCD@proc@color\@WIASCD@proc@colortrue
\DeclareOption{cover}{\@WIASCD@proc@covertrue}
\DeclareOption{color}{\@WIASCD@proc@colortrue}
\DeclareOption{nocolor}{\@WIASCD@proc@colorfalse}
\DeclareOption{bw}{\@WIASCD@proc@colorfalse}
\newif\if@WIASCD@crop\@WIASCD@cropfalse
\DeclareOption{crop}{\@WIASCD@croptrue}
\DeclareOption{nomathpazo}{\PassOptionsToPackage{nomathpazo}{wiascd}}
\DeclareOption{mathpazo}{\PassOptionsToPackage{mathpazo}{wiascd}}
\DeclareOption{nomathptmx}{\PassOptionsToPackage{nomathptmx}{wiascd}}
\DeclareOption{mathptmx}{\PassOptionsToPackage{mathptmx}{wiascd}}
\DeclareOption*{%
  \expandafter\WIASCD@addto@globaloptions%
    \expandafter{\CurrentOption}%
}
\ProcessOptions*\relax
\typeout{CLASS OPTIONS: \@classoptionslist}
\typeout{GLOBAL OPTIONS: \WIASCD@globaloptions}
\let\@classoptionslist\WIASCD@globaloptions

\LoadClass[\WIASCD@globaloptions]{scrbook}[2008/10/07]

\RequirePackage{wiascd}
\RequirePackage{eurosym}
\RequirePackage{eso-pic}
\RequirePackage{rotating}
\RequirePackage{subfig}
\RequirePackage{caption}
\RequirePackage[manualmark]{scrpage2}
\RequirePackage[multiple]{footmisc}
\RequirePackage{hyperref}
\RequirePackage{ifthen}
\RequirePackage{calc}
\RequirePackage{geometry}

\setlength{\paperwidth}{18.5cm}
\if@WIASCD@proc@cover
  \newlength{\WIASCD@proc@cover@spine}
  \setlength{\WIASCD@proc@cover@spine}{.9cm}
  \setlength{\paperwidth}{2\paperwidth}
  \addtolength{\paperwidth}{\WIASCD@proc@cover@spine}
  \geometry{%
     paperwidth=\the\paperwidth%
    ,paperheight=26.0cm%
    ,margin=\z@%
    ,nohead%
    ,nofoot%
    ,nomarginpar%
  }
\else
  \geometry{%
     includeheadfoot%
    ,heightrounded%
    ,paperwidth=\the\paperwidth%
    ,paperheight=26.0cm%
    ,hmargin={3cm,2cm}%
    ,top=1.25cm%
    ,headheight=.86cm%
    ,headsep=2.5cm%
    ,textheight=19.5cm%
  }
\fi
\if@WIASCD@crop
  \newlength{\WIASCD@crop@paperwidth}
  \newlength{\WIASCD@crop@paperheight}
  \setlength{\WIASCD@crop@paperwidth}{\paperwidth+3cm}
  \setlength{\WIASCD@crop@paperheight}{\paperheight+3cm}
  \RequirePackage[%
     cam,center%
    ,width=\the\WIASCD@crop@paperwidth%
    ,height=\the\WIASCD@crop@paperheight%
  ]{crop}
\fi   % \if@WIASCD@crop

\pagestyle{scrheadings}
\clearscrheadfoot
\renewcommand*{\baselinestretch}{1.1}
\small\normalsize
\setlength{\parindent}{\z@}
\setlength{\parskip}{2ex}
\clubpenalty         = 10000
\widowpenalty        = 10000
\displaywidowpenalty = 10000
\setcounter{secnumdepth}{5}
\setcounter{tocdepth}{3}
\providecommand\captionof[1]{\def\@captype{#1}\caption}
\DeclareCaptionFormat{plain}{#1#2#3\par}
\captionsetup{%
   font={footnotesize,it}%
  ,labelfont=bf%
  ,singlelinecheck=false%
  ,format=plain%
}





\newcommand{\WIASCD@proc@author@orig}{}
\let\WIASCD@proc@author@orig\author
\renewcommand{\author}[2][]{%
  \ifthenelse{\equal{#1}{}}{\renewcommand{\WIASCD@proc@authorshort}{#2}}%
                           {\renewcommand{\WIASCD@proc@authorshort}{#1}}%
  \begin{minipage}{\textwidth}%
    \renewcommand{\footnoterule}{}%
    #1%
  \end{minipage}%
  \newline
  \vspace{.75cm}%
}
\renewcommand{\title}[1]{\renewcommand{\WIASCD@proc@title}{#1}}
\newcommand{\editors}[1]{\renewcommand{\WIASCD@proc@editors}{#1}}
\newcommand{\person}[2]{\textbf{#1}\\#2}
\newcommand{\logos}[1]{\renewcommand{\WIASCD@proc@logos}{#1}}
\newcommand{\imprint}[1]{\renewcommand{\WIASCD@proc@imprint}{#1}}
\newcommand*{\backgroundpicture}[1]{%
  \renewcommand{\WIASCD@proc@backpic}{#1}%
}
\newcommand{\WIASCD@proc@title}{}
\newcommand{\WIASCD@proc@editors}{}
\newcommand{\WIASCD@proc@logos}{}
\newcommand{\WIASCD@proc@imprint}{}
\newcommand{\WIASCD@proc@backpic}{}
\newcommand{\WIASCD@proc@authorshort}{}
\newenvironment{abstract}[1][\textit{\abstractname}\\]{\par #1}{\par}
\renewcommand*{\chapterheadstartvskip}{\vspace{-2.2\baselineskip}}
\if@WIASCD@proc@color
  \newcommand*{\WIASCD@proc@wiasblue}{\color{wiasblue}}
  \addtokomafont{chapter}{\color{wiasblue}}
  \addtokomafont{section}{\color{wiasblue}}
  \addtokomafont{subsection}{\color{wiasblue}}
\else
  \newcommand*{\WIASCD@proc@wiasblue}{\color{gray30}}
  \addtokomafont{chapter}{\color{black}}
  \addtokomafont{section}{\color{black}}
  \addtokomafont{subsection}{\color{black}}
\fi

\if@WIASCD@proc@color
  \setkomafont{pagehead}{\normalfont\bfseries\large\color{wiasblue}}
  \setkomafont{pagefoot}{\normalfont\bfseries\large\color{wiasblue}}
  \setkomafont{pagenumber}{\color{wiasblue70}}
\else
  \setkomafont{pagehead}{\normalfont\bfseries\large\color{black}}
  \setkomafont{pagefoot}{\normalfont\bfseries\large\color{black}}
  \setkomafont{pagenumber}{\color{gray30}}
\fi
\setheadwidth[-2.2cm]{17.2cm}
\setheadtopline[20cm]{.4pt}[\WIASCD@proc@wiasblue]
\setheadsepline[20cm]{.4pt}[\WIASCD@proc@wiasblue]
\rohead{\rule[-2.5mm]{\z@}{\headheight-.5mm}\WIASCD@proc@authorshort\makebox[14.3mm][r]{\pagemark}}
\lehead{\makebox[14.3mm][l]{\pagemark}\rule[-2.5mm]{\z@}{\headheight-.5mm}\headmark}
\automark{chapter}
\renewcommand*{\chapterpagestyle}{scrheadings}

\setlength{\footnotesep}{1.2em}
\deffootnote{7mm}{\z@}{\makebox[7mm][l]{\bfseries\thefootnotemark}}
\renewcommand*{\footnoterule}{{\WIASCD@proc@wiasblue\rule{\columnwidth}{.25mm}}\vspace{3mm}}
\renewcommand*{\thempfootnote}{\Roman{mpfootnote}}
\renewcommand*\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \setlength\@tempdima{1.5em}%
    \if@tocleft
      \ifx\toc@l@number\@empty\else
        \setlength\@tempdima{0\toc@l@number}%
      \fi
    \fi
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      \@dottedtocline{0}{\z@}{1.1em}{\sectfont #1\hfil}{#2}%
      \penalty\@highpenalty
    \endgroup
  \fi
}
\if@WIASCD@proc@cover
\newcommand*{\printcover}{%
  \pagestyle{empty}
  \AddToShipoutPicture{%
    \WIASCD@proc@wiasblue%
    \put(-10,525){%
      \parbox[t][78mm]{\paperwidth+1cm}{%
        \centering
        \rule[\z@]{19cm+\WIASCD@proc@cover@spine}{78mm}%
        \includegraphics[width=19cm,height=78mm]{\WIASCD@proc@backpic}%
      }%
    }%
  }%
  \begin{minipage}[b][24.35cm][t]{18.65cm}%
    \rule[-1cm]{\z@}{1cm}%
  \end{minipage}%
  \begin{minipage}[b][24.35cm][b]{\WIASCD@proc@cover@spine}%
    \begin{sideways}%
      \begin{minipage}[b][\WIASCD@proc@cover@spine][c]{.8\WIASCD@proc@cover@spine}%
        \if@WIASCD@proc@color
          \wiaslogo[plain,height=\linewidth,angle=-90]%
        \else
          \wiaslogo[plain,gray,height=\linewidth,angle=-90]%
        \fi
      \end{minipage}%
      \hspace{5mm}%
      \begin{minipage}[b][\WIASCD@proc@cover@spine][c]{15cm}%
        \bfseries\WIASCD@proc@title
      \end{minipage}%
    \end{sideways}%
  \end{minipage}%
  \hspace{2cm}%
  \begin{minipage}[b][24.35cm][t]{18cm}%
    \setlength{\parskip}{2ex}%
    \vspace*{5.67cm}%
    \rlap{\smash{\parbox[b]{14cm}{%
      \raggedright
      \rmfamily
      \bfseries
      \Huge
      \color{white}%
      \WIASCD@proc@title
    }}}%
    \vspace{2.5cm}%
    \normalsize
    \WIASCD@proc@editors
    \par
    \vspace{.7cm}%
    \WIASCD@proc@logos
    \par
    \vfill
    \parbox[b][]{14cm}{%
      \hspace{-3.1mm}%
      \if@WIASCD@proc@color
        \wiaslogo[plain,width=1.8cm]%
      \else
        \wiaslogo[plain,gray,width=1.8cm]%
      \fi
      \vspace{1mm}%
      \par
      \raggedright
      \bfseries
      \footnotesize
      \WIASCD@proc@wiasblue%
      www.\WIASCD@domain
    }%
  \end{minipage}%
}
\else   % \if@WIASCD@proc@cover
\AtBeginDocument{%
  \begin{titlepage}%
    \addtolength{\oddsidemargin}{-1cm}%
    \AddToShipoutPicture*{%
      \put(-10,525){%
        \parbox[t][78mm]{19cm}{%
          \centering
          \includegraphics[width=19cm,height=78mm]{\WIASCD@proc@backpic}%
        }%
      }%
    }%
    \vspace*{.5cm}%
    \rlap{\smash{\parbox[b]{14cm}{%
      \raggedright
      \rmfamily
      \bfseries
      \Huge
      \color{white}%
      \WIASCD@proc@title
    }}}%
    \vspace{2.5cm}%
    {\normalsize
      \WIASCD@proc@editors
      \par\vspace{.7cm}%
      \WIASCD@proc@logos
      \par\vfill
      \parbox[b]{14cm}{%
        \hspace{-4.2mm}%
        \if@WIASCD@proc@color
          \wiaslogo[plain,width=1.8cm]%
        \else
          \wiaslogo[plain,gray,width=1.8cm]%
        \fi
        \par\vspace{1mm}%
        \raggedright
        \bfseries
        \footnotesize
        \WIASCD@proc@wiasblue%
        www.\WIASCD@domain
      }%
    }%
    \vspace{-.5cm}%
  \end{titlepage}%
  \thispagestyle{empty}%
  \begin{minipage}[b][19.5cm][b]{15cm}%
    \WIASCD@proc@imprint
  \end{minipage}%
  \clearpage
}
\fi   % \if@WIASCD@proc@cover



\endinput
%%
%% End of file `wiasproc.cls'.
