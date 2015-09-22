%%
%% This is file `wiasposter.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `poster')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.09}
\def\filedate{2013/02/12}
\def\filename{wiasposter}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Poster]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\WIASCD@addto@globaloptions{a0paper,26pt}
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

\LoadClass[\WIASCD@globaloptions]{scrartcl}

\RequirePackage{wiascd}
\RequirePackage{eurosym}
\RequirePackage{wrapfig}
\RequirePackage{caption}
\RequirePackage[manualmark]{scrpage2}
\RequirePackage{framed}
\RequirePackage{calc}
\RequirePackage{geometry}

\geometry{%
   includeheadfoot%
  ,nomarginpar%
  ,heightrounded%
  ,hmargin=.0386\paperwidth%
  ,top=.005\paperheight%
  ,bottom=.0252\paperheight%
  ,headsep=.0273\paperheight%
  ,headheight=.1421\paperheight%
}
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
\pagecolor{wiasblue10}
\renewcommand*{\baselinestretch}{1.1}
\small\normalsize
\setlength{\parindent}{\z@}
\clubpenalty         = 10000
\widowpenalty        = 10000
\displaywidowpenalty = 10000
\providecommand\captionof[1]{\def\@captype{#1}\caption}
\DeclareCaptionFormat{plain}{#1#2#3\par}
\captionsetup{%
   font={footnotesize,it}%
  ,labelfont=bf%
  ,singlelinecheck=false%
  ,format=plain%
}







\newcounter{WIASCD@poster@col}
\newlength{\WIASCD@poster@logoleftwidth}
\newlength{\WIASCD@poster@logorightwidth}
\newlength{\WIASCD@poster@titlewidth}
\newlength{\WIASCD@poster@tenmm}
\newlength{\WIASCD@poster@fivemm}
\newlength{\WIASCD@poster@rulewidth}
\newlength{\WIASCD@poster@colwidth}
\newlength{\WIASCD@poster@colsep}
\newlength{\WIASCD@poster@colsepwidth}
\newlength{\WIASCD@poster@headheight}
\setlength{\WIASCD@poster@tenmm}{0.01189\paperwidth}
\setlength{\WIASCD@poster@fivemm}{.5\WIASCD@poster@tenmm}
\setlength{\WIASCD@poster@rulewidth}{.1\WIASCD@poster@tenmm}
\setlength{\WIASCD@poster@colsep}{\textwidth / 64}
\setlength{\WIASCD@poster@colwidth}{7\WIASCD@poster@colsep}
\setlength{\WIASCD@poster@colsepwidth}{\WIASCD@poster@colwidth+\WIASCD@poster@colsep}
\setlength{\WIASCD@poster@headheight}{.667\headheight}
\setlength{\WIASCD@poster@logoleftwidth}{2\WIASCD@poster@colwidth}
\setlength{\WIASCD@poster@logorightwidth}{1\WIASCD@poster@colwidth}
\setlength{\WIASCD@poster@titlewidth}{5\WIASCD@poster@colwidth}
\addtolength{\WIASCD@poster@logoleftwidth}{-.5\WIASCD@poster@colsep}
\addtolength{\WIASCD@poster@logorightwidth}{-1.5\WIASCD@poster@colsep}
\addtolength{\WIASCD@poster@titlewidth}{2.4\WIASCD@poster@colsep}
\definecolor{wiasdarkblue}{cmyk}{1.0, 0.6, 0, 0.4}
\setlength{\abovecaptionskip}{.3\WIASCD@poster@tenmm}
\setlength{\belowcaptionskip}{.5\WIASCD@poster@tenmm}
\setlength{\intextsep}{\z@}
\setlength{\columnsep}{\WIASCD@poster@tenmm}
\setlength{\parskip}{\WIASCD@poster@tenmm}
\setfootwidth{\paperwidth}
\setkomafont{pagehead}{\normalfont\color{black}}
\setkomafont{pagefoot}{\normalfont\color{black}}
\lohead{%
  \hspace*{\WIASCD@poster@fivemm}%
  \setlength{\fboxsep}{\WIASCD@poster@tenmm}%
  \colorbox{white}{%
    \mbox{%
      \begin{minipage}[b][\WIASCD@poster@headheight][t]{\WIASCD@poster@logoleftwidth}%
        \raggedright
        \WIASCD@poster@logoleft
      \end{minipage}%
    }%
  }%
  \hfill
  \colorbox{wiasdarkblue}{%
    \mbox{%
      \begin{minipage}[b][\WIASCD@poster@headheight][t]{\WIASCD@poster@titlewidth}%
        \begin{minipage}[t]{.95\linewidth}%
          \raggedright
          \LARGE
          \color{white}%
          \WIASCD@poster@author%
        \end{minipage}%
        \vfill
        \begin{minipage}[b]{.95\linewidth}%
          \raggedright
          \Huge
          \bfseries
          \color{white}%
          \WIASCD@poster@title
        \end{minipage}%
      \end{minipage}%
    }%
  }%
  \hfill
  \colorbox{white}{%
    \mbox{%
      \begin{minipage}[b][\WIASCD@poster@headheight][t]{\WIASCD@poster@logorightwidth}%
        \raggedright
        \WIASCD@poster@logoright
        \rule{\z@}{.2\WIASCD@poster@tenmm}%
      \end{minipage}%
    }%
  }%
  \hspace*{\WIASCD@poster@fivemm}%
}
\lofoot{%
  \vrule width \paperwidth height \WIASCD@poster@rulewidth\\
  \colorbox{white}{%
    \makebox[\paperwidth][l]{%
      \rule[-.025\paperheight]{\z@}{.050\paperheight}%
      \hspace{4.5\WIASCD@poster@tenmm}%
      \normalsize
      \normalfont
      \color{black}%
      \WIASCD@poster@footer
    }%
  }%
}
\renewcommand*{\author}[1]{\renewcommand*{\WIASCD@poster@author}{#1}}
\renewcommand*{\title}[1]{\renewcommand*{\WIASCD@poster@title}{#1}}
\newcommand*{\logoleft}[1]{\renewcommand*{\WIASCD@poster@logoleft}{#1}}
\newcommand*{\logoright}[1]{\renewcommand*{\WIASCD@poster@logoright}{#1}}
\newcommand*{\contactname}[1]{\renewcommand*{\WIASCD@poster@contactname}{#1}}
\newcommand*{\name}[1]{\renewcommand*{\WIASCD@poster@name}{#1}}
\newcommand*{\institution}[1]{\renewcommand*{\WIASCD@poster@institution}{#1}}
\newcommand*{\address}[1]{\renewcommand*{\WIASCD@poster@address}{#1}}
\newcommand*{\telephone}[1]{\renewcommand*{\WIASCD@poster@telephone}{#1}}
\newcommand*{\fax}[1]{\renewcommand*{\WIASCD@poster@fax}{#1}}
\newcommand*{\url}[1]{\renewcommand*{\WIASCD@poster@url}{#1}}
\newcommand*{\email}[1]{\renewcommand*{\WIASCD@poster@email}{#1}}
\newcommand*{\footer}[1]{\renewcommand*{\WIASCD@poster@footer}{#1}}
\newcommand*{\addtologoleftwidth}[1]{%
  \addtolength{\WIASCD@poster@logoleftwidth}{#1}}
\newcommand*{\addtologorightwidth}[1]{%
  \addtolength{\WIASCD@poster@logorightwidth}{#1}}
\newcommand*{\addtotitlewidth}[1]{%
  \addtolength{\WIASCD@poster@titlewidth}{#1}}
\newcommand*{\WIASCD@poster@author}{}
\newcommand*{\WIASCD@poster@title}{}
\newcommand*{\WIASCD@poster@logoleft}{}
\newcommand*{\WIASCD@poster@logoright}{}
\newcommand*{\WIASCD@poster@name}{}
\newcommand*{\WIASCD@poster@institution}{}
\newcommand*{\WIASCD@poster@address}{}
\newcommand*{\WIASCD@poster@telephone}{}
\newcommand*{\WIASCD@poster@fax}{}
\newcommand*{\WIASCD@poster@url}{}
\newcommand*{\WIASCD@poster@email}{}
\newcommand*{\WIASCD@poster@contactname}{}
\newcommand*{\WIASCD@poster@footer}{}
\newif\if@WIASCD@poster@printfooteritemsep
\newcommand*{\WIASCD@poster@footeritemsep}{%
  \if@WIASCD@poster@printfooteritemsep
    ~$\cdot$~%
  \fi
  \@WIASCD@poster@printfooteritemseptrue
}
\footer{%
  \@WIASCD@poster@printfooteritemsepfalse
  \ifx\WIASCD@poster@contactname\@empty\else
    {\bfseries\WIASCD@poster@contactname: }%
  \fi
  \ifx\WIASCD@poster@name\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@name
  \fi
  \ifx\WIASCD@poster@institution\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@institution
  \fi
  \ifx\WIASCD@poster@address\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@address
  \fi
  \ifx\WIASCD@poster@telephone\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@telephone
  \fi
  \ifx\WIASCD@poster@fax\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@fax
  \fi
  \ifx\WIASCD@poster@email\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@email
  \fi
  \ifx\WIASCD@poster@url\@empty\else
    \WIASCD@poster@footeritemsep\WIASCD@poster@url
  \fi
}
\contactname{%
  \iflanguage{ngerman}{Kontakt}{}%
  \iflanguage{german}{Kontakt}{}%
  \iflanguage{english}{contact}{}%
}
\newcommand{\wiaspic}[3]{%
  \begin{wrapfigure}{#1}{#2}{\centering #3}\end{wrapfigure}%
}
\newcommand{\wiascombibox}[2]{%
  \ignorespaces
  \begingroup
  \raggedleft
  \setcounter{WIASCD@poster@col}{#1}%
  \addtocounter{WIASCD@poster@col}{-1}%
  \addtolength{\WIASCD@poster@colwidth}{\value{WIASCD@poster@col}\WIASCD@poster@colsepwidth}%
  \addtolength{\WIASCD@poster@colwidth}{\WIASCD@poster@colsep}%
  \setlength{\fboxsep}{\z@}%
  \begin{minipage}[t][][t]{\WIASCD@poster@colwidth}%
    \setlength{\WIASCD@poster@colwidth}{7\WIASCD@poster@colsep}%
    \setlength{\parskip}{\WIASCD@poster@tenmm}%
    #2%
  \end{minipage}%
  \endgroup
  \ignorespaces
}
\newcommand{\wiasframebox}[4]{%
  \hspace*{\WIASCD@poster@fivemm}%
  \setlength{\fboxsep}{\z@}%
  \setlength{\fboxrule}{\WIASCD@poster@rulewidth}%
  \fcolorbox{wiasdarkblue}{#3}{%
    \setcounter{WIASCD@poster@col}{#1}%
    \addtocounter{WIASCD@poster@col}{-1}%
    \addtolength{\WIASCD@poster@colwidth}{\value{WIASCD@poster@col}\WIASCD@poster@colsepwidth}%
    \begin{minipage}[t][#2mm][t]{\WIASCD@poster@colwidth}%
      \addtolength{\WIASCD@poster@colwidth}{-\WIASCD@poster@colsep}%
      \setlength{\fboxsep}{.6\WIASCD@poster@tenmm}%
      \colorbox{#3}{%
        \parbox[t]{\WIASCD@poster@colwidth}{%
          \setlength{\parskip}{\WIASCD@poster@fivemm}%
          \color{black}%
          \vspace{-\WIASCD@poster@fivemm}%
          #4%
        }%
      }%
    \end{minipage}%
  }%
  \hspace*{\WIASCD@poster@fivemm}%
  \ignorespaces
}
\newcommand{\wiasbarbox}[5]{%
  \hspace*{\WIASCD@poster@fivemm}%
  \setlength{\fboxsep}{\z@}%
  \setlength{\fboxrule}{\WIASCD@poster@rulewidth}%
  \fcolorbox{wiasdarkblue}{#3}{%
    \setcounter{WIASCD@poster@col}{#1}%
    \addtocounter{WIASCD@poster@col}{-1}%
    \addtolength{\WIASCD@poster@colwidth}{\value{WIASCD@poster@col}\WIASCD@poster@colsepwidth}%
    \begin{minipage}[t][#2mm][t]{\WIASCD@poster@colwidth}%
      \addtolength{\WIASCD@poster@colwidth}{-\WIASCD@poster@colsep}%
      \setlength{\fboxsep}{.6\WIASCD@poster@tenmm}%
      \colorbox{wiasdarkblue}{%
        \parbox[t]{\WIASCD@poster@colwidth}{%
          \color{white}%
          \large
          \bfseries
          \strut #4%
        }%
      }%
      \\
      \colorbox{#3}{%
        \parbox[t]{\WIASCD@poster@colwidth}{%
          \setlength{\parskip}{\WIASCD@poster@fivemm}%
          #5%
        }%
      }%
    \end{minipage}%
  }%
  \hspace*{\WIASCD@poster@fivemm}%
  \ignorespaces
}
\newcommand*{\WIASCD@poster@boxbackground}{white}
\newcommand*{\WIASCD@poster@rowheight}{200}
\newcommand*{\wiasrowheight}[1]{%
  \renewcommand*{\WIASCD@poster@rowheight}{#1}%
}
\newcommand*{\wiasboxbackground}[1]{%
  \renewcommand*{\WIASCD@poster@boxbackground}{#1}%
}
\newcommand{\wiasleftbox}[2]{%
  \wiasbarbox{2}{\WIASCD@poster@rowheight}{\WIASCD@poster@boxbackground}%
             {#1}{#2}%
}
\newcommand{\wiasbox}[2]{%
  \wiasframebox{#1}{\WIASCD@poster@rowheight}%
               {\WIASCD@poster@boxbackground}{#2}%
}
\newcommand{\wiasoneboxrow}[1]{\wiasbox{6}{#1}}
\newcommand{\wiastwoboxrow}[2]{\wiasbox{3}{#1}\wiasbox{3}{#2}}
\newcommand{\wiasthreeboxrow}[3]{\wiasbox{2}{#1}\wiasbox{2}{#2}\wiasbox{2}{#3}}
\logoleft{%
  \iflanguage{english}{%
    \wiaslogo[english,height=1.05\WIASCD@poster@headheight]%
  }{%
    \wiaslogo[german,height=1.05\WIASCD@poster@headheight]%
  }%
}

\endinput
%%
%% End of file `wiasposter.cls'.
