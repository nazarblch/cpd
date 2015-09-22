%%
%% This is file `wiasbadge.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `badge')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.14}
\def\filedate{2014/11/19}
\def\filename{wiasbadge}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Badges]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\WIASCD@addto@globaloptions{a4paper,16pt}
\newif\if@WIASCD@badge@vcardformat\@WIASCD@badge@vcardformatfalse
\newif\if@WIASCD@badge@selfadformat\@WIASCD@badge@selfadformatfalse
\DeclareOption{showboxes}{\PassOptionsToPackage{showboxes}{textpos}}
\DeclareOption{crosses}{\PassOptionsToPackage{showboxes}{textpos}}
\DeclareOption{vcardformat}{\@WIASCD@badge@vcardformattrue}
\DeclareOption{selfadformat}{\@WIASCD@badge@selfadformattrue}
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
\RequirePackage{mathpazo}
\RequirePackage[absolute]{textpos}
\RequirePackage{calc}
\RequirePackage{geometry}

\geometry{%
   margin=\z@%
  ,nohead%
  ,nofoot%
  ,nomarginpar%
}

\setlength{\unitlength}{0.1mm}
\pagestyle{empty}
\renewcommand*{\baselinestretch}{1.0}
\small\normalsize
\setlength{\parindent}{\z@}



\definecolor{WIASCD@badge@crosscolor}{gray}{.8}
\textblockrulecolour{WIASCD@badge@crosscolor}

\if@WIASCD@badge@selfadformat
  \TPshowboxesfalse
\fi

\newlength{\WIASCD@badge@width}
\newlength{\WIASCD@badge@height}
\newlength{\WIASCD@badge@gutter}
\newlength{\WIASCD@badge@gap}
\newlength{\WIASCD@badge@innermargin}
\newlength{\WIASCD@badge@start@x}
\newlength{\WIASCD@badge@start@y}
\newlength{\WIASCD@badge@offset@x}
\newlength{\WIASCD@badge@offset@y}
\newlength{\WIASCD@badge@auxpos@x}
\newlength{\WIASCD@badge@auxpos@y}
\newcount\WIASCD@badge@cols
\newcount\WIASCD@badge@rows
\newcount\WIASCD@badge@col
\newcount\WIASCD@badge@row

\newcommand*{\WIASCD@badge@namefontsize}{}
\newcommand*{\WIASCD@badge@currentname}{}
\newsavebox{\WIASCD@badge@namebox}
\newlength{\WIASCD@badge@nameboxlen}
\newcommand*{\WIASCD@badge@get@namefontsize}[1]{%
  \begingroup
    \let\\\relax
    \global\let\WIASCD@badge@namefontsize\Huge
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \ifdim\wd\WIASCD@badge@namebox>\WIASCD@badge@nameboxlen
    \global\let\WIASCD@badge@namefontsize\huge
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \fi
  \ifdim\wd\WIASCD@badge@namebox>\WIASCD@badge@nameboxlen
    \global\let\WIASCD@badge@namefontsize\LARGE
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \fi
  \ifdim\wd\WIASCD@badge@namebox>\WIASCD@badge@nameboxlen
    \global\let\WIASCD@badge@namefontsize\Large
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \fi
  \ifdim\wd\WIASCD@badge@namebox>\WIASCD@badge@nameboxlen
    \global\let\WIASCD@badge@namefontsize\large
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \fi
  \ifdim\wd\WIASCD@badge@namebox>\WIASCD@badge@nameboxlen
    \global\let\WIASCD@badge@namefontsize\normalsize
    \savebox{\WIASCD@badge@namebox}{\WIASCD@badge@namefontsize #1}%
  \fi
  \endgroup
}

\newcommand*{\WIASCD@badge@reset}{%
  \setlength{\WIASCD@badge@start@x}{0mm}%
  \setlength{\WIASCD@badge@start@y}{0mm}%
  \setlength{\WIASCD@badge@auxpos@x}{0mm}%
  \setlength{\WIASCD@badge@auxpos@y}{0mm}%
  \setlength{\WIASCD@badge@innermargin}{5mm}%
  \if@WIASCD@badge@vcardformat
    \setlength{\WIASCD@badge@offset@x}{15mm}%
    \setlength{\WIASCD@badge@offset@y}{10mm}%
    \setlength{\WIASCD@badge@width}{85mm}%
    \setlength{\WIASCD@badge@height}{55mm}%
    \setlength{\WIASCD@badge@gutter}{10mm}%
    \setlength{\WIASCD@badge@gap}{0mm}%
    \WIASCD@badge@rows=5%
  \else\if@WIASCD@badge@selfadformat
    \setlength{\WIASCD@badge@offset@x}{20mm}%
    \setlength{\WIASCD@badge@offset@y}{16mm}%
    \setlength{\WIASCD@badge@width}{75mm}%
    \setlength{\WIASCD@badge@height}{45mm}%
    \setlength{\WIASCD@badge@gutter}{20mm}%
    \setlength{\WIASCD@badge@gap}{10mm}%
    \WIASCD@badge@rows=5%
  \else
    \setlength{\WIASCD@badge@offset@x}{25mm}%
    \setlength{\WIASCD@badge@offset@y}{20mm}%
    \setlength{\WIASCD@badge@width}{75mm}%
    \setlength{\WIASCD@badge@height}{40mm}%
    \setlength{\WIASCD@badge@gutter}{0mm}%
    \setlength{\WIASCD@badge@gap}{0mm}%
    \WIASCD@badge@rows=6%
  \fi\fi
  \setlength{\WIASCD@badge@nameboxlen}{\WIASCD@badge@width-2\WIASCD@badge@innermargin}%
  \WIASCD@badge@cols=2%
  \WIASCD@badge@col=1%
  \WIASCD@badge@row=1%
}
\newcommand*{\printbadges}{%
  \WIASCD@badge@reset
  \begingroup
    \addtolength{\WIASCD@badge@start@x}{\WIASCD@badge@offset@x}%
    \addtolength{\WIASCD@badge@start@y}{\WIASCD@badge@offset@y}%
    \setlength{\WIASCD@badge@auxpos@x}{\WIASCD@badge@start@x}%
    \setlength{\WIASCD@badge@auxpos@y}{\WIASCD@badge@start@y}%
    \loop
      {%
      \loop
        \expandafter\let\expandafter\WIASCD@badge@currentname
          \csname WIASCD@badge@name@\ifnum\WIASCD@badge@col=1 l\else r\fi
            \number\WIASCD@badge@row\endcsname
        \WIASCD@badge@get@namefontsize{\bfseries\WIASCD@badge@currentname}%
        \begin{textblock*}{\WIASCD@badge@width}(\WIASCD@badge@auxpos@x,\WIASCD@badge@auxpos@y)%
          \parbox[t][\WIASCD@badge@height]{\linewidth}{%
            \vspace*{\WIASCD@badge@innermargin}%
            \hspace*{\WIASCD@badge@innermargin}%
            \WIASCD@badge@logos
            \hspace*{\WIASCD@badge@innermargin}%
          }%
        \end{textblock*}%
        \begin{textblock*}{\WIASCD@badge@width}(\WIASCD@badge@auxpos@x,\WIASCD@badge@auxpos@y)%
          \parbox[t][\WIASCD@badge@height]{\linewidth}{%
            \parbox[t][.5\WIASCD@badge@height]{\linewidth}{%
              \vspace*{\WIASCD@badge@innermargin}%
              \hspace*{\WIASCD@badge@innermargin}%
              \parbox[t]{\WIASCD@badge@titlewidth\WIASCD@badge@nameboxlen}{%
                {\raggedright\@title\par}%
              }%
            }%
            \par
            \@nameuse{WIASCD@badge@background@%
                      \ifnum\WIASCD@badge@col=1 l\else r\fi
                      \number\WIASCD@badge@row}%
            \parbox[t][.5\WIASCD@badge@height]{\linewidth}{%
              \vspace*{.5\WIASCD@badge@innermargin}%
              \hspace*{\WIASCD@badge@innermargin}%
              \parbox[t]{\WIASCD@badge@nameboxlen}{%
                \raggedright
                \WIASCD@badge@namefontsize
                \bfseries\WIASCD@badge@currentname
              }%
              \par\bigskip\vfill
              \hspace*{\WIASCD@badge@innermargin}%
              \parbox[b]{\WIASCD@badge@nameboxlen}{%
                \raggedright
                \fontsize{12}{16}\selectfont
                \@nameuse{WIASCD@badge@pos@%
                          \ifnum\WIASCD@badge@col=1 l\else r\fi%
                          \number\WIASCD@badge@row}%
                \par\null
              }%
              \vspace*{.5\WIASCD@badge@innermargin}%
            }%
          }%
        \end{textblock*}%
        \global\@namedef{WIASCD@badge@name@%
                         \ifnum\WIASCD@badge@col=1 l\else r\fi%
                         \number\WIASCD@badge@row}{}%
        \global\@namedef{WIASCD@badge@pos@%
                         \ifnum\WIASCD@badge@col=1 l\else r\fi%
                         \number\WIASCD@badge@row}{}%
        \global\@namedef{WIASCD@badge@background@%
                         \ifnum\WIASCD@badge@col=1 l\else r\fi%
                         \number\WIASCD@badge@row}{}%
        \addtolength{\WIASCD@badge@auxpos@x}{\WIASCD@badge@width}%
        \addtolength{\WIASCD@badge@auxpos@x}{\WIASCD@badge@gutter}%
      \ifnum\WIASCD@badge@col<\WIASCD@badge@cols
        \advance\WIASCD@badge@col by 1\relax
      \repeat
      }%
      \addtolength{\WIASCD@badge@auxpos@y}{\WIASCD@badge@height}%
      \addtolength{\WIASCD@badge@auxpos@y}{\WIASCD@badge@gap}%
    \ifnum\WIASCD@badge@row<\WIASCD@badge@rows
      \advance\WIASCD@badge@row by 1\relax
    \repeat
    \null
    \clearpage
  \endgroup
}
\def\badge{%
  \@ifnextchar[%]
    {\WIASCD@badge@@}{\WIASCD@badge@@[]}%
}
\def\WIASCD@badge@@[#1]{%
  \@ifnextchar[%]
    {\WIASCD@badge@@@[#1]}{\WIASCD@badge@@@[#1][]}%
}
\def\WIASCD@badge@@@[#1][#2]#3#4{%
  \def\WIASCD@tempa{#1}%
  \ifx\WIASCD@tempa\@empty
    \edef\WIASCD@tempa{\ifnum\WIASCD@badge@col=1 l\else r\fi%
                 \number\WIASCD@badge@row}%
    \advance\WIASCD@badge@col by 1\relax
    \ifnum\WIASCD@badge@col>\WIASCD@badge@cols
      \WIASCD@badge@col=1
      \advance\WIASCD@badge@row by 1\relax
    \fi
    \ifnum\WIASCD@badge@row>\WIASCD@badge@rows\WIASCD@badge@row=1\fi
  \fi
  \def\WIASCD@tempb{#2}\def\WIASCD@tempc{w}%
  \ifx\WIASCD@tempb\WIASCD@tempc
    \@namedef{WIASCD@badge@background@\WIASCD@tempa}{\WIASCD@badge@background}%
  \fi
  \def\WIASCD@tempb{#4}\def\WIASCD@tempc{WIAS}%
  \ifx\WIASCD@tempb\WIASCD@tempc
    \@namedef{WIASCD@badge@background@\WIASCD@tempa}{\WIASCD@badge@background}%
  \fi
  \@namedef{WIASCD@badge@name@\WIASCD@tempa}{#3}%
  \@namedef{WIASCD@badge@pos@\WIASCD@tempa}{#4}%
}
\newcommand*{\WIASCD@badge@background}{%
  \makebox[0mm][l]{%
    \if@WIASCD@badge@vcardformat
      \hspace{-\fboxsep}%
    \else\if@WIASCD@badge@selfadformat
      \hspace{-.5\fboxsep}%
    \else
      \hspace{\TPboxrulesize}%
    \fi\fi
    \colorbox{\WIASCD@badge@staffcolor}{%
      \parbox[t][.5\WIASCD@badge@height-2\fboxsep-2\TPboxrulesize]{%
        \if@WIASCD@badge@vcardformat
          \WIASCD@badge@width
        \else\if@WIASCD@badge@selfadformat
          \WIASCD@badge@width-\fboxsep
        \else
          \WIASCD@badge@width-2\fboxsep-2\TPboxrulesize
        \fi\fi
      }{\mbox{}}%
    }%
  }%
}
\newcommand*{\badgelogos}[1]{\renewcommand*{\WIASCD@badge@logos}{#1}}
\newcommand*{\WIASCD@badge@logos}{}
\newcommand*{\staffcolor}[1]{\renewcommand*{\WIASCD@badge@staffcolor}{#1}}
\newcommand*{\WIASCD@badge@staffcolor}{}
\newcommand*{\titlewidth}[1]{\renewcommand*{\WIASCD@badge@titlewidth}{#1}}
\newcommand*{\WIASCD@badge@titlewidth}{}
\badgelogos{\hfill\wiaslogo[plain,width=1.2cm]}
\staffcolor{wiasblue30}
\title{}
\titlewidth{.825}
\WIASCD@badge@reset





\endinput
%%
%% End of file `wiasbadge.cls'.
