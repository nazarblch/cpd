%%
%% This is file `wiasconf.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `conf')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.16}
\def\filedate{2015/07/03}
\def\filename{wiasconf}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Conference Poster, Flyer, and Infosigns]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\newif\if@WIASCD@conf@poster\@WIASCD@conf@posterfalse
\DeclareOption{poster}{\@WIASCD@conf@postertrue\@WIASCD@conf@twosidefalse}
\newif\if@WIASCD@conf@infosigns\@WIASCD@conf@infosignsfalse
\DeclareOption{infosigns}{\@WIASCD@conf@infosignstrue}
\newif\if@WIASCD@conf@expandbackpic\@WIASCD@conf@expandbackpictrue
\DeclareOption{noexpandbackpic}{\@WIASCD@conf@expandbackpicfalse}
\newif\if@WIASCD@conf@twoside\@WIASCD@conf@twosidetrue
\DeclareOption{oneside}{\@WIASCD@conf@twosidefalse}
\DeclareOption{twoside}{\@WIASCD@conf@twosidetrue}
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
\if@WIASCD@conf@poster
  \WIASCD@addto@globaloptions{a1paper,24pt}
\else
  \WIASCD@addto@globaloptions{a4paper,landscape,10pt,headings=normal}
\fi
\typeout{CLASS OPTIONS: \@classoptionslist}
\typeout{GLOBAL OPTIONS: \WIASCD@globaloptions}
\let\@classoptionslist\WIASCD@globaloptions

\LoadClass[\WIASCD@globaloptions]{scrartcl}

\RequirePackage{wiascd}
\RequirePackage{eurosym}
\RequirePackage{eso-pic}
\if@WIASCD@conf@infosigns
\RequirePackage{pgf}
\fi
\RequirePackage{calc}
\RequirePackage{geometry}

\if@WIASCD@conf@poster
  \geometry{%
     margin=\z@%
    ,nohead%
    ,nofoot%
    ,nomarginpar%
    ,heightrounded%
    ,textheight=82cm%
  }
\else\if@WIASCD@conf@infosigns
  \geometry{%
     margin=10mm%
    ,nohead%
    ,nofoot%
    ,nomarginpar%
    ,heightrounded%
  }
\else
  \geometry{%
     margin=\z@%
    ,nohead%
    ,nofoot%
    ,nomarginpar%
    ,heightrounded%
    ,textheight=20cm%
  }
\fi\fi
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

\pagestyle{empty}
\renewcommand*{\baselinestretch}{1.1}
\small\normalsize
\setlength{\parindent}{\z@}
\setlength{\leftmargini}{1.05cm}
\setlength{\leftmarginii}{1.05cm}
\setlength{\leftmarginiii}{1.05cm}
\if@WIASCD@conf@poster
  \setlength{\columnsep}{.024\paperwidth}
  \setlength{\parskip}{10mm}
\else
  \setlength{\parskip}{2ex}
\fi




\if@WIASCD@conf@poster
\newcommand*{\WIASCD@conf@poster@normalsize}{}
\let\WIASCD@conf@poster@normalsize\normalsize
\fi
\newcommand*{\WIASCD@conf@poster@colwidth@default}{.27\paperwidth}
\newcommand{\column}[3][\WIASCD@conf@poster@colwidth@default]{%
  \expandafter\renewcommand\expandafter*\expandafter{%
    \csname WIASCD@conf@poster@colwidth@#2\endcsname}{#1}%
  \expandafter\renewcommand\expandafter{%
    \csname WIASCD@conf@poster@colcontent@#2\endcsname}{#3}%
}
\newcommand*{\WIASCD@conf@poster@colwidth@l}{}
\newcommand*{\WIASCD@conf@poster@colwidth@c}{}
\newcommand*{\WIASCD@conf@poster@colwidth@r}{}
\newcommand*{\WIASCD@conf@poster@colcontent@l}{}
\newcommand*{\WIASCD@conf@poster@colcontent@c}{}
\newcommand*{\WIASCD@conf@poster@colcontent@r}{}
\let\WIASCD@conf@poster@colcontent@l\@empty
\let\WIASCD@conf@poster@colcontent@c\@empty
\let\WIASCD@conf@poster@colcontent@r\@empty
\newlength{\WIASCD@conf@logowidth}
\newlength{\WIASCD@conf@expandbackpicamount}
\newlength{\WIASCD@conf@paperwidth}
\if@WIASCD@conf@twoside
  \setlength{\WIASCD@conf@paperwidth}{.5\paperwidth}
\else
  \setlength{\WIASCD@conf@paperwidth}{\paperwidth}
\fi
\renewcommand{\title}[1]{\renewcommand{\WIASCD@conf@title}{#1}}
\newcommand{\booklet}[1]{\renewcommand{\WIASCD@conf@booklet}{#1}}
\newcommand*{\content}[1][\WIASCD@conf@poster@colwidth@default]{\column[#1]{c}}
\newcommand*{\orgdetails}[1][\WIASCD@conf@poster@colwidth@default]{\column[#1]{l}}
\newcommand*{\addinfo}[1][\WIASCD@conf@poster@colwidth@default]{\column[#1]{r}}
\newcommand{\WIASCD@conf@title}{}
\newcommand{\WIASCD@conf@booklet}{}

\newcommand{\confdata}[2]{%
  \textbf{\textcolor{wiasblue}{#1}}\\#2\par\vspace{2\baselineskip}%
}

\newcommand*{\backgroundpicture}[2][]{%
  \if@WIASCD@conf@poster
    \renewcommand*{\WIASCD@conf@backpicargs}{%
      % width=60cm,height=24cm at A1 paper format
      width=1.01\paperwidth,height=.285\paperheight%
    }%
  \else\if@WIASCD@conf@infosigns
    \renewcommand*{\WIASCD@conf@backpicargs}{%
      width=.7\paperwidth,height=.419186\paperheight%
    }%
  \else
    \setlength{\WIASCD@conf@expandbackpicamount}{\z@}%
    \if@WIASCD@conf@twoside
      \if@WIASCD@conf@expandbackpic
        \setlength{\WIASCD@conf@expandbackpicamount}{5mm}%
      \fi
      \renewcommand*{\WIASCD@conf@backpicargs}{%
        % width=15.35cm,height=65mm at A4 landscape paper format
        width=.5\paperwidth+\WIASCD@conf@expandbackpicamount,height=.3095\paperheight%
      }%
    \else % \if@WIASCD@conf@twoside
      \if@WIASCD@conf@expandbackpic
        \setlength{\WIASCD@conf@expandbackpicamount}{10mm}%
      \fi
      \renewcommand*{\WIASCD@conf@backpicargs}{%
        width=\paperwidth+\WIASCD@conf@expandbackpicamount,height=.3095\paperheight%
      }%
    \fi
  \fi\fi
  \g@addto@macro{\WIASCD@conf@backpicargs}{,#1}%
  \renewcommand*{\WIASCD@conf@backpic}{#2}%
}
\newcommand*{\WIASCD@conf@backpic}{}
\newcommand*{\WIASCD@conf@backpicargs}{}

\if@WIASCD@conf@poster
\newcommand*{\printposter}{%
  \setlength{\pagemargin}{.067\paperwidth}%
  \printbackground
  \hspace*{\pagemargin}%
  \begin{minipage}[t][.595\paperheight][t]{.866\paperwidth}%
    \setlength{\parskip}{2ex}%
    \raggedright
    \vspace*{.262\paperheight}%
    \rlap{\smash{\parbox[b]{\linewidth}{\printtitle}}}%
    \par\vspace{.095\paperheight}%
    \begin{minipage}[t][.595\paperheight][t]{\WIASCD@conf@poster@colwidth@l}%
      \setlength{\parskip}{.5\baselineskip}%
      \WIASCD@conf@poster@colcontent@l
      \par\vfill
    \end{minipage}%
    \ifx\WIASCD@conf@poster@colcontent@c\@empty\else
      \hspace{\columnsep}%
      \begin{minipage}[t][.595\paperheight][t]{\WIASCD@conf@poster@colwidth@c}%
        \setlength{\parskip}{.5\baselineskip}%
        \WIASCD@conf@poster@colcontent@c
      \end{minipage}%
    \fi
    \ifx\WIASCD@conf@poster@colcontent@r\@empty\else
      \hspace{\columnsep}%
      \begin{minipage}[t][.595\paperheight][t]{\WIASCD@conf@poster@colwidth@r}%
        \setlength{\parskip}{.5\baselineskip}%
        \WIASCD@conf@poster@colcontent@r
      \end{minipage}%
    \fi
  \end{minipage}%
}
\else\if@WIASCD@conf@infosigns % \if@WIASCD@conf@poster
\newcommand*{\WIASCD@conf@info@pictitle}{%
  \rule{\z@}{.4\paperheight}%
  \hspace*{.135\paperwidth}%
  \parbox[b]{.73\paperwidth}{%
    \fontsize{24}{36}\selectfont
    \bfseries
    \textcolor{white}{\WIASCD@conf@title}%
  }%
}
\newcommand*{\printinfosigns}{%
  \ifdim\pagemargin=\z@
    \setlength{\pagemargin}{10mm}%
  \fi
  \AddToShipoutPicture{%
    \put(\LenToUnit{.5\paperwidth},\LenToUnit{.525\paperheight}){%
      \makebox[0mm][c]{%
        \expandafter\includegraphics\expandafter[\WIASCD@conf@backpicargs]%
          {\WIASCD@conf@backpic}%
      }%
    }%
    \AtPageLowerLeft{%
      \hspace*{\pagemargin}%
      \hspace*{5mm}%
      \parbox[b]{\paperwidth}{%
        \hspace*{-5mm}%
        \wiaslogo[plain,height=2.5cm]%
        \par\vspace{1mm}%
        \raggedright
        \normalsize\bfseries
        \WIASCD@institutename@twoline
        \par\vspace{2mm}%
        \textcolor{wiasblue}{www.\WIASCD@domain}%
        \vspace*{\pagemargin}%
      }%
    }%
  }%

  %%% main entrance
  \vspace*{8.0cm}%
  \begin{center}%
    \fontsize{42}{56}\selectfont
    \bfseries
    \WIASCD@conf@title\\\@date
  \end{center}%
  \newpage

  %%% back door
  \WIASCD@conf@info@pictitle
  \vfill
  \begin{center}%
    \begin{pgfpicture}{(\z@)}{(\z@)}{(12cm)}{(7cm)}%
      \pgfsetlinewidth{2mm}%
      \pgfxyline(2,2)(10.1,2)%
      \pgfxyline(10,2)(10,5.1)%
      \pgfxyline(2,1.9)(2,5.1)%
      \pgfxyline(2,5)(10,5)%
      \pgfsetlinewidth{3mm}%
      \pgfxyline(7,1)(.85,1)%
      \pgfxyline(1,1)(1,6.15)%
      \pgfxyline(1,6)(6,6)%
      \pgfsetendarrow{\pgfarrowtriangle{5mm}}%
      \pgfxyline(6,6.15)(6,4.9)%
      \pgfputat{\pgfxy(6,3.5)}{\pgfbox[center,center]{%
        \fontsize{36}{44}\selectfont
        \bfseries
        WIAS
      }}%
    \end{pgfpicture}%
  \end{center}%
  \vfill
  \begin{center}%
    \fontsize{20}{24}\selectfont
    \bfseries
    Please, use entrance Mohrenstrasse 39! \\
    Nutzen Sie bitte den Eingang Mohrenstra\ss e 39!
  \end{center}%
  \newpage

  %%% registration desk
  \WIASCD@conf@info@pictitle
  \vspace*{3cm}%
  \begin{center}%
    \fontsize{42}{56}\selectfont
    \bfseries
    Registration
  \end{center}%
  \newpage

  %%% computer/session
  \WIASCD@conf@info@pictitle
  \vspace*{2cm}%
  \begin{center}%
    \begin{pgfpicture}{(\z@)}{(\z@)}{(17cm)}{(5cm)}%
      \pgfsetendarrow{\pgfarrowtriangle{5mm}}%
      \pgfsetlinewidth{3mm}%
      \pgfxyline(6.5,1)(16.25,1)%
      \pgfxyline(11.5,4)(.75,4)%
      \pgfputat{\pgfxy(17.25,4)}{\pgfbox[right,center]{%
        \fontsize{36}{44}\selectfont
        \bfseries
        Sessions
      }}%
      \pgfputat{\pgfxy(0,1)}{\pgfbox[left,center]{%
        \fontsize{36}{44}\selectfont
        \bfseries
        Computers
      }}%
    \end{pgfpicture}%
  \end{center}%
  \newpage

  %%% session/computer
  \WIASCD@conf@info@pictitle
  \vspace*{2cm}%
  \begin{center}%
    \begin{pgfpicture}{(\z@)}{(\z@)}{(17cm)}{(5cm)}%
      \pgfsetendarrow{\pgfarrowtriangle{5mm}}%
      \pgfsetlinewidth{3mm}%
      \pgfxyline(10.5,1)(.75,1)%
      \pgfxyline(5.5,4)(16.25,4)%
      \pgfputat{\pgfxy(0,4)}{\pgfbox[left,center]{%
        \fontsize{36}{44}\selectfont
        \bfseries
        Sessions
      }}%
      \pgfputat{\pgfxy(17.25,1)}{\pgfbox[right,center]{%
        \fontsize{36}{44}\selectfont
        \bfseries
        Computers
      }}%
    \end{pgfpicture}%
  \end{center}%
  \newpage
}
\else % \if@WIASCD@conf@infosigns
\newcommand*{\printcover}{%
  \ifdim\pagemargin=\z@
    \setlength{\pagemargin}{5mm}%
  \fi
  \printbackground
  \AddToShipoutPicture*{%
    \AtPageLowerLeft{%
      \if@WIASCD@conf@twoside\hspace*{.5\paperwidth}\fi
      \hspace*{.1\WIASCD@conf@paperwidth}%
      \begin{minipage}[b][.95\paperheight][t]{.8\WIASCD@conf@paperwidth}%
        \setlength{\parskip}{2ex}%
        \raggedright
        \vspace*{.22\paperheight}%
        \rlap{\smash{\parbox[b]{.8\WIASCD@conf@paperwidth}{\printtitle}}}%
        \par\vspace{.15\paperheight}%
        \printbooklet
        \par\vfill
      \end{minipage}%
    }%
  }%
  \null
}
\fi\fi   % \if@WIASCD@conf@infosigns, \if@WIASCD@conf@poster
\newcommand*{\printtitle}{%
  \bgroup
    \rmfamily
    \bfseries
    \if@WIASCD@conf@poster
      \fontsize{85}{100}\selectfont
    \else
      \Huge
    \fi
    \textcolor{white}{\WIASCD@conf@title}%
    \par
  \egroup
}
\newcommand*{\printbooklet}{%
  \bgroup
    \LARGE
    \textcolor{wiasblue}{\WIASCD@conf@booklet}%
    \par
  \egroup
}
\newsavebox{\WIASCD@conf@backpicbox}
\newcommand*{\printbackground}{%
  \sbox{\WIASCD@conf@backpicbox}{\expandafter\includegraphics\expandafter[\WIASCD@conf@backpicargs]{\WIASCD@conf@backpic}}%
  \AddToShipoutPicture*{%
    \AtPageUpperLeft{%
      \if@WIASCD@conf@poster\else
        \if@WIASCD@conf@twoside
          \parbox[t]{\WIASCD@conf@paperwidth}{%
            \vspace*{\z@}%
            \if@WIASCD@crop
              \vspace*{-5mm}%
              \hspace*{-5mm}%
            \fi
            \textcolor{wiasblue}{\rule[-\ht\WIASCD@conf@backpicbox]{\linewidth+5mm}{\ht\WIASCD@conf@backpicbox}}%
          }%
        \fi
      \fi
      \parbox[t]{\WIASCD@conf@paperwidth}{%
        \vspace*{\z@}%
        \if@WIASCD@crop
          \vspace*{-5mm}%
          \if@WIASCD@conf@twoside\else
            \hspace*{-5mm}%
          \fi
        \fi
        \rlap{\usebox{\WIASCD@conf@backpicbox}}%
      }%
    }%
    \AtPageLowerLeft{%
      \if@WIASCD@conf@twoside\hspace*{.5\paperwidth}\fi
      \hspace*{\pagemargin}%
      \parbox[b]{\WIASCD@conf@paperwidth}{%
        \bgroup
          \if@WIASCD@conf@poster
            \setlength{\WIASCD@conf@logowidth}{45mm}%
          \else
            \setlength{\WIASCD@conf@logowidth}{18mm}%
            \footnotesize
          \fi
          \hspace*{-.225\WIASCD@conf@logowidth}%
          \wiaslogo[plain,width=\WIASCD@conf@logowidth]%
          \par\vspace{.0555\WIASCD@conf@logowidth}%
          \raggedright
          \bfseries
          \if@WIASCD@conf@poster
            \bgroup
            \WIASCD@conf@poster@normalsize
          \fi
          \WIASCD@institutename@twoline
          \par\vspace{.111\WIASCD@conf@logowidth}%
          \textcolor{wiasblue}{www.\WIASCD@domain}%
          \if@WIASCD@conf@poster
            \egroup
          \fi
          \if@WIASCD@conf@poster
            \vspace*{.45\pagemargin}%
          \else
            \vspace*{\pagemargin}%
          \fi
        \egroup
      }%
    }%
  }%
}
\newlength{\pagemargin}
\setlength{\pagemargin}{\z@}




\endinput
%%
%% End of file `wiasconf.cls'.
