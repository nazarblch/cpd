%%
%% This is file `wiaslttr.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `letter')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.19}
\def\filedate{2015/01/19}
\def\filename{wiaslttr}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Letter]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\WIASCD@addto@globaloptions{a4paper,11pt}
\newif\if@WIASCD@letter@ipbb\@WIASCD@letter@ipbbfalse
\DeclareOption{inno2008}{\@WIASCD@letter@ipbbtrue}
\DeclareOption{noinno2008}{\@WIASCD@letter@ipbbfalse}
\ifnum\year>2008\ifnum\year<2011
  \ExecuteOptions{inno2008}
\fi\fi
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

\LoadClass[\WIASCD@globaloptions]{scrlttr2}

\RequirePackage{wiascd}
\RequirePackage{eurosym}
\RequirePackage{scrpage2}
\RequirePackage{lastpage}
\RequirePackage{calc}
\RequirePackage{geometry}

\geometry{%
   includeheadfoot%
  ,heightrounded%
  ,hmargin={2.5cm,4cm}%
  ,bottom=1.5cm%
  ,headheight=3.9cm%
  ,headsep=-.075cm%
  ,textheight=21cm%
}

\pagestyle{scrheadings}
\clearscrheadfoot
\renewcommand*{\baselinestretch}{1.1}
\small\normalsize
\setlength{\parindent}{\z@}
\setlength{\parskip}{2ex}
\clubpenalty         = 10000
\widowpenalty        = 10000
\displaywidowpenalty = 10000






\KOMAoptions{%
   numericaldate=true%
  ,fromalign=left%
  ,fromrule=false%
  ,fromphone=false%
  ,fromemail=false%
  ,fromurl=false%
  ,foldmarks=true%
  ,addrfield=true%
  ,enlargefirstpage=true%
}
\@setplength{toaddrhpos}{2.5cm}
\@setplength{tfoldmarkvpos}{10.5cm}
\@setplength{foldmarkhpos}{0cm}
\@setplength{sigindent}{0cm}
\@setplength{refaftervskip}{-3.35ex}
\@setplength{refvpos}{10.3cm}
\@setplength{firstheadwidth}{\paperwidth}
\@setplength{firstfootwidth}{\paperwidth}
\@setplength{firstfootvpos}{27.7cm}
\@setplength{sigbeforevskip}{3\baselineskip}%
\newcommand*{\WIASCD@letter@smallfont}{\fontsize{8}{10}\selectfont}
\setkomafont{pagehead}{\normalfont\itshape\WIASCD@letter@smallfont}
\setkomafont{pagefoot}{\normalfont\itshape\WIASCD@letter@smallfont}
\setkomafont{pagenumber}{\normalfont\itshape}
\setkomafont{backaddress}{\normalfont\WIASCD@letter@smallfont}
\lohead{\@date\ $\cdot$ \pagename\ \pagemark\ \WIASCD@letter@pagesof\ \pageref{LastPage}}
\rohead{\wiaslogo[plain,width=2.1cm] \hspace{-18mm}\vspace{17mm}}
\firsthead{%
  \raggedleft
  \begin{minipage}[b][3cm][t]{18.62cm}%
    \usekomavar{fromlogo}%
    \hfill
    \begin{minipage}[t][3cm][b]{4.37cm}%
      \wiaslogo[plain,width=2.1cm]%
    \end{minipage}%
  \end{minipage}%
  \par\nointerlineskip
  \begin{picture}(0,0)%
    \put(0,0){%
      \begin{minipage}[t][70mm][t]{6.5cm}%
        \vspace*{6.5mm}%
        \WIASCD@letter@smallfont
        \raggedright
        WIAS $\cdot$ \WIASCD@address@short\\
        Tel. \WIASCD@phoneprefix 0 $\cdot$ Fax \WIASCD@fax\\
        contact@\WIASCD@domain $\cdot$ www.\WIASCD@domain\\[3mm]%
        \textbf{%
          \ifkomavarempty{fromname}{}{\usekomavar{fromname}}%
          \ifkomavarempty{fromposition}{}{\\\usekomavar{fromposition}}%
        }%
        \ifkomavarempty{fromphone}{}{\\Tel. \usekomavar{fromphone}}%
        \ifkomavarempty{fromfax}{}{\\Fax \usekomavar{fromfax}}%
        \ifkomavarempty{fromemail}{}{\\\usekomavar{fromemail}}%
        \ifkomavarempty{writer}{}{\\[3mm]\usekomavar{writer}}%
        \vfill
      \end{minipage}%
      \hspace*{.5cm}%
    }%
  \end{picture}%
  % The width of the above minipage and \hspace.
  \hspace*{7cm}%
}
\firstfoot{%
  \hspace*{1in}\hspace*{\hoffset}\hspace*{\oddsidemargin}%
  \begin{picture}(0,0)%
    \setlength{\unitlength}{1mm}%
    \put(0,-8){%
      \parbox{\linewidth}{%
        \raisebox{-0.4mm}{\abuflogo[height=1.38cm]}%
        \hfill
        \parbox[b]{10.0cm}{%
          \WIASCD@letter@smallfont
          \vfill
          \bgroup
            \color{wiasblue}%
            \textbf{\WIASCD@institutename@oneline}\\%
            \textbf{\WIASCD@fvbname}%
          \egroup
          \par
          \usekomavar{frombank}%
        }%
        \hfill
        \raisebox{-3.0mm}{\wgllogo[height=1.6cm]}%
      }%
    }%
  \end{picture}%
}
\newcommand{\toname}{\setkomavar{toname}}
\newcommand{\toaddress}{\setkomavar{toaddress}}
\newcommand{\name}[2][]{%
  \def\WIASCD@tempa{#1}
  \ifx\WIASCD@tempa\@empty
    \setkomavar{fromnameshort}{#2}%
  \else
    \setkomavar{fromnameshort}{#1}%
  \fi
  \setkomavar{fromname}{#2}%
}
\newcommand{\signature}{\setkomavar{signature}}
\newcommand{\position}{\setkomavar{fromposition}}
\newcommand{\department}{\setkomavar{fromposition}}
\newcommand{\address}{\setkomavar{fromaddress}}
\newcommand{\backaddress}{\setkomavar{backaddress}}
\newcommand{\telephone}[2][\WIASCD@phoneprefix]{%
  \setkomavar{fromphone}{#1 #2}%
}
\newcommand{\fax}[2][\WIASCD@phoneprefix]{\setkomavar{fromfax}{#1 #2}}
\newcommand{\email}[1]{%
  \setkomavar{fromemail}{#1@\WIASCD@domain}%
}
\newcommand{\fromlogo}{\setkomavar{fromlogo}}
\newcommand{\subject}[1]{%
  \setkomavar{subject}{\parbox[t]{11.5cm}{\raggedright #1}}%
}
\newcommand{\writer}[2]{%
  \setkomavar{writer}{%
    \parbox[t]{5cm}{%
      \WIASCD@letter@writername: #1%
      \ $\cdot$\ %
      \WIASCD@letter@phoneextname: #2%
    }%
  }%
}
\newcommand*{\WIASCD@letter@writername}{%
  \iflanguage{german}{Bearbeiter}{}%
  \iflanguage{ngerman}{Bearbeiter}{}%
  \iflanguage{english}{writer}{}%
  \xspace
}
\newcommand*{\WIASCD@letter@phoneextname}{%
  \iflanguage{german}{Durchwahl}{}%
  \iflanguage{ngerman}{Durchwahl}{}%
  \iflanguage{english}{extension}{}%
  \xspace
}
\newcommand*{\WIASCD@letter@pagesof}{%
  \iflanguage{german}{von}{}%
  \iflanguage{ngerman}{von}{}%
  \iflanguage{english}{of}{}%
  \xspace
}
\newkomavar{fromnameshort}
\newkomavar{writer}
\newkomavar{fromposition}
\let\raggedsignature\raggedright

\if@WIASCD@letter@ipbb
  \fromlogo{%
    \begin{minipage}[t][2.9cm][b]{9cm}%
      \ipbblogo[width=.8cm] %
      \WIASCD@letter@smallfont
      Tr\"ager des Innovationspreises Berlin-Brandenburg 2008
    \end{minipage}%
  }
\fi

\setkomavar{frombank}{%
  \ifnum\WIASCD@year@month<201206
    \newcommand*{\WIASCD@letter@bankname}{Commerzbank AG}
    \newcommand*{\WIASCD@letter@banknumber}{BLZ 100 400 00}
    \newcommand*{\WIASCD@letter@accountnumber}{Kto.-Nr. 520 430 002}
    \newcommand*{\WIASCD@letter@iban}{}
    \newcommand*{\WIASCD@letter@bic}{}
    \newcommand*{\WIASCD@letter@accountinfo}{%
      \WIASCD@letter@banknumber\ $\cdot$\ \WIASCD@letter@accountnumber
    }
  \else\ifnum\WIASCD@year@month<201501
    \newcommand*{\WIASCD@letter@bankname}{GLS Gemeinschaftsbank eG}
    \newcommand*{\WIASCD@letter@banknumber}{BLZ 430 609 67}
    \newcommand*{\WIASCD@letter@accountnumber}{Kto.-Nr. 113 113 302}
    \newcommand*{\WIASCD@letter@iban}{IBAN DE75 4306 0967 0113 1133 02}
    \newcommand*{\WIASCD@letter@bic}{BIC GENODEM1GLS}
    \newcommand*{\WIASCD@letter@accountinfo}{%
      \WIASCD@letter@iban\ $\cdot$\ \WIASCD@letter@bic
    }
  \else
    \newcommand*{\WIASCD@letter@bankname}{Commerzbank AG}
    \newcommand*{\WIASCD@letter@banknumber}{BLZ 100 400 00}
    \newcommand*{\WIASCD@letter@accountnumber}{Kto.-Nr. 520 430 002}
    \newcommand*{\WIASCD@letter@iban}{IBAN DE45 1004 0000 0520 4300 02}
    \newcommand*{\WIASCD@letter@bic}{BIC COBADEFFXXX}
    \newcommand*{\WIASCD@letter@accountinfo}{%
      \WIASCD@letter@iban\ $\cdot$\ \WIASCD@letter@bic
    }
  \fi\fi
  \WIASCD@letter@bankname\ $\cdot$\ \WIASCD@letter@accountinfo\\Steuernr. 27/640/51604
}

\AtBeginLetter{%
  \backaddress{%
    \ifkomavarempty{fromnameshort}{}{\usekomavar{fromnameshort} $\cdot$ }%
    WIAS $\cdot$ \WIASCD@address@short
  }%
  \setkomavar{location}{}%
}
\renewcommand{\@foldmark}[1]{%
  \vskip  #1%
  \vb@t@z{\rlap{\move@topl\hskip\useplength{foldmarkhpos}%
    \rule{5mm}{.25pt}}}%
  \vskip -#1%
  \ignorespaces
}
\renewcommand{\@foldmarks}{%
  \if@fold
    \typeout{Foldmarks: yes}%
    \move@topt
    \@foldmark{\useplength{tfoldmarkvpos}}%
    \@foldmark{.5\paperheight}%
    \move@frompt
  \else
    \typeout{Foldmarks: no}%
  \fi
}
\newcommand*{\WIASCD@letter@addrfield@orig}{}
\let\WIASCD@letter@addrfield@orig\@addrfield
\newcommand*{\WIASCD@letter@vfil@orig}{}
\let\WIASCD@letter@vfil@orig\vfil
\providecommand{\backaddr@format}{}
\let\backaddr@format\@firstofone
\renewcommand*{\@addrfield}{%
  \begingroup
    \let\underline\@firstofone
    \def\vfil{\vspace{1mm}\let\vfil\WIASCD@letter@vfil@orig}%
    \WIASCD@letter@addrfield@orig
  \endgroup
}
\newcommand*{\WIASCD@letter@closing@orig}{}
\let\WIASCD@letter@closing@orig\closing
\renewcommand*{\closing}[1]{%
  \WIASCD@letter@closing@orig{#1}%
  \vspace{-.9\baselineskip}%
}
\newcommand{\formletter}[3]{%
  \begin{letter}{#1\\#2}%
    \opening{#3}%
    \@formlettertext
  \end{letter}%
}
\newcommand{\formlettertext}[1]{%
  \renewcommand{\WIASCD@letter@formlettertext}{#1}}
\newcommand{\WIASCD@letter@formlettertext}{}
\newcommand*{\makelabels}{%
  \AtBeginDocument{%
    \let\@startlabels\startlabels
    \let\@mlabel\mlabel
    \if@filesw
      \immediate\write\@mainaux{\string\@startlabels}%
    \fi
  }%
  \BeforeClosingMainAux{%
    \if@filesw
      \immediate\write\@mainaux{\string\clearpage}%
    \fi
  }%
}
\newcommand*{\startlabels}{%
  \pagestyle{empty}%
  \let\@texttop\relax
  \topmargin -.95in
  \headheight \z@
  \headsep \z@
  \oddsidemargin -.85in
  \evensidemargin -.85in
  \textheight 297mm
  \textwidth 202mm
  \columnsep 2mm
  \paperwidth 210mm
  \paperheight 297mm
  \activateareas
  \fontsize{10pt}{12pt}\selectfont
  \lineskip \z@
  \boxmaxdepth \z@
  \parindent \z@\parskip \z@\parfillskip 0pt plus 1fil\relax
  \twocolumn\relax
}
\let\@startlabels=\relax
\@onlypreamble\makelabels
\newcommand*{\mlabel}[2]{%
  \def\WIASCD@tempa{#2}\def\WIASCD@tempb{\\}%
    \parbox[b][41.765mm][c]{100mm}{%
      \centering\parbox[b][41.765mm][c]{95mm}{%
        \strut\ignorespaces
        \ifx\WIASCD@tempa\WIASCD@tempb\else
          \underline{\footnotesize #1}\\[.667\baselineskip]\relax #2%
        \fi
      }%
    }%
}
\let\@mlabel=\@gobbletwo
\AtEndLetter{\if@filesw
  \begingroup
    \let\\=\relax
    \let\protect\@unexpandable@protect
    \protected@edef\toname{\scr@toname@var}%
    \protected@edef\toaddress{\scr@toaddress@var}%
    \protected@edef\returnaddress{\scr@backaddress@var}%
    \immediate\write\@auxout{%
      \string\@mlabel{\returnaddress}{\toname\\\toaddress}%
    }%
  \endgroup
\fi}


\endinput
%%
%% End of file `wiaslttr.cls'.
