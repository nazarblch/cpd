%%
%% This is file `wiasvcard.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% weierstrass.dtx  (with options: `vcard')
%% 2009/2010 da vinci design GmbH
%% 2010-2015 WIAS (H. Pletat; with parts by J. Fuhrmann and K. Tabelow)

\def\fileversion{1.12}
\def\filedate{2013/02/12}
\def\filename{wiasvcard}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{\filename}[\filedate\space v\fileversion\space
                           WIAS Business Cards]

\newcommand*{\WIASCD@addto@globaloptions}[1]{%
  \g@addto@macro{\WIASCD@globaloptions}{,#1}%
}
\newcommand*{\WIASCD@globaloptions}{}
\RequirePackage{ifpdf}
\ifpdf\else\WIASCD@addto@globaloptions{dvips}\fi
\WIASCD@addto@globaloptions{a4paper,11pt}
\DeclareOption{nomathpazo}{\PassOptionsToPackage{nomathpazo}{wiascd}}
\DeclareOption{mathpazo}{\PassOptionsToPackage{mathpazo}{wiascd}}
\DeclareOption{nomathptmx}{\PassOptionsToPackage{nomathptmx}{wiascd}}
\DeclareOption{mathptmx}{\PassOptionsToPackage{mathptmx}{wiascd}}
\DeclareOption{showboxes}{\PassOptionsToPackage{showboxes}{textpos}}
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


\newlength{\WIASCD@vcard@start@x}
\newlength{\WIASCD@vcard@start@y}
\newlength{\WIASCD@vcard@offset@x}
\newlength{\WIASCD@vcard@offset@y}
\newlength{\WIASCD@vcard@width}
\newlength{\WIASCD@vcard@height}
\newlength{\WIASCD@vcard@gutter}
\newlength{\WIASCD@vcard@auxpos@x}
\newlength{\WIASCD@vcard@auxpos@y}
\newcount\WIASCD@vcard@cols
\newcount\WIASCD@vcard@rows
\newcount\WIASCD@vcard@col
\newcount\WIASCD@vcard@row

\newcommand*{\WIASCD@vcard@name}{}
\newcommand*{\WIASCD@vcard@position}{}
\newcommand*{\WIASCD@vcard@mailname}{}
\newcommand*{\WIASCD@vcard@phoneext}{}
\newcommand*{\WIASCD@vcard@fax}{\WIASCD@fax}
\newcommand*{\WIASCD@vcard@address}{%
  \WIASCD@address@oneline\\
  www.\WIASCD@domain\ $\cdot$ Tel. \WIASCD@phoneprefix\ \WIASCD@vcard@phoneext\\
  \WIASCD@vcard@mailname @\WIASCD@domain
}

\newcommand*{\WIASCD@vcard@reset}{%
  \setlength{\WIASCD@vcard@start@x}{0mm}%
  \setlength{\WIASCD@vcard@start@y}{0mm}%
  \setlength{\WIASCD@vcard@auxpos@x}{0mm}%
  \setlength{\WIASCD@vcard@auxpos@y}{0mm}%
  \setlength{\WIASCD@vcard@offset@x}{15mm}%
  \setlength{\WIASCD@vcard@offset@y}{10mm}%
  \setlength{\WIASCD@vcard@width}{85mm}%
  \setlength{\WIASCD@vcard@height}{55mm}%
  \setlength{\WIASCD@vcard@gutter}{10mm}%
  \WIASCD@vcard@cols=2%
  \WIASCD@vcard@rows=5%
  \WIASCD@vcard@col=1%
  \WIASCD@vcard@row=1%
}

\newcommand*{\WIASCD@vcard@institutename}{%
  \raggedright
  \bgroup
    \large\bfseries\color{wiasblue}%
    \iflanguage{german}{\WIASCD@institute@german}{}%
    \iflanguage{ngerman}{\WIASCD@institute@german}{}%
    \iflanguage{english}{\WIASCD@institute@english}{}%
  \egroup
  \\[-.4ex]%
  \bgroup
    \let\WIASCD@spnl\ % space
    \footnotesize
    \iflanguage{german}{\WIASCD@institutefor@german}{}%
    \iflanguage{ngerman}{\WIASCD@institutefor@german}{}%
    \iflanguage{english}{\WIASCD@institutefor@english}{}%
  \egroup
}

\define@key{WIASCD@vcard@key}{german}[true]{\selectlanguage{german}}
\define@key{WIASCD@vcard@key}{ngerman}[true]{\selectlanguage{german}}
\define@key{WIASCD@vcard@key}{english}[true]{\selectlanguage{english}}
\define@key{WIASCD@vcard@key}{hoffset}{\WIASCD@vcard@offset@x=#1}
\define@key{WIASCD@vcard@key}{voffset}{\WIASCD@vcard@offset@y=#1}
\newcommand*{\vcardfax}[1]{\renewcommand{\WIASCD@vcard@fax}{#1}}
\newcommand*{\vcard}[5][]{%
  \WIASCD@vcard@reset
  \begingroup
    \setkeys{WIASCD@vcard@key}{#1}%
    \renewcommand*{\WIASCD@vcard@name}{#2}%
    \renewcommand*{\WIASCD@vcard@position}{#3}%
    \renewcommand*{\WIASCD@vcard@mailname}{#4}%
    \renewcommand*{\WIASCD@vcard@phoneext}{#5}%
    \addtolength{\WIASCD@vcard@start@x}{\WIASCD@vcard@offset@x}%
    \addtolength{\WIASCD@vcard@start@y}{\WIASCD@vcard@offset@y}%
    \setlength{\WIASCD@vcard@auxpos@x}{\WIASCD@vcard@start@x}%
    \setlength{\WIASCD@vcard@auxpos@y}{\WIASCD@vcard@start@y}%
    \loop
      {%
      \loop
        \begin{textblock*}{\WIASCD@vcard@width}(\WIASCD@vcard@auxpos@x,\WIASCD@vcard@auxpos@y)
          \parbox[t][\WIASCD@vcard@height]{\linewidth}{%
            \hspace*{5mm}%
            \parbox[t]{12.8mm}{%
              \vspace*{0mm}%
              \vspace{6mm}%
              \wiaslogo[plain,width=12.8mm]%
            }%
            \hspace*{5mm}%
            \parbox[t]{62mm}{%
              \vspace*{0mm}%
              \nointerlineskip
              \parbox[b][17mm][b]{\linewidth}{%
                \WIASCD@vcard@institutename
              }%
              \par\nointerlineskip
              \parbox[b][19mm][b]{\linewidth}{%
                \footnotesize
                \raggedright
                \textbf{\color{wiasblue}\WIASCD@vcard@name}\\\WIASCD@vcard@position
              }%
              \par\nointerlineskip
              \parbox[b][13mm][b]{\linewidth}{%
                \footnotesize
                \raggedright
                \WIASCD@vcard@address
              }%
            }%
          }%
        \end{textblock*}
        \addtolength{\WIASCD@vcard@auxpos@x}{\WIASCD@vcard@width}%
        \addtolength{\WIASCD@vcard@auxpos@x}{\WIASCD@vcard@gutter}%
      \ifnum\WIASCD@vcard@col<\WIASCD@vcard@cols%
        \advance\WIASCD@vcard@col by 1\relax
      \repeat
      }%
      \addtolength{\WIASCD@vcard@auxpos@y}{\WIASCD@vcard@height}%
    \ifnum\WIASCD@vcard@row<\WIASCD@vcard@rows%
      \advance\WIASCD@vcard@row by 1\relax
    \repeat
    \null
    \clearpage
  \endgroup
}
\AtEndDocument{%
  \typeout{}%
  \typeout{====================================================}%
  \typeout{Please ensure NOT to use any shrink or resize option}%
  \typeout{while printing (especially from Adobe Reader).      }%
  \typeout{====================================================}%
  \typeout{}%
}






\endinput
%%
%% End of file `wiasvcard.cls'.
