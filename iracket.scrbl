#lang scribble/manual
@(require racket/runtime-path
          scribble/manual
          scribble/basic
          (for-label racket/base racket/contract iracket/install))

@title[#:version "1.3"]{IRacket: Racket Kernel for Jupyter}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library provides a Racket kernel for @hyperlink["http://jupyter.org/"]{Jupyter},
enabling interactive notebook-style programming with Racket.


@section[#:tag "install"]{Installing the IRacket Jupyter Kernel}

After installing the @tt{iracket} package, you must register the IRacket kernel
with Jupyter. Kernel registration can be done either through the @exec{raco
iracket install} command or by using the @racketmodname[iracket/install] module.

If @exec{racket} and @exec{jupyter} are both in your executable search
path, then you can register the kernel by either of the following:
@itemlist[
@item{at the command line: @exec{raco iracket install --untrusted}, or}
@item{in Racket: @racket[(begin (require iracket/install) (install-iracket! #:trusted? #f))]}
]
If the kernel is installed in untrusted mode (@exec{--untrusted}, also the
default), the kernel runs Racket programs in a restricted environment that
limits filesystem access and forbids network access.  If the kernel is installed
in trusted mode (with the @exec{--trusted} flag), then the kernel will run
Racket programs without any restrictions.

If the @exec{jupyter} command is not in your executable search path, you must
tell the installer the absolute path to the @exec{jupyter} executable using
the @exec{--jupyter-exe} flag or @racket[#:jupyter-exe] keyword argument. The
installer runs @exec{jupyter --data-dir} to find the directory where it
should install the IRacket kernel.

If the @exec{racket} command is not in your executable search path, or if you
want to register a kernel that uses a specific version of Racket, then you must
tell the installer the path to the executable to use. The executable must
support the same command-line interface that the @exec{racket} executable
supports, but it does not have to be named @litchar{racket}---for example, a
@exec{racketcgc} or @exec{racketcs} executable would also work. See the
output of @exec{raco iracket install --help} or the documentation for
@racket[install-iracket!] for details.

Note that if you register the kernel with a non-version-specific Racket command
(the default) and then change that command to run a different version of Racket
(for example, by changing your @tt{PATH} or by installing a new version of
Racket over the old one), then Jupyter will try to use the new version of Racket
to run the IRacket kernel. If the @tt{iracket} package is not installed in the
new version, this typically results in an error like ``collection not found for
module path: @racketmodname[(lib "iracket/iracket")]''. The same error will
occur if you try to run the kernel after removing the @tt{iracket} package.

@history[#:changed "1.1" @elem{Added @exec{raco iracket} command.}
         #:changed "1.3" @elem{Added @exec{--trusted} and @exec{--untrusted} flags.}]

@subsection[#:tag "install-api"]{IRacket Installation API}

@defmodule[iracket/install]

@defproc[(install-iracket! [#:jupyter-exe jupyter-exe
                            (or/c (and/c path-string? complete-path?) #f) #f]
                           [#:racket-exe racket-exe
                            (or/c path-string? 'auto 'this-version) #f]
                           [#:trusted? trusted? boolean? #f])
         void?]{

Registers the IRacket kernel in the location reported by @racket[jupyter-exe]
(with the @exec{--data-dir} flag). If @racket[racket-exe] is a path or
string, then it is included in the kernel registration as the command Jupyter
should use to run the kernel. If @racket[racket-exe] is @racket[#f], then the
registration uses the generic @exec{racket} command, but only if it is
included in the current executable search path; otherwise, an error is
raised. If @racket[racket-exe] is @racket['this-version], then the absolute path
to the currently running version of Racket is used.

If @racket[trusted?] is false (the default), then the kernel runs Racket
programs in a restricted environment that limits filesystem access and forbids
network access.  If @racket[trusted?] is true, then the kernel will run Racket
programs without any restrictions.

@history[#:added "1.1" #:changed "1.3" @elem{Added @racket[#:trusted?] argument.}]
}


@section[#:tag "lang"]{IRacket and Languages}

The IRacket kernel does not support Racket's @(hash-lang) syntax for selecting
languages, for the same reason that syntax doesn't work at the Racket REPL. That
is, @(hash-lang) is a syntax for @emph{whole modules}, whereas both the REPL and
Jupyter work with top-level forms and generally receive them one at a time. See
also @secref["hopeless"].

Instead of general @(hash-lang) support, IRacket recognizes
@litchar{#lang iracket/lang} as a special declaration for adjusting
the notebook's language.

@specform[(code:line #, @litchar{#lang iracket/lang} #:require lang-mod maybe-reader)
          #:grammar
          ([maybe-reader (code:line) (code:line #:reader reader-mod)])]{

Creates a new empty namespace, populates it by requiring
@racket[lang-mod] (a module path), and installs it as the kernel's
current namespace, used for evaluation. The namespace shares the
module instances of the kernel's original namespace, but it does not
include previous top-level definitions.

If @racket[reader-mod] is given, the kernel's reader is set to the
@racketidfont{read-syntax} export of @racket[reader-mod] (a module
path); otherwise the kernel's reader is set to Racket's
@racket[read-syntax].

Warning: If @racket[reader-mod] is given, its @racketidfont{read-syntax} export
must be suitable for reading top-level forms. For example,
@racketmodname[scribble/reader] is suitable, but
@racketmodname[at-exp/lang/reader] is not suitable, because it provides a
@emph{whole-module meta-reader}.
}

If a cell contains @litchar{#lang iracket/lang}, it must be the first
thing in the cell; no other forms, comments, or even whitespace can
appear before it. The declaration does not have to appear in the first
cell in the notebook; in fact, multiple cells may contain language
declarations. The declaration takes effect when it is evaluated, and
it affects the rest of the current cell and subsequent evaluations,
until the kernel is restarted or until the next @litchar{#lang
iracket/lang} declaration is evaluated.

For example, the following declaration sets the initial environment to
the exports of @racketmodname[racket/base] and sets the reader to
Scribble's reader:
@racketblock[
#, @tt{#lang iracket/lang #:require racket/base #:reader scribble/reader}
]

@history[#:added "1.2"]


@subsection[#:tag "hopeless"]{The Top Level (REPL) Is Hopeless}

@(require (for-label (only-in racket/list [range r:range])))
@(define (r:range) @racketlink[r:range]{@racketfont{range}})

Due to the combination of Racket's macro system, its recursive
top-level environment, and the fact that the REPL receives and
processes forms one at a time, the Racket REPL occasionally produces
unexpected behavior. These problems are known in the Racket community
as ``the top level is hopeless''. The same problems occur in Jupyter
notebooks.

For example, consider the following program:

@racketblock[
(code:comment "range : Real Real -> Real")
(code:comment "Compute the size of the given interval.")
(define (range x y)
  (cond [(<= x y) (- y x)]
        [else (code:comment "swap to normalize, try again")
         (range y x)]))
(range 5 10)
(range 10 5)
]

When placed inside a @racket[#, @(hash-lang) #, @racketmodname[racket]] module,
this code prints @racketresult[5] twice. But when run at the REPL (or in a
@racketmodname[racket/load] module), the same code prints @racketresult[5] and
then @racketresult['(5 6 7 8 9)]!

The problem is that when the REPL receives the definition of
@racketidfont{range}, it compiles the definition in an environment where
@racketidfont{range} is still bound to @(r:range) from @racketmodname[racket]
(which re-provides the exports of @racketmodname[racket/list]). So the
``recursive'' call to @racketidfont{range} gets compiled as a call to
@racketmodname[racket/list]'s @(r:range), not to the top-level
@racketidfont{range} function that has not yet been defined.

Racket's module system mainly avoids such problems by detecting defined names
before expanding the right-hand sides of definitions.

To avoid this problem, avoid redefining names.

(An alternative is to put @racket[(define-syntaxes (#, @racketidfont{range})
(values))] before the definition above. This form of @racket[define-syntaxes] is
only allowed at the top level, and it changes @racketidfont{range} to resolve as
a binding in the top-level environment without giving it a value.)


@subsection[#:tag "iracket-lang"]{@racket[#, @(hash-lang) #, @racketmodname[iracket/lang]]}

@defmodulelang[iracket/lang]

In addition to being interpreted specially by the IRacket kernel (see
@secref["lang"]), @racketmodname[iracket/lang] can be used as a language in
Racket code. The purpose of the language is for testing how code should work in
a Jupyter notebook; the @racketmodname[iracket/lang] language is not useful for
developing normal Racket libraries and programs.

As a language, it behaves similarly to @racketmodname[racket/load]. Like
@racketmodname[racket/load], it evaluates body forms one by one in a top-level
namespace. Unlike @racketmodname[racket/load], it allows controlling the reader,
and it delays reading the module body until run time, so that if one expression
dynamically changes the reader, it affects the reading of the rest of the module
body.

The following inconsistencies between Racket's interpretation of the
@racketmodname[iracket/lang] language and IRacket's interpretation of
a @litchar{#lang iracket/lang} declaration are known:
@itemlist[

@item{IRacket allows multiple @litchar{#lang iracket/lang} declarations in a
notebook, but a Racket module does not (unless you change reader parameters, but
then it still means something different).}

@item{Notebook cell boundaries can affect reader behavior, because the reader
stops at the end of each cell. Thus a module formed by simply concatenating cell
contents might behave differently.}

@item{When IRacket processes a @litchar{#lang iracket/lang} declaration, it does
not reset parameters like @racket[current-readtable], @racket[read-accept-dot],
etc. In contrast, Racket generally uses a fixed set of parameter values for
reading modules (see @racketmodname[syntax/modread]).}

]

@history[#:added "1.2"]
