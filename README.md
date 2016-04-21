> [Helm](https://github.com/emacs-helm/helm) interface to [CIDER](https://github.com/clojure-emacs/cider).

## Installation

Not yet released as a package.

## Setup

Load the file, then see that `cider-apropos` and `cider-apropos-select` are have been remapped to `helm-cider-apropos`.

## Features

### General

- View all completion candidates at once.
- Match against the entire candidate, not just the start.

### Apropos

- Group symbols together by namespace.
- Search for symbols across namespaces.
- Quick actions to view documentation, find location in source code, etc.

## See Also

- CIDER issue [#1541](https://github.com/clojure-emacs/cider/issues/1541), regarding [Ido](https://www.gnu.org/software/emacs/manual/html_mono/ido.html)/Helm support for `apropos`.
- CIDER issue [#1059](https://github.com/clojure-emacs/cider/issues/1059), regarding seeing more completion candidates and grouping them by namespace.
- [helm-clojure](https://github.com/prepor/helm-clojure), an earlier project with similar ideas.
