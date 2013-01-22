# Emacs as a command line tool

## emacsc(1)

Emacsc(1) is a wrapper of emacsclient(1) for use within a terminal,
which helps use Emacs more as command line tool than just a standalone
environment.

    usage: emacsc [-dk] [-e EXPR | -x EXPR | -f FUNC] [FILE..]

        -h, --help      show this help
        -d, --daemon    run Emacs as daemon and quit
        -k, --kill      kill Emacs daemon
        -e, --eval=EXPR
                        evaluate the Lisp expression EXPR and print the
                        result without a frame opened
        -x, --execute=EXPR
                        interactively execute the Lisp expression EXPR
        -f, --funcall=FUNC
                        interactively call the Lisp function FUNC

This command is a wrapper of emacsclient for use within a terminal.
It adds the -t option so that Emacs opens a new frame on the current
terminal, making the command itself suitable as a value for EDITOR.

A byte-compiled initialization file is automatically removed before
running Emacs if outdated, i.e. older than the original file.

In order for -x and -f to work, install lisp/emacsc.el into a
directory in your load-path and add this to your ~/.emacs.d/init.el.

    (require 'emacsc)

## ediff(1), ediff-merge(1)

Ediff(1) and ediff-merge(1) are frontend commands to invoke ediff
functions.

usage: ediff file1 file2

usage: ediff-merge local remote base merged
       ediff-merge local remote merged

These commands depend on emacsc(1) and ediff-batch.el.  Put the
following line in your Emacs initialization file in addition to the
one above for emacsc.

    (require 'ediff-batch)

To use them from Git, put the following lines in your ~/.gitconfig.

    [diff]
        tool = ediff
    [difftool "ediff"]
        cmd = ediff \"$LOCAL\" \"$REMOTE\"
    [merge]
        tool = ediff
    [mergetool "ediff"]
        cmd = ediff-merge \"$LOCAL\" \"$REMOTE\" \"$BASE\" \"$MERGED\"

## magit(1)

Magit(1) is a frontend command to invoke magit-status.

usage: magit [direcotry]

Run magit-status on a given directory.  If omitted, ask where with the
current directory as default.

This command depends on emacsc(1).  Put the following lines in your
Emacs initialization file.

    (require 'emacsc)

## SEE ALSO

- [e(1) - a smart wrapper for $EDITOR](https://github.com/knu/e)

## AUTHOR

Copyright (c) 2012, 2013 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for
details.

Visit [GitHub Repository](https://github.com/knu/emacsc) for the latest
information.
