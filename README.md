# Emacs as a command line tool

## emacsc(1)

Emacsc(1) is a wrapper of emacsclient(1) for use within a terminal,
which helps use Emacs more as command line tool than just a standalone
environment.

    usage: emacsc [-cdgkn] [-s NAME] [-e EXPR | -x EXPR | -f FUNC] [-C DIR] [FILE..]

        -h, --help      show this help
        -d, --daemon    run Emacs as daemon and quit
        -k, --kill      kill Emacs daemon
        -g, --no-tty    do not prefer tty
        -c, --create-frame
                        create a new frame
        -n, --no-wait
                        do not wait and return immediately
        -s, --socket-name=NAME
                        specify the file name of the socket file name for
                        communication
        -e, --eval=EXPR
                        evaluate the Lisp expression EXPR and print the
                        result without a frame opened
        -x, --execute=EXPR
                        interactively execute the Lisp expression EXPR
        -f, --funcall=FUNC
                        interactively call the Lisp function FUNC
        -C, --chdir, --directory=DIR
                        change to directory DIR before running code

This command is a wrapper of emacsclient for use within a terminal.
It adds the -t option so that Emacs opens a new frame on the current
terminal, making the command itself suitable as a value for EDITOR.

A byte-compiled initialization file is automatically removed before
running Emacs if outdated, i.e. older than the original file.

In order for the -x and -f options, and the following commands to
work, install lisp/emacsc.el into a directory in your load-path and
add this to your ~/.emacs.d/init.el:

    (require 'emacsc)

Or install emacsc from an ELPA package and you are good to go.

## dired(1)

Dired(1) is a frontend command to invoke dired.

    usage: dired [-n] [directory|file]

        -n, --no-wait   do not wait and return immediately

It takes a directory or a file name, defaulted to the current
directory, to open with dired.  If a non-directory is given, the point
will be automatically moved to the file on startup.

## ediff(1), ediff-merge(1)

Ediff(1) and ediff-merge(1) are frontend commands to invoke ediff
functions.

    usage: ediff file1 file2

    usage: ediff-merge local remote base merged
           ediff-merge local remote merged

To use them from Git, put the following lines in your
~/.config/git/config (or ~/.gitconfig).

    [diff]
        tool = ediff
    [difftool "ediff"]
        cmd = ediff \"$LOCAL\" \"$REMOTE\"
    [merge]
        tool = ediff
    [mergetool "ediff"]
        cmd = ediff-merge \"$LOCAL\" \"$REMOTE\" \"$BASE\" \"$MERGED\"
        trustExitCode = true

## evil(1)

Evil(1) is a command to edit given files in evil-local-mode.

    usage: evil [-s NAME] FILE..

        -h, --help      show this help
        -s, --socket-name=NAME
                        specify the file name of the socket file name for
                        communication

## magit(1)

Magit(1) is a frontend command to invoke magit-status.

    usage: magit [directory]

It runs magit-status on a given directory.  If omitted, ask where with
the current directory as default.

## SEE ALSO

- [e(1) - a smart wrapper for $EDITOR](https://github.com/knu/e)

## AUTHOR

Copyright (c) 2012-2023 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for
details.

Visit [GitHub Repository](https://github.com/knu/emacsc) for the latest
information.
