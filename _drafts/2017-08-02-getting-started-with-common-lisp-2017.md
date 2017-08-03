---
title: Getting started with Common Lisp in 2017
date: Wed 02 Aug 2017 06:03:00 PM HST
---

*"Because all the things were invented before we were born."*
As unimaginable as it may sound, Google has not yet "organized the
world's information" about Common Lisp. The best tutorials are hard
to find. Here's how I got started.

## Install roswell ##

[Roswell](https://github.com/roswell/roswell) is a tool that helps
you download various implementations of Common Lisp and switch
between them. It installs basically anywhere, including Windows.

(You don't have to use Roswell if you prefer to compile your
compilers from source. If you do, be prepared to read manuals
written a few decades ago. They work, but they require a high degree
of intellectual fortitude.)

There are a couple of simple ways
to
[install Roswell.](https://github.com/roswell/roswell/wiki/1.-Installation)

``` shell
$ brew install roswell
```

``` shell
$ yaourt -S roswell
```

Or a `.zip` file for Windows.

### Linux or Windows ###

If you're on Windows, you know it's come a long way in the last 10
years. Microsoft is even shipping Ubuntu Bash on Windows. But if you
plan to do serious development, do yourself a favor and
install [VirtualBox](https://www.virtualbox.org/) and a Linux
distribution of your choice in a virtual machine.

If you're not a Linux fan, it's fine to stick with Windows. There's
even a good choice of
editor, [Atom](#setting-up-atom-for-slime). That is, if you don't
want to use emacs. (See [Editors.](#editors)

## Install Common Lisp ##

Once Roswell is installed, do this: 

``` shell
$ ros install sbcl-bin
```

This installs the binary distribution of the latest version of [Steel
Bank Common Lisp,](http://www.sbcl.org/) the version that the cool
kids seem to be using these days.

Then, you can do things like

``` shell
$ ros list installed
$ ros use sbcl-bin
```

And finally, to launch into the world-famous, often-imitated but
never-surpassed, for (and from) the ages, read-eval-print-loop, also
known as, the one and only REPL:

``` shell
$ ros run
* 
```

It's kind of anticlimactic, isn't it? For some reason, Roswell
suppresses the banner. So, if you want:

``` common-lisp
* (lisp-implementation-type)

"SBCL"
* (lisp-implementation-version)

"1.3.19"
* (exit)
$ 
```

But nobody uses the REPL from the command line like this. You want
to use an editor with support for an integrated
REPL (notice I didn't say `emacs`). See [Editors.](#editors)

### Why SBCL? ###

There are lots of Common Lisp "implementations" to choose from, some
free and some commercial, so why choose SBCL? Because it has
a [github repository](https://github.com/sbcl/sbcl) (which is a
mirror, but at least the source is there); it has a monthly release
schedule, which means it's being actively improved.

One of the advantages cited by Common Lisp advocates is that there
are many compatible implementations of the language standard, which
means you can write code and try it out on different compilers. This
is true, but it's incredibly confusing for newcomers. Just start
with SBCL, see if you can get productive, and then shop around for
alternatives.

Another actively developed branch is ECL, which is designed to more
closely integrate with C libraries (which SBCL and most
implementations also support through `CFFI`) and even allows the
direct inclusion of C code in Lisp functions (which SBCL does
not). It runs on Android devices. Very exciting prospect, but as of
this writing it's finicky and certainly not the way to get started
in Common Lisp.


## Editors ##

The joke is that everybody wants to try Common Lisp until they hear
you have to install `emacs` in step one. Well, you don't.

The [Atom](https://atom.io/) editor is a good choice and even
supplies a [plugin](https://atom.io/packages/atom-slime) to get you
closer to the emacs-inspired interactive editing nirvana we all
dream about it.

### Setting up Atom for SLIME ###

You do have to jump through a few hoops but it's nothing like
getting a remotely complicated nodejs Javascript project up and
running with linting, testing, bundling, etc.

First, after installing the Atom editor, follow the instructions on
this page: [atom-slime](https://atom.io/packages/atom-slime)

Aside from installing the Atom packages `atom-slime`,
`language-lisp`, and `lisp-paredit`, you need to get a copy of the
`SLIME` code that `emacs` uses for its magic.

Then, you need to set the `atom-slime` settings to tell it how to
launch SBCL, and where to find the SLIME you downloaded.

To repeat, since this is unusual, you need to `git clone` the SLIME
repo and then tell `atom-slime` where to find it.

Since we're using Roswell, the "Lisp Process (Name of Lisp to run)"
setting should simply be `ros`. The `atom-slime` developers have
helpfully integrated knowledge of Roswell into their system, so
there's no need to do the usual `ros -Q run`, which is what `emacs`
users have to do.

The "SLIME Path" setting should be the path to wherever you
installed SLIME. (You did install it, right?)

After that, select Slime -> Connect from the Packages menu, wait a
few seconds for the initial compilation of SLIME to complete, and
you'll see a REPL pop up.

(When I did this, it didn't work the first time. I exited Atom,
checked my settings, and tried again, and then it worked.)

### Setting up Emacs for SLIME ###

If you're an `emacs` user, you know you live for the
frustration-followed-by-breakthrough-dopamine-rush of combing
through decades-old wikis or `info` pages to figure out how to set
yourself up, right?

Or, just install [Prelude](https://github.com/bbatsov/prelude) and
customize it from there. Prelude gives a great out-of-the-box
experience for new emacs installations. I always start there.

For Lisp, here is my complete `personal.el` file. You don't need all
of it, but note that 1) it's not long and 2) The SLIME-specific
settings are at the bottom. The main thing is you want to set
`slime-lisp-implementations` and apparently also the
unfortunately-named `inferior-lisp-program`. 

``` emacs-lisp
(prefer-coding-system 'utf-8-unix)

(with-eval-after-load "prelude-custom"
  (setq prelude-whitespace nil)
  (setq prelude-flyspell nil))

(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-[") 'ace-window)

(with-eval-after-load "slime"
  (setq inferior-lisp-program "ros -Q run")
  (put 'define-package 'common-lisp-indent-function '(4 2))
  (slime-setup '(slime-fancy slime-banner slime-repl-ansi-color))
  
  (setf slime-lisp-implementations
        `((roswell ("ros" "-Q" "run"))))
  (setf slime-default-lisp 'roswell))

```


## Getting libraries ##

You want to `npm install` but since Common Lisp standardized before
`npm` was even conceived, it's not quite that simple. There are two
tools you need to know about to get libraries: `quicklisp` and
`asdf.` Don't google them yet, you've already got them, since `asdf`
is bundled with SBCL, and `quicklisp` is bundled with Roswell.

The first thing to know about these tools is that they aren't
command-line tools. They are programs that run inside your
REPL. Apparently there was a time when people thought Common Lisp
would become an operating system and command-line shells would be
made obsolete. See, history *is* fascinating. (There are people
still working on this idea, by the way. Go check them out, it's
wild.)

### Quicklisp ###

Quicklisp is like clojars or the npm registry. It's a distribution
site for vetted libraries. One of its major drawbacks is that it's
only updated monthly, but in some sense it's an advantage, because
the libraries it includes have generally been stable for years.

So when you want to download a library from the web, you first fire
up your REPL, then do this:

``` common-lisp
CL-USER> (ql:quickload :alexandria)
```

to load the most-used, practically
mandatory [Alexandria](http://quickdocs.org/alexandria/) library.

Or, to load `retro`:

``` common-lisp
CL-USER> (ql:quickload :anticrisis.retro)
```

(By the way, if this doesn't work for `retro`, it's because it
hasn't been added to Quicklisp yet. One of the drawbacks of
Quicklisp is that you can't refer to git versions of libraries.)

Quickload will take care of downloading your libraries, but it won't
load them into your REPL. For that, you need ASDF.

### ASDF ###

ASDF loads code from your local disk into a running Lisp image. It's
also the way you configure your libraries for sharing with other
projects.

The terminology used in Common Lisp was invented in the 18th
century, so bear with me here.

A `package` is what we would call a `module` or a `namespace`
today. Basically, you should keep to one file per package, though
that isn't enforced by the Common Lisp standard.

A `system` is what we would call a `library` or an `application`
today. It's a collection of source files that represent packages,
plus additional things like test files, documentation, etc.

`Systems` are defined in `.asd` files using the `defsystem` form
provided by ASDF. Like most things, including Clojure, Node, etc,
getting this file right is a bit of a pain in the neck until you
learn it.

#### Version numbers ####

Apparently the Common Lisp community doesn't like to use or depend
on library version numbers. Now, I happen to know for a fact that
the whole idea of version numbering existed before Common Lisp was
standardized, so I'm not sure what the excuse here is. I suppose it
encourages library writers to always maintain backward
compatibility, which is a great thing. But it is confusing for
people coming from other library ecosystems.

ASDF does support declaring dependencies on external libraries (aka
"systems") with specific versions. But no one seems to use that, and
it isn't obviously clear how that would work with Quicklisp.

As far as I know, there is no tool like `npm` that can fetch a
specific version of a library from the Internets. Seems odd, no?


## Directories ##

Ok, so you've installed Roswell, you've learned about Quicklisp and
ASDF, and your home directory now has a few new `.` directories.

The most important thing to know right now is how to get ASDF to
find your source code when you want to load it up.

Firstly, if you've used `ql:quickload` to get it, there's nothing
else you have to do. ASDF and Quicklisp work well together.

This is mainly about code you've written yourself, or code which
isn't available in Quicklisp.

If you don't really care where you put your code, just put it in
directories under `~/common-lisp/` in your home directory, and ASDF
will find it. When ASDF starts up, it makes a catalog of every
`.asd` file it can find under that home directory. (This means, by
the way, that if you add new `.asd` files or systems, you need to
reset ASDF. The simplest way to do that is to restart your REPL, but
there are also ASDF commands. Unfortunately, I always forget which
command is which, and restarting the REPL takes about a half-second,
so that's what I do.)

(By the way, if you're on Windows, your home directory might be in
`\Users\you\AppData\Roaming\` or `\Users\you`. I use Windows and I still
can't figure out why sometimes it's one or the other.)

If you're more particular about your directories, you may need to
make this file and put it in your home directory. (This, I haven't
tested on Windows.)

`~/.config/common-lisp/source-registry.conf`

``` common-lisp
(:source-registry
  (:tree (:home "dev/common-lisp"))
  :inherit-configuration)
```

This file tells ASDF to look for code under my `~/dev/common-lisp`
directory, because I like to pretend there are non-dev things I
might use my computer for.

### Roswell and Quicklisp directories ###

The `.roswell` directory has a pretty complex organizational system,
because it needs to manage multiple Common Lisp implementations as
well as Quicklisp. So you might have to hunt around in there. 

Some guides tell you to use `local-projects` in the Quicklisp
directory. If you put symlinks to your projects in that directory,
Quicklisp will "load" them from there instead of the Internet. I
don't use that, but apparently there use to be something called
"symlink farming" which apparently was quite popular.

## References

- For other great beginner resources, see the sidebar
on [/r/lisp](https://reddit.com/r/lisp).

- The definitive (well, almost) ANSI Common Lisp spec is available
  online at something called
  the
  [HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm). It
  may make your eyes bleed because it was typeset in the 90s and has
  an overly restrictive license that prohibits anyone else from
  improving it. There is no better resource online, unfortunately, and
  once you get used to every other word being an underlined hyperlink
  (wow, if you use your pointing device and click the word, it takes
  you to another page!), it can be quite helpful to understand the
  finer points of Common Lisp.
  - The best way to use the HyperSpec is to use Google to search
    for, for example, `CLHS remove-if-not`

## Table of contents
* TOC
{:toc}

