---
title: Questions for 2017 Common Lisp experts
---

After a few years of hacking with Clojure, and before that Erlang,
Python, and JavaScript, I decided to dig into Common Lisp. This was
a few months ago, and I have some questions for the Common Lisp
experts out there in 2017.

When I've tried to improve products I've worked on, I've found it
helpful to hear from relative newcomers despite the fact they don't
seem to understand how things are supposed to be done. I'm hoping
this list of questions might serve a similar purpose for members of
the Common Lisp community.

#### Libraries
* Why don't libraries have version numbers?
* Why is adding libraries to Quicklisp a manual process that
  requires human review? Is there an alternative non-curated
  registry?
* Why aren't ASDF dependencies automatically fetched if missing?
* Why do so few libraries use [GitHub](https://github.com/trending/common-lisp)?

#### Open source
* Are there serious open source efforts to modernize documentation?
* Are there serious open source efforts to modernize libraries?
* Are there efforts to encourage open source contributions?
* My apologies, but I'd never even heard of GitLab prior to seeing
  all these libraries hosted on common-lisp.net. Why not GitHub?

#### Compilers
* Why hasn't the open source community standardized on a single
  implementation, i.e. SBCL?
* Would there be any benefit to extending SBCL beyond the Common
  Lisp standard to better support modern language features and
  paradigms?

#### Functional programming 
* Why aren't there more libraries using a functional, immutable
  paradigm? Most seem to use many dynamic variables and large
  functions spanning multiple screens, with variables mutating throughout
* Are there any functional programming advocates in the Common Lisp
  open source community?

#### Innovation
* Are there any efforts to bring innovative libraries from the
  Clojure (or elsewhere) community into Common Lisp?
* Is there software design innovation occurring in the Common Lisp
  community or is the community primarily focused on maintenance and
  tuning?
* Is it worth the time and energy to develop new libraries to
  modernize Common Lisp -- or perhaps a single extended
  implementation of it, like SBCL -- or would that time and energy
  be more productively spent in a non-Common Lisp community?

#### On-boarding 
* Why is on-boarding new developers so difficult? For instance, why
  isn't Roswell linked to from every About Common Lisp page?
* Why hasn't the Common Lisp Foundation updated the common-lisp.net
  home page since 2015?

#### Community
* Why are there so few instructional or advocacy bloggers?
* Why can't Google search find many good resources? I.e. a search
  for “Common Lisp” is not that helpful.

#### Quick advice for newcomers 
* See [/r/lisp](https://www.reddit.com/r/lisp/) sidebar for good learning resources
* Use [Roswell](https://github.com/roswell/roswell) to install and switch between implementations of Common Lisp, like SBCL and CCL
* [Emacs Slime](https://github.com/slime/slime) or [Atom Slime](https://atom.io/packages/atom-slime), take your pick
* See [Quickdocs](http://quickdocs.org/) for popular curated libraries
* ALL CAPS doesn't mean the compiler or documentation is mad at you, it's just a bit hard of hearing after all these years

Comments are on [Reddit](https://www.reddit.com/r/lisp/comments/6snw5d/questions_for_2017_common_lisp_experts/)
