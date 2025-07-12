# Dave's [Emacs][] Setup

This is the fourth time I've refreshed my Emacs config. This time I'm leaning
into [use-package][].

Some of my config I picked up from [Clojure for the Brave and True][brave]. You
can check that out at https://github.com/flyingmachine/emacs-for-clojure.

## Other setup notes

I primarily use macOS, with Emacs installed via [homebrew][].

## Server, sometimes

It's recommended to use [Emacs server][] to keep [Emacs][] running in the
background and help avoid long startup times with frequent use. I try it
occasionally, but when I do it's usually more trouble that it's worth.

Also on macOS that server will only launch without the window system, which is
usually not what I want.

Instead I just run `(server-start)` in my `init.el`, and use an alias for
`emacs` that will use that server if running, but launch a new editor if not.

```bash
alias emacs="emacsclient --create-frame --alternate-editor emacs"
```

## License

 [ISC](https://opensource.org/licenses/ISC)
 [brave]: http://www.braveclojure.com/
 [homebrew]: https://formulae.brew.sh/cask/emacs-app#default
 [emacs]: https://www.gnu.org/software/emacs/
 [emacs server]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
 [use-package]: https://github.com/jwiegley/use-package
