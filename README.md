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

When I do, I use [a launchd script][launchd] to keep an running in the
background, to avoid long startup times. I have an alias to use `emacsclient`
when the server is running, and fall back to plain `emacs` when it's not.

```bash
alias emacs="emacsclient --create-frame --alternate-editor emacs"
```

In order to get the `PATH` right for Emacs server, I run `sudo launchctl config
user path "$PATH"`. This sets the path for all services and all users, which is
usually what I want anyways.

## License

[ISC](https://opensource.org/licenses/ISC)

 [brave]: http://www.braveclojure.com/
 [homebrew]: https://formulae.brew.sh/cask/emacs-app#default
 [emacs]: https://www.gnu.org/software/emacs/
 [emacs server]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
 [launchd]: https://www.emacswiki.org/emacs/EmacsAsDaemon#toc7
 [use-package]: https://github.com/jwiegley/use-package
