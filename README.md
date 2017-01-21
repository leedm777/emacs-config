# Dave's [Emacs][] Setup

This is probably the third time I've decided to restart my Emacs setup from
scratch after something went horribly wrong. I guess it's time to keep it in
source control.

Some of my config I picked up from [Clojure for the Brave and True][brave]. You
can check that out at https://github.com/flyingmachine/emacs-for-clojure.

## Other setup notes

I primarily use macOS, with Emacs installed via [`homebrew cask`][cask].

I use [a launchd script][launchd] to keep an [Emacs server][] running in the
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
 [cask]: https://caskroom.github.io/
 [emacs]: https://www.gnu.org/software/emacs/
 [emacs server]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
 [launchd]: https://www.emacswiki.org/emacs/EmacsAsDaemon#toc7
