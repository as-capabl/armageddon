# armageddon
GUI mastodon client written in Haskell

## How to build
- External library: libgtk
- Build tool: stack

Execute `stack build`.

The gtk2hs build chain has trouble with nonstandard locale.

If you get an error while building gtk, `LANG=C stack build` will help you.

## Current status
Experimental.

Only the host http://pawoo.net is supprted.

- [ ] Authentication to arbitrary host
- [ ] Save credentials
- [ ] Fetch notification
- [ ] Show user profile
- [ ] Show icons
- [ ] Toot cache
- and so on ...

