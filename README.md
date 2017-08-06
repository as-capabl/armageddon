# armageddon
GUI mastodon client written in Haskell

## How to build
Build requirements are below.

- External library
    -libgtk
- Build tool
    - [stack](https://docs.haskellstack.org/en/stable/README/)
    - gtk2hs-buildtool -- run `stack install gtk2hs-buildtool` command to install the program in your local stack environment.

Install requirements and execute `stack build`.

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

