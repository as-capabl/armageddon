# armageddon
GUI mastodon client written in Haskell

## How to build
Build requirements are below.

- External library (via pkg-config)
    - gtk+-2.0
    - webkit-1.0
    - sqlite3
- Build tool
    - [stack](https://docs.haskellstack.org/en/stable/README/)
    - gtk2hs-buildtools -- run `stack install gtk2hs-buildtools` command to install the program in your local stack environment.

Install requirements and execute `stack build`.

Note that the gtk2hs build chain has trouble with nonstandard locale.

If you get an error while building gtk, `LANG=C stack build` will help you.

## Current status
Experimental.

- [X] Authentication to arbitrary host
- [X] Save credentials
- [X] Fetch notification
- [ ] Show user profile
- [X] Show icons
- [X] Cache timelines
- [X] Control timeline flow
    - [X] Show "read more"
    - [X] Stop fetching toots unless the scroll position is on top
- [ ] Reply
- [ ] Post media
- [ ] Search
- [ ] Secure authentication
- and more ...

