# all-the-functions

- ### Get GHC and Haskell-stack

  - Brew
    - `brew install ghc haskell-stack`
    - `brew install gobject-introspection gtk+ gtk+3`

- ### Clone and Build
  - Set up stack directory:
    - add `PATH=$PATH:~/.local/bin` to your .bash_profile, .bashrc, .zshrc, or whatever shell you use

  - `git clone https://github.com/masnax/all-the-functions.git`
  - `cd all-the-functions`
  - make sure PATH includes ~/.local/bin, else `export PATH=$PATH:~/.local/bin`
  - `stack install`
  - `wait for a really long time because Haskell sucks`

- ### RUN!
  - make sure PATH includes ~/.local/bin, else `export PATH=$PATH:~/.local/bin`
  - `cd all-the-functions`
  - `stack exec Picksell`
