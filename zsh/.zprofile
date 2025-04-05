# general globals
export LANG=en_US.UTF-8
export EDITOR=hx
export NAME='Tobias Tschinkowitz'
export EMAIL=me@zibebe.net
export TZ=Europe/Berlin

# homebrew
eval $(/opt/homebrew/bin/brew shellenv)

# go
export GOPATH="$HOME/Developer/go"
export PATH="$GOPATH/bin:$PATH"

# psql
export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"

# rust
. $HOME/.cargo/env

# fzf settings
export FZF_DEFAULT_COMMAND="fd --type f --follow"
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_ALT_C_COMMAND="fd --type d --follow"
# nord colors
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'
