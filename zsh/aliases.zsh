alias ag="ag $* --pager 'less -R'"

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

alias c='clear'
alias t='touch'

alias asdf='set-dvorak'
alias aoeu='set-us'

set-dvorak() {
    setxkbmap dvorak
    setxkbmap -option ctrl:swapcaps
}

set-us() {
    setxkbmap us
}