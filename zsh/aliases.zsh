alias sz='source ~/.zshrc'

alias asdf='set-dvorak'
alias aoeu='set-us'

set-dvorak() {
    setxkbmap dvorak
    setxkbmap -option ctrl:swapcaps
}

set-us() {
    setxkbmap us
}