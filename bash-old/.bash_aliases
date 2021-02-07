# -*- mode: shell-script; -*-
alias ls='/bin/ls --color=always'
alias lsnoext='/bin/ls | sed -e '"'"'s/\.[a-zA-Z]*$//'"'"
alias ll='/bin/ls --color=always -lh'
alias lt='/bin/ls --color=always -rtlh'
alias la='/bin/ls --color=always -lah'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

alias e='sensible-editor'

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# copy output of last command to clipboard
alias cl="fc -e - | pbcopy"

# copy the working directory path
alias cpwd='pwd|tr -d "\n" | pbcopy'

fail_icon="/usr/share/icons/HighContrast/48x48/stock/gtk-no.png"
finish_icon="/usr/share/icons/HighContrast/48x48/stock/gtk-ok.png"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send -i "$([ $? = 0 ] && echo $finish_icon || echo $fail_icon)" "Finished" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

#####################
## Useful functions
#####################

######
# unpacking files
unpack () {
    if [ -z "$1" ]; then
        echo "Usage: unpack <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|tar.bz2|tar.gz|tar.xz>"
    else
        if [ -f $1 ]; then
            case $1 in
                *.tar.bz2)   tar xjf $1 ;;
                *.tar.gz)    tar xzf $1 ;;
                *.tar.xz)    tar xvJf $1 ;;
                *.bz2)       bunzip2 $1 ;;
                *.rar)       unrar x $1 ;;
                *.gz)        gunzip $1 ;;
                *.tar)       tar xf $1 ;;
                *.tbz2)      tar xjf $1 ;;
                *.tbz)       tar -xjvf $1 ;;
                *.tgz)       tar xzf $1 ;;
                *.zip)       unzip $1 ;;
                *.Z)         uncompress $1 ;;
                *.7z)        7z x $1 ;;
                *.xz)        unxz $1 ;;
                *)           echo "I don't know how to extract '$1'..." ;;
            esac
        else
            echo "$1 - file does not exist"
        fi
    fi
}

_unpack() {
    local cur prev opts fls
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    fls=$(for x in `find . -maxdepth 1 -name "*.bz2" -o \
                                       -name "*.rar" -o \
                                       -name "*.gz" -o \
                                       -name "*.tar" -o \
                                       -name "*.tbz2" -o \
                                       -name "*.tbz" -o \
                                       -name "*.tgz" -o \
                                       -name "*.zip" -o \
                                       -name "*.Z" -o \
                                       -name "*.7z" -o \
                                       -name "*.xz"`; do
              echo ${x##*/}
          done)
    COMPREPLY=( $(compgen -W "$fls" -- ${cur}) )
    return 0
}
complete -F _unpack unpack

######
# packing files
pack () {
    if [ "$1" ] && [ "$2" ]; then
        case $1 in
            tbz)   tar cjvf $2.tar.bz2 $2 ;;
            tgz)   tar czvf $2.tar.gz  $2 ;;
            tar)   tar cpvf $2.tar  $2 ;;
            bz2)   bzip2 $2 ;;
            gz)    gzip -c -9 -n $2 > $2.gz ;;
            zip)   zip -r $2.zip $2 ;;
            7z)    7z a $2.7z $2 ;;
            *)     echo "'$1' cannot be packed via pack()" ;;
        esac
    else
        echo "Usage: pack <tbz|tgz|tar|bz2|gz|zip|7z> <path>"
    fi

}   
_pack() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="tbz tgz tar bz2 gz zip 7z"
    
    case "${prev}" in
        tbz|tgz|tar|bz2|gz|zip|7z)
            COMPREPLY=( $(compgen -f ${cur}) )
            return 0 ;;
    esac
    
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}
complete -F _pack pack
