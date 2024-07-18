HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

fpath+="$HOME/.myzsh/zsh-completions/src"

autoload -Uz compinit
compinit

source $HOME/.myzsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.myzsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

eval "$(oh-my-posh init zsh --config $HOME/.config/oh-my-posh/config.json)"
