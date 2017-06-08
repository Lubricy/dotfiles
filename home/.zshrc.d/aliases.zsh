alias ls="ls -G -F"
foo_cd () {
  cd "$@";
  ls
}
alias cd=foo_cd
alias vim=nvim
alias vi=nvim

