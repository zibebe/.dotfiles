function fish_prompt
    set_color bryellow
    echo -n (hostname -s)
    if [ $PWD != $HOME ]
        set_color brblack
        echo -n ':'
        set_color brblue
        echo -n (basename $PWD)
    end
    set_color brmagenta
    printf '%s ' (__fish_git_prompt)
    set_color brred
    echo -n '| '
    set_color normal
end
