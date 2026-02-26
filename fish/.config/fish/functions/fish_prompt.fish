function fish_prompt
    set_color brblue
    echo -n (command -q hostname; and hostname; or hostnamectl hostname)
    if [ $PWD != $HOME ]
        set_color brblack
        echo -n ':'
        set_color bryellow
        echo -n (basename $PWD)
    end
    set_color brgreen
    printf '%s\n' (__fish_git_prompt)
    set_color brgreen
    echo 'λ '
end
