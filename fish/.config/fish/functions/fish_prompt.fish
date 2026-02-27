function fish_prompt
    set_color blue
    echo -n (command -q hostname; and hostname; or hostnamectl hostname)
    if [ $PWD != $HOME ]
        set_color brblack
        echo -n ':'
        set_color yellow
        echo -n (basename $PWD)
    end
    set_color green
    printf '%s\n' (__fish_git_prompt)
    set_color red
    echo -n 'λ '
    set_color normal
end
