# .dotfiles

My personal dotfiles for macOS and VoidLinux

The configuration files are managed with [GNU Stow].
Each top-level directory represents a "group" of configs, and you can
"install" (by symlinking) the configs of a group using

```console
$ stow -v <group>
```

You can use `-n` to just show what it _would_ install.

To `unstow` you can use

```console
$ stow -D <group>
```

[GNU Stow]: https://www.gnu.org/software/stow/
