---@diagnostic disable: undefined-global

-- Set <space> as the leader key
vim.g.mapleader = ' '

-- [[ Setting options ]]

-- Make line numbers default
vim.opt.number = true
-- Also show them relative
vim.opt.relativenumber = true

-- Enable mouse mode, can be useful for resizing splits for example!
vim.opt.mouse = 'a'

-- Sync clipboard between OS and Neovim.
vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)

-- Save undo history
vim.opt.undofile = true

-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = 'yes'

-- Keep current content top + left when splitting
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 2

-- Show a column at 80 characters as a guide for long lines
vim.opt.colorcolumn = '80'
--- except in Rust where the rule is 100 characters
vim.api.nvim_create_autocmd('Filetype', { pattern = 'rust', command = 'set colorcolumn=100' })

-- Sets how neovim will display certain whitespace characters in the editor.
vim.opt.list = true
vim.opt.listchars = { tab = '^ ', nbsp = '¬', extends = '»', precedes = '«', trail = '•' }

-- [[ Basic Keymaps ]]

-- Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- [[ Install `lazy.nvim` plugin manager ]]
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
require("lazy").setup({
  spec = {
    'tpope/vim-sleuth',
    'tpope/vim-fugitive',
    {
      "gbprod/nord.nvim",
      lazy = false,
      priority = 1000,
      config = function()
        require("nord").setup({})
        vim.cmd.colorscheme 'nord'
      end
    },
    {
      "nvim-treesitter/nvim-treesitter",
      build = ":TSUpdate",
      config = function()
        local configs = require("nvim-treesitter.configs")

        configs.setup({
          ensure_installed = { "c", "diff", "go", "gomod", "haskell", "lua", "luadoc", "markdown", "markdown_inline", "query", "rust", "vim", "vimdoc" },
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        })
      end
    },
  },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "nord" } },
  -- automatically check for plugin updates
  checker = { enabled = true },
})
