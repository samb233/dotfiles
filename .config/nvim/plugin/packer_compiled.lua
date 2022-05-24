-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/jiesamb/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/jiesamb/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/jiesamb/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/jiesamb/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/jiesamb/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Comment.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/Comment.nvim",
    url = "https://hub.fastgit.xyz/numToStr/Comment.nvim"
  },
  ["FTerm.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/FTerm.nvim",
    url = "https://hub.fastgit.xyz/numToStr/FTerm.nvim"
  },
  ["bufferline.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/bufferline.nvim",
    url = "https://hub.fastgit.xyz/akinsho/bufferline.nvim"
  },
  ["candle-grey"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/candle-grey",
    url = "https://hub.fastgit.xyz/aditya-azad/candle-grey"
  },
  catppuccin = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/catppuccin",
    url = "https://hub.fastgit.xyz/catppuccin/nvim"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/cmp-buffer",
    url = "https://hub.fastgit.xyz/hrsh7th/cmp-buffer"
  },
  ["cmp-cmdline"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/cmp-cmdline",
    url = "https://hub.fastgit.xyz/hrsh7th/cmp-cmdline"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp",
    url = "https://hub.fastgit.xyz/hrsh7th/cmp-nvim-lsp"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/cmp-path",
    url = "https://hub.fastgit.xyz/hrsh7th/cmp-path"
  },
  ["cmp-vsnip"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/cmp-vsnip",
    url = "https://hub.fastgit.xyz/hrsh7th/cmp-vsnip"
  },
  ["friendly-snippets"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/friendly-snippets",
    url = "https://hub.fastgit.xyz/rafamadriz/friendly-snippets"
  },
  ["indent-blankline.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    url = "https://hub.fastgit.xyz/lukas-reineke/indent-blankline.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/lspkind-nvim",
    url = "https://hub.fastgit.xyz/onsails/lspkind-nvim"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/lspsaga.nvim",
    url = "https://hub.fastgit.xyz/tami5/lspsaga.nvim"
  },
  ["lualine-lsp-progress"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/lualine-lsp-progress",
    url = "https://hub.fastgit.xyz/arkav/lualine-lsp-progress"
  },
  ["lualine.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/lualine.nvim",
    url = "https://hub.fastgit.xyz/nvim-lualine/lualine.nvim"
  },
  ["neovim-ayu"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/neovim-ayu",
    url = "https://hub.fastgit.xyz/Shatur/neovim-ayu"
  },
  ["null-ls.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/null-ls.nvim",
    url = "https://hub.fastgit.xyz/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://hub.fastgit.xyz/windwp/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-cmp",
    url = "https://hub.fastgit.xyz/hrsh7th/nvim-cmp"
  },
  ["nvim-lsp-installer"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-lsp-installer",
    url = "https://hub.fastgit.xyz/williamboman/nvim-lsp-installer"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://hub.fastgit.xyz/neovim/nvim-lspconfig"
  },
  ["nvim-tree.lua"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-tree.lua",
    url = "https://hub.fastgit.xyz/kyazdani42/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://hub.fastgit.xyz/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://hub.fastgit.xyz/kyazdani42/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://hub.fastgit.xyz/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://hub.fastgit.xyz/nvim-lua/plenary.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://hub.fastgit.xyz/nvim-telescope/telescope.nvim"
  },
  ["tokyodark.nvim"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/tokyodark.nvim",
    url = "https://hub.fastgit.xyz/tiagovla/tokyodark.nvim"
  },
  ["vim-bbye"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/vim-bbye",
    url = "https://hub.fastgit.xyz/moll/vim-bbye"
  },
  ["vim-vsnip"] = {
    loaded = true,
    path = "/home/jiesamb/.local/share/nvim/site/pack/packer/start/vim-vsnip",
    url = "https://hub.fastgit.xyz/hrsh7th/vim-vsnip"
  }
}

time([[Defining packer_plugins]], false)
if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
