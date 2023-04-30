export TERM=xterm-256color
export GOROOT="${HOME}/.g/go"
export GOPATH="${HOME}/.go"
export PATH="${HOME}/.g/go/bin:${HOME}/.go/bin:$PATH"
export G_MIRROR=https://mirrors.aliyun.com/golang/

export EDITOR=nvim
export VISUAL=nvim


export PATH="${HOME}/.local/bin:$PATH"

# 配置 GOPROXY 环境变量
export GOPROXY=https://goproxy.cn


# rust
. "$HOME/.cargo/env"
# 用于更新 toolchain
export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
# 用于更新 rustup
export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup


