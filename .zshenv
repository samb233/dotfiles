export TERM=xterm-256color
export PATH="${HOME}/Env/bin:$PATH"

# go
export G_EXPERIMENTAL=true
export G_HOME="${HOME}/Env/go"
export G_MIRROR=https://mirrors.aliyun.com/golang/

export GOROOT="${HOME}/Env/go/go"
export GOPATH="${HOME}/Env/go/path"
export PATH="${HOME}/Env/go/go/bin:${HOME}/Env/go/path/bin:$PATH"

export GOPROXY="https://goproxy.cn,direct"
export CGO_ENABLED=0

# rust
export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
export RUSTUP_HOME="${HOME}/Env/rust/rustup"
export CARGO_HOME="${HOME}/Env/rust/cargo"
export PATH="${HOME}/Env/rust/cargo/bin:$PATH"
