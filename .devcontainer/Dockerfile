FROM debian:buster

# Install base packages
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential

# Install packages for Gloss graphics library
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends libglu1-mesa-dev freeglut3-dev mesa-common-dev mesa-utils libgl1-mesa-glx

# Install needed packages and setup non-root user
ARG INSTALL_ZSH=true
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
ARG UPGRADE_PACKAGE=false
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true"\
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

# Install GHCup
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup \
    && chmod +x /usr/bin/ghcup \
    && ghcup config set gpg-setting GPGStrict

# install GHC and Cabal
ARG GHC=8.10.7
ARG CABAL=3.8.1.0
USER vscode
RUN ghcup install ghc ${GHC} \
    && ghcup set ghc ${GHC} \
    && ghcup install cabal ${CABAL} \
    && ghcup set cabal ${CABAL}
ENV PATH="${PATH}:/home/vscode/.ghcup/bin"

# Link cabal user directory to be local to the workspace
ARG WORKSPACE=orrey
RUN ln -s /workspaces/${WORKSPACE}/.cabal /home/vscode/.cabal
