FROM debian:buster-slim

# Add user
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN groupadd -g $USER_GID $USERNAME && useradd -s /bin/bash -g $USERNAME -u $USER_UID $USERNAME

# Install OpenGL packages for Gloss
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y libglu1-mesa freeglut3 libgl1-mesa-glx \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

COPY dist-newstyle/build/x86_64-linux/ghc-8.10.7/orrey-1.0.0/build/orrey/orrey /usr/local/bin

CMD /usr/local/bin/orrey
