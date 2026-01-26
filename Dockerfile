FROM eddelbuettel/r2u:24.04
LABEL org.opencontainers.image.authors="mark.padgham@email.com"

ARG GITHUB_TOKEN
ARG GIT_USER_NAME
ARG GIT_USER_EMAIL
ARG GIT_REMOTE_URL
ARG TITLE
ARG ORGMETRICS_PERIOD

RUN apt-get update && apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes

RUN apt-get update -qq && apt-get install -y \
    curl \
    g++ \
    gcc \
    git \
    global \
    libcurl4 \
    libssh-dev && \
    apt-get clean

# ctags install
RUN git clone https://github.com/universal-ctags/ctags.git \
    && cd ctags \
    && ./autogen.sh \
    && ./configure --prefix=/usr \
    && make \
    && make install

RUN install2.r \
  rsconnect \
  quarto \
  visNetwork

RUN --mount=type=secret,id=GITHUB_PAT,env=GITHUB_PAT installGithub.r \
  ropensci-review-tools/orgmetrics

RUN echo "GITHUB_TOKEN='${GITHUB_TOKEN}'" > ~/.Renviron \
    && echo "GITHUB_PAT='${GITHUB_TOKEN}'" >> ~/.Renviron \
    && git config --global user.name "${GIT_USER_NAME}" \
    && git config --global user.email "${GIT_USER_EMAIL}" \
    && git clone "${GIT_REMOTE_URL}" repo

# WORKDIR repo

# CMD ["sh", "-c", "Rscript -e 'orgmetrics::orgmetrics_deploy_r_univ()' && cd quarto && quarto publish gh-pages && cd .."]
