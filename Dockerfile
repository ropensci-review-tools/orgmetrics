FROM eddelbuettel/r2u:24.04
LABEL org.opencontainers.image.authors="mark.padgham@email.com"

ARG GITHUB_TOKEN
ARG GIT_USER_NAME
ARG GIT_USER_EMAIL
ARG GIT_REMOTE_URL
ARG TITLE
ARG ORGMETRICS_PERIOD

ENV GITHUB_TOKEN=${GITHUB_TOKEN}
ENV GIT_USER_NAME=${GIT_USER_NAME}
ENV GIT_USER_EMAIL=${GIT_USER_EMAIL}
ENV GIT_REMOTE_URL=${GIT_REMOTE_URL}
ENV TITLE=${TITLE}
ENV ORGMETRICS_PERIOD=${ORGMETRICS_PERIOD}

RUN apt-get update && apt-get install -y git

RUN echo "GITHUB_TOKEN='${GITHUB_TOKEN}'" > ~/.Renviron \
    && echo "GITHUB_PAT='${GITHUB_TOKEN}'" >> ~/.Renviron \
    && git config --global user.name "${GIT_USER_NAME}" \
    && git config --global user.email "${GIT_USER_EMAIL}" \
    && git clone "${GIT_REMOTE_URL}" repo

RUN apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes

RUN apt-get install -y \
    automake \
    curl \
    global \
    libcurl4 \
    libssh-dev \
    libxml2-dev && \
    apt-get clean

# ctags install
RUN git clone https://github.com/universal-ctags/ctags.git \
    && cd ctags \
    && ./autogen.sh \
    && ./configure --prefix=/usr \
    && make \
    && make install \
    && cd ..

# Quarto binary:
RUN QUARTO_VERSION=$(curl -s https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest | grep '"tag_name"' | sed -E 's/.*"tag_name": *"(v[^"]+)".*/\1/') \
    && wget https://github.com/quarto-dev/quarto-cli/releases/download/${QUARTO_VERSION}/quarto-${QUARTO_VERSION#v}-linux-amd64.tar.gz \
    && mkdir -p ~/opt \
    && tar -C ~/opt -xvzf quarto-${QUARTO_VERSION#v}-linux-amd64.tar.gz \
    && ln -s ~/opt/quarto-${QUARTO_VERSION#v}/bin/quarto /usr/local/bin/quarto \
    && rm quarto-${QUARTO_VERSION#v}-linux-amd64.tar.gz

# installGithub compiles, but install2.r uses bspm binaries so much quicker:
RUN install2.r \
  # rsconnect \
  backports \
  brio \
  checkmate \
  commonmark \
  curl \
  gert \
  gh \
  git2r \
  httr2 \
  igraph \
  lazyeval \
  lubridate \
  pbapply \
  pkgload \
  readr \
  roxygen2 \
  tidyr \
  tzdb \
  quarto \
  selectr \
  SnowballC \
  timechange \
  tokenizers \
  treesitter.r \
  treesitter \
  visNetwork \
  vroom \
  xml2

RUN installGithub.r \
  ropensci-review-tools/pkgmatch \
  ropensci-review-tools/orgmetrics

COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
