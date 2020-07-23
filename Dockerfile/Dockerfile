
FROM rocker/r-parallel

# RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  
 
# install common R packages
RUN install2.r --error \
    -r "https://mirrors.tuna.tsinghua.edu.cn/CRAN/" \
    vroom \
    feather \
    microbenchmark

# Set timezone
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

WORKDIR /app

COPY order_detail.tsv app/