FROM ubuntu:23.10 AS base
RUN apt-get -qq update &&\
    apt-get -qq install -y sbcl curl gcc git libffi-dev\
                           libopengl-dev libsdl2-dev libglew-dev

WORKDIR /opt
RUN curl -s 'https://beta.quicklisp.org/quicklisp.lisp' > /opt/quicklisp.lisp
RUN sbcl --noinform --load /opt/quicklisp.lisp\
         --eval '(quicklisp-quickstart:install :path "/opt/quicklisp")'\
         --eval '(sb-ext:quit)'
RUN mkdir -p quicklisp
RUN mkdir -p /opt/data
RUN apt-get -qq remove curl -y &&\
    apt-get -qq autoremove -y &&\
    apt-get -qq autoclean -y

from base AS build

WORKDIR /opt
ADD src quicklisp/local-projects/gmsh/src
ADD test quicklisp/local-projects/gmsh/test
ADD gmsh.asd quicklisp/local-projects/gmsh
ADD run-tests.sh quicklisp/local-projects/gmsh/run-tests.sh
RUN mkdir -p ~/quicklisp/ &&\
    ln -s /opt/quicklisp/setup.lisp ~/quicklisp/setup.lisp

WORKDIR /opt/quicklisp/local-projects
RUN git clone https://github.com/inconvergent/cl-veq.git veq
RUN git clone https://github.com/inconvergent/cl-grph.git grph
RUN git clone https://github.com/inconvergent/lqn.git lqn
RUN git clone https://github.com/inconvergent/auxin.git auxin

WORKDIR /opt/quicklisp/local-projects/gmsh/
CMD ["bash", "./run-tests.sh"]

