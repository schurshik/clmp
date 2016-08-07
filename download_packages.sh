#!/bin/bash

# CLMP
# download_packages.sh
# Developer: Branitskiy Alexander <schurshick@yahoo.com>

packages_dir=packages

function download {
    wget --output-document ${packages_dir}/asdf.tar.gz http://common-lisp.net/project/asdf/asdf.tar.gz
    ([ -d packages/asdf ] || mkdir packages/asdf) && tar zxvf ${packages_dir}/asdf.tar.gz -C ${packages_dir}/asdf
    rm -f ${packages_dir}/asdf.tar.gz
#    wget --output-document ${packages_dir}/uffi.tar.gz http://files.b9.com/uffi/uffi-latest.tar.gz
#    tar zxvf ${packages_dir}/uffi.tar.gz -C ${packages_dir}
#    rm -f ${packages_dir}/uffi.tar.gz
    wget --output-document ${packages_dir}/uffi.tgz http://beta.quicklisp.org/archive/uffi/2015-09-23/uffi-20150923-git.tgz
    tar zxvf ${packages_dir}/uffi.tgz -C ${packages_dir}
    rm -f ${packages_dir}/uffi.tgz
    mv ${packages_dir}/uffi* ${packages_dir}/uffi
    #wget --output-document ${packages_dir}/alexandria.zip https://gitlab.common-lisp.net/alexandria/alexandria/repository/archive.zip
    #unzip ${packages_dir}/alexandria.zip -d ${packages_dir}
    #rm -f ${packages_dir}/alexandria.zip
    git clone https://gitlab.common-lisp.net/alexandria/alexandria.git ${packages_dir}/alexandria
    mv ${packages_dir}/alexandria* ${packages_dir}/alexandria
    wget --output-document ${packages_dir}/trivial-features.tar.gz wget http://common-lisp.net/~loliveira/tarballs/trivial-features/trivial-features_0.6.tar.gz
    tar zxvf ${packages_dir}/trivial-features.tar.gz -C ${packages_dir}
    rm -f ${packages_dir}/trivial-features.tar.gz
    mv ${packages_dir}/trivial-features* ${packages_dir}/trivial-features
    wget --output-document ${packages_dir}/babel.tar.gz https://common-lisp.net/project/babel/releases/babel_latest.tar.gz
    tar zxvf ${packages_dir}/babel.tar.gz -C ${packages_dir}
    rm -f ${packages_dir}/babel.tar.gz
    mv ${packages_dir}/babel* ${packages_dir}/babel
    wget --output-document ${packages_dir}/cffi.tar.gz https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz
    tar zxvf ${packages_dir}/cffi.tar.gz -C ${packages_dir}
    rm -f ${packages_dir}/cffi.tar.gz
    mv ${packages_dir}/cffi* ${packages_dir}/cffi
    wget --output-document ${packages_dir}/cl-ncurses.tgz http://common-lisp.net/project/cl-ncurses/files/cl-ncurses_latest-version.tgz
    tar zxvf ${packages_dir}/cl-ncurses.tgz -C ${packages_dir}
    rm -f ${packages_dir}/cl-ncurses.tgz
    mv ${packages_dir}/cl-ncurses* ${packages_dir}/cl-ncurses
    wget --output-document ${packages_dir}/cl-ppcre.tar.gz wget http://weitz.de/files/cl-ppcre.tar.gz
    tar zxvf ${packages_dir}/cl-ppcre.tar.gz -C ${packages_dir}
    rm -f ${packages_dir}/cl-ppcre.tar.gz
    mv ${packages_dir}/cl-ppcre* ${packages_dir}/cl-ppcre
    wget --output-document ${packages_dir}/bordeaux-threads.tar.gz https://common-lisp.net/project/bordeaux-threads/releases/bordeaux-threads.tar.gz
    tar zxvf ${packages_dir}/bordeaux-threads.tar.gz -C ${packages_dir}
    rm -f ${packages_dir}/bordeaux-threads.tar.gz
    mv ${packages_dir}/bordeaux-threads* ${packages_dir}/bordeaux-threads
    wget --output-document ${packages_dir}/buildapp.tgz http://www.xach.com/lisp/buildapp.tgz
    tar zxvf ${packages_dir}/buildapp.tgz -C ${packages_dir}
    rm -f ${packages_dir}/buildapp.tgz
    mv ${packages_dir}/buildapp* ${packages_dir}/buildapp
}

function compile {
    cd ${packages_dir}/asdf && make
    cd -
    cd ${packages_dir}/uffi && make
    cd -
    cd ${packages_dir}/cffi && make
    cd -
    cd ${packages_dir}/cl-ncurses && make
    cd -
    cd ${packages_dir}/buildapp && make
    cd -
}

function main {
    if ! [ -d packages ]; then
	mkdir $packages_dir
    fi
    download
    compile
}

main
