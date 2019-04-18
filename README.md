Emacs Starter Collab
======================

This is a simple starter template that provides everything needed to begin writing a literate
configuration for Emacs. It includes several optimizations that ensures that the config is
loaded very quickly. It benefits heavily from being byte compiled.


### Usage

Install

    git clone https://github.com/DynamicMetaFlow/emacs-starter-collab.git ~/.emacs.d

Compile (not required, but recommended)

    cd ~/.emacs.d
    make compile

Run

    emacs


### Make Commands

**clean**: Delete compiled files

    make clean

**compile**: Byte compile for performance (Recompile required when new changes are made)

    make compile

