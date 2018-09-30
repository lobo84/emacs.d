emacs.d
=======

my emacs config

create symlink to emacs home:

```
ln -s emacs.d ~/.emacs.d
```

c++
----

run M-x rtags-install

Create a file .dir-locals.el in your cmake project.

Example content:

(nil . ((cmake-ide-project-dir . "/home/niclas/tmp/build")))