# Updating LFE Documentation

## Structure

In order to support multiple format document generation as well as inclusion
in a LFE static documentation site generator, the LFE docs moved from ``.txt``
files to ``.md`` files. The old ``.txt`` files are still provided in their
original locations (though their formatting has changed a little), however
these files are now generated from the Markdown source files in ``doc/src``.

In addition to generating the "classic" LFE text file documentation, LFE man
pages are now also generated and provided in the LFE git repo. There are
also ``make`` targets for generating ``.pdf`` and ``.epub`` versions of the
LFE docs, but these are not maintained in the git repo.

The layout of the LFE documentation files tracked by the LFE git repository is
as follows:

* ``doc/src/*.md`` - Canonical LFE documentation
* ``doc/*.txt`` - Auto-generated LFE documentation in plain-text format
* ``doc/man/*.[1-7]`` - Auto-generated LFE documentation in man-page format

Files not tracked by the repository but present after running ``make docs-all``
are as follows:

* ``doc/pdf/*.pdf``
* ``doc/epub/*.epub``


## (Re)Building the Docs

There are two ways of updating the LFE documentation:

* Native: running the ``make docs`` or ``make docs-all`` targets
* Docker: running the ``make docker-docs`` target


### Native

Building LFE documentation natively on one's system depends upon Pandoc, which
in turn depends upon Haskell. Furthermore, the ``groff`` and ``col`` utilities
are used to required to convert man page files to the preferred plain-text
format.

On Debian-based systems, these may be installed with the following:

```
$ sudo apt-get install -y pandoc groff groff-base bsdmainutils
```

Since ``col`` will need to have a UTF-8 locate configured, we also recommend
the following:

```
$ sudo locale-gen en_US.UTF-8 UTF-8
$ sudo dpkg-reconfigure locales
```

After running the first command, if you're ``/etc/locale.gen`` file still has
your preferred locale commented out, you may need to edit that file and then
re-run ``sudo locale-gen && sudo dpkg-reconfigure locales``.

At this point, you should have everything ready to generate the LFE docs from
Markdown source. Regenerating the docs from the source ``.md`` files may then
be performed by executing the following:

```
$ make docs
```


### Docker

Since the overhead of installing the above documentation dependencies may be
too much for some users, we have also provided a ``Dockerfile`` in the ``doc``
directory for building LFE docs from source without having to install Pandoc,
Haskell, etc., on your own system. This does, of course, require that you have
``docker`` installed and running on your system.

You will need to use the LFE docs ``Dockerfile`` to generate an LFE Docker
image for that has the latest LFE docs in it:

```
$ make docker-build
```

Then, to generate the LFE docs using the Docker image:

```
$ make docker-docs
```

This ``make`` target will generate the LFE docs inside the Docker image, but
it will also mount the local/host ``./doc`` directory, over-writing the
existing ``doc/*.txt`` and ``doc/man/*.[1-7]`` files.  Since these files
will be regenerated as ``root`` in the Docker image, the final step in the
``make `` target is to ``chown`` these files to ``$USER:$USER``.
