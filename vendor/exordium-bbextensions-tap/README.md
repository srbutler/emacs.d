# Exordium tap with Bloomberg specific extenstions

This is a `tap` for [Exordium](https://github.com/philippe-grenet/exordium). It provides
Bloomberg specific extensions that don't really fit into original Exordium repository.

## Installation

Just clone this repository into a new sub-directory in your Exordium setup. I.e,

```sh
mkdir ~/.emacs.d/taps
git clone bbgithub:emacs/exordium-bbextensions-tap ~/.emacs.d/taps/exordium-bbextensions-tap
```

## Extensions

### Proxy set up

It uses `before-init.el` to set up Bloomberg development proxies.

### Magit commit extra validation

When the word that start with `@` is detected it will be validated with `bbempinf`.
The validation is run iff the latter is present on the system. User still has a possibility
to ignore false positives and proceed with commit.

Additionally, when the word `[Pp]romote` is detected in commit message it ensures it's
followed with either `minor` or `patchf`.

### org-mode Bloomberg links

Add a new type of link, `bbg` to org-mode. Together with the function that launches them in
the Terminal.

### Bloomberg documentation browser

A new function `browse-doc` that will launch `eww` browser with Bloomberg library documentation
directly in your `emacs`.

### bde-format

It uses original `clang-format` to fire `bde-format` executable. The latter can be customised
by changing `exordium-bde-format-executable` value.

The following key bindings are added to `c-mode-map`:


Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>C-c t</kbd>     | Run `bde-format` on selected region.
<kbd>C-u C-c t</kbd> | Run `bde-format` on the whole buffer.
