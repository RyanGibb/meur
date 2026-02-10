# Meur

> Meur (/ˈmĩãɾ/) -- Scots Gaelic: branch.

A highly opinionated static site generator built with [Hakyll](https://jaspervdj.be/hakyll/) and [Pandoc](https://pandoc.org/) featuring:

- Paper and talks pages generated from `.bib` files
- Referencing with CSL formatting
- Photo gallery generated from EXIF metadata
- Dual HTML/Markdown output
- Org-mode log rendering
- RSS/Atom and JSON feeds

This was built for [https://ryan.freumh.org/](https://ryan.freumh.org/).

## Building

With cabal;

```bash
$ cabal build
```

Or nix;

```bash
nix build
```

## Configuration

Create a `site.yaml` file:

```yaml
title: "My Site"
description: "My awesome site"
root: "https://example.com"
author:
  name: "Your Name"
  email: "you@example.com"
  url: "https://example.com"
```

See a complete template at [`template/`](./template) and live at [https://ryan.freumh.org/var/meur-template/](https://ryan.freumh.org/var/meur-template/).
